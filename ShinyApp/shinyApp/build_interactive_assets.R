#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(Seurat)
  library(Matrix)
})

args <- commandArgs(trailingOnly = TRUE)
default_seurat_candidates <- c(
  "specificCellID_slim_nocounts.rds",
  "specificCellID_slim.rds",
  "specificCellID.rds",
  "final_staged_object_slim_nocounts.rds"
)
seurat_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  pick <- default_seurat_candidates[file.exists(default_seurat_candidates)][1]
  if (is.na(pick) || !nzchar(pick)) "final_staged_object_slim_nocounts.rds" else pick
}
out_dir <- if (length(args) >= 2) args[[2]] else "."

if (!file.exists(seurat_path)) {
  stop(sprintf("Seurat object not found at: %s", seurat_path))
}
if (!dir.exists(out_dir)) {
  stop(sprintf("Output directory does not exist: %s", out_dir))
}

message(sprintf("Loading Seurat object: %s", seurat_path))
obj <- readRDS(seurat_path)

read_assay_data <- function(obj, assay_name) {
  if (!(assay_name %in% Assays(obj))) {
    return(NULL)
  }
  tryCatch(
    GetAssayData(obj, assay = assay_name, layer = "data"),
    error = function(...) {
      tryCatch(
        GetAssayData(obj, assay = assay_name, slot = "data"),
        error = function(...) NULL
      )
    }
  )
}

pick_assay_data <- function(obj, assay_preferences, purpose) {
  for (assay_name in assay_preferences) {
    mat <- read_assay_data(obj, assay_name)
    if (!is.null(mat)) {
      return(list(assay = assay_name, mat = mat))
    }
  }
  stop(
    sprintf(
      "Could not read assay data for %s. Tried: %s",
      purpose,
      paste(assay_preferences, collapse = ", ")
    ),
    call. = FALSE
  )
}

ra_assay_data <- pick_assay_data(obj, c("SCT", "RNA"), "RA assets")
spg_assay_data <- pick_assay_data(obj, c("RNA", "SCT"), "spermatogenesis table assets")

message(sprintf("Using assay '%s' for RA assets.", ra_assay_data$assay))
message(sprintf("Using assay '%s' for spermatogenesis table assets.", spg_assay_data$assay))

ra_expr <- ra_assay_data$mat
spg_expr <- spg_assay_data$mat

interactive_genes <- intersect(rownames(ra_expr), rownames(spg_expr))
if (!length(interactive_genes)) {
  stop("No overlapping genes between RA assay matrix and spermatogenesis assay matrix.", call. = FALSE)
}

# Keep all generated assets on the same gene set for consistent downstream indexing.
ra_expr <- ra_expr[interactive_genes, , drop = FALSE]
spg_expr <- spg_expr[interactive_genes, , drop = FALSE]

meta <- obj@meta.data
meta$sample <- as.character(meta$sample)
meta$generalCellID <- as.character(meta$generalCellID)
meta$generalCellID[meta$generalCellID == "Somatic"] <- "Sertoli"

pick_specific_col <- function(meta_df) {
  candidates <- c("specificCellID", "correct_cellTypes", "specificCellID.1")
  available <- candidates[candidates %in% colnames(meta_df)]
  if (!length(available)) {
    stop(
      sprintf(
        "Missing specific-cell column. Need one of: %s",
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  non_na_counts <- vapply(
    available,
    function(col) sum(!is.na(meta_df[[col]]) & nzchar(as.character(meta_df[[col]]))),
    numeric(1)
  )
  available[[which.max(non_na_counts)]]
}

specific_col <- pick_specific_col(meta)
meta$specificCellID <- as.character(meta[[specific_col]])
message(sprintf("Using '%s' for specificCellID grouping.", specific_col))

required_cols <- c("sample", "generalCellID")
missing_cols <- setdiff(required_cols, colnames(meta))
if (length(missing_cols)) {
  stop(sprintf("Missing required meta.data columns: %s", paste(missing_cols, collapse = ", ")))
}

summarize_by_group <- function(rna_mat, groups) {
  if (length(groups) != ncol(rna_mat)) {
    stop("Group vector length must match number of cells in expression matrix.", call. = FALSE)
  }

  groups <- as.character(groups)
  keep_cells <- !is.na(groups) & nzchar(groups)
  if (!all(keep_cells)) {
    rna_mat <- rna_mat[, keep_cells, drop = FALSE]
    groups <- groups[keep_cells]
  }
  if (!length(groups)) {
    stop("No cells left after removing NA/empty group labels.", call. = FALSE)
  }

  groups <- factor(groups)
  design <- Matrix::sparse.model.matrix(~0 + groups)
  colnames(design) <- levels(groups)

  n_per_group <- Matrix::colSums(design)
  keep <- n_per_group > 0
  if (!all(keep)) {
    design <- design[, keep, drop = FALSE]
    n_per_group <- n_per_group[keep]
  }

  avg <- rna_mat %*% design
  avg <- avg %*% Matrix::Diagonal(x = as.numeric(1 / n_per_group))
  rownames(avg) <- rownames(rna_mat)
  colnames(avg) <- colnames(design)

  rna_bin <- rna_mat
  rna_bin@x <- as.numeric(rna_bin@x > 0)
  pct <- rna_bin %*% design
  pct <- pct %*% Matrix::Diagonal(x = as.numeric(100 / n_per_group))
  rownames(pct) <- rownames(rna_mat)
  colnames(pct) <- colnames(design)

  list(
    avg = as.matrix(avg),
    pct = as.matrix(pct),
    groups = colnames(design)
  )
}

# ---- Gene list (full coverage) ----
message(sprintf("Saving %d genes", length(interactive_genes)))
saveRDS(interactive_genes, file.path(out_dir, "interactive_genes.rds"))

# ---- RA DotPlot assets (avg + pct by active Idents) ----
idents <- factor(meta$specificCellID, levels = unique(meta$specificCellID))
if (!length(idents)) stop("Unable to resolve identities for RA DotPlot.", call. = FALSE)
idents <- factor(idents, levels = levels(idents))
message("Computing RA DotPlot summaries...")
ra_dot <- summarize_by_group(ra_expr, idents)
saveRDS(ra_dot$avg, file.path(out_dir, "ra_dot_avg_expr.rds"))
saveRDS(ra_dot$pct, file.path(out_dir, "ra_dot_pct_expr.rds"))

# ---- RA LinePlot assets (mean by sample + generalCellID) ----
message("Computing RA LinePlot summaries...")
stage_levels_ref <- c(
  "I-VI (Weak to Strong)",
  "VII-VIII (Dark)",
  "IX-X (Pale)",
  "XI-XII (Pale to Weak)"
)
sample_levels <- c(stage_levels_ref[stage_levels_ref %in% unique(meta$sample)],
                   setdiff(unique(meta$sample), stage_levels_ref))
general_ref <- c("Spermatogonia", "Spermatocyte", "Round Spermatid", "Elongating Spermatid", "Sertoli")
general_levels <- c(general_ref[general_ref %in% unique(meta$generalCellID)],
                    setdiff(unique(meta$generalCellID), general_ref))

sample_factor <- factor(meta$sample, levels = sample_levels)
general_factor <- factor(meta$generalCellID, levels = general_levels)
combo_key <- interaction(sample_factor, general_factor, drop = TRUE, sep = "__")

ra_line <- summarize_by_group(ra_expr, combo_key)
combo_levels <- ra_line$groups
combo_parts <- strsplit(combo_levels, "__", fixed = TRUE)
combo_sample <- vapply(combo_parts, `[`, character(1), 1)
combo_general <- vapply(combo_parts, `[`, character(1), 2)

line_arr <- array(
  NA_real_,
  dim = c(length(interactive_genes), length(sample_levels), length(general_levels)),
  dimnames = list(
    gene = interactive_genes,
    sample = sample_levels,
    generalCellID = general_levels
  )
)

for (i in seq_along(combo_levels)) {
  s_idx <- match(combo_sample[[i]], sample_levels)
  g_idx <- match(combo_general[[i]], general_levels)
  if (!is.na(s_idx) && !is.na(g_idx)) {
    line_arr[, s_idx, g_idx] <- ra_line$avg[, combo_levels[[i]]]
  }
}

saveRDS(line_arr, file.path(out_dir, "ra_line_mean_expr.rds"))

# ---- Spermatogenesis table assets (avg by button mapping) ----
message("Computing spermatogonia table summaries...")
mapping_path <- file.path(out_dir, "button_mapping_general.R")
if (!file.exists(mapping_path)) {
  script_file <- sys.frame(1)$ofile
  script_dir <- if (!is.null(script_file)) dirname(normalizePath(script_file)) else getwd()
  mapping_path <- file.path(script_dir, "button_mapping_general.R")
}
if (!file.exists(mapping_path)) {
  stop("button_mapping_general.R not found in output dir or script dir.")
}
source(mapping_path)

mapping_ids <- names(general_button_mapping)
if (length(mapping_ids) == 0) {
  stop("general_button_mapping is empty. Check button_mapping_general.R")
}

mapping_df <- do.call(rbind, lapply(mapping_ids, function(id) {
  entry <- general_button_mapping[[id]]
  data.frame(
    btn_id = id,
    sample = as.character(entry$sample),
    specificCellID = as.character(entry$specificCellID),
    stringsAsFactors = FALSE
  )
}))

mapping_df$group_key <- paste(mapping_df$sample, mapping_df$specificCellID, sep = "__")

meta_group_key <- paste(meta$sample, meta$specificCellID, sep = "__")
keep_cells <- meta_group_key %in% mapping_df$group_key

if (!any(keep_cells)) {
  stop("No cells matched the button mapping (sample + specificCellID).")
}

spg_groups <- factor(meta_group_key[keep_cells], levels = unique(mapping_df$group_key))
spg_summary <- summarize_by_group(spg_expr[, keep_cells, drop = FALSE], spg_groups)

spg_avg_by_group <- spg_summary$avg
spg_avg_by_button <- matrix(
  NA_real_,
  nrow = nrow(spg_avg_by_group),
  ncol = nrow(mapping_df),
  dimnames = list(rownames(spg_avg_by_group), mapping_df$btn_id)
)

match_idx <- match(mapping_df$group_key, colnames(spg_avg_by_group))
for (i in seq_along(match_idx)) {
  idx <- match_idx[[i]]
  if (!is.na(idx)) {
    spg_avg_by_button[, i] <- spg_avg_by_group[, idx]
  }
}

saveRDS(spg_avg_by_button, file.path(out_dir, "spg_avg_expr_by_button.rds"))

message("Done. Interactive assets written to:")
message(normalizePath(out_dir))
