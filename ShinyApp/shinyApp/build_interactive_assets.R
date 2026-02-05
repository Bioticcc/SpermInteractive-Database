#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(Seurat)
  library(Matrix)
})

args <- commandArgs(trailingOnly = TRUE)
seurat_path <- if (length(args) >= 1) args[[1]] else "final_staged_object_slim_nocounts.rds"
out_dir <- if (length(args) >= 2) args[[2]] else "."

if (!file.exists(seurat_path)) {
  stop(sprintf("Seurat object not found at: %s", seurat_path))
}
if (!dir.exists(out_dir)) {
  stop(sprintf("Output directory does not exist: %s", out_dir))
}

message(sprintf("Loading Seurat object: %s", seurat_path))
obj <- readRDS(seurat_path)

rna <- GetAssayData(obj, assay = "RNA", slot = "data")
meta <- obj@meta.data

required_cols <- c("sample", "specificCellID", "generalCellID")
missing_cols <- setdiff(required_cols, colnames(meta))
if (length(missing_cols)) {
  stop(sprintf("Missing required meta.data columns: %s", paste(missing_cols, collapse = ", ")))
}

summarize_by_group <- function(rna_mat, groups) {
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
interactive_genes <- rownames(rna)
message(sprintf("Saving %d genes", length(interactive_genes)))
saveRDS(interactive_genes, file.path(out_dir, "interactive_genes.rds"))

# ---- RA DotPlot assets (avg + pct by active Idents) ----
idents <- Idents(obj)
if (is.null(idents)) {
  stop("Active identities (Idents) are not set on the Seurat object.")
}
idents <- factor(idents, levels = levels(idents))
message("Computing RA DotPlot summaries...")
ra_dot <- summarize_by_group(rna, idents)
saveRDS(ra_dot$avg, file.path(out_dir, "ra_dot_avg_expr.rds"))
saveRDS(ra_dot$pct, file.path(out_dir, "ra_dot_pct_expr.rds"))

# ---- RA LinePlot assets (mean by sample + generalCellID) ----
message("Computing RA LinePlot summaries...")
sample_levels <- unique(as.character(meta$sample))
general_levels <- unique(as.character(meta$generalCellID))

sample_factor <- factor(meta$sample, levels = sample_levels)
general_factor <- factor(meta$generalCellID, levels = general_levels)
combo_key <- interaction(sample_factor, general_factor, drop = TRUE, sep = "__")

ra_line <- summarize_by_group(rna, combo_key)
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
spg_summary <- summarize_by_group(rna[, keep_cells, drop = FALSE], spg_groups)

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
