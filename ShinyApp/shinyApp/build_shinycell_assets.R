#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(Seurat)
  library(ShinyCell)
  library(data.table)
})

bootstrap_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(...) NULL)
bootstrap_dir <- if (!is.null(bootstrap_path) && nzchar(bootstrap_path)) {
  dirname(normalizePath(bootstrap_path, mustWork = FALSE))
} else {
  getwd()
}
source(file.path(bootstrap_dir, "app_support.R"))
app_dir <- sc_set_app_dir(sc_find_app_dir(start = sc_script_dir()))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop(
    paste(
      "Usage:",
      "  Rscript build_shinycell_assets.R <seurat_rds> <prefix> [out_dir] [chunk_size]",
      "",
      "Example:",
      "  Rscript ShinyApp/shinyApp/build_shinycell_assets.R /path/to/02_Sertoli.rds sc4 ShinyApp/shinyApp 500",
      sep = "\n"
    ),
    call. = FALSE
  )
}

seurat_path <- sc_first_existing(c(args[[1]]), app_dir = app_dir)
prefix <- args[[2]]
out_dir <- if (length(args) >= 3) sc_dir_arg(args[[3]], app_dir = app_dir) else app_dir
chunk_size <- if (length(args) >= 4) as.integer(args[[4]]) else 500L
if (is.na(chunk_size) || chunk_size <= 0) {
  chunk_size <- 500L
}

if (!file.exists(seurat_path)) {
  stop(sprintf("Seurat object not found: %s", seurat_path), call. = FALSE)
}
if (!dir.exists(out_dir)) {
  stop(sprintf("Output directory does not exist: %s", out_dir), call. = FALSE)
}

ensure_trailing_sep <- function(path) file.path(path, "")
shiny_dir <- ensure_trailing_sep(out_dir)

# Paper-matched palette for correct_cellTypes
my_cols <- c(
  "Aund" = "#d13732",
  "A1-2" = "#e89d9b",
  "A3-4" = "#90be6d",
  "Ain" = "#3a77b1",
  "Type B" = "#aecee3",
  "ePL" = "#D24B27",
  "lPL" = "#80823C",
  "L" = "#f2ec52",
  "L/Z" = "#37572f",
  "Z" = "#4c8749",
  "PaI-VI" = "#99c458",
  "PaVII-VIII" = "#C8d751",
  "PaIX-X" = "#e6863b",
  "D/MI" = "#f3c17b",
  "Rd1" = "#88CCEE",
  "Rd2-3" = "#CC61B0",
  "Rd4-5" = "#11A579",
  "Rd6" = "#E73F74",
  "Rd7" = "#0C727C",
  "Rd8" = "#6E4B9E",
  "El9" = "#3BBCA8",
  "El10" = "#D24B27",
  "El11" = "#9983BD",
  "El12-13" = "#8A9FD1",
  "El14-15" = "#E6C2DC",
  "El16" = "#7E1416",
  "SC_I-VIII" = "#89288F",
  "SC_VII-VIII" = "#2F8AC4",
  "SC_IX-XII" = "#D8A767",
  "SC_XI-VI" = "#7F6B40",
  "SC_All_Stages" = "#BCBD22",
  "Leydig" = "#8C6D31",
  "PTM" = "#D27C2C",
  "Macrophage" = "#E9A9B8"
)

message(sprintf("Loading Seurat object: %s", seurat_path))
obj <- readRDS(seurat_path)

if (inherits(obj, "Seurat")) {
  # Images can be large and are not needed for ShinyCell assets.
  obj@images <- list()
}

standardize_umap <- function(seu) {
  if (!inherits(seu, "Seurat")) {
    return(seu)
  }
  reds <- names(seu@reductions)
  if (!length(reds)) {
    return(seu)
  }
  for (r in reds) {
    emb <- tryCatch(Seurat::Embeddings(seu, r), error = function(e) NULL)
    if (is.null(emb) || ncol(emb) < 2) {
      next
    }
    cn <- colnames(emb)
    if (identical(cn[1:2], c("umap_1", "umap_2"))) {
      colnames(seu@reductions[[r]]@cell.embeddings)[1:2] <- c("UMAP_1", "UMAP_2")
    }
  }
  seu
}

add_correct_celltypes <- function(seu, mapping_path) {
  if (!inherits(seu, "Seurat")) {
    return(seu)
  }
  if ("correct_cellTypes" %in% colnames(seu@meta.data)) {
    return(seu)
  }
  if (!file.exists(mapping_path)) {
    message(sprintf("Note: mapping file not found (%s); not adding correct_cellTypes.", mapping_path))
    return(seu)
  }

  master <- readRDS(mapping_path)
  if (!is.data.frame(master) || !"correct_cellTypes" %in% names(master)) {
    message(sprintf("Note: %s does not contain correct_cellTypes; not adding.", mapping_path))
    return(seu)
  }

  key <- NULL
  if ("sampleID" %in% names(master)) {
    key <- as.character(master[["sampleID"]])
  } else if (!is.null(rownames(master))) {
    key <- rownames(master)
  }
  if (is.null(key) || !length(key)) {
    message(sprintf("Note: unable to derive cell keys from %s; not adding.", mapping_path))
    return(seu)
  }

  master_ct <- master[["correct_cellTypes"]]
  if (is.factor(master_ct)) {
    master_levels <- levels(master_ct)
    master_ct <- as.character(master_ct)
  } else {
    master_levels <- NULL
    master_ct <- as.character(master_ct)
  }
  names(master_ct) <- key

  cells <- colnames(seu)
  ct <- master_ct[cells]
  missing_mask <- is.na(ct) | !nzchar(ct)

  # Fallback mapping for legacy subset objects that don't fully overlap with the
  # staged-testis master meta (e.g. older exports).
  if (any(missing_mask) && "CellType" %in% colnames(seu@meta.data)) {
    legacy <- as.character(seu@meta.data[["CellType"]])
    legacy_map <- c(
      "Aund-A1" = "Aund",
      "A2-4 Diff SPG" = "A3-4",
      "Ain" = "Ain",
      "Type B" = "Type B"
    )
    filled <- legacy_map[legacy[missing_mask]]
    ok <- !is.na(filled) & nzchar(filled)
    ct[which(missing_mask)[ok]] <- filled[ok]
    missing_mask <- is.na(ct) | !nzchar(ct)
  }

  n_missing <- sum(is.na(ct) | !nzchar(ct))
  if (n_missing > 0) {
    message(sprintf("Warning: correct_cellTypes mapping missing for %d/%d cells.", n_missing, length(cells)))
  }
  if (!is.null(master_levels)) {
    ct <- factor(ct, levels = master_levels)
  } else {
    ct <- factor(ct)
  }
  seu@meta.data$correct_cellTypes <- ct
  seu
}

obj <- standardize_umap(obj)
obj <- add_correct_celltypes(obj, file.path(out_dir, "sc3meta.rds"))

assays <- tryCatch(Seurat::Assays(obj), error = function(e) character(0))
assay <- if ("RNA" %in% assays) "RNA" else tryCatch(Seurat::DefaultAssay(obj), error = function(e) NA_character_)
if (is.na(assay) || !nzchar(assay)) {
  if (length(assays)) {
    assay <- assays[[1]]
  } else {
    stop("Unable to determine an assay to use (no assays detected).", call. = FALSE)
  }
}

try(Seurat::DefaultAssay(obj) <- assay, silent = TRUE)

if ("JoinLayers" %in% getNamespaceExports("Seurat")) {
  obj <- tryCatch(
    Seurat::JoinLayers(obj),
    error = function(e) {
      message(sprintf("JoinLayers() skipped/failed: %s", conditionMessage(e)))
      obj
    }
  )
}

message("Creating ShinyCell config…")
sc_conf <- ShinyCell::createConfig(obj)

message(sprintf("Writing ShinyCell assets to %s (prefix %s)…", normalizePath(out_dir), prefix))
ShinyCell::makeShinyFiles(
  obj,
  sc_conf,
  gex.assay = assay,
  gex.slot = "data",
  gene.mapping = TRUE,
  shiny.prefix = prefix,
  shiny.dir = shiny_dir,
  chunkSize = chunk_size
)

conf_path <- file.path(out_dir, paste0(prefix, "conf.rds"))
def_path <- file.path(out_dir, paste0(prefix, "def.rds"))
meta_path <- file.path(out_dir, paste0(prefix, "meta.rds"))

if (!file.exists(conf_path) || !file.exists(def_path) || !file.exists(meta_path)) {
  stop(
    sprintf(
      "Expected output files were not written. Missing: %s",
      paste(
        c(
          if (!file.exists(conf_path)) conf_path,
          if (!file.exists(def_path)) def_path,
          if (!file.exists(meta_path)) meta_path
        ),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

message("Patching defaults + paper-matched palette…")
meta <- readRDS(meta_path)
conf <- as.data.table(readRDS(conf_path))
def <- readRDS(def_path)

if (is.list(def)) {
  if ("correct_cellTypes" %in% names(meta)) {
    def$meta1 <- "correct_cellTypes"
  } else {
    message("Note: meta.data does not contain 'correct_cellTypes'; leaving defaults unchanged.")
  }
} else {
  message("Warning: def file is not a list; leaving defaults unchanged.")
}

if ("correct_cellTypes" %in% names(meta) && "correct_cellTypes" %in% conf$ID) {
  ct <- meta[["correct_cellTypes"]]
  if (!is.factor(ct)) {
    ct <- factor(as.character(ct))
  }
  lvls <- levels(ct)
  missing <- setdiff(lvls, names(my_cols))
  if (length(missing)) {
    message(sprintf(
      "Warning: missing palette entries for: %s",
      paste(missing, collapse = ", ")
    ))
  }
  cols <- unname(my_cols[lvls])
  conf[ID == "correct_cellTypes", `:=`(
    fID = paste(lvls, collapse = "|"),
    fCL = paste(cols, collapse = "|")
  )]
}

saveRDS(conf, conf_path)
saveRDS(def, def_path)

message("Done.")
message(sprintf("  %s", normalizePath(conf_path)))
message(sprintf("  %s", normalizePath(def_path)))
message(sprintf("  %s", normalizePath(meta_path)))
