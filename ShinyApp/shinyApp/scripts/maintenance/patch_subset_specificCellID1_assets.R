#!/usr/bin/env Rscript

# ---------------------------------------------------------------------------
# Patch subset ShinyCell metadata/config assets
# ---------------------------------------------------------------------------
# This maintenance script backfills specificCellID.1 into subset ShinyCell
# meta/conf files using the full-atlas sc3 metadata and palette as the source of
# truth. It is intended for regenerated subset assets, not runtime execution.

# ---------------------------------------------------------------------------
# Package imports and app-root bootstrap
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
})

bootstrap_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(...) NULL)
bootstrap_dir <- if (!is.null(bootstrap_path) && nzchar(bootstrap_path)) {
  dirname(normalizePath(bootstrap_path, mustWork = FALSE))
} else {
  getwd()
}
app_root <- normalizePath(file.path(bootstrap_dir, "..", ".."), mustWork = FALSE)
source(file.path(app_root, "app_support.R"))
app_dir <- sc_set_app_dir(sc_find_app_dir(start = app_root))

# ---------------------------------------------------------------------------
# Command-line arguments
# ---------------------------------------------------------------------------
# Usage:
#   Rscript patch_subset_specificCellID1_assets.R [out_dir] [prefix_csv]
# Defaults patch sc4, sc5, and sc7 in the app Data/ directory.
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) sc_dir_arg(args[[1]], app_dir = app_dir) else sc_data_dir(app_dir = app_dir)
prefixes <- if (length(args) >= 2) strsplit(args[[2]], ",", fixed = TRUE)[[1]] else c("sc4", "sc5", "sc7")
prefixes <- trimws(prefixes)

master_meta_path <- file.path(out_dir, "sc3meta.rds")
master_conf_path <- file.path(out_dir, "sc3conf.rds")

# ---------------------------------------------------------------------------
# Full-atlas source metadata
# ---------------------------------------------------------------------------
# The full atlas defines the canonical cell labels and colors that subset assets
# should mirror when the same cells are present.
if (!file.exists(master_meta_path)) {
  stop(sprintf("Missing master meta file: %s", master_meta_path), call. = FALSE)
}
if (!file.exists(master_conf_path)) {
  stop(sprintf("Missing master conf file: %s", master_conf_path), call. = FALSE)
}

master_meta <- readRDS(master_meta_path)
master_conf <- as.data.table(readRDS(master_conf_path))

if (!("sampleID" %in% names(master_meta))) {
  stop("sc3meta.rds must contain a 'sampleID' column.", call. = FALSE)
}
if (!("specificCellID.1" %in% names(master_meta))) {
  stop("sc3meta.rds must contain a 'specificCellID.1' column.", call. = FALSE)
}

base_row <- master_conf[UI == "specificCellID.1"]
if (nrow(base_row) != 1) {
  stop("Expected exactly one config row for UI == 'specificCellID.1' in sc3conf.rds.", call. = FALSE)
}

base_levels <- strsplit(base_row$fID[[1]], "\\|")[[1]]
base_colors <- strsplit(base_row$fCL[[1]], "\\|")[[1]]
if (length(base_levels) != length(base_colors)) {
  stop("Master palette fID/fCL length mismatch for specificCellID.1.", call. = FALSE)
}
color_map <- setNames(base_colors, base_levels)

value_map <- master_meta[["specificCellID.1"]]
if (is.factor(value_map)) {
  value_map <- as.character(value_map)
} else {
  value_map <- as.character(value_map)
}
names(value_map) <- as.character(master_meta[["sampleID"]])

# ---------------------------------------------------------------------------
# Per-prefix patcher
# ---------------------------------------------------------------------------
# Patch each subset in place. Missing files are skipped so the same command can
# be run against partial asset directories during development.
patch_one <- function(prefix) {
  meta_path <- file.path(out_dir, paste0(prefix, "meta.rds"))
  conf_path <- file.path(out_dir, paste0(prefix, "conf.rds"))

  if (!file.exists(meta_path)) {
    message(sprintf("[%s] Skip: missing %s", prefix, meta_path))
    return(invisible(NULL))
  }
  if (!file.exists(conf_path)) {
    message(sprintf("[%s] Skip: missing %s", prefix, conf_path))
    return(invisible(NULL))
  }

  meta <- readRDS(meta_path)
  conf <- as.data.table(readRDS(conf_path))

  if (!("sampleID" %in% names(meta))) {
    message(sprintf("[%s] Skip: meta missing sampleID", prefix))
    return(invisible(NULL))
  }

  ids <- as.character(meta[["sampleID"]])
  vals <- unname(value_map[ids])

  missing <- is.na(vals) | !nzchar(vals)
  if (any(missing) && "CellType" %in% names(meta)) {
    legacy <- as.character(meta[["CellType"]])
    vals[missing] <- legacy[missing]
    missing <- is.na(vals) | !nzchar(vals)
  }

  if (any(missing)) {
    message(sprintf("[%s] Warning: %d/%d cells missing specificCellID.1 mapping.", prefix, sum(missing), length(vals)))
  }

  present <- unique(vals[!is.na(vals) & nzchar(vals)])
  present_levels <- base_levels[base_levels %in% present]
  if (!length(present_levels)) {
    present_levels <- sort(present)
  }

  meta[["specificCellID.1"]] <- factor(vals, levels = present_levels)

  cols <- unname(color_map[present_levels])
  if (anyNA(cols)) {
    fallback <- grDevices::hcl.colors(sum(is.na(cols)), "Set 3")
    cols[is.na(cols)] <- fallback
  }

  new_row <- data.table(
    ID = "specificCellID.1",
    UI = "specificCellID.1",
    fID = paste(present_levels, collapse = "|"),
    fCL = paste(cols, collapse = "|"),
    fRow = base_row$fRow[[1]],
    grp = TRUE,
    dimred = FALSE
  )

  if (any(conf$UI == "specificCellID.1", na.rm = TRUE)) {
    conf[UI == "specificCellID.1", `:=`(
      ID = new_row$ID,
      fID = new_row$fID,
      fCL = new_row$fCL,
      fRow = new_row$fRow,
      grp = TRUE,
      dimred = FALSE
    )]
  } else {
    conf <- rbind(conf, new_row, fill = TRUE)
  }

  saveRDS(meta, meta_path)
  saveRDS(conf, conf_path)
  return(message(sprintf("[%s] Patched: added/updated specificCellID.1 in meta+conf.", prefix)))
}

# ---------------------------------------------------------------------------
# Script entrypoint
# ---------------------------------------------------------------------------
for (p in prefixes) {
  if (!nzchar(p)) next
  patch_one(p)
}
