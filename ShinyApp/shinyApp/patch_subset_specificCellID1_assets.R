#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[[1]] else "."
prefixes <- if (length(args) >= 2) strsplit(args[[2]], ",", fixed = TRUE)[[1]] else c("sc4", "sc5", "sc7")
prefixes <- trimws(prefixes)

master_meta_path <- file.path(out_dir, "sc3meta.rds")
master_conf_path <- file.path(out_dir, "sc3conf.rds")

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
  message(sprintf("[%s] Patched: added/updated specificCellID.1 in meta+conf.", prefix))
}

for (p in prefixes) {
  if (!nzchar(p)) next
  patch_one(p)
}

