if (!exists("sc_get_app_dir", mode = "function")) {
  bootstrap_args <- commandArgs(trailingOnly = FALSE)
  bootstrap_file <- grep("^--file=", bootstrap_args, value = TRUE)
  bootstrap_dir <- if (length(bootstrap_file)) {
    dirname(normalizePath(sub("^--file=", "", bootstrap_file[[1]]), mustWork = FALSE))
  } else {
    frame_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(...) NULL)
    if (!is.null(frame_path) && nzchar(frame_path)) {
      dirname(normalizePath(frame_path, mustWork = FALSE))
    } else {
      getwd()
    }
  }
  source(file.path(bootstrap_dir, "app_support.R"))
}

app_dir <- sc_set_app_dir(sc_find_app_dir(start = sc_script_dir()))

read_rds_file <- function(path) readRDS(path)
read_csv_file <- function(path) utils::read.csv(path, check.names = FALSE)

get_final_staged_object <- sc_lazy_loader(
  c("final_staged_object_slim.rds", "final_staged_object.rds"),
  reader = read_rds_file,
  app_dir = app_dir
)

get_specific_obj <- sc_lazy_loader(
  c("specificCellID_slim.rds", "specificCellID.rds"),
  reader = read_rds_file,
  app_dir = app_dir
)

get_sc3conf <- sc_lazy_loader(
  c("sc3conf_slim.rds", "sc3conf.rds"),
  reader = read_rds_file,
  app_dir = app_dir
)
get_sc3def <- sc_lazy_loader(
  c("sc3def_slim.rds", "sc3def.rds"),
  reader = read_rds_file,
  app_dir = app_dir
)
get_sc3gene <- sc_lazy_loader(
  c("sc3gene_slim.rds", "sc3gene.rds"),
  reader = read_rds_file,
  postprocess = sc_normalize_gene_index,
  app_dir = app_dir
)
get_sc3meta <- sc_lazy_loader(
  c("sc3meta_slim.rds", "sc3meta.rds"),
  reader = read_rds_file,
  app_dir = app_dir
)
get_sc3gexpr_path <- sc_lazy_path(
  c("sc3gexpr_slim.h5", "sc3gexpr.h5"),
  app_dir = app_dir
)

get_cellchat_scores <- sc_lazy_loader(
  c(
    "cellchat_scores_slim.rds",
    "CellChat_all_stage_communication_score_LR_reverse.rds",
    "CellChat_all_stage_communication_score_LR_reverse.csv"
  ),
  reader = function(path) {
    if (grepl("\\.rds$", path, ignore.case = TRUE)) {
      read_rds_file(path)
    } else {
      read_csv_file(path)
    }
  },
  postprocess = function(df) {
    if (!("lr_pair" %in% names(df)) && "X" %in% names(df)) {
      df$lr_pair <- df$X
    }
    rownames(df) <- df$lr_pair
    df
  },
  app_dir = app_dir
)
