
library(Seurat)
library(ShinyCell)
library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
library(ggplot2) 
library(ggrepel) 
library(hdf5r) 
library(ggdendro) 
library(gridExtra) 
library(jsonlite)
library(scales)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(viridisLite)
library(patchwork)

source("app_support.R")
app_dir <- sc_set_app_dir(sc_find_app_dir())
sc_source("metadata_overrides.R")
sc_source("ra_tabs.R")

metadata_rules <- get_metadata_overrides(base_dir = sc_www_path(app_dir = app_dir))

# Lazy loader so large RDS files are only read when first needed
lazy_rds_loader <- function(paths) {
  if (length(paths) == 0) {
    stop("No paths provided to loader.", call. = FALSE)
  }
  sc_lazy_loader(
    paths,
    reader = function(path) {
      message(sprintf("Loading %s ...", basename(path)))
      readRDS(path)
    },
    app_dir = app_dir
  )
}

normalize_gene_index <- sc_normalize_gene_index

# Ensure all base renderPlot outputs inherit a transparent background so the
# surrounding dark theme container shows through instead of white gutters.
renderPlot <- function(expr, ..., bg = "transparent") {
  expr <- substitute(expr)
  env <- parent.frame()
  shiny::renderPlot(expr, ..., bg = bg, quoted = TRUE, env = env)
}

sc_is_truthy <- function(value) {
  candidate <- tolower(trimws(as.character(value)[1]))
  nzchar(candidate) && candidate %in% c("1", "true", "t", "yes", "y", "on")
}

sc_profile_enabled <- function() {
  isTRUE(getOption("sperminteractive.profile_lag", FALSE)) ||
    sc_is_truthy(Sys.getenv("SPERMINTERACTIVE_PROFILE_LAG", unset = ""))
}

sc_profile_env <- local({
  env <- new.env(parent = emptyenv())
  env$stack <- list()
  env$next_id <- 0L
  env
})

sc_profile_compact <- function(values) {
  keep <- vapply(values, function(value) !is.null(value), logical(1))
  values[keep]
}

sc_profile_current <- function() {
  stack <- sc_profile_env$stack
  if (!length(stack)) {
    return(NULL)
  }
  stack[[length(stack)]]
}

sc_profile_push <- function(prefix, plot_type, metadata = NULL) {
  sc_profile_env$next_id <- sc_profile_env$next_id + 1L
  ctx <- new.env(parent = emptyenv())
  ctx$id <- sc_profile_env$next_id
  ctx$prefix <- prefix
  ctx$plot_type <- plot_type
  metadata_values <- if (is.null(metadata)) list() else as.list(metadata)
  ctx$metadata <- sc_profile_compact(metadata_values)
  ctx$metrics <- list(
    h5_read_ms = 0,
    h5_read_calls = 0L,
    reshape_ms = 0,
    aggregation_ms = 0,
    plotting_ms = 0
  )
  ctx$started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  sc_profile_env$stack[[length(sc_profile_env$stack) + 1L]] <- ctx
  ctx
}

sc_profile_pop <- function() {
  stack <- sc_profile_env$stack
  if (!length(stack)) {
    return(invisible(NULL))
  }
  sc_profile_env$stack <- stack[-length(stack)]
  invisible(NULL)
}

sc_profile_note <- function(...) {
  ctx <- sc_profile_current()
  if (is.null(ctx)) {
    return(invisible(NULL))
  }
  updates <- sc_profile_compact(list(...))
  if (!length(updates)) {
    return(invisible(NULL))
  }
  ctx$metadata[names(updates)] <- updates
  invisible(NULL)
}

sc_profile_add_metric <- function(name, value) {
  ctx <- sc_profile_current()
  if (is.null(ctx)) {
    return(invisible(NULL))
  }
  current <- ctx$metrics[[name]]
  if (is.null(current) || !is.numeric(current)) {
    current <- 0
  }
  ctx$metrics[[name]] <- current + value
  invisible(NULL)
}

sc_profile_increment_metric <- function(name, amount = 1L) {
  sc_profile_add_metric(name, amount)
}

sc_profile_time_block <- function(metric_name, expr) {
  expr <- substitute(expr)
  env <- parent.frame()
  if (!sc_profile_enabled() || is.null(sc_profile_current())) {
    return(eval(expr, env))
  }
  started <- proc.time()[["elapsed"]]
  on.exit(
    sc_profile_add_metric(metric_name, (proc.time()[["elapsed"]] - started) * 1000),
    add = TRUE
  )
  eval(expr, env)
}

sc_profile_finish <- function(ctx, total_ms, status = "ok", error_message = NULL) {
  if (identical(status, "silent")) {
    return(invisible(NULL))
  }

  metric_values <- ctx$metrics
  numeric_metrics <- c("h5_read_ms", "reshape_ms", "aggregation_ms", "plotting_ms")
  for (metric_name in numeric_metrics) {
    metric_values[[metric_name]] <- round(as.numeric(metric_values[[metric_name]]), 2)
  }
  metric_values[["h5_read_calls"]] <- as.integer(round(metric_values[["h5_read_calls"]]))

  payload <- c(
    list(
      timestamp = ctx$started_at,
      dataset_prefix = ctx$prefix,
      plot_type = ctx$plot_type,
      profile_id = ctx$id,
      status = status,
      total_ms = round(total_ms, 2)
    ),
    ctx$metadata,
    metric_values
  )
  if (!is.null(error_message)) {
    payload$error <- error_message
  }

  message(
    "[lag_profile] ",
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = NA)
  )
  invisible(NULL)
}

sc_profile_eval <- function(prefix, plot_type, expr, metadata = NULL) {
  expr <- substitute(expr)
  env <- parent.frame()
  if (!sc_profile_enabled()) {
    return(eval(expr, env))
  }

  ctx <- sc_profile_push(prefix = prefix, plot_type = plot_type, metadata = metadata)
  started <- proc.time()[["elapsed"]]
  status <- "ok"
  error_message <- NULL

  on.exit({
    total_ms <- (proc.time()[["elapsed"]] - started) * 1000
    sc_profile_pop()
    sc_profile_finish(ctx, total_ms = total_ms, status = status, error_message = error_message)
  }, add = TRUE)

  tryCatch(
    eval(expr, env),
    shiny.silent.error = function(err) {
      status <<- "silent"
      stop(err)
    },
    error = function(err) {
      status <<- "error"
      error_message <<- conditionMessage(err)
      stop(err)
    }
  )
}

options(shiny.maxRequestSize = 5 * 1024^3) # allow uploads up to ~5 GB

theme_default <- "lightblue"

# === Load CCC heatmap data (Figure 6D) ===
communication_score <- read.csv(sc_app_path("CellChat_all_stage_communication_score_LR_reverse.csv", app_dir = app_dir))

# IMPORTANT: On shinyapps.io the container has a strict RAM limit. Loading the
# full Seurat object at runtime OOMs the process. Interactive figures now use
# precomputed, minimal assets generated by build_interactive_assets.R.
get_spg_avg_expr <- lazy_rds_loader(c("spg_avg_expr_by_button.rds"))
get_ra_dot_avg <- lazy_rds_loader(c("ra_dot_avg_expr.rds"))
get_ra_dot_pct <- lazy_rds_loader(c("ra_dot_pct_expr.rds"))
get_ra_line_mean <- lazy_rds_loader(c("ra_line_mean_expr.rds"))
get_interactive_genes <- lazy_rds_loader(c("interactive_genes.rds"))

# Developer-only parity check between precomputed RA line assets and a direct
# summary from specificCellID object. This is never called in normal runtime.
dev_check_ra_line <- function(
  genes = c("Stra8", "Stra6", "Aldh1a1", "Aldh1a2", "Aldh1a3"),
  enable = getOption("dev_mode_ra_check", FALSE),
  specific_paths = c("specificCellID_slim.rds", "specificCellID.rds")
) {
  if (!isTRUE(enable)) {
    return(invisible(NULL))
  }

  line_mean <- get_ra_line_mean()
  keep_genes <- intersect(genes, dimnames(line_mean)[[1]])
  if (!length(keep_genes)) {
    message("[dev_check_ra_line] No requested genes found in ra_line_mean_expr.rds")
    return(invisible(NULL))
  }

  asset_df <- as.data.frame(as.table(line_mean[keep_genes, , , drop = FALSE]))
  colnames(asset_df) <- c("gene", "sample", "generalCellID", "mean_z_asset")
  asset_df$generalCellID <- as.character(asset_df$generalCellID)
  asset_df$generalCellID[asset_df$generalCellID == "Somatic"] <- "Sertoli"
  asset_df <- asset_df[, c("gene", "sample", "generalCellID", "mean_z_asset"), drop = FALSE]

  specific_path <- specific_paths[file.exists(specific_paths)][1]
  if (is.na(specific_path) || !nzchar(specific_path)) {
    message("[dev_check_ra_line] specificCellID .rds not found; skipping direct comparison")
    return(invisible(asset_df))
  }

  obj <- readRDS(specific_path)
  meta <- obj@meta.data
  required_cols <- c("sample", "generalCellID")
  missing_cols <- setdiff(required_cols, colnames(meta))
  if (length(missing_cols)) {
    message(sprintf(
      "[dev_check_ra_line] Missing meta columns in %s: %s",
      basename(specific_path), paste(missing_cols, collapse = ", ")
    ))
    return(invisible(asset_df))
  }

  fetch_vars <- unique(c(keep_genes, "sample", "generalCellID"))
  direct <- Seurat::FetchData(obj, vars = fetch_vars)
  direct$cell <- rownames(direct)
  direct$sample <- as.character(direct$sample)
  direct$generalCellID <- as.character(direct$generalCellID)
  direct$generalCellID[direct$generalCellID == "Somatic"] <- "Sertoli"

  long_direct <- do.call(
    rbind,
    lapply(keep_genes, function(gene_name) {
      data.frame(
        gene = gene_name,
        sample = direct$sample,
        generalCellID = direct$generalCellID,
        value = as.numeric(direct[[gene_name]]),
        stringsAsFactors = FALSE
      )
    })
  )

  direct_df <- long_direct %>%
    dplyr::group_by(gene, sample, generalCellID) %>%
    dplyr::summarise(
      mean_z_direct = mean(value, na.rm = TRUE),
      n_cells = dplyr::n(),
      .groups = "drop"
    )

  cmp <- dplyr::full_join(
    direct_df,
    asset_df,
    by = c("gene", "sample", "generalCellID")
  ) %>%
    dplyr::mutate(abs_diff = abs(mean_z_direct - mean_z_asset))

  cmp$abs_diff[is.na(cmp$abs_diff)] <- Inf
  max_diff <- cmp %>%
    dplyr::group_by(gene, generalCellID, sample) %>%
    dplyr::summarise(
      max_abs_diff = max(abs_diff),
      n_cells = ifelse(all(is.na(n_cells)), NA_integer_, max(n_cells, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(max_abs_diff), gene, generalCellID, sample)

  message("[dev_check_ra_line] Max absolute difference by (gene, cell type, stage):")
  print(max_diff)
  message(sprintf("[dev_check_ra_line] overall max abs diff: %.6f", max(max_diff$max_abs_diff, na.rm = TRUE)))
  invisible(cmp)
}

sc_source("button_mapping_general.R", app_dir = app_dir)

make_lazy_data <- function(name, path, postprocess = identity, env = parent.frame()) {
  target_env <- env
  makeActiveBinding(
    sym = name,
    fun = local({
      cache <- NULL
      function(value) {
        if (!missing(value)) {
          stop(sprintf("%s is read-only", name), call. = FALSE)
        }
        if (is.null(cache)) {
          resolved_path <- sc_first_existing(c(path), app_dir = app_dir)
          if (!file.exists(resolved_path)) {
            stop(sprintf("Required file '%s' was not found.", path), call. = FALSE)
          }
          cache <<- postprocess(readRDS(resolved_path))
        }
        cache
      }
    }),
    env = target_env
  )
}

make_lazy_data("sc3conf", "sc3conf.rds", function(obj) apply_metadata_overrides_to_conf(obj, rules = metadata_rules))
make_lazy_data("sc3def",  "sc3def.rds", function(obj) apply_metadata_overrides_to_def(obj, sc3conf, rules = metadata_rules))
make_lazy_data("sc3gene", "sc3gene.rds", normalize_gene_index)
make_lazy_data("sc3meta", "sc3meta.rds")

make_lazy_data("sc4conf", "sc4conf.rds", function(obj) apply_metadata_overrides_to_conf(obj, rules = metadata_rules))
make_lazy_data("sc4def",  "sc4def.rds", function(obj) apply_metadata_overrides_to_def(obj, sc4conf, rules = metadata_rules))
make_lazy_data("sc4gene", "sc4gene.rds", normalize_gene_index)
make_lazy_data("sc4meta", "sc4meta.rds")

make_lazy_data("sc5conf", "sc5conf.rds", function(obj) apply_metadata_overrides_to_conf(obj, rules = metadata_rules))
make_lazy_data("sc5def",  "sc5def.rds", function(obj) apply_metadata_overrides_to_def(obj, sc5conf, rules = metadata_rules))
make_lazy_data("sc5gene", "sc5gene.rds", normalize_gene_index)
make_lazy_data("sc5meta", "sc5meta.rds")

make_lazy_data("sc6conf", "sc6conf.rds", function(obj) apply_metadata_overrides_to_conf(obj, rules = metadata_rules))
make_lazy_data("sc6def",  "sc6def.rds", function(obj) apply_metadata_overrides_to_def(obj, sc6conf, rules = metadata_rules))
make_lazy_data("sc6gene", "sc6gene.rds", normalize_gene_index)
make_lazy_data("sc6meta", "sc6meta.rds")

make_lazy_data("sc7conf", "sc7conf.rds", function(obj) apply_metadata_overrides_to_conf(obj, rules = metadata_rules))
make_lazy_data("sc7def",  "sc7def.rds", function(obj) apply_metadata_overrides_to_def(obj, sc7conf, rules = metadata_rules))
make_lazy_data("sc7gene", "sc7gene.rds", normalize_gene_index)
make_lazy_data("sc7meta", "sc7meta.rds")

h5_dataset_reader <- local({
  cache <- new.env(parent = emptyenv())
  function(path, cache_ok = TRUE) {
    resolved_path <- sc_first_existing(c(path), app_dir = app_dir)
    if (!nzchar(resolved_path) || !file.exists(resolved_path)) {
      stop(sprintf("Expression matrix '%s' not found.", path), call. = FALSE)
    }
    if (isTRUE(cache_ok)) {
      entry <- cache[[resolved_path]]
      if (is.null(entry) || !entry$file$is_valid) {
        file <- H5File$new(resolved_path, mode = "r")
        cache[[resolved_path]] <- list(file = file, dataset = file[["grp"]][["data"]])
      }
      return(cache[[resolved_path]]$dataset)
    }
    NULL
  }
})

read_h5_gene <- function(path, gene_idx) {
  resolved_path <- sc_first_existing(c(path), app_dir = app_dir)
  cache_ok <- !grepl("^usr", basename(resolved_path))
  sc_profile_note(expression_file = basename(resolved_path))
  if (isTRUE(cache_ok)) {
    dataset <- h5_dataset_reader(resolved_path, cache_ok = TRUE)
    return(sc_profile_time_block("h5_read_ms", {
      sc_profile_increment_metric("h5_read_calls")
      dataset$read(args = list(gene_idx, quote(expr = )))
    }))
  }
  file <- H5File$new(resolved_path, mode = "r")
  on.exit(try(file$close_all(), silent = TRUE))
  dataset <- file[["grp"]][["data"]]
  sc_profile_time_block("h5_read_ms", {
    sc_profile_increment_metric("h5_read_calls")
    dataset$read(args = list(gene_idx, quote(expr = )))
  })
}

get_gene_index <- function(inpGene, gene_name) {
  gene_name <- as.character(gene_name)
  if (length(gene_name) == 0 || !nzchar(gene_name[1])) {
    stop("No gene selected. Please choose a gene from the dropdown list.", call. = FALSE)
  }
  idx <- inpGene[gene_name[1]]
  if (length(idx) == 0 || is.na(idx)) {
    stop(sprintf("Gene '%s' is not present in the current dataset. Select a gene from the suggestions.", gene_name[1]), call. = FALSE)
  }
  as.integer(idx)
}

multiple_geneexpr_default_genes <- list(
  sc3 = c("Zbtb16", "Meiob", "Acrv1", "Ddx4", "Gata1"),
  sc4 = c("Zbtb16", "Meiob", "Acrv1", "Ddx4", "Gata1"),
  sc5 = c("Zbtb16", "Meiob", "Acrv1", "Ddx4", "Gata1"),
  sc6 = c("Zbtb16", "Meiob", "Acrv1", "Ddx4", "Gata1"),
  sc7 = c("Zbtb16", "Meiob", "Acrv1", "Ddx4", "Gata1")
)

default_multiple_geneexpr_genes <- function(prefix, inpGene, fallback = NULL) {
  gene_names <- names(inpGene)
  defaults <- multiple_geneexpr_default_genes[[prefix]]
  defaults <- defaults[defaults %in% gene_names]
  if (length(defaults)) {
    return(defaults)
  }

  fallback <- as.character(fallback)
  fallback <- fallback[!is.na(fallback) & nzchar(fallback) & fallback %in% gene_names]
  if (length(fallback)) {
    return(fallback[[1]])
  }

  gene_names[seq_len(min(5, length(gene_names)))]
}



active_button <- reactiveVal(NULL)

ra_tab_ids <- c("spermatogonia_table", "retinoic_acid")


### Useful stuff 
# Colour palette 
cList = list(c("grey85","#FFF7EC","#FEE8C8","#FDD49E","#FDBB84", 
               "#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"), 
             c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF", 
               "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)], 
             c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C", 
               "#2C728E","#3B528B","#472D7B","#440154")) 
names(cList) = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple") 
 
# Panel sizes 
pList = c("400px", "600px", "800px") 
names(pList) = c("Small", "Medium", "Large") 
pList2 = c("500px", "700px", "900px") 
names(pList2) = c("Small", "Medium", "Large") 
pList3 = c("600px", "800px", "1000px") 
names(pList3) = c("Small", "Medium", "Large") 
sList = c(18,24,30) 
names(sList) = c("Small", "Medium", "Large") 
lList = c(5,6,7) 
names(lList) = c("Small", "Medium", "Large") 
 
# Function to extract legend 
g_legend <- function(a.gplot){  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")  
  legend <- tmp$grobs[[leg]]  
  legend 
}  
 
# Plot theme 
sctheme <- function(base_size = 24, XYval = TRUE, Xang = 0, XjusH = 0.5, dark = FALSE){ 
  panel_bg <- if (dark) "#050815" else "white"
  plot_bg  <- panel_bg
  text_col <- if (dark) "#e2e8f0" else "black"
  axis_col <- if (dark) "#94a3b8" else "black"
  grid_major <- if (dark) "#1f2937" else "grey92"
  grid_minor <- if (dark) "#111827" else "grey98"
  strip_bg <- if (dark) "#0f172a" else "grey90"
  legend_bg <- panel_bg

  oupTheme = theme( 
    text =             element_text(size = base_size, family = "Helvetica", color = text_col), 
    panel.background = element_rect(fill = panel_bg, colour = NA), 
    plot.background  = element_rect(fill = plot_bg, colour = NA),
    axis.line =   element_line(colour = axis_col), 
    axis.ticks =  element_line(colour = axis_col, size = base_size / 20), 
    axis.title =  element_text(face = "bold", color = text_col), 
    axis.text =   element_text(size = base_size, color = text_col), 
    axis.text.x = element_text(angle = Xang, hjust = XjusH, color = text_col), 
    legend.position = "bottom", 
    legend.key =      element_rect(colour = NA, fill = legend_bg),
    legend.background = element_rect(fill = legend_bg, colour = NA),
    legend.text = element_text(color = text_col),
    legend.title = element_text(color = text_col),
    strip.background = element_rect(fill = strip_bg, colour = NA),
    strip.text = element_text(color = text_col),
    panel.grid.major = element_line(color = grid_major, size = 0.4),
    panel.grid.minor = element_line(color = grid_minor, size = 0.2)
  ) 
  if(!XYval){ 
    oupTheme = oupTheme + theme( 
      axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
      axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
  } 
  return(oupTheme) 
} 
 
### Common plotting functions 
# Detect when a DR plot is using the UMAP embedding
is_umap_view <- function(inpConf, inpdrX, inpdrY) {
  dr_ids <- inpConf[UI %in% c(inpdrX, inpdrY)]$ID
  dr_ids <- tolower(dr_ids)
  length(dr_ids) == 2 && setequal(dr_ids, c("umap_1", "umap_2"))
}

first_valid_ui <- function(inpConf) {
  ui_vals <- as.character(inpConf$UI)
  ui_vals <- ui_vals[!is.na(ui_vals) & nzchar(ui_vals)]
  if (length(ui_vals)) {
    return(ui_vals[[1]])
  }
  NA_character_
}

# Plot cell information on dimred 
scDRcell <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt, inplab,
                     dark_theme = FALSE,
                     stage_split = FALSE,
                     stage_facet_ncol = 2){ 
  if(is.null(inpsub1)){inpsub1 = first_valid_ui(inpConf)} 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, 
                       inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),  
                   with = FALSE] 
  colnames(ggData) = c("X", "Y", "val", "sub") 
  stage_split_active <- isTRUE(stage_split)
  if (stage_split_active) {
    stage_vals <- inpMeta[["sample"]]
    if (!is.factor(stage_vals)) {
      stage_vals <- factor(stage_vals)
    }
    ggData$stage <- stage_vals
  }
  rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y)) 
  bgCells = FALSE 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    bgCells = TRUE 
    ggData2 = ggData[!sub %in% inpsub2] 
    ggData = ggData[sub %in% inpsub2] 
  } 
  if(inpord == "Max-1st"){ 
    ggData = ggData[order(val)] 
  } else if(inpord == "Min-1st"){ 
    ggData = ggData[order(-val)] 
  } else if(inpord == "Random"){ 
    ggData = ggData[sample(nrow(ggData))] 
  } 
  
  # Do factoring if required 
  if(!is.na(inpConf[UI == inp1]$fCL)){ 
    ggCol = strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]] 
    names(ggCol) = levels(ggData$val) 
    ggLvl = levels(ggData$val)[levels(ggData$val) %in% unique(ggData$val)] 
    ggData$val = factor(ggData$val, levels = ggLvl) 
    ggCol = ggCol[ggLvl] 
  } 
 
  # Actual ggplot 
  ggOut = ggplot(ggData, aes(X, Y, color = val)) 
  bg_point_col <- if (dark_theme) "#0f172a" else "snow2"
  if(bgCells){ 
    ggOut = ggOut + 
      geom_point(data = ggData2, color = bg_point_col, size = inpsiz, shape = 16) 
  } 
  ggOut = ggOut + 
    geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) + 
    sctheme(base_size = sList[inpfsz], XYval = inptxt, dark = dark_theme) +
    theme(legend.position = "right", legend.direction = "vertical", legend.box = "vertical")
  if(is.na(inpConf[UI == inp1]$fCL)){ 
    ggOut = ggOut + scale_color_gradientn("", colours = cList[[inpcol]]) + 
      guides(color = guide_colorbar(barheight = grid::unit(4.0, "cm"),
                                    barwidth = grid::unit(0.45, "cm"))) 
  } else { 
    sListX = min(nchar(paste0(levels(ggData$val), collapse = "")), 200) 
    sListX = 0.75 * (sList - (1.5 * floor(sListX/50))) 
    ggOut = ggOut + scale_color_manual("", values = ggCol) + 
      guides(color = guide_legend(override.aes = list(size = 5),  
                                  ncol = 1, byrow = FALSE)) + 
      theme(legend.text = element_text(size = sListX[inpfsz])) 
    if(inplab){ 
      if (stage_split_active) {
        ggData3 = ggData[, .(X = mean(X), Y = mean(Y)), by = c("stage", "val")]
      } else {
        ggData3 = ggData[, .(X = mean(X), Y = mean(Y)), by = "val"]
      }
      lListX = min(nchar(paste0(ggData3$val, collapse = "")), 200) 
      lListX = lList - (0.25 * floor(lListX/50)) 
      label_text_col <- if (dark_theme) "#f8fafc" else "grey10"
      label_bg_col <- if (dark_theme) "#0f172a" else "grey95"
      ggOut = ggOut + 
        geom_text_repel(data = ggData3, aes(X, Y, label = val), 
                        color = label_text_col, bg.color = label_bg_col, bg.r = 0.15, 
                        size = lListX[inpfsz], seed = 42) 
    } 
  } 
  if(inpasp == "Square") { 
    ggOut = ggOut + coord_fixed(ratio = rat) 
  } else if(inpasp == "Fixed") { 
    ggOut = ggOut + coord_fixed() 
  } 
  if (stage_split_active) {
    ncol <- suppressWarnings(as.integer(stage_facet_ncol))
    if (is.na(ncol) || ncol < 1) {
      ncol <- 2
    }
    ggOut = ggOut + facet_wrap(~stage, ncol = ncol)
  }
  return(ggOut) 
} 
 
scDRnum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, 
                    inpH5, inpGene, inpsplt){ 
  if(is.null(inpsub1)){inpsub1 = first_valid_ui(inpConf)} 
  if (is.null(inpsplt) || !nzchar(inpsplt)) {
    inpsplt <- "Decile"
  }
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID), 
                   with = FALSE] 
  colnames(ggData) = c("group", "sub") 
  gene_idx <- get_gene_index(inpGene, inp2)
  ggData$val2 = read_h5_gene(inpH5, gene_idx)
  ggData[val2 < 0]$val2 = 0 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Split inp1 if necessary 
  if(is.na(inpConf[UI == inp1]$fCL)){ 
    if(inpsplt == "Quartile"){nBk = 4} 
    if(inpsplt == "Decile"){nBk = 10} 
    ggData$group = cut(ggData$group, breaks = nBk) 
  } 
  
  # Actual data.table 
  ggData$express = FALSE 
  ggData[val2 > 0]$express = TRUE 
  ggData1 = ggData[express == TRUE, .(nExpress = .N), by = "group"] 
  ggData = ggData[, .(nCells = .N), by = "group"] 
  ggData = ggData1[ggData, on = "group"] 
  ggData = ggData[, c("group", "nCells", "nExpress"), with = FALSE] 
  ggData[is.na(nExpress)]$nExpress = 0 
  ggData$pctExpress = 100 * ggData$nExpress / ggData$nCells 
  ggData = ggData[order(group)] 
  colnames(ggData)[3] = paste0(colnames(ggData)[3], "_", inp2) 
  return(ggData) 
} 
# Plot gene expression on dimred 
scDRgene <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2, 
                     inpH5, inpGene, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt,
                     dark_theme = FALSE,
                     stage_split = FALSE){ 
  if(is.null(inpsub1)){inpsub1 = first_valid_ui(inpConf)} 
  sc_profile_note(
    selected_genes = as.character(inp1),
    gene_count = 1L,
    stage_split = isTRUE(stage_split)
  )
  # Prepare ggData 
  ggData = sc_profile_time_block("reshape_ms", {
    ggData_local = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
                               inpConf[UI == inpsub1]$ID),
                           with = FALSE]
    colnames(ggData_local) = c("X", "Y", "sub")
    stage_split_active <- isTRUE(stage_split)
    if (stage_split_active) {
      stage_vals <- inpMeta[["sample"]]
      if (!is.factor(stage_vals)) {
        stage_vals <- factor(stage_vals)
      }
      ggData_local$stage <- stage_vals
    }
    ggData_local
  })
  stage_split_active <- isTRUE(stage_split)
  rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y)) 
 
  gene_idx <- get_gene_index(inpGene, inp1)
  ggData$val = read_h5_gene(inpH5, gene_idx)
  reshape_values <- sc_profile_time_block("reshape_ms", {
    ggData[val < 0]$val = 0
    bgCells_local = FALSE
    ggData2_local = NULL
    if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
      bgCells_local = TRUE
      ggData2_local = ggData[!sub %in% inpsub2]
      ggData = ggData[sub %in% inpsub2]
    }
    if(inpord == "Max-1st"){
      ggData = ggData[order(val)]
    } else if(inpord == "Min-1st"){
      ggData = ggData[order(-val)]
    } else if(inpord == "Random"){
      ggData = ggData[sample(nrow(ggData))]
    }
    list(ggData = ggData, ggData2 = ggData2_local, bgCells = bgCells_local)
  })
  ggData <- reshape_values$ggData
  ggData2 <- reshape_values$ggData2
  bgCells <- reshape_values$bgCells
  
  # Actual ggplot 
  ggOut = sc_profile_time_block("plotting_ms", {
    ggOut = ggplot(ggData, aes(X, Y, color = val))
    bg_point_col <- if (dark_theme) "#0f172a" else "snow2"
    if(bgCells){
      ggOut = ggOut +
        geom_point(data = ggData2, color = bg_point_col, size = inpsiz, shape = 16)
    }
    ggOut = ggOut +
      geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) +
      sctheme(base_size = sList[inpfsz], XYval = inptxt, dark = dark_theme) +
      scale_color_gradientn(inp1, colours = cList[[inpcol]]) +
      guides(color = guide_colorbar(barheight = grid::unit(4.0, "cm"),
                                    barwidth = grid::unit(0.45, "cm"))) +
      theme(legend.position = "right", legend.direction = "vertical", legend.box = "vertical")
    if(inpasp == "Square") {
      ggOut = ggOut + coord_fixed(ratio = rat)
    } else if(inpasp == "Fixed") {
      ggOut = ggOut + coord_fixed()
    }
    if (stage_split_active) {
      ggOut = ggOut + facet_wrap(~stage, ncol = 2)
    }
    ggOut
  })
  return(ggOut) 
} 
 
# Plot gene coexpression on dimred 
bilinear <- function(x,y,xy,Q11,Q21,Q12,Q22){ 
  oup = (xy-x)*(xy-y)*Q11 + x*(xy-y)*Q21 + (xy-x)*y*Q12 + x*y*Q22 
  oup = oup / (xy*xy) 
  return(oup) 
} 
scDRcoex <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inp2, 
                     inpsub1, inpsub2, inpH5, inpGene, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt,
                     stage_split = FALSE,
                     dark_theme = FALSE){ 
  if(is.null(inpsub1)){inpsub1 = first_valid_ui(inpConf)} 
  sc_profile_note(
    selected_genes = as.character(c(inp1, inp2)),
    gene_count = 2L,
    stage_split = isTRUE(stage_split)
  )
  # Prepare ggData 
  ggData = sc_profile_time_block("reshape_ms", {
    ggData_local = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
                               inpConf[UI == inpsub1]$ID),
                           with = FALSE]
    colnames(ggData_local) = c("X", "Y", "sub")
    stage_split_active <- isTRUE(stage_split)
    if (stage_split_active) {
      stage_vals <- inpMeta[["sample"]]
      if (!is.factor(stage_vals)) {
        stage_vals <- factor(stage_vals)
      }
      ggData_local$stage <- stage_vals
    }
    ggData_local
  })
  stage_split_active <- isTRUE(stage_split)
  rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y)) 
 
  gene_idx1 <- get_gene_index(inpGene, inp1)
  ggData$val1 = read_h5_gene(inpH5, gene_idx1)
  gene_idx2 <- get_gene_index(inpGene, inp2)
  ggData$val2 = read_h5_gene(inpH5, gene_idx2)
  reshape_values <- sc_profile_time_block("reshape_ms", {
    ggData[val1 < 0]$val1 = 0
    ggData[val2 < 0]$val2 = 0
    bgCells_local = FALSE
    ggData2_local = NULL
    if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
      bgCells_local = TRUE
      ggData2_local = ggData[!sub %in% inpsub2]
      ggData = ggData[sub %in% inpsub2]
    }

    cInp = strsplit(inpcol, "; ")[[1]]
    if(cInp[1] == "Red (Gene1)"){
      c10 = c(255,0,0)
    } else if(cInp[1] == "Orange (Gene1)"){
      c10 = c(255,140,0)
    } else {
      c10 = c(0,255,0)
    }
    if(cInp[2] == "Green (Gene2)"){
      c01 = c(0,255,0)
    } else {
      c01 = c(0,0,255)
    }
    c00 = c(217,217,217) ; c11 = c10 + c01
    nGrid = 16; nPad = 2; nTot = nGrid + nPad * 2
    gg = data.table(v1 = rep(0:nTot,nTot+1), v2 = sort(rep(0:nTot,nTot+1)))
    gg$vv1 = gg$v1 - nPad ; gg[vv1 < 0]$vv1 = 0; gg[vv1 > nGrid]$vv1 = nGrid
    gg$vv2 = gg$v2 - nPad ; gg[vv2 < 0]$vv2 = 0; gg[vv2 > nGrid]$vv2 = nGrid
    gg$cR = bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1])
    gg$cG = bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2])
    gg$cB = bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3])
    gg$cMix = rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255)
    gg = gg[, c("v1", "v2", "cMix")]

    ggData$v1 = round(nTot * ggData$val1 / max(ggData$val1))
    ggData$v2 = round(nTot * ggData$val2 / max(ggData$val2))
    ggData$v0 = ggData$v1 + ggData$v2
    ggData = gg[ggData, on = c("v1", "v2")]
    if(inpord == "Max-1st"){
      ggData = ggData[order(v0)]
    } else if(inpord == "Min-1st"){
      ggData = ggData[order(-v0)]
    } else if(inpord == "Random"){
      ggData = ggData[sample(nrow(ggData))]
    }

    list(ggData = ggData, ggData2 = ggData2_local, bgCells = bgCells_local)
  })
  ggData <- reshape_values$ggData
  ggData2 <- reshape_values$ggData2
  bgCells <- reshape_values$bgCells
  
  # Actual ggplot 
  ggOut = sc_profile_time_block("plotting_ms", {
    ggOut = ggplot(ggData, aes(X, Y))
    bg_point_col <- if (dark_theme) "#0f172a" else "snow2"
    if(bgCells){
      ggOut = ggOut +
        geom_point(data = ggData2, color = bg_point_col, size = inpsiz, shape = 16)
    }
    ggOut = ggOut +
      geom_point(size = inpsiz, shape = 16, color = ggData$cMix) +
      xlab(inpdrX) + ylab(inpdrY) +
      sctheme(base_size = sList[inpfsz], XYval = inptxt, dark = dark_theme) +
      scale_color_gradientn(inp1, colours = cList[[1]]) +
      guides(color = guide_colorbar(barwidth = 15))
    if(inpasp == "Square") {
      ggOut = ggOut + coord_fixed(ratio = rat)
    } else if(inpasp == "Fixed") {
      ggOut = ggOut + coord_fixed()
    }
    if (stage_split_active) {
      ggOut = ggOut + facet_wrap(~stage, ncol = 2)
    }
    ggOut
  })
  return(ggOut) 
} 
 
scDRcoexLeg <- function(inp1, inp2, inpcol, inpfsz, dark_theme = FALSE){ 
  # Generate coex color palette 
  cInp = strsplit(inpcol, "; ")[[1]] 
  if(cInp[1] == "Red (Gene1)"){ 
    c10 = c(255,0,0) 
  } else if(cInp[1] == "Orange (Gene1)"){ 
    c10 = c(255,140,0) 
  } else { 
    c10 = c(0,255,0) 
  } 
  if(cInp[2] == "Green (Gene2)"){ 
    c01 = c(0,255,0) 
  } else { 
    c01 = c(0,0,255) 
  } 
  c00 = c(217,217,217) ; c11 = c10 + c01 
  nGrid = 16; nPad = 2; nTot = nGrid + nPad * 2 
  gg = data.table(v1 = rep(0:nTot,nTot+1), v2 = sort(rep(0:nTot,nTot+1))) 
  gg$vv1 = gg$v1 - nPad ; gg[vv1 < 0]$vv1 = 0; gg[vv1 > nGrid]$vv1 = nGrid 
  gg$vv2 = gg$v2 - nPad ; gg[vv2 < 0]$vv2 = 0; gg[vv2 > nGrid]$vv2 = nGrid 
  gg$cR = bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1]) 
  gg$cG = bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2]) 
  gg$cB = bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3]) 
  gg$cMix = rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255) 
  gg = gg[, c("v1", "v2", "cMix")] 
  
  # Actual ggplot 
  ggOut = ggplot(gg, aes(v1, v2)) + 
    geom_tile(fill = gg$cMix) + 
    xlab(inp1) + ylab(inp2) + coord_fixed(ratio = 1) + 
    scale_x_continuous(breaks = c(0, nTot), label = c("low", "high")) + 
    scale_y_continuous(breaks = c(0, nTot), label = c("low", "high")) + 
    sctheme(base_size = sList[inpfsz], XYval = TRUE, dark = dark_theme) 
  return(ggOut) 
} 
 
scDRcoexNum <- function(inpConf, inpMeta, inp1, inp2, 
                        inpsub1, inpsub2, inpH5, inpGene){ 
  if(is.null(inpsub1)){inpsub1 = first_valid_ui(inpConf)} 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpsub1]$ID), with = FALSE] 
  colnames(ggData) = c("sub") 
  gene_idx1 <- get_gene_index(inpGene, inp1)
  ggData$val1 = read_h5_gene(inpH5, gene_idx1)
  ggData[val1 < 0]$val1 = 0 
  gene_idx2 <- get_gene_index(inpGene, inp2)
  ggData$val2 = read_h5_gene(inpH5, gene_idx2)
  ggData[val2 < 0]$val2 = 0 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData = ggData[sub %in% inpsub2] 
  } 
 
  # Actual data.table 
  ggData$express = "none" 
  ggData[val1 > 0]$express = inp1 
  ggData[val2 > 0]$express = inp2 
  ggData[val1 > 0 & val2 > 0]$express = "both" 
  ggData$express = factor(ggData$express, levels = unique(c("both", inp1, inp2, "none"))) 
  ggData = ggData[, .(nCells = .N), by = "express"] 
  ggData$percent = 100 * ggData$nCells / sum(ggData$nCells) 
  ggData = ggData[order(express)] 
  colnames(ggData)[1] = "expression > 0" 
  return(ggData) 
} 
 
# Plot violin / boxplot 
scVioBox <- function(inpConf, inpMeta, inp1, inp2,
                     inpsub1, inpsub2, inpH5, inpGene,
                     inptyp, inppts, inpsiz, inpfsz, show_legend = TRUE, dark_theme = FALSE){
  if(is.null(inpsub1)){inpsub1 = first_valid_ui(inpConf)} 
  sc_profile_note(
    selected_x = as.character(inp1),
    selected_feature = as.character(inp2),
    feature_kind = if (inp2 %in% inpConf$UI) "metadata" else "gene",
    gene_count = if (inp2 %in% inpConf$UI) 0L else 1L,
    selected_genes = if (inp2 %in% inpConf$UI) NULL else as.character(inp2)
  )
  # Prepare ggData 
  ggData = sc_profile_time_block("reshape_ms", {
    ggData_local = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),
                           with = FALSE]
    colnames(ggData_local) = c("X", "sub")
    ggData_local
  })
  
  # Load in either cell meta or gene expr
  if(inp2 %in% inpConf$UI){ 
    ggData = sc_profile_time_block("reshape_ms", {
      ggData$val = inpMeta[[inpConf[UI == inp2]$ID]]
      ggData
    })
  } else { 
    gene_idx <- get_gene_index(inpGene, inp2)
    ggData$val = read_h5_gene(inpH5, gene_idx)
    ggData = sc_profile_time_block("reshape_ms", {
      ggData[val < 0]$val = 0
      set.seed(42)
      tmpNoise = rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000
      ggData$val = ggData$val + tmpNoise
      ggData
    })
  } 
  ggData = sc_profile_time_block("reshape_ms", {
    if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
      ggData = ggData[sub %in% inpsub2]
    }
    ggData
  })
  
  # Do factoring 
  factor_values = sc_profile_time_block("aggregation_ms", {
    ggCol_local = strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]]
    names(ggCol_local) = levels(ggData$X)
    ggLvl_local = levels(ggData$X)[levels(ggData$X) %in% unique(ggData$X)]
    ggData$X = factor(ggData$X, levels = ggLvl_local)
    list(ggData = ggData, ggCol = ggCol_local[ggLvl_local])
  })
  ggData <- factor_values$ggData
  ggCol <- factor_values$ggCol
  
  # Actual ggplot 
  ggOut = sc_profile_time_block("plotting_ms", {
    if(inptyp == "violin"){
      ggOut = ggplot(ggData, aes(X, val, fill = X)) + geom_violin(scale = "width")
    } else {
      ggOut = ggplot(ggData, aes(X, val, fill = X)) + geom_boxplot()
    }
    if(inppts){
      ggOut = ggOut + geom_jitter(size = inpsiz, shape = 16)
    }
    ggOut = ggOut + xlab(inp1) + ylab(inp2) +
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1, dark = dark_theme) +
      scale_fill_manual("", values = ggCol) +
      theme(legend.position = if (isTRUE(show_legend)) "right" else "none")
    ggOut
  })
  return(ggOut) 
} 
 
# Plot proportion plot 
scProp <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, 
                   inptyp, inpflp, inpfsz, dark_theme = FALSE){ 
  if(is.null(inpsub1)){inpsub1 = first_valid_ui(inpConf)} 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inp2]$ID, 
                       inpConf[UI == inpsub1]$ID),  
                   with = FALSE] 
  colnames(ggData) = c("X", "grp", "sub") 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData = ggData[sub %in% inpsub2] 
  } 
  ggData = ggData[, .(nCells = .N), by = c("X", "grp")] 
  ggData = ggData[, {tot = sum(nCells) 
                      .SD[,.(pctCells = 100 * sum(nCells) / tot, 
                             nCells = nCells), by = "grp"]}, by = "X"] 
  
  # Do factoring 
  ggCol = strsplit(inpConf[UI == inp2]$fCL, "\\|")[[1]] 
  names(ggCol) = levels(ggData$grp) 
  ggLvl = levels(ggData$grp)[levels(ggData$grp) %in% unique(ggData$grp)] 
  ggData$grp = factor(ggData$grp, levels = ggLvl) 
  ggCol = ggCol[ggLvl] 
  
  # Actual ggplot 
  if(inptyp == "Proportion"){ 
    ggOut = ggplot(ggData, aes(X, pctCells, fill = grp)) + 
      geom_col() + ylab("Cell Proportion (%)") 
  } else { 
    ggOut = ggplot(ggData, aes(X, nCells, fill = grp)) + 
      geom_col() + ylab("Number of Cells") 
  } 
  if(inpflp){ 
    ggOut = ggOut + coord_flip() 
  } 
  ggOut = ggOut + xlab(inp1) + 
    sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1, dark = dark_theme) +  
    scale_fill_manual("", values = ggCol) + 
    theme(legend.position = "right") 
  return(ggOut) 
} 
 
# Get gene list 
scGeneList <- function(inp, inpGene){ 
  geneList = data.table(gene = unique(trimws(strsplit(inp, ",|;|
")[[1]])), 
                        present = TRUE) 
  geneList[!gene %in% names(inpGene)]$present = FALSE 
  return(geneList) 
} 

scParseGeneVector <- function(inp_values, inpGene) {
  values <- as.character(inp_values)
  values <- values[!is.na(values)]
  if (!length(values)) {
    return(list(valid = character(0), missing = character(0), input = character(0)))
  }

  parsed <- trimws(unlist(strsplit(paste(values, collapse = "\n"), ",|;|\n")))
  parsed <- parsed[!is.na(parsed) & nzchar(parsed)]
  parsed <- unique(parsed)
  if (!length(parsed)) {
    return(list(valid = character(0), missing = character(0), input = character(0)))
  }

  available <- names(inpGene)
  available <- available[!is.na(available) & nzchar(available)]
  if (!length(available)) {
    return(list(valid = character(0), missing = parsed, input = parsed))
  }

  available_lower <- tolower(available)
  valid <- character(0)
  missing <- character(0)

  for (token in parsed) {
    idx <- match(tolower(token), available_lower)
    if (is.na(idx)) {
      missing <- c(missing, token)
    } else {
      canonical <- available[[idx]]
      if (!canonical %in% valid) {
        valid <- c(valid, canonical)
      }
    }
  }

  list(valid = valid, missing = missing, input = parsed)
}

scDRnumMulti <- function(inpConf, inpMeta, inpGrp, inpGenes,
                         inpsub1, inpsub2, inpH5, inpGene) {
  if (is.null(inpsub1)) {
    inpsub1 <- first_valid_ui(inpConf)
  }

  parsed_genes <- scParseGeneVector(inpGenes, inpGene)
  gene_list <- parsed_genes$valid
  shiny::validate(need(length(gene_list) > 0, "Select at least one valid gene to plot."))

  grp_id <- inpConf[UI == inpGrp]$ID[1]
  sub_id <- inpConf[UI == inpsub1]$ID[1]
  shiny::validate(need(!is.na(grp_id) && nzchar(grp_id), "Grouping variable is unavailable for this dataset."))
  shiny::validate(need(!is.na(sub_id) && nzchar(sub_id), "Subset variable is unavailable for this dataset."))

  grp_vals <- inpMeta[[grp_id]]
  sub_vals <- inpMeta[[sub_id]]

  cell_keep <- rep(TRUE, nrow(inpMeta))
  unique_sub <- unique(as.character(sub_vals))
  unique_sub <- unique_sub[!is.na(unique_sub)]
  if (length(inpsub2) != 0 && length(inpsub2) != length(unique_sub)) {
    cell_keep <- as.character(sub_vals) %in% as.character(inpsub2)
  }

  grp_chr <- trimws(as.character(grp_vals))
  grp_ok <- !is.na(grp_chr) & nzchar(grp_chr)
  cell_keep <- cell_keep & grp_ok
  shiny::validate(need(any(cell_keep), "No cells remain after subsetting."))

  grp_levels <- if (is.factor(grp_vals)) levels(grp_vals) else unique(grp_chr[cell_keep])
  grp_levels <- grp_levels[grp_levels %in% unique(grp_chr[cell_keep])]
  if (!length(grp_levels)) {
    grp_levels <- sort(unique(grp_chr[cell_keep]))
  }
  grouped_cells <- factor(grp_chr[cell_keep], levels = grp_levels)
  n_cells_dt <- data.table(group = grouped_cells)[, .(nCells = .N), by = "group"]

  chunks <- vector("list", length(gene_list))
  for (i in seq_along(gene_list)) {
    gene_name <- gene_list[[i]]
    gene_idx <- get_gene_index(inpGene, gene_name)
    expr_vals <- read_h5_gene(inpH5, gene_idx)[cell_keep]
    expr_vals <- as.numeric(expr_vals)
    expr_vals[!is.finite(expr_vals)] <- 0
    expr_vals[expr_vals < 0] <- 0

    gene_dt <- data.table(group = grouped_cells, expr = expr_vals)
    agg <- gene_dt[, .(
      nExpress = sum(expr > 0, na.rm = TRUE),
      pctExpress = 100 * sum(expr > 0, na.rm = TRUE) / .N,
      avgExpr = mean(expm1(expr), na.rm = TRUE)
    ), by = "group"]
    agg <- n_cells_dt[agg, on = "group"]
    agg[is.na(nExpress), `:=`(nExpress = 0, pctExpress = 0, avgExpr = 0)]
    agg[, gene := gene_name]
    chunks[[i]] <- agg
  }

  dot_data <- rbindlist(chunks, use.names = TRUE)
  dot_data[, group := factor(as.character(group), levels = grp_levels)]
  dot_data[, gene := factor(gene, levels = rev(gene_list))]
  dot_data[, avgExprLog := log1p(pmax(avgExpr, 0))]
  dot_data[, avgExprScaled := if (uniqueN(avgExprLog) <= 1) 0 else as.numeric(scale(avgExprLog)), by = "gene"]
  dot_data[!is.finite(avgExprScaled), avgExprScaled := 0]
  dot_data[, avgExprScaled := pmax(pmin(avgExprScaled, 2.5), -2.5)]
  setorder(dot_data, group, gene)

  table_data <- dcast(
    dot_data[, .(group, nCells, gene, nExpress, pctExpress)],
    group + nCells ~ gene,
    value.var = c("nExpress", "pctExpress")
  )
  table_data[, group := as.character(group)]
  setorderv(table_data, "group")

  list(
    dot_data = dot_data,
    table_data = table_data,
    valid_genes = gene_list,
    missing_genes = parsed_genes$missing
  )
}

scMultiGeneDotPlot <- function(dot_data, inpfsz = "Medium", inpdotsz = 1, dark_theme = FALSE) {
  shiny::validate(need(!is.null(dot_data) && nrow(dot_data) > 0, "No data available to plot."))

  publication_rd_bu <- rev(
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(100)
  )
  axis_col <- if (dark_theme) "#e2e8f0" else "#1e293b"
  bg_col <- if (dark_theme) "#050815" else "#ffffff"
  border_col <- if (dark_theme) "#475569" else "#000000"
  point_stroke <- if (dark_theme) "#f8fafc" else "#000000"
  size_key <- as.character(inpfsz)[1]
  if (is.na(size_key) || !nzchar(size_key) || !size_key %in% c("Small", "Medium", "Large")) {
    size_key <- "Large"
  }
  base_size_map <- c(Small = 10, Medium = 12, Large = 14)
  point_max_map <- c(Small = 4.0, Medium = 6.0, Large = 8.0)
  legend_text_map <- c(Small = 8, Medium = 9, Large = 10)
  legend_title_map <- c(Small = 8.5, Medium = 9.5, Large = 10.5)
  axis_angle_map <- c(Small = 35, Medium = 40, Large = 45)
  legend_scale <- 1.5

  base_size <- base_size_map[[size_key]]
  dot_scale <- suppressWarnings(as.numeric(inpdotsz)[1])
  if (!is.finite(dot_scale) || dot_scale <= 0) {
    dot_scale <- 1
  }
  point_max <- point_max_map[[size_key]] * dot_scale
  legend_text_size <- legend_text_map[[size_key]] * legend_scale
  legend_title_size <- legend_title_map[[size_key]] * legend_scale
  axis_angle <- axis_angle_map[[size_key]]

  ggplot(dot_data, aes(x = gene, y = group)) +
    geom_point(
      aes(size = pctExpress, fill = avgExprScaled),
      shape = 21, stroke = 0.25, color = point_stroke
    ) +
    scale_fill_gradientn(
      colors = publication_rd_bu,
      values = scales::rescale(c(-2.5, 0, 2.5)),
      limits = c(-2.5, 2.5),
      oob = scales::squish
    ) +
    scale_size(range = c(0, point_max), limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = bg_col, colour = NA),
      plot.background = element_rect(fill = bg_col, colour = NA),
      panel.grid = element_blank(),
      axis.text.x = element_text(color = axis_col, angle = axis_angle, hjust = 1, vjust = 1),
      axis.text.y = element_text(color = axis_col),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_rect(color = border_col, fill = NA, size = 1),
      axis.ticks = element_line(color = border_col, size = 0.5),
      axis.ticks.length = unit(0.25, "cm"),
      legend.position = "right",
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.key.height = unit(0.28 * legend_scale, "cm"),
      legend.key.width = unit(0.9 * legend_scale, "cm"),
      legend.background = element_blank(),
      legend.box.background = element_rect(fill = bg_col, colour = border_col),
      legend.text = element_text(color = axis_col, size = legend_text_size),
      legend.title = element_text(color = axis_col, size = legend_title_size),
      legend.margin = margin(t = 3, r = 4, b = 3, l = 4),
      legend.spacing.y = unit(0.10 * legend_scale, "cm")
    ) +
    guides(
      fill = guide_colorbar(
        title = "Average Expression",
        direction = "vertical",
        barheight = unit(2.6 * legend_scale, "cm"),
        barwidth = unit(0.2 * legend_scale, "cm"),
        order = 1
      ),
      size = guide_legend(
        title = "Percent Expressed",
        order = 2
      )
    ) +
    coord_flip()
}
 
# Plot gene expression bubbleplot / heatmap 
scBubbHeat <- function(inpConf, inpMeta, inp, inpGrp, inpPlt,
                       inpsub1, inpsub2, inpH5, inpGene, inpScl, inpRow, inpCol,
                       inpcols, inpfsz, save = FALSE, show_legend = TRUE, dark_theme = FALSE){
  if(is.null(inpsub1)){inpsub1 = first_valid_ui(inpConf)} 
  # Identify genes that are in our dataset 
  geneList = scGeneList(inp, inpGene) 
  geneList = geneList[present == TRUE] 
  sc_profile_note(
    selected_genes = as.character(geneList$gene),
    gene_count = nrow(geneList),
    group_by = as.character(inpGrp),
    plot_variant = as.character(inpPlt)
  )
  shiny::validate(need(nrow(geneList) <= 50, "More than 50 genes to plot! Please reduce the gene list!")) 
  shiny::validate(need(nrow(geneList) > 1, "Please input at least 2 genes to plot!")) 
  
  # Prepare ggData 
  chunks <- vector("list", nrow(geneList))
  for(i in seq_len(nrow(geneList))){
    iGene <- geneList$gene[[i]]
    tmp = inpMeta[, c("sampleID", inpConf[UI == inpsub1]$ID), with = FALSE]
    colnames(tmp) = c("sampleID", "sub")
    tmp$grpBy = inpMeta[[inpConf[UI == inpGrp]$ID]]
    tmp$geneName = iGene
    gene_idx <- get_gene_index(inpGene, iGene)
    tmp$val = read_h5_gene(inpH5, gene_idx)
    chunks[[i]] <- tmp
  }
  ggData = sc_profile_time_block("reshape_ms", {
    ggData_local = rbindlist(chunks)
    if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData_local$sub)){
      ggData_local = ggData_local[sub %in% inpsub2]
    }
    ggData_local
  })
  shiny::validate(need(uniqueN(ggData$grpBy) > 1, "Only 1 group present, unable to plot!")) 
   
  # Aggregate 
  ggData = sc_profile_time_block("aggregation_ms", {
    ggData$val = expm1(ggData$val)
    ggData = ggData[, .(val = mean(val), prop = sum(val>0) / length(sampleID)),
                    by = c("geneName", "grpBy")]
    ggData$val = log1p(ggData$val)
    ggData
  })
   
  # Scale if required 
  colRange = range(ggData$val, na.rm = TRUE) 
  if(inpScl){ 
    scale_values <- sc_profile_time_block("aggregation_ms", {
      ggData[, val := as.numeric(scale(val)), keyby = "geneName"]
      ggData[!is.finite(val), val := 0]
      max_abs <- suppressWarnings(max(abs(ggData$val), na.rm = TRUE))
      if (!is.finite(max_abs) || max_abs <= 0) {
        max_abs <- 1
      }
      list(ggData = ggData, colRange = c(-max_abs, max_abs))
    })
    ggData <- scale_values$ggData
    colRange <- scale_values$colRange
  } else {
    if (!all(is.finite(colRange))) {
      colRange <- c(0, 1)
    }
    if (diff(colRange) == 0) {
      colRange <- colRange + c(-0.5, 0.5)
    }
  }
   
  # hclust row/col if necessary 
  cluster_values <- sc_profile_time_block("aggregation_ms", {
    ggMat = dcast.data.table(ggData, geneName ~ grpBy, value.var = "val", fill = 0)
    tmp = ggMat$geneName
    ggMat = as.matrix(ggMat[, -1, with = FALSE])
    rownames(ggMat) = tmp
    ggMat[!is.finite(ggMat)] <- 0
    dendro_col <- if (isTRUE(dark_theme)) "#e2e8f0" else "#111827"
    row_cluster_ok <- FALSE
    ggRow <- NULL
    if(isTRUE(inpRow) && nrow(ggMat) > 1){
      hcRow <- tryCatch(
        dendro_data(as.dendrogram(hclust(dist(ggMat)))),
        error = function(e) NULL
      )
      if (!is.null(hcRow)) {
        row_cluster_ok <- TRUE
      }
    }
    if(row_cluster_ok){
      ggRow = ggplot() + coord_flip() +
        geom_segment(data = hcRow$segments, aes(x=x,y=y,xend=xend,yend=yend), color = dendro_col, size = 0.35) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0.5)) +
        sctheme(base_size = sList[inpfsz], dark = dark_theme) +
        theme(axis.title = element_blank(), axis.line = element_blank(),
              axis.ticks = element_blank(), axis.text.y = element_blank(),
              axis.text.x = element_blank())
      ggData$geneName = factor(ggData$geneName, levels = hcRow$labels$label)
    } else {
      ggData$geneName = factor(ggData$geneName, levels = rev(geneList$gene))
    }
    col_cluster_ok <- FALSE
    ggCol <- NULL
    if(isTRUE(inpCol) && ncol(ggMat) > 1){
      hcCol <- tryCatch(
        dendro_data(as.dendrogram(hclust(dist(t(ggMat))))),
        error = function(e) NULL
      )
      if (!is.null(hcCol)) {
        col_cluster_ok <- TRUE
      }
    }
    if(col_cluster_ok){
      ggCol = ggplot() +
        geom_segment(data = hcCol$segments, aes(x=x,y=y,xend=xend,yend=yend), color = dendro_col, size = 0.35) +
        scale_x_continuous(expand = c(0.05, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1, dark = dark_theme) +
        theme(axis.title = element_blank(), axis.line = element_blank(),
              axis.ticks = element_blank(), axis.text.x = element_blank(),
              axis.text.y = element_blank())
      ggData$grpBy = factor(ggData$grpBy, levels = hcCol$labels$label)
    }
    list(
      ggData = ggData,
      ggRow = ggRow,
      ggCol = ggCol,
      row_cluster_ok = row_cluster_ok,
      col_cluster_ok = col_cluster_ok
    )
  })
  ggData <- cluster_values$ggData
  ggRow <- cluster_values$ggRow
  ggCol <- cluster_values$ggCol
  row_cluster_ok <- cluster_values$row_cluster_ok
  col_cluster_ok <- cluster_values$col_cluster_ok
   
  # Actual plot according to plottype 
  ggOut = sc_profile_time_block("plotting_ms", {
    if(inpPlt == "Bubbleplot"){
      ggOut = ggplot(ggData, aes(grpBy, geneName, color = val, size = prop)) +
        geom_point() +
        sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1, dark = dark_theme) +
        scale_x_discrete(expand = c(0.05, 0)) +
        scale_y_discrete(expand = c(0, 0.5)) +
        scale_size_continuous("proportion", range = c(0, 8),
                              limits = c(0, 1), breaks = c(0.00,0.25,0.50,0.75,1.00)) +
        scale_color_gradientn("expression", limits = colRange, colours = cList[[inpcols]]) +
        guides(color = guide_colorbar(barwidth = 15)) +
        theme(
          axis.title = element_blank(),
          legend.box = "vertical",
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
          plot.margin = grid::unit(c(0.5, 0.5, 1.2, 0.5), "lines")
        )
    } else {
      ggOut = ggplot(ggData, aes(grpBy, geneName, fill = val)) +
        geom_tile() +
        sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1, dark = dark_theme) +
        scale_x_discrete(expand = c(0.05, 0)) +
        scale_y_discrete(expand = c(0, 0.5)) +
        scale_fill_gradientn("expression", limits = colRange, colours = cList[[inpcols]]) +
        guides(fill = guide_colorbar(barwidth = 15)) +
        theme(
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
          plot.margin = grid::unit(c(0.5, 0.5, 1.2, 0.5), "lines")
        )
    }

    ggLeg <- if (isTRUE(show_legend)) {
      g_legend(ggOut)
    } else {
      grid::nullGrob()
    }
    ggOut <- ggOut +
      theme(legend.position = "none") +
      guides(color = "none", fill = "none", size = "none")
    empty_grob <- grid::nullGrob()
    ggOut <- ggplotGrob(ggOut)
    top_grob <- if (col_cluster_ok) ggplotGrob(ggCol) else empty_grob
    right_grob <- if (row_cluster_ok) ggplotGrob(ggRow) else empty_grob
    if (col_cluster_ok) {
      shared_widths <- grid::unit.pmax(ggOut$widths, top_grob$widths)
      ggOut$widths <- shared_widths
      top_grob$widths <- shared_widths
    }
    if (row_cluster_ok) {
      shared_heights <- grid::unit.pmax(ggOut$heights, right_grob$heights)
      ggOut$heights <- shared_heights
      right_grob$heights <- shared_heights
    }
    assembled_args <- list(
      grobs = list(ggOut, ggLeg, top_grob, right_grob, empty_grob, empty_grob),
      widths = c(7, if (isTRUE(show_legend)) 1 else 0.05),
      heights = c(1, 7, 2),
      layout_matrix = rbind(c(3, 5), c(1, 4), c(2, 6))
    )
    if(!save){
      do.call(grid.arrange, assembled_args)
    } else {
      do.call(arrangeGrob, assembled_args)
    }
  })
  return(ggOut) 
} 
 




### Start server code 
shinyServer(function(input, output, session) { 
  
  ### For all tags and Server-side selectize 
  observe_helpers() 

  ra_assets_ready <- reactiveValues(
    spg = FALSE,
    dot = FALSE,
    line = FALSE
  )

  ra_plot_gate <- reactiveValues(
    spermatogonia = FALSE,
    ra_dotplot = FALSE,
    ra_lineplot = FALSE
  )
  ra_dot_tick  <- reactiveVal(1)  # auto render once
  ra_line_tick <- reactiveVal(1)
  ccc_tick     <- reactiveVal(1)
  ra_line_dev_checked <- reactiveVal(FALSE)
  startup_reload_retries <- reactiveValues(
    retinoic_acid = 0L,
    cell2cell_heatmaps = 0L
  )

  open_ra_gate <- function(name, delay = 0) {
    if (isTRUE(delay <= 0)) {
      ra_plot_gate[[name]] <- TRUE
      return(invisible(NULL))
    }
    ra_plot_gate[[name]] <- FALSE
    later::later(function() {
      ra_plot_gate[[name]] <- TRUE
    }, delay = delay)
  }

  update_ra_gene_inputs <- function(genes) {
    desired_genes <- rev(c(
      "Stra8", "Stra6",
      "Aldh1a1", "Aldh1a2", "Aldh1a3",
      "Cyp26a1", "Cyp26b1", "Cyp26c1",
      "Rara", "Rarb", "Rarg",
      "Rxra", "Rxrb", "Rxrg",
      "Dmrt1", "Rdh10", "Rbp4", "Rbp1"
    ))
    default_genes <- rev(c(
      "Stra8", "Stra6",
      "Aldh1a1", "Aldh1a2", "Aldh1a3",
      "Cyp26a1", "Cyp26b1", "Cyp26c1",
      "Rdh10", "Rbp4", "Rbp1"
    ))
    default_genes <- default_genes[default_genes %in% genes]
    ordered_choices <- c(default_genes, setdiff(genes, default_genes))
    selected <- input$ra_genes
    if (is.null(selected) || !length(selected)) {
      selected <- default_genes
    }
    if (!is.null(initial_query[["ra_genes"]])) {
      selected <- decode_query_value(initial_query[["ra_genes"]])
    }
    updateSelectizeInput(
      session,
      "ra_genes",
      choices = ordered_choices,
      selected = selected,
      server = TRUE
    )
  }

  update_ra_celltype_inputs <- function(cell_types) {
    desired_cell_types <- c(
      "Aund", "A1-2", "A3-4", "Ain", "Type B",
      "ePL", "lPL", "L", "L/Z", "Z",
      "PaI-VI", "PaVII-VIII", "PaIX-X", "D/MI",
      "Rd1", "Rd2-3", "Rd4-5", "Rd6", "Rd7", "Rd8",
      "El9", "El10", "El11", "El12-13", "El14-15", "El16",
      "SC_I-VIII", "SC_VII-VIII", "SC_IX-XII", "SC_XI-VI", "SC_All_Stages",
      "PTM", "Leydig", "Macrophage"
    )
    ordered_choices <- c(desired_cell_types[desired_cell_types %in% cell_types],
                         setdiff(cell_types, desired_cell_types))
    selected <- input$ra_cell_types
    if (is.null(selected) || !length(selected)) {
      selected <- ordered_choices
    }
    if (!is.null(initial_query[["ra_cell_types"]])) {
      selected <- decode_query_value(initial_query[["ra_cell_types"]])
    }
    updateCheckboxGroupInput(
      session,
      "ra_cell_types",
      choices = ordered_choices,
      selected = selected
    )
  }

  update_ra_line_inputs <- function(genes) {
    default_genes_row1 <- c("Stra8", "Stra6", "Rbp1")
    default_genes_row2 <- c("Aldh1a1", "Aldh1a2", "Aldh1a3", "Rdh10")
    default_genes_row3 <- c("Cyp26a1", "Cyp26b1", "Cyp26c1", "Rarg")

    default_genes_row1 <- default_genes_row1[default_genes_row1 %in% genes]
    default_genes_row2 <- default_genes_row2[default_genes_row2 %in% genes]
    default_genes_row3 <- default_genes_row3[default_genes_row3 %in% genes]

    selected1 <- input$ra_line_genes_row1
    selected2 <- input$ra_line_genes_row2
    selected3 <- input$ra_line_genes_row3

    if (is.null(selected1) || !length(selected1)) selected1 <- default_genes_row1
    if (is.null(selected2) || !length(selected2)) selected2 <- default_genes_row2
    if (is.null(selected3) || !length(selected3)) selected3 <- default_genes_row3

    updateSelectizeInput(
      session,
      "ra_line_genes_row1",
      choices = genes,
      selected = selected1,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "ra_line_genes_row2",
      choices = genes,
      selected = selected2,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "ra_line_genes_row3",
      choices = genes,
      selected = selected3,
      server = TRUE
    )
  }

  update_spg_gene_input <- function(genes) {
    genes <- unique(as.character(genes))
    genes <- genes[nzchar(genes)]
    selected <- isolate(input$gene_search)
    if (!is.null(initial_query[["gene_search"]])) {
      selected <- initial_query[["gene_search"]]
    }
    if (is.null(selected) || !length(selected) || !nzchar(selected[[1]])) {
      selected <- character(0)
    }

    updateSelectizeInput(
      session,
      "gene_search",
      choices = genes,
      selected = selected,
      server = TRUE,
      options = list(
        placeholder = "Type a gene…",
        create = TRUE,
        persist = FALSE,
        maxOptions = 20,
        openOnFocus = FALSE
      )
    )
  }

  ensure_spg_assets <- function(show_progress = TRUE) {
    if (isTRUE(ra_assets_ready$spg)) {
      return(TRUE)
    }
    if (isTRUE(show_progress)) {
      withProgress(message = "Loading spermatogenesis table data…", value = 0, {
        get_spg_avg_expr()
        incProgress(1)
      })
    } else {
      get_spg_avg_expr()
    }
    spg_mat <- get_spg_avg_expr()
    ra_assets_ready$spg <- TRUE
    update_spg_gene_input(rownames(spg_mat))
    TRUE
  }

  ensure_dot_assets <- function(show_progress = TRUE) {
    if (!isTRUE(ra_assets_ready$dot)) {
      if (isTRUE(show_progress)) {
        withProgress(message = "Loading RA dotplot data…", value = 0, {
          get_ra_dot_avg()
          get_ra_dot_pct()
          incProgress(1)
        })
      } else {
        get_ra_dot_avg()
        get_ra_dot_pct()
      }
      ra_assets_ready$dot <- TRUE
    }
    genes <- get_interactive_genes()
    update_ra_gene_inputs(genes)
    update_ra_celltype_inputs(colnames(get_ra_dot_avg()))
    TRUE
  }

  ensure_line_assets <- function(show_progress = TRUE) {
    if (!isTRUE(ra_assets_ready$line)) {
      if (isTRUE(show_progress)) {
        withProgress(message = "Loading RA lineplot data…", value = 0, {
          get_ra_line_mean()
          incProgress(1)
        })
      } else {
        get_ra_line_mean()
      }
      ra_assets_ready$line <- TRUE
    }
    if (isTRUE(getOption("dev_mode_ra_check", FALSE)) && !isTRUE(ra_line_dev_checked())) {
      dev_check_ra_line()
      ra_line_dev_checked(TRUE)
    }
    genes <- get_interactive_genes()
    update_ra_line_inputs(genes)
    TRUE
  }

  observeEvent(input$mainTabs, {
    tab <- input$mainTabs
    if (is.null(tab)) return()
    if (tab %in% ra_tab_ids) {
      if (identical(tab, "spermatogonia_table")) {
        ensure_spg_assets()
        open_ra_gate("spermatogonia")
      } else if (identical(tab, "retinoic_acid")) {
        ensure_dot_assets()
        ensure_line_assets()
        open_ra_gate("ra_dotplot")
        open_ra_gate("ra_lineplot")
        ra_dot_tick(ra_dot_tick() + 1)
        ra_line_tick(ra_line_tick() + 1)
        startup_reload_retries$retinoic_acid <- 4L
      }
    }
  }, ignoreNULL = TRUE)

  observe({
    attempts_left <- startup_reload_retries$retinoic_acid
    if (attempts_left <= 0L || !identical(input$mainTabs, "retinoic_acid")) return()
    invalidateLater(1400, session)
    ensure_dot_assets(show_progress = FALSE)
    ensure_line_assets(show_progress = FALSE)
    # Keep the gates open during startup retries so one successful paint is guaranteed.
    ra_plot_gate$ra_dotplot <- TRUE
    ra_plot_gate$ra_lineplot <- TRUE
    ra_dot_tick(ra_dot_tick() + 1)
    ra_line_tick(ra_line_tick() + 1)
    startup_reload_retries$retinoic_acid <- attempts_left - 1L
  })

  observeEvent(input$preset_copy_notice, {
    msg <- input$preset_copy_notice$message
    if (is.null(msg) || msg == "") {
      msg <- "Sharable link copied"
    }
    showNotification(msg, type = "message", duration = 4)
  }, ignoreNULL = TRUE)

  current_theme <- reactive({
    theme <- input$theme_mode
    if (is.null(theme) || theme == "") {
      theme <- theme_default
    }
    theme
  })

  is_dark_mode <- reactive({
    identical(current_theme(), "dark")
  })

  with_dark <- function(fun, ...) {
    fun(..., dark_theme = is_dark_mode())
  }

  with_dark_static <- function(fun, ...) {
    fun(..., dark_theme = isolate(is_dark_mode()))
  }
  preset_select_inputs <- c(
#     "sc1a1drX","sc1a1drY","sc1a1inp1","sc1a1inp2",
#     "sc2a1drX","sc2a1drY","sc2a1inp1","sc2a1inp2",
    "sc3a1drX","sc3a1drY","sc3a1inp1","sc3a1inp2",
    "sc4a1drX","sc4a1drY","sc4a1inp1","sc4a1inp2",
    "sc5a1drX","sc5a1drY","sc5a1inp1","sc5a1inp2",
    "sc6a1drX","sc6a1drY","sc6a1inp1","sc6a1inp2",
    "sc7a1drX","sc7a1drY","sc7a1inp1","sc7a1inp2"
  )
  preset_multi_inputs <- c("ra_genes", "ra_cell_types")
  encode_query_value <- function(value) paste(value, collapse = ",")
  decode_query_value <- function(value) strsplit(value, ",", fixed = TRUE)[[1]]
  initial_query <- isolate(parseQueryString(session$clientData$url_search))

  # Exclude transient controls/events from bookmarked links.
  setBookmarkExclude(c(
    "copy_view_link",
    "preset_copy_notice",
    "home_card_nav",
    "btn_click",
    "modalClosedBtn",
    "__modal__closed__",
    "gene_search_btn",
    "extended_tutorial_btn"
  ))

  observeEvent(input$copy_view_link, {
    tryCatch(
      session$doBookmark(),
      error = function(e) {
        showNotification(
          sprintf("Unable to build share link: %s", conditionMessage(e)),
          type = "error",
          duration = 5
        )
      }
    )
  }, ignoreInit = TRUE)

  onBookmarked(function(url) {
    tab_value <- isolate(input$mainTabs)
    shared_url <- sub("#.*$", "", url)
    shared_url <- gsub("(/_w_[^/?#]+)+", "", shared_url)
    if (!is.null(tab_value) && nzchar(tab_value)) {
      shared_url <- paste0(shared_url, "#", utils::URLencode(tab_value, reserved = TRUE))
    }
    session$sendCustomMessage("copy-to-clipboard", list(text = shared_url))
  })

#   user_upload <- reactiveValues(
#     temp_dir = NULL,
#     dataset_name = NULL,
#     conf = NULL,
#     def = NULL,
#     gene = NULL,
#     meta = NULL,
#     gexpr_path = NULL,
#     summary = NULL,
#     error = NULL
#   )
# 
#   clear_user_upload <- function() {
#     try(updateNavbarPage(session, "mainTabs", selected = "user_upload"), silent = TRUE)
#     temp_dir <- isolate(user_upload$temp_dir)
#     if (!is.null(temp_dir) && dir.exists(temp_dir)) {
#       unlink(temp_dir, recursive = TRUE, force = TRUE)
#     }
#     user_upload$temp_dir <- NULL
#     user_upload$dataset_name <- NULL
#     user_upload$conf <- NULL
#     user_upload$def <- NULL
#     user_upload$gene <- NULL
#     user_upload$meta <- NULL
#     user_upload$gexpr_path <- NULL
#     user_upload$summary <- NULL
#   }
# 
#   reset_user_error <- function() {
#     user_upload$error <- NULL
#   }
# 
#   user_ready <- reactive({
#     !is.null(user_upload$conf) &&
#       !is.null(user_upload$meta) &&
#       !is.null(user_upload$gene) &&
#       !is.null(user_upload$def) &&
#       !is.null(user_upload$gexpr_path) &&
#       file.exists(user_upload$gexpr_path)
#   })
# 
#   sanitize_choices <- function(values) {
#     vals <- as.character(values)
#     vals <- vals[!is.na(vals) & nzchar(vals)]
#     unique(vals)
#   }
# 
#   pick_first_choice <- function(value, choices, preferred = NULL) {
#     choices <- sanitize_choices(choices)
#     if (!length(choices)) {
#       return(NULL)
#     }
#     candidate <- sanitize_choices(value)
#     if (length(candidate) && candidate[1] %in% choices) {
#       candidate[1]
#     } else if (!is.null(preferred) && preferred %in% choices) {
#       preferred
#     } else {
#       choices[1]
#     }
#   }
# 
#   gene_choice_vector <- function(gene_vector) {
#     if (is.null(gene_vector)) {
#       return(character())
#     }
#     gene_names <- names(gene_vector)
#     if (is.null(gene_names) || !length(gene_names)) {
#       gene_names <- as.character(unname(gene_vector))
#     }
#     sanitize_choices(gene_names)
#   }
# 
#   observeEvent(input$copy_view_link, {
#     vals <- reactiveValuesToList(input, all.names = TRUE)
#     state <- list()
#     if (!is.null(vals$mainTabs) && nzchar(vals$mainTabs)) {
#     state$mainTabs <- vals$mainTabs
#   }
#     for (id in preset_select_inputs) {
#       val <- vals[[id]]
#       if (!is.null(val) && length(val) && nzchar(val[[1]])) {
#         state[[id]] <- val[[1]]
#       }
#     }
#     for (id in preset_multi_inputs) {
#       val <- vals[[id]]
#       if (!is.null(val) && length(val)) {
#         state[[id]] <- val
#       }
#     }
#     if (!length(state)) {
#       showNotification("Select some options before copying a link.", type = "warning", duration = 4)
#       return()
#     }
#     query <- paste(
#       sprintf(
#         "%s=%s",
#         names(state),
#         vapply(state, function(x) utils::URLencode(encode_query_value(x), reserved = TRUE), character(1))
#       ),
#       collapse = "&"
#     )
#     updateQueryString(if (nzchar(query)) paste0("?", query) else "", mode = "push")
#     base_url <- paste0(
#       session$clientData$url_protocol, "//", session$clientData$url_hostname,
#       if (!is.null(session$clientData$url_port) && nzchar(session$clientData$url_port)) paste0(":", session$clientData$url_port) else "",
#       session$clientData$url_pathname
#     )
#     full_url <- if (nzchar(query)) paste0(base_url, "?", query) else base_url
#     session$sendCustomMessage("copy-to-clipboard", list(text = full_url))
#   })
# 
#   session$onFlushed(function() {
#     if (length(initial_query) == 0) {
#       return()
#     }
#     if (!is.null(initial_query[["mainTabs"]])) {
#       try(updateNavbarPage(session, "mainTabs", selected = initial_query[["mainTabs"]]), silent = TRUE)
#     }
#     for (id in preset_select_inputs) {
#       if (!is.null(initial_query[[id]])) {
#         try(updateSelectInput(session, id, selected = initial_query[[id]]), silent = TRUE)
#       }
#     }
#   }, once = TRUE)
# 
#   user_conf <- reactive({
#     req(user_ready())
#     user_upload$conf
#   })
# 
#   user_gene <- reactive({
#     req(user_ready())
#     user_upload$gene
#   })
# 
#   user_def <- reactive({
#     req(user_ready())
#     def <- user_upload$def
#     conf <- user_conf()
# 
#     cellinfo_choices <- sanitize_choices(conf$UI)
#     default_cellinfo <- pick_first_choice("nCount_RNA", cellinfo_choices, preferred = "nCount_RNA")
#     def$meta1 <- default_cellinfo
#     def$meta2 <- pick_first_choice(def$meta2, cellinfo_choices, preferred = default_cellinfo)
# 
#     gene_choices <- gene_choice_vector(user_gene())
#     if (length(gene_choices)) {
#       def$gene1 <- pick_first_choice(def$gene1, gene_choices)
#       def$gene2 <- pick_first_choice(def$gene2, gene_choices)
# 
#       existing_genes <- sanitize_choices(def$genes)
#       valid_genes <- existing_genes[existing_genes %in% gene_choices]
#       if (length(valid_genes)) {
#         def$genes <- valid_genes
#       } else {
#         max_genes <- min(10, length(gene_choices))
#         def$genes <- gene_choices[seq_len(max_genes)]
#       }
#     }
# 
#     def
#   })
# 
#   user_meta <- reactive({
#     req(user_ready())
#     user_upload$meta
#   })
# 
#   user_gexpr <- reactive({
#     req(user_ready())
#     user_upload$gexpr_path
#   })
# 
#   user_dataset_label <- reactive({
#     req(user_ready())
#     label <- user_upload$dataset_name
#     if (is.null(label) || !nzchar(label)) {
#       "Uploaded Dataset"
#     } else {
#       label
#     }
#   })
# 
#   user_summary <- reactive({
#     req(user_ready())
#     list(
#       cells = nrow(user_meta()),
#       genes = length(user_gene()),
#       reductions = sum(user_conf()$dimred, na.rm = TRUE)
#     )
#   })
# 
#   observeEvent(input$user_seurat_file, {
#     file <- input$user_seurat_file
#     if (is.null(file)) {
#       return()
#     }
#     reset_user_error()
#     clear_user_upload()
#     temp_dir <- tempfile("usr_dataset_")
#     dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
#     update_upload_indicator <- function(state, message = NULL, progress = NULL) {
#       payload <- list(state = state)
#       if (!is.null(message)) {
#         payload$message <- message
#       }
#       if (!is.null(progress)) {
#         payload$progress <- progress
#       }
#       session$sendCustomMessage("user-upload-status", payload)
#     }
#     update_upload_indicator(
#       "processing",
#       "Validating uploaded file…",
#       0.1
#     )
# 
#     fail_upload <- function(msg) {
#       user_upload$error <- msg
#       update_upload_indicator("error", msg, 0)
#       clear_user_upload()
#       if (dir.exists(temp_dir)) {
#         unlink(temp_dir, recursive = TRUE, force = TRUE)
#       }
#       return(invisible(NULL))
#     }
# 
#     update_upload_indicator(
#       "processing",
#       "Reading Seurat object…",
#       0.25
#     )
#     seu <- tryCatch(readRDS(file$datapath), error = identity)
#     if (inherits(seu, "error")) {
#       return(fail_upload(paste("Unable to read RDS:", seu$message)))
#     }
#     if (!inherits(seu, "Seurat")) {
#       return(fail_upload("Uploaded file does not contain a Seurat object."))
#     }
# 
#     default_assay <- tryCatch(Seurat::DefaultAssay(seu), error = function(e) NULL)
#     if (is.null(default_assay) || !default_assay %in% names(seu@assays)) {
#       default_assay <- if (length(seu@assays) > 0) names(seu@assays)[1] else "RNA"
#     }
#     update_upload_indicator(
#       "processing",
#       sprintf("Using %s assay…", default_assay),
#       0.35
#     )
#     assay_obj <- seu@assays[[default_assay]]
#     if ("Assay5" %in% class(assay_obj)) {
#       if ("JoinLayers" %in% getNamespaceExports("Seurat")) {
#         seu <- Seurat::JoinLayers(seu)
#       } else {
#         return(fail_upload(
#           "The Seurat object uses layered assays but `Seurat::JoinLayers()` is not available in this environment."
#         ))
#       }
#     }
# 
#     conf <- tryCatch(
#       ShinyCell::createConfig(seu),
#       error = identity
#     )
#     update_upload_indicator(
#       "processing",
#       "Building viewer configuration…",
#       0.55
#     )
#     if (inherits(conf, "error")) {
#       return(fail_upload(paste("Failed to build ShinyCell configuration:", conf$message)))
#     }
# 
#     sc_build <- tryCatch(
#       ShinyCell::makeShinyFiles(
#         obj = seu,
#         scConf = conf,
#         gex.assay = default_assay,
#         gex.slot = "data",
#         gene.mapping = TRUE,
#         shiny.prefix = "usr",
#         shiny.dir = temp_dir
#       ),
#       error = identity
#     )
#     update_upload_indicator(
#       "processing",
#       "Generating ShinyCell files…",
#       0.8
#     )
#     if (inherits(sc_build, "error")) {
#       return(fail_upload(paste("Unable to generate ShinyCell files:", sc_build$message)))
#     }
# 
#     user_upload$temp_dir <- temp_dir
#     user_upload$conf <- readRDS(file.path(temp_dir, "usrconf.rds"))
#     user_upload$meta <- readRDS(file.path(temp_dir, "usrmeta.rds"))
#     user_upload$gene <- normalize_gene_index(readRDS(file.path(temp_dir, "usrgene.rds")))
#     user_upload$def <- readRDS(file.path(temp_dir, "usrdef.rds"))
#     user_upload$gexpr_path <- file.path(temp_dir, "usrgexpr.h5")
#     if (!file.exists(user_upload$gexpr_path)) {
#       return(fail_upload("Gene expression HDF5 file was not generated."))
#     }
#     project_name <- tryCatch(seu@project.name, error = function(e) NULL)
#     if (!is.null(project_name) && nzchar(project_name)) {
#       display_label <- project_name
#     } else {
#       display_label <- tools::file_path_sans_ext(basename(file$name))
#     }
#     user_upload$dataset_name <- display_label
#     update_upload_indicator(
#       "complete",
#       sprintf("%s loaded successfully.", display_label),
#       1
#     )
#     reset_user_error()
#   }, ignoreNULL = TRUE)
# 
#   output$user_upload_feedback <- renderUI({
#     if (!is.null(user_upload$error)) {
#       tags$div(
#         class = "alert alert-danger",
#         icon("triangle-exclamation"),
#         tags$span(class = "sr-only", "Error"),
#         tags$span(sprintf(" Upload failed: %s", user_upload$error))
#       )
#     } else if (user_ready()) {
#       tags$div(
#         class = "alert alert-success",
#         icon("circle-check"),
#         tags$span(sprintf(" %s loaded successfully.", user_dataset_label()))
#       )
#     } else {
#       tags$p(
#         class = "text-muted",
#         "Upload a Seurat .rds file to generate a custom dataset tab."
#       )
#     }
#   })
# 
#   output$user_dataset_summary <- renderUI({
#     if (!user_ready()) {
#       return(NULL)
#     }
#     summary <- user_summary()
#     tags$ul(
#       class = "list-unstyled upload-summary-list",
#       tags$li(tags$strong("Cells: "), format(summary$cells, big.mark = ",")),
#       tags$li(tags$strong("Genes: "), format(summary$genes, big.mark = ",")),
#       tags$li(tags$strong("Dimensional reductions: "), format(summary$reductions, big.mark = ","))
#     )
#   })
# 
#   user_placeholder_panel <- function(title) {
#     tags$div(
#       class = "user-placeholder-tab",
#       tags$div(
#         class = "user-placeholder-card",
#         icon("cloud-upload"),
#         tags$h3("Dataset not loaded"),
#         tags$p(sprintf("Upload a Seurat object in the \"Upload\" tab to view %s.", title))
#       )
#     )
#   }
# 
#   output$usr_cellinfo_gene_panel <- renderUI({
#     if (!user_ready()) {
#       return(user_placeholder_panel("CellInfo vs GeneExpr"))
#     }
#     build_cellinfo_gene_tab("usr", user_conf(), user_def(), user_dataset_label(), as_tab = FALSE)
#   })
# 
#   output$usr_cellinfo_cellinfo_panel <- renderUI({
#     if (!user_ready()) {
#       return(user_placeholder_panel("CellInfo vs CellInfo"))
#     }
#     build_cellinfo_cellinfo_tab("usr", user_conf(), user_def(), user_dataset_label(), as_tab = FALSE)
#   })
# 
#   output$usr_gene_gene_panel <- renderUI({
#     if (!user_ready()) {
#       return(user_placeholder_panel("GeneExpr vs GeneExpr"))
#     }
#     build_gene_gene_tab("usr", user_conf(), user_def(), user_dataset_label(), as_tab = FALSE)
#   })
# 
#   output$usr_gene_coexpression_panel <- renderUI({
#     if (!user_ready()) {
#       return(user_placeholder_panel("Gene coexpression"))
#     }
#     build_gene_coexpression_tab("usr", user_conf(), user_def(), user_dataset_label(), as_tab = FALSE)
#   })
# 
#   output$usr_violin_boxplot_panel <- renderUI({
#     if (!user_ready()) {
#       return(user_placeholder_panel("Violinplot / Boxplot"))
#     }
#     build_violin_boxplot_tab("usr", user_conf(), user_def(), user_dataset_label(), as_tab = FALSE)
#   })
# 
#   output$usr_proportion_plot_panel <- renderUI({
#     if (!user_ready()) {
#       return(user_placeholder_panel("Proportion plot"))
#     }
#     build_proportion_plot_tab("usr", user_conf(), user_def(), user_dataset_label(), as_tab = FALSE)
#   })
# 
#   output$usr_bubble_heatmap_panel <- renderUI({
#     if (!user_ready()) {
#       return(user_placeholder_panel("Bubbleplot / Heatmap"))
#     }
#     build_bubble_heatmap_tab("usr", user_conf(), user_def(), user_dataset_label(), as_tab = FALSE)
#   })
# 
#   observeEvent(user_ready(), {
#     req(user_ready())
#     session$onFlushed(function() {
#       updateNavbarPage(session, "mainTabs", selected = "usr_cellinfo_gene")
#       session$sendCustomMessage("close-nav-dropdown", list(target = "usr_cellinfo_gene"))
#     }, once = TRUE)
#   }, ignoreNULL = TRUE)
# 
#   optCrt <- "{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }"
#   gene_select_options <- list(
#     maxOptions = 7,
#     create = FALSE,
#     persist = TRUE,
#     render = I(optCrt)
#   )
# 
#   register_user_gene_select <- function(input_id, selected_fn) {
#     gene_observer <- NULL
#     observeEvent(user_ready(), {
#       req(user_ready())
#       if (!is.null(gene_observer)) {
#         gene_observer$destroy()
#         gene_observer <<- NULL
#       }
#       gene_observer <<- observeEvent(input[[input_id]], {
#         gene_choices <- gene_choice_vector(user_gene())
#         if (!length(gene_choices)) {
#           updateSelectizeInput(
#             session,
#             input_id,
#             choices = list(),
#             selected = NULL,
#             server = TRUE,
#             options = gene_select_options
#           )
#           return()
#         }
#         selected_val <- pick_first_choice(selected_fn(), gene_choices)
#         updateSelectizeInput(
#           session,
#           input_id,
#           choices = gene_choices,
#           selected = selected_val,
#           server = TRUE,
#           options = gene_select_options
#         )
#       }, ignoreNULL = TRUE, once = TRUE)
#     }, ignoreNULL = TRUE)
#   }
# 
#   register_user_gene_select("usra1inp2", function() user_def()$gene1)
#   register_user_gene_select("usra3inp1", function() user_def()$gene1)
#   register_user_gene_select("usra3inp2", function() user_def()$gene2)
#   register_user_gene_select("usrb2inp1", function() user_def()$gene1)
#   register_user_gene_select("usrb2inp2", function() user_def()$gene2)
# 
#   register_user_metric_select <- function(input_id) {
#     metric_observer <- NULL
#     observeEvent(user_ready(), {
#       req(user_ready())
#       if (!is.null(metric_observer)) {
#         metric_observer$destroy()
#         metric_observer <<- NULL
#       }
#       metric_observer <<- observeEvent(input[[input_id]], {
#         gene_choices <- gene_choice_vector(user_gene())
#         numeric_metrics <- sanitize_choices(user_conf()[is.na(fID)]$UI)
#         extra_gene_choices <- gene_choices[!gene_choices %in% numeric_metrics]
#         combined_choices <- c(numeric_metrics, extra_gene_choices)
#         if (!length(combined_choices)) {
#           updateSelectizeInput(
#             session,
#             input_id,
#             choices = list(),
#             selected = NULL,
#             server = TRUE,
#             options = list(
#               maxOptions = 5,
#               create = FALSE,
#               persist = TRUE,
#               render = I(optCrt)
#             )
#           )
#           return()
#         }
#         selected_metric <- pick_first_choice(user_def()$meta1, combined_choices, preferred = user_def()$meta1)
#         updateSelectizeInput(
#           session,
#           input_id,
#           choices = combined_choices,
#           selected = selected_metric,
#           server = TRUE,
#           options = list(
#             maxOptions = max(length(numeric_metrics) + 3, 5),
#             create = FALSE,
#             persist = TRUE,
#             render = I(optCrt)
#           )
#         )
#       }, ignoreNULL = TRUE, once = TRUE)
#     }, ignoreNULL = TRUE)
#   }
# 
#   register_user_metric_select("usrc1inp2")
# 
  ### Plots for tab a1 
#   output$usra1sub1.ui <- renderUI({ 
#   sub = strsplit(user_conf()[UI == input$usra1sub1]$fID, "\\|")[[1]] 
#   checkboxGroupInput("usra1sub2", "Select which cells to show", inline = TRUE, 
#   choices = sub, selected = sub) 
#   }) 
#   observeEvent(input$usra1sub1non, { 
#   sub = strsplit(user_conf()[UI == input$usra1sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usra1sub2", label = "Select which cells to show", 
#   choices = sub, selected = NULL, inline = TRUE) 
#   }) 
#   observeEvent(input$usra1sub1all, { 
#   sub = strsplit(user_conf()[UI == input$usra1sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usra1sub2", label = "Select which cells to show", 
#   choices = sub, selected = sub, inline = TRUE) 
#   }) 
#   output$usra1oup1 <- renderPlot({ 
#   scDRcell(user_conf(), user_meta(), input$usra1drX, input$usra1drY, input$usra1inp1,  
#   input$usra1sub1, input$usra1sub2, 
#   input$usra1siz, input$usra1col1, input$usra1ord1, 
#   input$usra1fsz, input$usra1asp, input$usra1txt, input$usra1lab1,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usra1oup1.ui <- renderUI({ 
#   plotOutput("usra1oup1", height = pList[input$usra1psz]) 
#   }) 
#   output$usra1oup1.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra1drX,"_",input$usra1drY,"_",  
#   input$usra1inp1,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usra1oup1.h, width = input$usra1oup1.w, useDingbats = FALSE, 
#   plot = scDRcell(user_conf(), user_meta(), input$usra1drX, input$usra1drY, input$usra1inp1,   
#   input$usra1sub1, input$usra1sub2, 
#   input$usra1siz, input$usra1col1, input$usra1ord1,  
#   input$usra1fsz, input$usra1asp, input$usra1txt, input$usra1lab1,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usra1oup1.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra1drX,"_",input$usra1drY,"_",  
#   input$usra1inp1,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usra1oup1.h, width = input$usra1oup1.w, 
#   plot = scDRcell(user_conf(), user_meta(), input$usra1drX, input$usra1drY, input$usra1inp1,   
#   input$usra1sub1, input$usra1sub2, 
#   input$usra1siz, input$usra1col1, input$usra1ord1,  
#   input$usra1fsz, input$usra1asp, input$usra1txt, input$usra1lab1,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usra1.dt <- renderDataTable({ 
#   ggData = scDRnum(user_conf(), user_meta(), input$usra1inp1, input$usra1inp2, 
#   input$usra1sub1, input$usra1sub2, 
#   user_gexpr(), user_gene(), input$usra1splt) 
#   datatable(
#     ggData,
#     rownames = FALSE,
#     extensions = "Buttons",
#     server = TRUE,
#     options = list(
#       pageLength = 50,
#       lengthMenu = c(25, 50, 100),
#       deferRender = TRUE,
#       dom = "tBfrtip",
#       buttons = c("copy", "csv", "excel")
#     )
#   ) %>% 
#   formatRound(columns = c("pctExpress"), digits = 2) 
#   }) 
#   
#   output$usra1oup2 <- renderPlot({ 
#   scDRgene(user_conf(), user_meta(), input$usra1drX, input$usra1drY, input$usra1inp2,  
#   input$usra1sub1, input$usra1sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra1siz, input$usra1col2, input$usra1ord2, 
#   input$usra1fsz, input$usra1asp, input$usra1txt,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usra1oup2.ui <- renderUI({ 
#   plotOutput("usra1oup2", height = pList[input$usra1psz]) 
#   }) 
#   output$usra1oup2.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra1drX,"_",input$usra1drY,"_",  
#   input$usra1inp2,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usra1oup2.h, width = input$usra1oup2.w, useDingbats = FALSE, 
#   plot = scDRgene(user_conf(), user_meta(), input$usra1drX, input$usra1drY, input$usra1inp2,   
#   input$usra1sub1, input$usra1sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra1siz, input$usra1col2, input$usra1ord2,  
#   input$usra1fsz, input$usra1asp, input$usra1txt,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usra1oup2.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra1drX,"_",input$usra1drY,"_",  
#   input$usra1inp2,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usra1oup2.h, width = input$usra1oup2.w, 
#   plot = scDRgene(user_conf(), user_meta(), input$usra1drX, input$usra1drY, input$usra1inp2,   
#   input$usra1sub1, input$usra1sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra1siz, input$usra1col2, input$usra1ord2,  
#   input$usra1fsz, input$usra1asp, input$usra1txt,
#   dark_theme = dark_theme) ) 
#   }) 
#   
  ### Plots for tab a2 
#   output$usra2sub1.ui <- renderUI({ 
#   sub = strsplit(user_conf()[UI == input$usra2sub1]$fID, "\\|")[[1]] 
#   checkboxGroupInput("usra2sub2", "Select which cells to show", inline = TRUE, 
#   choices = sub, selected = sub) 
#   }) 
#   observeEvent(input$usra2sub1non, { 
#   sub = strsplit(user_conf()[UI == input$usra2sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usra2sub2", label = "Select which cells to show", 
#   choices = sub, selected = NULL, inline = TRUE) 
#   }) 
#   observeEvent(input$usra2sub1all, { 
#   sub = strsplit(user_conf()[UI == input$usra2sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usra2sub2", label = "Select which cells to show", 
#   choices = sub, selected = sub, inline = TRUE) 
#   }) 
#   output$usra2oup1 <- renderPlot({ 
#   scDRcell(user_conf(), user_meta(), input$usra2drX, input$usra2drY, input$usra2inp1,  
#   input$usra2sub1, input$usra2sub2, 
#   input$usra2siz, input$usra2col1, input$usra2ord1, 
#   input$usra2fsz, input$usra2asp, input$usra2txt, input$usra2lab1,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usra2oup1.ui <- renderUI({ 
#   plotOutput("usra2oup1", height = pList[input$usra2psz]) 
#   }) 
#   output$usra2oup1.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra2drX,"_",input$usra2drY,"_",  
#   input$usra2inp1,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usra2oup1.h, width = input$usra2oup1.w, useDingbats = FALSE, 
#   plot = scDRcell(user_conf(), user_meta(), input$usra2drX, input$usra2drY, input$usra2inp1,   
#   input$usra2sub1, input$usra2sub2, 
#   input$usra2siz, input$usra2col1, input$usra2ord1,  
#   input$usra2fsz, input$usra2asp, input$usra2txt, input$usra2lab1,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usra2oup1.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra2drX,"_",input$usra2drY,"_",  
#   input$usra2inp1,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usra2oup1.h, width = input$usra2oup1.w, 
#   plot = scDRcell(user_conf(), user_meta(), input$usra2drX, input$usra2drY, input$usra2inp1,   
#   input$usra2sub1, input$usra2sub2, 
#   input$usra2siz, input$usra2col1, input$usra2ord1,  
#   input$usra2fsz, input$usra2asp, input$usra2txt, input$usra2lab1,
#   dark_theme = dark_theme) ) 
#   }) 
#   
#   output$usra2oup2 <- renderPlot({ 
#   scDRcell(user_conf(), user_meta(), input$usra2drX, input$usra2drY, input$usra2inp2,  
#   input$usra2sub1, input$usra2sub2, 
#   input$usra2siz, input$usra2col2, input$usra2ord2, 
#   input$usra2fsz, input$usra2asp, input$usra2txt, input$usra2lab2,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usra2oup2.ui <- renderUI({ 
#   plotOutput("usra2oup2", height = pList[input$usra2psz]) 
#   }) 
#   output$usra2oup2.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra2drX,"_",input$usra2drY,"_",  
#   input$usra2inp2,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usra2oup2.h, width = input$usra2oup2.w, useDingbats = FALSE, 
#   plot = scDRcell(user_conf(), user_meta(), input$usra2drX, input$usra2drY, input$usra2inp2,   
#   input$usra2sub1, input$usra2sub2, 
#   input$usra2siz, input$usra2col2, input$usra2ord2,  
#   input$usra2fsz, input$usra2asp, input$usra2txt, input$usra2lab2,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usra2oup2.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra2drX,"_",input$usra2drY,"_",  
#   input$usra2inp2,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usra2oup2.h, width = input$usra2oup2.w, 
#   plot = scDRcell(user_conf(), user_meta(), input$usra2drX, input$usra2drY, input$usra2inp2,   
#   input$usra2sub1, input$usra2sub2, 
#   input$usra2siz, input$usra2col2, input$usra2ord2,  
#   input$usra2fsz, input$usra2asp, input$usra2txt, input$usra2lab2,
#   dark_theme = dark_theme) ) 
#   }) 
#   
#   
  ### Plots for tab a3 
#   output$usra3sub1.ui <- renderUI({ 
#   sub = strsplit(user_conf()[UI == input$usra3sub1]$fID, "\\|")[[1]] 
#   checkboxGroupInput("usra3sub2", "Select which cells to show", inline = TRUE, 
#   choices = sub, selected = sub) 
#   }) 
#   observeEvent(input$usra3sub1non, { 
#   sub = strsplit(user_conf()[UI == input$usra3sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usra3sub2", label = "Select which cells to show", 
#   choices = sub, selected = NULL, inline = TRUE) 
#   }) 
#   observeEvent(input$usra3sub1all, { 
#   sub = strsplit(user_conf()[UI == input$usra3sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usra3sub2", label = "Select which cells to show", 
#   choices = sub, selected = sub, inline = TRUE) 
#   }) 
#   output$usra3oup1 <- renderPlot({ 
#   scDRgene(user_conf(), user_meta(), input$usra3drX, input$usra3drY, input$usra3inp1,  
#   input$usra3sub1, input$usra3sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra3siz, input$usra3col1, input$usra3ord1, 
#   input$usra3fsz, input$usra3asp, input$usra3txt,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usra3oup1.ui <- renderUI({ 
#   plotOutput("usra3oup1", height = pList[input$usra3psz]) 
#   }) 
#   output$usra3oup1.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra3drX,"_",input$usra3drY,"_",  
#   input$usra3inp1,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usra3oup1.h, width = input$usra3oup1.w, useDingbats = FALSE, 
#   plot = scDRgene(user_conf(), user_meta(), input$usra3drX, input$usra3drY, input$usra3inp1,  
#   input$usra3sub1, input$usra3sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra3siz, input$usra3col1, input$usra3ord1, 
#   input$usra3fsz, input$usra3asp, input$usra3txt,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usra3oup1.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra3drX,"_",input$usra3drY,"_",  
#   input$usra3inp1,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usra3oup1.h, width = input$usra3oup1.w, 
#   plot = scDRgene(user_conf(), user_meta(), input$usra3drX, input$usra3drY, input$usra3inp1,  
#   input$usra3sub1, input$usra3sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra3siz, input$usra3col1, input$usra3ord1, 
#   input$usra3fsz, input$usra3asp, input$usra3txt,
#   dark_theme = dark_theme) ) 
#   }) 
#   
#   output$usra3oup2 <- renderPlot({ 
#   scDRgene(user_conf(), user_meta(), input$usra3drX, input$usra3drY, input$usra3inp2,  
#   input$usra3sub1, input$usra3sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra3siz, input$usra3col2, input$usra3ord2, 
#   input$usra3fsz, input$usra3asp, input$usra3txt,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usra3oup2.ui <- renderUI({ 
#   plotOutput("usra3oup2", height = pList[input$usra3psz]) 
#   }) 
#   output$usra3oup2.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra3drX,"_",input$usra3drY,"_",  
#   input$usra3inp2,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usra3oup2.h, width = input$usra3oup2.w, useDingbats = FALSE, 
#   plot = scDRgene(user_conf(), user_meta(), input$usra3drX, input$usra3drY, input$usra3inp2,  
#   input$usra3sub1, input$usra3sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra3siz, input$usra3col2, input$usra3ord2, 
#   input$usra3fsz, input$usra3asp, input$usra3txt,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usra3oup2.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usra3drX,"_",input$usra3drY,"_",  
#   input$usra3inp2,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usra3oup2.h, width = input$usra3oup2.w, 
#   plot = scDRgene(user_conf(), user_meta(), input$usra3drX, input$usra3drY, input$usra3inp2,  
#   input$usra3sub1, input$usra3sub2, 
#   user_gexpr(), user_gene(), 
#   input$usra3siz, input$usra3col2, input$usra3ord2, 
#   input$usra3fsz, input$usra3asp, input$usra3txt,
#   dark_theme = dark_theme) ) 
#   }) 
#   
  ### Plots for tab b2 
#   output$usrb2sub1.ui <- renderUI({ 
#   sub = strsplit(user_conf()[UI == input$usrb2sub1]$fID, "\\|")[[1]] 
#   checkboxGroupInput("usrb2sub2", "Select which cells to show", inline = TRUE, 
#   choices = sub, selected = sub) 
#   }) 
#   observeEvent(input$usrb2sub1non, { 
#   sub = strsplit(user_conf()[UI == input$usrb2sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usrb2sub2", label = "Select which cells to show", 
#   choices = sub, selected = NULL, inline = TRUE) 
#   }) 
#   observeEvent(input$usrb2sub1all, { 
#   sub = strsplit(user_conf()[UI == input$usrb2sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usrb2sub2", label = "Select which cells to show", 
#   choices = sub, selected = sub, inline = TRUE) 
#   }) 
#   output$usrb2oup1 <- renderPlot({ 
#   scDRcoex(user_conf(), user_meta(), input$usrb2drX, input$usrb2drY,   
#   input$usrb2inp1, input$usrb2inp2, input$usrb2sub1, input$usrb2sub2, 
#   input$usrb2siz, input$usrb2col1, input$usrb2ord1, 
#   input$usrb2fsz, input$usrb2asp, input$usrb2txt,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usrb2oup1.ui <- renderUI({ 
#   plotOutput("usrb2oup1", height = pList2[input$usrb2psz]) 
#   }) 
#   output$usrb2oup1.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrb2drX,"_",input$usrb2drY,"_",  
#   input$usrb2inp1,"_",input$usrb2inp2,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usrb2oup1.h, width = input$usrb2oup1.w, useDingbats = FALSE, 
#   plot = scDRcoex(user_conf(), user_meta(), input$usrb2drX, input$usrb2drY,  
#   input$usrb2inp1, input$usrb2inp2, input$usrb2sub1, input$usrb2sub2, 
#   input$usrb2siz, input$usrb2col1, input$usrb2ord1, 
#   input$usrb2fsz, input$usrb2asp, input$usrb2txt,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usrb2oup1.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrb2drX,"_",input$usrb2drY,"_",  
#   input$usrb2inp1,"_",input$usrb2inp2,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usrb2oup1.h, width = input$usrb2oup1.w, 
#   plot = scDRcoex(user_conf(), user_meta(), input$usrb2drX, input$usrb2drY,  
#   input$usrb2inp1, input$usrb2inp2, input$usrb2sub1, input$usrb2sub2, 
#   input$usrb2siz, input$usrb2col1, input$usrb2ord1, 
#   input$usrb2fsz, input$usrb2asp, input$usrb2txt,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usrb2oup2 <- renderPlot({ 
#   scDRcoexLeg(input$usrb2inp1, input$usrb2inp2, input$usrb2col1, input$usrb2fsz,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usrb2oup2.ui <- renderUI({ 
#   plotOutput("usrb2oup2", height = "300px") 
#   }) 
#   output$usrb2oup2.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrb2drX,"_",input$usrb2drY,"_",  
#   input$usrb2inp1,"_",input$usrb2inp2,"_leg.pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = 6, width = 6, useDingbats = FALSE, 
#   plot = scDRcoexLeg(input$usrb2inp1, input$usrb2inp2, input$usrb2col1, input$usrb2fsz,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usrb2oup2.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrb2drX,"_",input$usrb2drY,"_",  
#   input$usrb2inp1,"_",input$usrb2inp2,"_leg.png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = 6, width = 6, 
#   plot = scDRcoexLeg(input$usrb2inp1, input$usrb2inp2, input$usrb2col1, input$usrb2fsz,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usrb2.dt <- renderDataTable({ 
#   ggData = scDRcoexNum(user_conf(), user_meta(), input$usrb2inp1, input$usrb2inp2, 
#   input$usrb2sub1, input$usrb2sub2, user_gexpr(), user_gene()) 
#   datatable(
#     ggData,
#     rownames = FALSE,
#     extensions = "Buttons",
#     server = TRUE,
#     options = list(
#       pageLength = 50,
#       lengthMenu = c(25, 50, 100),
#       deferRender = TRUE,
#       dom = "tBfrtip",
#       buttons = c("copy", "csv", "excel")
#     )
#   ) %>% 
#   formatRound(columns = c("pctExpress_gene1", "pctExpress_gene2"), digits = 2) 
#   }) 
#   
  ### Plots for tab c1 
#   output$usrc1sub1.ui <- renderUI({ 
#   sub = strsplit(user_conf()[UI == input$usrc1sub1]$fID, "\\|")[[1]] 
#   checkboxGroupInput("usrc1sub2", "Select which cells to show", inline = TRUE, 
#   choices = sub, selected = sub) 
#   }) 
#   observeEvent(input$usrc1sub1non, { 
#   sub = strsplit(user_conf()[UI == input$usrc1sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usrc1sub2", label = "Select which cells to show", 
#   choices = sub, selected = NULL, inline = TRUE) 
#   }) 
#   observeEvent(input$usrc1sub1all, { 
#   sub = strsplit(user_conf()[UI == input$usrc1sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usrc1sub2", label = "Select which cells to show", 
#   choices = sub, selected = sub, inline = TRUE) 
#   }) 
#   output$usrc1oup <- renderPlot({ 
#   scVioBox(user_conf(), user_meta(), input$usrc1inp1, input$usrc1inp2, 
#   input$usrc1sub1, input$usrc1sub2, input$usrc1pts, input$usrc1p2d, 
#   input$usrc1flp, input$usrc1fsz, input$usrc1dsp, input$usrc1vln,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usrc1oup.ui <- renderUI({ 
#   plotOutput("usrc1oup", height = pList2[input$usrc1psz]) 
#   }) 
#   output$usrc1oup.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrc1vln,"_",input$usrc1inp1,"_",  
#   input$usrc1inp2,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usrc1oup.h, width = input$usrc1oup.w, useDingbats = FALSE, 
#   plot = scVioBox(user_conf(), user_meta(), input$usrc1inp1, input$usrc1inp2, 
#   input$usrc1sub1, input$usrc1sub2, input$usrc1pts, input$usrc1p2d, 
#   input$usrc1flp, input$usrc1fsz, input$usrc1dsp, input$usrc1vln,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usrc1oup.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrc1vln,"_",input$usrc1inp1,"_",  
#   input$usrc1inp2,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usrc1oup.h, width = input$usrc1oup.w, 
#   plot = scVioBox(user_conf(), user_meta(), input$usrc1inp1, input$usrc1inp2, 
#   input$usrc1sub1, input$usrc1sub2, input$usrc1pts, input$usrc1p2d, 
#   input$usrc1flp, input$usrc1fsz, input$usrc1dsp, input$usrc1vln,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usrc1oup.dt <- renderDataTable({ 
#   ggData = scVioBoxnum(user_conf(), user_meta(), input$usrc1inp1, input$usrc1inp2, 
#   input$usrc1sub1, input$usrc1sub2, user_gexpr(), user_gene()) 
#   datatable(
#     ggData,
#     rownames = FALSE,
#     extensions = "Buttons",
#     server = TRUE,
#     options = list(
#       pageLength = 50,
#       lengthMenu = c(25, 50, 100),
#       deferRender = TRUE,
#       dom = "tBfrtip",
#       buttons = c("copy", "csv", "excel")
#     )
#   ) %>% 
#   formatRound(columns = c("meanExpr"), digits = 3) 
#   }) 
#   
  ### Plots for tab c2 
#   output$usrc2sub1.ui <- renderUI({ 
#   sub = strsplit(user_conf()[UI == input$usrc2sub1]$fID, "\\|")[[1]] 
#   checkboxGroupInput("usrc2sub2", "Select which cells to show", inline = TRUE, 
#   choices = sub, selected = sub) 
#   }) 
#   observeEvent(input$usrc2sub1non, { 
#   sub = strsplit(user_conf()[UI == input$usrc2sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usrc2sub2", label = "Select which cells to show", 
#   choices = sub, selected = NULL, inline = TRUE) 
#   }) 
#   observeEvent(input$usrc2sub1all, { 
#   sub = strsplit(user_conf()[UI == input$usrc2sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usrc2sub2", label = "Select which cells to show", 
#   choices = sub, selected = sub, inline = TRUE) 
#   }) 
#   output$usrc2oup <- renderPlot({ 
#   scProp(user_conf(), user_meta(), input$usrc2inp1, input$usrc2inp2,  
#   input$usrc2sub1, input$usrc2sub2, 
#   input$usrc2typ, input$usrc2flp, input$usrc2fsz,
#   dark_theme = is_dark_mode()) 
#   }) 
#   output$usrc2oup.ui <- renderUI({ 
#   plotOutput("usrc2oup", height = pList2[input$usrc2psz]) 
#   }) 
#   output$usrc2oup.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrc2typ,"_",input$usrc2inp1,"_",  
#   input$usrc2inp2,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usrc2oup.h, width = input$usrc2oup.w, useDingbats = FALSE, 
#   plot = scProp(user_conf(), user_meta(), input$usrc2inp1, input$usrc2inp2,  
#   input$usrc2sub1, input$usrc2sub2, 
#   input$usrc2typ, input$usrc2flp, input$usrc2fsz,
#   dark_theme = dark_theme) ) 
#   }) 
#   output$usrc2oup.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrc2typ,"_",input$usrc2inp1,"_",  
#   input$usrc2inp2,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usrc2oup.h, width = input$usrc2oup.w, 
#   plot = scProp(user_conf(), user_meta(), input$usrc2inp1, input$usrc2inp2,  
#   input$usrc2sub1, input$usrc2sub2, 
#   input$usrc2typ, input$usrc2flp, input$usrc2fsz,
#   dark_theme = dark_theme) ) 
#   }) 
#   
#   
  ### Plots for tab d1 
#   output$usrd1sub1.ui <- renderUI({ 
#   sub = strsplit(user_conf()[UI == input$usrd1sub1]$fID, "\\|")[[1]] 
#   checkboxGroupInput("usrd1sub2", "Select which cells to show", inline = TRUE, 
#   choices = sub, selected = sub) 
#   }) 
#   observeEvent(input$usrd1sub1non, { 
#   sub = strsplit(user_conf()[UI == input$usrd1sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usrd1sub2", label = "Select which cells to show", 
#   choices = sub, selected = NULL, inline = TRUE) 
#   }) 
#   observeEvent(input$usrd1sub1all, { 
#   sub = strsplit(user_conf()[UI == input$usrd1sub1]$fID, "\\|")[[1]] 
#   updateCheckboxGroupInput(session, inputId = "usrd1sub2", label = "Select which cells to show", 
#   choices = sub, selected = sub, inline = TRUE) 
#   }) 
#   output$usrd1oupTxt <- renderUI({ 
#   geneList = scGeneList(input$usrd1inp, user_gene()) 
#   if(nrow(geneList) > 50){ 
#   HTML("More than 50 input genes! Please reduce the gene list!") 
#   } else { 
#   oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
#   if(nrow(geneList[present == FALSE]) > 0){ 
#   oup = paste0(oup, "<br/>", 
#   nrow(geneList[present == FALSE]), " genes not found (", 
#   paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
#   } 
#   HTML(oup) 
#   } 
#   }) 
#   output$usrd1oup <- renderPlot({ 
#   scBubbHeat(user_conf(), user_meta(), input$usrd1inp, input$usrd1grp, input$usrd1plt, 
#   input$usrd1sub1, input$usrd1sub2, user_gexpr(), user_gene(), 
#   input$usrd1scl, input$usrd1row, input$usrd1col, 
#   input$usrd1cols, input$usrd1fsz, dark_theme = is_dark_mode()) 
#   }) 
#   output$usrd1oup.ui <- renderUI({ 
#   plotOutput("usrd1oup", height = pList3[input$usrd1psz]) 
#   }) 
#   output$usrd1oup.pdf <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrd1plt,"_",input$usrd1grp,".pdf") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "pdf", height = input$usrd1oup.h, width = input$usrd1oup.w, 
#   plot = scBubbHeat(user_conf(), user_meta(), input$usrd1inp, input$usrd1grp, input$usrd1plt, 
#   input$usrd1sub1, input$usrd1sub2, user_gexpr(), user_gene(), 
#   input$usrd1scl, input$usrd1row, input$usrd1col, 
#   input$usrd1cols, input$usrd1fsz, save = TRUE, dark_theme = dark_theme) ) 
#   }) 
#   output$usrd1oup.png <- downloadHandler( 
#   filename = function() { paste0("usr",input$usrd1plt,"_",input$usrd1grp,".png") }, 
#   content = function(file) { 
#   dark_theme <- isolate(is_dark_mode())
#   ggsave( 
#   file, device = "png", height = input$usrd1oup.h, width = input$usrd1oup.w, 
#   plot = scBubbHeat(user_conf(), user_meta(), input$usrd1inp, input$usrd1grp, input$usrd1plt, 
#   input$usrd1sub1, input$usrd1sub2, user_gexpr(), user_gene(), 
#   input$usrd1scl, input$usrd1row, input$usrd1col, 
#   input$usrd1cols, input$usrd1fsz, dark_theme = dark_theme) ) 
#   }) 
  observeEvent(input$home_card_nav, {
    # allow home hero cards to jump directly to their target tabs
    req(input$home_card_nav)
    updateNavbarPage(session, "mainTabs", selected = input$home_card_nav)
    session$sendCustomMessage("close-nav-dropdown", list(target = input$home_card_nav, delay = 280))
  })

  observeEvent(input$extended_tutorial_btn, {
    showModal(
      modalDialog(
        title = "Extended Tutorial",
        "Tutorial in progress",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })

  show_ra_download_modal <- function(entries,
                                     title = "Figure downloads",
                                     intro = "Choose a figure, file type, and export size.") {
    showModal(
      modalDialog(
        title = title,
        tags$div(
          class = "ra-download-modal-body",
          if (!is.null(intro) && nzchar(intro)) {
            tags$p(class = "ra-download-modal-intro", intro)
          } else {
            NULL
          },
          entries
        ),
        easyClose = TRUE,
        size = "l",
        class = "spg-modal ra-download-modal",
        footer = tagList(
          htmltools::tagAppendAttributes(
            modalButton("Close"),
            class = "spg-modal-close ra-modal-close"
          )
        )
      )
    )
  }

  resolve_download_filename <- function(custom_value, default_stem, extension) {
    custom_value <- as.character(custom_value)[1]
    if (is.na(custom_value)) {
      custom_value <- ""
    }
    custom_value <- trimws(custom_value)
    if (!nzchar(custom_value)) {
      return(paste0(default_stem, extension))
    }

    custom_value <- sub("\\.(pdf|png)$", "", custom_value, ignore.case = TRUE)
    custom_value <- gsub("[\\\\/:*?\"<>|]+", "_", custom_value)
    custom_value <- gsub("[[:cntrl:]]+", "", custom_value)
    custom_value <- trimws(custom_value)
    if (!nzchar(custom_value)) {
      return(paste0(default_stem, extension))
    }

    paste0(custom_value, extension)
  }

  download_dimension <- function(input_id, default_value) {
    value <- suppressWarnings(as.numeric(input[[input_id]]))
    if (!length(value) || !is.finite(value) || value <= 0) {
      return(default_value)
    }
    value
  }

  save_ggplot_pdf <- function(file, plot, width, height) {
    grDevices::cairo_pdf(file, width = width, height = height, onefile = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot)
  }

  save_ggplot_png <- function(file, plot, width, height) {
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      device = "png",
      width = width,
      height = height,
      units = "in",
      dpi = 300,
      bg = "transparent"
    )
  }

  bind_single_download_modal <- function(button_id,
                                         figure_title,
                                         pdf_id,
                                         png_id,
                                         filename_id,
                                         height_id,
                                         width_id,
                                         height_value,
                                         width_value,
                                         intro = "Choose a file type and export size.") {
    observeEvent(input[[button_id]], {
      show_ra_download_modal(
        entries = ra_download_entry(
          pdf_id = pdf_id,
          png_id = png_id,
          height_id = height_id,
          width_id = width_id,
          height_value = height_value,
          width_value = width_value,
          title = figure_title,
          filename_id = filename_id
        ),
        intro = intro
      )
    }, ignoreInit = TRUE)
  }

  bind_single_download_modal(
    button_id = "ra_dot_downloads_open",
    figure_title = "Retinoic Acid dotplot",
    pdf_id = "ra_dotplot_pdf",
    png_id = "ra_dotplot_png",
    filename_id = "ra_dotplot_name",
    height_id = "ra_dotplot_h",
    width_id = "ra_dotplot_w",
    height_value = 7.5,
    width_value = 12
  )

  bind_single_download_modal(
    button_id = "ra_line_downloads_open",
    figure_title = "Retinoic Acid lineplot",
    pdf_id = "ra_lineplot_pdf",
    png_id = "ra_lineplot_png",
    filename_id = "ra_lineplot_name",
    height_id = "ra_lineplot_h",
    width_id = "ra_lineplot_w",
    height_value = 7.5,
    width_value = 12
  )

  observeEvent(input$ccc_downloads_open, {
    show_ra_download_modal(
      title = "Figure downloads",
      intro = NULL,
      entries = tags$div(
        class = "ra-download-entry",
        tags$p(
          class = "ra-sub",
          "To download this figure, please hover over the plot to the right, and click the small camera icon to download it as a png."
        )
      )
    )
  }, ignoreInit = TRUE)

  bind_main_figures <- function(prefix, get_conf, get_meta) {
    make_id <- function(part) paste0(prefix, "mf", part)
    get_target_ui <- function() {
      resolve_ui_from_id(
        get_conf(),
        preferred_ids = c("correct_cellTypes", "correct_cellType"),
        fallback = "correct_cellTypes"
      )
    }

    get_groups <- function() {
      conf <- get_conf()
      target_ui <- get_target_ui()
      vals <- conf[UI == target_ui]$fID
      vals <- if (length(vals) && !is.na(vals[[1]])) vals[[1]] else ""
      if (!nzchar(vals)) {
        return(character(0))
      }
      strsplit(vals, "\\|")[[1]]
    }

    download_name <- function(name_id, default_stem, extension) {
      resolve_download_filename(input[[make_id(name_id)]], default_stem, extension)
    }

    observeEvent(input[[make_id("none")]], {
      sub <- get_groups()
      updateCheckboxGroupInput(
        session,
        inputId = make_id("cells"),
        label = NULL,
        choices = sub,
        selected = NULL,
        inline = FALSE
      )
    })

    observeEvent(input[[make_id("all")]], {
      sub <- get_groups()
      updateCheckboxGroupInput(
        session,
        inputId = make_id("cells"),
        label = NULL,
        choices = sub,
        selected = sub,
        inline = FALSE
      )
    })

    output[[make_id("main")]] <- renderPlot({
      sc_profile_eval(prefix, "main_figure", {
        target_ui <- get_target_ui()
        selected_groups <- as.character(input[[make_id("cells")]])
        pt_size <- input[[make_id("pt")]]
        if (is.null(pt_size) || !is.finite(pt_size)) {
          pt_size <- 2.5
        }
        show_labels <- isTRUE(input[[make_id("labels")]])
        sc_profile_note(
          selected_metadata = target_ui,
          selected_groups = selected_groups,
          selected_group_count = length(selected_groups),
          stage_split = FALSE
        )
        p <- with_dark(
          scDRcell,
          get_conf(),
          get_meta(),
          "UMAP1",
          "UMAP2",
          target_ui,
          target_ui,
          input[[make_id("cells")]],
          pt_size,              # point size
          "Blue-Yellow-Red",   # ignored for categorical
          "Original",
          "Medium",
          "Square",
          FALSE,               # show axis text
          show_labels,         # show labels
          stage_split = FALSE
        )
        p +
          ggplot2::theme(
            legend.position = "right",
            legend.direction = "vertical"
          ) +
          ggplot2::guides(
            color = ggplot2::guide_legend(
              ncol = 1,
              byrow = FALSE,
              override.aes = list(size = 4)
            ),
            fill = "none"
        )
      })
    })
    outputOptions(output, make_id("main"), suspendWhenHidden = TRUE)
    output[[make_id("main.pdf")]] <- downloadHandler(
      filename = function() download_name("main.name", paste0(prefix, "_main_figures_overview"), ".pdf"),
      content = function(file) {
        target_ui <- get_target_ui()
        pt_size <- input[[make_id("pt")]]
        if (is.null(pt_size) || !is.finite(pt_size)) {
          pt_size <- 2.5
        }
        show_labels <- isTRUE(input[[make_id("labels")]])
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("main.h")]],
          width = input[[make_id("main.w")]],
          useDingbats = FALSE,
          plot = with_dark_static(
            scDRcell,
            get_conf(),
            get_meta(),
            "UMAP1",
            "UMAP2",
            target_ui,
            target_ui,
            input[[make_id("cells")]],
            pt_size,
            "Blue-Yellow-Red",
            "Original",
            "Medium",
            "Square",
            FALSE,
            show_labels,
            stage_split = FALSE
          ) +
            ggplot2::theme(
              legend.position = "right",
              legend.direction = "vertical"
            ) +
            ggplot2::guides(
              color = ggplot2::guide_legend(
                ncol = 1,
                byrow = FALSE,
                override.aes = list(size = 4)
              ),
              fill = "none"
            )
        )
      }
    )
    output[[make_id("main.png")]] <- downloadHandler(
      filename = function() download_name("main.name", paste0(prefix, "_main_figures_overview"), ".png"),
      content = function(file) {
        target_ui <- get_target_ui()
        pt_size <- input[[make_id("pt")]]
        if (is.null(pt_size) || !is.finite(pt_size)) {
          pt_size <- 2.5
        }
        show_labels <- isTRUE(input[[make_id("labels")]])
        ggsave(
          file,
          device = "png",
          height = input[[make_id("main.h")]],
          width = input[[make_id("main.w")]],
          plot = with_dark_static(
            scDRcell,
            get_conf(),
            get_meta(),
            "UMAP1",
            "UMAP2",
            target_ui,
            target_ui,
            input[[make_id("cells")]],
            pt_size,
            "Blue-Yellow-Red",
            "Original",
            "Medium",
            "Square",
            FALSE,
            show_labels,
            stage_split = FALSE
          ) +
            ggplot2::theme(
              legend.position = "right",
              legend.direction = "vertical"
            ) +
            ggplot2::guides(
              color = ggplot2::guide_legend(
                ncol = 1,
                byrow = FALSE,
                override.aes = list(size = 4)
              ),
              fill = "none"
            )
        )
      }
    )

    output[[make_id("split")]] <- renderPlot({
      sc_profile_eval(prefix, "main_figure_split", {
        target_ui <- get_target_ui()
        selected_groups <- as.character(input[[make_id("cells")]])
        show_labels <- isTRUE(input[[make_id("labels")]])
        sc_profile_note(
          selected_metadata = target_ui,
          selected_groups = selected_groups,
          selected_group_count = length(selected_groups),
          stage_split = TRUE
        )
        p <- with_dark(
          scDRcell,
          get_conf(),
          get_meta(),
          "UMAP1",
          "UMAP2",
          target_ui,
          target_ui,
          input[[make_id("cells")]],
          0.9,                 # point size
          "Blue-Yellow-Red",
          "Original",
          "Small",
          "Square",
          FALSE,
          show_labels,
          stage_split = TRUE,
          stage_facet_ncol = 4
        )
        # Bottom row should show only the panels (no color legend) to avoid crowding.
        p + ggplot2::theme(legend.position = "none") +
          ggplot2::guides(color = "none", fill = "none")
      })
    })
    outputOptions(output, make_id("split"), suspendWhenHidden = TRUE)
    output[[make_id("split.pdf")]] <- downloadHandler(
      filename = function() download_name("split.name", paste0(prefix, "_main_figures_stage_split"), ".pdf"),
      content = function(file) {
        target_ui <- get_target_ui()
        show_labels <- isTRUE(input[[make_id("labels")]])
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("split.h")]],
          width = input[[make_id("split.w")]],
          useDingbats = FALSE,
          plot = with_dark_static(
            scDRcell,
            get_conf(),
            get_meta(),
            "UMAP1",
            "UMAP2",
            target_ui,
            target_ui,
            input[[make_id("cells")]],
            0.9,
            "Blue-Yellow-Red",
            "Original",
            "Small",
            "Square",
            FALSE,
            show_labels,
            stage_split = TRUE,
            stage_facet_ncol = 4
          ) +
            ggplot2::theme(legend.position = "none") +
            ggplot2::guides(color = "none", fill = "none")
        )
      }
    )
    output[[make_id("split.png")]] <- downloadHandler(
      filename = function() download_name("split.name", paste0(prefix, "_main_figures_stage_split"), ".png"),
      content = function(file) {
        target_ui <- get_target_ui()
        show_labels <- isTRUE(input[[make_id("labels")]])
        ggsave(
          file,
          device = "png",
          height = input[[make_id("split.h")]],
          width = input[[make_id("split.w")]],
          plot = with_dark_static(
            scDRcell,
            get_conf(),
            get_meta(),
            "UMAP1",
            "UMAP2",
            target_ui,
            target_ui,
            input[[make_id("cells")]],
            0.9,
            "Blue-Yellow-Red",
            "Original",
            "Small",
            "Square",
            FALSE,
            show_labels,
            stage_split = TRUE,
            stage_facet_ncol = 4
          ) +
            ggplot2::theme(legend.position = "none") +
            ggplot2::guides(color = "none", fill = "none")
        )
      }
    )

    observeEvent(input[[make_id("downloads_open")]], {
      show_ra_download_modal(
        entries = build_main_figures_download_entries(prefix),
        intro = "Export the overview UMAP or the stage-split panel at the size you need."
      )
    }, ignoreInit = TRUE)
  }

  bind_shinycell_dataset <- function(prefix,
                                     get_conf,
                                     get_meta,
                                     get_def,
                                     get_gene,
                                     gexpr_path) {
    make_id <- function(part) paste0(prefix, part)
    legend_enabled <- function(block, default = TRUE) {
      input_id <- make_id(paste0(block, "leg"))
      value <- input[[input_id]]
      if (is.null(value)) {
        return(isTRUE(default))
      }
      isTRUE(value)
    }
    apply_legend_visibility <- function(plot_obj, block, default = TRUE) {
      if (legend_enabled(block, default)) {
        return(plot_obj)
      }
      if (inherits(plot_obj, c("gg", "ggplot"))) {
        return(
          plot_obj +
            ggplot2::theme(legend.position = "none") +
            ggplot2::guides(
              color = "none",
              fill = "none",
              size = "none",
              shape = "none",
              linetype = "none",
              alpha = "none"
            )
        )
      }
      plot_obj
    }

    optCrt_local <- "{ option_create: function(data,escape) {return('<div class=\\\"create\\\"><strong>' + '</strong></div>');} }"
    choose_violin_y <- function(current, conf, def, genes) {
      numeric_mask <- is.na(conf$fID)
      if ("dimred" %in% names(conf)) {
        numeric_mask <- numeric_mask & (is.na(conf$dimred) | !conf$dimred)
      }
      numeric_meta <- unique(as.character(conf$UI[numeric_mask]))
      numeric_meta <- numeric_meta[!is.na(numeric_meta) & nzchar(numeric_meta)]

      gene_names <- names(genes)
      if (is.null(gene_names)) {
        gene_names <- character(0)
      }
      choices <- unique(c(numeric_meta, gene_names))
      if (!length(choices)) {
        return("")
      }

      current <- as.character(current)[1]
      if (!is.na(current) && nzchar(current) && current %in% choices) {
        return(current)
      }

      preferred_numeric <- c("Genes per cell", "UMIs per cell", "nFeature_SCT")
      preferred_hit <- preferred_numeric[preferred_numeric %in% numeric_meta]
      if (length(preferred_hit)) {
        return(preferred_hit[[1]])
      }

      defaults <- c(
        as.character(def$meta1)[1],
        as.character(def$meta2)[1],
        as.character(def$gene1)[1],
        numeric_meta[1],
        gene_names[1]
      )
      defaults <- defaults[!is.na(defaults) & nzchar(defaults)]
      defaults <- defaults[defaults %in% choices]
      if (length(defaults)) {
        return(defaults[[1]])
      }

      choices[[1]]
    }

    update_gene_selectize <- function(input_id, selected) {
      genes <- get_gene()
      req(!is.null(genes), length(genes) > 0, !is.null(names(genes)))

      updateSelectizeInput(
        session,
        input_id,
        choices = names(genes),
        server = TRUE,
        selected = selected,
        options = list(maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt_local))
      )
    }

    download_name <- function(name_id, default_stem, extension) {
      resolve_download_filename(input[[make_id(name_id)]], default_stem, extension)
    }

    update_violin_y_selectize <- function() {
      genes <- get_gene()
      conf <- get_conf()
      def <- get_def()
      numeric_mask <- is.na(conf$fID)
      if ("dimred" %in% names(conf)) {
        numeric_mask <- numeric_mask & (is.na(conf$dimred) | !conf$dimred)
      }
      numeric_meta <- unique(as.character(conf$UI[numeric_mask]))
      numeric_meta <- numeric_meta[!is.na(numeric_meta) & nzchar(numeric_meta)]
      current_y <- isolate(input[[make_id("c1inp2")]])
      selected_y <- choose_violin_y(current_y, conf, def, genes)
      updateSelectizeInput(
        session,
        make_id("c1inp2"),
        server = TRUE,
        choices = c(numeric_meta, names(genes)),
        selected = selected_y,
        options = list(
          maxOptions = length(numeric_meta) + 3,
          create = TRUE,
          persist = TRUE,
          render = I(optCrt_local)
        )
      )
    }

    update_active_tab_inputs <- function(tab_value = isolate(input$mainTabs)) {
      tab_value <- as.character(tab_value)[1]
      if (is.na(tab_value) || !startsWith(tab_value, paste0(prefix, "_"))) {
        return(invisible(FALSE))
      }

      suffix <- sub(paste0("^", prefix, "_"), "", tab_value)

      if (identical(suffix, "cellinfo_gene")) {
        def <- get_def()
        update_gene_selectize(make_id("a1inp2"), def$gene1)
      } else if (identical(suffix, "multiple_geneexpr")) {
        def <- get_def()
        update_gene_selectize(
          make_id("m1genes"),
          default_multiple_geneexpr_genes(prefix, get_gene(), def$gene1)
        )
      } else if (identical(suffix, "gene_gene")) {
        def <- get_def()
        update_gene_selectize(make_id("a3inp1"), def$gene1)
        update_gene_selectize(make_id("a3inp2"), def$gene2)
      } else if (identical(suffix, "gene_coexpression")) {
        def <- get_def()
        update_gene_selectize(make_id("b2inp1"), def$gene1)
        update_gene_selectize(make_id("b2inp2"), def$gene2)
      } else if (identical(suffix, "violin_boxplot")) {
        update_violin_y_selectize()
      }

      invisible(TRUE)
    }

    queue_active_tab_input_refresh <- function(tab_value = isolate(input$mainTabs)) {
      session$onFlushed(function() {
        update_active_tab_inputs(tab_value)
      }, once = TRUE)
      invisible(NULL)
    }

    observeEvent(input$mainTabs, {
      tab_value <- input$mainTabs
      if (is.null(tab_value) || !startsWith(tab_value, paste0(prefix, "_"))) {
        return()
      }
      queue_active_tab_input_refresh(tab_value)
    }, ignoreInit = FALSE)

    # ---- Helpers: subset checkbox UI + buttons ----
    bind_subset_controls <- function(block) {
      output[[make_id(paste0(block, "sub1.ui"))]] <- renderUI({
        conf <- get_conf()
        selected <- input[[make_id(paste0(block, "sub1"))]]
        sub <- strsplit(conf[UI == selected]$fID, "\\|")[[1]]
        checkboxGroupInput(
          make_id(paste0(block, "sub2")),
          "Select which cells to show",
          inline = TRUE,
          choices = sub,
          selected = sub
        )
      })

      observeEvent(input[[make_id(paste0(block, "sub1non"))]], {
        conf <- get_conf()
        selected <- input[[make_id(paste0(block, "sub1"))]]
        sub <- strsplit(conf[UI == selected]$fID, "\\|")[[1]]
        updateCheckboxGroupInput(
          session,
          inputId = make_id(paste0(block, "sub2")),
          label = "Select which cells to show",
          choices = sub,
          selected = NULL,
          inline = TRUE
        )
      })

      observeEvent(input[[make_id(paste0(block, "sub1all"))]], {
        conf <- get_conf()
        selected <- input[[make_id(paste0(block, "sub1"))]]
        sub <- strsplit(conf[UI == selected]$fID, "\\|")[[1]]
        updateCheckboxGroupInput(
          session,
          inputId = make_id(paste0(block, "sub2")),
          label = "Select which cells to show",
          choices = sub,
          selected = sub,
          inline = TRUE
        )
      })
    }

    bind_subset_controls("a1")
    bind_subset_controls("m1")
    bind_subset_controls("a2")
    bind_subset_controls("a3")
    bind_subset_controls("b2")
    bind_subset_controls("c1")
    bind_subset_controls("c2")
    bind_subset_controls("d1")

    bind_download_modal <- function(button_suffix,
                                    entries_builder,
                                    intro = "Choose a figure, file type, and export size.") {
      observeEvent(input[[make_id(button_suffix)]], {
        show_ra_download_modal(
          entries = entries_builder(),
          intro = intro
        )
      }, ignoreInit = TRUE)
    }

    bind_download_modal(
      "a1downloads_open",
      function() {
        tagList(
          build_plot_download_entry(prefix, "a1", "1", "Cell information overlay", 6, 8),
          build_plot_download_entry(prefix, "a1", "2", "Gene expression overlay", 6, 8)
        )
      }
    )
    bind_download_modal(
      "m1downloads_open",
      function() {
        tagList(
          build_plot_download_entry(prefix, "m1", "", "Multiple GeneExpr dotplot", 8, 10)
        )
      }
    )
    bind_download_modal(
      "a2downloads_open",
      function() {
        tagList(
          build_plot_download_entry(prefix, "a2", "1", "Cell information overlay 1", 6, 8),
          build_plot_download_entry(prefix, "a2", "2", "Cell information overlay 2", 6, 8)
        )
      }
    )
    bind_download_modal(
      "a3downloads_open",
      function() {
        tagList(
          build_plot_download_entry(prefix, "a3", "1", "Gene expression overlay 1", 6, 8),
          build_plot_download_entry(prefix, "a3", "2", "Gene expression overlay 2", 6, 8)
        )
      }
    )
    bind_download_modal(
      "b2downloads_open",
      function() {
        tagList(
          build_plot_download_entry(prefix, "b2", "1", "Coexpression view", 6, 8)
        )
      }
    )
    bind_download_modal(
      "c1downloads_open",
      function() {
        tagList(
          build_plot_download_entry(prefix, "c1", "", "Violin / boxplot figure", 8, 10)
        )
      }
    )
    bind_download_modal(
      "c2downloads_open",
      function() {
        tagList(
          build_plot_download_entry(prefix, "c2", "", "Proportion plot figure", 8, 10)
        )
      }
    )
    bind_download_modal(
      "d1downloads_open",
      function() {
        tagList(
          build_plot_download_entry(prefix, "d1", "", "Bubbleplot / heatmap figure", 10, 10)
        )
      }
    )

    c1_y_value <- reactive({
      choose_violin_y(input[[make_id("c1inp2")]], get_conf(), get_def(), get_gene())
    })

    multi_gene_upload_status <- reactiveVal(NULL)
    set_multi_gene_upload_status <- function(message = NULL, type = "info") {
      if (is.null(message) || !nzchar(trimws(as.character(message)))) {
        multi_gene_upload_status(NULL)
      } else {
        multi_gene_upload_status(list(message = as.character(message), type = as.character(type)))
      }
      invisible(NULL)
    }

    output[[make_id("m1upload_status")]] <- renderUI({
      status <- multi_gene_upload_status()
      if (is.null(status)) {
        return(NULL)
      }
      alert_class <- switch(
        tolower(status$type),
        error = "alert alert-danger",
        warning = "alert alert-warning",
        success = "alert alert-success",
        "alert alert-info"
      )
      tags$div(
        class = alert_class,
        style = "margin-top:12px;",
        status$message
      )
    })

    observeEvent(input[[make_id("m1upload_open")]], {
      set_multi_gene_upload_status(NULL)
      show_ra_download_modal(
        title = "Upload Gene List",
        intro = "Upload a CSV file containing exactly one uniquely named column called GeneName. Values in GeneName will be used as selected genes.",
        entries = tagList(
          tags$div(
            class = "ra-download-entry",
            tags$div(class = "ra-download-entry-title", "CSV requirements"),
            tags$ul(
              style = "margin:10px 0 12px 18px;",
              tags$li("File type must be .csv"),
              tags$li("Header must contain one column named GeneName"),
              tags$li("GeneName column name cannot be duplicated"),
              tags$li("At least one valid gene name must be present")
            ),
            fileInput(
              inputId = make_id("m1upload_file"),
              label = "Gene list CSV",
              accept = c(".csv", "text/csv", "text/comma-separated-values")
            ),
            actionButton(
              inputId = make_id("m1upload_apply"),
              label = "Use Uploaded Genes",
              class = "ra-btn ra-download-btn ra-sidebar-action-btn"
            ),
            uiOutput(make_id("m1upload_status"))
          )
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input[[make_id("m1upload_apply")]], {
      upload <- input[[make_id("m1upload_file")]]
      set_multi_gene_upload_status(NULL)

      if (is.null(upload) || is.null(upload$datapath) || !nzchar(upload$datapath)) {
        set_multi_gene_upload_status("Please choose a CSV file before applying.", "error")
        return()
      }

      ext <- tolower(tools::file_ext(upload$name %||% ""))
      if (!identical(ext, "csv")) {
        set_multi_gene_upload_status("Uploaded file must be a CSV (.csv).", "error")
        return()
      }

      csv_data <- tryCatch(
        read.csv(upload$datapath, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) e
      )
      if (inherits(csv_data, "error")) {
        set_multi_gene_upload_status(
          sprintf("Unable to read CSV file: %s", conditionMessage(csv_data)),
          "error"
        )
        return()
      }

      col_names <- names(csv_data)
      col_names <- trimws(sub("^\ufeff", "", col_names))
      gene_col_matches <- which(col_names == "GeneName")
      if (!length(gene_col_matches)) {
        set_multi_gene_upload_status("CSV is missing the required GeneName column.", "error")
        return()
      }
      if (length(gene_col_matches) > 1) {
        set_multi_gene_upload_status("CSV contains duplicate GeneName columns. Keep exactly one.", "error")
        return()
      }

      uploaded_genes <- trimws(as.character(csv_data[[gene_col_matches[[1]]]]))
      uploaded_genes <- uploaded_genes[!is.na(uploaded_genes) & nzchar(uploaded_genes)]
      uploaded_genes <- unique(uploaded_genes)
      if (!length(uploaded_genes)) {
        set_multi_gene_upload_status("No valid gene names were provided in GeneName.", "error")
        return()
      }

      parsed <- scParseGeneVector(uploaded_genes, get_gene())
      if (!length(parsed$valid)) {
        set_multi_gene_upload_status("No valid gene names were provided in GeneName.", "error")
        return()
      }

      update_gene_selectize(make_id("m1genes"), parsed$valid)
      removeModal()

      if (length(parsed$missing)) {
        showNotification(
          sprintf(
            "Loaded %d gene(s). Ignored %d not found in this dataset.",
            length(parsed$valid),
            length(parsed$missing)
          ),
          type = "warning",
          duration = 8
        )
      } else {
        showNotification(
          sprintf("Loaded %d gene(s) from GeneName.", length(parsed$valid)),
          type = "message",
          duration = 5
        )
      }
    }, ignoreInit = TRUE)

    output[[make_id("m1oupTxt")]] <- renderUI({
      parsed <- scParseGeneVector(input[[make_id("m1genes")]], get_gene())
      if (!length(parsed$input)) {
        return(HTML("Select genes manually or upload a CSV list."))
      }
      if (!length(parsed$valid)) {
        return(HTML("No valid genes found in the current dataset."))
      }

      msg <- paste0(length(parsed$valid), " gene(s) selected")
      if (length(parsed$missing)) {
        preview <- paste(head(parsed$missing, 6), collapse = ", ")
        if (length(parsed$missing) > 6) {
          preview <- paste0(preview, ", ...")
        }
        msg <- paste0(
          msg,
          "<br/>",
          length(parsed$missing),
          " gene(s) not found in this dataset (",
          preview,
          ")"
        )
      }
      HTML(msg)
    })

    m1_data <- reactive({
      data <- scDRnumMulti(
        get_conf(),
        get_meta(),
        input[[make_id("m1grp")]],
        input[[make_id("m1genes")]],
        input[[make_id("m1sub1")]],
        input[[make_id("m1sub2")]],
        gexpr_path,
        get_gene()
      )
      sc_profile_note(
        selected_genes = as.character(data$valid_genes),
        gene_count = length(data$valid_genes),
        selected_grouping = as.character(input[[make_id("m1grp")]]),
        selected_group_count = length(unique(as.character(data$table_data$group)))
      )
      data
    })

    output[[make_id("m1oup")]] <- renderPlot({
      sc_profile_eval(prefix, "multiple_geneexpr_dotplot", {
        plot_data <- m1_data()
        apply_legend_visibility(
          with_dark(
            scMultiGeneDotPlot,
            plot_data$dot_data,
            input[[make_id("m1fsz")]],
            input[[make_id("m1dsz")]]
          ),
          "m1"
        )
      })
    })
    output[[make_id("m1oup.ui")]] <- renderUI({
      height <- pList2[input[[make_id("m1psz")]]]
      sc_spinner_plot_output(make_id("m1oup"), height = height, proxy.height = height)
    })
    output[[make_id("m1oup.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "m1oup.name",
        paste0(prefix, "_multiple_geneexpr_", input[[make_id("m1grp")]]),
        ".pdf"
      ),
      content = function(file) {
        plot_data <- m1_data()
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("m1oup.h")]],
          width = input[[make_id("m1oup.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scMultiGeneDotPlot,
              plot_data$dot_data,
              input[[make_id("m1fsz")]],
              input[[make_id("m1dsz")]]
            ),
            "m1"
          )
        )
      }
    )
    output[[make_id("m1oup.png")]] <- downloadHandler(
      filename = function() download_name(
        "m1oup.name",
        paste0(prefix, "_multiple_geneexpr_", input[[make_id("m1grp")]]),
        ".png"
      ),
      content = function(file) {
        plot_data <- m1_data()
        ggsave(
          file,
          device = "png",
          height = input[[make_id("m1oup.h")]],
          width = input[[make_id("m1oup.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scMultiGeneDotPlot,
              plot_data$dot_data,
              input[[make_id("m1fsz")]],
              input[[make_id("m1dsz")]]
            ),
            "m1"
          )
        )
      }
    )

    output[[make_id("m1.dt")]] <- renderDataTable({
      table_data <- m1_data()$table_data
      pct_cols <- grep("^pctExpress_", names(table_data), value = TRUE)
      dt <- datatable(
        table_data,
        rownames = FALSE,
        class = "stripe hover nowrap",
        extensions = "Buttons",
        options = list(
          pageLength = 50,
          lengthMenu = c(25, 50, 100),
          deferRender = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          scrollX = TRUE,
          scrollCollapse = TRUE,
          autoWidth = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "  var api = this.api();",
            "  $(api.table().container()).css('width', '100%');",
            "  api.columns.adjust();",
            "  setTimeout(function(){ api.columns.adjust(); }, 80);",
            "  setTimeout(function(){ api.columns.adjust(); }, 260);",
            "}"
          )
        ),
        callback = JS(
          "var adjustM1 = function(){ table.columns.adjust(); };",
          "setTimeout(adjustM1, 0);",
          "setTimeout(adjustM1, 120);",
          "setTimeout(adjustM1, 320);",
          "$(document).off('shown.bs.tab.m1dt').on('shown.bs.tab.m1dt', function(){ setTimeout(adjustM1, 50); });",
          "var rowgroup = $(table.table().container()).closest('.ra-rowgroup');",
          "rowgroup.find('.ra-advanced-toggle').off('click.m1dt').on('click.m1dt', function(){",
          "  setTimeout(adjustM1, 40);",
          "  setTimeout(adjustM1, 220);",
          "});",
          "table.on('draw.dt', function(){ table.columns.adjust(); });",
          "$(window).on('resize', function(){ table.columns.adjust(); });"
        )
      )
      if (length(pct_cols)) {
        dt <- dt %>% formatRound(columns = pct_cols, digits = 2)
      }
      dt
    }, server = FALSE)

    # ---- Tab a1: CellInfo vs GeneExpr ----
    output[[make_id("a1oup1")]] <- renderPlot({
      apply_legend_visibility(
        with_dark(
          scDRcell,
          get_conf(),
          get_meta(),
          input[[make_id("a1drX")]],
          input[[make_id("a1drY")]],
          input[[make_id("a1inp1")]],
          input[[make_id("a1sub1")]],
          input[[make_id("a1sub2")]],
          input[[make_id("a1siz")]],
          input[[make_id("a1col1")]],
          input[[make_id("a1ord1")]],
          input[[make_id("a1fsz")]],
          input[[make_id("a1asp")]],
          input[[make_id("a1txt")]],
          input[[make_id("a1lab1")]],
          stage_split = input[[make_id("a1split")]]
        ),
        "a1"
      )
    })
    output[[make_id("a1oup1.ui")]] <- renderUI({
      height <- pList[input[[make_id("a1psz")]]]
      sc_spinner_plot_output(make_id("a1oup1"), height = height, proxy.height = height)
    })
    output[[make_id("a1oup1.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "a1oup1.name",
        paste0(prefix, input[[make_id("a1drX")]], "_", input[[make_id("a1drY")]], "_", input[[make_id("a1inp1")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("a1oup1.h")]],
          width = input[[make_id("a1oup1.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scDRcell,
              get_conf(),
              get_meta(),
              input[[make_id("a1drX")]],
              input[[make_id("a1drY")]],
              input[[make_id("a1inp1")]],
              input[[make_id("a1sub1")]],
              input[[make_id("a1sub2")]],
              input[[make_id("a1siz")]],
              input[[make_id("a1col1")]],
              input[[make_id("a1ord1")]],
              input[[make_id("a1fsz")]],
              input[[make_id("a1asp")]],
              input[[make_id("a1txt")]],
              input[[make_id("a1lab1")]],
              stage_split = input[[make_id("a1split")]]
            ),
            "a1"
          )
        )
      }
    )
    output[[make_id("a1oup1.png")]] <- downloadHandler(
      filename = function() download_name(
        "a1oup1.name",
        paste0(prefix, input[[make_id("a1drX")]], "_", input[[make_id("a1drY")]], "_", input[[make_id("a1inp1")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("a1oup1.h")]],
          width = input[[make_id("a1oup1.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scDRcell,
              get_conf(),
              get_meta(),
              input[[make_id("a1drX")]],
              input[[make_id("a1drY")]],
              input[[make_id("a1inp1")]],
              input[[make_id("a1sub1")]],
              input[[make_id("a1sub2")]],
              input[[make_id("a1siz")]],
              input[[make_id("a1col1")]],
              input[[make_id("a1ord1")]],
              input[[make_id("a1fsz")]],
              input[[make_id("a1asp")]],
              input[[make_id("a1txt")]],
              input[[make_id("a1lab1")]],
              stage_split = input[[make_id("a1split")]]
            ),
            "a1"
          )
        )
      }
    )

    output[[make_id("a1.dt")]] <- renderDataTable({
      ggData <- scDRnum(
        get_conf(),
        get_meta(),
        input[[make_id("a1inp1")]],
        input[[make_id("a1inp2")]],
        input[[make_id("a1sub1")]],
        input[[make_id("a1sub2")]],
        gexpr_path,
        get_gene(),
        input[[make_id("a1splt")]]
      )
      datatable(
        ggData,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 50,
          lengthMenu = c(25, 50, 100),
          deferRender = TRUE,
          dom = "tBfrtip",
          buttons = c("copy", "csv", "excel")
        )
      ) %>% formatRound(columns = c("pctExpress"), digits = 2)
    }, server = TRUE)

    output[[make_id("a1oup2")]] <- renderPlot({
      sc_profile_eval(prefix, "gene_umap_primary", {
        apply_legend_visibility(
          with_dark(
            scDRgene,
            get_conf(),
            get_meta(),
            input[[make_id("a1drX")]],
            input[[make_id("a1drY")]],
            input[[make_id("a1inp2")]],
            input[[make_id("a1sub1")]],
            input[[make_id("a1sub2")]],
            gexpr_path,
            get_gene(),
            input[[make_id("a1siz")]],
            input[[make_id("a1col2")]],
            input[[make_id("a1ord2")]],
            input[[make_id("a1fsz")]],
            input[[make_id("a1asp")]],
            input[[make_id("a1txt")]],
            stage_split = input[[make_id("a1split")]]
          ),
          "a1"
        )
      })
    })
    output[[make_id("a1oup2.ui")]] <- renderUI({
      height <- pList[input[[make_id("a1psz")]]]
      sc_spinner_plot_output(make_id("a1oup2"), height = height, proxy.height = height)
    })
    output[[make_id("a1oup2.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "a1oup2.name",
        paste0(prefix, input[[make_id("a1drX")]], "_", input[[make_id("a1drY")]], "_", input[[make_id("a1inp2")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("a1oup2.h")]],
          width = input[[make_id("a1oup2.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scDRgene,
              get_conf(),
              get_meta(),
              input[[make_id("a1drX")]],
              input[[make_id("a1drY")]],
              input[[make_id("a1inp2")]],
              input[[make_id("a1sub1")]],
              input[[make_id("a1sub2")]],
              gexpr_path,
              get_gene(),
              input[[make_id("a1siz")]],
              input[[make_id("a1col2")]],
              input[[make_id("a1ord2")]],
              input[[make_id("a1fsz")]],
              input[[make_id("a1asp")]],
              input[[make_id("a1txt")]],
              stage_split = input[[make_id("a1split")]]
            ),
            "a1"
          )
        )
      }
    )
    output[[make_id("a1oup2.png")]] <- downloadHandler(
      filename = function() download_name(
        "a1oup2.name",
        paste0(prefix, input[[make_id("a1drX")]], "_", input[[make_id("a1drY")]], "_", input[[make_id("a1inp2")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("a1oup2.h")]],
          width = input[[make_id("a1oup2.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scDRgene,
              get_conf(),
              get_meta(),
              input[[make_id("a1drX")]],
              input[[make_id("a1drY")]],
              input[[make_id("a1inp2")]],
              input[[make_id("a1sub1")]],
              input[[make_id("a1sub2")]],
              gexpr_path,
              get_gene(),
              input[[make_id("a1siz")]],
              input[[make_id("a1col2")]],
              input[[make_id("a1ord2")]],
              input[[make_id("a1fsz")]],
              input[[make_id("a1asp")]],
              input[[make_id("a1txt")]],
              stage_split = input[[make_id("a1split")]]
            ),
            "a1"
          )
        )
      }
    )

    # ---- Tab a2: CellInfo vs CellInfo ----
    output[[make_id("a2oup1")]] <- renderPlot({
      apply_legend_visibility(
        with_dark(
          scDRcell,
          get_conf(),
          get_meta(),
          input[[make_id("a2drX")]],
          input[[make_id("a2drY")]],
          input[[make_id("a2inp1")]],
          input[[make_id("a2sub1")]],
          input[[make_id("a2sub2")]],
          input[[make_id("a2siz")]],
          input[[make_id("a2col1")]],
          input[[make_id("a2ord1")]],
          input[[make_id("a2fsz")]],
          input[[make_id("a2asp")]],
          input[[make_id("a2txt")]],
          input[[make_id("a2lab1")]],
          stage_split = input[[make_id("a2split")]]
        ),
        "a2"
      )
    })
    output[[make_id("a2oup1.ui")]] <- renderUI({
      height <- pList[input[[make_id("a2psz")]]]
      sc_spinner_plot_output(make_id("a2oup1"), height = height, proxy.height = height)
    })
    output[[make_id("a2oup1.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "a2oup1.name",
        paste0(prefix, input[[make_id("a2drX")]], "_", input[[make_id("a2drY")]], "_", input[[make_id("a2inp1")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("a2oup1.h")]],
          width = input[[make_id("a2oup1.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scDRcell,
              get_conf(),
              get_meta(),
              input[[make_id("a2drX")]],
              input[[make_id("a2drY")]],
              input[[make_id("a2inp1")]],
              input[[make_id("a2sub1")]],
              input[[make_id("a2sub2")]],
              input[[make_id("a2siz")]],
              input[[make_id("a2col1")]],
              input[[make_id("a2ord1")]],
              input[[make_id("a2fsz")]],
              input[[make_id("a2asp")]],
              input[[make_id("a2txt")]],
              input[[make_id("a2lab1")]],
              stage_split = input[[make_id("a2split")]]
            ),
            "a2"
          )
        )
      }
    )
    output[[make_id("a2oup1.png")]] <- downloadHandler(
      filename = function() download_name(
        "a2oup1.name",
        paste0(prefix, input[[make_id("a2drX")]], "_", input[[make_id("a2drY")]], "_", input[[make_id("a2inp1")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("a2oup1.h")]],
          width = input[[make_id("a2oup1.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scDRcell,
              get_conf(),
              get_meta(),
              input[[make_id("a2drX")]],
              input[[make_id("a2drY")]],
              input[[make_id("a2inp1")]],
              input[[make_id("a2sub1")]],
              input[[make_id("a2sub2")]],
              input[[make_id("a2siz")]],
              input[[make_id("a2col1")]],
              input[[make_id("a2ord1")]],
              input[[make_id("a2fsz")]],
              input[[make_id("a2asp")]],
              input[[make_id("a2txt")]],
              input[[make_id("a2lab1")]],
              stage_split = input[[make_id("a2split")]]
            ),
            "a2"
          )
        )
      }
    )

    output[[make_id("a2oup2")]] <- renderPlot({
      apply_legend_visibility(
        with_dark(
          scDRcell,
          get_conf(),
          get_meta(),
          input[[make_id("a2drX")]],
          input[[make_id("a2drY")]],
          input[[make_id("a2inp2")]],
          input[[make_id("a2sub1")]],
          input[[make_id("a2sub2")]],
          input[[make_id("a2siz")]],
          input[[make_id("a2col2")]],
          input[[make_id("a2ord2")]],
          input[[make_id("a2fsz")]],
          input[[make_id("a2asp")]],
          input[[make_id("a2txt")]],
          input[[make_id("a2lab2")]],
          stage_split = input[[make_id("a2split")]]
        ),
        "a2"
      )
    })
    output[[make_id("a2oup2.ui")]] <- renderUI({
      height <- pList[input[[make_id("a2psz")]]]
      sc_spinner_plot_output(make_id("a2oup2"), height = height, proxy.height = height)
    })
    output[[make_id("a2oup2.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "a2oup2.name",
        paste0(prefix, input[[make_id("a2drX")]], "_", input[[make_id("a2drY")]], "_", input[[make_id("a2inp2")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("a2oup2.h")]],
          width = input[[make_id("a2oup2.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scDRcell,
              get_conf(),
              get_meta(),
              input[[make_id("a2drX")]],
              input[[make_id("a2drY")]],
              input[[make_id("a2inp2")]],
              input[[make_id("a2sub1")]],
              input[[make_id("a2sub2")]],
              input[[make_id("a2siz")]],
              input[[make_id("a2col2")]],
              input[[make_id("a2ord2")]],
              input[[make_id("a2fsz")]],
              input[[make_id("a2asp")]],
              input[[make_id("a2txt")]],
              input[[make_id("a2lab2")]],
              stage_split = input[[make_id("a2split")]]
            ),
            "a2"
          )
        )
      }
    )
    output[[make_id("a2oup2.png")]] <- downloadHandler(
      filename = function() download_name(
        "a2oup2.name",
        paste0(prefix, input[[make_id("a2drX")]], "_", input[[make_id("a2drY")]], "_", input[[make_id("a2inp2")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("a2oup2.h")]],
          width = input[[make_id("a2oup2.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scDRcell,
              get_conf(),
              get_meta(),
              input[[make_id("a2drX")]],
              input[[make_id("a2drY")]],
              input[[make_id("a2inp2")]],
              input[[make_id("a2sub1")]],
              input[[make_id("a2sub2")]],
              input[[make_id("a2siz")]],
              input[[make_id("a2col2")]],
              input[[make_id("a2ord2")]],
              input[[make_id("a2fsz")]],
              input[[make_id("a2asp")]],
              input[[make_id("a2txt")]],
              input[[make_id("a2lab2")]],
              stage_split = input[[make_id("a2split")]]
            ),
            "a2"
          )
        )
      }
    )

    # ---- Tab a3: GeneExpr vs GeneExpr ----
    output[[make_id("a3oup1")]] <- renderPlot({
      sc_profile_eval(prefix, "gene_umap_compare_left", {
        apply_legend_visibility(
          with_dark(
            scDRgene,
            get_conf(),
            get_meta(),
            input[[make_id("a3drX")]],
            input[[make_id("a3drY")]],
            input[[make_id("a3inp1")]],
            input[[make_id("a3sub1")]],
            input[[make_id("a3sub2")]],
            gexpr_path,
            get_gene(),
            input[[make_id("a3siz")]],
            input[[make_id("a3col1")]],
            input[[make_id("a3ord1")]],
            input[[make_id("a3fsz")]],
            input[[make_id("a3asp")]],
            input[[make_id("a3txt")]],
            stage_split = input[[make_id("a3split")]]
          ),
          "a3"
        )
      })
    })
    output[[make_id("a3oup1.ui")]] <- renderUI({
      height <- pList[input[[make_id("a3psz")]]]
      sc_spinner_plot_output(make_id("a3oup1"), height = height, proxy.height = height)
    })
    output[[make_id("a3oup1.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "a3oup1.name",
        paste0(prefix, input[[make_id("a3drX")]], "_", input[[make_id("a3drY")]], "_", input[[make_id("a3inp1")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("a3oup1.h")]],
          width = input[[make_id("a3oup1.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scDRgene,
              get_conf(),
              get_meta(),
              input[[make_id("a3drX")]],
              input[[make_id("a3drY")]],
              input[[make_id("a3inp1")]],
              input[[make_id("a3sub1")]],
              input[[make_id("a3sub2")]],
              gexpr_path,
              get_gene(),
              input[[make_id("a3siz")]],
              input[[make_id("a3col1")]],
              input[[make_id("a3ord1")]],
              input[[make_id("a3fsz")]],
              input[[make_id("a3asp")]],
              input[[make_id("a3txt")]],
              stage_split = input[[make_id("a3split")]]
            ),
            "a3"
          )
        )
      }
    )
    output[[make_id("a3oup1.png")]] <- downloadHandler(
      filename = function() download_name(
        "a3oup1.name",
        paste0(prefix, input[[make_id("a3drX")]], "_", input[[make_id("a3drY")]], "_", input[[make_id("a3inp1")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("a3oup1.h")]],
          width = input[[make_id("a3oup1.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scDRgene,
              get_conf(),
              get_meta(),
              input[[make_id("a3drX")]],
              input[[make_id("a3drY")]],
              input[[make_id("a3inp1")]],
              input[[make_id("a3sub1")]],
              input[[make_id("a3sub2")]],
              gexpr_path,
              get_gene(),
              input[[make_id("a3siz")]],
              input[[make_id("a3col1")]],
              input[[make_id("a3ord1")]],
              input[[make_id("a3fsz")]],
              input[[make_id("a3asp")]],
              input[[make_id("a3txt")]],
              stage_split = input[[make_id("a3split")]]
            ),
            "a3"
          )
        )
      }
    )

    output[[make_id("a3oup2")]] <- renderPlot({
      sc_profile_eval(prefix, "gene_umap_compare_right", {
        apply_legend_visibility(
          with_dark(
            scDRgene,
            get_conf(),
            get_meta(),
            input[[make_id("a3drX")]],
            input[[make_id("a3drY")]],
            input[[make_id("a3inp2")]],
            input[[make_id("a3sub1")]],
            input[[make_id("a3sub2")]],
            gexpr_path,
            get_gene(),
            input[[make_id("a3siz")]],
            input[[make_id("a3col2")]],
            input[[make_id("a3ord2")]],
            input[[make_id("a3fsz")]],
            input[[make_id("a3asp")]],
            input[[make_id("a3txt")]],
            stage_split = input[[make_id("a3split")]]
          ),
          "a3"
        )
      })
    })
    output[[make_id("a3oup2.ui")]] <- renderUI({
      height <- pList[input[[make_id("a3psz")]]]
      sc_spinner_plot_output(make_id("a3oup2"), height = height, proxy.height = height)
    })
    output[[make_id("a3oup2.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "a3oup2.name",
        paste0(prefix, input[[make_id("a3drX")]], "_", input[[make_id("a3drY")]], "_", input[[make_id("a3inp2")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("a3oup2.h")]],
          width = input[[make_id("a3oup2.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scDRgene,
              get_conf(),
              get_meta(),
              input[[make_id("a3drX")]],
              input[[make_id("a3drY")]],
              input[[make_id("a3inp2")]],
              input[[make_id("a3sub1")]],
              input[[make_id("a3sub2")]],
              gexpr_path,
              get_gene(),
              input[[make_id("a3siz")]],
              input[[make_id("a3col2")]],
              input[[make_id("a3ord2")]],
              input[[make_id("a3fsz")]],
              input[[make_id("a3asp")]],
              input[[make_id("a3txt")]],
              stage_split = input[[make_id("a3split")]]
            ),
            "a3"
          )
        )
      }
    )
    output[[make_id("a3oup2.png")]] <- downloadHandler(
      filename = function() download_name(
        "a3oup2.name",
        paste0(prefix, input[[make_id("a3drX")]], "_", input[[make_id("a3drY")]], "_", input[[make_id("a3inp2")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("a3oup2.h")]],
          width = input[[make_id("a3oup2.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scDRgene,
              get_conf(),
              get_meta(),
              input[[make_id("a3drX")]],
              input[[make_id("a3drY")]],
              input[[make_id("a3inp2")]],
              input[[make_id("a3sub1")]],
              input[[make_id("a3sub2")]],
              gexpr_path,
              get_gene(),
              input[[make_id("a3siz")]],
              input[[make_id("a3col2")]],
              input[[make_id("a3ord2")]],
              input[[make_id("a3fsz")]],
              input[[make_id("a3asp")]],
              input[[make_id("a3txt")]],
              stage_split = input[[make_id("a3split")]]
            ),
            "a3"
          )
        )
      }
    )

    # ---- Tab b2: Gene coexpression ----
    output[[make_id("b2oup1")]] <- renderPlot({
      sc_profile_eval(prefix, "coexpression_umap", {
        apply_legend_visibility(
          with_dark(
            scDRcoex,
            get_conf(),
            get_meta(),
            input[[make_id("b2drX")]],
            input[[make_id("b2drY")]],
            input[[make_id("b2inp1")]],
            input[[make_id("b2inp2")]],
            input[[make_id("b2sub1")]],
            input[[make_id("b2sub2")]],
            gexpr_path,
            get_gene(),
            input[[make_id("b2siz")]],
            input[[make_id("b2col1")]],
            input[[make_id("b2ord1")]],
            input[[make_id("b2fsz")]],
            input[[make_id("b2asp")]],
            input[[make_id("b2txt")]],
            stage_split = input[[make_id("b2split")]]
          ),
          "b2"
        )
      })
    })
    output[[make_id("b2oup1.ui")]] <- renderUI({
      height <- pList2[input[[make_id("b2psz")]]]
      sc_spinner_plot_output(make_id("b2oup1"), height = height, proxy.height = height)
    })
    output[[make_id("b2oup1.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "b2oup1.name",
        paste0(prefix, input[[make_id("b2drX")]], "_", input[[make_id("b2drY")]], "_", input[[make_id("b2inp1")]], "_", input[[make_id("b2inp2")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("b2oup1.h")]],
          width = input[[make_id("b2oup1.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scDRcoex,
              get_conf(),
              get_meta(),
              input[[make_id("b2drX")]],
              input[[make_id("b2drY")]],
              input[[make_id("b2inp1")]],
              input[[make_id("b2inp2")]],
              input[[make_id("b2sub1")]],
              input[[make_id("b2sub2")]],
              gexpr_path,
              get_gene(),
              input[[make_id("b2siz")]],
              input[[make_id("b2col1")]],
              input[[make_id("b2ord1")]],
              input[[make_id("b2fsz")]],
              input[[make_id("b2asp")]],
              input[[make_id("b2txt")]],
              stage_split = input[[make_id("b2split")]]
            ),
            "b2"
          )
        )
      }
    )
    output[[make_id("b2oup1.png")]] <- downloadHandler(
      filename = function() download_name(
        "b2oup1.name",
        paste0(prefix, input[[make_id("b2drX")]], "_", input[[make_id("b2drY")]], "_", input[[make_id("b2inp1")]], "_", input[[make_id("b2inp2")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("b2oup1.h")]],
          width = input[[make_id("b2oup1.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scDRcoex,
              get_conf(),
              get_meta(),
              input[[make_id("b2drX")]],
              input[[make_id("b2drY")]],
              input[[make_id("b2inp1")]],
              input[[make_id("b2inp2")]],
              input[[make_id("b2sub1")]],
              input[[make_id("b2sub2")]],
              gexpr_path,
              get_gene(),
              input[[make_id("b2siz")]],
              input[[make_id("b2col1")]],
              input[[make_id("b2ord1")]],
              input[[make_id("b2fsz")]],
              input[[make_id("b2asp")]],
              input[[make_id("b2txt")]],
              stage_split = input[[make_id("b2split")]]
            ),
            "b2"
          )
        )
      }
    )

    output[[make_id("b2oup2")]] <- renderPlot({
      with_dark(
        scDRcoexLeg,
        input[[make_id("b2inp1")]],
        input[[make_id("b2inp2")]],
        input[[make_id("b2col1")]],
        input[[make_id("b2fsz")]]
      )
    })
    output[[make_id("b2oup2.pdf")]] <- downloadHandler(
      filename = function() paste0(prefix, "_coex_legend.pdf"),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = 3,
          width = 5,
          useDingbats = FALSE,
          plot = with_dark_static(
            scDRcoexLeg,
            input[[make_id("b2inp1")]],
            input[[make_id("b2inp2")]],
            input[[make_id("b2col1")]],
            input[[make_id("b2fsz")]]
          )
        )
      }
    )
    output[[make_id("b2oup2.png")]] <- downloadHandler(
      filename = function() paste0(prefix, "_coex_legend.png"),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = 3,
          width = 5,
          plot = with_dark_static(
            scDRcoexLeg,
            input[[make_id("b2inp1")]],
            input[[make_id("b2inp2")]],
            input[[make_id("b2col1")]],
            input[[make_id("b2fsz")]]
          )
        )
      }
    )

    output[[make_id("b2.dt")]] <- renderDataTable({
      ggData <- scDRcoexNum(
        get_conf(),
        get_meta(),
        input[[make_id("b2inp1")]],
        input[[make_id("b2inp2")]],
        input[[make_id("b2sub1")]],
        input[[make_id("b2sub2")]],
        gexpr_path,
        get_gene()
      )
      datatable(
        ggData,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 50,
          lengthMenu = c(25, 50, 100),
          deferRender = TRUE,
          dom = "tBfrtip",
          buttons = c("copy", "csv", "excel")
        )
      ) %>% formatRound(columns = c("percent"), digits = 2)
    }, server = TRUE)

    # ---- Tab c1: Violinplot / Boxplot ----
    output[[make_id("c1oup")]] <- renderPlot({
      sc_profile_eval(prefix, "violin_box", {
        y_value <- c1_y_value()
        with_dark(
          scVioBox,
          get_conf(),
          get_meta(),
          input[[make_id("c1inp1")]],
          y_value,
          input[[make_id("c1sub1")]],
          input[[make_id("c1sub2")]],
          gexpr_path,
          get_gene(),
          input[[make_id("c1typ")]],
          input[[make_id("c1pts")]],
          input[[make_id("c1siz")]],
          input[[make_id("c1fsz")]],
          show_legend = legend_enabled("c1")
        )
      })
    })
    output[[make_id("c1oup.ui")]] <- renderUI({
      height <- pList2[input[[make_id("c1psz")]]]
      sc_spinner_plot_output(make_id("c1oup"), height = height, proxy.height = height)
    })
    output[[make_id("c1oup.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "c1oup.name",
        paste0(prefix, input[[make_id("c1typ")]], "_", input[[make_id("c1inp1")]], "_", c1_y_value()),
        ".pdf"
      ),
      content = function(file) {
        y_value <- isolate(c1_y_value())
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("c1oup.h")]],
          width = input[[make_id("c1oup.w")]],
          useDingbats = FALSE,
          plot = with_dark_static(
            scVioBox,
            get_conf(),
            get_meta(),
            input[[make_id("c1inp1")]],
            y_value,
            input[[make_id("c1sub1")]],
            input[[make_id("c1sub2")]],
            gexpr_path,
            get_gene(),
            input[[make_id("c1typ")]],
            input[[make_id("c1pts")]],
            input[[make_id("c1siz")]],
            input[[make_id("c1fsz")]],
            show_legend = legend_enabled("c1")
          )
        )
      }
    )
    output[[make_id("c1oup.png")]] <- downloadHandler(
      filename = function() download_name(
        "c1oup.name",
        paste0(prefix, input[[make_id("c1typ")]], "_", input[[make_id("c1inp1")]], "_", c1_y_value()),
        ".png"
      ),
      content = function(file) {
        y_value <- isolate(c1_y_value())
        ggsave(
          file,
          device = "png",
          height = input[[make_id("c1oup.h")]],
          width = input[[make_id("c1oup.w")]],
          plot = with_dark_static(
            scVioBox,
            get_conf(),
            get_meta(),
            input[[make_id("c1inp1")]],
            y_value,
            input[[make_id("c1sub1")]],
            input[[make_id("c1sub2")]],
            gexpr_path,
            get_gene(),
            input[[make_id("c1typ")]],
            input[[make_id("c1pts")]],
            input[[make_id("c1siz")]],
            input[[make_id("c1fsz")]],
            show_legend = legend_enabled("c1")
          )
        )
      }
    )

    # ---- Tab c2: Proportion plot ----
    output[[make_id("c2oup")]] <- renderPlot({
      apply_legend_visibility(
        with_dark(
          scProp,
          get_conf(),
          get_meta(),
          input[[make_id("c2inp1")]],
          input[[make_id("c2inp2")]],
          input[[make_id("c2sub1")]],
          input[[make_id("c2sub2")]],
          input[[make_id("c2typ")]],
          input[[make_id("c2flp")]],
          input[[make_id("c2fsz")]]
        ),
        "c2"
      )
    })
    output[[make_id("c2oup.ui")]] <- renderUI({
      height <- pList2[input[[make_id("c2psz")]]]
      sc_spinner_plot_output(make_id("c2oup"), height = height, proxy.height = height)
    })
    output[[make_id("c2oup.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "c2oup.name",
        paste0(prefix, input[[make_id("c2typ")]], "_", input[[make_id("c2inp1")]], "_", input[[make_id("c2inp2")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("c2oup.h")]],
          width = input[[make_id("c2oup.w")]],
          useDingbats = FALSE,
          plot = apply_legend_visibility(
            with_dark_static(
              scProp,
              get_conf(),
              get_meta(),
              input[[make_id("c2inp1")]],
              input[[make_id("c2inp2")]],
              input[[make_id("c2sub1")]],
              input[[make_id("c2sub2")]],
              input[[make_id("c2typ")]],
              input[[make_id("c2flp")]],
              input[[make_id("c2fsz")]]
            ),
            "c2"
          )
        )
      }
    )
    output[[make_id("c2oup.png")]] <- downloadHandler(
      filename = function() download_name(
        "c2oup.name",
        paste0(prefix, input[[make_id("c2typ")]], "_", input[[make_id("c2inp1")]], "_", input[[make_id("c2inp2")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("c2oup.h")]],
          width = input[[make_id("c2oup.w")]],
          plot = apply_legend_visibility(
            with_dark_static(
              scProp,
              get_conf(),
              get_meta(),
              input[[make_id("c2inp1")]],
              input[[make_id("c2inp2")]],
              input[[make_id("c2sub1")]],
              input[[make_id("c2sub2")]],
              input[[make_id("c2typ")]],
              input[[make_id("c2flp")]],
              input[[make_id("c2fsz")]]
            ),
            "c2"
          )
        )
      }
    )

    # ---- Tab d1: Bubbleplot / Heatmap ----
    output[[make_id("d1oupTxt")]] <- renderUI({
      geneList <- scGeneList(input[[make_id("d1inp")]], get_gene())
      if (nrow(geneList) > 50) {
        HTML("More than 50 input genes! Please reduce the gene list!")
      } else {
        oup <- paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted")
        if (nrow(geneList[present == FALSE]) > 0) {
          oup <- paste0(
            oup,
            "<br/>",
            nrow(geneList[present == FALSE]),
            " genes not found (",
            paste0(geneList[present == FALSE]$gene, collapse = ", "),
            ")"
          )
        }
        HTML(oup)
      }
    })

    output[[make_id("d1oup")]] <- renderPlot({
      sc_profile_eval(prefix, "bubble_heatmap", {
        with_dark(
          scBubbHeat,
          get_conf(),
          get_meta(),
          input[[make_id("d1inp")]],
          input[[make_id("d1grp")]],
          input[[make_id("d1plt")]],
          input[[make_id("d1sub1")]],
          input[[make_id("d1sub2")]],
          gexpr_path,
          get_gene(),
          input[[make_id("d1scl")]],
          input[[make_id("d1row")]],
          input[[make_id("d1col")]],
          input[[make_id("d1cols")]],
          input[[make_id("d1fsz")]],
          show_legend = legend_enabled("d1")
        )
      })
    })
    output[[make_id("d1oup.ui")]] <- renderUI({
      height <- pList3[input[[make_id("d1psz")]]]
      sc_spinner_plot_output(make_id("d1oup"), height = height, proxy.height = height)
    })
    output[[make_id("d1oup.pdf")]] <- downloadHandler(
      filename = function() download_name(
        "d1oup.name",
        paste0(prefix, input[[make_id("d1plt")]], "_", input[[make_id("d1grp")]]),
        ".pdf"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "pdf",
          height = input[[make_id("d1oup.h")]],
          width = input[[make_id("d1oup.w")]],
          plot = with_dark_static(
            scBubbHeat,
            get_conf(),
            get_meta(),
            input[[make_id("d1inp")]],
            input[[make_id("d1grp")]],
            input[[make_id("d1plt")]],
            input[[make_id("d1sub1")]],
            input[[make_id("d1sub2")]],
            gexpr_path,
            get_gene(),
            input[[make_id("d1scl")]],
            input[[make_id("d1row")]],
            input[[make_id("d1col")]],
            input[[make_id("d1cols")]],
            input[[make_id("d1fsz")]],
            save = TRUE,
            show_legend = legend_enabled("d1")
          )
        )
      }
    )
    output[[make_id("d1oup.png")]] <- downloadHandler(
      filename = function() download_name(
        "d1oup.name",
        paste0(prefix, input[[make_id("d1plt")]], "_", input[[make_id("d1grp")]]),
        ".png"
      ),
      content = function(file) {
        ggsave(
          file,
          device = "png",
          height = input[[make_id("d1oup.h")]],
          width = input[[make_id("d1oup.w")]],
          plot = with_dark_static(
            scBubbHeat,
            get_conf(),
            get_meta(),
            input[[make_id("d1inp")]],
            input[[make_id("d1grp")]],
            input[[make_id("d1plt")]],
            input[[make_id("d1sub1")]],
            input[[make_id("d1sub2")]],
            gexpr_path,
            get_gene(),
            input[[make_id("d1scl")]],
            input[[make_id("d1row")]],
            input[[make_id("d1col")]],
            input[[make_id("d1cols")]],
            input[[make_id("d1fsz")]],
            save = TRUE,
            show_legend = legend_enabled("d1")
          )
        )
      }
    )

    suspend_ids <- c(
      "a1sub1.ui", "a1oup1", "a1oup1.ui", "a1.dt", "a1oup2", "a1oup2.ui",
      "m1sub1.ui", "m1oupTxt", "m1oup", "m1oup.ui", "m1.dt",
      "a2sub1.ui", "a2oup1", "a2oup1.ui", "a2oup2", "a2oup2.ui",
      "a3sub1.ui", "a3oup1", "a3oup1.ui", "a3oup2", "a3oup2.ui",
      "b2sub1.ui", "b2oup1", "b2oup1.ui", "b2oup2", "b2.dt",
      "c1sub1.ui", "c1oup", "c1oup.ui",
      "c2sub1.ui", "c2oup", "c2oup.ui",
      "d1sub1.ui", "d1oupTxt", "d1oup", "d1oup.ui"
    )
    for (output_id in suspend_ids) {
      outputOptions(output, make_id(output_id), suspendWhenHidden = TRUE)
    }
  }

  dataset_tab_definitions <- list(
    list(title = "Main Figures", suffix = "main_figures", builder = build_main_figures_tab),
    list(title = "CellInfo vs GeneExpr", suffix = "cellinfo_gene", builder = build_cellinfo_gene_tab),
    list(title = "Multiple GeneExpr", suffix = "multiple_geneexpr", builder = build_multiple_geneexpr_tab),
    list(title = "Gene coexpression", suffix = "gene_coexpression", builder = build_gene_coexpression_tab),
    list(title = "Violinplot / Boxplot", suffix = "violin_boxplot", builder = build_violin_boxplot_tab),
    list(title = "Proportion plot", suffix = "proportion_plot", builder = build_proportion_plot_tab),
    list(title = "Bubbleplot / Heatmap", suffix = "bubble_heatmap", builder = build_bubble_heatmap_tab)
  )

  dataset_specs <- list(
    sc3 = list(
      prefix = "sc3",
      dataset_name = "Full Atlas",
      get_conf = function() sc3conf,
      get_meta = function() sc3meta,
      get_def = function() sc3def,
      get_gene = function() sc3gene,
      gexpr_path = "sc3gexpr.h5"
    ),
    sc4 = list(
      prefix = "sc4",
      dataset_name = "Sertoli Subset",
      get_conf = function() sc4conf,
      get_meta = function() sc4meta,
      get_def = function() sc4def,
      get_gene = function() sc4gene,
      gexpr_path = "sc4gexpr.h5"
    ),
    sc5 = list(
      prefix = "sc5",
      dataset_name = "Spermatogonia Subset",
      get_conf = function() sc5conf,
      get_meta = function() sc5meta,
      get_def = function() sc5def,
      get_gene = function() sc5gene,
      gexpr_path = "sc5gexpr.h5"
    ),
    sc6 = list(
      prefix = "sc6",
      dataset_name = "Spermatocyte Subset",
      get_conf = function() sc6conf,
      get_meta = function() sc6meta,
      get_def = function() sc6def,
      get_gene = function() sc6gene,
      gexpr_path = "sc6gexpr.h5"
    ),
    sc7 = list(
      prefix = "sc7",
      dataset_name = "Spermatid Subset",
      get_conf = function() sc7conf,
      get_meta = function() sc7meta,
      get_def = function() sc7def,
      get_gene = function() sc7gene,
      gexpr_path = "sc7gexpr.h5"
    )
  )

  dataset_status <- lapply(names(dataset_specs), function(prefix) {
    list(
      initialized = reactiveVal(FALSE),
      ui_ready = reactiveVal(FALSE)
    )
  })
  names(dataset_status) <- names(dataset_specs)

  dataset_tab_output_id <- function(tab_value) {
    paste0("lazy_", tab_value, "_body")
  }

  dataset_prefix_from_tab <- function(tab_value) {
    tab_value <- as.character(tab_value)[1]
    if (is.na(tab_value) || !nzchar(tab_value)) {
      return(NULL)
    }
    match <- regexec("^(sc[3-7])_", tab_value)
    parts <- regmatches(tab_value, match)[[1]]
    if (length(parts) < 2) {
      return(NULL)
    }
    parts[[2]]
  }

  make_lazy_dataset_loading_ui <- function(spec, tab_title) {
    tags$div(
      class = "ra-pane",
      tags$div(
        class = "ra-card glass-card",
        style = "margin:16px 0;",
        tags$div(
          class = "ra-card-head",
          tags$h3(class = "ra-title", spec$dataset_name),
          tags$p(class = "ra-sub", sprintf("Loading %s ...", tab_title))
        )
      )
    )
  }

  for (prefix in names(dataset_specs)) {
    spec <- dataset_specs[[prefix]]
    status <- dataset_status[[prefix]]

    for (tab_def in dataset_tab_definitions) {
      local({
        spec_local <- spec
        status_local <- status
        tab_def_local <- tab_def
        tab_value_local <- paste0(spec_local$prefix, "_", tab_def_local$suffix)
        output_id_local <- dataset_tab_output_id(tab_value_local)

        output[[output_id_local]] <- renderUI({
          if (!isTRUE(status_local$ui_ready())) {
            return(make_lazy_dataset_loading_ui(spec_local, tab_def_local$title))
          }

          tab_def_local$builder(
            spec_local$prefix,
            spec_local$get_conf(),
            spec_local$get_def(),
            spec_local$dataset_name,
            as_tab = FALSE
          )
        })
        outputOptions(output, output_id_local, suspendWhenHidden = TRUE)
      })
    }
  }

  ensure_dataset_initialized <- function(prefix) {
    spec <- dataset_specs[[prefix]]
    status <- dataset_status[[prefix]]
    if (is.null(spec) || is.null(status)) {
      return(invisible(FALSE))
    }

    if (!isTRUE(status$initialized())) {
      bind_main_figures(
        spec$prefix,
        get_conf = spec$get_conf,
        get_meta = spec$get_meta
      )
      bind_shinycell_dataset(
        spec$prefix,
        get_conf = spec$get_conf,
        get_meta = spec$get_meta,
        get_def = spec$get_def,
        get_gene = spec$get_gene,
        gexpr_path = spec$gexpr_path
      )
      status$initialized(TRUE)
    }

    if (!isTRUE(status$ui_ready())) {
      status$ui_ready(TRUE)
    }

    invisible(TRUE)
  }

  observeEvent(input$mainTabs, {
    prefix <- dataset_prefix_from_tab(input$mainTabs)
    if (is.null(prefix)) {
      return()
    }
    ensure_dataset_initialized(prefix)
  }, ignoreInit = FALSE)
  
  
  
  #------------------------------------------------->
  # Monitor for modal closure and remove highlight
  # ==== Spermatogenesis SVG layout (uses your old UI constants) ====
  make_layout <- function() {
    # Constants that matched your old absolute buttons (900px-wide rendering)
    origin_x <- 37
    origin_y <- 29
    cell_w   <- 62
    cell_h   <- 61
    col_gap  <- 5
    row_gap  <- 7
    
    n_rows <- 6
    n_cols <- 11
    
    df <- expand.grid(row = 1:n_rows, col = 1:n_cols, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    df <- subset(df, !(row == 1 & col > 7))  # top row only 7 cells
    df$btn_id <- sprintf("btn_%d_%d", df$row, df$col)
    
    df$x <- origin_x + (df$col - 1) * (cell_w + col_gap)
    df$y <- origin_y + (df$row - 1) * (cell_h + row_gap)
    df$w <- cell_w
    df$h <- cell_h
    df
  }
  
  buttons_layout <- make_layout()
  
  # ==== SVG renderer for the table ====
  output$spermatogonia_svg <- renderUI({
    req(ra_plot_gate$spermatogonia)
    # Original art sized for ~900px width; height derived from 2000x1081 -> 900x486
    view_w <- 900
    view_h <- 486
    
    bg_img_light <- tags$image(
      href = "interactiveTable.png",  # file must be in www/
      class = "spg-table-bg spg-table-bg-light",
      x = 0, y = 0, width = view_w, height = view_h,
      preserveAspectRatio = "none"
    )

    bg_img_dark <- tags$image(
      href = "interactiveTable_dark.png",
      class = "spg-table-bg spg-table-bg-dark",
      x = 0, y = 0, width = view_w, height = view_h,
      preserveAspectRatio = "none"
    )
    
    rects <- lapply(seq_len(nrow(buttons_layout)), function(i) {
      r <- buttons_layout[i, ]
      tags$rect(
        id = r$btn_id,
        class = "cell-btn",
        x = r$x, y = r$y,
        width = r$w, height = r$h
      )
    })
    
    tags$svg(
      id = "sperma-svg",
      viewBox = sprintf("0 0 %d %d", view_w, view_h),
      preserveAspectRatio = "xMidYMid meet",
      bg_img_light,
      bg_img_dark,
      rects
    )
  })
  outputOptions(output, "spermatogonia_svg", suspendWhenHidden = TRUE)
  
  
  # When ANY modal closes, remove highlight from the active button
  observeEvent(input$`__modal__closed__`, {
    last_btn <- active_button()
    if (!is.null(last_btn)) {
      session$sendCustomMessage("unhighlightButton", last_btn)
      active_button(NULL)   # clear stored id so nothing stays highlighted
    }
    spg_modal_open(FALSE)
    session$sendCustomMessage("spgModalLock", FALSE)
  }, ignoreInit = TRUE)
  

  # ====== GLOBAL SETTINGS / HELPERS ======
  DEFAULT_EXPR_THRESHOLD <- 0.25
  
  `%||%` <- function(x, y) if (is.null(x)) y else x

  format_spg_expr <- function(x) {
    formatC(x, format = "f", digits = 3)
  }

  sanitize_spg_expr_threshold <- function(value) {
    threshold <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(threshold)) {
      return(DEFAULT_EXPR_THRESHOLD)
    }
    max(threshold, 0)
  }
  
  minmax01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
      return(rep(1, length(x)))  # flat case → treat as max shade
    }
    (x - rng[1]) / (rng[2] - rng[1])
  }

  empty_spg_gene_table <- function() {
    data.frame(
      `Gene Name` = character(0),
      `Gene Expression` = numeric(0),
      `Gene Ensembl ID` = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  get_mouse_gene_mapping <- local({
    cache <- NULL
    function() {
      if (!is.null(cache)) {
        return(cache)
      }

      candidate_paths <- c(
        "www/mouseGeneMapping.tsv",
        "www/mouseGeneMapping.txt",
        "www/mouseGeneMapping"
      )
      resolved_paths <- sc_resolve_paths(candidate_paths, app_dir = app_dir)
      mapping_path <- resolved_paths[file.exists(resolved_paths)][1]
      if (is.na(mapping_path) || !nzchar(mapping_path)) {
        cache <<- data.frame(
          gene_name = character(0),
          gene_ensembl_id = character(0),
          stringsAsFactors = FALSE
        )
        return(cache)
      }

      mapping_raw <- tryCatch(
        data.table::fread(
          mapping_path,
          sep = "\t",
          header = TRUE,
          data.table = FALSE,
          showProgress = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(mapping_raw) || !nrow(mapping_raw)) {
        cache <<- data.frame(
          gene_name = character(0),
          gene_ensembl_id = character(0),
          stringsAsFactors = FALSE
        )
        return(cache)
      }

      name_cols <- c("Gene name", "Gene Name", "gene_name")
      id_cols <- c("Gene stable ID", "Gene stable id", "gene_id")
      name_col <- name_cols[name_cols %in% colnames(mapping_raw)][1]
      id_col <- id_cols[id_cols %in% colnames(mapping_raw)][1]
      if (is.na(name_col) || is.na(id_col)) {
        cache <<- data.frame(
          gene_name = character(0),
          gene_ensembl_id = character(0),
          stringsAsFactors = FALSE
        )
        return(cache)
      }

      mapping_df <- data.frame(
        gene_name = trimws(as.character(mapping_raw[[name_col]])),
        gene_ensembl_id = trimws(as.character(mapping_raw[[id_col]])),
        stringsAsFactors = FALSE
      )
      mapping_df <- mapping_df[nzchar(mapping_df$gene_name), , drop = FALSE]
      mapping_df <- mapping_df[!duplicated(mapping_df$gene_name), , drop = FALSE]

      cache <<- mapping_df
      cache
    }
  })

  add_ensembl_column <- function(df) {
    if (!nrow(df)) {
      df$`Gene Ensembl ID` <- character(0)
      return(df)
    }
    mapping_df <- get_mouse_gene_mapping()
    if (!nrow(mapping_df)) {
      df$`Gene Ensembl ID` <- "Not found"
      return(df)
    }
    idx <- match(df$`Gene Name`, mapping_df$gene_name)
    ensembl_ids <- mapping_df$gene_ensembl_id[idx]
    ensembl_ids[is.na(ensembl_ids) | !nzchar(ensembl_ids)] <- "Not found"
    df$`Gene Ensembl ID` <- ensembl_ids
    df
  }

  spg_modal_table_full <- reactiveVal(empty_spg_gene_table())
  spg_modal_state <- reactiveVal(list(btn_id = NULL, sample = NULL, cell_type = NULL))
  spg_modal_open <- reactiveVal(FALSE)
  spg_search_state <- reactiveVal(list(
    gene_name = NULL,
    min_expr = NA_real_,
    max_expr = NA_real_
  ))
  spg_last_searched_gene <- reactiveVal(NULL)

  spg_expr_threshold <- reactive({
    sanitize_spg_expr_threshold(input$spg_expr_threshold)
  })

  output$spg_expr_threshold_help <- renderUI({
    state <- spg_search_state()
    threshold_label <- format_spg_expr(spg_expr_threshold())

    if (!nzchar(state$gene_name %||% "")) {
      return(tags$p(
        class = "heat-legend-note",
        paste0(
          "Default threshold: ",
          threshold_label,
          ". Search a gene to view its minimum and maximum average expression across cells."
        )
      ))
    }

    if (!is.finite(state$min_expr) || !is.finite(state$max_expr)) {
      return(tags$p(
        class = "heat-legend-note",
        tags$span("Range unavailable for "),
        tags$strong(state$gene_name),
        tags$span(". Cells are shown when expression is > "),
        tags$span(threshold_label),
        tags$span(".")
      ))
    }

    tags$p(
      class = "heat-legend-note",
      tags$span("Current gene range for "),
      tags$strong(state$gene_name),
      tags$span(": "),
      tags$span(format_spg_expr(state$min_expr)),
      tags$span(" to "),
      tags$span(format_spg_expr(state$max_expr)),
      tags$span(". Cells are shown when expression is > "),
      tags$span(threshold_label),
      tags$span(".")
    )
  })

  output$spg_modal_genes_csv <- downloadHandler(
    filename = function() {
      state <- spg_modal_state()
      suffix <- state$btn_id %||% "selected_cell"
      suffix <- gsub("[^A-Za-z0-9_-]+", "_", suffix)
      paste0("genes_above_threshold_", suffix, ".csv")
    },
    content = function(file) {
      full_df <- spg_modal_table_full()
      utils::write.csv(full_df, file, row.names = FALSE)
    }
  )

  clear_spg_gene_search <- function(reset_state = TRUE) {
    session$sendCustomMessage("bulkHighlight", list(match_ids = character()))
    session$sendCustomMessage("heatmap-colorize", list(colors = list(), clear = TRUE))
    if (isTRUE(reset_state)) {
      spg_search_state(list(gene_name = NULL, min_expr = NA_real_, max_expr = NA_real_))
      spg_last_searched_gene(NULL)
    }
    invisible(NULL)
  }

  run_spg_gene_search <- function(query = NULL, show_notifications = TRUE) {
    ensure_spg_assets(show_progress = FALSE)
    query <- trimws((query %||% input$gene_search %||% ""))

    if (!nzchar(query)) {
      clear_spg_gene_search(reset_state = TRUE)
      return(invisible(FALSE))
    }

    spg_mat <- get_spg_avg_expr()
    gene_names <- rownames(spg_mat)
    gene_idx <- match(toupper(query), toupper(gene_names))
    if (is.na(gene_idx[1])) {
      clear_spg_gene_search(reset_state = TRUE)
      if (isTRUE(show_notifications)) {
        showNotification(
          sprintf("“%s” isn’t in the gene list.", query),
          type = "warning",
          duration = 4
        )
      }
      return(invisible(FALSE))
    }
    gene_name <- gene_names[gene_idx[1]]
    spg_last_searched_gene(gene_name)

    if (!identical(query, gene_name)) {
      updateSelectizeInput(session, "gene_search", selected = gene_name, server = TRUE)
    }

    session$sendCustomMessage("spgBusy", TRUE)
    on.exit(session$sendCustomMessage("spgBusy", FALSE), add = TRUE)

    expr_values <- spg_mat[gene_idx[1], ]
    finite_expr <- expr_values[is.finite(expr_values)]
    if (!length(finite_expr)) {
      spg_search_state(list(gene_name = gene_name, min_expr = NA_real_, max_expr = NA_real_))
      clear_spg_gene_search(reset_state = FALSE)
      if (isTRUE(show_notifications)) {
        showNotification(
          sprintf("No expression values are available for %s.", gene_name),
          type = "warning",
          duration = 3
        )
      }
      return(invisible(FALSE))
    }

    spg_search_state(list(
      gene_name = gene_name,
      min_expr = min(finite_expr),
      max_expr = max(finite_expr)
    ))

    thr <- spg_expr_threshold()
    keep_mask <- is.finite(expr_values) & (expr_values > thr)
    match_ids <- names(expr_values)[keep_mask]

    if (!length(match_ids)) {
      clear_spg_gene_search(reset_state = FALSE)
      if (isTRUE(show_notifications)) {
        showNotification(
          sprintf("No cells express %s > %s.", gene_name, format_spg_expr(thr)),
          type = "warning",
          duration = 3
        )
      }
      return(invisible(TRUE))
    }

    expr_vals <- expr_values[match_ids]
    ord <- order(expr_vals, decreasing = TRUE)
    match_ids <- match_ids[ord]
    expr_vals <- expr_vals[ord]

    scaled <- as.numeric(minmax01(expr_vals))

    pal <- colorRampPalette(c("#e0f2fe", "#3b82f6", "#1e3a8a"))
    cols <- pal(101)[pmax(1, pmin(101, floor(scaled * 100) + 1))]

    payload <- Map(
      function(id, col, val, rank_idx, sc) {
        list(
          id = id,
          color = col,
          value = round(val, 3),
          rank = rank_idx,
          scaled = round(sc, 3)
        )
      },
      match_ids,
      cols,
      as.numeric(expr_vals),
      seq_along(match_ids),
      scaled
    )

    session$sendCustomMessage("bulkHighlight", list(match_ids = match_ids))
    session$sendCustomMessage("heatmap-colorize", list(colors = unname(payload), clear = FALSE))
    if (isTRUE(show_notifications)) {
      showNotification(
        sprintf("Found %d cell(s) with %s > %s", length(match_ids), gene_name, format_spg_expr(thr)),
        type = "message",
        duration = 3
      )
    }

    invisible(TRUE)
  }
  
  
  show_button_modal <- function(row, col) {
    ensure_spg_assets(show_progress = FALSE)
    # Unhighlight last active button
    {
      last_btn <- active_button()
      if (!is.null(last_btn)) session$sendCustomMessage("unhighlightButton", last_btn)
    }
    
    btn_id <- paste0("btn_", row, "_", col)
    mapping <- general_button_mapping[[btn_id]]
    
    if (is.null(mapping)) {
      showModal(modalDialog("No mapping found for this button.", easyClose = TRUE))
      return(invisible(NULL))
    }
    
    sample    <- mapping$sample
    cell_type <- mapping$specificCellID

    spg_mat <- get_spg_avg_expr()
    if (!btn_id %in% colnames(spg_mat)) {
      showModal(modalDialog(
        title = "No mapping data available",
        HTML(sprintf(
          "<b>Button:</b> %s<br><br>
         The precomputed table does not include this button. Check <code>button_mapping_general.R</code> and rebuild interactive assets.",
          btn_id
        )),
        easyClose = TRUE
      ))
      return(invisible(NULL))
    }

    # Proceed normally
    session$sendCustomMessage("highlightButton", btn_id)
    active_button(btn_id)
    avg_expr <- spg_mat[, btn_id]
    
    expr_threshold <- spg_expr_threshold()
    filtered_expr <- avg_expr[is.finite(avg_expr) & avg_expr > expr_threshold]
    sorted_genes <- sort(filtered_expr, decreasing = TRUE)
    full_df <- data.frame(
      `Gene Name` = names(sorted_genes),
      `Gene Expression` = as.numeric(sorted_genes),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    full_df <- add_ensembl_column(full_df)

    preview_limit <- 50L
    top_gene <- if (nrow(full_df)) full_df$`Gene Name`[1] else "(none)"

    spg_modal_table_full(full_df)
    spg_modal_state(list(btn_id = btn_id, sample = sample, cell_type = cell_type))
    
    cell_label <- mapping$specificCellID
    search_id <- paste0("spg_modal_gene_search_", btn_id)
    search_result_id <- paste0("spg_modal_search_result_", btn_id)
    table_body_id <- paste0("spg_modal_table_body_", btn_id)

    full_records_json <- jsonlite::toJSON(full_df, dataframe = "rows", auto_unbox = TRUE)
    js_code <- paste0(
      "(function(){",
      "  var records = ", full_records_json, ";",
      "  var searchEl = document.getElementById(", shQuote(search_id), ");",
      "  var resultEl = document.getElementById(", shQuote(search_result_id), ");",
      "  var tbodyEl  = document.getElementById(", shQuote(table_body_id), ");",
      "  var PREVIEW_LIMIT = ", preview_limit, ";",
      "  function esc(v){",
      "    return String(v == null ? '' : v).replace(/[&<>\\\"']/g, function(c){",
      "      return {'&':'&amp;','<':'&lt;','>':'&gt;','\\\"':'&quot;',\"'\":'&#39;'}[c];",
      "    });",
      "  }",
      "  function fmtExpr(v){",
      "    var n = Number(v);",
      "    if (!isFinite(n)) return 'NA';",
      "    return n.toFixed(3);",
      "  }",
      "  function renderRows(rows){",
      "    if (!tbodyEl) return;",
      "    if (!rows || rows.length === 0){",
      "      tbodyEl.innerHTML = '<tr><td colspan=\"3\"><em>No genes found above threshold.</em></td></tr>';",
      "      return;",
      "    }",
      "    var html = rows.map(function(r){",
      "      return '<tr>' +",
      "        '<td>' + esc(r['Gene Name']) + '</td>' +",
      "        '<td>' + esc(fmtExpr(r['Gene Expression'])) + '</td>' +",
      "        '<td>' + esc(r['Gene Ensembl ID']) + '</td>' +",
      "      '</tr>';",
      "    }).join('');",
      "    tbodyEl.innerHTML = html;",
      "  }",
      "  function onSearch(){",
      "    if (!searchEl || !resultEl) return;",
      "    var q = (searchEl.value || '').trim().toLowerCase();",
      "    if (!q){",
      "      var topRows = records.slice(0, PREVIEW_LIMIT);",
      "      renderRows(topRows);",
      "      resultEl.textContent = 'Showing top ' + Math.min(PREVIEW_LIMIT, records.length) + ' of ' + records.length + ' genes above threshold.';",
      "      return;",
      "    }",
      "    var startsWithMatches = records.filter(function(r){",
      "      var name = String(r['Gene Name'] || '').toLowerCase();",
      "      return name.indexOf(q) === 0;",
      "    });",
      "    var containsMatches = startsWithMatches;",
      "    if (!containsMatches.length){",
      "      containsMatches = records.filter(function(r){",
      "        return String(r['Gene Name'] || '').toLowerCase().indexOf(q) !== -1;",
      "      });",
      "    }",
      "    if (containsMatches.length){",
      "      renderRows(containsMatches);",
      "      if (startsWithMatches.length){",
      "        resultEl.textContent = 'Found ' + containsMatches.length + ' gene(s) starting with \"' + searchEl.value + '\" above threshold.';",
      "      } else {",
      "        resultEl.textContent = 'Found ' + containsMatches.length + ' gene(s) containing \"' + searchEl.value + '\" above threshold.';",
      "      }",
      "    } else {",
      "      renderRows([]);",
      "      resultEl.textContent = 'No genes matching \"' + searchEl.value + '\" are above threshold for this cell.';",
      "    }",
      "  }",
      "  if (searchEl){ searchEl.addEventListener('input', onSearch); }",
      "  onSearch();",
      "})();"
    )

    modal_body <- tagList(
      tags$div(
        class = "spg-modal-summary",
        tags$b("Top Gene:"), " ", top_gene, tags$br(),
        tags$b("Total Genes Above Threshold:"), " ", nrow(full_df), tags$br(),
        tags$b("Showing On Screen:"), " ", min(preview_limit, nrow(full_df)), " (top genes by expression)", tags$br(),
        tags$b("Expression Threshold:"), " > ", format_spg_expr(expr_threshold), " (log-normalized RNA)"
      ),
      tags$div(
        class = "spg-modal-search-block",
        tags$input(
          id = search_id,
          type = "text",
          placeholder = "Search genes by name...",
          class = "spg-modal-search form-control"
        ),
        tags$div(
          id = search_result_id,
          class = "spg-modal-search-result"
        )
      ),
      tags$div(
        class = "spg-modal-gene-table",
        tags$table(
          class = "table table-striped table-hover",
          tags$colgroup(
            tags$col(style = "width: 30%;"),
            tags$col(style = "width: 24%;"),
            tags$col(style = "width: 46%;")
          ),
          tags$thead(
            tags$tr(
              tags$th("Gene Name"),
              tags$th("Gene Expression"),
              tags$th("Gene Ensembl ID")
            )
          ),
          tags$tbody(id = table_body_id)
        )
      ),
      tags$script(HTML(js_code))
    )
    
    showModal(modalDialog(
      title     = paste("Genes for", cell_label, "in", mapping$sample),
      modal_body,
      easyClose = FALSE,
      size = "l",
      class = "spg-modal",
      footer = tagList(
        downloadButton(
          outputId = "spg_modal_genes_csv",
          label = "Download Full List as CSV",
          class = "btn btn-primary spg-modal-download"
        ),
        tags$button(
          type = "button", class = "btn btn-primary spg-modal-close",
          onclick = sprintf("Shiny.setInputValue('modalClosedBtn','%s',{priority:'event'});", btn_id),
          "Close"
        )
      )
    ))
  }
  
  
  
  
  observeEvent(input$modalClosedBtn, {
    btn_id <- input$modalClosedBtn
    session$sendCustomMessage("unhighlightButton", btn_id)  # remove blue ring
    active_button(NULL)                                     # reset reactiveVal
    spg_modal_open(FALSE)
    session$sendCustomMessage("spgModalLock", FALSE)
    removeModal()
  }, ignoreInit = TRUE)
  
  
  # ==== Single handler for any cell click ====
  observeEvent(input$btn_click, {
    if (isTRUE(spg_modal_open())) {
      session$sendCustomMessage("spgModalLock", TRUE)
      return(invisible(NULL))
    }

    btn_id <- input$btn_click
    parts <- strsplit(btn_id, "_", fixed = TRUE)[[1]]  # "btn", row, col
    if (length(parts) == 3) {
      row <- suppressWarnings(as.integer(parts[2]))
      col <- suppressWarnings(as.integer(parts[3]))
      if (!is.na(row) && !is.na(col)) {
        spg_modal_open(TRUE)
        session$sendCustomMessage("spgModalLock", TRUE)
        tryCatch(
          show_button_modal(row, col),
          error = function(e) {
            spg_modal_open(FALSE)
            session$sendCustomMessage("spgModalLock", FALSE)
            stop(e)
          }
        )
        return(invisible(NULL))
      }
    }
    session$sendCustomMessage("spgModalLock", FALSE)
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(input$gene_search_btn, {
    run_spg_gene_search(show_notifications = TRUE)
  })

  spg_threshold_recalc <- debounce(reactive(spg_expr_threshold()), 250)

  observeEvent(spg_threshold_recalc(), {
    last_gene <- spg_last_searched_gene()
    current_query <- trimws(input$gene_search %||% "")
    if (nzchar(last_gene %||% "") &&
        nzchar(current_query) &&
        identical(toupper(current_query), toupper(last_gene))) {
      run_spg_gene_search(query = last_gene, show_notifications = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$theme_mode, {
    last_gene <- spg_last_searched_gene()
    if (identical(input$mainTabs, "spermatogonia_table") &&
        nzchar(last_gene %||% "")) {
      run_spg_gene_search(query = last_gene, show_notifications = FALSE)
    }
  }, ignoreInit = TRUE)
  
  # (Optional) Clear when the search box is emptied
  observeEvent(input$gene_search, {
    if (!nzchar(trimws(input$gene_search))) {
      clear_spg_gene_search(reset_state = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # NOTE: Interactive inputs are now updated from precomputed assets inside
  # ensure_dot_assets() / ensure_line_assets() when tabs are opened.
  
  
  
    # ============================
  # PUBLISH/REUSEABLE BUILDERS
  # ============================
  
  # --- Figure 5A: RA DotPlot (returns ggplot) ---
  make_fig5A <- function(pub_theme = FALSE, dark_theme = FALSE){
    avg <- get_ra_dot_avg()
    pct <- get_ra_dot_pct()

    desired_genes_order <- rev(c(
      "Stra8", "Stra6",
      "Aldh1a1", "Aldh1a2", "Aldh1a3",
      "Cyp26a1", "Cyp26b1", "Cyp26c1",
      "Rara", "Rarb", "Rarg",
      "Rxra", "Rxrb", "Rxrg",
      "Dmrt1", "Rdh10", "Rbp4", "Rbp1"
    ))
    default_genes <- rev(c(
      "Stra8", "Stra6",
      "Aldh1a1", "Aldh1a2", "Aldh1a3",
      "Cyp26a1", "Cyp26b1", "Cyp26c1",
      "Rdh10", "Rbp4", "Rbp1"
    ))
    default_genes <- default_genes[default_genes %in% rownames(avg)]
    selected_genes <- input$ra_genes
    if (is.null(selected_genes) || !length(selected_genes)) {
      selected_genes <- default_genes
    }
    selected_genes <- selected_genes[selected_genes %in% rownames(avg)]
    if (!length(selected_genes)) {
      selected_genes <- rownames(avg)
    }
    extras <- setdiff(selected_genes, desired_genes_order)
    genes <- c(desired_genes_order[desired_genes_order %in% selected_genes], extras)
    desired_cell_types <- c(
      "Aund", "A1-2", "A3-4", "Ain", "Type B",
      "ePL", "lPL", "L", "L/Z", "Z",
      "PaI-VI", "PaVII-VIII", "PaIX-X", "D/MI",
      "Rd1", "Rd2-3", "Rd4-5", "Rd6", "Rd7", "Rd8",
      "El9", "El10", "El11", "El12-13", "El14-15", "El16",
      "SC_I-VIII", "SC_VII-VIII", "SC_IX-XII", "SC_XI-VI", "SC_All_Stages",
      "PTM", "Leydig", "Macrophage"
    )
    selected_cell_types <- input$ra_cell_types
    if (is.null(selected_cell_types) || !length(selected_cell_types)) {
      selected_cell_types <- colnames(avg)
    }
    # Accept the figure shorthand MO as an alias for Macrophage.
    selected_cell_types[selected_cell_types == "M\330"] <- "Macrophage"
    selected_cell_types <- unique(selected_cell_types)
    selected_cell_types <- selected_cell_types[selected_cell_types %in% colnames(avg)]
    cell_types <- c(
      desired_cell_types[desired_cell_types %in% selected_cell_types],
      setdiff(selected_cell_types, desired_cell_types)
    )
    if (!length(cell_types)) {
      cell_types <- colnames(avg)
    }
    if (!length(genes) || !length(cell_types)) {
      return(
        ggplot() +
          theme_void() +
          annotate("text", x = 0, y = 0, label = "Select genes and cell types")
      )
    }

    avg_sub <- avg[genes, cell_types, drop = FALSE]
    pct_sub <- pct[genes, cell_types, drop = FALSE]

    scale_rows <- function(mat) {
      t(apply(mat, 1, function(x) {
        if (length(unique(x)) <= 1) {
          return(rep(0, length(x)))
        }
        as.numeric(scale(x))
      }))
    }

    scaled <- scale_rows(avg_sub)
    scaled[is.na(scaled)] <- 0
    scaled <- pmax(pmin(scaled, 2.5), -2.5)

    df <- expand.grid(features = genes, id = cell_types, stringsAsFactors = FALSE)
    df$features <- factor(df$features, levels = genes)
    df$id <- factor(df$id, levels = cell_types)
    df$avg.exp.scaled <- as.vector(scaled)
    df$pct.exp <- as.vector(pct_sub)

    # Match publication Figure 5A palette (RA_Signaling_Figure5)
    publication_rd_bu <- rev(
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(100)
    )

    axis_col <- if (dark_theme) "#e2e8f0" else "#1e293b"
    grid_col <- if (dark_theme) "#1f2937" else "#e5e7eb"
    bg_col   <- if (dark_theme) "#050815" else "#ffffff"
    border_col <- if (dark_theme) "#475569" else "#000000"
    point_stroke <- if (dark_theme) "#f8fafc" else "#000000"

    p <- ggplot(df, aes(x = features, y = id)) +
      geom_point(
        aes(size = pct.exp, fill = avg.exp.scaled),
        shape = 21, stroke = 0.3, color = point_stroke
      ) +
      scale_fill_gradientn(
        colors = publication_rd_bu,
        values = scales::rescale(c(-2.5, 0, 2.5)),
        limits = c(-2.5, 2.5),
        oob    = scales::squish
      ) +
      scale_size(range = c(0, 8.25), limits = c(0, 100)) +
      theme_minimal(base_size = if (pub_theme) 11 else 14) +
      theme(
        panel.background = element_rect(fill = bg_col, colour = NA),
        plot.background = element_rect(fill = bg_col, colour = NA),
        panel.grid.major = element_line(color = grid_col),
        panel.grid.minor = element_line(color = grid_col),
        axis.text.x = element_text(color = axis_col, size = 10, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(color = axis_col, size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = border_col, fill = NA, size = if (pub_theme) 0.6 else 1),
        axis.ticks = element_line(color = border_col, size = 0.5),
        axis.ticks.length = unit(0.25, "cm"),
        legend.background = element_rect(fill = bg_col, colour = border_col),
        legend.text = element_text(color = axis_col),
        legend.title = element_text(color = axis_col)
      ) +
      labs(title = NULL) +
      guides(
        fill = guide_colorbar(title = "Average Expression"),
        size  = guide_legend(title = "Percent Expressed")
      ) +
      coord_flip()

    p
  }
  
  # --- Figure 5C: RA LinePlot (returns ggplot/patchwork) ---
  make_fig5C <- function(pub_theme = TRUE, dark_theme = FALSE, pub_layout = TRUE){
    line_mean <- get_ra_line_mean()
    row1_genes <- input$ra_line_genes_row1
    row2_genes <- input$ra_line_genes_row2
    row3_genes <- input$ra_line_genes_row3
    if (is.null(row1_genes) || !length(row1_genes)) row1_genes <- c("Stra8", "Stra6", "Rbp1")
    if (is.null(row2_genes) || !length(row2_genes)) row2_genes <- c("Aldh1a1", "Aldh1a2", "Aldh1a3", "Rdh10")
    if (is.null(row3_genes) || !length(row3_genes)) row3_genes <- c("Cyp26a1", "Cyp26b1", "Cyp26c1", "Rarg")
    all_genes <- unique(c(row1_genes, row2_genes, row3_genes))
    genes <- intersect(all_genes, dimnames(line_mean)[[1]])
    stage_levels <- c(
      "I-VI (Weak to Strong)",
      "VII-VIII (Dark)",
      "IX-X (Pale)",
      "XI-XII (Pale to Weak)"
    )
    stage_levels_with_loop <- c(stage_levels, "I-VI (looped)")
    stage_label_short <- function(values) {
      sub("\\s*\\(.*\\)", "", as.character(values))
    }

    if (!length(genes)) {
      return(
        ggplot() +
          theme_void() +
          annotate("text", x = 0, y = 0, label = "Select genes to plot")
      )
    }

    df_summary <- as.data.frame(as.table(line_mean[genes, , , drop = FALSE]))
    colnames(df_summary) <- c("gene", "sample", "generalCellID", "mean_z")
    df_summary$mean_z <- as.numeric(df_summary$mean_z)
    df_summary$sample <- as.character(df_summary$sample)
    df_summary$generalCellID <- as.character(df_summary$generalCellID)
    df_summary$generalCellID[df_summary$generalCellID == "Somatic"] <- "Sertoli"
    df_summary <- df_summary[df_summary$sample %in% stage_levels, , drop = FALSE]
    df_summary <- df_summary[is.finite(df_summary$mean_z), , drop = FALSE]
    if (!nrow(df_summary)) {
      return(
        ggplot() +
          theme_void() +
          annotate("text", x = 0, y = 0, label = "No data available for selected genes")
      )
    }

    df_looped <- df_summary %>%
      dplyr::filter(sample == "I-VI (Weak to Strong)") %>%
      dplyr::mutate(sample = "I-VI (looped)")

    df_summary_looped <- dplyr::bind_rows(df_summary, df_looped)

    df_summary_looped$sample <- factor(
      df_summary_looped$sample,
      levels = stage_levels_with_loop
    )

    scale_vec <- function(x) {
      if (length(unique(x)) <= 1) {
        return(rep(0, length(x)))
      }
      as.numeric(scale(x))
    }

    df_rescaled <- df_summary_looped %>%
      dplyr::group_by(gene, generalCellID) %>%
      dplyr::mutate(scaled_expr = scale_vec(mean_z)) %>%
      dplyr::ungroup()
    desired_cols <- c("Spermatogonia", "Spermatocyte", "Round Spermatid", "Elongating Spermatid", "Sertoli")
    df_rescaled$generalCellID <- factor(
      df_rescaled$generalCellID,
      levels = c(desired_cols, setdiff(unique(df_rescaled$generalCellID), desired_cols))
    )

    df_rescaled$plot_row <- dplyr::case_when(
      df_rescaled$gene %in% row1_genes ~ "Row 1",
      df_rescaled$gene %in% row2_genes ~ "Row 2",
      df_rescaled$gene %in% row3_genes ~ "Row 3"
    )
    df_rescaled$plot_row <- factor(df_rescaled$plot_row, levels = c("Row 1", "Row 2", "Row 3"))
    df_rescaled$n_cells <- NA_integer_

    pal_manual <- c(
      "Stra8"   = "#1F78B4",
      "Stra6"   = "#A6CEE3",
      "Aldh1a1" = "#B2DF8A",
      "Aldh1a2" = "#33A02C",
      "Aldh1a3" = "#1D6914",
      "Cyp26a1" = "#FB9A99",
      "Cyp26b1" = "#E31A1C",
      "Cyp26c1" = "#FF7F00"
    )
    pal_use <- pal_manual[intersect(names(pal_manual), unique(df_rescaled$gene))]
    extra_genes <- setdiff(unique(df_rescaled$gene), names(pal_use))
    if (length(extra_genes)) {
      pal_use <- c(pal_use, setNames(scales::hue_pal()(length(extra_genes)), extra_genes))
    }

    axis_col <- if (dark_theme) "#e2e8f0" else "#1e293b"
    grid_col <- if (dark_theme) "#1f2937" else "grey85"
    bg_col   <- if (dark_theme) "#050815" else "#ffffff"
    strip_bg <- if (dark_theme) "#0f172a" else "grey92"

    base_size <- if (pub_theme) 6 else 12

    build_row_plot <- function(row_name, show_x = FALSE) {
      df_row <- dplyr::filter(df_rescaled, plot_row == row_name)
      x_text <- if (show_x) {
        element_text(angle = 45, hjust = 1, color = axis_col, size = base_size * 1.1)
      } else {
        element_blank()
      }
      ggplot(df_row, aes(x = sample, y = scaled_expr, color = gene, group = gene,
                         text = sprintf(
                           "Gene: %s<br>Cell: %s<br>Stage: %s<br>Scaled: %.3f<br>Mean: %.3f<br>N cells: %s",
                           gene, generalCellID, as.character(sample), scaled_expr, mean_z,
                           ifelse(is.na(n_cells), "N/A", as.character(n_cells))
                         ))) +
        geom_line(linewidth = 0.5) +
        geom_point(size = 1.5) +
        facet_wrap(~ generalCellID, nrow = 1, scales = "fixed") +
        coord_cartesian(ylim = c(-2, 2)) +
        theme_minimal(base_size = base_size) +
        labs(x = NULL, y = NULL) +
        theme(
          panel.background = element_rect(fill = bg_col, colour = NA),
          plot.background = element_rect(fill = bg_col, colour = NA),
          panel.grid.major = element_line(color = grid_col),
          panel.grid.minor = element_blank(),
          axis.text.x = x_text,
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = if (show_x) element_line(color = axis_col, linewidth = 0.2) else element_blank(),
          strip.background = element_rect(fill = strip_bg, colour = NA),
          strip.text = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")
        ) +
        scale_x_discrete(drop = FALSE, labels = stage_label_short) +
        scale_color_manual(values = pal_use)
    }

    if (isTRUE(pub_layout)) {
      rows <- levels(df_rescaled$plot_row)
      plots <- lapply(seq_along(rows), function(i) build_row_plot(rows[[i]], show_x = i == length(rows)))
      combined <- patchwork::wrap_plots(plots, ncol = 1, guides = "collect") &
        theme(
          plot.background = element_rect(fill = bg_col, colour = NA),
          panel.background = element_rect(fill = bg_col, colour = NA),
          legend.position = "right",
          legend.background = element_rect(fill = bg_col, colour = NA),
          legend.box.background = element_rect(fill = bg_col, colour = NA),
          legend.text = element_text(color = axis_col),
          legend.title = element_text(color = axis_col)
        )
      return(
        combined +
          patchwork::plot_annotation(
            theme = theme(
              plot.background = element_rect(fill = bg_col, colour = NA),
              panel.background = element_rect(fill = bg_col, colour = NA)
            )
          )
      )
    }

    ggplot(df_rescaled, aes(x = sample, y = scaled_expr, color = gene, group = gene,
                            text = sprintf("Gene: %s<br>Cell: %s<br>Stage: %s<br>Scaled: %.2f<br>Mean: %.2f",
                                           gene, generalCellID, sample, scaled_expr, mean_z))) +
      geom_line(linewidth = if (pub_theme) 0.4 else 0.5) +
      geom_point(size = if (pub_theme) 1.2 else 1.5) +
      facet_grid(plot_row ~ generalCellID, scales = "fixed") +
      coord_cartesian(ylim = c(-2, 2)) +
      theme_minimal(base_size = if (pub_theme) 11 else 14) +
      labs(x = "Stage", y = "Z-scored Expression") +
      theme(
        panel.background = element_rect(fill = bg_col, colour = NA),
        plot.background = element_rect(fill = bg_col, colour = NA),
        panel.grid.major = element_line(color = grid_col),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, color = axis_col),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1,1,1,3.5), "lines"),
        strip.background = element_rect(fill = strip_bg, colour = NA),
        strip.text = element_text(color = axis_col),
        strip.text.y = element_text(angle = 0, color = axis_col),
        legend.background = element_rect(fill = bg_col, colour = NA),
        legend.text = element_text(color = axis_col),
        legend.title = element_text(color = axis_col)
      ) +
      scale_x_discrete(drop = FALSE, labels = stage_label_short) +
      scale_color_manual(values = pal_use, guide = guide_legend(override.aes = list(size = 3)))
  }
  
  # DotPlot rendering for Figure 5A
  ra_dot_refresh_state <- debounce(reactive({
    list(input$ra_genes, input$ra_cell_types, is_dark_mode())
  }), 800)
  observeEvent(ra_dot_refresh_state(), {
    ra_dot_tick(ra_dot_tick() + 1)
  }, ignoreInit = TRUE)

  ra_dotplot_event <- eventReactive(ra_dot_tick(), {
    req(ra_plot_gate$ra_dotplot)
    make_fig5A(pub_theme = FALSE, dark_theme = is_dark_mode())
  }, ignoreNULL = FALSE)

  output$ra_dotplot <- renderPlot({
    sc_profile_eval("ra", "ra_dotplot", {
      # Read control state without subscribing the output to live input changes.
      selected_genes <- isolate(as.character(unlist(input$ra_genes, use.names = FALSE)))
      selected_cell_types <- isolate(as.character(input$ra_cell_types))
      sc_profile_note(
        selected_genes = selected_genes,
        gene_count = length(selected_genes),
        selected_cell_types = selected_cell_types,
        selected_cell_type_count = length(selected_cell_types)
      )
      ra_dotplot_event()
    })
  }, res = 96)
  outputOptions(output, "ra_dotplot", suspendWhenHidden = TRUE)
  
  
  
  #FIGURE 5c CODE:
  
  # Lineplot defaults now set in ensure_line_assets().
  
  
  # 5C Lineplot rendering
  ra_line_refresh_state <- debounce(reactive({
    list(
      input$ra_line_genes_row1,
      input$ra_line_genes_row2,
      input$ra_line_genes_row3,
      is_dark_mode()
    )
  }), 800)
  observeEvent(ra_line_refresh_state(), {
    ra_line_tick(ra_line_tick() + 1)
  }, ignoreInit = TRUE)

  ra_lineplot_event <- eventReactive(ra_line_tick(), {
    req(ra_plot_gate$ra_lineplot)
    make_fig5C(pub_theme = FALSE, dark_theme = is_dark_mode())
  }, ignoreNULL = FALSE)

  output$ra_lineplot <- renderPlot({
    sc_profile_eval("ra", "ra_lineplot", {
      # Read control state without subscribing the output to live input changes.
      selected_genes <- unique(as.character(c(
        isolate(input$ra_line_genes_row1),
        isolate(input$ra_line_genes_row2),
        isolate(input$ra_line_genes_row3)
      )))
      selected_genes <- selected_genes[nzchar(selected_genes)]
      sc_profile_note(
        selected_genes = selected_genes,
        gene_count = length(selected_genes)
      )
      ra_lineplot_event()
    })
  }, res = 96)
  outputOptions(output, "ra_lineplot", suspendWhenHidden = TRUE)
  

  #6D figure:
  if (!exists("communication_score")) {
    communication_score <- read.csv(sc_app_path("CellChat_all_stage_communication_score_LR_reverse.csv", app_dir = app_dir))
    rownames(communication_score) <- communication_score$lr_pair
  }
  
  ccc_default_lr <- c(
    "FGF17_FGFR1", "FGF2_FGFR3", "FGF7_FGFR2", "FGF1_FGFR1",
    "FGF9_FGFR3",
    "IGF1_IGF1R", "IGF2_IGF1R", "KITL_KIT", "GDNF_GFRA1", "NRTN_GFRA2",
    "PDGFA_PDGFRA", "CXCL12_CXCR4",
    "WNT3A_FZD7_LRP5", "WNT9A_FZD7_LRP5", "WNT8B_FZD7_LRP5", "WNT7B_FZD7_LRP5",
    "WNT3_FZD7_LRP5", "WNT1_FZD7_LRP5", "WNT5A_FZD7", "WNT11_FZD7",
    "DHH_PTCH1", "DLL3_NOTCH3", "DLL4_NOTCH1", "JAG2_NOTCH2", "JAG1_NOTCH1",
    "SEMA5A_PLXNA1", "SEMA6B_PLXNA2", "SEMA6A_PLXNA4", "SEMA3C_NRP1_PLXNA1",
    "TGFB1_TGFBR1_TGFBR2", "TGFB2_TGFBR1_TGFBR2"
  )

  ensure_ccc_defaults <- function() {
    ccc_choices <- unique(communication_score[["lr_pair"]])
    defaults <- ccc_default_lr[ccc_default_lr %in% ccc_choices]
    if (!length(defaults)) defaults <- head(ccc_choices, 10)
    if (is.null(input$ccc_lr_select) || length(input$ccc_lr_select) == 0) {
      updateSelectizeInput(
        session,
        "ccc_lr_select",
        choices = ccc_choices,
        selected = defaults,
        server = TRUE
      )
    }
    invisible(NULL)
  }

  # Seed defaults at startup, then keep retrying while the first heatmap renders.
  observe({
    ensure_ccc_defaults()
  })

  observeEvent(input$mainTabs, {
    if (!identical(input$mainTabs, "cell2cell_heatmaps")) return()
    ensure_ccc_defaults()
    startup_reload_retries$cell2cell_heatmaps <- 4L
  }, ignoreNULL = TRUE)

  observe({
    attempts_left <- startup_reload_retries$cell2cell_heatmaps
    if (attempts_left <= 0L || !identical(input$mainTabs, "cell2cell_heatmaps")) return()
    invalidateLater(1400, session)
    ensure_ccc_defaults()
    ccc_tick(ccc_tick() + 1)
    startup_reload_retries$cell2cell_heatmaps <- attempts_left - 1L
  })
  
  # Generate heatmap
  ccc_refresh_state <- debounce(reactive({
    list(input$ccc_lr_select, is_dark_mode())
  }), 2000)
  observeEvent(ccc_refresh_state(), {
    ccc_tick(ccc_tick() + 1)
  }, ignoreInit = TRUE)

  ccc_heatmap_event <- eventReactive(ccc_tick(), {
    req(input$ccc_lr_select)
    
    selected <- input$ccc_lr_select
    
    # Safer subsetting
    filtered <- communication_score[communication_score$lr_pair %in% selected, 
                                    c("lr_pair", "X_DARK_score", "X_PALE_score", "X_PALE2WEAK_score", "X_WEAK2STRONG_score")]
    
    if (nrow(filtered) == 0) return(NULL)  # Guard for safety
    
    mat <- as.matrix(filtered[, -1])  # Remove lr_pair column
    rownames(mat) <- filtered$lr_pair
    
    colnames(mat) <- c("I-VI", "VII-VIII", "IX-X", "XI-XII")
    
    dark_mode <- is_dark_mode()
    plot_bg <- if (dark_mode) "#050815" else "#ffffff"
    axis_col <- if (dark_mode) "#e2e8f0" else "#1e293b"
    colorscale <- if (dark_mode) viridisLite::viridis(256) else "Blues"
    
    plot_ly(
      z = mat,
      x = colnames(mat),
      y = rownames(mat),
      type = "heatmap",
      colors = colorscale
    ) %>%
      layout(
        xaxis = list(title = "Stage", color = axis_col, tickfont = list(color = axis_col)),
        yaxis = list(title = "Ligand-Receptor Pair", color = axis_col, tickfont = list(color = axis_col)),
        margin = list(l = 160),
        paper_bgcolor = plot_bg,
        plot_bgcolor = plot_bg,
        font = list(color = axis_col)
      )
  }, ignoreNULL = FALSE)

  output$ccc_heatmap <- renderPlotly({
    sc_profile_eval("ccc", "ccc_heatmap", {
      selected_pairs <- as.character(input$ccc_lr_select)
      sc_profile_note(
        selected_lr_pairs = selected_pairs,
        selected_pair_count = length(selected_pairs)
      )
      ccc_heatmap_event()
    })
  })
  # Plotly widgets can render at an incorrect size if computed while their tab is hidden.
  # Keep the default suspend behavior so the widget renders when visible, and rely on
  # client-side Plotly resize hooks on tab switches.
  outputOptions(output, "ccc_heatmap", suspendWhenHidden = TRUE)
  
  # Static builder for publication PDF (separate from Plotly view)
  make_fig6D_static <- function(selected_pairs, dark_theme = FALSE){
    req(selected_pairs)
    validate(need(exists("communication_score"), "communication_score not loaded."))
    
    filtered <- communication_score[communication_score$lr_pair %in% selected_pairs,
                                    c("lr_pair","X_DARK_score","X_PALE_score","X_PALE2WEAK_score","X_WEAK2STRONG_score")]
    validate(need(nrow(filtered) > 0, "No LR pairs selected."))
    
    mat <- as.matrix(filtered[, -1])
    rownames(mat) <- filtered$lr_pair
    colnames(mat) <- c("I-VI","VII-VIII","IX-X","XI-XII")
    
    df_long <- reshape2::melt(mat, varnames = c("lr_pair","stage"), value.name = "score")
    df_long$stage <- factor(df_long$stage, levels = c("I-VI","VII-VIII","IX-X","XI-XII"))
    
    axis_col <- if (dark_theme) "#e2e8f0" else "#1e293b"
    grid_col <- if (dark_theme) "#1f2937" else "grey90"
    bg_col   <- if (dark_theme) "#050815" else "#ffffff"
    border_col <- if (dark_theme) "#475569" else "#d1d5db"

    ggplot2::ggplot(df_long, ggplot2::aes(x = stage, y = lr_pair, fill = score)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(option = "B", name = "Score") +
      ggplot2::labs(x = "Stage", y = "Ligand–Receptor Pair") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = bg_col, colour = NA),
        plot.background = ggplot2::element_rect(fill = bg_col, colour = NA),
        panel.grid.major = ggplot2::element_line(color = grid_col),
        axis.text.x = ggplot2::element_text(color = axis_col),
        axis.text.y = ggplot2::element_text(size = 8, color = axis_col),
        axis.title = ggplot2::element_text(color = axis_col),
        panel.grid = ggplot2::element_blank(),
        legend.position = "right",
        legend.background = ggplot2::element_rect(fill = bg_col, colour = border_col),
        legend.text = ggplot2::element_text(color = axis_col),
        legend.title = ggplot2::element_text(color = axis_col),
        plot.margin = grid::unit(c(0.5,0.6,0.5,1.6), "lines")
      )
  }
  
  
  # ============================
  # PUBLICATION PDF DOWNLOADS
  # ============================
  
  # Figure 5A — RA DotPlot (vector PDF)
  output$ra_dotplot_pdf <- downloadHandler(
    filename = function() resolve_download_filename(input$ra_dotplot_name, "Figure_5A", ".pdf"),
    content = function(file){
      dark_theme <- isolate(is_dark_mode())
      p <- make_fig5A(pub_theme = TRUE, dark_theme = dark_theme)
      save_ggplot_pdf(
        file,
        p,
        width = download_dimension("ra_dotplot_w", 12),
        height = download_dimension("ra_dotplot_h", 7.5)
      )
    }
  )

  output$ra_dotplot_png <- downloadHandler(
    filename = function() resolve_download_filename(input$ra_dotplot_name, "Figure_5A", ".png"),
    content = function(file){
      dark_theme <- isolate(is_dark_mode())
      p <- make_fig5A(pub_theme = TRUE, dark_theme = dark_theme)
      save_ggplot_png(
        file,
        p,
        width = download_dimension("ra_dotplot_w", 12),
        height = download_dimension("ra_dotplot_h", 7.5)
      )
    }
  )
  
  # Figure 5C — RA LinePlot (vector PDF)
  output$ra_lineplot_pdf <- downloadHandler(
    filename = function() resolve_download_filename(input$ra_lineplot_name, "Figure_5C", ".pdf"),
    content = function(file){
      dark_theme <- isolate(is_dark_mode())
      p <- make_fig5C(pub_theme = TRUE, dark_theme = dark_theme)
      save_ggplot_pdf(
        file,
        p,
        width = download_dimension("ra_lineplot_w", 12),
        height = download_dimension("ra_lineplot_h", 7.5)
      )
    }
  )

  output$ra_lineplot_png <- downloadHandler(
    filename = function() resolve_download_filename(input$ra_lineplot_name, "Figure_5C", ".png"),
    content = function(file){
      dark_theme <- isolate(is_dark_mode())
      p <- make_fig5C(pub_theme = TRUE, dark_theme = dark_theme)
      save_ggplot_png(
        file,
        p,
        width = download_dimension("ra_lineplot_w", 12),
        height = download_dimension("ra_lineplot_h", 7.5)
      )
    }
  )
  
  # Publication PDF for 6D (vector)
  output$ccc_pdf <- downloadHandler(
    filename = function() resolve_download_filename(input$ccc_name, "Figure_6D", ".pdf"),
    content = function(file){
      dark_theme <- isolate(is_dark_mode())
      p <- make_fig6D_static(input$ccc_lr_select, dark_theme = dark_theme)
      save_ggplot_pdf(
        file,
        p,
        width = download_dimension("ccc_w", 12),
        height = download_dimension("ccc_h", 9.5)
      )
    }
  )

  output$ccc_png <- downloadHandler(
    filename = function() resolve_download_filename(input$ccc_name, "Figure_6D", ".png"),
    content = function(file){
      dark_theme <- isolate(is_dark_mode())
      p <- make_fig6D_static(input$ccc_lr_select, dark_theme = dark_theme)
      save_ggplot_png(
        file,
        p,
        width = download_dimension("ccc_w", 12),
        height = download_dimension("ccc_h", 9.5)
      )
    }
  )
  
#   session$onSessionEnded(function() {
#     clear_user_upload()
#     reset_user_error()
#   })
  
  
  
}) 
