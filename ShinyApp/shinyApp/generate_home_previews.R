#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(Seurat)
  library(ggplot2)
  library(dplyr)
  library(reshape2)
  library(scales)
  library(viridisLite)
  library(grid)
})

args <- commandArgs(trailingOnly = FALSE)
script_path <- NULL
if (length(args)) {
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
  }
}
if (is.null(script_path)) {
  script_path <- normalizePath(".")
}
script_dir <- dirname(script_path)
setwd(script_dir)

message("Generating home tab preview images...")

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

output_dir <- file.path(script_dir, "www")
ensure_dir(output_dir)

specific_obj <- readRDS("specificCellID.rds")
Idents(specific_obj) <- Idents(specific_obj)

default_ra_genes <- c("Stra8", "Stra6", "Aldh1a1", "Aldh1a2", "Cyp26a1", "Rxra")
default_ra_genes <- intersect(default_ra_genes, rownames(specific_obj))
if (!length(default_ra_genes)) {
  stop("No default RA genes found in specificCellID object.")
}

cell_types <- levels(Idents(specific_obj))
if (!length(cell_types)) {
  stop("No cell types available in specificCellID object.")
}

make_ra_dotplot <- function(obj, genes, idents_keep) {
  subset_obj <- if (length(idents_keep)) subset(obj, idents = idents_keep) else obj
  DotPlot(
    subset_obj,
    features  = genes,
    dot.scale = 8.25,
    assay     = NULL,
    col.min   = -2.5,
    col.max   = 2.5,
    dot.min   = 0,
    idents    = NULL,
    group.by  = NULL,
    split.by  = NULL,
    cluster.idents = FALSE,
    scale     = TRUE,
    scale.by  = "size",
    scale.min = NA,
    scale.max = 30
  ) +
    scale_color_gradientn(
      colors = c("#93c5fd", "#a78bfa", "#34d399"),
      values = rescale(c(-2.5, 0, 2.5)),
      limits = c(-2.5, 2.5),
      oob    = squish
    ) +
    theme_minimal(base_size = 12) +
    Seurat::RotatedAxis() +
    theme(
      axis.text.x = element_text(color = "#1e293b", size = 8, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(color = "#1e293b", size = 9),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.ticks.length = unit(0.18, "cm")
    ) +
    guides(
      color = guide_colorbar(title = "Avg. expression"),
      size  = guide_legend(title = "% expressed")
    ) +
    geom_point(
      mapping = aes(size = pct.exp, color = avg.exp.scaled),
      shape = 21,
      stroke = 0.25,
      fill = NA,
      colour = "black"
    ) +
    coord_flip()
}

line_defaults <- list(
  row1 = c("Stra8", "Stra6"),
  row2 = c("Aldh1a1", "Aldh1a2"),
  row3 = c("Cyp26a1", "Rxra")
)

line_defaults <- lapply(line_defaults, function(genes) intersect(genes, rownames(specific_obj)))
if (!all(lengths(line_defaults))) {
  stop("One of the default RA line plot gene groups is empty.")
}

make_ra_lineplot <- function(obj, row1_genes, row2_genes, row3_genes) {
  Idents(obj) <- "generalCellID"
  all_genes <- unique(c(row1_genes, row2_genes, row3_genes))
  df <- FetchData(obj, vars = c(all_genes, "sample", "generalCellID"))
  df$cell <- rownames(df)
  
  df_long <- reshape2::melt(
    df,
    id.vars = c("cell", "sample", "generalCellID"),
    variable.name = "gene",
    value.name = "zscore"
  )
  
  df_summary <- df_long %>%
    group_by(gene, generalCellID, sample) %>%
    summarise(mean_z = mean(zscore, na.rm = TRUE), .groups = "drop")
  
  df_looped <- df_summary %>%
    filter(sample == "I-VI (Weak to Strong)") %>%
    mutate(sample = "I-VI (looped)")
  
  df_summary_looped <- bind_rows(df_summary, df_looped)
  
  df_summary_looped$stage <- case_when(
    df_summary_looped$sample == "I-VI (Weak to Strong)" ~ "I-VI",
    df_summary_looped$sample == "VII-VIII (Dark)" ~ "VII-VIII",
    df_summary_looped$sample == "IX-X (Pale)" ~ "IX-X",
    df_summary_looped$sample == "XI-XII (Pale to Weak)" ~ "XI-XII",
    df_summary_looped$sample == "I-VI (looped)" ~ "I-VI",
    TRUE ~ as.character(df_summary_looped$sample)
  )
  df_summary_looped$stage <- factor(df_summary_looped$stage, levels = c("I-VI", "VII-VIII", "IX-X", "XI-XII"))
  
  df_rescaled <- df_summary_looped %>%
    group_by(gene, generalCellID) %>%
    mutate(scaled_expr = scale(mean_z)[, 1]) %>%
    ungroup()
  
  df_rescaled$plot_row <- case_when(
    df_rescaled$gene %in% row1_genes ~ "Row 1",
    df_rescaled$gene %in% row2_genes ~ "Row 2",
    df_rescaled$gene %in% row3_genes ~ "Row 3",
    TRUE ~ NA_character_
  )
  df_rescaled$plot_row <- factor(df_rescaled$plot_row, levels = c("Row 1", "Row 2", "Row 3"))
  
  ggplot(df_rescaled, aes(x = stage, y = scaled_expr, color = gene, group = gene)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.5) +
    facet_grid(plot_row ~ generalCellID, scales = "fixed") +
    coord_cartesian(ylim = c(-2, 2)) +
    theme_minimal(base_size = 12) +
    labs(x = "Stage", y = "Z-scored expression") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 1, 3.5), "lines"),
      strip.text.y = element_text(angle = 0)
    )
}

dotplot_path <- file.path(output_dir, "ra_dotplot_preview.png")
lineplot_path <- file.path(output_dir, "ra_lineplot_preview.png")
heatmap_path <- file.path(output_dir, "ccc_heatmap_preview.png")

dot_plot <- make_ra_dotplot(specific_obj, default_ra_genes, cell_types)
ggsave(dotplot_path, dot_plot, width = 5.5, height = 3.5, dpi = 160)

line_plot <- make_ra_lineplot(
  specific_obj,
  line_defaults$row1,
  line_defaults$row2,
  line_defaults$row3
)
ggsave(lineplot_path, line_plot, width = 6.2, height = 3.6, dpi = 160)

communication_score <- read.csv("CellChat_all_stage_communication_score_LR_reverse.csv")
if (!("lr_pair" %in% names(communication_score))) {
  if ("X" %in% names(communication_score)) {
    communication_score$lr_pair <- communication_score$X
  } else {
    stop("Expected column 'lr_pair' not present in communication score data.")
  }
}

selected_pairs <- unique(communication_score$lr_pair)
if (!length(selected_pairs)) {
  stop("Communication score CSV has no lr_pair values.")
}
selected_pairs <- head(selected_pairs, 10)

make_fig6d_preview <- function(df, selection) {
  filtered <- df[df$lr_pair %in% selection,
                 c("lr_pair", "X_DARK_score", "X_PALE_score", "X_PALE2WEAK_score", "X_WEAK2STRONG_score")]
  if (!nrow(filtered)) {
    stop("No rows available for selected LR pairs.")
  }
  mat <- as.matrix(filtered[, -1])
  rownames(mat) <- filtered$lr_pair
  colnames(mat) <- c("I-VI", "VII-VIII", "IX-X", "XI-XII")
  
  df_long <- reshape2::melt(mat, varnames = c("lr_pair", "stage"), value.name = "score")
  df_long$stage <- factor(df_long$stage, levels = c("I-VI", "VII-VIII", "IX-X", "XI-XII"))
  
  ggplot(df_long, aes(x = stage, y = lr_pair, fill = score)) +
    geom_tile() +
    scale_fill_viridis_c(option = "B", name = "Score") +
    labs(x = "Stage", y = "Ligand–Receptor Pair") +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y = element_text(size = 7),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.margin = unit(c(0.5, 0.6, 0.5, 1.2), "lines")
    )
}

heatmap_plot <- make_fig6d_preview(communication_score, selected_pairs)
ggsave(heatmap_path, heatmap_plot, width = 5.4, height = 3.4, dpi = 160)

message("Preview images saved to ", output_dir)
