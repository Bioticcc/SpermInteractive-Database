##########################################
#Retinoic Acid Signaling Heatmap Figure 5a
##########################################
library(scales)
library(Seurat)
library(ggplot2)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE)))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

output_dir <- file.path(get_script_dir(), "www")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

input_candidates <- c(
  file.path(get_script_dir(), "specificCellID_Original.rds"),
  file.path(get_script_dir(), "specificCellID_slim_nocounts.rds"),
  file.path(get_script_dir(), "specificCellID_slim.rds"),
  file.path(get_script_dir(), "specificCellID.rds"),
  "ShinyApp/shinyApp/specificCellID_Original.rds",
  "ShinyApp/shinyApp/specificCellID_slim_nocounts.rds",
  "ShinyApp/shinyApp/specificCellID_slim.rds",
  "ShinyApp/shinyApp/specificCellID.rds"
)
input_path <- input_candidates[file.exists(input_candidates)][1]
if (is.na(input_path) || !nzchar(input_path)) {
  stop("Could not find a specificCellID input file. Checked: ", paste(input_candidates, collapse = ", "))
}
message("Using input Seurat file: ", normalizePath(input_path, mustWork = FALSE))
specificCellID <- readRDS(file = input_path)
if (length(Seurat::Reductions(specificCellID)) > 0) {
  DimPlot(specificCellID)
}
obj_ra_dotplot <- specificCellID

# Select genes to plot 
#***(would be great if we can make this a drop down on the Shinny App Database)***
ra_genes_to_plot <- c("Rbp1", "Rbp4","Rdh10", "Dmrt1", "Rxrg", "Rxrb", "Rxra", "Rarg", "Rarb",  "Rara",  "Cyp26c1",  "Cyp26b1", "Cyp26a1", "Aldh1a3", "Aldh1a2", "Aldh1a1", "Stra6", "Stra8")  

#select color scheme for heatmap
RdBu <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(100))

RA_dp <- DotPlot(obj_ra_dotplot, features = ra_genes_to_plot, dot.scale = 5.5, assay = NULL,
              col.min = -2.5,
              col.max = 2.5,
              dot.min = 0,
              idents = NULL,
              group.by = NULL,
              split.by = NULL,
              cluster.idents = FALSE,
              scale = TRUE,
              scale.by = "size",
              scale.min = NA,
              scale.max = 30) +
  scale_color_gradientn(
    colors = RdBu,
    values = rescale(c(-2.5, 0, 2.5)),  # Ensures white is at 0
    limits = c(-2.5, 2.5),              # Make sure limits match
    oob = squish                        # Prevents clipping outliers
  ) +
  theme_minimal(base_size = 14) +
  RotatedAxis() +
  theme(# Axes text
    axis.text.x = element_text(color = "black", size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(color = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    
    
    # Panel border
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # <- black border
    
    # Tick marks
    axis.ticks = element_line(color = "black", size = 0.5),  # black ticks, slightly thicker
    axis.ticks.length = unit(0.25, "cm")  # longer ticks
  ) +
  labs(title = NULL) + 
  guides(color = guide_colorbar(title = "Average Expression"),
         size = guide_legend(title = "Percent Expressed")) +
  # Override the point aesthetics to include a black border
  geom_point(
    mapping = aes(size = pct.exp, color = avg.exp.scaled),
    shape = 21,             # circle with fill and stroke
    stroke = 0.3,           # border thickness
    fill = NA,              # keeps original color scale as fill
    colour = "black"        # sets outline color
  ) + coord_flip() & NoLegend()



ggsave(
  filename = file.path(output_dir, "dotplot_RA_specificCellID.pdf"),
  plot = RA_dp,                           
  width = 7,                        
  height = 5,                       
  units = "in",                    
  dpi = 600                         
)











######################################################
#Retinoic Acid Signaling Across Stages Figure 5c
######################################################
library(Seurat)
library(reshape2)
library(dplyr)
library(ggplot2)


obj_ra_lineplot <- obj_ra_dotplot
levels(obj_ra_lineplot)
obj_ra_lineplot <- subset(obj_ra_lineplot, idents = c("Aund", "A1-2", "A3-4", "Ain", "Type B", "ePL", "lPL", "L", "L/Z", 
                                                    "Z", "PaI-VI", "PaVII-VIII", "PaIX-X", "D/MI", "Rd1", "Rd2-3", "Rd4-5", "Rd6", 
                                                    "Rd7", "Rd8", "El9", "El10", "El11", "El12-13", "El14-15", "El16", 
                                                    "SC_I-VIII", "SC_VII-VIII", "SC_IX-XII", "SC_XI-VI", "SC_All_Stages"))
Idents(obj_ra_lineplot) <- "generalCellID"
obj_ra_lineplot <- RenameIdents(obj_ra_lineplot, "Somatic" = "Sertoli")
levels(obj_ra_lineplot)

####
#nonscaled
####


make_rescaled_looped_plot <- function(obj, genes_to_plot, title_label) {
  #Fetch expression + metadata
  df <- FetchData(obj, vars = c(genes_to_plot, "sample", "generalCellID"))
  df$cell <- rownames(df)
  
  #Long format
  df_long <- melt(df, id.vars = c("cell", "sample", "generalCellID"),
                  variable.name = "gene", value.name = "zscore")
  
  # Summary
  df_summary <- df_long %>%
    group_by(gene, generalCellID, sample) %>%
    summarise(mean_z = mean(zscore, na.rm = TRUE), .groups = "drop")
  
  # Add looped sample
  df_looped <- df_summary %>%
    filter(sample == "I-VI (Weak to Strong)") %>%
    mutate(sample = "I-VI (looped)")
  df_summary_looped <- bind_rows(df_summary, df_looped)
  
  # Set sample factor levels
  df_summary_looped$sample <- factor(df_summary_looped$sample, levels = c(
    "I-VI (Weak to Strong)", "VII-VIII (Dark)", "IX-X (Pale)", 
    "XI-XII (Pale to Weak)", "I-VI (looped)"
  ))
  
  # Rescale expression by gene within cell type
  df_rescaled <- df_summary_looped %>%
    group_by(gene, generalCellID) %>%
    mutate(scaled_expr = scale(mean_z)[, 1]) %>%
    ungroup()
  
  # Plot
  ggplot(df_rescaled, aes(x = sample, y = scaled_expr, color = gene, group = gene)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.5) +
    facet_wrap(~ generalCellID, scales = "fixed", nrow = 1) +
    coord_cartesian(ylim = c(-2, 2)) +
    theme_minimal(base_size = 14) +
    labs(
      x = "Stage",
      y = "Z-scored Expression"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

#***(would be great if we could allow databaset users to select their genes of interest)***
# For Stra8 and Stra6
stra_colors <- c("Stra8" = "#A6CEE3", "Stra6" = "#33A02C")
# For Aldh1a genes
aldh_colors <- c("Aldh1a1" = "#1F78B4", "Aldh1a2" = "#FB9A99", "Aldh1a3" = "#E31A1C")
# For Cyp26 genes
cyp26_colors <- c("Cyp26a1" = "#FDBF6F", "Cyp26b1" = "#FF7F00", "Cyp26c1" = "#CAB2D6")


# For Stra8 and Stra6
stra_colors <- c("Stra8" = "#1F78B4", "Stra6" = "#A6CEE3")
# For Aldh1a genes
aldh_colors <- c("Aldh1a1" = "#B2DF8A", "Aldh1a2" = "#33A02C", "Aldh1a3" = "#1D6914")
# For Cyp26 genes
cyp26_colors <- c("Cyp26a1" = "#FB9A99", "Cyp26b1" = "#E31A1C", "Cyp26c1" = "#FF7F00")



stra68 <- make_rescaled_looped_plot(obj_ra_lineplot, 
                                    genes_to_plot = c("Stra8", "Stra6"), 
                                    title_label = "Stra Genes Across Stages (Z-Scored)") + scale_color_manual(values = stra_colors)

aldh1a123 <- make_rescaled_looped_plot(obj_ra_lineplot, 
                                       genes_to_plot = c("Aldh1a1", "Aldh1a2", "Aldh1a3"), 
                                       title_label = "Aldh1a Genes Across Stages (Z-Scored)") + scale_color_manual(values = aldh_colors)

cyp26abc1 <- make_rescaled_looped_plot(obj_ra_lineplot, 
                                       genes_to_plot = c("Cyp26a1", "Cyp26b1", "Cyp26c1"), 
                                       title_label = "Cyp26 Genes Across Stages (Z-Scored)") + scale_color_manual(values = cyp26_colors)

stra68 <- stra68 +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank(),  
        panel.grid.minor = element_blank()
  ) + theme_minimal(base_size = 6, base_family = "sans")

aldh1a123 <- aldh1a123 +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank(),  
        panel.grid.minor = element_blank()
  ) + theme_minimal(base_size = 6, base_family = "sans")

cyp26abc1 <- cyp26abc1 +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank(),  
        panel.grid.minor = element_blank(), 
  ) 


figure7_C <- (stra68 / aldh1a123 / cyp26abc1) &
  theme_minimal(base_size = 6, base_family = "sans") +
 theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave(
  filename = file.path(output_dir, "figure7_C_RA_across_stages.pdf"),
  plot = figure7_C,
  width = 177,
  height = 82,
  units = "mm"
)
