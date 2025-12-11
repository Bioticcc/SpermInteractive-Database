# Benchmark memory requirements for the Shiny app assets.
# Run from the repo root with:
#   Rscript ShinyApp/shinyApp/bench_memory_usage.R

suppressPackageStartupMessages({
  library(bench)
  library(Seurat)
  library(ShinyCell)
})

args <- commandArgs(trailingOnly = FALSE)
script_path <- grep("^--file=", args, value = TRUE)
if (length(script_path)) {
  app_dir <- dirname(normalizePath(sub("^--file=", "", script_path[1])))
  setwd(app_dir)
} else {
  app_dir <- getwd()
}

source("data_loaders.R", chdir = TRUE)

report_memory <- function(label) {
  mem <- bench::bench_process_memory()
  cat(
    sprintf(
      "%-32s current: %s | max: %s\n",
      paste0(label, ":"),
      format(mem["current"]),
      format(mem["max"])
    )
  )
}

cat(sprintf("Using app assets in: %s\n\n", app_dir))

report_memory("Baseline (bench loaded)")

loaded <- list()

loaded$final_staged_object <- get_final_staged_object()
report_memory("After final_staged_object")

loaded$specific_obj <- get_specific_obj()
report_memory("After specificCellID")

loaded$sc1 <- list(
  conf = get_sc1conf(),
  def = get_sc1def(),
  gene = get_sc1gene(),
  meta = get_sc1meta()
)
report_memory("After sc1 assets")

loaded$sc2 <- list(
  conf = get_sc2conf(),
  def = get_sc2def(),
  gene = get_sc2gene(),
  meta = get_sc2meta()
)
report_memory("After sc2 assets")

loaded$sc3 <- list(
  conf = get_sc3conf(),
  def = get_sc3def(),
  gene = get_sc3gene(),
  meta = get_sc3meta()
)
report_memory("After sc3 assets")

loaded$cellchat <- get_cellchat_scores()
report_memory("After CellChat scores")

loaded$gene_metadata <- get_gene_metadata()
report_memory("After gene metadata")

cat("\nRunning garbage collection before final check...\n")
gc()
report_memory("Post-GC total")
