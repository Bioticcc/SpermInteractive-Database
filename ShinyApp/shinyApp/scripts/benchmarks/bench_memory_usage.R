# ---------------------------------------------------------------------------
# Shiny app asset memory benchmark
# ---------------------------------------------------------------------------
# Run from the repo root with:
#   Rscript ShinyApp/shinyApp/scripts/benchmarks/bench_memory_usage.R
# This script loads representative runtime assets in sequence and reports the
# process memory watermark after each load.

# ---------------------------------------------------------------------------
# Package imports and app-root bootstrap
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(bench)
  library(Seurat)
  library(ShinyCell)
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
source(sc_app_path("scripts", "support", "data_loaders.R", app_dir = app_dir))

# ---------------------------------------------------------------------------
# Reporting helper
# ---------------------------------------------------------------------------
report_memory <- function(label) {
  mem <- bench::bench_process_memory()
  return(cat(
    sprintf(
      "%-32s current: %s | max: %s\n",
      paste0(label, ":"),
      format(mem["current"]),
      format(mem["max"])
    )
  ))
}

cat(sprintf("Using app assets in: %s\n\n", app_dir))

# ---------------------------------------------------------------------------
# Sequential asset load benchmark
# ---------------------------------------------------------------------------
# The order mirrors historically expensive runtime surfaces so changes in output
# are easy to compare across cleanup passes.
report_memory("Baseline (bench loaded)")

loaded <- list()

loaded$final_staged_object <- get_final_staged_object()
report_memory("After final_staged_object")

loaded$specific_obj <- get_specific_obj()
report_memory("After specificCellID")

loaded$sc3 <- list(
  conf = get_sc3conf(),
  def = get_sc3def(),
  gene = get_sc3gene(),
  meta = get_sc3meta()
)
report_memory("After sc3 assets")

loaded$cellchat <- get_cellchat_scores()
report_memory("After CellChat scores")

cat("\nRunning garbage collection before final check...\n")
gc()
report_memory("Post-GC total")
