# SpermInteractive (README IS WIP)
Welcome to SpermInteractive. This database lets you generate editable figures based primarily on:

- Hayden McSwiggin, *Single Nuclei Analysis of Staged Seminiferous Tubules* (Unpublished, expected mid 2026)

You can also build custom figures from the Seurat-derived datasets used in this project. Figure outputs can be downloaded as PNG or PDF.

## Documentation Folder
- AI handoff summary: [`Documentation/AI_HANDOFF_SUMMARY.md`](Documentation/AI_HANDOFF_SUMMARY.md)
- Repo file classification: [`Documentation/REPO_FILE_CLASSIFICATION.md`](Documentation/REPO_FILE_CLASSIFICATION.md)
- Networking baseline protocol and current findings: [`Documentation/AI_HANDOFF_SUMMARY.md#networking`](Documentation/AI_HANDOFF_SUMMARY.md#networking)

## Usage Guide
### 1. Start on Home
- On the home page, use the icon cards below the welcome/get started boxes.
- Each icon redirects to a corresponding main figure or interactive view.

### 2. Navigate with the top menu
- **Full Atlas**: Primary full dataset used across the project. The top menu opens its Main Figures page; the other Full Atlas figure pages are reached from the embedded mini navigation bar inside the page.
- **Cell Subsets**: Top-level dropdown for Sertoli, Spermatogonia, Spermatocyte, and Spermatid subsets. Each dropdown item opens that subset's Main Figures page; other subset figure pages are reached from the embedded mini navigation bar inside the page.
- **Interactive Data**: Custom interactive views, including the spermatogenesis table, RA analysis, and cell-cell communication heatmap. The top menu opens the spermatogenesis table; the other interactive views are reached from the embedded mini navigation bar.

Navigation implementation note:
- Some tabs are intentionally registered in `ui.R` but hidden from the primary navbar with CSS so `window.navToTab(...)`, URL hashes, and lazy `mainTabs` routing still work.
- The dataset mini navigation is built in `dataset_tab_builders.R`; the interactive-data mini navigation is built in `ui.R`.

### 3. Use any figure tab
- Most tabs follow the same layout:
- Left panel: core controls (axes, metadata/grouping, genes, point size, etc.).
- Advanced controls: expandable section for detailed plotting/styling options.
- Export controls: download buttons for PNG/PDF near the output figure.

### 4. Interactive Data notes
- **Spermatogenesis table** (modified from Makela et al., JoVE 2020, https://dx.doi.org/10.3791/61800):
- Click a table cell to open a modal with genes above threshold for that cell, including expression values.
- Use the modal search bar to locate and highlight genes in that cell.
- Use the left-side table search to overlay expression values for a queried gene across all cells.
- The left-side panel includes an editable expression threshold, default `0.25`, and reports the searched gene's min/max average expression across table cells.
- If a gene is absent from the search list entirely, that indicates it is missing from the current generated gene assets rather than merely falling below the UI threshold.
- **RA signaling figures**:
- Interactive RA dotplot and line plot, editable and downloadable, with default views matching publication-style outputs.
- **Cell-cell communication heatmap**:
- Interactive LR-pair heatmap across stages with export support.

## Tech Stack
- **Language/runtime**: R `4.3.3` (the Docker image is based on `rocker/shiny:4.3.3`).
- **Web framework/UI**: Shiny, bslib/Bootstrap 5, custom CSS in `www/css/`, and small browser helpers in `www/presets.js` plus inline JavaScript in `ui.R`.
- **Core analysis/data packages used by runtime code**: Seurat, ShinyCell, data.table, Matrix, hdf5r, dplyr, magrittr, reshape2, jsonlite, xml2.
- **Plotting, tables, and export packages used by runtime code**: ggplot2, plotly, DT, ggrepel, ggdendro, gridExtra, patchwork, viridisLite, RColorBrewer, scales, png, ragg.
- **Shiny support packages used by runtime code**: shinyhelper, shinycssloaders, htmltools, and later.
- **Build/maintenance script packages**: Seurat, ShinyCell, data.table, Matrix, ggplot2, dplyr, png, reshape2, scales, viridisLite, jsonlite, bench, and optional `magick` support for preview-image generation.
- **Deployment/reproducibility**: `Dockerfile`, `renv.lock`, and `ShinyApp/shinyApp/.rscignore`. The app is currently hosted on shinyapps.io, with rsconnect-based deployment helper logic in `ShinyApp/global.R`.
- **Current lockfile note**: `renv.lock` currently pins the R version and a small Shiny/bslib dependency set, but it does not list every package imported by `server.R` and the build scripts. Refresh `renv.lock` before treating a clean restore or Docker build as the complete production dependency manifest.
- **App structure**:
  - UI: `ShinyApp/shinyApp/ui.R`
  - Server logic: `ShinyApp/shinyApp/server.R`
  - Shared tab builders: `ShinyApp/shinyApp/dataset_tab_builders.R`
  - Runtime/generated data root: `ShinyApp/shinyApp/Data/`
- **Key app functions**:
  - Embedding/overlay plotting: `scDRcell()`, `scDRgene()`, `scDRcoex()`
  - RA figures: `make_fig5A()`, `make_fig5C()`
  - Main-figure wiring: `build_main_figures_tab()`, `bind_main_figures()`
  - Spermatogenesis modal/table logic: `show_button_modal()`
- **Precomputed assets (memory-optimized runtime)**:
  - ShinyCell config/meta/gene/h5 files: `Data/sc3*` to `Data/sc7*` (`.rds`, `.h5`)
  - RA assets: `Data/ra_dot_avg_expr.rds`, `Data/ra_dot_pct_expr.rds`, `Data/ra_line_mean_expr.rds`
  - Table assets: `Data/spg_avg_expr_by_button.rds`, `Data/interactive_genes.rds`
  - Cell-cell communication scores: `Data/CellChat_all_stage_communication_score_LR_reverse.csv`
  - Raw generation inputs and disabled launch datasets may also live in `Data/`, but `.rscignore` excludes those from shinyapps.io deployment.

## Performance Baselines
- Network baselines should use Firefox Network Monitor export or HAR, not
  Firefox performance profile JSON.
- Capture and interpretation guidance lives in the `Networking` section of
  `Documentation/AI_HANDOFF_SUMMARY.md`.
- HAR summaries can be generated with:
  `Rscript ShinyApp/shinyApp/scripts/benchmarks/bench_network_har.R path/to/home_cold_1.har`
