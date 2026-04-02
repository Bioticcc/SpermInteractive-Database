# SpermInteractive (README IS WIP)
Welcome to SpermInteractive. This database lets you generate editable figures based primarily on:

- Hayden McSwiggin, *Single Nuclei Analysis of Staged Seminifierous Tubules* (Unpublished, expected mid 2026)

You can also build custom figures from the Seurat-derived datasets used in this project. Figure outputs can be downloaded as PNG or PDF.

## Documentation Folder
- AI handoff summary: [`AI_HANDOFF_SUMMARY.md`](AI_HANDOFF_SUMMARY.md)
- Repo file classification: [`REPO_FILE_CLASSIFICATION.md`](REPO_FILE_CLASSIFICATION.md)
- Network baseline protocol: [`NETWORK_BASELINE_RESET.md`](NETWORK_BASELINE_RESET.md)
- Current network findings report: [`NETWORK_PERFORMANCE_REPORT.md`](NETWORK_PERFORMANCE_REPORT.md)

## Usage Guide
### 1. Start on Home
- On the home page, use the icon cards below the welcome/get started boxes.
- Each icon redirects to a corresponding main figure or interactive view.

### 2. Navigate with the top menu
- **Staged Testis**: Primary full dataset used across the project.
- **Subset tabs**: Same figure workflows as Staged Testis, but restricted to subset-specific cells.
- **Interactive Data**: Custom interactive views (including the spermatogenesis table, RA analysis, and cell-cell communication heatmap).

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
- **Language/Framework**: R, Shiny
- **Core analysis/data packages**: Seurat, ShinyCell, data.table, Matrix, hdf5r
- **Plotting/interactive packages**: ggplot2, plotly, DT, ggrepel, patchwork, viridisLite, RColorBrewer, scales
- **App structure**:
- UI: `ShinyApp/shinyApp/ui.R`
- Server logic: `ShinyApp/shinyApp/server.R`
- Shared tab builders: `ShinyApp/shinyApp/ra_tabs.R`
- **Key app functions**:
- Embedding/overlay plotting: `scDRcell()`, `scDRgene()`, `scDRcoex()`
- RA figures: `make_fig5A()`, `make_fig5C()`
- Main-figure wiring: `build_main_figures_tab()`, `bind_main_figures()`
- Spermatogenesis modal/table logic: `show_button_modal()`
- **Precomputed assets (memory-optimized runtime)**:
- ShinyCell config/meta/gene/h5 files: `sc3*` to `sc7*` (`.rds`, `.h5`)
- RA assets: `ra_dot_avg_expr.rds`, `ra_dot_pct_expr.rds`, `ra_line_mean_expr.rds`
- Table assets: `spg_avg_expr_by_button.rds`, `interactive_genes.rds`

## Performance Baselines
- Network baselines should use Firefox Network Monitor export or HAR, not
  Firefox performance profile JSON.
- Capture and interpretation guidance lives in `NETWORK_BASELINE_RESET.md`.
- HAR summaries can be generated with:
  `Rscript ShinyApp/shinyApp/bench_network_har.R path/to/home_cold_1.har`
