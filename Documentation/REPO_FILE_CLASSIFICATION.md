# Repo File Classification

This note records which files are part of the active app surface after the
foundation cleanup pass, and which files are reference-only.

## Project Configuration
- `stylelint.config.cjs`
- `renv.lock`
- `Dockerfile`
- `.dockerignore`

## Runtime
- `ShinyApp/shinyApp/ui.R`
- `ShinyApp/shinyApp/server.R`
- `ShinyApp/shinyApp/dataset_tab_builders.R`
- `ShinyApp/shinyApp/metadata_overrides.R`
- `ShinyApp/shinyApp/button_mapping_general.R`
- `ShinyApp/shinyApp/app_support.R`
- `ShinyApp/shinyApp/www/css/style.css`
- `ShinyApp/shinyApp/www/css/theme.css`
- `ShinyApp/shinyApp/www/css/layout.css`
- `ShinyApp/shinyApp/www/css/legacy-tabs.css`
- `ShinyApp/shinyApp/www/css/interactive-data.css`
- `ShinyApp/shinyApp/www/presets.js`
- `ShinyApp/shinyApp/www/upload.js`
- `ShinyApp/shinyApp/www/favicon.ico`
- `ShinyApp/shinyApp/www/logo.png`
- `ShinyApp/shinyApp/www/interactiveTable.png`
- `ShinyApp/shinyApp/www/ra_preview_publication_icon.png`
- `ShinyApp/shinyApp/www/ccc_heatmap_preview.png`
- `ShinyApp/shinyApp/www/metadata.csv`
- `ShinyApp/shinyApp/www/metadata.xlsx`
- `ShinyApp/shinyApp/www/mouseGeneMapping.txt`
- `ShinyApp/shinyApp/Data/CellChat_all_stage_communication_score_LR_reverse.csv`
- Runtime dataset assets generated outside git now live under `ShinyApp/shinyApp/Data/`, including `sc3*` to `sc7*` RDS/HDF5 files and interactive RDS assets.
- Raw generation inputs may also live under `ShinyApp/shinyApp/Data/`, including `M_Seurat_object.rds`, `final_staged_object*.rds`, `specificCellID*.rds`, and raw subset objects; `.rscignore` excludes those raw-only files from shinyapps.io deployment.

## Build And Utility
- `ShinyApp/global.R`
- `ShinyApp/shinyApp/scripts/build/build_shinycell_assets.R`
- `ShinyApp/shinyApp/scripts/build/build_interactive_assets.R`
- `ShinyApp/shinyApp/scripts/build/generate_home_previews.R`
- `ShinyApp/shinyApp/scripts/maintenance/patch_subset_specificCellID1_assets.R`
- `ShinyApp/shinyApp/scripts/benchmarks/bench_memory_usage.R`
- `ShinyApp/shinyApp/scripts/benchmarks/bench_network_har.R`
- `ShinyApp/shinyApp/scripts/support/data_loaders.R`

## Reference
- `Documentation/AI_HANDOFF_SUMMARY.md`
- Networking baseline protocol and current findings are consolidated in the
  `Networking` section of `Documentation/AI_HANDOFF_SUMMARY.md`.
- `ShinyApp/todolist.txt`
- `ShinyApp/shinyApp/scripts/references/ra_signaling_figure5_reference.R`

## Archive
- `ShinyApp/shinyApp/www/archive/reference/`
  - Former `www/` assets, loose generated PDFs, and reference-only preview
    images that are not referenced by the current UI/server path.
  - This includes publication PDFs, unused preview images, unused icon/image
    assets, and the removed `gene_data.json`.
- `ShinyApp/shinyApp/www/archive/network_profiles/`
  - Browser performance captures used for reference or HAR-based network analysis.
  - These artifacts are not runtime assets and should remain deployment-ignored.

## Notes
- Root `README.md` is the single project README and includes the project guide plus links into `Documentation/`.
- CSS runtime styling is split into focused files and imported through `www/css/style.css`.
- `stylelint.config.cjs` records the Stylelint standard rules and the project-specific exceptions required for Shiny, DataTables, and SVG-generated selectors.
- `metadata.csv` is the active metadata override file. `metadata.xlsx` remains only as fallback when the CSV is absent.
- `sc3` to `sc7` now share the generic staged/subset binder path for detailed tabs. `sc3` main figures remain on the already-shared main-figure binder, while the spermatogenesis interactive SVG/modal workflow is still separate.
- `scripts/build/generate_home_previews.R` writes runtime previews into `www/`; reference-only preview outputs go to `www/archive/reference/`.
- `www/archive/` is reference-only and deployment-ignored. Runtime `.rds`,
  `.h5`, and CSV data live under `ShinyApp/shinyApp/Data/` when the app loads
  them directly; keeping them out of `www/` avoids exposing internal data as
  static files.
- Firefox performance profile exports are directional-only reference artifacts. HAR or Firefox Network Monitor export is the authoritative network baseline input.
