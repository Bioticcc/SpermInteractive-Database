# Repo File Classification

This note records which files are part of the active app surface after the
foundation cleanup pass, and which files are reference-only.

## Runtime
- `ShinyApp/shinyApp/ui.R`
- `ShinyApp/shinyApp/server.R`
- `ShinyApp/shinyApp/ra_tabs.R`
- `ShinyApp/shinyApp/metadata_overrides.R`
- `ShinyApp/shinyApp/button_mapping_general.R`
- `ShinyApp/shinyApp/app_support.R`
- `ShinyApp/shinyApp/www/style.css`
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
- `ShinyApp/shinyApp/CellChat_all_stage_communication_score_LR_reverse.csv`
- Runtime dataset assets generated outside git, including `sc3*` to `sc7*` RDS/HDF5 files and interactive RDS assets.

## Build And Utility
- `ShinyApp/global.R`
- `ShinyApp/shinyApp/build_shinycell_assets.R`
- `ShinyApp/shinyApp/build_interactive_assets.R`
- `ShinyApp/shinyApp/patch_subset_specificCellID1_assets.R`
- `ShinyApp/shinyApp/generate_home_previews.R`
- `ShinyApp/shinyApp/bench_memory_usage.R`
- `ShinyApp/shinyApp/bench_network_har.R`
- `ShinyApp/shinyApp/data_loaders.R`

## Reference
- `Documentation/AI_HANDOFF_SUMMARY.md`
- `Documentation/NETWORK_BASELINE_RESET.md`
- `Documentation/README.md`
- `NETWORK_PERFORMANCE_REPORT.md`
- `ShinyApp/todolist.txt`
- `ShinyApp/shinyApp/RA_Signaling_Figure5 (1).R`

## Archive
- `ShinyApp/shinyApp/archive/reference_www/`
  - Former `www/` assets that are not referenced by the current UI/server path.
  - This includes publication PDFs, unused preview images, unused icon/image assets, and the removed `gene_data.json`.
- `ShinyApp/shinyApp/archive/network_profiles/`
  - Browser performance captures used for reference or HAR-based network analysis.
  - These artifacts are not runtime assets and should not remain in `www/`.

## Notes
- Root `README.md` is now a lightweight entrypoint that points into `Documentation/`.
- `metadata.csv` is the active metadata override file. `metadata.xlsx` remains only as fallback when the CSV is absent.
- `sc3` to `sc7` now share the generic staged/subset binder path for detailed tabs. `sc3` main figures remain on the already-shared main-figure binder, while the spermatogenesis interactive SVG/modal workflow is still separate.
- `generate_home_previews.R` now writes only the active heatmap preview back into `www/`; reference-only preview outputs go to the archive directory.
- Firefox performance profile exports are directional-only reference artifacts. HAR or Firefox Network Monitor export is the authoritative network baseline input.
