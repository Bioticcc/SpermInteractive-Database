# AI Handoff Summary

## Purpose
This file is the current-state handoff for a new AI agent entering the project.
It should be read before new feature work or broad refactors.

It summarizes:
- the active runtime structure
- what changed after the March 2026 cleanup/performance pass
- the current operational and deployment surfaces
- the main remaining risks

Current snapshot date: `2026-07-05`

## Environment Snapshot
- Repo root: `/home/jeezu/ShinyApp Project`
- Active app directory: `ShinyApp/shinyApp`
- Local launch/deploy helper: `ShinyApp/global.R`
- Primary runtime entrypoints:
  - `ShinyApp/shinyApp/ui.R`
  - `ShinyApp/shinyApp/server.R`
  - `ShinyApp/shinyApp/dataset_tab_builders.R`
  - `ShinyApp/shinyApp/app_support.R`
  - `ShinyApp/shinyApp/metadata_overrides.R`
- Environment pinning:
  - `renv.lock` pins R `4.3.3` and the app package set
  - `Dockerfile` builds from `rocker/shiny:4.3.3` and restores with `renv`
- Style tooling:
  - R source is formatted with `styler` and checked with targeted `lintr` rules for explicit returns, left assignment, and no `attach()` usage
  - CSS source is checked with `stylelint.config.cjs` and `stylelint-config-standard`
- Deployment/bundle filtering:
  - `ShinyApp/shinyApp/.rscignore` excludes raw Seurat objects, `archive/`, `www/archive/`, local rsconnect metadata, and stray network/profile artifacts
  - `.dockerignore` excludes large local artifacts from container builds

## Documentation Layout
- Root `README.md`
  - single project README and longer-form project guide
- `Documentation/AI_HANDOFF_SUMMARY.md`
  - this handoff
- `Documentation/REPO_FILE_CLASSIFICATION.md`
  - runtime vs build vs reference vs archive notes

## What Changed Since The 2026-03-23 Snapshot

### 1. Dataset tabs are now lazy end-to-end
- `ui.R` no longer eagerly loads `sc3` to `sc7` config/default objects or their tab bodies.
- Each staged/subset page is now a lazy `uiOutput(...)` placeholder in the navbar.
- `server.R` initializes `bind_main_figures()` and `bind_shinycell_dataset()` only when a user first enters that dataset family.
- After first render, outputs return to `suspendWhenHidden = TRUE`.

### 2. The `#home` refactor was rebaselined
- The authoritative networking notes now live in the `Networking` section of this file.
- Post-refactor medians from the `2026-03-24` HAR rebaseline:
  - cold: `89` requests, `1.2 MB`, `368 ms` document TTFB
  - warm: `62` requests, `757.5 KB`, `346 ms` document TTFB
- Initial `#home` no longer bootstraps staged/subset `dataobj/sc*` requests.
- The main remaining network issue is duplicated static asset loading under mixed worker-prefixed URLs on shinyapps.io.

### 3. Release history is now part of the live app surface
- `ShinyApp/shinyApp/release_notes.csv` is the active release-history source.
- `ui.R` builds the patch-notes page and header version link from that CSV.
- `ShinyApp/global.R` validates release metadata and writes the current release row back to `release_notes.csv`.

### 4. Theme + shareable-link infrastructure was added
- The app now supports a light/dark toggle persisted in `localStorage`.
- URL bookmarking is enabled with `enableBookmarking("url")`.
- The server generates shareable URLs with `session$doBookmark()`.
- Client-side code keeps navbar hash/history state synchronized and copies links through `www/presets.js`.

### 5. Dataset UX expanded
- Each of `sc3` to `sc7` now has a `Main Figures` page.
- Shared dataset controls now include a stage-split toggle for embedding-based tabs.
- Download flows for staged/subset figures now open modal-based export dialogs with optional custom filenames.
- Primary navigation now exposes `Full Atlas` as a single top-level page and `Cell Subsets` as a dropdown of subset names only.
- Detailed `sc3` to `sc7` pages are still registered as tabs for lazy routing and URL hashes, but secondary page navigation happens through embedded mini nav bars.

### 5a. Primary navbar and embedded mini navigation
- `Full Atlas` opens `sc3_main_figures`; the remaining `sc3_*` pages are hidden from the primary navbar and reached through the embedded dataset mini nav.
- `Cell Subsets` opens each subset's Main Figures page (`sc4_main_figures` to `sc7_main_figures`); the remaining subset pages are hidden from the dropdown and reached through the same embedded dataset mini nav.
- `Interactive Data` opens `spermatogonia_table`; `retinoic_acid` and `cell2cell_heatmaps` remain registered tabs but are hidden from the primary navbar and reached through the embedded interactive-data mini nav.
- The header patch-notes link opens the hidden `patch_notes` tab; both its anchor and parent navbar item are hidden to avoid creating a blank gap between top-level navbar entries.
- Do not remove hidden registered tab panels just because they are not visible in the primary navbar: they are required for `window.navToTab(...)`, hash/history routing, and Shiny lazy initialization.

### 6. The spermatogenesis interactive table is richer than before
- The interactive table still uses `interactiveTable.png` as the art asset, but the clickable surface is now an SVG with generated rectangle overlays in `server.R`.
- Search now colors matching cells directly on the SVG and can overlay expression-ranked labels.
- The search threshold is now a user-editable numeric control in the sidebar, defaulting to `0.25`.
- The control panel now reports the current searched gene's minimum and maximum average expression across table cells.
- Modal gene tables now add Ensembl IDs by looking up `www/mouseGeneMapping.txt`.
- Modal results can be searched client-side and exported as CSV, and the modal now uses the same live threshold as the SVG overlay.

### 7. Operational files are more explicit
- `Dockerfile` and `renv.lock` are now meaningful project entrypoints for reproducible runtime setup.
- `ShinyApp/shinyApp/.rscignore` is important operational documentation for what is and is not deployed.

## Current High-Level Architecture

### 1. Lazy ShinyCell dataset plane for `sc3` to `sc7`
Per dataset prefix, runtime uses files under `ShinyApp/shinyApp/Data/`:
- `scNconf.rds`
- `scNdef.rds`
- `scNgene.rds`
- `scNmeta.rds`
- `scNgexpr.h5`

These drive:
- Full Atlas main figures
- subset main figures
- staged/subset detailed tabs
- stage-split views inside the generic dataset binder

### 2. Precomputed interactive-data plane
These include files under `ShinyApp/shinyApp/Data/`:
- `interactive_genes.rds`
- `spg_avg_expr_by_button.rds`
- `ra_dot_avg_expr.rds`
- `ra_dot_pct_expr.rds`
- `ra_line_mean_expr.rds`
- `CellChat_all_stage_communication_score_LR_reverse.csv`

These drive:
- spermatogenesis table modal/search data
- RA dotplot
- RA lineplot
- Cell-cell communication heatmap

### 3. Runtime sidecars
In `www/`:
- `metadata.csv`
- `metadata.xlsx`
- `mouseGeneMapping.txt`
- `interactiveTable.png`
- `interactiveTable_preview.png`
- `ra_preview_publication_icon.png`
- `ccc_heatmap_preview.png`
- CSS files
- JS helpers

At app root:
- `release_notes.csv`

### 4. Client-side augmentation layer
`ui.R` and `www/*.js` now also own:
- theme persistence
- share-link clipboard copy
- navbar dropdown auto-close and hash/history syncing
- SVG highlighting and heat overlay behavior for the interactive table
- hidden-primary-tab CSS for secondary pages and patch notes
- embedded mini navigation for interactive-data pages

### 5. Separate interactive-data logic still exists
The following remain custom runtime paths outside the generic ShinyCell dataset binder:
- spermatogenesis SVG/modal/table workflow
- RA dotplot
- RA lineplot
- Cell-cell communication heatmap
- patch notes / release-note display

## Active File Map

### Runtime-critical source files
- `ShinyApp/shinyApp/ui.R`
  - builds the header, home page, patch notes page, interactive-data tabs, embedded interactive-data navigation, and lazy placeholders for `sc3` to `sc7`
  - owns theme-toggle, tab-history, SVG heat-overlay, and navbar JS bootstrapping
- `ShinyApp/shinyApp/server.R`
  - main runtime logic
  - owns plotting helpers, lazy active bindings, first-open dataset initialization, interactive-data logic, bookmarking/share-link handling, and lag profiling
- `ShinyApp/shinyApp/dataset_tab_builders.R`
  - shared UI builders for main figures, detailed dataset tabs, and the embedded dataset mini navigation
- `ShinyApp/shinyApp/app_support.R`
  - shared runtime support layer for path discovery, loaders, spinners, and gene-index normalization
- `ShinyApp/shinyApp/metadata_overrides.R`
  - applies metadata rename/remove rules from `www/`
- `ShinyApp/shinyApp/button_mapping_general.R`
  - maps spermatogenesis table button IDs to stage/cell-type groups

### Runtime-critical data and config files
- `ShinyApp/shinyApp/release_notes.csv`
  - live patch-note source
- `ShinyApp/shinyApp/www/metadata.csv`
  - primary metadata override file
- `ShinyApp/shinyApp/www/metadata.xlsx`
  - fallback metadata override file when CSV is absent
- `ShinyApp/shinyApp/www/mouseGeneMapping.txt`
  - Ensembl lookup table used by spermatogenesis modal results
- `ShinyApp/shinyApp/Data/CellChat_all_stage_communication_score_LR_reverse.csv`
  - CellChat heatmap source

### Build and utility scripts
- `ShinyApp/global.R`
  - local bootstrap helper
  - release-note sync helper
  - currently also contains an active shinyapps deployment call
- `ShinyApp/shinyApp/scripts/build/build_shinycell_assets.R`
  - builds ShinyCell assets for `sc3` to `sc7`
- `ShinyApp/shinyApp/scripts/build/build_interactive_assets.R`
  - builds RA and spermatogenesis interactive assets from Seurat objects
- `ShinyApp/shinyApp/scripts/build/generate_home_previews.R`
  - generates runtime home previews in `www/` and archival reference previews in `www/archive/reference/`
- `ShinyApp/shinyApp/scripts/maintenance/patch_subset_specificCellID1_assets.R`
  - patches subset ShinyCell metadata/config using staged-testis master metadata
- `ShinyApp/shinyApp/scripts/benchmarks/bench_memory_usage.R`
  - measures app asset memory footprint
- `ShinyApp/shinyApp/scripts/benchmarks/bench_network_har.R`
  - summarizes HAR exports for network baseline work
- `ShinyApp/shinyApp/scripts/support/data_loaders.R`
  - helper script for loading assets outside the runtime server path
- `Dockerfile`
  - container entrypoint for running the deployed app surface from `ShinyApp/shinyApp`
- `renv.lock`
  - package lockfile for reproducible installs
- `ShinyApp/shinyApp/.rscignore`
  - deployment bundle exclusions

### Reference and archive material
- `Documentation/`
  - current docs
- `ShinyApp/shinyApp/www/archive/reference/`
  - former `www/` assets, generated PDFs, and reference images no longer used at runtime
- `ShinyApp/shinyApp/www/archive/network_profiles/`
  - HAR/profile captures and archive README
- `ShinyApp/shinyApp/www/archive/`
  - reference-only static archive excluded from deployment through `.rscignore`
- `ShinyApp/shinyApp/scripts/references/ra_signaling_figure5_reference.R`
  - reference publication script, not live runtime logic

## Startup And Runtime Dataflow

### UI startup
Source path:
- `ShinyApp/shinyApp/ui.R`

Current flow:
1. Source `app_support.R`.
2. Enable URL bookmarking.
3. Load release notes from `release_notes.csv`.
4. Define lazy dataset-tab helpers and patch-notes builders.
5. Inject CSS, theme/nav/SVG JS, and clipboard helpers.
6. Build the header, home page, hidden patch-notes tab, interactive-data tabs, and lazy `uiOutput(...)` placeholders for `sc3` to `sc7`.
7. Register secondary pages as tabs even when hidden from the primary navbar, so embedded mini nav links and URL hashes can still select them.

Important consequence:
- The UI no longer eagerly reads `sc3` to `sc7` config/default assets.
- The home route is materially lighter than the March 23 pre-refactor state.

### Server startup
Source path:
- `ShinyApp/shinyApp/server.R`

Current flow:
1. Source shared helpers, metadata overrides, and tab builders.
2. Create lazy active bindings for `sc3` to `sc7` config/default/gene/meta assets.
3. Create lazy RDS loaders for interactive assets.
4. Define plotting helpers and lag-profiling wrappers.
5. Set up theme state, bookmarking, share-link generation, and interactive-data readiness gates.
6. Register lazy dataset placeholder outputs for every `sc3` to `sc7` subpage.
7. Initialize a dataset family only when the user first enters one of its tabs.
8. Register interactive-data logic for the spermatogenesis table, RA plots, and CellChat heatmap.

Important consequence:
- Dataset binders are first-open work now, not startup work.
- The architectural split is now:
  - lazy generic dataset families
  - separate interactive-data pages
  - release/share/theme client/runtime layer

## Runtime Feature Areas

### 1. Main Figure pages for `sc3` to `sc7`
Source path:
- UI from `dataset_tab_builders.R`
- server binding from `bind_main_figures()` in `server.R`

Behavior:
- uses harmonized cell-type metadata overlays
- renders a UMAP overview plus stage-split UMAP panels
- avoids gene HDF5 reads
- exports through modal-based download flows

### 2. Detailed staged/subset tabs for `sc3` to `sc7`
Source path:
- UI from `dataset_tab_builders.R`
- server binding from `bind_shinycell_dataset()` in `server.R`

Behavior:
- updates gene selectize inputs lazily from `scNgene`
- updates violin Y-axis choices from either numeric metadata or genes
- builds subset checkboxes from the config object
- renders with:
  - `scDRcell()`
  - `scDRgene()`
  - `scDRcoex()`
  - `scVioBox()`
  - `scProp()`
  - `scBubbHeat()`

Important consequence:
- this remains the main HDF5-backed hotspot area
- stage-split mode is now available inside the generic embedding tabs

### 3. Spermatogenesis interactive page
Source path:
- `server.R`
- `button_mapping_general.R`
- `www/mouseGeneMapping.txt`
- `Data/spg_avg_expr_by_button.rds`

Behavior:
- renders a generated SVG click surface over `interactiveTable.png`
- opens a modal on button click
- filters genes above a sidebar-controlled threshold, default `0.25`
- shows the searched gene's min/max average expression range in the control panel
- enriches modal tables with Ensembl IDs
- supports CSV export and client-side search
- supports search-driven SVG heat coloring and ranking overlays

### 4. RA dotplot
Source path:
- `Data/ra_dot_avg_expr.rds`
- `Data/ra_dot_pct_expr.rds`
- `make_fig5A()` in `server.R`

Behavior:
- precomputed matrix path
- event-driven invalidation with debounce
- mostly plotting/reshape work, not HDF5 read work

### 5. RA lineplot
Source path:
- `Data/ra_line_mean_expr.rds`
- `make_fig5C()` in `server.R`

Behavior:
- precomputed array path
- event-driven invalidation with debounce
- publication-style patchwork layout for export
- more CPU/reshape-bound than I/O-bound

### 6. Cell-cell communication heatmap
Source path:
- `Data/CellChat_all_stage_communication_score_LR_reverse.csv`
- interactive plotly path plus `make_fig6D_static()` for PDF export

Behavior:
- separate from the ShinyCell binders
- uses interactive plotly for on-screen rendering
- uses static ggplot for publication PDF download

### 7. Release/share/theme infrastructure
Source path:
- `ui.R`
- `server.R`
- `www/presets.js`
- `release_notes.csv`

Behavior:
- hidden `patch_notes` tab is linked from the header
- patch-notes navbar anchor and parent item are hidden to avoid a blank navbar gap while preserving the route
- theme is stored in browser `localStorage`
- share links are generated from bookmark state and copied to clipboard

## Networking

This section is the canonical networking handoff. The former standalone network
baseline and performance markdown files were folded here so future agents do
not need to reconcile multiple network-status documents.

### Scope
- Published route tested: `https://ward-bio.shinyapps.io/SpermInteractive/#home`
- Baseline HARs live under `ShinyApp/shinyApp/www/archive/network_profiles/`.
- HAR summaries are generated with `ShinyApp/shinyApp/scripts/benchmarks/bench_network_har.R`.
- Firefox performance profile JSON is directional only. Use Firefox Network Monitor export or HAR for authoritative request counts, transferred bytes, cache behavior, TTFB, and waterfall order.

### Capture Protocol
- Capture cold loads with cache disabled and warm reloads with cache enabled.
- Run each scenario three times and compare medians.
- Do not click other tabs during capture.
- Stop capture only after the page is visually idle and network activity is quiet.
- Name files with `cold` or `warm`, for example `home_cold_1.har`, so the benchmark script can group repeated runs automatically.

Run from the repo root:

```bash
Rscript ShinyApp/shinyApp/scripts/benchmarks/bench_network_har.R \
  path/to/home_cold_1.har \
  path/to/home_cold_2.har \
  path/to/home_cold_3.har \
  path/to/home_warm_1.har \
  path/to/home_warm_2.har \
  path/to/home_warm_3.har
```

### Current `#home` Rebaseline
- Date: `2026-03-24`
- Cold median improved from `101` requests, `1.9 MB`, and `2609 ms` document TTFB to `89` requests, `1.2 MB`, and `368 ms` document TTFB.
- Warm median changed from `80` requests, `593.7 KB`, and `2579 ms` document TTFB to `62` requests, `757.5 KB`, and `346 ms` document TTFB.
- Cold transferred bytes dropped by about `37%`, beating the first-pass `25%` target.
- Initial `#home` no longer makes `session/.../dataobj/sc[3-7]` requests.
- The first major SockJS init payload dropped from roughly `36225` bytes to roughly `4780` bytes.
- Delivered HTML dropped from roughly `896696` bytes uncompressed and `53559` bytes compressed to roughly `74875` bytes uncompressed and `13285` bytes compressed.

### Current Findings
- The lazy-tab refactor made `#home` a much lighter entry route by moving staged/subset dataset initialization off the initial home load.
- Runtime preview images fixed the cold image payload. Home should request `interactiveTable_preview.png`, `ra_preview_publication_icon.png`, `ccc_heatmap_preview.png`, and `logo.png`; the full-size `interactiveTable.png` should appear only when the live spermatogenesis SVG/table page is opened.
- Current runtime preview sizes from the `2026-03-24` HAR rebaseline were:
  - `interactiveTable_preview.png`: `518x280`, about `83 KB`
  - `ra_preview_publication_icon.png`: `482x280`, about `51 KB`
  - `ccc_heatmap_preview.png`: `445x280`, about `27 KB`
- Production heatmap debug is query-param gated instead of shipping debug mode by default.
- CSS caching was not the main startup bottleneck in the HARs. Revisit `css/style.css` import chaining only if future HARs show CSS blocking time matters.

### Remaining Networking Issue
- Warm loads still waste bytes because shinyapps.io sometimes loads duplicated static assets under mixed worker-prefixed URL forms.
- The duplicated assets observed in the `2026-03-24` rebaseline included core JS/CSS and `logo.png`.
- This looks like a worker-path URL resolution mismatch rather than a return of staged/subset eager bootstrap.
- Next networking pass: normalize app-controlled asset URL generation first, then re-run warm `#home` HARs to determine whether remaining duplication is hosting-layer behavior.

### Networking Acceptance Targets
- Keep staged/subset `dataobj/sc*` requests absent from initial `#home`.
- Keep the home route on preview-sized image assets, not full-size interactive backgrounds.
- Keep the initial SockJS/init payload from enumerating the full hidden `sc3` to `sc7` output tree.
- Reduce median current-site home interactive time materially; the initial target was under 10 seconds, then reassess.

## Current Performance Model

### HDF5-backed tab interaction lag
Still the leading cause of dataset-tab slowness.

Most likely hotspots:
- `scDRgene()`
- `scDRcoex()`
- `scVioBox()` when plotting genes
- `scBubbHeat()`

Why:
- they read gene vectors from `scNgexpr.h5`
- they often reshape, aggregate, scale, or cluster before plotting

Current measurement method:
- enable profiling with either:
  - `options(sperminteractive.profile_lag = TRUE)`
  - `SPERMINTERACTIVE_PROFILE_LAG=1`
- inspect `[lag_profile]` JSON lines in the R console/log

### Memory/deployment posture
- Interactive figures use precomputed assets specifically to avoid loading full Seurat objects on shinyapps.io.
- Runtime still depends on generated `Data/sc3*` to `Data/sc7*` and interactive `.rds`/`.h5` assets that are not committed to git.
- `options(shiny.maxRequestSize = 5 * 1024^3)` is set, but the upload-dataset feature is currently commented out and not part of the live UI path.

## Known Issues, Risks, And Open Questions

### 1. `global.R` still mixes local run, release-note mutation, and deployment
Status:
- confirmed

Why it matters:
- sourcing `global.R` writes to `release_notes.csv`
- it also retains an active `rsconnect::deployApp(...)` path
- this is easy to trigger accidentally when treating it as a simple local helper

### 2. Network guidance is consolidated in this handoff
Status:
- confirmed

Why it matters:
- the former standalone network baseline and performance markdown files were
  folded into the `Networking` section above
- future networking updates should revise this handoff and the HAR archives,
  not recreate parallel status files

### 3. Asset provenance and assay policy still need explicit documentation
Status:
- still an active architecture risk

Why it matters:
- `scripts/build/build_interactive_assets.R` and `scripts/build/build_shinycell_assets.R` can read different preferred assays
- figure differences may be pipeline-driven rather than biologically intended

### 4. Runtime depends on generated assets that are not in git
Status:
- confirmed

Why it matters:
- a clean clone is not enough to run the app
- missing `Data/sc3*` to `Data/sc7*` or interactive asset files will break runtime paths even if the source repo looks complete

### 5. Warm-load shinyapps asset duplication remains unresolved
Status:
- confirmed by the `2026-03-24` HAR rebaseline in the `Networking` section

Why it matters:
- this is now the main remaining network inefficiency on `#home`

### 6. The upload-dataset feature is dormant, not live runtime
Status:
- confirmed

Why it matters:
- `server.R` still contains a large commented `usr*` upload/viewer path
- `ui.R` keeps `upload.js` commented out
- future agents should not assume those paths are active

### 7. Style tooling is config-based, not vendored
Status:
- confirmed

Why it matters:
- `stylelint.config.cjs` is tracked, but Node dependencies are not vendored.
- Run Stylelint from a local or temporary install that includes `stylelint` and `stylelint-config-standard`.

### 8. Metadata override precedence is still easy to misunderstand
Status:
- confirmed

Rule:
- `metadata.csv` wins over `metadata.xlsx`

### 9. `mouseGeneMapping.txt` is large reference data
Status:
- intentional

Why it matters:
- it is useful for modal Ensembl lookup, but it is not part of the main plotting path
- future cleanup should treat it as a runtime lookup table, not as a general-purpose editable source file

### 10. Some missing-gene reports may be asset-coverage issues, not threshold issues
Status:
- confirmed for at least two reported genes

Why it matters:
- `Prssly` and `Teyorf1` are absent from the current generated runtime gene assets (`spg_avg_expr_by_button.rds`, `interactive_genes.rds`, and `sc3` to `sc7` gene indices)
- if a user cannot find one of these genes in search at all, that is not caused by the UI threshold
- follow-up investigation belongs in the upstream Seurat objects and the build scripts, not in the sidebar search logic

## Recommended Next Investigation Order

1. Resolve the shinyapps warm-load duplicate static asset requests.
2. Separate `global.R` into explicit local-run vs deploy/release tasks.
3. Create an asset provenance note/table covering source object, assay, build script, and output files.
4. Use `[lag_profile]` output to target the slowest HDF5-backed dataset tabs before optimizing blindly.
5. Clean or remove the dormant upload-dataset path if it is not coming back soon.
6. Audit missing-gene reports against the upstream Seurat objects and build outputs, starting with `Prssly` and `Teyorf1`.

## Practical Search Anchors
Use these names first when re-entering the codebase:

- lazy dataset initialization:
  - `dataset_specs`
  - `ensure_dataset_initialized(`
  - `dataset_tab_output_id(`
- shared dataset binders:
  - `bind_main_figures(`
  - `bind_shinycell_dataset(`
- release/share/theme:
  - `release_notes.csv`
  - `theme_mode`
  - `doBookmark`
  - `copy-to-clipboard`
- navigation:
  - `make_cell_subsets_menu(`
  - `make_interactive_data_nav(`
  - `build_dataset_secondary_nav(`
  - `window.navToTab`
  - `#mainTabs > li:has(`
- performance profiling:
  - `sperminteractive.profile_lag`
  - `SPERMINTERACTIVE_PROFILE_LAG`
  - `[lag_profile]`
- interactive custom figures:
  - `make_fig5A(`
  - `make_fig5C(`
  - `make_fig6D_static(`
  - `show_button_modal(`
- shared runtime support:
  - `app_support.R`
  - `metadata.csv`
  - `button_mapping_general.R`

## Bottom Line
The repo is no longer in the eager-startup state described by the March 23 handoff.

The most important current facts are:
- staged/subset dataset families are lazy-rendered and bound on first visit
- the home route is materially lighter after the March 23 refactor and March 24 rebaseline
- release notes, patch notes, theme state, and shareable links are now part of the live app surface
- Full Atlas, Interactive Data, and Cell Subsets now use primary navbar entries for section entry points and embedded mini nav bars for secondary pages; hidden registered tabs are intentional
- the spermatogenesis table threshold is now user-editable and shared between the SVG overlay and the modal gene table
- the biggest remaining technical risks are:
  - shinyapps warm-load asset duplication
  - mixed asset provenance / assay policy
  - feature omissions in generated gene assets
  - HDF5-backed render cost on detailed tabs
  - `global.R` still conflating local run and deploy/release behavior

If a new agent needs the highest-leverage next task, it should start with the warm-load asset duplication issue or the HDF5-heavy detailed-tab profiler output, not with the already-completed eager-home bootstrap cleanup.
