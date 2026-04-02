# Network Performance Report

Date: `2026-03-24`

Scope:
- Published route tested: `https://ward-bio.shinyapps.io/SpermInteractive/#home`
- Baseline captures analyzed:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_2.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_3.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_1.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_3.har`
- Post-refactor captures analyzed:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_cold_1_2.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_cold_2_2.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_cold_3_2.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_1_2.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_3_2.har`
- HAR files were summarized with `ShinyApp/shinyApp/bench_network_har.R`.

## Implementation Status

Code changes for the first `#home` startup pass were applied on `2026-03-23`.
The first post-refactor HAR rebaseline was completed on `2026-03-24`.

- `ui.R` now lazy-renders `sc3` to `sc7` tab bodies through `uiOutput(...)` placeholders instead of embedding their full tab markup in the initial home document.
- `ui.R` no longer eagerly loads staged/subset `conf` and `def` objects at app startup.
- `server.R` now initializes `bind_main_figures()` and `bind_shinycell_dataset()` one dataset at a time, on first entry into each `sc3` to `sc7` tab family.
- Staged/subset outputs now opt into `suspendWhenHidden = TRUE` as a defensive cleanup after the lazy-load refactor.
- The home card now uses `interactiveTable_preview.png`; `interactiveTable.png` remains the full-size asset used by the live spermatogenesis SVG/table page.
- `ra_preview_publication_icon.png` and `ccc_heatmap_preview.png` were regenerated as runtime-sized previews at `280px` height.
- Client heatmap debug is now query-param gated instead of shipping `HEAT_DEBUG = true` by default.

Important:
- The "Current Rebaseline" section below is now the authoritative status for `#home`.
- The later baseline sections are intentionally preserved as pre-refactor evidence for comparison.

## Executive Summary

The first `#home` networking pass worked. Relative to the pre-refactor baseline, the published home route now:

1. Delivers much smaller HTML and init payloads.
2. No longer bootstraps staged/subset `dataobj/sc*` traffic on initial `#home`.
3. Ships much smaller cold-load preview imagery.
4. Still wastes warm-load bytes on duplicated static asset requests under mixed worker-prefixed URLs.

## Current Rebaseline

Derived from the six post-refactor HARs via `bench_network_har.R`, with the pre-refactor median kept for direct comparison:

- Cold median:
  - pre: `101` requests, `1.9 MB` transferred, `2609 ms` document TTFB
  - post: `89` requests, `1.2 MB` transferred, `368 ms` document TTFB
- Warm median:
  - pre: `80` requests, `593.7 KB` transferred, `2579 ms` document TTFB
  - post: `62` requests, `757.5 KB` transferred, `346 ms` document TTFB

Interpretation:
- The primary `#home` startup bottleneck improved substantially.
- Cold transferred bytes dropped by about `37%`, which beats the `25%` target for the first pass.
- Warm transferred bytes regressed because the post-refactor deployment is loading some static assets twice under two URL forms.

Acceptance check:
- Met: cold transferred bytes dropped from `1.9 MB` to `1.2 MB`.
- Met: the first major SockJS init request dropped from `36225` bytes in `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:6165` and `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:6196` to `4780` bytes in `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:7001` and `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:7032`.
- Met: delivered HTML dropped from `896696` bytes uncompressed and `53559` bytes compressed in `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:135` and `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:140` to `74875` bytes uncompressed and `13285` bytes compressed in `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:135` and `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:140`.
- Met: no `session/.../dataobj/sc[3-7]` requests appear in any of the six post-refactor `#home` HARs.
- Partially met: the init payload no longer enumerates the old `sc3` to `sc7` output tree, but it still includes hidden flags for the lightweight lazy placeholders at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:7083`.

## Current Findings

### 1. `#home` is now a meaningfully lighter entry route

Evidence:
- The main document content size fell from `896696` to `74875` bytes at:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:135`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:135`
- The compressed body fell from `53559` to `13285` bytes at:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:140`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:140`
- The first major SockJS init request fell from `36225` to `4780` bytes at:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:6165`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:7001`

Implication:
- The lazy-tab refactor removed most of the startup work that used to be attached to initial `#home`.
- The first pass achieved the main goal of making `#home` materially lighter before the user clicks into staged/subset content.

### 2. The preview-asset pass fixed the cold image payload

Evidence:
- The post-refactor cold HAR now requests the dedicated home previews at:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_cold_2_2.har:10869`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_cold_2_2.har:11012`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_cold_2_2.har:11155`
- Cold image bytes fell from `1.1 MB` to `252.1 KB` in the benchmark medians.

Implication:
- The home route is no longer dominated by oversized preview PNG transfer on a cold load.

### 3. The staged/subset eager bootstrap regression on `#home` was removed

Evidence:
- The post-refactor init payload still starts with `mainTabs":"home"` but now only reports lightweight `lazy_sc3` to `lazy_sc7` hidden placeholders instead of the old full staged/subset output tree at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:7083`.
- No `dataobj/sc[3-7]` requests appear in any post-refactor `#home` HAR.

Implication:
- The expensive staged/subset dataset bootstrap has moved off of initial `#home`, which was the core intended behavior change.

### 4. The remaining networking issue is duplicated static asset loading on shinyapps worker URLs

Evidence:
- The post-refactor document still contains a worker-relative `<base href>` and a script that removes it at runtime in `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:136`.
- The same warm HAR then requests both doubled-worker and normal worker-relative copies of key assets:
  - `jquery.min.js` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:163` and `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:2873`
  - `shiny.min.js` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:291` and `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:3001`
  - `bootstrap.min.css` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:419` and `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:3129`
  - `logo.png` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:2744` and `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_post_home_refactor/home_warm_2_2.har:3424`

Implication:
- This is the main reason warm transferred bytes regressed even though the document got smaller and the request count fell.
- The pattern looks like a worker-path URL resolution mismatch rather than a return of the staged/subset bootstrap problem.

## Pre-Refactor Baseline Summary

Derived from the original six HARs via `bench_network_har.R`:

- Cold median:
  - `101` requests
  - `1.9 MB` transferred
  - `2609 ms` document TTFB
- Warm median:
  - `80` requests
  - `593.7 KB` transferred
  - `2579 ms` document TTFB

Interpretation:
- Caching reduced bytes significantly even before the refactor.
- Caching did not materially reduce the main document wait.
- That is why the first pass focused on HTML/bootstrap weight instead of only static asset caching.

## Pre-Refactor Baseline Findings

### 1. The initial document response is the main startup bottleneck

Evidence:
- Cold run `home_cold_1.har` records a document wait of `2492 ms` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:157`.
- Warm run `home_warm_2.har` records a document wait of `2659 ms` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:149`.
- The document remains gzip-encoded HTML in both cases:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:112`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:104`
- Representative page timing is only about `3.5s` to `onLoad`:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:16-17`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:16-17`

Implication:
- The network is mostly done fairly early.
- The persistent startup cost is likely a combination of server-side document generation and browser/Shiny initialization work after the HTML arrives.

### 2. Cold-load bytes are dominated by two oversized home preview images

Evidence:
- `interactiveTable.png` is requested at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:7757`.
- Its recorded image content size is `674100` bytes at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:7871`.
- Its transferred response body is `658604` bytes at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:7877`.
- Its request wait is `76 ms` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:7886`.

- `ra_preview_publication_icon.png` is requested at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:7900`.
- Its transferred response body is `365382` bytes at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:8020`.
- Its request wait is `82 ms` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:8029`.

- `ccc_heatmap_preview.png` is much smaller by comparison, with `40347` transferred bytes at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:8163`.

Implication:
- The two largest previews should be optimized first.
- This is the cleanest cold-start bandwidth win and should materially reduce first-load transfer size.

### 3. The home document itself is very large

Evidence:
- The HTML document content size is `896696` bytes at:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:143`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:135`
- The compressed body size is only `53559` bytes in `home_cold_1.har` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:148`.

Interpretation:
- Network transfer of the HTML is not the only cost.
- The browser is also being handed a large HTML document to parse and execute against.

### 4. The home route appears to bootstrap the full staged/subset app

Evidence:
- The first major SockJS init POST starts at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:6163`.
- It declares `Content-Length: 36225` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:6196`.
- Its payload at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:6247` includes:
  - `mainTabs":"home"`
  - large hidden-output state for `sc3`
  - large hidden-output state for `sc4`
  - large hidden-output state for `sc5`
  - large hidden-output state for `sc6`
  - large hidden-output state for `sc7`

Implication:
- Even on the home page, the browser appears to serialize state for much more than the visible route.
- This is the strongest sign that `#home` is not a light entry point.
- Reducing what gets initialized on home should be a higher-priority optimization than CSS cleanup.

### 5. Production HTML still contains debug logic

Evidence:
- The delivered document still contains `HEAT_DEBUG = true` in:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:144`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:136`

Implication:
- Debug-only client logic should be stripped or gated in production builds.
- This is unlikely to be the top bottleneck by itself, but it is unnecessary startup work.

### 6. CSS is caching correctly and is not the first place to optimize

Evidence:
- `bootstrap.min.css` is requested at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:3230`.
- It returns `304 Not Modified` at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:3296`.

Implication:
- CSS split/import structure may still be worth revisiting later.
- It is not the main startup problem shown by these HARs.

### 7. Font loading is a secondary cleanup target

Evidence:
- Google font CSS contains multiple `@font-face` declarations for weights and ranges at `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_cold_1.har:4440`.
- Warm runs still show repeated requests to the same `Quicksand` latin `woff2` at:
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:4538`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:4702`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:4866`
  - `ShinyApp/shinyApp/archive/network_profiles/2026-03-23_pre_home_refactor/home_warm_2.har:5030`

Implication:
- Font loading can be simplified later.
- It is not the first change to make if the goal is the biggest user-visible improvement.

## Recommended Fix Order

1. Make the home page a genuinely light route.
   - Implemented in code on `2026-03-23` via lazy staged/subset placeholders and one-time dataset initialization.
   - Validated by the `2026-03-24` rebaseline.

2. Optimize the two large home preview PNGs.
   - Implemented in code on `2026-03-23`.
   - `interactiveTable.png` remains the live interactive asset, so the home route now uses a separate `interactiveTable_preview.png`.
   - `ra_preview_publication_icon.png` and `ccc_heatmap_preview.png` keep their runtime filenames but were regenerated for the actual card height.
   - Validated by the `2026-03-24` rebaseline.

3. Reduce the size and complexity of the delivered home HTML.
   - Implemented in code on `2026-03-23` by removing eager staged/subset markup and gating heatmap debug behind an explicit query param.
   - Validated by the `2026-03-24` rebaseline.

4. Remove duplicated static asset loading on shinyapps worker-prefixed routes.
   - Investigate why the published page is loading both doubled-worker and normal worker-relative copies of the same JS, CSS, and image assets.
   - Normalize whatever asset URL generation the app controls first, then verify whether the remaining duplication is a hosting-layer behavior.
   - Re-run the `#home` warm HARs after that pass.

5. Only after that, revisit secondary asset cleanup.
   - font loading
   - CSS import/bundling strategy
   - lower-priority JS cleanup

## Practical Takeaway

The first networking pass succeeded: `#home` is now much lighter, the eager staged/subset bootstrap is gone from initial load, and the oversized previews are no longer dominating cold-start transfer. The remaining networking task is a smaller follow-up pass to eliminate duplicated static asset requests on published worker URLs before moving on to broader CSS or font work.
