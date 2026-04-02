# Network Performance Baseline Reset

This repo now treats Firefox performance profile exports as directional evidence
only. They are useful for spotting likely hotspots, but they are not the source
of truth for request counts, transferred bytes, cache hits, or waterfall
blocking order.

## Current directional findings
- The later Firefox profile capture includes a `TTFI after 14540ms` marker for
  the published `#home` route.
- As of the `2026-03-23` code changes, the home page should now load these
  preview assets on `#home`:
  - `interactiveTable_preview.png`
  - `ra_preview_publication_icon.png`
  - `ccc_heatmap_preview.png`
  - `logo.png`
- The live spermatogenesis SVG/table page still uses `interactiveTable.png` as
  its full-size background asset, so that file should not appear in a clean
  `#home` HAR unless another route is opened.
- Current generated runtime preview sizes are:
  - `interactiveTable_preview.png`: `518x280`, about `83 KB`
  - `ra_preview_publication_icon.png`: `482x280`, about `51 KB`
  - `ccc_heatmap_preview.png`: `445x280`, about `27 KB`
- The home card previews still render at `140px` tall in
  [interactive-data.css](/home/biotic/ShinyApp%20Project/ShinyApp/shinyApp/www/interactive-data.css).
- The staged/subset tab families (`sc3` to `sc7`) are now intended to lazy-load
  on first visit, so initial `#home` captures should no longer show their
  selectize bootstrap requests or large hidden-output payloads.

## Authoritative baseline method
Use Firefox Network Monitor export or HAR, not Firefox performance profile JSON.

### Capture scenario
- Route: `https://ward-bio.shinyapps.io/SpermInteractive/#home`
- Scenario A: cold load with cache disabled
- Scenario B: warm reload with cache enabled
- Run each scenario 3 times and use the median
- Do not click other tabs during capture
- Stop capture only after the page is visually idle and the network is quiet

### File naming
Use filenames that include `cold` or `warm` so the benchmark script can group
repeated runs automatically:

```text
home_cold_1.har
home_cold_2.har
home_cold_3.har
home_warm_1.har
home_warm_2.har
home_warm_3.har
```

## HAR analysis script
Run from the repo root:

```bash
Rscript ShinyApp/shinyApp/bench_network_har.R \
  path/to/home_cold_1.har \
  path/to/home_cold_2.har \
  path/to/home_cold_3.har \
  path/to/home_warm_1.har \
  path/to/home_warm_2.har \
  path/to/home_warm_3.har
```

The script reports:
- document TTFB
- total request count
- total transferred bytes
- top requests by transferred bytes
- CSS request count and aggregate request/TTFB time
- JS request count and aggregate request/TTFB time
- image request count and aggregate request/TTFB time
- websocket/bootstrap request count and aggregate request/TTFB time
- median metrics for repeated `cold` and `warm` runs

## Optimization queue after HAR confirmation
1. Re-run the `#home` HAR baseline after the lazy-load and preview-image
   changes.
2. Confirm that early `session/.../dataobj/sc*` requests and staged/subset
   hidden-output flags are absent from the initial `#home` bootstrap.
3. If document TTFB still dominates after the HTML/bootstrap shrink, isolate
   server-side document generation cost.
4. Check whether `style.css` import chaining adds meaningful blocking time; only
   rebundle CSS if HAR shows it matters.
5. Defer secondary JS work unless HAR shows `jquery`, `shiny.min.js`,
   `sockjs`, or `shinyhelper` dominating early startup.

## Acceptance targets for the next optimization pass
- Reduce median cold-load total transferred bytes on `#home` by at least 25%.
- Eliminate staged/subset `dataobj/sc*` requests from the initial `#home`
  route.
- Shrink the initial SockJS/init payload so it no longer enumerates hidden
  outputs for `sc3` to `sc7`.
- Reduce median current-site home interactive time materially; initial target is
  under 10 seconds, then reassess.
