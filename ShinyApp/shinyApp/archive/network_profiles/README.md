# Network Profile Archive

This directory is for browser performance/reference captures that are useful for
analysis but are not part of the runtime app surface.

- Firefox performance profile JSON and extracted profile folders stored here are
  reference-only.
- HAR or Firefox Network Monitor exports stored here are the authoritative input
  for future network baseline comparisons.
- Nothing in this directory should be loaded by the Shiny app or included in a
  deployment bundle.
