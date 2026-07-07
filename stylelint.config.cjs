// ---------------------------------------------------------------------------
// Stylelint configuration
// ---------------------------------------------------------------------------
// The app CSS uses Bootstrap, Shiny, DataTables, and generated SVG selectors,
// so this config keeps the standard baseline while disabling rules that are too
// noisy for those framework-owned selector contracts.
module.exports = {
  extends: ["stylelint-config-standard"],
  rules: {
    // Shiny, DataTables, Bootstrap, and generated SVG selectors are not under
    // this app's naming control, so selector naming is checked by review.
    "selector-class-pattern": null,
    "selector-id-pattern": null,

    // Theme override files intentionally group light and dark selectors by
    // feature, which makes Stylelint's global specificity ordering too noisy.
    "no-descending-specificity": null,

    // SVG presentation properties such as rx/ry are valid for the generated
    // interactive table overlays, but the standard value rule flags them.
    "declaration-property-value-no-unknown": null,

    // Legacy placeholder selectors are kept for browser compatibility.
    "selector-pseudo-class-no-unknown": [
      true,
      {
        ignorePseudoClasses: ["input-placeholder"]
      }
    ],
    "selector-pseudo-element-no-unknown": [
      true,
      {
        ignorePseudoElements: ["input-placeholder"]
      }
    ]
  }
};
