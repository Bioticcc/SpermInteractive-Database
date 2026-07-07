# ---------------------------------------------------------------------------
# SpermInteractive Shiny UI
# ---------------------------------------------------------------------------
# This file defines the app shell, page navigation, static UI helpers, inline
# client-side behavior, and the lazily-registered tab structure consumed by
# server.R. Most dataset-specific figure bodies are generated in
# dataset_tab_builders.R and inserted here through uiOutput placeholders.

# ---------------------------------------------------------------------------
# Package imports and shared helpers
# ---------------------------------------------------------------------------
library(shiny)
library(bslib)

source("app_support.R")

# ---------------------------------------------------------------------------
# Theme, bookmarking, and release defaults
# ---------------------------------------------------------------------------
theme_light <- "lightblue"
theme_dark <- "dark"
default_spg_expr_threshold <- 0.25

# Default theme on first load (may be overridden by localStorage).
theme_default <- theme_light

# Shareable links should capture the full interactive state in the URL.
enableBookmarking("url")

# Build the collapsible explanatory note shown below interactive-data figures.
make_interactive_explanation_box <- function(explanation_text) {
  return(tags$div(
    class = "figure-expl-wrap",
    tags$details(
      class = "figure-expl-details",
      tags$summary(
        tags$span(class = "fa fa-circle-info", `aria-hidden` = "true"),
        "Figure Explanation"
      ),
      tags$div(
        class = "home-card glass-card figure-expl-card",
        tags$p(
          class = "ra-sub",
          style = "margin-bottom:0;",
          explanation_text
        )
      )
    )
  ))
}

# Build the common figure-download action row used by interactive-data pages.
make_interactive_download_section <- function(button_id) {
  return(tags$div(
    class = "ra-rowgroup interactive-download-section",
    tags$div(class = "ra-rowtitle", "Figure Downloads"),
    actionButton(
      inputId = button_id,
      label = "Download Figures",
      class = "btn btn-outline-primary ra-download-modal-trigger no-snapshot"
    )
  ))
}

# Long-form figure descriptions used by the interactive-data explanation boxes.
interactive_explanations <- list(
  spermatogenesis_table = "The Interactive Spermatogenesis Table provides a visually intuitive, stage-by-stage reference for cell types present in the mouse testis, based on a modified version of the classic spermatogenesis diagram from Mäkelä et al. (JoVE 2020). Each tile in the table represents a specific cell type at a given seminiferous tubule stage (I-XII), and clicking any tile links directly to the matched cells in the single-nuclei dataset and gives users a list of marker genes for that cell type. Users can optionally query a gene of interest and set an expression threshold to overlay average expression values directly onto the tiles, with the color scale reflecting relative expression levels - tiles below the threshold remain unlabeled to reduce noise. This makes it straightforward to contextualize where a gene of interest is expressed within the developmental hierarchy of spermatogenesis.",
  retinoic_acid_analysis = "The Retinoic Acid Analysis tab offers two complementary views for exploring the expression of retinoic acid pathway genes across the testicular cell atlas. The dot plot visualizes a user-selected set of RA-related genes (including Stra8, Stra6, Rbp1, Rbp4, Rdh10, Cyp26a1, Cyp26b1, Cyp26c1, Aldh1a1, Aldh1a2, and Aldh1a3) across any combination of annotated cell types, with dot size encoding the percentage of expressing cells and color encoding the scaled average expression level. The companion line plot displays expression trajectories for the same genes across spermatogenic stages, organized into three customizable gene rows to facilitate direct comparison of trends - for example, contrasting RA synthesis genes (Aldh1a1-3, Rdh10) against RA-responsive (Stra8, Stra6) and RA-degrading (Cyp26a1-c1, Rarg) factors.",
  cell_to_cell_heatmap = "The Cell-to-Cell Communication tab presents a heatmap of ligand-receptor (LR) communication scores across four grouped seminiferous tubule stages (I-VI, VII-VIII, IX-X, and XI-XII). A curated panel of biologically relevant LR pairs is available for selection, spanning major signaling pathways involved in spermatogenesis including FGF/FGFR, IGF/IGF1R, KITL/KIT, GDNF/GFRA1, WNT/FZD, Notch (DLL/JAG-NOTCH), Hedgehog (DHH-PTCH1), CXCL12/CXCR4, PDGF, TGF-beta, and Semaphorin/Plexin pairs. The color intensity of each heatmap cell reflects the communication score for that LR pair at that stage grouping, enabling users to identify stage-specific peaks of paracrine or juxtacrine signaling and download customized heatmaps for publication or further analysis."
)

release_notes_path <- "release_notes.csv"

# Fallback release-note row used when release_notes.csv is missing or invalid.
default_release_notes <- function() {
  return(data.frame(
    version_number = "0.6",
    update_type = "Major",
    update_title = "Networking Update",
    update_description = paste(
      "This release focused on network and startup performance. The home page now avoids eagerly bootstrapping the staged and subset tabs, preview imagery was reduced to web-sized assets, and the initial #home startup payload is smaller than in earlier versions.",
      "The same release also included follow-up layout and responsiveness fixes across the interactive pages, plus cleanup for the spermatogonia modal and staged/subset statistics panels.",
      sep = "\n\n"
    ),
    update_date = "2026-03-24",
    stringsAsFactors = FALSE
  ))
}

# Load release notes from CSV and normalize them to the schema expected by the
# Patch Notes page.
load_release_notes <- function(path = release_notes_path) {
  required_cols <- c(
    "version_number",
    "update_type",
    "update_title",
    "update_description",
    "update_date"
  )

  if (!file.exists(path)) {
    return(default_release_notes())
  }

  notes <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )

  if (is.null(notes) || !all(required_cols %in% names(notes))) {
    return(default_release_notes())
  }

  notes <- notes[, required_cols, drop = FALSE]
  notes[] <- lapply(notes, function(x) trimws(as.character(x)))
  notes <- notes[!is.na(notes$version_number) & nzchar(notes$version_number), , drop = FALSE]

  if (!nrow(notes)) {
    return(default_release_notes())
  }

  return(notes)
}

# Format a release heading, including the update title for major releases.
format_release_heading <- function(entry) {
  stopifnot(nrow(entry) == 1)
  if (identical(tolower(entry$update_type[[1]]), "major") && nzchar(entry$update_title[[1]])) {
    return(paste0("Version ", entry$version_number[[1]], " - ", entry$update_title[[1]]))
  }
  return(paste0("Version ", entry$version_number[[1]]))
}

# Display release dates in a reader-friendly form while preserving unparseable
# values as written.
format_release_date <- function(value) {
  parsed <- suppressWarnings(as.Date(value))
  if (is.na(parsed)) {
    return(as.character(value))
  }
  return(format(parsed, "%B %d, %Y"))
}

# Convert release-note markdown-like text into Shiny tag nodes. Bullet-only
# descriptions become lists; paragraph text remains paragraph text.
render_release_description <- function(text) {
  normalized <- gsub("\r\n?", "\n", text)
  lines <- unlist(strsplit(normalized, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (!length(lines)) {
    return(list(tags$p("No release description provided.")))
  }

  is_bullet <- grepl("^\\*\\s+", lines)
  if (all(is_bullet)) {
    bullet_items <- sub("^\\*\\s+", "", lines)
    return(list(tags$ul(lapply(bullet_items, tags$li))))
  }

  paragraphs <- trimws(unlist(strsplit(normalized, "\n\\s*\n", perl = TRUE)))
  paragraphs <- paragraphs[nzchar(paragraphs)]
  if (!length(paragraphs)) {
    return(list(tags$p("No release description provided.")))
  }

  return(lapply(paragraphs, function(paragraph) {
    paragraph_lines <- trimws(unlist(strsplit(paragraph, "\n", fixed = TRUE)))
    paragraph_lines <- paragraph_lines[nzchar(paragraph_lines)]
    if (length(paragraph_lines) && all(grepl("^\\*\\s+", paragraph_lines))) {
      bullet_items <- sub("^\\*\\s+", "", paragraph_lines)
      return(tags$ul(lapply(bullet_items, tags$li)))
    }
    return(tags$p(paragraph))
  }))
}

# Build one Patch Notes entry and optionally prepend a divider.
make_patch_notes_entry <- function(entry, include_divider = FALSE) {
  stopifnot(nrow(entry) == 1)
  return(tagList(
    if (isTRUE(include_divider)) tags$hr(class = "ra-divider") else NULL,
    tags$div(
      class = "patch-notes-entry",
      tags$h3(format_release_heading(entry)),
      tags$p(class = "ra-sub", format_release_date(entry$update_date[[1]])),
      render_release_description(entry$update_description[[1]])
    )
  ))
}

release_notes_df <- load_release_notes()
current_release <- release_notes_df[1, , drop = FALSE]
current_release_label <- format_release_heading(current_release)

# Build the Patch Notes tab from the release-note data loaded at startup.
make_patch_notes_page <- function() {
  entry_nodes <- lapply(seq_len(nrow(release_notes_df)), function(i) {
    return(make_patch_notes_entry(release_notes_df[i, , drop = FALSE], include_divider = i > 1))
  })

  return(tabPanel(
    title = "Patch Notes",
    value = "patch_notes",
    tags$div(
      class = "patch-notes-page",
      tags$div(
        class = "home-card glass-card patch-notes-card",
        tags$h2("Patch Notes"),
        tags$p(
          class = "ra-sub",
          "Release history and update notes for the interactive dataset."
        ),
        entry_nodes
      )
    )
  ))
}

# ---------------------------------------------------------------------------
# Lazy dataset tab registration
# ---------------------------------------------------------------------------

# Convert a tab value into the output ID that server.R fills on first open.
dataset_tab_output_id <- function(tab_value) {
  return(paste0("lazy_", tab_value, "_body"))
}

# Register a tab shell whose body is rendered lazily by server.R.
make_lazy_dataset_tab <- function(title, value) {
  return(tabPanel(
    title = HTML(title),
    value = value,
    uiOutput(dataset_tab_output_id(value))
  ))
}

# Figure-page suffixes shared by the Full Atlas and Cell Subsets menus.
dataset_tab_specs <- list(
  list(title = "Main Figures", suffix = "main_figures"),
  list(title = "CellInfo vs GeneExpr", suffix = "cellinfo_gene"),
  list(title = "Multiple GeneExpr", suffix = "multiple_geneexpr"),
  list(title = "Gene coexpression", suffix = "gene_coexpression"),
  list(title = "Violinplot / Boxplot", suffix = "violin_boxplot"),
  list(title = "Proportion plot", suffix = "proportion_plot"),
  list(title = "Bubbleplot / Heatmap", suffix = "bubble_heatmap")
)

# Build a dataset menu when every tab should appear directly under one menu.
make_lazy_dataset_menu <- function(menu_title, prefix, tab_specs = dataset_tab_specs) {
  tabs <- lapply(tab_specs, function(tab_spec) {
    return(make_lazy_dataset_tab(
      title = tab_spec$title,
      value = paste0(prefix, "_", tab_spec$suffix)
    ))
  })

  return(do.call(navbarMenu, c(list(menu_title), tabs)))
}

# Build the Cell Subsets dropdown. Main-figure tabs are visible first; the
# remaining subset tabs are registered for hash routing and mini navigation.
make_cell_subsets_menu <- function() {
  subset_specs <- list(
    list(title = "Sertoli", prefix = "sc4"),
    list(title = "Spermatogonia", prefix = "sc5"),
    list(title = "Spermatocyte", prefix = "sc6"),
    list(title = "Spermatid", prefix = "sc7")
  )

  menu_items <- list()

  for (subset_spec in subset_specs) {
    menu_items <- c(
      menu_items,
      list(
        make_lazy_dataset_tab(
          title = subset_spec$title,
          value = paste0(subset_spec$prefix, "_main_figures")
        )
      )
    )
  }

  for (subset_spec in subset_specs) {
    hidden_tabs <- lapply(dataset_tab_specs[-1], function(tab_spec) {
      return(make_lazy_dataset_tab(
        title = tab_spec$title,
        value = paste0(subset_spec$prefix, "_", tab_spec$suffix)
      ))
    })
    menu_items <- c(menu_items, hidden_tabs)
  }

  return(do.call(navbarMenu, c(list("Cell Subsets"), menu_items)))
}

# ---------------------------------------------------------------------------
# Interactive-data navigation
# ---------------------------------------------------------------------------

# Mini-navigation links shown within the specialized interactive-data pages.
interactive_data_nav_specs <- list(
  list(title = "Spermatogenesis Table", value = "spermatogonia_table"),
  list(title = "Retinoic Acid Analysis", value = "retinoic_acid"),
  list(title = "Cell-to-Cell Heatmap", value = "cell2cell_heatmaps")
)

# Build the active-state mini navigation for specialized interactive-data tabs.
make_interactive_data_nav <- function(active_value) {
  links <- lapply(interactive_data_nav_specs, function(nav_spec) {
    classes <- c("subset-secondary-link")
    if (identical(nav_spec$value, active_value)) {
      classes <- c(classes, "is-active")
    }

    return(tags$a(
      class = paste(classes, collapse = " "),
      href = paste0("#", nav_spec$value),
      role = "button",
      `data-target-tab` = nav_spec$value,
      `aria-current` = if (identical(nav_spec$value, active_value)) "page" else NULL,
      onclick = "return window.navToTab(this.getAttribute('data-target-tab'), this);",
      nav_spec$title
    ))
  })

  return(tags$nav(
    class = "subset-secondary-nav",
    `aria-label` = "Interactive data navigation",
    tags$div(class = "subset-secondary-nav-inner", links)
  ))
}

# ---------------------------------------------------------------------------
# App shell
# ---------------------------------------------------------------------------
shinyUI(
  # THEMES: lightblue, dark, mint, berry, sand, forest, sunset, ocean, lavender, default
  fluidPage(
    # Apply the default theme before the rest of the page renders so first paint
    # does not flash with an unset data-theme value.
    tags$head(
      tags$script(HTML(sprintf(
        "document.documentElement.setAttribute('data-theme', '%s');",
        theme_default
      ))),
      tags$style(HTML("
    .shiny-output-error-validation {color: red; font-weight: bold;}
    .navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }
  ")),
    ),

    # Bootstrap theme baseline. Runtime light/dark switching is handled by the
    # data-theme attribute and CSS variables rather than rebuilding the page.
    theme = bs_theme(
      version = 5,
      bootswatch = "minty", # Try "sandstone" or "materia" too
      base_font = font_google("Open Sans"),
      primary = "#4A90E2",
      bg = "#D3D3D3",
      fg = "#2C3E50"
    ),
    # Runtime CSS, static assets, navbar sizing, and persistent theme switching.
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
      tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      tags$script(src = "presets.js"),
      # Keep CSS layout variables in sync with the rendered navbar dimensions.
      tags$script(HTML("
    function setNavVars() {
      // Prefer the inner container (accounts for navbar padding)
      var el = document.querySelector('.navbar .container, .navbar .container-fluid')
               || document.querySelector('.navbar');
      if (!el) return;

      var rect = el.getBoundingClientRect();
      var vw   = document.documentElement.clientWidth;   // excludes scrollbar

      // Absolute offsets from viewport edges
      var left  = Math.max(0, rect.left + window.scrollX);
      var right = Math.max(0, vw - rect.right + window.scrollX);
      var width = Math.max(0, rect.width);

      document.documentElement.style.setProperty('--nav-left',  left + 'px');
      document.documentElement.style.setProperty('--nav-right', right + 'px');
      document.documentElement.style.setProperty('--nav-width', width + 'px');
      document.documentElement.style.setProperty('--viewport-w', vw + 'px');

      // total navbar stack height (brand row + menu row if any)
      var totalH = 0;
      $('.navbar:visible').each(function(){ totalH += $(this).outerHeight(true) || 0; });
      document.documentElement.style.setProperty('--nav-total-h', totalH + 'px');
    }

    $(document).on('shiny:connected', setNavVars);
    $(window).on('resize', setNavVars);
    document.addEventListener('DOMContentLoaded', setNavVars);
    setTimeout(setNavVars, 250);
  ")),
      # Persist the light/dark theme in localStorage and notify server.R through
      # input$theme_mode.
      tags$script(HTML(sprintf("
    (function() {
      var STORAGE_KEY = 'sc-theme';
      var DARK_THEME = '%s';
      var LIGHT_THEME = '%s';
      var START_THEME = '%s';
      var root = document.documentElement;
      var pendingTheme = null;

      function notifyShiny(theme) {
        var nextTheme = theme || START_THEME;
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue('theme_mode', nextTheme, {priority: 'event'});
          pendingTheme = null;
        } else {
          pendingTheme = nextTheme;
        }
      }

      function applyTheme(theme) {
        var nextTheme = theme || START_THEME;
        root.setAttribute('data-theme', nextTheme);
        var toggle = document.getElementById('theme-toggle');
        if (toggle) {
          var isDark = nextTheme === DARK_THEME;
          toggle.checked = isDark;
          toggle.setAttribute('aria-checked', String(isDark));
          toggle.setAttribute('aria-label', isDark ? 'Switch to light mode' : 'Switch to dark mode');
        }
        notifyShiny(nextTheme);
      }

      function bindToggle() {
        var toggle = document.getElementById('theme-toggle');
        if (!toggle || toggle.dataset.bound === 'true') { return; }
        toggle.dataset.bound = 'true';

        var storedTheme = null;
        try {
          storedTheme = window.localStorage && localStorage.getItem(STORAGE_KEY);
        } catch (err) {
          storedTheme = null;
        }

        var initial = storedTheme || root.getAttribute('data-theme') || START_THEME;
        applyTheme(initial);

        toggle.addEventListener('change', function(evt) {
          var theme = evt.target.checked ? DARK_THEME : LIGHT_THEME;
          applyTheme(theme);
          try {
            window.localStorage && localStorage.setItem(STORAGE_KEY, theme);
          } catch (err) {
            /* ignore */
          }
        });
      }

	      function init() {
	        if (document.readyState === 'loading') {
	          document.addEventListener('DOMContentLoaded', bindToggle);
	        } else {
	          bindToggle();
	        }
	        function whenShinyReady(callback) {
	          var tries = 0;
	          (function tick() {
	            if (window.Shiny && typeof Shiny.setInputValue === 'function') {
	              callback();
	              return;
	            }
	            if (tries++ < 200) {
	              setTimeout(tick, 100);
	            }
	          })();
	        }

	        // Ensure the server always receives an initial theme value (even if Shiny
	        // wasn't available when bindToggle() first ran).
	        whenShinyReady(function() {
	          notifyShiny(pendingTheme || root.getAttribute('data-theme') || START_THEME);
	        });
	      }

	      init();
	    })();
	  ", theme_dark, theme_light, theme_default))),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Quicksand:wght@400;500;600;700&display=swap",
        rel = "stylesheet"
      )
    ),

    # -----------------------------------------------------------------------
    # Client-side handlers
    # -----------------------------------------------------------------------
    tags$head(
      # On-SVG numeric labels for the spermatogenesis heat overlay.
      tags$style(HTML("
    .heat-label{
      font: 15px/1.05 'Open Sans', sans-serif;
      font-weight: 700;
      fill:#111827;
      text-anchor: middle;
      dominant-baseline: central;
      pointer-events: none;
      paint-order: stroke;
      stroke: #fff; stroke-width: 2px;
      opacity: .9;
    }
    html[data-theme='dark'] .heat-label{
      fill:#f8fafc;
      stroke:#020617;
      stroke-width: 3px;
    }
  ")),


      # Spermatogenesis SVG click handling, modal locks, heatmap overlays, and
      # client-side cleanup after Bootstrap modals close.
      tags$script(HTML("
    (function () {
      var activeBtnId = null;
      var spgModalLocked = false;
      var hasOwn = Object.prototype.hasOwnProperty;

      // ===== DEBUG TOGGLES =====
      var HEAT_DEBUG = /(?:^|[?&])heat_debug=1(?:&|$)/.test(window.location.search);
      var HEAT_LABEL_MODE = 'value';      // 'value' | 'scaled' | 'rank'

      function getNode(id) { return document.getElementById(id); }

      function setSpgModalLocked(isLocked) {
        spgModalLocked = !!isLocked;
        var host = document.getElementById('spermatogonia_container');
        if (host) {
          if (spgModalLocked) host.classList.add('spg-modal-locked');
          else host.classList.remove('spg-modal-locked');
        }
      }

      function clamp01(x) {
        if (!isFinite(x)) return 0;
        if (x < 0) return 0;
        if (x > 1) return 1;
        return x;
      }

      function hexToRgb(hex) {
        if (typeof hex !== 'string') return null;
        var clean = hex.trim();
        if (!clean) return null;
        if (clean.charAt(0) === '#') clean = clean.slice(1);
        if (clean.length === 3) clean = clean.replace(/./g, function (c) { return c + c; });
        if (clean.length !== 6) return null;
        var num = parseInt(clean, 16);
        if (!isFinite(num)) return null;
        return { r: (num >> 16) & 255, g: (num >> 8) & 255, b: num & 255 };
      }

      function channelToHex(v) {
        var clamped = Math.max(0, Math.min(255, Math.round(v)));
        var str = clamped.toString(16);
        return str.length === 1 ? '0' + str : str;
      }

      function rgbToHex(r, g, b) { return '#' + channelToHex(r) + channelToHex(g) + channelToHex(b); }

      function mixWithWhite(hex, weight) {
        var rgb = hexToRgb(hex);
        if (!rgb) return hex;
        var t = clamp01(weight);
        var r = rgb.r * t + 255 * (1 - t);
        var g = rgb.g * t + 255 * (1 - t);
        var b = rgb.b * t + 255 * (1 - t);
        return rgbToHex(r, g, b);
      }

      function isDarkTheme() {
        return document.documentElement.getAttribute('data-theme') === 'dark';
      }

      function interpolateHeatColor(scaled, stops) {
        var t = clamp01(scaled);
        var last = stops.length - 1;
        var pos = t * last;
        var idx = Math.min(last - 1, Math.floor(pos));
        var frac = pos - idx;
        var a = hexToRgb(stops[idx]);
        var b = hexToRgb(stops[idx + 1]);
        if (!a || !b) return stops[idx] || '#38bdf8';
        return rgbToHex(
          a.r + (b.r - a.r) * frac,
          a.g + (b.g - a.g) * frac,
          a.b + (b.b - a.b) * frac
        );
      }

      function selectedHighlightVisual() {
        if (isDarkTheme()) {
          return {
            stroke: '#fbbf24',
            fill: 'rgba(251,191,36,0.20)'
          };
        }
        return {
          stroke: '#4F46E5',
          fill: 'rgba(79,70,229,0.12)'
        };
      }

	      // ===== SVG helpers for heat labels =====
	      function isSvgNode(el){ return !!(el && (el.ownerSVGElement || el.tagName === 'svg' || /svg/i.test(el.namespaceURI||''))); }
	      function ensureHeatLabel(el, text){
	        if (!isSvgNode(el)) return;
	        var bb;
	        try {
	          bb = el.getBBox();
	        } catch (err) {
	          clearHeatLabel(el);
	          return;
	        }
	        if (!bb || !isFinite(bb.x) || !isFinite(bb.y) || !isFinite(bb.width) || !isFinite(bb.height) || bb.width <= 0 || bb.height <= 0) {
	          clearHeatLabel(el);
	          return;
	        }
	        var id = el.id + '__label';
	        var label = document.getElementById(id);
	        if (!label){
          label = document.createElementNS('http://www.w3.org/2000/svg', 'text');
          label.setAttribute('id', id);
          label.setAttribute('class', 'heat-label');
          if (el.parentNode) el.parentNode.insertBefore(label, el.nextSibling);
        }
        label.setAttribute('x', (bb.x + bb.width/2));
        label.setAttribute('y', (bb.y + bb.height/2));
        label.textContent = text;
      }
      function clearHeatLabel(el){
        var lab = document.getElementById(el.id + '__label');
        if (lab && lab.parentNode) lab.parentNode.removeChild(lab);
      }

      function applyHeatVisual(el, stroke, fill, alpha) {
        if (stroke) {
          el.style.stroke = stroke;
          el.style.strokeWidth = '3px';
        } else {
          el.style.removeProperty('stroke');
          el.style.removeProperty('stroke-width');
        }
        if (fill) {
          el.style.fill = fill;
          if (alpha !== undefined && alpha !== null && alpha !== '') {
            el.style.fillOpacity = alpha;
          } else {
            el.style.removeProperty('fill-opacity');
          }
        } else {
          el.style.removeProperty('fill');
          el.style.removeProperty('fill-opacity');
        }
      }

      function clearHeatVisual(el) {
        el.style.removeProperty('stroke');
        el.style.removeProperty('stroke-width');
        el.style.removeProperty('fill');
        el.style.removeProperty('fill-opacity');
        clearHeatLabel(el); // remove debug label
      }

      function restoreHighlight(el) {
        if (!el) return;
        el.classList.remove('btn-highlight');

        var stroke = hasOwn.call(el.dataset, 'restoreStroke') ? el.dataset.restoreStroke : el.dataset.heatStroke || '';
        var fill   = hasOwn.call(el.dataset, 'restoreFill')   ? el.dataset.restoreFill   : el.dataset.heatFill   || '';
        var alpha  = hasOwn.call(el.dataset, 'restoreAlpha')  ? el.dataset.restoreAlpha  : el.dataset.heatAlpha  || '';

        applyHeatVisual(el, stroke, fill, alpha);

        if (hasOwn.call(el.dataset, 'restoreStroke')) delete el.dataset.restoreStroke;
        if (hasOwn.call(el.dataset, 'restoreFill')) delete el.dataset.restoreFill;
        if (hasOwn.call(el.dataset, 'restoreAlpha')) delete el.dataset.restoreAlpha;

        if (!el.classList.contains('heat-on') && !stroke) {
          el.style.removeProperty('stroke-width');
        }
      }

      // Click → server (open modal)
      document.addEventListener('click', function (e) {
        var btn = e.target.closest && e.target.closest('.cell-btn');
        if (!btn) return;
        if (spgModalLocked) {
          e.preventDefault();
          e.stopPropagation();
          return false;
        }
        setSpgModalLocked(true);
        Shiny.setInputValue('btn_click', btn.id, {priority: 'event'});
      });

      Shiny.addCustomMessageHandler('spgModalLock', function (isLocked) {
        setSpgModalLocked(isLocked);
      });

      Shiny.addCustomMessageHandler('highlightButton', function (btn_id) {
        if (activeBtnId && activeBtnId !== btn_id) restoreHighlight(getNode(activeBtnId));
        var el = getNode(btn_id);
        if (!el) return;

        el.dataset.restoreStroke = el.dataset.heatStroke || el.style.stroke || '';
        el.dataset.restoreFill   = el.dataset.heatFill   || el.style.fill   || '';
        el.dataset.restoreAlpha  = el.dataset.heatAlpha  || el.style.fillOpacity || '';

        var selectedVisual = selectedHighlightVisual();
        el.classList.add('btn-highlight');
        el.style.stroke = selectedVisual.stroke;
        el.style.strokeWidth = '3px';
        el.style.fill = selectedVisual.fill;
        el.style.fillOpacity = '1';
        activeBtnId = btn_id;
      });

      Shiny.addCustomMessageHandler('unhighlightButton', function (btn_id) {
        var el = getNode(btn_id);
        restoreHighlight(el);
        if (activeBtnId === btn_id) activeBtnId = null;
      });

      Shiny.addCustomMessageHandler('bulkHighlight', function (payload) {
        var allBtns = document.querySelectorAll('.cell-btn');
        Array.prototype.forEach.call(allBtns, function (el) {
          el.classList.remove('btn-gmatch');
        });
        if (!payload || !Array.isArray(payload.match_ids)) return;
        var seen = Object.create(null);
        payload.match_ids.forEach(function (id) {
          var key = String(id);
          if (seen[key]) return;
          seen[key] = true;
          var el = getNode(key);
          if (el) el.classList.add('btn-gmatch');
        });
      });

      Shiny.addCustomMessageHandler('spgBusy', function (isBusy) {
        var host = document.querySelector('.ra-card.ra-plot');
        if (!host) return;
        if (isBusy) host.classList.add('is-busy'); else host.classList.remove('is-busy');
      });

      function cleanupModalScrollLock() {
        window.setTimeout(function () {
          if (document.querySelector('.modal.show, .modal.in')) return;
          document.body.classList.remove('modal-open');
          document.body.style.removeProperty('overflow');
          document.body.style.removeProperty('padding-right');
          Array.prototype.forEach.call(document.querySelectorAll('.modal-backdrop'), function (backdrop) {
            if (backdrop && backdrop.parentNode) backdrop.parentNode.removeChild(backdrop);
          });
        }, 50);
      }

      function handleModalHidden() {
        setSpgModalLocked(false);
        if (window.Shiny) {
          Shiny.setInputValue('__modal__closed__', Date.now(), {priority: 'event'});
        }
        if (activeBtnId) {
            restoreHighlight(getNode(activeBtnId));
            activeBtnId = null;
          }
          cleanupModalScrollLock();
        }

      // FIX for earlier error: attach to document (always exists).
      // Bootstrap 3 routes this through jQuery; newer Bootstrap can emit a native event.
      document.addEventListener('hidden.bs.modal', handleModalHidden);
      if (window.jQuery && window.jQuery.fn) {
        window.jQuery(document).on('hidden.bs.modal', handleModalHidden);
      }

      document.addEventListener('mousedown', function (e) {
        var btn = e.target.closest && e.target.closest('.cell-btn');
        if (btn && btn.blur) btn.blur();
      });

      // ===== HEATMAP =====
      Shiny.addCustomMessageHandler('heatmap-colorize', function (payload) {
        var colorsRaw = payload && payload.colors;
        var colors = [];

        if (Array.isArray(colorsRaw)) {
          colors = colorsRaw;
        } else if (colorsRaw && typeof colorsRaw === 'object') {
          // 👇 convert {id: item, ...} into an array; inject id if missing
          colors = Object.keys(colorsRaw).map(function(k){
            var item = colorsRaw[k];
            if (item && item.id == null) item.id = k;
            return item;
          });
        }
        var clear = payload && payload.clear;
        var nodes = document.querySelectorAll('.cell-btn');
        if (HEAT_DEBUG) {
          window._heat_last_payload = payload;
        }

        if (clear || !colors.length) {
          Array.prototype.forEach.call(nodes, function (el) {
            el.classList.remove('heat-on');
            delete el.dataset.heatStroke;
            delete el.dataset.heatFill;
            delete el.dataset.heatAlpha;
            delete el.dataset.heatRank;
            delete el.dataset.heatValue;
            el.removeAttribute('title');

            if (el.classList.contains('btn-highlight')) {
              el.dataset.restoreStroke = '';
              el.dataset.restoreFill = '';
              el.dataset.restoreAlpha = '';
            } else {
              clearHeatVisual(el);
            }
          });
          return;
        }

        var map = Object.create(null);
        colors.forEach(function (item) {
          if (!item || item.id === undefined || item.id === null) return;
          map[String(item.id)] = item;
        });

        Array.prototype.forEach.call(nodes, function (el) {
          var info = map[el.id];

          // Not in this batch → clear visuals for this node
          if (!info) {
            el.classList.remove('heat-on');
            delete el.dataset.heatStroke;
            delete el.dataset.heatFill;
            delete el.dataset.heatAlpha;
            delete el.dataset.heatRank;
            delete el.dataset.heatValue;
            el.removeAttribute('title');

            if (el.classList.contains('btn-highlight')) {
              el.dataset.restoreStroke = '';
              el.dataset.restoreFill = '';
              el.dataset.restoreAlpha = '';
            } else {
              clearHeatVisual(el);
            }
            return;
          }

          // Use server-provided base color and our lightening mix
          var color = info.color || '#2171b5';
          var scaledRaw = (typeof info.scaled === 'number') ? info.scaled : parseFloat(info.scaled);
          var hasScaled = isFinite(scaledRaw);
          var scaled = hasScaled ? clamp01(scaledRaw) : null;
          var darkMode = isDarkTheme();
          var fillColor;
          var alpha;
          var strokeColor;
          if (darkMode) {
            color = hasScaled ? interpolateHeatColor(scaled, ['#38bdf8', '#22d3ee', '#facc15']) : '#38bdf8';
            fillColor = color;
            strokeColor = mixWithWhite(color, 0.82);
            alpha = (scaled === null) ? 0.62 : (0.48 + 0.34 * scaled);
          } else {
            var mix = (scaled === null) ? 0.75 : (0.35 + 0.65 * scaled);
            fillColor = mixWithWhite(color, mix);
            strokeColor = color;
            alpha = (scaled === null) ? 0.75 : (0.65 + 0.25 * scaled);
          }

          // Remember values for restore
          el.dataset.heatStroke = strokeColor;
          el.dataset.heatFill   = fillColor;
          el.dataset.heatAlpha  = String(alpha);
          if (info.rank !== undefined && info.rank !== null) el.dataset.heatRank = String(info.rank); else delete el.dataset.heatRank;
          if (info.value !== undefined && info.value !== null) el.dataset.heatValue = String(info.value); else delete el.dataset.heatValue;

          el.classList.add('heat-on');

          // Tooltip
          var tooltip = [];
          if (info.rank !== undefined && info.rank !== null) tooltip.push('#' + info.rank);
          if (info.value !== undefined && info.value !== null) tooltip.push('avg ' + info.value);
          if (tooltip.length) el.setAttribute('title', tooltip.join(' • ')); else el.removeAttribute('title');

          // Remove teal fallback so it doesn't blend with heat color
          el.classList.remove('btn-gmatch');

          // Apply to SVG
          if (el.classList.contains('btn-highlight')) {
            el.dataset.restoreStroke = strokeColor;
            el.dataset.restoreFill   = fillColor;
            el.dataset.restoreAlpha  = String(alpha);
          } else {
            applyHeatVisual(el, strokeColor, fillColor, String(alpha));
          }

	          // Draw numeric label for the searched gene values.
	          var txt = '';
	          if (HEAT_LABEL_MODE === 'scaled' && hasScaled) txt = scaled.toFixed(2);
	          else if (HEAT_LABEL_MODE === 'rank' && info.rank != null) txt = String(info.rank);
	          else if (HEAT_LABEL_MODE === 'value' && info.value != null && isFinite(info.value)) txt = Number(info.value).toFixed(2);
	          if (txt) ensureHeatLabel(el, txt);
	          else clearHeatLabel(el);
	        });
	      });
	    })();
  "))
    ),
    # Numbered tooltip bubbles shown when the user enables extended tooltips.
    tags$script(HTML("
  (function () {
    var layer = null;
    var specs = [];
    var bubbles = Object.create(null);
    var enabled = false;
    var currentTab = 'home';
    var updateHandle = null;

    function normalizeTabValue(value) {
      if (!value) return null;
      if (value === 'retinoic_acid_line') return 'retinoic_acid';
      return value;
    }

    function getTabValueFromAnchor(anchor) {
      if (!anchor) return null;
      return anchor.getAttribute('data-value') ||
        anchor.getAttribute('data-tab-value') ||
        (anchor.getAttribute('data-bs-target') || '').replace('#', '') ||
        (anchor.getAttribute('href') || '').replace('#', '');
    }

    function readActiveTab() {
      var active = document.querySelector('#mainTabs li.active > a, #mainTabs a.active');
      return normalizeTabValue(getTabValueFromAnchor(active)) || currentTab || 'home';
    }

    function ensureLayer() {
      if (layer) return layer;
      layer = document.createElement('div');
      layer.id = 'extended-tooltip-layer';
      layer.className = 'extended-tooltip-layer';
      layer.setAttribute('aria-hidden', 'true');
      document.body.appendChild(layer);
      return layer;
    }

    function getCurrentTab() {
      currentTab = normalizeTabValue(readActiveTab()) || normalizeTabValue(currentTab) || 'home';
      return currentTab;
    }

    function isVisibleElement(el) {
      if (!el || !el.getBoundingClientRect) return false;
      var rect = el.getBoundingClientRect();
      if (!rect || (rect.width <= 0 && rect.height <= 0)) return false;
      var style = window.getComputedStyle ? window.getComputedStyle(el) : null;
      if (style && (style.display === 'none' || style.visibility === 'hidden' || style.opacity === '0')) {
        return false;
      }
      return true;
    }

    function activePane() {
      return document.querySelector('.tab-pane.active') || document.body || document;
    }

    function firstVisible(selector, root) {
      if (!selector) return null;
      root = root || document;
      var nodes = root.querySelectorAll ? root.querySelectorAll(selector) : [];
      for (var i = 0; i < nodes.length; i++) {
        if (isVisibleElement(nodes[i])) return nodes[i];
      }
      return null;
    }

    function activeTarget(selector) {
      return firstVisible(selector, activePane()) || firstVisible(selector, document);
    }

    function findCellSubsetsDropdown() {
      var anchors = document.querySelectorAll('#mainTabs > li > a, #mainTabs > li.dropdown > a, .navbar .dropdown > a');
      for (var i = 0; i < anchors.length; i++) {
        var text = (anchors[i].textContent || '').replace(/\\s+/g, ' ').trim();
        if (text === 'Cell Subsets') return anchors[i];
      }
      return null;
    }

    function findCellSubsetsMenu() {
      var anchor = findCellSubsetsDropdown();
      if (!anchor || !anchor.closest) return null;
      var dropdown = anchor.closest('.dropdown');
      if (!dropdown) return null;
      var menu = dropdown.querySelector('.dropdown-menu');
      return isVisibleElement(menu) ? menu : null;
    }

    function getTarget(spec) {
      if (!spec) return null;
      if (typeof spec.target === 'function') return spec.target();
      if (spec.selector) return activeTarget(spec.selector);
      return null;
    }

    function tabInGroup(group, tab) {
      if (group === 'any') return true;
      if (group === 'interactive_data') {
        return tab === 'spermatogonia_table' || tab === 'retinoic_acid' || tab === 'cell2cell_heatmaps';
      }
      if (group === 'full_atlas') return /^sc3_/.test(tab);
      if (group === 'cell_subsets') return /^sc[4-7]_/.test(tab);
      return tab === group;
    }

    function matchesPage(spec, tab) {
      var pages = spec.pages || spec.page;
      if (!pages) return true;
      if (typeof pages === 'function') return !!pages(tab);
      if (!Array.isArray(pages)) pages = [pages];
      for (var i = 0; i < pages.length; i++) {
        if (tabInGroup(pages[i], tab)) return true;
      }
      return false;
    }

    function shouldRenderSpec(spec, tab) {
      if (!enabled || !spec || !spec.id) return false;
      if (!matchesPage(spec, tab)) return false;
      if (typeof spec.showWhen === 'function' && !spec.showWhen(tab)) return false;
      if (spec.placement === 'center') return true;
      return isVisibleElement(getTarget(spec));
    }

    function collapseAll(exceptId, shouldSchedule) {
      Object.keys(bubbles).forEach(function (id) {
        if (id === exceptId) return;
        bubbles[id].classList.remove('is-expanded');
        bubbles[id].setAttribute('aria-expanded', 'false');
      });
      if (shouldSchedule !== false) scheduleUpdate();
    }

    function createBubble(spec) {
      var specIndex = specs.indexOf(spec);
      var stepNumber = spec.number || spec.step || (specIndex >= 0 ? specIndex + 1 : '');
      var bubble = document.createElement('button');
      bubble.type = 'button';
      bubble.className = 'extended-tooltip-bubble';
      bubble.setAttribute('aria-expanded', 'false');
      bubble.setAttribute('aria-label', 'Tutorial step ' + stepNumber + ': ' + (spec.label || spec.id));

      var icon = document.createElement('span');
      icon.className = 'extended-tooltip-icon';
      icon.textContent = stepNumber;

      var content = document.createElement('span');
      content.className = 'extended-tooltip-content';
      content.textContent = spec.text || '';

      bubble.appendChild(icon);
      bubble.appendChild(content);
      bubble.addEventListener('click', function (event) {
        event.preventDefault();
        event.stopPropagation();
        var willExpand = !bubble.classList.contains('is-expanded');
        collapseAll(spec.id);
        bubble.classList.toggle('is-expanded', willExpand);
        bubble.setAttribute('aria-expanded', String(willExpand));
        scheduleUpdate();
      });

      ensureLayer().appendChild(bubble);
      return bubble;
    }

    function updateBubbleContent(spec, bubble) {
      var specIndex = specs.indexOf(spec);
      var stepNumber = spec.number || spec.step || (specIndex >= 0 ? specIndex + 1 : '');
      var icon = bubble.querySelector('.extended-tooltip-icon');
      var content = bubble.querySelector('.extended-tooltip-content');
      if (icon) icon.textContent = stepNumber;
      if (content) content.textContent = spec.text || '';
      bubble.setAttribute('aria-label', 'Tutorial step ' + stepNumber + ': ' + (spec.label || spec.id));
    }

    function clampPosition(bubble, left, top) {
      var viewportW = document.documentElement.clientWidth || window.innerWidth || 0;
      var viewportH = document.documentElement.clientHeight || window.innerHeight || 0;
      var rect = bubble.getBoundingClientRect();
      var margin = 12;
      if (viewportW > 0) {
        left = Math.max(margin, Math.min(left, viewportW - rect.width - margin));
      }
      if (viewportH > 0) {
        top = Math.max(margin, Math.min(top, viewportH - rect.height - margin));
      }
      bubble.style.left = left + 'px';
      bubble.style.top = top + 'px';
    }

    function positionBubble(spec, bubble) {
      if (spec.placement === 'center') {
        bubble.hidden = false;
        bubble.style.left = '0px';
        bubble.style.top = '0px';
        var viewportW = document.documentElement.clientWidth || window.innerWidth || 0;
        var viewportH = document.documentElement.clientHeight || window.innerHeight || 0;
        var bubbleRect = bubble.getBoundingClientRect();
        var centerLeft = Math.round((viewportW - bubbleRect.width) / 2);
        var centerTop = Math.round((viewportH - bubbleRect.height) / 2);
        clampPosition(bubble, centerLeft, centerTop);
        return;
      }

      var target = getTarget(spec);
      if (!target) {
        bubble.hidden = true;
        return;
      }
      var rect = target.getBoundingClientRect();
      if (!rect || (rect.width <= 0 && rect.height <= 0)) {
        bubble.hidden = true;
        return;
      }

      bubble.hidden = false;
      bubble.style.left = '0px';
      bubble.style.top = '0px';
      var left = rect.right + (spec.offsetX == null ? 6 : spec.offsetX);
      var top = rect.top + (spec.offsetY == null ? -12 : spec.offsetY);
      bubble.style.left = left + 'px';
      bubble.style.top = top + 'px';
      clampPosition(bubble, left, top);
    }

    function renderTooltips() {
      ensureLayer();
      var tab = getCurrentTab();
      var visibleSpecs = [];
      if (enabled) {
        visibleSpecs = specs.filter(function (spec) {
          return shouldRenderSpec(spec, tab);
        });
      }

      var visible = visibleSpecs.length > 0;
      layer.classList.toggle('is-visible', visible);
      layer.setAttribute('aria-hidden', visible ? 'false' : 'true');
      if (!visible) {
        collapseAll(null, false);
        Object.keys(bubbles).forEach(function (id) { bubbles[id].hidden = true; });
        return;
      }

      var visibleIds = Object.create(null);
      visibleSpecs.forEach(function (spec) {
        if (!spec || !spec.id) return;
        visibleIds[spec.id] = true;
        var bubble = bubbles[spec.id];
        if (!bubble) {
          bubble = createBubble(spec);
          bubbles[spec.id] = bubble;
        }
        updateBubbleContent(spec, bubble);
        if (spec.defaultExpanded && bubble.dataset.defaultExpanded !== 'true') {
          collapseAll(spec.id, false);
          bubble.classList.add('is-expanded');
          bubble.setAttribute('aria-expanded', 'true');
          bubble.dataset.defaultExpanded = 'true';
        }
        positionBubble(spec, bubble);
      });
      Object.keys(bubbles).forEach(function (id) {
        if (visibleIds[id]) return;
        bubbles[id].hidden = true;
        bubbles[id].classList.remove('is-expanded');
        bubbles[id].setAttribute('aria-expanded', 'false');
      });
    }

    function scheduleUpdate() {
      if (updateHandle) window.cancelAnimationFrame(updateHandle);
      updateHandle = window.requestAnimationFrame(function () {
        updateHandle = null;
        currentTab = normalizeTabValue(currentTab || readActiveTab()) || 'home';
        renderTooltips();
      });
    }

    function setEnabled(value) {
      enabled = !!value;
      scheduleUpdate();
    }

    function bindToggle() {
      var toggle = document.getElementById('extended_tooltips');
      if (!toggle || toggle.dataset.extendedTooltipBound === 'true') return;
      toggle.dataset.extendedTooltipBound = 'true';
      setEnabled(toggle.checked);
      toggle.addEventListener('change', function () {
        setEnabled(toggle.checked);
      });
    }

    function register(newSpecs) {
      if (!Array.isArray(newSpecs)) newSpecs = [newSpecs];
      newSpecs.forEach(function (spec) {
        if (!spec || !spec.id) return;
        var existingIndex = specs.findIndex(function (item) { return item.id === spec.id; });
        if (existingIndex >= 0) specs[existingIndex] = spec;
        else specs.push(spec);
      });
      scheduleUpdate();
    }

    window.extendedTooltipOverlay = {
      register: register,
      setEnabled: setEnabled,
      update: scheduleUpdate,
      findCellSubsetsDropdown: findCellSubsetsDropdown,
      findCellSubsetsMenu: findCellSubsetsMenu,
      getDefaultTutorialSteps: getDefaultTutorialSteps,
      registerDefaults: registerDefaultTutorial
    };

    function getDefaultTutorialSteps() {
      return [
        {
          id: 'home-intro',
          page: 'home',
          placement: 'center',
          defaultExpanded: true,
          label: 'Tutorial overview',
          text: 'This tutorial will walk through the full database one step at a time. Use Next and Previous to navigate between tutorial steps, or End Tutorial to stop early.'
        },
        {
          id: 'home-theme',
          page: 'home',
          selector: '.theme-toggle',
          offsetX: -18,
          offsetY: 34,
          label: 'Theme toggle',
          text: 'Use this switch to move between the light and dark site themes. The setting is saved in this browser.'
        },
        {
          id: 'home-support',
          page: 'home',
          selector: '.home-hero',
          offsetX: -22,
          offsetY: 60,
          label: 'Support emails',
          text: 'The feedback emails separate code or database questions from manuscript and science questions, so your message reaches the right maintainer.'
        },
        {
          id: 'home-nav',
          page: 'home',
          selector: '#mainTabs',
          offsetX: 10,
          offsetY: 12,
          label: 'Navigation bar',
          text: 'Use the top navigation bar to move between Home, Interactive Data, Full Atlas, and Cell Subsets.'
        },
        {
          id: 'home-interactive-card',
          page: 'home',
          selector: '.home-card-link[data-target-tab=spermatogonia_table]',
          offsetX: -10,
          offsetY: 12,
          label: 'Next page',
          text: 'Click Next to be automatically taken to Interactive Data, starting with the Spermatogenesis Interactive Table.'
        },
        {
          id: 'spg-interactive-nav',
          page: 'spermatogonia_table',
          selector: '.interactive-data-stack .subset-secondary-nav',
          offsetX: 6,
          offsetY: 8,
          label: 'Interactive data pages',
          text: 'These links switch between the spermatogenesis table, retinoic acid plots, and cell-to-cell communication heatmaps.'
        },
        {
          id: 'spg-controls',
          page: 'spermatogonia_table',
          selector: '.interactive-data-stack .ra-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Spermatogenesis controls',
          text: 'Search for a gene and set the expression threshold to overlay average expression values onto the table.'
        },
        {
          id: 'spg-table',
          page: 'spermatogonia_table',
          selector: '#spermatogonia_container',
          offsetX: 6,
          offsetY: 10,
          label: 'Interactive table',
          text: 'Click any table cell to see the matching cell population and marker genes. A selected gene appears as a heatmap overlay across stages and cell types.'
        },
        {
          id: 'spg-download',
          page: 'spermatogonia_table',
          selector: '#spg_downloads_open',
          offsetX: 12,
          offsetY: -6,
          label: 'Download table',
          text: 'After selecting a gene, use Download Figures to export the table with the heatmap overlay as PNG or PDF.'
        },
        {
          id: 'spg-next-full-atlas',
          page: 'spermatogonia_table',
          selector: '#mainTabs a[data-value=sc3_main_figures]',
          offsetX: 8,
          offsetY: 12,
          label: 'Next page',
          text: 'Click Next to be automatically taken to Full Atlas, where the main UMAP figures and shared figure controls are introduced.'
        },
        {
          id: 'ra-analysis-note',
          page: 'retinoic_acid',
          selector: '.interactive-data-stack .ra-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'RA analysis',
          text: 'The retinoic acid page compares selected pathway genes as dot plots and stage trends, with the same download popup style as other figures.'
        },
        {
          id: 'ccc-analysis-note',
          page: 'cell2cell_heatmaps',
          selector: '.interactive-data-stack .ra-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Cell-to-cell heatmaps',
          text: 'The cell-to-cell page shows ligand-receptor communication scores across grouped stages. Select LR pairs, then export the customized heatmap.'
        },
        {
          id: 'atlas-secondary-nav',
          page: 'sc3_main_figures',
          selector: '.sc3-mainfig-stack .subset-secondary-nav',
          offsetX: 6,
          offsetY: 8,
          label: 'Full Atlas pages',
          text: 'These links switch between the customizable figure types for the Full Atlas dataset.'
        },
        {
          id: 'atlas-cell-selection',
          page: 'sc3_main_figures',
          selector: '.sc3-mainfig-stack .mainfig-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Cell selection',
          text: 'Choose which annotated cell groups are emphasized in the UMAPs, then adjust point size and labels before downloading.'
        },
        {
          id: 'atlas-umap-overview',
          page: 'sc3_main_figures',
          selector: '.sc3-mainfig-stack .mainfig-main',
          offsetX: 8,
          offsetY: 12,
          label: 'UMAP overview',
          text: 'This panel shows the full atlas in one UMAP colored by the selected cell annotation.'
        },
        {
          id: 'atlas-stage-split',
          page: 'sc3_main_figures',
          selector: '.sc3-mainfig-stack .mainfig-split',
          offsetX: 8,
          offsetY: 12,
          label: 'Stage split',
          text: 'The split UMAP repeats the same view across seminiferous tubule stages, making stage-specific shifts easier to compare.'
        },
        {
          id: 'atlas-explanation',
          page: 'sc3_main_figures',
          selector: '.sc3-mainfig-stack .figure-expl-wrap',
          offsetX: 8,
          offsetY: -6,
          label: 'Figure explanation',
          text: 'Most interactive pages include a Figure Explanation box near the bottom. Use it for a concise description of what the figure is showing.'
        },
        {
          id: 'atlas-next-cellinfo',
          page: 'sc3_main_figures',
          selector: '.sc3-mainfig-stack .subset-secondary-link[data-target-tab=sc3_cellinfo_gene]',
          offsetX: 8,
          offsetY: -6,
          label: 'Next page',
          text: 'Click Next to be automatically taken to CellInfo vs GeneExpr, where the shared axis, overlay, and advanced control pattern are introduced.'
        },
        {
          id: 'cellinfo-base-controls',
          page: 'sc3_cellinfo_gene',
          selector: '.sc3-cellinfo-gene-stack .legacy-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Base controls',
          text: 'Set the X and Y embedding axes here, then choose the cell information overlay and gene expression overlay to compare.'
        },
        {
          id: 'cellinfo-cell-box',
          page: 'sc3_cellinfo_gene',
          selector: '#sc3a1inp1',
          offsetX: 12,
          offsetY: -8,
          label: 'Cell information',
          text: 'Cell information fields color cells by metadata such as cell type, stage, sample, or quality metrics. Categorical and continuous values use different color behavior.'
        },
        {
          id: 'cellinfo-advanced',
          page: 'sc3_cellinfo_gene',
          selector: '.sc3-cellinfo-gene-stack .legacy-advanced',
          offsetX: 8,
          offsetY: 12,
          label: 'Advanced controls',
          text: 'Open Toggle Advanced Controls to reveal subsetting, point size, plot size, font size, ordering, labels, and other styling options.'
        },
        {
          id: 'cellinfo-plots',
          page: 'sc3_cellinfo_gene',
          selector: '.sc3-cellinfo-gene-stack .legacy-plots',
          offsetX: 8,
          offsetY: 12,
          label: 'Customized outputs',
          text: 'The plots update from the controls on the left. Use Download Figures from the controls card when the figure matches what you need.'
        },
        {
          id: 'cellinfo-next-multi',
          page: 'sc3_cellinfo_gene',
          selector: '.sc3-cellinfo-gene-stack .subset-secondary-link[data-target-tab=sc3_multiple_geneexpr]',
          offsetX: 8,
          offsetY: -6,
          label: 'Next page',
          text: 'Click Next to continue through the remaining Full Atlas tools: Multiple GeneExpr, Gene coexpression, Violinplot or Boxplot, Proportion plot, and Bubbleplot or Heatmap.'
        },
        {
          id: 'multi-geneexpr',
          page: 'sc3_multiple_geneexpr',
          selector: '.sc3-multi-gene-stack .legacy-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Multiple GeneExpr',
          text: 'Use this page to compare several genes across a selected cell grouping. Gene lists can be typed directly or uploaded.'
        },
        {
          id: 'gene-coexpression',
          page: 'sc3_gene_coexpression',
          selector: '.sc3-coexpression-stack .legacy-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Gene coexpression',
          text: 'Use this page to view two genes on the same embedding and inspect where their expression overlaps.'
        },
        {
          id: 'violin-proportion',
          pages: ['sc3_violin_boxplot', 'sc3_proportion_plot'],
          selector: '.legacy-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Distribution summaries',
          text: 'These pages summarize metadata or gene values across groups, either as distributions or as cell proportions and counts.'
        },
        {
          id: 'bubble-heatmap',
          page: 'sc3_bubble_heatmap',
          selector: '.sc3-bubble-stack .legacy-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Bubbleplot and heatmap',
          text: 'Paste or type a gene list, choose how cells are grouped, then switch between bubbleplot and heatmap summaries.'
        },
        {
          id: 'open-cell-subsets',
          page: 'sc3_bubble_heatmap',
          target: findCellSubsetsDropdown,
          offsetX: 8,
          offsetY: 12,
          label: 'Next section',
          text: 'Click Next to be automatically directed to the Cell Subsets dropdown, where the focused subset atlases are listed.'
        },
        {
          id: 'cell-subsets-menu',
          page: 'any',
          target: findCellSubsetsMenu,
          showWhen: function () { return !!findCellSubsetsMenu(); },
          offsetX: 8,
          offsetY: 12,
          label: 'Subset choices',
          text: 'Sertoli, Spermatogonia, Spermatocyte, and Spermatid are focused subsets from the full atlas. Each subset keeps the same customizable figure pages, but uses subset-specific cells.'
        },
        {
          id: 'subset-main-figures',
          page: 'cell_subsets',
          selector: '.mainfig-controls',
          offsetX: 8,
          offsetY: 12,
          label: 'Subset figures',
          text: 'Subset Main Figures work like the Full Atlas Main Figures, but the selected cell groups and plots are limited to the chosen subset.'
        },
        {
          id: 'subset-custom-pages',
          page: 'cell_subsets',
          selector: '.subset-secondary-nav',
          offsetX: 6,
          offsetY: 8,
          label: 'Subset tools',
          text: 'Use these links to open the same customizable figure types for this subset. Click Next when you are ready to return Home for sharing and release notes.'
        },
        {
          id: 'home-share-link',
          page: 'home',
          selector: '.hdr-copy-link',
          offsetX: 8,
          offsetY: -6,
          label: 'Share view',
          text: 'Use this link button to copy a URL that preserves the current tab and compatible figure settings, so the view can be reopened later.'
        },
        {
          id: 'home-patch-notes',
          page: 'home',
          selector: '.hdr-version-link',
          offsetX: 8,
          offsetY: -6,
          label: 'Patch notes',
          text: 'Patch Notes lists recent database and interface changes. Check it when behavior or available figures change between releases.'
        },
        {
          id: 'home-finish',
          page: 'home',
          selector: '#extended_tutorial_start',
          offsetX: 8,
          offsetY: -8,
          label: 'Finish tutorial',
          text: 'That is the end of the tutorial. Use End Tutorial or Finish to close this tour, and reopen it from this button whenever needed.'
        }
      ];
    }

    function registerDefaultTutorial() {
      register(getDefaultTutorialSteps());
    }

    window.defaultTutorialSteps = getDefaultTutorialSteps;

    document.addEventListener('click', function (event) {
      if (event.target.closest && event.target.closest('.extended-tooltip-bubble')) return;
      collapseAll();
    });

    document.addEventListener('keydown', function (event) {
      if (event.key === 'Escape') collapseAll();
    });

    document.addEventListener('change', function (event) {
      if (event.target && event.target.id === 'extended_tooltips') {
        setEnabled(event.target.checked);
      }
    });

    document.addEventListener('shiny:inputchanged', function (event) {
      if (!event) return;
      if (event.name === 'extended_tooltips') {
        setEnabled(event.value === true || event.value === 'true' || event.value === 1 || event.value === '1');
      }
      if (event.name === 'mainTabs') {
        currentTab = normalizeTabValue(event.value) || currentTab;
        scheduleUpdate();
      }
    });

    document.addEventListener('shown.bs.tab', function () {
      currentTab = readActiveTab();
      scheduleUpdate();
    });

    if (window.jQuery && window.jQuery.fn) {
      window.jQuery(document).on('shown.bs.tab', function () {
        currentTab = readActiveTab();
        scheduleUpdate();
      });
    }

    window.addEventListener('resize', scheduleUpdate);
    window.addEventListener('scroll', scheduleUpdate, true);

    function init() {
      ensureLayer();
      bindToggle();
      currentTab = readActiveTab();
      window.setTimeout(function () {
        bindToggle();
        currentTab = readActiveTab();
        scheduleUpdate();
      }, 250);
    }

    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', init);
    } else {
      init();
    }
  })();
")),
    # Step-by-step tutorial controller. This reuses the tooltip registry for
    # targets but controls its own modal-like tutorial card and navigation.
    tags$script(HTML("
  (function () {
    var layer = null;
    var card = null;
    var active = false;
    var currentIndex = 0;
    var renderToken = 0;
    var positionHandle = null;
    var cachedSteps = null;

    var stepOrder = [
      'home-intro',
      'home-theme',
      'home-support',
      'home-nav',
      'home-interactive-card',
      'spg-interactive-nav',
      'spg-controls',
      'spg-table',
      'spg-download',
      'ra-analysis-note',
      'ccc-analysis-note',
      'spg-next-full-atlas',
      'atlas-secondary-nav',
      'atlas-cell-selection',
      'atlas-umap-overview',
      'atlas-stage-split',
      'atlas-explanation',
      'atlas-next-cellinfo',
      'cellinfo-base-controls',
      'cellinfo-cell-box',
      'cellinfo-advanced',
      'cellinfo-plots',
      'cellinfo-next-multi',
      'multi-geneexpr',
      'gene-coexpression',
      'violin-proportion',
      'bubble-heatmap',
      'open-cell-subsets',
      'cell-subsets-menu',
      'subset-main-figures',
      'subset-custom-pages',
      'home-share-link',
      'home-patch-notes',
      'home-finish'
    ];

    var stepOverrides = {
      'home-intro': {
        text: 'This tutorial will walk through the full database one step at a time. Use Next and Previous to navigate between tutorial steps, or End Tutorial to stop early.'
      },
      'home-support': {
        text: 'If you have any questions or concerns, feel free to send an email to either of the feedback emails. If you notice a bug while using the database, please email adam.ward@wsu.edu with a screenshot of the bug in question (if possible) as well as an explanation of what lead to it, and we will do our best to ensure it gets fixed.'
      },
      'home-nav': {
        placement: 'below'
      },
      'spg-interactive-nav': {
        placement: 'below'
      },
      'spg-table': {
        selector: '.spg-table-title',
        placement: 'below'
      },
      'spg-next-full-atlas': {
        page: 'cell2cell_heatmaps',
        selector: '#mainTabs a[data-value=sc3_main_figures]',
        text: 'Click Next to be automatically taken to Full Atlas, where the main UMAP figures and shared figure controls are introduced.'
      },
      'atlas-secondary-nav': {
        placement: 'below'
      },
      'atlas-umap-overview': {
        selector: '.sc3-mainfig-stack .mainfig-main .ra-title',
        placement: 'right-start'
      },
      'atlas-stage-split': {
        selector: '.sc3-mainfig-stack .mainfig-split .ra-title',
        placement: 'right-start'
      },
      'atlas-explanation': {
        target: atlasFigureExplanationTarget,
        placement: 'below'
      },
      'atlas-next-cellinfo': {
        text: 'Click Next to be automatically taken to CellInfo vs GeneExpr, where the shared axis, overlay, and advanced control pattern are introduced.'
      },
      'cellinfo-cell-box': {
        selector: '#sc3a1inp1 + .selectize-control, #sc3a1inp1',
        placement: 'right-start'
      },
      'cellinfo-advanced': {
        selector: '.sc3-cellinfo-gene-stack .legacy-advanced .ra-advanced-toggle',
        placement: 'right-end'
      },
      'cellinfo-plots': {
        selector: '#sc3a1downloads_open',
        placement: 'right-start'
      },
      'cellinfo-next-multi': {
        text: 'Click Next to continue through the remaining Full Atlas tools: Multiple GeneExpr, Gene coexpression, Violinplot or Boxplot, Proportion plot, and Bubbleplot or Heatmap.'
      },
      'open-cell-subsets': {
        text: 'Click Next to be automatically directed to the Cell Subsets dropdown, where the focused subset atlases are listed.'
      },
      'subset-main-figures': {
        target: currentSubsetMainFiguresControls
      },
      'subset-custom-pages': {
        target: currentSubsetSecondaryNav,
        text: 'Use these links to open the same customizable figure types for this subset. Click Next when you are ready to return Home for sharing and release notes.'
      },
      'home-finish': {
        selector: '#extended_tutorial_start',
        text: 'That is the end of the tutorial. Use End Tutorial or Finish to close this tour, and reopen it from this button whenever needed.'
      }
    };

    function normalizeTabValue(value) {
      if (!value) return null;
      if (value === 'retinoic_acid_line') return 'retinoic_acid';
      return value;
    }

    function getTabValueFromAnchor(anchor) {
      if (!anchor) return null;
      return anchor.getAttribute('data-value') ||
        anchor.getAttribute('data-tab-value') ||
        (anchor.getAttribute('data-bs-target') || '').replace('#', '') ||
        (anchor.getAttribute('href') || '').replace('#', '');
    }

    function readActiveTab() {
      var activeAnchor = document.querySelector('#mainTabs li.active > a, #mainTabs a.active');
      return normalizeTabValue(getTabValueFromAnchor(activeAnchor)) || 'home';
    }

    function cloneStep(step) {
      var copy = {};
      Object.keys(step || {}).forEach(function (key) {
        copy[key] = step[key];
      });
      return copy;
    }

    function mergeStep(base, override) {
      var step = cloneStep(base);
      Object.keys(override || {}).forEach(function (key) {
        step[key] = override[key];
      });
      return step;
    }

    function getTutorialSteps() {
      if (cachedSteps) return cachedSteps;
      var baseSteps = [];
      if (typeof window.defaultTutorialSteps === 'function') {
        baseSteps = window.defaultTutorialSteps();
      }
      var byId = Object.create(null);
      baseSteps.forEach(function (step) {
        if (step && step.id) byId[step.id] = step;
      });
      cachedSteps = stepOrder.map(function (id) {
        return mergeStep(byId[id] || { id: id }, stepOverrides[id]);
      }).filter(function (step) {
        return !!(step && step.id && step.text);
      });
      return cachedSteps;
    }

    function isVisibleElement(el) {
      if (!el || !el.getBoundingClientRect) return false;
      var rect = el.getBoundingClientRect();
      if (!rect || (rect.width <= 0 && rect.height <= 0)) return false;
      var style = window.getComputedStyle ? window.getComputedStyle(el) : null;
      return !(style && (style.display === 'none' || style.visibility === 'hidden' || style.opacity === '0'));
    }

    function activePane() {
      return document.querySelector('.tab-pane.active') || document.body || document;
    }

    function firstVisible(selector, root) {
      if (!selector) return null;
      root = root || document;
      var nodes = root.querySelectorAll ? root.querySelectorAll(selector) : [];
      for (var i = 0; i < nodes.length; i++) {
        if (isVisibleElement(nodes[i])) return nodes[i];
      }
      return null;
    }

    function activeTarget(selector) {
      return firstVisible(selector, activePane()) || firstVisible(selector, document);
    }

    function activePaneOnlyTarget(selector) {
      return firstVisible(selector, activePane());
    }

    function atlasFigureExplanationTarget() {
      return activeTarget('.sc3-mainfig-stack .figure-expl-details > summary');
    }

    function currentSubsetMainFiguresControls() {
      return activePaneOnlyTarget('.mainfig-controls');
    }

    function currentSubsetSecondaryNav() {
      return activePaneOnlyTarget('.subset-secondary-nav');
    }

    function findCellSubsetsDropdown() {
      if (window.extendedTooltipOverlay && typeof window.extendedTooltipOverlay.findCellSubsetsDropdown === 'function') {
        return window.extendedTooltipOverlay.findCellSubsetsDropdown();
      }
      return null;
    }

    function findCellSubsetsMenu() {
      if (window.extendedTooltipOverlay && typeof window.extendedTooltipOverlay.findCellSubsetsMenu === 'function') {
        return window.extendedTooltipOverlay.findCellSubsetsMenu();
      }
      return null;
    }

    function openCellSubsetsDropdown() {
      var anchor = findCellSubsetsDropdown();
      if (!anchor || !anchor.closest) return;
      var dropdown = anchor.closest('.dropdown');
      var menu = dropdown ? dropdown.querySelector('.dropdown-menu') : null;
      anchor.setAttribute('aria-expanded', 'true');
      if (dropdown) {
        dropdown.classList.add('open');
        dropdown.classList.add('show');
      }
      if (menu) {
        menu.classList.add('show');
        menu.style.display = 'block';
      }
    }

    function closeCellSubsetsDropdown() {
      if (window.closeAllNavbarDropdowns) {
        window.closeAllNavbarDropdowns('tutorial');
      }
      var anchor = findCellSubsetsDropdown();
      var dropdown = anchor && anchor.closest ? anchor.closest('.dropdown') : null;
      var menu = dropdown ? dropdown.querySelector('.dropdown-menu') : null;
      if (anchor) anchor.setAttribute('aria-expanded', 'false');
      if (dropdown) {
        dropdown.classList.remove('open');
        dropdown.classList.remove('show');
      }
      if (menu) {
        menu.classList.remove('show');
        menu.style.display = '';
      }
      Array.prototype.forEach.call(document.querySelectorAll('.dropdown-backdrop'), function (backdrop) {
        if (backdrop && backdrop.parentNode) backdrop.parentNode.removeChild(backdrop);
      });
    }

    function cellinfoAdvancedBody() {
      return activeTarget('.sc3-cellinfo-gene-stack .legacy-advanced-body');
    }

    function cellinfoAdvancedToggle() {
      return activeTarget('.sc3-cellinfo-gene-stack .legacy-advanced .ra-advanced-toggle');
    }

    function isCellinfoAdvancedOpen() {
      return isVisibleElement(cellinfoAdvancedBody());
    }

    function setCellinfoAdvancedOpen(shouldOpen) {
      var toggle = cellinfoAdvancedToggle();
      if (!toggle) return 0;
      if (isCellinfoAdvancedOpen() === !!shouldOpen) return 0;
      if (toggle.click) toggle.click();
      return 180;
    }

    function prepareTutorialStep(step) {
      if (!step) return 0;
      if (step.id === 'cellinfo-advanced') {
        return setCellinfoAdvancedOpen(true);
      }
      return 0;
    }

    function cleanupTutorialStep(step) {
      var delay = 0;
      if (!step) return delay;
      if (step.id === 'cellinfo-advanced') {
        delay = Math.max(delay, setCellinfoAdvancedOpen(false));
      }
      if (step.id === 'cell-subsets-menu' || step.id === 'open-cell-subsets') {
        closeCellSubsetsDropdown();
      }
      return delay;
    }

    function getTarget(step) {
      if (!step || step.placement === 'center') return null;
      if (typeof step.target === 'function') return step.target();
      if (step.selector) return activeTarget(step.selector);
      return null;
    }

    function desiredPageForStep(step) {
      if (!step) return null;
      var page = step.page || step.pages;
      if (Array.isArray(page)) page = page[0];
      if (typeof page === 'function' || !page || page === 'any') return null;
      if (page === 'cell_subsets') {
        var current = readActiveTab();
        return /^sc[4-7]_/.test(current) ? current : 'sc4_main_figures';
      }
      return normalizeTabValue(page);
    }

    function navigateToTab(tabValue) {
      var target = normalizeTabValue(tabValue);
      if (!target || readActiveTab() === target) return;
      closeCellSubsetsDropdown();
      if (window.navToTab && window.navToTab(target, null)) return;
      if (window.Shiny && typeof Shiny.setInputValue === 'function') {
        Shiny.setInputValue('home_card_nav', target, { priority: 'event' });
        return;
      }
      var anchor = document.querySelector('#mainTabs a[data-value=' + target + '], #mainTabs a[href=\"#' + target + '\"]');
      if (anchor && anchor.click) anchor.click();
    }

    function ensureLayer() {
      if (layer && card) return layer;
      layer = document.createElement('div');
      layer.id = 'tutorial-step-layer';
      layer.className = 'tutorial-step-layer';
      layer.setAttribute('aria-hidden', 'true');

      card = document.createElement('section');
      card.className = 'tutorial-step-card';
      card.setAttribute('role', 'dialog');
      card.setAttribute('aria-live', 'polite');
      card.setAttribute('aria-label', 'Extended tutorial');
      card.innerHTML = [
        '<button type=\"button\" class=\"tutorial-step-end\" data-tutorial-action=\"end\">End Tutorial</button>',
        '<div class=\"tutorial-step-kicker\"></div>',
        '<h3 class=\"tutorial-step-title\"></h3>',
        '<p class=\"tutorial-step-text\"></p>',
        '<div class=\"tutorial-step-controls\">',
        '<button type=\"button\" class=\"tutorial-step-btn\" data-tutorial-action=\"prev\">Previous</button>',
        '<span class=\"tutorial-step-count\"></span>',
        '<button type=\"button\" class=\"tutorial-step-btn tutorial-step-btn-primary\" data-tutorial-action=\"next\">Next</button>',
        '</div>'
      ].join('');

      card.addEventListener('click', function (event) {
        var control = event.target.closest && event.target.closest('[data-tutorial-action]');
        if (!control) return;
        event.preventDefault();
        event.stopPropagation();
        var action = control.getAttribute('data-tutorial-action');
        if (action === 'prev') previousStep();
        if (action === 'next') nextStep();
        if (action === 'end') endTutorial();
      });

      layer.appendChild(card);
      document.body.appendChild(layer);
      return layer;
    }

    function renderCardContent(step, total) {
      ensureLayer();
      layer.classList.add('is-visible');
      layer.setAttribute('aria-hidden', 'false');
      card.hidden = false;
      card.querySelector('.tutorial-step-kicker').textContent = 'Extended Tutorial';
      card.querySelector('.tutorial-step-title').textContent = step.label || 'Tutorial Step';
      card.querySelector('.tutorial-step-text').textContent = step.text || '';
      card.querySelector('.tutorial-step-count').textContent = (currentIndex + 1) + '/' + total;
      var prev = card.querySelector('[data-tutorial-action=prev]');
      var next = card.querySelector('[data-tutorial-action=next]');
      prev.disabled = currentIndex === 0;
      next.textContent = currentIndex >= total - 1 ? 'Finish' : 'Next';
    }

    function clamp(value, min, max) {
      return Math.max(min, Math.min(value, max));
    }

    function updateLayerBounds() {
      ensureLayer();
      var doc = document.documentElement;
      var body = document.body || {};
      var height = Math.max(
        doc.scrollHeight || 0,
        body.scrollHeight || 0,
        doc.clientHeight || 0,
        window.innerHeight || 0
      );
      layer.style.height = height + 'px';
    }

    function getViewport() {
      return {
        left: window.pageXOffset || docScrollLeft(),
        top: window.pageYOffset || docScrollTop(),
        width: document.documentElement.clientWidth || window.innerWidth || 0,
        height: document.documentElement.clientHeight || window.innerHeight || 0
      };
    }

    function docScrollLeft() {
      return document.documentElement.scrollLeft || document.body.scrollLeft || 0;
    }

    function docScrollTop() {
      return document.documentElement.scrollTop || document.body.scrollTop || 0;
    }

    function documentRect(el) {
      var rect = el.getBoundingClientRect();
      var viewport = getViewport();
      return {
        left: rect.left + viewport.left,
        right: rect.right + viewport.left,
        top: rect.top + viewport.top,
        bottom: rect.bottom + viewport.top,
        width: rect.width,
        height: rect.height
      };
    }

    function cardDocumentRect(left, top) {
      var rect = card.getBoundingClientRect();
      return {
        left: left,
        right: left + rect.width,
        top: top,
        bottom: top + rect.height,
        width: rect.width,
        height: rect.height
      };
    }

    function clampToViewport(left, top) {
      var viewport = getViewport();
      var rect = card.getBoundingClientRect();
      var margin = 14;
      return {
        left: clamp(left, viewport.left + margin, Math.max(viewport.left + margin, viewport.left + viewport.width - rect.width - margin)),
        top: Math.max(margin, top)
      };
    }

    function setCardPosition(left, top, pointer) {
      updateLayerBounds();
      card.dataset.pointer = pointer || 'top-left';
      card.style.left = Math.round(left) + 'px';
      card.style.top = Math.round(top) + 'px';
      var rect = cardDocumentRect(left, top);
      var currentHeight = parseFloat(layer.style.height) || 0;
      layer.style.height = Math.max(currentHeight, rect.bottom + 32) + 'px';
      return rect;
    }

    function positionCenter() {
      ensureLayer();
      updateLayerBounds();
      card.classList.add('is-center');
      card.dataset.pointer = 'none';
      var viewport = getViewport();
      var rect = card.getBoundingClientRect();
      var left = viewport.left + Math.round((viewport.width - rect.width) / 2);
      var top = viewport.top + Math.round((viewport.height - rect.height) / 2);
      return setCardPosition(left, top, 'none');
    }

    function positionNearTarget(target, step) {
      if (!isVisibleElement(target)) {
        return { cardRect: positionCenter(), targetRect: null };
      }
      card.classList.remove('is-center');
      card.style.left = '0px';
      card.style.top = '0px';
      var margin = 14;
      var gap = 18;
      var viewport = getViewport();
      var targetRect = target.getBoundingClientRect();
      var targetDoc = documentRect(target);
      var cardRect = card.getBoundingClientRect();
      var placement = step && step.placement ? step.placement : 'auto';
      var left = targetDoc.right + gap;
      var top = targetDoc.top;
      var pointer = 'top-left';

      if (placement === 'below') {
        left = targetDoc.left;
        top = targetDoc.bottom + gap;
        pointer = 'top';
      } else if (placement === 'right-start') {
        left = targetDoc.right + gap;
        top = targetDoc.top;
        pointer = 'top-left';
      } else if (placement === 'right-end') {
        left = targetDoc.right + gap;
        top = targetDoc.bottom - cardRect.height;
        pointer = 'bottom-left';
      } else if (viewport.width - targetRect.right >= cardRect.width + gap + margin) {
        left = targetDoc.right + gap;
        top = targetDoc.top;
        pointer = 'top-left';
      } else if (targetRect.left >= cardRect.width + gap + margin) {
        left = targetDoc.left - cardRect.width - gap;
        top = targetDoc.top;
        pointer = 'top-right';
      } else if (viewport.height - targetRect.bottom >= cardRect.height + gap + margin) {
        left = targetDoc.left;
        top = targetDoc.bottom + gap;
        pointer = 'top';
      } else {
        left = targetDoc.left;
        top = targetDoc.top - cardRect.height - gap;
        pointer = 'bottom-left';
      }

      var clamped = clampToViewport(left, top);
      return {
        cardRect: setCardPosition(clamped.left, clamped.top, pointer),
        targetRect: targetDoc
      };
    }

    function scrollCardIntoView(cardRect) {
      if (!cardRect) return false;
      var viewport = getViewport();
      var margin = 16;
      var viewTop = viewport.top;
      var viewBottom = viewport.top + viewport.height;
      var topLimit = viewTop + margin;
      var bottomLimit = viewBottom - margin;
      var nextTop = viewTop;

      if (cardRect.top >= topLimit && cardRect.bottom <= bottomLimit) {
        return false;
      }

      if (cardRect.height > viewport.height - margin * 2) {
        nextTop = cardRect.top - margin;
      } else if (cardRect.top < topLimit) {
        nextTop = cardRect.top - margin;
      } else if (cardRect.bottom > bottomLimit) {
        nextTop = cardRect.bottom + margin - viewport.height;
      }

      nextTop = Math.max(0, Math.round(nextTop));
      if (Math.abs(nextTop - viewTop) < 2) return false;
      window.scrollTo(viewport.left, nextTop);
      return true;
    }

    function positionAndScrollStep(step, target) {
      var positionInfo;
      if (step.placement === 'center') {
        positionInfo = { cardRect: positionCenter(), targetRect: null };
      } else {
        positionInfo = positionNearTarget(target, step);
      }
      scrollCardIntoView(positionInfo.cardRect);
    }

    function positionAtActivePane(step) {
      var pane = activePane();
      if (isVisibleElement(pane)) {
        positionAndScrollStep(mergeStep(step || {}, { placement: 'below' }), pane);
        return;
      }
      positionCenter();
    }

    function activePageMatches(step, desiredPage) {
      if (!desiredPage) return true;
      var current = readActiveTab();
      if (step && step.page === 'cell_subsets') return current === desiredPage;
      return current === desiredPage;
    }

    function waitAndPosition(step, token, attempt, scrolled) {
      if (!active || token !== renderToken) return;
      attempt = attempt || 0;
      var desiredPage = desiredPageForStep(step);
      if (!activePageMatches(step, desiredPage) && attempt < 50) {
        window.setTimeout(function () {
          waitAndPosition(step, token, attempt + 1, scrolled);
        }, 100);
        return;
      }

      if (step.id === 'cell-subsets-menu') openCellSubsetsDropdown();
      var target = getTarget(step);
      if (step.placement === 'center') {
        positionCenter();
        return;
      }
      if (!target && attempt < 50) {
        window.setTimeout(function () {
          waitAndPosition(step, token, attempt + 1, scrolled);
        }, 100);
        return;
      }
      if (!target) {
        positionAtActivePane(step);
        return;
      }
      positionAndScrollStep(step, target);
    }

    function showStep(index) {
      var steps = getTutorialSteps();
      if (!steps.length) return;
      var previousStep = active ? steps[currentIndex] : null;
      var cleanupDelay = cleanupTutorialStep(previousStep);
      active = true;
      currentIndex = clamp(index, 0, steps.length - 1);
      if (window.extendedTooltipOverlay && typeof window.extendedTooltipOverlay.setEnabled === 'function') {
        window.extendedTooltipOverlay.setEnabled(false);
      }
      var step = steps[currentIndex];
      var token = ++renderToken;
      window.setTimeout(function () {
        if (!active || token !== renderToken) return;
        renderCardContent(step, steps.length);
        if (step.id !== 'cell-subsets-menu') closeCellSubsetsDropdown();
        navigateToTab(desiredPageForStep(step));
        var prepDelay = prepareTutorialStep(step);
        window.setTimeout(function () {
          waitAndPosition(step, token, 0, false);
        }, prepDelay);
      }, cleanupDelay);
    }

    function startTutorial() {
      cachedSteps = null;
      showStep(0);
    }

    function previousStep() {
      if (!active || currentIndex <= 0) return;
      showStep(currentIndex - 1);
    }

    function nextStep() {
      var steps = getTutorialSteps();
      if (!active || !steps.length) return;
      if (currentIndex >= steps.length - 1) {
        endTutorial();
        return;
      }
      showStep(currentIndex + 1);
    }

    function endTutorial() {
      var steps = getTutorialSteps();
      cleanupTutorialStep(steps[currentIndex]);
      active = false;
      renderToken++;
      closeCellSubsetsDropdown();
      if (layer) {
        layer.classList.remove('is-visible');
        layer.setAttribute('aria-hidden', 'true');
      }
      if (card) card.hidden = true;
    }

    function schedulePositionUpdate() {
      if (!active) return;
      if (positionHandle) window.cancelAnimationFrame(positionHandle);
      positionHandle = window.requestAnimationFrame(function () {
        positionHandle = null;
        var steps = getTutorialSteps();
        var step = steps[currentIndex];
        if (!step) return;
        var target = getTarget(step);
        if (step.placement === 'center' || !target) positionCenter();
        else positionNearTarget(target, step);
      });
    }

    document.addEventListener('click', function (event) {
      var startButton = event.target.closest && event.target.closest('#extended_tutorial_start');
      if (!startButton) return;
      event.preventDefault();
      startTutorial();
    });

    document.addEventListener('keydown', function (event) {
      if (!active) return;
      if (event.key === 'Escape') endTutorial();
      if (event.key === 'ArrowRight') nextStep();
      if (event.key === 'ArrowLeft') previousStep();
    });

    document.addEventListener('shown.bs.tab', schedulePositionUpdate);
    document.addEventListener('shiny:inputchanged', function (event) {
      if (event && event.name === 'mainTabs') schedulePositionUpdate();
    });
    if (window.jQuery && window.jQuery.fn) {
      window.jQuery(document).on('shown.bs.tab', schedulePositionUpdate);
    }
    window.addEventListener('resize', schedulePositionUpdate);

    window.tutorialTour = {
      start: startTutorial,
      end: endTutorial,
      next: nextStep,
      previous: previousStep
    };
  })();
")),
    br(),
    # Persistent app header with share-link control, release link, theme toggle,
    # and lab logo.
    tags$div(
      class = "hdr-wrap",
      # Left: title + subtitle
      tags$div(
        class = "hdr-text",
        tags$div(
          class = "hdr-title-wrap",
          tags$h1(
            class = "hdr-title",
            "Single Nuclei Analysis of Staged Seminiferous Tubules"
          ),
          actionLink(
            inputId = "copy_view_link",
            label = NULL,
            icon = icon("link"),
            class = "hdr-copy-link",
            title = "Copy a sharable URL for this view"
          )
        ),
        tags$p(
          class = "hdr-subtitle",
          "An Interactive Dataset for Exploring Spermatogenesis"
        ),
        tags$p(
          class = "hdr-version",
          current_release_label, " - ",
          tags$a(
            href = "#patch_notes",
            class = "hdr-version-link",
            onclick = "return window.navToTab('patch_notes', this);",
            "(View Patch Notes)"
          )
        )
      ),
      # Right: clickable logo button
      tags$div(
        class = "hdr-actions",
        tags$div(
          class = "theme-toggle",
          tags$input(
            id = "theme-toggle",
            type = "checkbox",
            class = "theme-toggle-input",
            `aria-label` = "Toggle dark mode"
          ),
          tags$label(
            `for` = "theme-toggle",
            class = "theme-toggle-label",
            role = "switch",
            `aria-hidden` = "true",
            tags$span(
              class = "theme-toggle-icon",
              icon("sun")
            ),
            tags$span(
              class = "theme-toggle-track",
              tags$span(class = "theme-toggle-thumb")
            ),
            tags$span(
              class = "theme-toggle-icon",
              icon("moon")
            )
          ),
        ),
        tags$a(
          class = "hdr-logo",
          href = "https://weiyanlab.com",
          target = "_blank",
          `aria-label` = "Wei Yan Lab website (opens in a new tab)",
          tags$img(
            src = "yanlablogo.png",
            alt = "Wei Yan Lab Logo"
          )
        )
      )
    ),
    # Main navbar. Many tabs are intentionally registered but hidden by CSS/JS so
    # hash routing and the lazy server binders can address them directly.
    navbarPage(
      "",
      id = "mainTabs", # add an id so we can reference tabs
      header = tagList(
        # Keep navbar menus closed after internal navigation, synchronize URL
        # hashes with tab state, and resize Plotly widgets after tab switches.
        tags$script(HTML("
    (function bootstrapNavAutoClose() {
      function waitFor(condition, callback, interval) {
        interval = interval || 75;
        if (condition()) {
          callback();
        } else {
          setTimeout(function() { waitFor(condition, callback, interval); }, interval);
        }
      }
      window.navToTab = function(target, el) {
        if (el && el.blur) { el.blur(); }
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue('home_card_nav', target, {priority: 'event'});
        }
        if (window.closeAllNavbarDropdowns) {
          window.closeAllNavbarDropdowns('home-card');
          window.setTimeout(function() { window.closeAllNavbarDropdowns('home-card'); }, 200);
        }
        return false;
      };
      waitFor(function() { return !!window.jQuery; }, function() {
        var $ = window.jQuery;
	          function closeAllDropdowns(source) {
	            var $dropdowns = $('.navbar .dropdown');
	            if (!$dropdowns.length) { return; }
	            $dropdowns.each(function() {
	              var $dropdown = $(this);
	              var $menu = $dropdown.find('.dropdown-menu');
              $dropdown.removeClass('open show');
              $menu.removeClass('show');
              $dropdown
                .find('> a[data-toggle=\"dropdown\"], > a[data-bs-toggle=\"dropdown\"]')
                .attr('aria-expanded', 'false')
                .blur();
            });
            if (window.bootstrap && window.bootstrap.Dropdown) {
              $('.navbar .dropdown-toggle').each(function() {
                var inst = window.bootstrap.Dropdown.getInstance(this);
                if (inst) { inst.hide(); }
              });
            }
	            $('.dropdown-backdrop').remove();
	          }
	          function markInternalNavItems() {
	            var selectors = [
	              '#mainTabs > li > a[data-value^=\"sc3_\"]:not([data-value$=\"_main_figures\"])',
	              '#mainTabs > li > a[data-value=\"retinoic_acid\"]',
	              '#mainTabs > li > a[data-value=\"cell2cell_heatmaps\"]',
	              '#mainTabs > li > a[data-value=\"patch_notes\"]',
	              '#mainTabs > li > a[href=\"#patch_notes\"]',
	              '#mainTabs .dropdown-menu > li > a[data-value^=\"sc4_\"]:not([data-value$=\"_main_figures\"])',
	              '#mainTabs .dropdown-menu > li > a[data-value^=\"sc5_\"]:not([data-value$=\"_main_figures\"])',
	              '#mainTabs .dropdown-menu > li > a[data-value^=\"sc6_\"]:not([data-value$=\"_main_figures\"])',
	              '#mainTabs .dropdown-menu > li > a[data-value^=\"sc7_\"]:not([data-value$=\"_main_figures\"])'
	            ];
	            selectors.forEach(function(selector) {
	              Array.prototype.forEach.call(document.querySelectorAll(selector), function(anchor) {
	                var item = anchor.closest && anchor.closest('li');
	                if (item) item.classList.add('nav-hidden-by-app');
	              });
	            });
	          }
	          function resizePlotlyActive() {
	            if (!(window.Plotly && window.Plotly.Plots && typeof window.Plotly.Plots.resize === 'function')) {
	              return;
	            }
	            function resizePlotly(el) {
	              if (!el) return;
	              try { window.Plotly.Plots.resize(el); } catch (err) { /* ignore */ }
	              if (typeof window.Plotly.relayout === 'function') {
	                try { window.Plotly.relayout(el, {autosize: true}); } catch (err2) { /* ignore */ }
	              }
	            }
	            var pane = document.querySelector('.tab-pane.active');
	            if (!pane) { return; }
	            var ccc = document.getElementById('ccc_heatmap');
	            if (ccc && pane.contains(ccc)) {
	              resizePlotly(ccc);
	            }
	            var nodes = pane.querySelectorAll('.plotly.html-widget');
	            Array.prototype.forEach.call(nodes, function(el) {
	              resizePlotly(el);
	            });
	          }
	          function schedulePlotlyResizes() {
	            window.setTimeout(resizePlotlyActive, 120);
	            window.setTimeout(resizePlotlyActive, 360);
	            window.setTimeout(resizePlotlyActive, 900);
	            window.setTimeout(resizePlotlyActive, 1800);
	          }
	          var suppressHistory = false;
	          function getTabValueFromAnchor(anchor) {
	            if (!anchor) return null;
	            return anchor.getAttribute('data-value') ||
	              anchor.getAttribute('data-tab-value') ||
	              (anchor.getAttribute('data-bs-target') || '').replace('#','') ||
	              (anchor.getAttribute('href') || '').replace('#','');
	          }
	          function normalizeTabValue(val) {
	            if (!val) return false;
	            if (val === 'retinoic_acid_line') { val = 'retinoic_acid'; }
	            return val;
	          }
	          function selectTabByValue(val, options) {
	            var nextVal = normalizeTabValue(val);
	            if (!nextVal) return false;
	            options = options || {};
	            if (window.Shiny && typeof Shiny.setInputValue === 'function') {
	              suppressHistory = !!options.suppressHistory;
	              Shiny.setInputValue('home_card_nav', nextVal, {priority: 'event'});
	              return true;
	            }
	            return false;
	          }
	          function pushTabHistory(val, replace) {
	            if (!val) return;
	            if (window.history && window.history.replaceState && window.history.pushState) {
	              var publicPath = (window.location.pathname || '/')
	                .replace(/\\/(_w_[^/]+)/g, '')
	                .replace(/\\/+/g, '/');
	              if (!publicPath) {
	                publicPath = '/';
	              }
	              var url = publicPath + (window.location.search || '') + '#' + val;
	              if (replace) {
	                window.history.replaceState({tab: val}, '', url);
	              } else {
	                window.history.pushState({tab: val}, '', url);
	              }
	            } else {
	              window.location.hash = val;
	            }
	          }
	          window.addEventListener('popstate', function(evt) {
	            var stateTab = evt && evt.state ? evt.state.tab : null;
	            var hashTab = (window.location.hash || '').replace('#','');
	            var target = stateTab || hashTab;
	            if (target) {
	              selectTabByValue(target, {suppressHistory: true});
	            }
	          });
	          markInternalNavItems();
	          waitFor(
	            function() { return window.Shiny && typeof Shiny.setInputValue === 'function'; },
	            function() {
	              window.setTimeout(function() {
	                markInternalNavItems();
	                var requestedHash = normalizeTabValue((window.location.hash || '').replace('#',''));
	                if (requestedHash && selectTabByValue(requestedHash, {suppressHistory: true})) {
	                  pushTabHistory(requestedHash, true);
	                  return;
	                }
	                var active = document.querySelector('#mainTabs li.active a');
	                var activeVal = normalizeTabValue(getTabValueFromAnchor(active));
	                if (activeVal) {
	                  pushTabHistory(activeVal, true);
	                }
	              }, 0);
	            }
	          );
	          $(document).on('shown.bs.tab', '#mainTabs a[data-toggle=\"tab\"], #mainTabs a[data-bs-toggle=\"tab\"], #mainTabs a[role=\"tab\"]', function() {
	            markInternalNavItems();
	            var tabVal = getTabValueFromAnchor(this);
	            if (suppressHistory) {
	              suppressHistory = false;
	            } else {
	              pushTabHistory(tabVal, false);
	            }
	            window.setTimeout(function() { closeAllDropdowns('shown'); }, 160);
	            schedulePlotlyResizes();
	          });
	          $(document).on('shiny:inputchanged', function(event) {
	            if (!event || event.name !== 'mainTabs') return;
	            markInternalNavItems();
	            window.setTimeout(function() { closeAllDropdowns('mainTabs'); }, 160);
	            schedulePlotlyResizes();
	          });
	          waitFor(function() { return !!document.getElementById('theme-toggle'); }, function() {
	            var toggle = document.getElementById('theme-toggle');
	            if (toggle) {
	              toggle.addEventListener('change', schedulePlotlyResizes);
	            }
	          });
	          $(window).on('resize', function() {
	            window.setTimeout(resizePlotlyActive, 60);
	          });
	          waitFor(
	            function() { return window.Shiny && window.Shiny.addCustomMessageHandler; },
	            function() {
	              Shiny.addCustomMessageHandler('close-nav-dropdown', function(payload) {
	                var delay = (payload && payload.delay) ? payload.delay : 160;
	                window.setTimeout(function() { closeAllDropdowns('message'); }, delay);
	              });
	            }
	          );
          window.closeAllNavbarDropdowns = function() { closeAllDropdowns('direct'); };
        });
      })();
    ")),
        # tags$script(src = "upload.js")
      ),
      footer = tagList(
        br(),
        p(
          strong("Reference: "),
          "Hayden McSwiggin, ",
          "Single Nuclei Analysis of Staged Seminiferous Tubules (Unpublished, expected mid 2026)",
          style = "font-size: 125%;"
        ),
        p(
          em("This webpage was made using "),
          a("ShinyCell", href = "https://github.com/SGDDNB/ShinyCell", target = "_blank"),
          em(" — By Adam Tomasz Ward "),
          a("(GitHub)", href = "https://github.com/Bioticcc", target = "_blank")
        ),
        br(), br(), br(), br(), br()
      ),
      selected = "home", # make Home the default landing page

      # Home page and launch cards for the major app workflows.
      tabPanel(
        title = tagList(icon("house"), "Home"),
        value = "home",

        # ===== Row 1: Welcome (left) + Get Started (right) =====
        fluidRow(
          class = "home-top-row",
          column(
            width = 8,
            div(
              class = "home-card home-hero",
              h2("Welcome"),
              p(
                "Explore gene expression throughout mouse spermatogenesis with an intuitive, interactive interface. ",
                "Query your genes of interest, visualize stage-specific expression patterns, and export publication-ready figures - no coding required! ",
                "The current release features our full stage-resolved transcriptomic atlas of seminiferous tubules, with developmental time-course, spatial transcriptomic datasets, and additional modalities coming soon."
              ),
              p(tags$strong("Feedback:")),
              tags$ul(
                tags$li("adam.ward@wsu.edu for database development/code related questions"),
                tags$li("hayden.mcswiggin@wsu.edu for database/paper science related questions")
              )
            )
          ),
          column(
            width = 4,
            div(
              class = "home-card get-started",
              h3("Get Started"),
              tags$ol(
                tags$li("Open the Full Atlas or Interactive Data tabs from the navigation bar."),
                tags$li("Pick a figure type and set filters (genes, stages, cell groups)."),
                tags$li("Customize aesthetics and download your figure.")
              ),
              p(em("You can return here anytime from the “Home” tab.")),
              tags$div(
                class = "extended-tutorial-start",
                actionButton(
                  inputId = "extended_tutorial_start",
                  label = "Extended Tutorial",
                  class = "btn btn-primary tutorial-start-button"
                )
              )
            )
          )
        ),

        # ===== Row 2: Full-width Custom Interactive Figures =====
        fluidRow(
          column(
            width = 12,
            div(
              class = "home-card",
              h3("Explore the Atlas: Featured Interactive Analyses"),
              p("These interactive views reproduce and extend figures from the paper using our pre-loaded Seurat objects:"),
              tags$div(
                class = "row home-icon-grid",
                column(
                  width = 4,
                  tags$a(
                    class = "home-card-link",
                    href = "#",
                    role = "button",
                    tabindex = "0",
                    `data-target-tab` = "spermatogonia_table",
                    onclick = "return window.navToTab(this.getAttribute('data-target-tab'), this);",
                    div(
                      class = "home-card",
                      tags$img(
                        src = "interactiveTable_preview.png",
                        class = "home-card-preview home-preview-light",
                        loading = "lazy",
                        alt = "Preview of the spermatogenesis interactive table"
                      ),
                      tags$img(
                        src = "interactiveTable_preview_dark.png",
                        class = "home-card-preview home-preview-dark",
                        loading = "lazy",
                        alt = "Preview of the spermatogenesis interactive table"
                      ),
                      h4("Spermatogenesis Interactive Table"),
                      p("Browse all spermatogenic cell types and their stage-specific marker genes, or enter a gene of interest to visualize its expression across the full spermatogenesis landscape as an interactive heatmap.")
                    )
                  )
                ),
                column(
                  width = 4,
                  tags$a(
                    class = "home-card-link",
                    href = "#",
                    role = "button",
                    tabindex = "0",
                    `data-target-tab` = "retinoic_acid",
                    onclick = "return window.navToTab(this.getAttribute('data-target-tab'), this);",
                    div(
                      class = "home-card",
                      tags$img(
                        src = "ra_preview_publication_icon.png",
                        class = "home-card-preview home-preview-light",
                        loading = "lazy",
                        alt = "Preview of the RA line plot figure"
                      ),
                      tags$img(
                        src = "ra_preview_publication_icon_dark.png",
                        class = "home-card-preview home-preview-dark",
                        loading = "lazy",
                        alt = "Preview of the RA line plot figure"
                      ),
                      h4("Retinoic Acid Analysis"),
                      p("Investigate how retinoic acid signaling genes are expressed across spermatogenic cell types and stages.")
                    )
                  )
                ),
                column(
                  width = 4,
                  tags$a(
                    class = "home-card-link",
                    href = "#",
                    role = "button",
                    tabindex = "0",
                    `data-target-tab` = "cell2cell_heatmaps",
                    onclick = "return window.navToTab(this.getAttribute('data-target-tab'), this);",
                    div(
                      class = "home-card",
                      tags$img(
                        src = "ccc_heatmap_preview.png",
                        class = "home-card-preview home-preview-light",
                        loading = "lazy",
                        alt = "Preview of the cell-to-cell communication heatmap"
                      ),
                      tags$img(
                        src = "ccc_heatmap_preview_dark.png",
                        class = "home-card-preview home-preview-dark",
                        loading = "lazy",
                        alt = "Preview of the cell-to-cell communication heatmap"
                      ),
                      h4("Cell-Cell Communication"),
                      p("Explore predicted ligand–receptor signaling interactions between cell types at each spermatogenic stage. Filter by LR pairs of interest and export publication-ready heatmaps.")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      make_patch_notes_page(),

      # =========================
      # SPERMATOGENESIS TABLE
      # =========================
      tabPanel(
        "Interactive Data",
        value = "spermatogonia_table",
        tags$div(
          class = "legacy-stack interactive-data-stack",
          make_interactive_data_nav("spermatogonia_table"),
          tags$div(
            class = "ra-pane",

            # ---- LEFT: controls card ----
            tags$div(
              class = "ra-card ra-controls glass-card",
              tags$div(
                class = "ra-card-head",
                tags$h3(class = "ra-title", "Spermatogenesis Controls"),
                tags$p(class = "ra-sub", "Optional filters for highlighting/querying genes.")
              ),
              tags$div(
                class = "ra-field",
                tags$label(class = "ra-label", "Gene query"),
                selectizeInput(
                  inputId = "gene_search",
                  label = NULL,
                  choices = NULL,
                  selected = NULL,
                  multiple = FALSE,
                  options = list(
                    placeholder = "Type a gene…",
                    create = TRUE,
                    persist = FALSE,
                    maxOptions = 20,
                    openOnFocus = FALSE
                  ),
                  width = "100%"
                )
              ),
              tags$div(
                class = "ra-field",
                tags$label(class = "ra-label", "Expression threshold"),
                numericInput(
                  inputId = "spg_expr_threshold",
                  label = NULL,
                  value = default_spg_expr_threshold,
                  min = 0,
                  step = 0.001,
                  width = "100%"
                ),
                uiOutput("spg_expr_threshold_help")
              ),
              tags$div(
                class = "ra-field",
                style = "margin-bottom: 14px;",
                actionButton(
                  inputId = "gene_search_btn",
                  label   = "Search",
                  icon    = icon("search"),
                  class   = "btn btn-primary btn-sm"
                )
              ),
              div(
                class = "heat-legend",
                span("Lower expr"),
                div(class = "heat-gradient"),
                span("Higher expr")
              ),
              tags$p(
                class = "heat-legend-note",
                "Overlay values represent the mean RNA expression of the queried gene per cell type and stage. Color intensity reflects expression level; tiles below the threshold are not labeled."
              ),
              make_interactive_download_section("spg_downloads_open")
            ),

            # ---- RIGHT: figure card ----
            tags$div(
              class = "ra-card ra-plot glass-card",
              style = "position:relative;",


              # Card header
              tags$div(
                class = "ra-card-head",
                tags$h3(class = "ra-title spg-table-title", "Interactive Spermatogenesis Table"),
                tags$p(class = "ra-sub", "Click a cell to view matched cells in the dataset.")
              ),

              # Capture container (what the PNG will include)
              tags$div(
                id = "spermatogonia_container",
                style = "border-radius:12px;",
                sc_spinner_ui_output("spermatogonia_svg")
              ),
              tags$p(
                class = "ra-sub",
                HTML("Image Modified from M&auml;kel&auml; et. al. JoVE 2020, "),
                a("https://dx.doi.org/10.3791/61800", href = "https://dx.doi.org/10.3791/61800", target = "_blank")
              )
            )
          ),
          make_interactive_explanation_box(interactive_explanations$spermatogenesis_table)
        )
      ),

      # ==========================================
      # RETINOIC ACID SIGNALING PLOTS
      # ==========================================
      tabPanel(
        "Retinoic Acid Analysis",
        value = "retinoic_acid",
        tags$div(
          class = "legacy-stack interactive-data-stack",
          make_interactive_data_nav("retinoic_acid"),
          tags$div(
            class = "ra-pane",

            # LEFT: controls
            tags$div(
              class = "ra-card ra-controls glass-card",
              tags$div(
                class = "ra-card-head",
                tags$h3(class = "ra-title", "Retinoic Acid (RA) Controls"),
                tags$p(class = "ra-sub", "Choose genes and cell types to show in the dot plot.")
              ),
              tags$div(
                class = "ra-field",
                tags$label(class = "ra-label", "Select RA Genes"),
                selectizeInput(
                  inputId = "ra_genes", label = NULL,
                  choices = NULL, selected = NULL, multiple = TRUE,
                  options = list(placeholder = "Select or type genes"),
                  width = "100%"
                )
              ),
              tags$div(
                class = "ra-field",
                tags$label(class = "ra-label", "Cell Types"),
                checkboxGroupInput(
                  inputId = "ra_cell_types", label = NULL,
                  choices = NULL, selected = NULL, width = "100%"
                )
              ),
              make_interactive_download_section("ra_dot_downloads_open")
            ),

            # RIGHT: plot card
            tags$div(
              class = "ra-card ra-plot glass-card",
              style = "position:relative;",
              tags$div(
                class = "ra-card-head",
                tags$div(
                  class = "ra-head-main",
                  tags$h3(class = "ra-title", "DotPlot")
                ),
                tags$p(class = "ra-sub", "Expression of selected RA genes across chosen cell types.")
              ),
              # Capture container
              tags$div(
                id = "ra_dotplot_container",
                style = "padding:12px; border-radius:12px;",
                sc_spinner_plot_output("ra_dotplot", height = "750px", width = "100%")
              )
            )
          ),
          tags$hr(class = "ra-divider"),
          tags$div(
            class = "ra-pane",

            # LEFT: controls
            tags$div(
              class = "ra-card ra-controls glass-card",
              tags$div(
                class = "ra-card-head",
                tags$h3(class = "ra-title", "Retinoic Acid (RA) Line Controls"),
                tags$p(class = "ra-sub", "Pick genes for each row to compare expression trends.")
              ),
              # Row 1
              tags$div(
                class = "ra-field ra-rowgroup",
                tags$label(class = "ra-label ra-rowtitle", "Row 1 Genes"),
                selectizeInput(
                  inputId = "ra_line_genes_row1", label = NULL,
                  choices = NULL, selected = NULL, multiple = TRUE,
                  options = list(placeholder = "Select genes for row 1"),
                  width = "100%"
                )
              ),
              # Row 2
              tags$div(
                class = "ra-field ra-rowgroup",
                tags$label(class = "ra-label ra-rowtitle", "Row 2 Genes"),
                selectizeInput(
                  inputId = "ra_line_genes_row2", label = NULL,
                  choices = NULL, selected = NULL, multiple = TRUE,
                  options = list(placeholder = "Select genes for row 2"),
                  width = "100%"
                )
              ),
              # Row 3
              tags$div(
                class = "ra-field ra-rowgroup",
                tags$label(class = "ra-label ra-rowtitle", "Row 3 Genes"),
                selectizeInput(
                  inputId = "ra_line_genes_row3", label = NULL,
                  choices = NULL, selected = NULL, multiple = TRUE,
                  options = list(placeholder = "Select genes for row 3"),
                  width = "100%"
                )
              ),
              make_interactive_download_section("ra_line_downloads_open")
            ),

            # RIGHT: plot card
            tags$div(
              class = "ra-card ra-plot glass-card",
              style = "position:relative;",
              tags$div(
                class = "ra-card-head",
                tags$div(
                  class = "ra-head-main",
                  tags$h3(class = "ra-title", "LinePlot")
                ),
                tags$p(class = "ra-sub", "Expression trajectories of selected genes across stages.")
              ),
              # Capture container
              tags$div(
                id = "ra_lineplot_container",
                style = "padding:12px; border-radius:12px;",
                sc_spinner_plot_output("ra_lineplot", height = "750px", width = "100%")
              )
            )
          ),
          make_interactive_explanation_box(interactive_explanations$retinoic_acid_analysis)
        )
      ),

      # ============================================
      # CELL-TO-CELL COMMUNICATION HEATMAP
      # ============================================
      tabPanel(
        "Cell-to-Cell Heatmap",
        value = "cell2cell_heatmaps",
        tags$div(
          class = "legacy-stack interactive-data-stack",
          make_interactive_data_nav("cell2cell_heatmaps"),
          tags$div(
            class = "ra-pane",

            # LEFT: controls
            tags$div(
              class = "ra-card ra-controls glass-card",
              tags$div(
                class = "ra-card-head",
                tags$h3(class = "ra-title", "Cell-to-Cell Controls"),
                tags$p(class = "ra-sub", "Choose ligand–receptor pairs to visualize communication scores.")
              ),
              tags$div(
                class = "ra-field",
                tags$label(class = "ra-label", "Ligand–Receptor Pairs"),
                selectizeInput(
                  inputId = "ccc_lr_select", label = NULL,
                  choices = NULL, multiple = TRUE,
                  options = list(placeholder = "Choose ligand–receptor pairs..."),
                  width = "100%"
                )
              ),
              make_interactive_download_section("ccc_downloads_open")
            ),

            # RIGHT: plot card
            tags$div(
              class = "ra-card ra-plot glass-card",
              style = "position:relative;",
              tags$div(
                class = "ra-card-head",
                tags$div(
                  class = "ra-head-main",
                  tags$h3(class = "ra-title", "Heatmap")
                ),
                tags$p(class = "ra-sub", "Communication scores across stages for selected LR pairs.")
              ),

              # Capture container
              tags$div(
                id = "ccc_heatmap_container",
                style = "padding:12px; border-radius:12px;",
                sc_spinner_plotly_output("ccc_heatmap", height = "680px", width = "100%")
              )
            )
          ),
          make_interactive_explanation_box(interactive_explanations$cell_to_cell_heatmap)
        )
      ),
      # Register Full Atlas detailed tabs as lazy placeholders. server.R fills
      # these bodies and binds outputs only after a matching tab is opened.
      make_lazy_dataset_tab("Full Atlas", "sc3_main_figures"),
      make_lazy_dataset_tab("CellInfo vs GeneExpr", "sc3_cellinfo_gene"),
      make_lazy_dataset_tab("Multiple GeneExpr", "sc3_multiple_geneexpr"),
      make_lazy_dataset_tab("Gene coexpression", "sc3_gene_coexpression"),
      make_lazy_dataset_tab("Violinplot / Boxplot", "sc3_violin_boxplot"),
      make_lazy_dataset_tab("Proportion plot", "sc3_proportion_plot"),
      make_lazy_dataset_tab("Bubbleplot / Heatmap", "sc3_bubble_heatmap"),
      # Register visible and hidden subset tabs through the shared subset menu.
      make_cell_subsets_menu()
    )
  )
)
