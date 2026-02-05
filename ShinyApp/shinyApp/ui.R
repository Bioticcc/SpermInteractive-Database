library(Seurat)
library(ShinyCell)
library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
library(bslib)
library(plotly)

theme_light <- "lightblue"
theme_dark <- "dark"

# Default theme on first load (may be overridden by localStorage).
theme_default <- theme_light

# sc1conf = readRDS("sc1conf.rds")
# sc1def  = readRDS("sc1def.rds")

# sc2conf = readRDS("sc2conf.rds")
# sc2def  = readRDS("sc2def.rds")

sc3conf = readRDS("sc3conf.rds")
sc3def  = readRDS("sc3def.rds")

source("ra_tabs.R")

shinyUI(
  #THEMES: lightblue, dark, mint, berry, sand, forest, sunset, ocean, lavender, default
  tags$html(`data-theme` = theme_default,
            fluidPage(
tags$head(
  tags$style(HTML("
    .shiny-output-error-validation {color: red; font-weight: bold;}
    .navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }
  ")),
  tags$script(HTML("
  console.log('Custom JS loaded');

  Shiny.addCustomMessageHandler('highlightButton', function(id) {
    console.log('Highlighting', id);
    document.getElementById(id)?.classList.add('btn-highlight');
  });

  Shiny.addCustomMessageHandler('unhighlightButton', function(id) {
    console.log('Un-highlighting', id);
    document.getElementById(id)?.classList.remove('btn-highlight');
  });
")),
  
  
),

#theme work:
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",  # Try "sandstone" or "materia" too
    base_font = font_google("Open Sans"),
    primary = "#4A90E2",
    bg = "#D3D3D3",
    fg = "#2C3E50"
  ),
  
tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
  tags$script(src = "presets.js"),
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

# ---- JS handlers ----
tags$head(
  # (A) Optional: tiny CSS for on-SVG numeric labels
  tags$style(HTML("
    .heat-label{
      font: 15px/1 monospace;
      fill:#111;
      text-anchor: middle;
      dominant-baseline: central;
      pointer-events: none;
      paint-order: stroke;
      stroke: #fff; stroke-width: 2px;
      opacity: .9;
    }
  ")),
  
  
  # (B) Your interaction handlers + debug helpers
  tags$script(HTML("
    (function () {
      var activeBtnId = null;
      var hasOwn = Object.prototype.hasOwnProperty;

      // ===== DEBUG TOGGLES =====
      // Set HEAT_DEBUG=false when you're done testing
      var HEAT_DEBUG = true;              // true = log + draw numbers
      var HEAT_LABEL_MODE = 'value';      // 'value' | 'scaled' | 'rank'

      function getNode(id) { return document.getElementById(id); }

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

      // ===== SVG helpers for debug labels =====
      function isSvgNode(el){ return !!(el && (el.ownerSVGElement || el.tagName === 'svg' || /svg/i.test(el.namespaceURI||''))); }
      function ensureHeatLabel(el, text){
        if (!HEAT_DEBUG || !isSvgNode(el)) return;
        var id = el.id + '__label';
        var label = document.getElementById(id);
        if (!label){
          label = document.createElementNS('http://www.w3.org/2000/svg', 'text');
          label.setAttribute('id', id);
          label.setAttribute('class', 'heat-label');
          if (el.parentNode) el.parentNode.insertBefore(label, el.nextSibling);
        }
        var bb = el.getBBox();
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
        if (btn) Shiny.setInputValue('btn_click', btn.id, {priority: 'event'});
      });

      Shiny.addCustomMessageHandler('highlightButton', function (btn_id) {
        if (activeBtnId && activeBtnId !== btn_id) restoreHighlight(getNode(activeBtnId));
        var el = getNode(btn_id);
        if (!el) return;

        el.dataset.restoreStroke = el.dataset.heatStroke || el.style.stroke || '';
        el.dataset.restoreFill   = el.dataset.heatFill   || el.style.fill   || '';
        el.dataset.restoreAlpha  = el.dataset.heatAlpha  || el.style.fillOpacity || '';

        el.classList.add('btn-highlight');
        el.style.stroke = '#4F46E5';
        el.style.strokeWidth = '3px';
        el.style.fill = 'rgba(79,70,229,0.12)';
        el.style.fillOpacity = '1';
        activeBtnId = btn_id;
      });

      Shiny.addCustomMessageHandler('unhighlightButton', function (btn_id) {
        var el = getNode(btn_id);
        restoreHighlight(el);
        if (activeBtnId === btn_id) activeBtnId = null;
      });

      Shiny.addCustomMessageHandler('bulkHighlight', function (payload) {
        console.info('[bulkHighlight] ids:', (payload && payload.match_ids) ? payload.match_ids.slice(0,10) : '(none)');
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

      // FIX for earlier error: attach to document (always exists)
      document.addEventListener('hidden.bs.modal', function () {
        if (!activeBtnId) return;
        restoreHighlight(getNode(activeBtnId));
        activeBtnId = null;
      });

      document.addEventListener('mousedown', function (e) {
        var btn = e.target.closest && e.target.closest('.cell-btn');
        if (btn && btn.blur) btn.blur();
      });

      // ===== HEATMAP =====
      Shiny.addCustomMessageHandler('heatmap-colorize', function (payload) {
        console.info('[heatmap] message received:', payload);

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
      
        // 👇 B: store last payload globally so you can inspect it any time
        window._heat_last_payload = payload;
      
        // 👇 C: log the first 10 rows even if HEAT_DEBUG=false
        if (colors.length) {
          console.table(colors.slice(0,10).map(function(d){
            return { id:d.id, value:d.value, scaled:d.scaled, rank:d.rank, color:d.color };
          }));
        } else {
          console.warn('[heatmap] colors empty; clear=', clear, '| nodes(.cell-btn)=', nodes.length);
        }
        // Debug: show the first 10 items arriving from server
        if (HEAT_DEBUG && colors.length) {
          console.groupCollapsed('[heatmap] first 10');
          console.table(colors.slice(0,10).map(function(d){
            return { id:d.id, value:d.value, scaled:d.scaled, rank:d.rank, color:d.color };
          }));
          console.groupEnd();
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
          var mix = (scaled === null) ? 0.75 : (0.35 + 0.65 * scaled);
          var fillColor = mixWithWhite(color, mix);
          var alpha = (scaled === null) ? 0.75 : (0.65 + 0.25 * scaled);

          // Remember values for restore
          el.dataset.heatStroke = color;
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
            el.dataset.restoreStroke = color;
            el.dataset.restoreFill   = fillColor;
            el.dataset.restoreAlpha  = String(alpha);
          } else {
            applyHeatVisual(el, color, fillColor, String(alpha));
          }

          // Draw numeric label (debug)
          if (HEAT_DEBUG) {
            var txt = '';
            if (HEAT_LABEL_MODE === 'scaled' && hasScaled) txt = scaled.toFixed(2);
            else if (HEAT_LABEL_MODE === 'rank' && info.rank != null) txt = String(info.rank);
            else if (HEAT_LABEL_MODE === 'value' && info.value != null && isFinite(info.value)) txt = Number(info.value).toFixed(2);
            if (txt) ensureHeatLabel(el, txt);
          }
        });
      });
    })();
  "))
),



br(),
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
    tags$p(class = "hdr-subtitle",
           "An Interactive Dataset for Exploring Spermatogenesis")
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
        src = "logo.png",
        alt = "Wei Yan Lab Logo"
      )
    )
  )
),

navbarPage(
  "",
  id = "mainTabs",          # add an id so we can reference tabs
  header = tagList(
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
	          function selectTabByValue(val) {
	            if (!val) return;
	            if (val === 'retinoic_acid_line') { val = 'retinoic_acid'; }
	            var selector = '#mainTabs a[data-toggle=\"tab\"][data-value=\"' + val + '\"], ' +
	              '#mainTabs a[data-bs-toggle=\"tab\"][data-value=\"' + val + '\"], ' +
	              '#mainTabs a[role=\"tab\"][data-value=\"' + val + '\"]';
	            var link = document.querySelector(selector);
	            if (!link) {
	              selector = '#mainTabs a[href=\"#' + val + '\"]';
	              link = document.querySelector(selector);
	            }
	            if (link) {
	              suppressHistory = true;
	              if (window.jQuery && window.jQuery.fn && window.jQuery.fn.tab) {
	                window.jQuery(link).tab('show');
	              } else {
	                link.click();
	              }
	            }
	          }
	          function pushTabHistory(val, replace) {
	            if (!val) return;
	            if (window.history && window.history.replaceState && window.history.pushState) {
	              var url = '#' + val;
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
	              selectTabByValue(target);
	            }
	          });
	          window.setTimeout(function() {
	            var active = document.querySelector('#mainTabs li.active a');
	            var activeVal = getTabValueFromAnchor(active);
	            if (activeVal) {
	              pushTabHistory(activeVal, true);
	            }
	          }, 0);
	          $(document).on('shown.bs.tab', '#mainTabs a[data-toggle=\"tab\"], #mainTabs a[data-bs-toggle=\"tab\"], #mainTabs a[role=\"tab\"]', function() {
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
    "))
    ,
#     tags$script(src = "upload.js")
  ),
  selected = "home",        # make Home the default landing page
  
  tabPanel(
    title = tagList(icon("house"), "Home"),
    value = "home",
    
    # ===== Row 1: Welcome (left) + Get Started (right) =====
    fluidRow(
      column(
        width = 8,
        div(class = "home-card home-hero",
            h2("Welcome"),
            p("Explore gene expression throughout mouse spermatogenesis with an an intuitive, interactive interface. ",
              "Query your genes of interest, visualize stage-specific expression patterns, and export publication-ready figures - no coding required! ",
              "The current release features our staged testis atlas, with developmental time-course and spatial transcription datasets coming soon")
        )
      ),
      column(
        width = 4,
        div(class = "home-card get-started",
            h3("Get Started"),
            tags$ol(
              tags$li("Open the Staged Testis or Interactive Data tabs from the navigation bar."),
              tags$li("Pick a figure type and set filters (genes, stages, cell groups)."),
              tags$li("Customize aesthetics and download your figure.")
            ),
            p(em("You can return here anytime from the “Home” tab."))
        )
      )
    ),
    
    # ===== Row 2: Full-width Custom Interactive Figures =====
    fluidRow(
      column(
        width = 12,
        div(class = "home-card",
            h3("Our Custom Interactive Figures (Based on Staged Testis Dataset)"),
            p("These interactive views reproduce and extend figures from the paper using our pre-loaded Seurat objects:"),
            fluidRow(
              column(
                width = 3,
                tags$a(
                  class = "home-card-link",
                  href = "#",
                  role = "button",
                  tabindex = "0",
                  `data-target-tab` = "sc3_cellinfo_gene",
                  onclick = "return window.navToTab(this.getAttribute('data-target-tab'), this);",
                  div(
                    class = "home-card",
                    tags$img(
                      src = "staged_testis_umap_preview.png",
                      class = "home-card-preview",
                      loading = "lazy",
                      alt = "Preview of the staged testis UMAP embeddings"
                    ),
                    h4("Staged Testis UMAPS"),
                    p("Visualise cell information and gene expression side-by-side on low-dimensional representations.")
                  )
                )
              ),
              column(
                width = 3,
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
                      src = "interactiveTable.png",
                      class = "home-card-preview",
                      loading = "lazy",
                      alt = "Preview of the spermatogenesis interactive table"
                    ),
                    h4("Spermatogenesis Interactive Table"),
                    p("Navigate the stage-positioned table to open context-specific modals with curated gene lists and matched cell subsets.")
                  )
                )
              ),
              column(
                width = 3,
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
                      src = "ra_lineplot_preview.png",
                      class = "home-card-preview",
                      loading = "lazy",
                      alt = "Preview of the RA line plot figure"
                    ),
                    h4("Retinoic Acid Analysis"),
                    p("Explore RA gene expression across cell populations and developmental trajectories, recreating Figures 5A–B to your own specifications")
                  )
                )
              ),
              column(
                width = 3,
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
                      class = "home-card-preview",
                      loading = "lazy",
                      alt = "Preview of the cell-to-cell communication heatmap"
                    ),
                    h4("Cell-Cell Communication Analysis"),
                    p("Visualize ligand–receptor communication scores across stages, select LR pairs, and download customized heatmaps.")
                  )
                )
              )
            )
        )
      )
    )
  ),
  

  
  
    navbarMenu(
    "Staged Testis",
    build_cellinfo_gene_tab("sc3", sc3conf, sc3def, "Staged Testis"),
    build_cellinfo_cellinfo_tab("sc3", sc3conf, sc3def, "Staged Testis"),
    build_gene_gene_tab("sc3", sc3conf, sc3def, "Staged Testis"),
    build_gene_coexpression_tab("sc3", sc3conf, sc3def, "Staged Testis"),
    build_violin_boxplot_tab("sc3", sc3conf, sc3def, "Staged Testis"),
    build_proportion_plot_tab("sc3", sc3conf, sc3def, "Staged Testis"),
    build_bubble_heatmap_tab("sc3", sc3conf, sc3def, "Staged Testis")
  ),
#   navbarMenu(
#     "Developemental Testis",
#     build_cellinfo_gene_tab("sc1", sc1conf, sc1def, "Developmental Testis"),
#     build_cellinfo_cellinfo_tab("sc1", sc1conf, sc1def, "Developmental Testis"),
#     build_gene_gene_tab("sc1", sc1conf, sc1def, "Developmental Testis"),
#     build_gene_coexpression_tab("sc1", sc1conf, sc1def, "Developmental Testis"),
#     build_violin_boxplot_tab("sc1", sc1conf, sc1def, "Developmental Testis"),
#     build_proportion_plot_tab("sc1", sc1conf, sc1def, "Developmental Testis"),
#     build_bubble_heatmap_tab("sc1", sc1conf, sc1def, "Developmental Testis")
#   ),
#   navbarMenu(
#     "Developemental Sertolis",
#     build_cellinfo_gene_tab("sc2", sc2conf, sc2def, "Developmental Sertolis"),
#     build_cellinfo_cellinfo_tab("sc2", sc2conf, sc2def, "Developmental Sertolis"),
#     build_gene_gene_tab("sc2", sc2conf, sc2def, "Developmental Sertolis"),
#     build_gene_coexpression_tab("sc2", sc2conf, sc2def, "Developmental Sertolis"),
#     build_violin_boxplot_tab("sc2", sc2conf, sc2def, "Developmental Sertolis"),
#     build_proportion_plot_tab("sc2", sc2conf, sc2def, "Developmental Sertolis"),
#     build_bubble_heatmap_tab("sc2", sc2conf, sc2def, "Developmental Sertolis")
#   ),
#   navbarMenu(
#     "User Upload",
#     tabPanel(
#       "Upload",
#       value = "user_upload",
#       tags$div(
#         class = "user-upload-pane",
#         tags$section(
#           class = "upload-panel glass-card",
#           tags$div(
#             class = "upload-grid",
#             tags$div(
#               class = "upload-hero",
#               tags$div(
#                 class = "upload-icon-wrap",
#                 icon("cloud-upload", lib = "font-awesome")
#               ),
#               tags$div(
#                 class = "upload-hero-copy",
#                 tags$h3(class = "upload-title", "Bring your Seurat data"),
#                 tags$p(
#                   class = "upload-hint",
#                   HTML("Drop a prepared <code>.rds</code> file, or browse to load a Seurat object for this session.")
#                 ),
#                 tags$div(
#                   class = "upload-guidelines",
#                   tags$span(class = "upload-pill", "Save as .rds"),
#                   tags$span(class = "upload-pill", "JoinLayers for v5"),
#                   tags$span(class = "upload-pill", "No PHI")
#                 ),
#                 tags$p(
#                   class = "upload-footnote",
#                   "Tip: For Seurat v5 objects, run JoinLayers() before saving."
#                 )
#               )
#             ),
#             tags$div(
#               class = "upload-form",
#               tags$div(
#                 class = "upload-dropzone",
#                 fileInput(
#                   inputId = "user_seurat_file",
#                   label = NULL,
#                   buttonLabel = "Browse .rds",
#                   placeholder = "No file selected",
#                   accept = c(".rds")
#                 )
#               ),
#               tags$div(
#                 class = "upload-progress-area",
#                 tags$div(
#                   class = "upload-progress-header",
#                   tags$div(
#                     class = "upload-progress-copy",
#                     tags$span(class = "upload-progress-label", "Upload status"),
#                     tags$p(
#                       id = "user-upload-status",
#                       class = "upload-progress-note",
#                       `data-default` = "Select an .rds file to begin.",
#                       "Select an .rds file to begin."
#                     )
#                   ),
#                   tags$div(
#                     class = "upload-progress-spinner",
#                     icon("circle-notch", class = "fa-spin"),
#                     tags$span(class = "sr-only", "Processing uploaded data")
#                   )
#                 )
#               ),
#               tags$div(
#                 class = "upload-feedback-area",
#                 uiOutput("user_upload_feedback"),
#                 uiOutput("user_dataset_summary")
#               )
#             )
#           )
#         )
#       )
#     ),
#     tabPanel(
#       title = HTML("CellInfo vs GeneExpr"),
#       value = "usr_cellinfo_gene",
#       uiOutput("usr_cellinfo_gene_panel")
#     ),
#     tabPanel(
#       title = HTML("CellInfo vs CellInfo"),
#       value = "usr_cellinfo_cellinfo",
#       uiOutput("usr_cellinfo_cellinfo_panel")
#     ),
#     tabPanel(
#       title = HTML("GeneExpr vs GeneExpr"),
#       value = "usr_gene_gene",
#       uiOutput("usr_gene_gene_panel")
#     ),
#     tabPanel(
#       title = HTML("Gene coexpression"),
#       value = "usr_gene_coexpression",
#       uiOutput("usr_gene_coexpression_panel")
#     ),
#     tabPanel(
#       title = HTML("Violinplot / Boxplot"),
#       value = "usr_violin_boxplot",
#       uiOutput("usr_violin_boxplot_panel")
#     ),
#     tabPanel(
#       title = HTML("Proportion plot"),
#       value = "usr_proportion_plot",
#       uiOutput("usr_proportion_plot_panel")
#     ),
#     tabPanel(
#       title = HTML("Bubbleplot / Heatmap"),
#       value = "usr_bubble_heatmap",
#       uiOutput("usr_bubble_heatmap_panel")
#     )
#   ),
navbarMenu(
  "Interactive Data",
  
  # =========================
  # SPERMATOGENESIS TABLE (PNG only)
  # =========================
  tabPanel(
    "Spermatogenesis Table",
    value = "spermatogonia_table",
    tags$div(
      class = "ra-pane",
      style = "display:flex; align-items:stretch; gap:16px; width:100%;",
      
      # ---- LEFT: controls card (UNCHANGED) ----
      tags$div(
        class = "ra-card ra-controls glass-card",
        style = "align-self: flex-start;",
        tags$div(
          class = "ra-card-head",
          tags$h3(class = "ra-title", "Spermatogenesis Controls"),
          tags$p(class = "ra-sub", "Optional filters for highlighting/querying genes.")
        ),
        tags$div(
          class = "ra-field",
          tags$label(class = "ra-label", "Gene query"),
          textInput(
            inputId = "gene_search",
            label = NULL,
            placeholder = "Type a gene…",
            width = "100%"
          ),
          actionButton(
            inputId = "gene_search_btn",
            label   = "Search",
            icon    = icon("search"),
            class   = "btn btn-primary btn-sm",
            style   = "margin-top: 6px;"
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
          "Overlay numbers show the average expression of the searched gene for the cells in each tile (RNA data); higher values mean higher expression. Tiles below the threshold are unlabeled."
        )
      ),
      
      # ---- RIGHT: figure card ----
      tags$div(
        class = "ra-card ra-plot glass-card",
        style = "flex:1 1 0%; min-width:0; max-width:unset; width:100%; position:relative;",
        
        
        # Card header
        tags$div(
          class = "ra-card-head",
          tags$h3(class = "ra-title", "Interactive Spermatogenesis Table"),
          tags$p(class = "ra-sub", "Click a cell to view matched cells in the dataset.")
        ),
        
        # Capture container (what the PNG will include)
        tags$div(
          id = "spermatogonia_container",
          style = "padding:12px; border-radius:12px;",
          shinycssloaders::withSpinner(
            uiOutput("spermatogonia_svg"),
            type = 3,
            color = "#4F46E5",
            color.background = "transparent",
            proxy.height = "720px"
          )
        ),
        tags$p(
          class = "ra-sub",
          HTML("Image Modified from M&auml;kel&auml; et. al. JoVE 2020, "),
          a("https://dx.doi.org/10.3791/61800", href = "https://dx.doi.org/10.3791/61800", target = "_blank")
        )
      )
    )
  ),
  
  # ==========================================
  # RETINOIC ACID SIGNALING PLOTS (COMBINED)
  # ==========================================
  tabPanel(
    "Retinoic Acid Analysis",
    value = "retinoic_acid",
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
        )
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
        tags$div(
          class = "ra-card-download",
          downloadButton(
            outputId = "ra_dotplot_pdf",
            label    = "Download PDF",
            class    = "ra-btn ra-download-btn no-snapshot"
          )
        ),
        tags$details(
          class = "ra-reference",
          tags$summary("Compare to publication (Fig. 5A)"),
          tags$img(
            src = "ra_dotplot_preview.png",
            alt = "Published RA dotplot reference",
            class = "ra-reference-img",
            loading = "lazy"
          ),
          tags$p(
            class = "ra-reference-caption",
            "Use this reference image to keep downloaded plots aligned with the manuscript figure."
          )
        ),
        
        # Capture container
        tags$div(
          id = "ra_dotplot_container",
          style = "padding:12px; border-radius:12px;",
          shinycssloaders::withSpinner(
            plotOutput("ra_dotplot", height = "750px", width = "100%"),
            type = 3,
            color = "#4F46E5",
            color.background = "transparent",
            proxy.height = "750px"
          )
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
        )
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
        tags$div(
          class = "ra-card-download",
          downloadButton(
            outputId = "ra_lineplot_pdf",
            label    = "Download PDF",
            class    = "ra-btn ra-download-btn no-snapshot"
          )
        ),
        tags$details(
          class = "ra-reference",
          tags$summary("Compare to publication (Fig. 5C)"),
          tags$img(
            src = "ra_lineplot_preview.png",
            alt = "Published RA lineplot reference",
            class = "ra-reference-img",
            loading = "lazy"
          ),
          tags$p(
            class = "ra-reference-caption",
            "Match your trajectories against the published layout for quick visual QA."
          )
        ),
        
        # Capture container
        tags$div(
          id = "ra_lineplot_container",
          style = "padding:12px; border-radius:12px;",
          shinycssloaders::withSpinner(
            plotOutput("ra_lineplot", height = "750px", width = "100%"),
            type = 3,
            color = "#4F46E5",
            color.background = "transparent",
            proxy.height = "750px"
          )
        )
      )
    )
  ),
  
  # ============================================
  # CELL-TO-CELL HEATMAP (PNG + PDF via server)
  # ============================================
  tabPanel(
    "Cell-to-Cell Heatmap",
    value = "cell2cell_heatmaps",
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
        )
      ),
      
      # RIGHT: plot card
      tags$div(
        class = "ra-card ra-plot glass-card",
        style = "position:relative;",
        
        tags$div(
          class = "ra-card-head",
          tags$h3(class = "ra-title", "Heatmap"),
          tags$p(class = "ra-sub", "Communication scores across stages for selected LR pairs.")
        ),
        
        # Capture container
        tags$div(
          id = "ccc_heatmap_container",
          style = "padding:12px; border-radius:12px;",
          shinycssloaders::withSpinner(
            plotlyOutput("ccc_heatmap", height = "680px", width = "100%"),
            type = 3,
            color = "#4F46E5",
            color.background = "transparent"
          )
        )
      )
    )
  )
),



   
br(), 
p(
  strong("Reference: "),
  "Hayden McSwiggin, ",
  "Single Nuclei Analysis of Staged Seminifierous Tubules (Unpublished, expected mid 2026)",
  style = "font-size: 125%;"
), 
p(
  em("This webpage was made using "),
  a("ShinyCell", href = "https://github.com/SGDDNB/ShinyCell", target = "_blank"),
  em(" — By Adam Tomasz Ward "),
  a("(GitHub)", href = "https://github.com/Bioticcc", target = "_blank")
),


br(),br(),br(),br(),br() 
))))
 
 
 
 
