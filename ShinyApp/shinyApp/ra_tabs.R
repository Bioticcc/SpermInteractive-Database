ra_card_head <- function(title = NULL, subtitle = NULL, ...) {
  if (is.null(title)) {
    return(NULL)
  }
  extras <- Filter(Negate(is.null), list(...))
  head_children <- c(list(tags$h3(class = "ra-title", title)), extras)
  tags$div(
    class = "ra-card-head",
    do.call(
      tags$div,
      c(list(class = "ra-head-main"), head_children)
    ),
    if (!is.null(subtitle)) {
      tags$p(class = "ra-sub", subtitle)
    } else {
      NULL
    }
  )
}

ra_rowgroup <- function(title = NULL, ...) {
  tags$div(
    class = "ra-rowgroup",
    if (!is.null(title)) {
      tags$div(class = "ra-rowtitle", title)
    } else {
      NULL
    },
    ...
  )
}

ra_field <- function(label = NULL, input) {
  tags$div(
    class = "ra-field",
    if (!is.null(label)) {
      tags$label(class = "ra-label", label)
    } else {
      NULL
    },
    input
  )
}

ra_download_row <- function(pdf_id,
                            png_id,
                            height_id,
                            width_id,
                            height_label = "PDF / PNG height:",
                            width_label = "PDF / PNG width:",
                            height_value = 8,
                            width_value = 10,
                            height_min = 4,
                            height_max = 20,
                            width_min = 4,
                            width_max = 20) {
  tags$div(
    class = "ra-download-row",
    tags$div(
      class = "ra-download-buttons",
      downloadButton(pdf_id, "Download PDF"),
      downloadButton(png_id, "Download PNG")
    ),
    tags$div(
      class = "ra-download-sizing",
      numericInput(
        inputId = height_id,
        label = height_label,
        value = height_value,
        min = height_min,
        max = height_max,
        step = 0.5,
        width = "148px"
      ),
      numericInput(
        inputId = width_id,
        label = width_label,
        value = width_value,
        min = width_min,
        max = width_max,
        step = 0.5,
        width = "148px"
      )
    )
  )
}

ra_download_buttons_only <- function(pdf_id, png_id) {
  tags$div(
    class = "ra-download-row",
    tags$div(
      class = "ra-download-buttons",
      downloadButton(pdf_id, "Download PDF"),
      downloadButton(png_id, "Download PNG")
    )
  )
}

ra_button_row <- function(...) {
  tags$div(class = "ra-button-row", ...)
}

ra_taglist <- function(...) {
  nodes <- unlist(list(...), recursive = FALSE)
  nodes <- Filter(Negate(is.null), nodes)
  if (length(nodes) == 0) {
    return(NULL)
  }
  do.call(tagList, nodes)
}

build_common_controls <- function(prefix, block, conf, def) {
  make_id <- function(suffix) paste0(prefix, block, suffix)
    base <- list(
      ra_rowgroup(
        "Dimension reduction",
        ra_field(
          "X-axis",
        selectInput(
          inputId = make_id("drX"),
          label = NULL,
          choices = conf[dimred == TRUE]$UI,
          selected = def$dimred[1]
        )
      ),
      ra_field(
        "Y-axis",
        selectInput(
          inputId = make_id("drY"),
          label = NULL,
          choices = conf[dimred == TRUE]$UI,
          selected = def$dimred[2]
        )
      )
    )
  )
  advanced <- list(
    ra_rowgroup(
      "Subset cells",
      ra_field(
        "Cell information to subset",
        selectInput(
          inputId = make_id("sub1"),
          label = NULL,
          choices = conf[grp == TRUE]$UI,
          selected = def$grp1
        )
      ),
      uiOutput(make_id("sub1.ui")),
      ra_button_row(
        actionButton(
          inputId = make_id("sub1all"),
          label = "Select all groups",
          class = "btn btn-primary btn-sm"
        ),
        actionButton(
          inputId = make_id("sub1non"),
          label = "Deselect all groups",
          class = "btn btn-outline-secondary btn-sm"
        )
      )
      ),
      ra_rowgroup(
        "Display options",
        ra_field(
          "Point size",
          sliderInput(
            inputId = make_id("siz"),
            label = NULL,
            min = 0,
            max = 4,
            value = 1.25,
            step = 0.25
          )
        ),
        ra_field(
          "Plot size",
          radioButtons(
            inputId = make_id("psz"),
            label = NULL,
            choices = c("Small", "Medium", "Large"),
            selected = "Large",
            inline = TRUE
          )
        ),
        ra_field(
          "Font size",
          radioButtons(
            inputId = make_id("fsz"),
            label = NULL,
            choices = c("Small", "Medium", "Large"),
            selected = "Medium",
            inline = TRUE
          )
        ),
        ra_field(
          "Aspect ratio",
          radioButtons(
            inputId = make_id("asp"),
            label = NULL,
            choices = c("Square", "Fixed", "Free"),
            selected = "Free",
            inline = TRUE
          )
        ),
        ra_field(
          "Split by stage (sample)",
          checkboxInput(
            inputId = make_id("split"),
            label = NULL,
            value = FALSE
          )
        ),
        tags$div(
          class = "ra-field-checkbox",
          checkboxInput(
            inputId = make_id("txt"),
            label = "Show axis text",
          value = FALSE
        )
      )
    )
  )
  list(
    base = base,
    advanced = advanced
  )
}

build_cellinfo_overlay_controls <- function(prefix, block, suffix, conf, default_meta) {
  make_id <- function(part) paste0(prefix, block, part)
  title <- if (suffix == "1") {
    "Cell information overlay"
  } else {
    sprintf("Cell information %s overlay", suffix)
  }
  base <- ra_rowgroup(
    title,
    ra_field(
      "Cell information",
      selectInput(
        inputId = make_id(paste0("inp", suffix)),
        label = NULL,
        choices = conf$UI,
        selected = default_meta
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Cell information to colour cells by",
          content = c(
            "Select cell information to colour cells",
            "- Categorical covariates have a fixed colour palette",
            "- Continuous covariates are coloured in a Blue-Yellow-Red colour scheme, which can be changed in the plot controls"
          )
        )
    )
  )
  advanced <- ra_rowgroup(
    paste(title, "styling"),
    ra_field(
      "Colour (continuous data)",
      radioButtons(
        inputId = make_id(paste0("col", suffix)),
        label = NULL,
        choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"),
        selected = "Blue-Yellow-Red"
      )
    ),
    ra_field(
      "Plot order",
      radioButtons(
        inputId = make_id(paste0("ord", suffix)),
        label = NULL,
        choices = c("Max-1st", "Min-1st", "Original", "Random"),
        selected = "Original",
        inline = TRUE
      )
    ),
    tags$div(
      class = "ra-field-checkbox",
      checkboxInput(
        inputId = make_id(paste0("lab", suffix)),
        label = "Show cell info labels",
        value = TRUE
      )
    )
  )
  list(
    base = list(base),
    advanced = list(advanced)
  )
}

build_gene_overlay_controls <- function(prefix, block, suffix, colour_default = "White-Red", order_default = "Max-1st") {
  make_id <- function(part) paste0(prefix, block, part)
  title <- if (suffix == "1") {
    "Gene expression overlay"
  } else {
    sprintf("Gene expression %s overlay", suffix)
  }
  base <- ra_rowgroup(
    title,
    ra_field(
      "Gene name",
      selectizeInput(
        inputId = make_id(paste0("inp", suffix)),
        label = NULL,
        choices = NULL,
        options = list(placeholder = "Type a gene name")
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Gene expression to colour cells by",
          content = c(
            "Select gene to colour cells by gene expression",
            "- Gene expression are coloured in a White-Red colour scheme which can be changed in the plot controls"
          )
        )
    )
  )
  advanced <- ra_rowgroup(
    paste(title, "styling"),
    ra_field(
      "Colour",
      radioButtons(
        inputId = make_id(paste0("col", suffix)),
        label = NULL,
        choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"),
        selected = colour_default
      )
    ),
    ra_field(
      "Plot order",
      radioButtons(
        inputId = make_id(paste0("ord", suffix)),
        label = NULL,
        choices = c("Max-1st", "Min-1st", "Original", "Random"),
        selected = order_default,
        inline = TRUE
      )
    )
  )
  list(
    base = list(base),
    advanced = list(advanced)
  )
}

build_cellinfo_output_section <- function(prefix,
                                          block,
                                          suffix,
                                          title,
                                          height_value,
                                          width_value,
                                          include_stats = FALSE) {
  make_id <- function(part) paste0(prefix, block, part)
  content <- list(
    tags$div(
      class = "ra-plot-holder",
      uiOutput(make_id(paste0("oup", suffix, ".ui")))
    ),
    ra_download_row(
      pdf_id = make_id(paste0("oup", suffix, ".pdf")),
      png_id = make_id(paste0("oup", suffix, ".png")),
      height_id = make_id(paste0("oup", suffix, ".h")),
      width_id = make_id(paste0("oup", suffix, ".w")),
      height_value = height_value,
      width_value = width_value
    )
  )
  if (include_stats) {
    content <- append(
      content,
      list(
        ra_button_row(
          actionButton(
            inputId = make_id("tog9"),
            label = "Toggle cell numbers / statistics",
            class = "btn btn-outline-primary ra-toggle-btn"
          )
        ),
        conditionalPanel(
          condition = sprintf("input.%s %% 2 == 1", make_id("tog9")),
          ra_field(
            "Split continuous cell info into",
            radioButtons(
              inputId = make_id("splt"),
              label = NULL,
              choices = c("Quartile", "Decile"),
              selected = "Decile",
              inline = TRUE
            )
          ),
          dataTableOutput(make_id(".dt"))
        )
      )
    )
  }
  do.call(
    ra_rowgroup,
    c(list(title), content)
  )
}

build_gene_output_section <- function(prefix,
                                      block,
                                      suffix,
                                      title,
                                      height_value,
                                      width_value) {
  make_id <- function(part) paste0(prefix, block, part)
  ra_rowgroup(
    title,
    tags$div(
      class = "ra-plot-holder",
      uiOutput(make_id(paste0("oup", suffix, ".ui")))
    ),
    ra_download_row(
      pdf_id = make_id(paste0("oup", suffix, ".pdf")),
      png_id = make_id(paste0("oup", suffix, ".png")),
      height_id = make_id(paste0("oup", suffix, ".h")),
      width_id = make_id(paste0("oup", suffix, ".w")),
      height_value = height_value,
      width_value = width_value
    )
  )
}

build_main_figures_tab <- function(prefix, conf, def, dataset_name, as_tab = TRUE) {
  make_id <- function(part) paste0(prefix, "mf", part)

  # Main Figures use the harmonized cell-type annotation.
  target_ui <- "correct_cellTypes"
  levels_raw <- conf[UI == target_ui]$fID
  levels_raw <- if (length(levels_raw) && !is.na(levels_raw[[1]])) levels_raw[[1]] else ""
  cell_levels <- if (nzchar(levels_raw)) strsplit(levels_raw, "\\|")[[1]] else character(0)

  controls_card <- tags$div(
    class = "ra-card ra-controls glass-card mainfig-controls",
    ra_card_head(
      sprintf("%s Main Figures", dataset_name),
      "Select which annotated cell groups to emphasize across the UMAP panels."
    ),
    ra_rowgroup(
      "Cell selection",
      checkboxGroupInput(
        inputId = make_id("cells"),
        label = NULL,
        choices = cell_levels,
        selected = cell_levels,
        inline = FALSE
      ),
      ra_button_row(
        actionButton(
          inputId = make_id("all"),
          label = "Select all groups",
          class = "btn btn-primary btn-sm"
        ),
        actionButton(
          inputId = make_id("none"),
          label = "Deselect all groups",
          class = "btn btn-outline-secondary btn-sm"
        )
      ),
      ra_field(
        "Main UMAP point size",
        sliderInput(
          inputId = make_id("pt"),
          label = NULL,
          min = 1,
          max = 5,
          value = 2.5,
          step = 0.1
        )
      ),
      ra_field(
        "Cell labels",
        checkboxInput(
          inputId = make_id("labels"),
          label = "Show cell labels",
          value = TRUE
        )
      )
    )
  )

  main_plot_card <- tags$div(
    class = "ra-card ra-plot glass-card mainfig-main",
    ra_card_head(
      "UMAP Overview",
      sprintf("UMAP1 vs UMAP2 coloured by %s (not split by stage).", target_ui)
    ),
    ra_rowgroup(
      NULL,
      tags$div(
        class = "ra-plot-holder mainfig-plot-holder",
        tags$div(
          class = "mainfig-square",
          plotOutput(make_id("main"), height = "100%", width = "100%")
        )
      )
    )
  )

  split_plot_card <- tags$div(
    class = "ra-card ra-plot glass-card mainfig-split",
    ra_card_head(
      "Stage-Split UMAPs",
      sprintf("Same view split by stage (I–XII), coloured by %s.", target_ui)
    ),
    ra_rowgroup(
      NULL,
      tags$div(
        class = "ra-plot-holder",
        plotOutput(make_id("split"), height = "420px", width = "100%")
      )
    )
  )

  content <- tags$div(
    class = sprintf("legacy-stack %s-mainfig-stack", prefix),
    tags$div(
      class = "legacy-pane mainfig-pane",
      controls_card,
      main_plot_card
    ),
    split_plot_card
  )

  if (as_tab) {
    tabPanel(
      title = HTML("Main Figures"),
      value = sprintf("%s_main_figures", prefix),
      content
    )
  } else {
    content
  }
}

build_cellinfo_gene_tab <- function(prefix, conf, def, dataset_name, as_tab = TRUE) {
  make_id <- function(part) paste0(prefix, "a1", part)
  common <- build_common_controls(prefix, "a1", conf, def)
  cellinfo_overlay <- build_cellinfo_overlay_controls(prefix, "a1", "1", conf, def$meta1)
  gene_overlay <- build_gene_overlay_controls(prefix, "a1", "2", colour_default = "White-Red", order_default = "Max-1st")

  advanced_content <- ra_taglist(common$advanced, cellinfo_overlay$advanced, gene_overlay$advanced)
  base_content <- ra_taglist(common$base, cellinfo_overlay$base, gene_overlay$base)

  adv_toggle_id <- make_id("adv")
  advanced_card <- if (!is.null(advanced_content)) {
    tags$div(
      class = "ra-card ra-controls glass-card legacy-advanced",
      ra_card_head(
        "Toggle Advanced Controls",
        "Quick access to subset and styling controls.",
        tags$div(
          class = "legacy-advanced-toggle",
          actionButton(
            inputId = adv_toggle_id,
            label = NULL,
            class = "btn btn-outline-primary ra-advanced-toggle",
            icon = icon("chevron-down")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s %% 2 == 1", adv_toggle_id),
        tags$div(
          class = "legacy-advanced-body",
          advanced_content
        )
      )
    )
  } else {
    NULL
  }

  base_card <- tags$div(
    class = "ra-card ra-controls glass-card legacy-controls",
    ra_card_head(
      sprintf("%s Controls", dataset_name),
      "Configure axes, cell subsets, and overlays."
    ),
    tags$p(
      class = "ra-subtext",
      "Visualise cell information and gene expression side-by-side on low-dimensional representations."
    ),
    base_content
  )

  plot_card <- tags$div(
    class = "ra-card ra-plot glass-card legacy-plots",
    ra_card_head(
      sprintf("%s Embeddings", dataset_name),
      "Compare overlays and export publication-ready figures."
    ),
    build_cellinfo_output_section(prefix, "a1", "1", "Cell information overlay", 6, 8, include_stats = TRUE),
    build_gene_output_section(prefix, "a1", "2", "Gene expression overlay", 6, 8)
  )

  content <- tags$div(
    class = sprintf("legacy-stack %s-cellinfo-gene-stack", prefix),
    advanced_card,
    tags$div(
      class = sprintf("ra-pane legacy-pane %s-cellinfo-gene", prefix),
      base_card,
      plot_card
    )
  )

  if (as_tab) {
    tabPanel(
      title = HTML("CellInfo vs GeneExpr"),
      value = sprintf("%s_cellinfo_gene", prefix),
      content
    )
  } else {
    content
  }
}

build_cellinfo_cellinfo_tab <- function(prefix, conf, def, dataset_name, as_tab = TRUE) {
  make_id <- function(part) paste0(prefix, "a2", part)
  common <- build_common_controls(prefix, "a2", conf, def)
  overlay1 <- build_cellinfo_overlay_controls(prefix, "a2", "1", conf, def$meta1)
  overlay2 <- build_cellinfo_overlay_controls(prefix, "a2", "2", conf, def$meta2)

  advanced_content <- ra_taglist(common$advanced, overlay1$advanced, overlay2$advanced)
  base_content <- ra_taglist(common$base, overlay1$base, overlay2$base)

  adv_toggle_id <- make_id("adv")
  advanced_card <- if (!is.null(advanced_content)) {
    tags$div(
      class = "ra-card ra-controls glass-card legacy-advanced",
      ra_card_head(
        "Toggle Advanced Controls",
        "Subset and styling options for each overlay.",
        tags$div(
          class = "legacy-advanced-toggle",
          actionButton(
            inputId = adv_toggle_id,
            label = NULL,
            class = "btn btn-outline-primary ra-advanced-toggle",
            icon = icon("chevron-down")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s %% 2 == 1", adv_toggle_id),
        tags$div(
          class = "legacy-advanced-body",
          advanced_content
        )
      )
    )
  } else {
    NULL
  }

  base_card <- tags$div(
    class = "ra-card ra-controls glass-card legacy-controls",
    ra_card_head(
      sprintf("%s Controls", dataset_name),
      "Configure axes, cell subsets, and twin metadata overlays."
    ),
    tags$p(
      class = "ra-subtext",
      "Compare two cell information tracks on the same embedding."
    ),
    base_content
  )

  plot_card <- tags$div(
    class = "ra-card ra-plot glass-card legacy-plots",
    ra_card_head(
      sprintf("%s Embeddings", dataset_name),
      "Side-by-side metadata overlays."
    ),
    build_cellinfo_output_section(prefix, "a2", "1", "Cell information overlay 1", 6, 8),
    build_cellinfo_output_section(prefix, "a2", "2", "Cell information overlay 2", 6, 8)
  )

  content <- tags$div(
    class = sprintf("legacy-stack %s-cellinfo-cellinfo-stack", prefix),
    advanced_card,
    tags$div(
      class = sprintf("ra-pane legacy-pane %s-cellinfo-cellinfo", prefix),
      base_card,
      plot_card
    )
  )

  if (as_tab) {
    tabPanel(
      title = HTML("CellInfo vs CellInfo"),
      value = sprintf("%s_cellinfo_cellinfo", prefix),
      content
    )
  } else {
    content
  }
}

build_gene_gene_tab <- function(prefix, conf, def, dataset_name, as_tab = TRUE) {
  make_id <- function(part) paste0(prefix, "a3", part)
  common <- build_common_controls(prefix, "a3", conf, def)
  gene1 <- build_gene_overlay_controls(prefix, "a3", "1", colour_default = "White-Red", order_default = "Max-1st")
  gene2 <- build_gene_overlay_controls(prefix, "a3", "2", colour_default = "White-Red", order_default = "Max-1st")

  advanced_content <- ra_taglist(common$advanced, gene1$advanced, gene2$advanced)
  base_content <- ra_taglist(common$base, gene1$base, gene2$base)

  adv_toggle_id <- make_id("adv")
  advanced_card <- if (!is.null(advanced_content)) {
    tags$div(
      class = "ra-card ra-controls glass-card legacy-advanced",
      ra_card_head(
        "Toggle Advanced Controls",
        "Subset and styling tweaks for both gene overlays.",
        tags$div(
          class = "legacy-advanced-toggle",
          actionButton(
            inputId = adv_toggle_id,
            label = NULL,
            class = "btn btn-outline-primary ra-advanced-toggle",
            icon = icon("chevron-down")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s %% 2 == 1", adv_toggle_id),
        tags$div(
          class = "legacy-advanced-body",
          advanced_content
        )
      )
    )
  } else {
    NULL
  }

  base_card <- tags$div(
    class = "ra-card ra-controls glass-card legacy-controls",
    ra_card_head(
      sprintf("%s Controls", dataset_name),
      "Configure axes, cell subsets, and gene overlays."
    ),
    tags$p(
      class = "ra-subtext",
      "Visualise two gene expression signals on the same embedding."
    ),
    base_content
  )

  plot_card <- tags$div(
    class = "ra-card ra-plot glass-card legacy-plots",
    ra_card_head(
      sprintf("%s Embeddings", dataset_name),
      "Dual gene expression overlays."
    ),
    build_gene_output_section(prefix, "a3", "1", "Gene expression overlay 1", 6, 8),
    build_gene_output_section(prefix, "a3", "2", "Gene expression overlay 2", 6, 8)
  )

  content <- tags$div(
    class = sprintf("legacy-stack %s-gene-gene-stack", prefix),
    advanced_card,
    tags$div(
      class = sprintf("ra-pane legacy-pane %s-gene-gene", prefix),
      base_card,
      plot_card
    )
  )

  if (as_tab) {
    tabPanel(
      title = HTML("GeneExpr vs GeneExpr"),
      value = sprintf("%s_gene_gene", prefix),
      content
    )
  } else {
    content
  }
}

build_bubble_heatmap_tab <- function(prefix, conf, def, dataset_name, as_tab = TRUE) {
  make_id <- function(part) paste0(prefix, "d1", part)

  gene_group <- ra_rowgroup(
    "Gene list",
    ra_field(
      "List of gene names",
      textAreaInput(
        inputId = make_id("inp"),
        label = NULL,
        height = "220px",
        value = paste0(def$genes, collapse = ", ")
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "List of genes to plot on bubbleplot / heatmap",
          content = c(
            "Input genes to plot",
            "- Maximum 50 genes (due to plotting space limitations)",
            "- Genes should be separated by comma, semicolon or newline"
          )
        )
    )
  )

  layout_group <- ra_rowgroup(
    "Grouping & layout",
    ra_field(
      "Group by",
      selectInput(
        inputId = make_id("grp"),
        label = NULL,
        choices = conf[grp == TRUE]$UI,
        selected = conf[grp == TRUE]$UI[1]
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Cell information to group cells by",
          content = c(
            "Select categorical cell information to group cells by",
            "- Single cells are grouped by this categorical covariate",
            "- Plotted as the X-axis of the bubbleplot / heatmap"
          )
        )
    ),
    ra_field(
      "Plot type",
      radioButtons(
        inputId = make_id("plt"),
        label = NULL,
        choices = c("Bubbleplot", "Heatmap"),
        selected = "Bubbleplot",
        inline = TRUE
      )
    )
  )

  options_group <- ra_rowgroup(
    "Options",
    tags$div(
      class = "ra-field-checkbox",
      checkboxInput(
        inputId = make_id("scl"),
        label = "Scale gene expression",
        value = TRUE
      )
    ),
    tags$div(
      class = "ra-field-checkbox",
      checkboxInput(
        inputId = make_id("row"),
        label = "Cluster rows (genes)",
        value = TRUE
      )
    ),
    tags$div(
      class = "ra-field-checkbox",
      checkboxInput(
        inputId = make_id("col"),
        label = "Cluster columns (samples)",
        value = FALSE
      )
    )
  )

  subset_group <- ra_rowgroup(
    "Subset cells",
    ra_field(
      "Cell information to subset",
      selectInput(
        inputId = make_id("sub1"),
        label = NULL,
        choices = conf[grp == TRUE]$UI,
        selected = def$grp1
      )
    ),
    uiOutput(make_id("sub1.ui"))
  )

  display_group <- ra_rowgroup(
    "Display options",
    ra_field(
      "Colour scheme",
      radioButtons(
        inputId = make_id("cols"),
        label = NULL,
        choices = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"),
        selected = "Blue-Yellow-Red"
      )
    ),
    ra_field(
      "Plot size",
      radioButtons(
        inputId = make_id("psz"),
        label = NULL,
        choices = c("Small", "Medium", "Large"),
        selected = "Large",
        inline = TRUE
      )
    ),
    ra_field(
      "Font size",
      radioButtons(
        inputId = make_id("fsz"),
        label = NULL,
        choices = c("Small", "Medium", "Large"),
        selected = "Medium",
        inline = TRUE
      )
    )
  )

  base_content <- ra_taglist(gene_group, layout_group, options_group)
  advanced_content <- ra_taglist(subset_group, display_group)

  adv_toggle_id <- make_id("adv")
  advanced_card <- if (!is.null(advanced_content)) {
    tags$div(
      class = "ra-card ra-controls glass-card legacy-advanced",
      ra_card_head(
        "Toggle Advanced Controls",
        "Subset groups and reveal styling controls.",
        tags$div(
          class = "legacy-advanced-toggle",
          actionButton(
            inputId = adv_toggle_id,
            label = NULL,
            class = "btn btn-outline-primary ra-advanced-toggle",
            icon = icon("chevron-down")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s %% 2 == 1", adv_toggle_id),
        tags$div(
          class = "legacy-advanced-body",
          advanced_content
        )
      )
    )
  } else {
    NULL
  }

  base_card <- tags$div(
    class = "ra-card ra-controls glass-card legacy-controls",
    ra_card_head(
      sprintf("%s Controls", dataset_name),
      "Configure the gene panels, grouping, and visual style."
    ),
    tags$p(
      class = "ra-subtext",
      "Visualise the gene expression patterns of multiple genes grouped by categorical cell information."
    ),
    base_content
  )

  plot_card <- tags$div(
    class = "ra-card ra-plot glass-card legacy-plots",
    ra_card_head(
      sprintf("%s Bubbleplot / Heatmap", dataset_name),
      "Visualise averaged expression across groups and export the figure."
    ),
    ra_rowgroup(
      NULL,
      h4(htmlOutput(make_id("oupTxt"))),
      tags$div(
        class = "ra-plot-holder",
        uiOutput(make_id("oup.ui"))
      ),
      ra_download_row(
        pdf_id = make_id("oup.pdf"),
        png_id = make_id("oup.png"),
        height_id = make_id("oup.h"),
        width_id = make_id("oup.w"),
        height_value = 10,
        width_value = 10
      )
    )
  )

  content <- tags$div(
    class = sprintf("legacy-stack %s-bubble-stack", prefix),
    advanced_card,
    tags$div(
      class = sprintf("ra-pane legacy-pane %s-bubble", prefix),
      base_card,
      plot_card
    )
  )

  if (as_tab) {
    tabPanel(
      title = HTML("Bubbleplot / Heatmap"),
      value = sprintf("%s_bubble_heatmap", prefix),
      content
    )
  } else {
    content
  }
}

build_gene_coexpression_tab <- function(prefix, conf, def, dataset_name, as_tab = TRUE) {
  common <- build_common_controls(prefix, "b2", conf, def)
  make_id <- function(part) paste0(prefix, "b2", part)

  gene_group <- ra_rowgroup(
    "Gene selection",
    ra_field(
      "Gene 1",
      selectizeInput(
        inputId = make_id("inp1"),
        label = NULL,
        choices = NULL,
        options = list(placeholder = "Type a gene name")
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Gene 1 to visualise",
          content = c(
            "Select the first gene to colour cells by gene expression",
            "- Expression values are coloured in a White-Red scheme by default"
          )
        )
    ),
    ra_field(
      "Gene 2",
      selectizeInput(
        inputId = make_id("inp2"),
        label = NULL,
        choices = NULL,
        options = list(placeholder = "Type a gene name")
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Gene 2 to visualise",
          content = c(
            "Select the second gene to colour cells by gene expression",
            "- Use this to explore coexpression patterns"
          )
        )
    )
  )

  style_group <- ra_rowgroup(
    "Gene coexpression styling",
    ra_field(
      "Colour palette",
      radioButtons(
        inputId = make_id("col1"),
        label = NULL,
        choices = c(
          "Red (Gene1); Blue (Gene2)",
          "Orange (Gene1); Blue (Gene2)",
          "Red (Gene1); Green (Gene2)",
          "Green (Gene1); Blue (Gene2)"
        ),
        selected = "Red (Gene1); Blue (Gene2)"
      )
    ),
    ra_field(
      "Plot order",
      radioButtons(
        inputId = make_id("ord1"),
        label = NULL,
        choices = c("Max-1st", "Min-1st", "Original", "Random"),
        selected = "Max-1st",
        inline = TRUE
      )
    )
  )

  advanced_content <- ra_taglist(common$advanced, list(style_group))
  base_content <- ra_taglist(common$base, list(gene_group))

  adv_toggle_id <- make_id("adv")
  advanced_card <- if (!is.null(advanced_content)) {
    tags$div(
      class = "ra-card ra-controls glass-card legacy-advanced",
      ra_card_head(
        "Toggle Advanced Controls",
        "Subset cells, adjust aesthetics, and fine-tune the coexpression palette.",
        tags$div(
          class = "legacy-advanced-toggle",
          actionButton(
            inputId = adv_toggle_id,
            label = NULL,
            class = "btn btn-outline-primary ra-advanced-toggle",
            icon = icon("chevron-down")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s %% 2 == 1", adv_toggle_id),
        tags$div(
          class = "legacy-advanced-body",
          advanced_content
        )
      )
    )
  } else {
    NULL
  }

  base_card <- tags$div(
    class = "ra-card ra-controls glass-card legacy-controls",
    ra_card_head(
      sprintf("%s Controls", dataset_name),
      "Pick embeddings and genes to explore coexpression."
    ),
    tags$p(
      class = "ra-subtext",
      "Visualise overlapping and unique expression for two genes on the selected embedding."
    ),
    base_content
  )

  plot_card <- tags$div(
    class = "ra-card ra-plot glass-card legacy-plots",
    ra_card_head(
      sprintf("%s Coexpression View", dataset_name),
      "Inspect the coexpression map, legend, and supporting counts."
    ),
    ra_rowgroup(
      NULL,
      tags$div(
        class = "ra-plot-holder",
        uiOutput(make_id("oup1.ui"))
      ),
      ra_download_row(
        pdf_id = make_id("oup1.pdf"),
        png_id = make_id("oup1.png"),
        height_id = make_id("oup1.h"),
        width_id = make_id("oup1.w"),
        height_value = 6,
        width_value = 8
      )
    ),
    ra_rowgroup(
      "Legend",
      tags$div(
        class = "ra-plot-holder",
        uiOutput(make_id("oup2.ui"))
      ),
      ra_download_buttons_only(
        pdf_id = make_id("oup2.pdf"),
        png_id = make_id("oup2.png")
      )
    ),
    ra_rowgroup(
      "Cell numbers",
      dataTableOutput(make_id(".dt"))
    )
  )

  content <- tags$div(
    class = sprintf("legacy-stack %s-coexpression-stack", prefix),
    advanced_card,
    tags$div(
      class = sprintf("ra-pane legacy-pane %s-coexpression", prefix),
      base_card,
      plot_card
    )
  )

  if (as_tab) {
    tabPanel(
      title = HTML("Gene coexpression"),
      value = sprintf("%s_gene_coexpression", prefix),
      content
    )
  } else {
    content
  }
}

build_violin_boxplot_tab <- function(prefix, conf, def, dataset_name, as_tab = TRUE) {
  make_id <- function(part) paste0(prefix, "c1", part)

  inputs_group <- ra_rowgroup(
    "Value selection",
    ra_field(
      "Cell information (X-axis)",
      selectInput(
        inputId = make_id("inp1"),
        label = NULL,
        choices = conf[grp == TRUE]$UI,
        selected = def$grp1
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Cell information to group cells by",
          content = c(
            "Select categorical cell information to group cells by",
            "- Samples are grouped by this covariate on the X-axis"
          )
        )
    ),
    ra_field(
      "Cell info / gene (Y-axis)",
      selectizeInput(
        inputId = make_id("inp2"),
        label = NULL,
        choices = NULL,
        options = list(placeholder = "Type a gene or metric")
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Cell info / gene to plot",
          content = c(
            "Choose a continuous cell metric or gene expression to plot on the Y-axis",
            "- Useful for nUMI, module scores, or gene-level expression"
          )
        )
    ),
    ra_field(
      "Plot type",
      radioButtons(
        inputId = make_id("typ"),
        label = NULL,
        choices = c("violin", "boxplot"),
        selected = "violin",
        inline = TRUE
      )
    ),
    tags$div(
      class = "ra-field-checkbox",
      checkboxInput(
        inputId = make_id("pts"),
        label = "Show data points",
        value = FALSE
      )
    )
  )

  subset_group <- ra_rowgroup(
    "Subset cells",
    ra_field(
      "Cell information to subset",
      selectInput(
        inputId = make_id("sub1"),
        label = NULL,
        choices = conf[grp == TRUE]$UI,
        selected = def$grp1
      )
    ),
    uiOutput(make_id("sub1.ui"))
  )

  display_group <- ra_rowgroup(
    "Display options",
    ra_field(
      "Data point size",
      sliderInput(
        inputId = make_id("siz"),
        label = NULL,
        min = 0,
        max = 4,
        value = 1.25,
        step = 0.25
      )
    ),
    ra_field(
      "Plot size",
      radioButtons(
        inputId = make_id("psz"),
        label = NULL,
        choices = c("Small", "Medium", "Large"),
        selected = "Large",
        inline = TRUE
      )
    ),
    ra_field(
      "Font size",
      radioButtons(
        inputId = make_id("fsz"),
        label = NULL,
        choices = c("Small", "Medium", "Large"),
        selected = "Medium",
        inline = TRUE
      )
    )
  )

  base_card <- tags$div(
    class = "ra-card ra-controls glass-card legacy-controls",
    ra_card_head(
      sprintf("%s Violin / Boxplot Controls", dataset_name),
      "Configure groupings, select values, and tune plot appearance."
    ),
    tags$p(
      class = "ra-subtext",
      "Compare continuous metadata or gene expression distributions across categorical groups."
    ),
    ra_taglist(inputs_group)
  )

  advanced_content <- ra_taglist(subset_group, display_group)

  adv_toggle_id <- make_id("adv")
  advanced_card <- if (!is.null(advanced_content)) {
    tags$div(
      class = "ra-card ra-controls glass-card legacy-advanced",
      ra_card_head(
        "Toggle Advanced Controls",
        "Subset cells and adjust rendering details.",
        tags$div(
          class = "legacy-advanced-toggle",
          actionButton(
            inputId = adv_toggle_id,
            label = NULL,
            class = "btn btn-outline-primary ra-advanced-toggle",
            icon = icon("chevron-down")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s %% 2 == 1", adv_toggle_id),
        tags$div(
          class = "legacy-advanced-body",
          advanced_content
        )
      )
    )
  } else {
    NULL
  }

  plot_card <- tags$div(
    class = "ra-card ra-plot glass-card legacy-plots",
    ra_card_head(
      sprintf("%s Violin / Boxplot", dataset_name),
      "Preview the distribution plot and export images."
    ),
    ra_rowgroup(
      NULL,
      tags$div(
        class = "ra-plot-holder",
        uiOutput(make_id("oup.ui"))
      ),
      ra_download_row(
        pdf_id = make_id("oup.pdf"),
        png_id = make_id("oup.png"),
        height_id = make_id("oup.h"),
        width_id = make_id("oup.w"),
        height_value = 8,
        width_value = 10
      )
    )
  )

  content <- tags$div(
    class = sprintf("legacy-stack %s-violin-stack", prefix),
    advanced_card,
    tags$div(
      class = sprintf("ra-pane legacy-pane %s-violin", prefix),
      base_card,
      plot_card
    )
  )

  if (as_tab) {
    tabPanel(
      title = HTML("Violinplot / Boxplot"),
      value = sprintf("%s_violin_boxplot", prefix),
      content
    )
  } else {
    content
  }
}

build_proportion_plot_tab <- function(prefix, conf, def, dataset_name, as_tab = TRUE) {
  make_id <- function(part) paste0(prefix, "c2", part)

  inputs_group <- ra_rowgroup(
    "Proportion inputs",
    ra_field(
      "Cell information (X-axis)",
      selectInput(
        inputId = make_id("inp1"),
        label = NULL,
        choices = conf[grp == TRUE]$UI,
        selected = def$grp2
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Cell information to plot",
          content = c(
            "Select categorical cell information for the X-axis",
            "- Each bar represents one level of this covariate"
          )
        )
    ),
    ra_field(
      "Group / colour by",
      selectInput(
        inputId = make_id("inp2"),
        label = NULL,
        choices = conf[grp == TRUE]$UI,
        selected = def$grp1
      ) %>%
        helper(
          type = "inline",
          size = "m",
          fade = TRUE,
          title = "Cell information to colour by",
          content = c(
            "Select categorical cell information to colour cells by",
            "- Proportions or counts are split by this covariate"
          )
        )
    ),
    ra_field(
      "Plot value",
      radioButtons(
        inputId = make_id("typ"),
        label = NULL,
        choices = c("Proportion", "CellNumbers"),
        selected = "Proportion",
        inline = TRUE
      )
    ),
    tags$div(
      class = "ra-field-checkbox",
      checkboxInput(
        inputId = make_id("flp"),
        label = "Flip X/Y axes",
        value = FALSE
      )
    )
  )

  subset_group <- ra_rowgroup(
    "Subset cells",
    ra_field(
      "Cell information to subset",
      selectInput(
        inputId = make_id("sub1"),
        label = NULL,
        choices = conf[grp == TRUE]$UI,
        selected = def$grp1
      )
    ),
    uiOutput(make_id("sub1.ui"))
  )

  display_group <- ra_rowgroup(
    "Display options",
    ra_field(
      "Plot size",
      radioButtons(
        inputId = make_id("psz"),
        label = NULL,
        choices = c("Small", "Medium", "Large"),
        selected = "Large",
        inline = TRUE
      )
    ),
    ra_field(
      "Font size",
      radioButtons(
        inputId = make_id("fsz"),
        label = NULL,
        choices = c("Small", "Medium", "Large"),
        selected = "Medium",
        inline = TRUE
      )
    )
  )

  base_card <- tags$div(
    class = "ra-card ra-controls glass-card legacy-controls",
    ra_card_head(
      sprintf("%s Proportion Plot Controls", dataset_name),
      "Choose grouping variables and adjust the bar plot output."
    ),
    tags$p(
      class = "ra-subtext",
      "Quantify how categorical covariates distribute across another grouping."
    ),
    ra_taglist(inputs_group)
  )

  advanced_content <- ra_taglist(subset_group, display_group)

  adv_toggle_id <- make_id("adv")
  advanced_card <- if (!is.null(advanced_content)) {
    tags$div(
      class = "ra-card ra-controls glass-card legacy-advanced",
      ra_card_head(
        "Toggle Advanced Controls",
        "Filter cells and refine the bar chart presentation.",
        tags$div(
          class = "legacy-advanced-toggle",
          actionButton(
            inputId = adv_toggle_id,
            label = NULL,
            class = "btn btn-outline-primary ra-advanced-toggle",
            icon = icon("chevron-down")
          )
        )
      ),
      conditionalPanel(
        condition = sprintf("input.%s %% 2 == 1", adv_toggle_id),
        tags$div(
          class = "legacy-advanced-body",
          advanced_content
        )
      )
    )
  } else {
    NULL
  }

  plot_card <- tags$div(
    class = "ra-card ra-plot glass-card legacy-plots",
    ra_card_head(
      sprintf("%s Proportion Plot", dataset_name),
      "View proportional or absolute cell counts across groups."
    ),
    ra_rowgroup(
      NULL,
      tags$div(
        class = "ra-plot-holder",
        uiOutput(make_id("oup.ui"))
      ),
      ra_download_row(
        pdf_id = make_id("oup.pdf"),
        png_id = make_id("oup.png"),
        height_id = make_id("oup.h"),
        width_id = make_id("oup.w"),
        height_value = 8,
        width_value = 10
      )
    )
  )

  content <- tags$div(
    class = sprintf("legacy-stack %s-proportion-stack", prefix),
    advanced_card,
    tags$div(
      class = sprintf("ra-pane legacy-pane %s-proportion", prefix),
      base_card,
      plot_card
    )
  )

  if (as_tab) {
    tabPanel(
      title = HTML("Proportion plot"),
      value = sprintf("%s_proportion_plot", prefix),
      content
    )
  } else {
    content
  }
}
