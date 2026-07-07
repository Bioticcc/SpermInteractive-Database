# ---------------------------------------------------------------------------
# SpermInteractive shared support helpers
# ---------------------------------------------------------------------------
# This file is sourced by the app runtime and by build/maintenance scripts. Keep
# helpers here free of Shiny session state so scripts can reuse path discovery,
# lazy loading, and data-root conventions without sourcing ui.R or server.R.

# ---------------------------------------------------------------------------
# Small value and path predicates
# ---------------------------------------------------------------------------
sc_default <- function(value, fallback) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    return(fallback)
  }
  return(value)
}

sc_is_absolute_path <- function(path) {
  return(grepl("^(/|~|[A-Za-z]:[/\\\\])", path))
}

# Resolve the directory of the currently executing script when Rscript supplies
# --file=..., with a working-directory fallback for interactive sessions.
sc_script_dir <- function(default = getwd()) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)))
  }

  frame_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(...) NULL)
  if (!is.null(frame_path) && nzchar(frame_path)) {
    return(dirname(normalizePath(frame_path, mustWork = FALSE)))
  }

  return(normalizePath(default, mustWork = FALSE))
}

# ---------------------------------------------------------------------------
# App-root discovery
# ---------------------------------------------------------------------------
# The app root is identified by the runtime entrypoints plus the tab-builder
# module. This avoids accidentally treating repo root or a scripts/ folder as
# the Shiny application directory.
sc_is_app_dir <- function(path) {
  return(dir.exists(path) &&
    file.exists(file.path(path, "ui.R")) &&
    file.exists(file.path(path, "server.R")) &&
    file.exists(file.path(path, "dataset_tab_builders.R")) &&
    dir.exists(file.path(path, "www")))
}

# Walk upward from a script or working directory and also check the repo's
# conventional ShinyApp/shinyApp child path.
sc_find_app_dir <- function(start = getwd()) {
  if (!dir.exists(start) && file.exists(start)) {
    start <- dirname(start)
  }

  start <- normalizePath(start, mustWork = FALSE)
  candidates <- character(0)
  cursor <- start

  repeat {
    candidates <- c(candidates, cursor, file.path(cursor, "ShinyApp", "shinyApp"))
    parent <- dirname(cursor)
    if (identical(parent, cursor)) {
      break
    }
    cursor <- parent
  }

  candidates <- unique(candidates)
  for (candidate in candidates) {
    if (sc_is_app_dir(candidate)) {
      return(normalizePath(candidate, mustWork = FALSE))
    }
  }

  stop(
    sprintf(
      "Unable to locate the Shiny app directory from '%s'.",
      start
    ),
    call. = FALSE
  )
}

# Cache the app root in an option so repeated helper calls do not rediscover it.
sc_set_app_dir <- function(path) {
  normalized <- normalizePath(path, mustWork = FALSE)
  options(sperminteractive.app_dir = normalized)
  return(invisible(normalized))
}

sc_get_app_dir <- function(start = NULL, refresh = FALSE) {
  cached <- getOption("sperminteractive.app_dir")
  if (!isTRUE(refresh) && !is.null(cached) && dir.exists(cached)) {
    return(normalizePath(cached, mustWork = FALSE))
  }

  app_dir <- sc_find_app_dir(sc_default(start, getwd()))
  return(sc_set_app_dir(app_dir))
}

# ---------------------------------------------------------------------------
# App-relative path helpers
# ---------------------------------------------------------------------------
# Candidate paths are resolved both as supplied and relative to the app root so
# scripts can be launched from repo root, app root, or their own folder.
sc_resolve_paths <- function(paths, app_dir = sc_get_app_dir()) {
  resolved <- character(0)
  for (path in paths) {
    if (is.null(path) || is.na(path) || !nzchar(path)) {
      next
    }

    if (sc_is_absolute_path(path)) {
      resolved <- c(resolved, normalizePath(path, mustWork = FALSE))
      next
    }

    resolved <- c(
      resolved,
      normalizePath(path, mustWork = FALSE),
      normalizePath(file.path(app_dir, path), mustWork = FALSE)
    )
  }

  return(unique(resolved))
}

# Return the first real file or directory from a candidate list. This keeps
# fallback ordering explicit at call sites while centralizing the error message.
sc_first_existing <- function(paths, app_dir = sc_get_app_dir()) {
  resolved <- sc_resolve_paths(paths, app_dir = app_dir)
  existing <- resolved[file.exists(resolved) | dir.exists(resolved)]
  if (!length(existing)) {
    stop(
      sprintf("None of the candidate paths exist: %s", paste(paths, collapse = ", ")),
      call. = FALSE
    )
  }
  return(existing[[1]])
}

sc_app_path <- function(..., app_dir = sc_get_app_dir()) {
  return(normalizePath(file.path(app_dir, ...), mustWork = FALSE))
}

sc_www_path <- function(..., app_dir = sc_get_app_dir()) {
  return(sc_app_path("www", ..., app_dir = app_dir))
}

# Data assets live outside www/ so they can be loaded by R without exposing them
# as static browser-downloadable files.
sc_data_path <- function(..., app_dir = sc_get_app_dir()) {
  return(sc_app_path("Data", ..., app_dir = app_dir))
}

sc_data_dir <- function(app_dir = sc_get_app_dir(), create = FALSE) {
  path <- sc_data_path(app_dir = app_dir)
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  return(path)
}

# Prefer Data/ assets while retaining legacy app-root fallbacks during the
# migration away from loose runtime files.
sc_data_candidates <- function(paths, include_legacy = TRUE) {
  candidates <- character(0)
  for (path in paths) {
    if (is.null(path) || is.na(path) || !nzchar(path)) {
      next
    }

    if (sc_is_absolute_path(path) || startsWith(path, "Data/") || startsWith(path, "Data\\")) {
      candidates <- c(candidates, path)
      next
    }

    candidates <- c(candidates, file.path("Data", path))
    if (isTRUE(include_legacy)) {
      candidates <- c(candidates, path)
    }
  }
  return(unique(candidates))
}

# ---------------------------------------------------------------------------
# Spinner wrappers
# ---------------------------------------------------------------------------
# Central loading spinner definition shared by every slow plot/figure output.
sc_spinner_config <- function(overrides = list()) {
  base <- list(
    type = getOption("sperminteractive.spinner.type", 8),
    color = getOption("sperminteractive.spinner.color", "#4F46E5"),
    color.background = getOption("sperminteractive.spinner.background", "transparent")
  )
  return(utils::modifyList(base, overrides))
}

sc_with_spinner <- function(ui, proxy.height = NULL, ...) {
  spinner_args <- sc_spinner_config(list(...))
  if (!is.null(proxy.height)) {
    spinner_args$proxy.height <- proxy.height
  }
  spinner_args <- c(list(ui), spinner_args)
  return(do.call(shinycssloaders::withSpinner, spinner_args))
}

sc_spinner_plot_output <- function(output_id,
                                   height = "400px",
                                   width = "100%",
                                   proxy.height = NULL,
                                   ...) {
  return(sc_with_spinner(
    shiny::plotOutput(output_id, height = height, width = width),
    proxy.height = sc_default(proxy.height, height),
    ...
  ))
}

sc_spinner_plotly_output <- function(output_id,
                                     height = "400px",
                                     width = "100%",
                                     proxy.height = NULL,
                                     ...) {
  return(sc_with_spinner(
    plotly::plotlyOutput(output_id, height = height, width = width),
    proxy.height = sc_default(proxy.height, height),
    ...
  ))
}

sc_spinner_ui_output <- function(output_id, proxy.height = NULL, ...) {
  return(sc_with_spinner(
    shiny::uiOutput(output_id),
    proxy.height = proxy.height,
    ...
  ))
}

# ---------------------------------------------------------------------------
# Script and lazy-loading helpers
# ---------------------------------------------------------------------------
# Normalize user-provided output directories against the app root. Existing
# relative directories win over speculative app-relative paths.
sc_dir_arg <- function(path, app_dir = sc_get_app_dir()) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    return(normalizePath(app_dir, mustWork = FALSE))
  }
  if (sc_is_absolute_path(path)) {
    return(normalizePath(path, mustWork = FALSE))
  }

  resolved <- sc_resolve_paths(c(path), app_dir = app_dir)
  existing_dirs <- resolved[dir.exists(resolved)]
  if (length(existing_dirs)) {
    return(existing_dirs[[1]])
  }

  return(normalizePath(file.path(app_dir, path), mustWork = FALSE))
}

# Source app-local modules from scripts or runtime entrypoints without assuming
# the current working directory is the app directory.
sc_source <- function(path, local = parent.frame(), ..., app_dir = sc_get_app_dir()) {
  return(source(sc_first_existing(c(path), app_dir = app_dir), local = local, ...))
}

# Return a zero-argument loader that reads a heavy asset only on first use.
sc_lazy_loader <- function(paths, reader = readRDS, postprocess = identity, app_dir = sc_get_app_dir()) {
  cache <- NULL
  return(function() {
    if (is.null(cache)) {
      path <- sc_first_existing(paths, app_dir = app_dir)
      cache <<- postprocess(reader(path))
    }
    return(cache)
  })
}

# Return a zero-argument resolver for path-only assets such as HDF5 matrices.
sc_lazy_path <- function(paths, app_dir = sc_get_app_dir()) {
  cached_path <- NULL
  return(function() {
    if (is.null(cached_path)) {
      cached_path <<- sc_first_existing(paths, app_dir = app_dir)
    }
    return(cached_path)
  })
}

# ---------------------------------------------------------------------------
# Gene-index normalization
# ---------------------------------------------------------------------------
# Uploaded or generated gene indices may arrive as named vectors, character
# vectors, lists, or data frames. Normalize them into a named integer index so
# ShinyCell and custom upload paths can share one lookup contract.
sc_normalize_gene_index <- function(gene_data) {
  if (is.null(gene_data)) {
    return(NULL)
  }

  extract_first <- function(data, fields) {
    names_lower <- tolower(names(data))
    idx <- match(fields, names_lower, nomatch = 0)
    idx <- idx[idx > 0]
    if (!length(idx)) {
      return(NULL)
    }
    return(names(data)[idx[1]])
  }

  finalize_mapping <- function(genes, idx) {
    genes <- trimws(as.character(genes))
    keep <- !is.na(genes) & nzchar(genes)
    genes <- genes[keep]
    idx <- idx[keep]
    idx <- as.integer(idx)
    if (!length(genes)) {
      stop("No genes detected in uploaded gene index.", call. = FALSE)
    }
    if (!length(idx) || length(idx) != length(genes)) {
      idx <- seq_along(genes)
    }
    if (anyDuplicated(genes)) {
      genes <- make.unique(genes)
    }
    names(idx) <- genes
    return(idx)
  }

  if (is.atomic(gene_data)) {
    nm <- names(gene_data)
    if (!is.null(nm) && any(nzchar(nm))) {
      return(gene_data)
    }
    if (is.character(gene_data)) {
      return(finalize_mapping(gene_data, seq_along(gene_data)))
    }
    stop("Uploaded gene index must include gene names.", call. = FALSE)
  }

  if (is.list(gene_data) && !is.data.frame(gene_data)) {
    possible <- c("gene", "genes", "gene_name", "name", "symbol", "feature")
    idx_fields <- c("index", "idx", "gene_index", "gene_idx", "id", "gene_id", "geneid", "feature_id")
    names_lower <- tolower(names(gene_data))
    gene_field <- names(gene_data)[match(possible, names_lower, nomatch = 0)]
    gene_field <- gene_field[nzchar(gene_field)]
    idx_field <- names(gene_data)[match(idx_fields, names_lower, nomatch = 0)]
    idx_field <- idx_field[nzchar(idx_field)]
    if (length(gene_field)) {
      genes <- gene_data[[gene_field[1]]]
      gene_idx <- if (length(idx_field)) gene_data[[idx_field[1]]] else seq_along(genes)
      return(finalize_mapping(genes, gene_idx))
    }
    gene_data <- tryCatch(as.data.frame(gene_data, stringsAsFactors = FALSE), error = function(...) NULL)
    if (is.null(gene_data)) {
      stop("Unable to parse uploaded gene index.", call. = FALSE)
    }
  }

  if (is.data.frame(gene_data)) {
    colnames(gene_data) <- trimws(colnames(gene_data))
    name_fields <- c("gene", "genes", "gene_name", "name", "symbol", "feature", "feature_name")
    idx_fields <- c("index", "idx", "gene_index", "gene_idx", "id", "gene_id", "geneid", "feature_id")
    gene_col <- extract_first(gene_data, tolower(name_fields))
    idx_col <- extract_first(gene_data, tolower(idx_fields))
    if (is.null(gene_col)) {
      stop("Gene index table must contain a column with gene names.", call. = FALSE)
    }
    genes <- gene_data[[gene_col]]
    idx <- if (!is.null(idx_col)) gene_data[[idx_col]] else seq_along(genes)
    return(finalize_mapping(genes, idx))
  }

  stop("Unsupported gene index format.", call. = FALSE)
}
