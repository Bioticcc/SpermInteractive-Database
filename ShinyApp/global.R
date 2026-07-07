library(Seurat)
library(ShinyCell)

# ---------------------------------------------------------------------------
# SpermInteractive bootstrap and deployment helper
# ---------------------------------------------------------------------------
# This script is intended to be sourced manually when preparing a release. It
# refreshes the app's release notes from the metadata below and, when enabled,
# deploys the Shiny application to shinyapps.io.
#
# Local preview command:
# shiny::runApp("ShinyApp/shinyApp")
# Release-note sync and deployment command:
# source("ShinyApp/global.R")

# Resolve paths relative to the script location when sourced, while still
# supporting interactive use from either the repository root or ShinyApp/.
bootstrap_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(...) NULL)
project_dir <- if (!is.null(bootstrap_path) && nzchar(bootstrap_path)) {
  dirname(normalizePath(bootstrap_path, mustWork = FALSE))
} else if (dir.exists("shinyApp")) {
  normalizePath(".", mustWork = FALSE)
} else {
  normalizePath("ShinyApp", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(project_dir, "shinyApp"), mustWork = FALSE)
release_notes_file <- normalizePath(file.path(app_dir, "release_notes.csv"), mustWork = FALSE)

# ---------------------------------------------------------------------------
# Release metadata
# ---------------------------------------------------------------------------
# Release metadata: edit these values before deployment.
release_version_number <- "0.9.1"
release_update_type <- "Major" # "Minor" or "Major"
release_update_title <- "Tutorial Update"
release_update_description <- paste(
  c(
    "* Added a tutorial, allowing users to move through a series of tutorial steps to familiarize themselves with the website, and how to use and navigate it.",
    "* Fixed various popup bugs, mainly related to spam clicking a popup button like the Download Figures button while a page was still refreshing.",
    "* Additional minor bug fixes and improvements."
  ),
  collapse = "\n\n"
)

# Validate the user-edited release metadata before it is written to
# release_notes.csv. Minor releases intentionally omit titles; major releases
# require them so the release notes have a stable heading.
validate_release_inputs <- function(version_number,
                                    update_type,
                                    update_title,
                                    update_description) {
  version_number <- trimws(as.character(version_number))
  update_type <- trimws(as.character(update_type))
  update_title <- trimws(as.character(update_title))
  update_description <- trimws(as.character(update_description))

  if (!nzchar(version_number)) {
    stop("release_version_number must not be empty.", call. = FALSE)
  }

  if (!update_type %in% c("Minor", "Major")) {
    stop("release_update_type must be either 'Minor' or 'Major'.", call. = FALSE)
  }

  if (!nzchar(update_description)) {
    stop("release_update_description must not be empty.", call. = FALSE)
  }

  if (identical(update_type, "Minor")) {
    update_title <- ""
  }

  if (identical(update_type, "Major") && !nzchar(update_title)) {
    stop("release_update_title is required for Major updates.", call. = FALSE)
  }

  return(list(
    version_number = version_number,
    update_type = update_type,
    update_title = update_title,
    update_description = update_description
  ))
}

# Return an empty release-note table with the exact schema expected by the app.
empty_release_notes <- function() {
  return(data.frame(
    version_number = character(0),
    update_type = character(0),
    update_title = character(0),
    update_description = character(0),
    update_date = character(0),
    stringsAsFactors = FALSE
  ))
}

# Read existing release notes defensively. Missing, unreadable, or schema-mismatched
# files are treated as empty so a release command can rebuild the expected CSV.
read_existing_release_notes <- function(path = release_notes_file) {
  required_cols <- c(
    "version_number",
    "update_type",
    "update_title",
    "update_description",
    "update_date"
  )

  if (!file.exists(path)) {
    return(empty_release_notes())
  }

  notes <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) empty_release_notes()
  )

  if (!all(required_cols %in% names(notes))) {
    return(empty_release_notes())
  }

  notes <- notes[, required_cols, drop = FALSE]
  notes[] <- lapply(notes, function(x) as.character(x))
  return(notes)
}

# Insert the current release at the top of release_notes.csv and remove any
# older row for the same version number to keep the file idempotent.
sync_release_notes <- function(update_date = Sys.Date(),
                               path = release_notes_file) {
  entry <- validate_release_inputs(
    version_number = release_version_number,
    update_type = release_update_type,
    update_title = release_update_title,
    update_description = release_update_description
  )

  entry_df <- data.frame(
    version_number = entry$version_number,
    update_type = entry$update_type,
    update_title = entry$update_title,
    update_description = entry$update_description,
    update_date = as.character(update_date),
    stringsAsFactors = FALSE
  )

  existing <- read_existing_release_notes(path)
  if (nrow(existing)) {
    existing <- existing[existing$version_number != entry$version_number, , drop = FALSE]
  }

  notes <- rbind(entry_df, existing)
  write.csv(notes, path, row.names = FALSE, na = "")
  return(invisible(notes))
}

# Keep the local app preview in sync with the release metadata you are preparing.
# This writes release_notes.csv even when deployment is disabled.
sync_release_notes()

# Memory benchmark command:
# Rscript ShinyApp/shinyApp/scripts/benchmarks/bench_memory_usage.R

# ---------------------------------------------------------------------------
# Optional shinyapps.io deployment
# ---------------------------------------------------------------------------
# Deploy to shinyapps.io when explicitly enabled above.
# Set to TRUE only when you intentionally want to deploy.
deploy_to_shinyapps <- TRUE

if (isTRUE(deploy_to_shinyapps)) {
  # Install rsconnect on demand for release environments that have not already
  # prepared the deployment dependency.
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    install.packages("rsconnect")
  }
  # Re-sync immediately before deployment so the hosted copy contains today's
  # release-note date.
  sync_release_notes(update_date = Sys.Date())
  rsconnect::deployApp(appDir = app_dir, appName = "SpermInteractive", forceUpdate = TRUE)
}
