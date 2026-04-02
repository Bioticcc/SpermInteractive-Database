library(Seurat)
library(ShinyCell)

# RUN IN R TO RUN LOCALLY:
# shiny::runApp("ShinyApp/shinyApp") 


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

# ---- Release metadata: edit these before deploying ----
release_version_number <- "0.6.4"
release_update_type <- "Minor"   # "Minor" or "Major"
release_update_title <- "Added custom expression levels to Spermatogenesis table"
release_update_description <- paste(
  "Added a bar that users can input a custom threshold for gene expression levels into. Doesnt require clicking search or a table refresh, just type in the number and itll change. ",
  sep = "\n\n"
)

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

  list(
    version_number = version_number,
    update_type = update_type,
    update_title = update_title,
    update_description = update_description
  )
}

empty_release_notes <- function() {
  data.frame(
    version_number = character(0),
    update_type = character(0),
    update_title = character(0),
    update_description = character(0),
    update_date = character(0),
    stringsAsFactors = FALSE
  )
}

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
  notes
}

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
  invisible(notes)
}

# Keep the local app preview in sync with the release metadata you are preparing.
sync_release_notes()

# RUN THIS ONE IN BASH TO TEST MEMORY USAGE:
# Rscript ShinyApp/shinyApp/bench_memory_usage.R

#------DEPLOY TO SHINYAPPS.IO------->
# if (interactive()) {
#   if (!requireNamespace("rsconnect", quietly = TRUE)) {
#     install.packages("rsconnect")
#   }
#   rsconnect::setAccountInfo(name='ward-bio', token='NUH-UH', secret='ItsASecret :P')
#
#   # Just rerun this when making updates.
  sync_release_notes(update_date = Sys.Date())
  rsconnect::deployApp(appDir = app_dir, appName = "SpermInteractive", forceUpdate = TRUE)
# }
#--------------------------------------->
