# Benchmark Firefox/DevTools HAR exports for the published home route.
# Run from the repo root with:
#   Rscript ShinyApp/shinyApp/bench_network_har.R path/to/home_cold_1.har path/to/home_cold_2.har
#
# Filenames containing "cold" or "warm" are grouped automatically so the script
# can report per-scenario medians across repeated runs.

suppressPackageStartupMessages({
  library(jsonlite)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

usage <- function() {
  stop(
    paste(
      "Usage:",
      "Rscript ShinyApp/shinyApp/bench_network_har.R",
      "path/to/home_cold_1.har [path/to/home_cold_2.har ...]",
      sep = "\n  "
    ),
    call. = FALSE
  )
}

safe_num <- function(x, default = 0) {
  value <- suppressWarnings(as.numeric(x)[1])
  if (is.na(value)) {
    return(default)
  }
  value
}

safe_chr <- function(x, default = "") {
  value <- as.character(x)[1]
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  value
}

format_bytes <- function(bytes) {
  bytes <- safe_num(bytes, default = 0)
  units <- c("B", "KB", "MB", "GB")
  scale <- 1L
  while (bytes >= 1024 && scale < length(units)) {
    bytes <- bytes / 1024
    scale <- scale + 1L
  }
  sprintf("%.1f %s", bytes, units[[scale]])
}

detect_scenario <- function(path) {
  name <- tolower(basename(path))
  if (grepl("cold", name, fixed = TRUE)) {
    return("cold")
  }
  if (grepl("warm", name, fixed = TRUE)) {
    return("warm")
  }
  "unlabeled"
}

normalize_url <- function(url) {
  url <- safe_chr(url)
  url <- sub("#.*$", "", url)
  sub("\\?.*$", "", url)
}

entry_mime_type <- function(entry) {
  tolower(safe_chr(entry$response$content$mimeType))
}

entry_transfer_bytes <- function(entry) {
  response <- entry$response %||% list()
  response_transfer <- response[["_transferSize"]]
  entry_transfer <- entry[["_transferSize"]]
  for (candidate in list(response_transfer, entry_transfer)) {
    value <- safe_num(candidate, default = NA_real_)
    if (!is.na(value) && value >= 0) {
      return(value)
    }
  }

  headers <- safe_num(response$headersSize, default = 0)
  body <- safe_num(response$bodySize, default = 0)
  total <- max(headers, 0) + max(body, 0)
  if (total > 0) {
    return(total)
  }

  0
}

entry_ttfb_ms <- function(entry) {
  timings <- entry$timings %||% list()
  components <- c(
    blocked = safe_num(timings$blocked, default = 0),
    dns = safe_num(timings$dns, default = 0),
    connect = safe_num(timings$connect, default = 0),
    ssl = safe_num(timings$ssl, default = 0),
    send = safe_num(timings$send, default = 0),
    wait = safe_num(timings$wait, default = 0)
  )
  components[components < 0] <- 0
  total <- sum(components)
  if (total > 0) {
    return(total)
  }

  receive <- safe_num(timings$receive, default = 0)
  request_time <- safe_num(entry$time, default = 0)
  max(request_time - max(receive, 0), 0)
}

entry_category <- function(entry) {
  url <- tolower(normalize_url(entry$request$url))
  mime <- entry_mime_type(entry)

  if (startsWith(mime, "text/html")) {
    return("document")
  }
  if (grepl("\\.css$", url) || startsWith(mime, "text/css")) {
    return("css")
  }
  if (grepl("\\.js$", url) || grepl("javascript", mime, fixed = TRUE)) {
    return("js")
  }
  if (grepl("\\.(png|jpe?g|gif|svg|webp|ico)$", url) || startsWith(mime, "image/")) {
    return("image")
  }
  if (grepl("sockjs|shiny-server-client|/websocket|websocket", url)) {
    return("bootstrap")
  }
  "other"
}

entry_row <- function(entry) {
  url <- safe_chr(entry$request$url)
  total_ms <- safe_num(entry$time, default = 0)
  transfer_bytes <- entry_transfer_bytes(entry)
  category <- entry_category(entry)

  data.frame(
    started = safe_chr(entry$startedDateTime),
    method = safe_chr(entry$request$method),
    url = url,
    normalized_url = normalize_url(url),
    mime_type = entry_mime_type(entry),
    status = safe_num(entry$response$status, default = 0),
    request_time_ms = total_ms,
    ttfb_ms = entry_ttfb_ms(entry),
    transfer_bytes = transfer_bytes,
    category = category,
    is_bootstrap = identical(category, "bootstrap"),
    stringsAsFactors = FALSE
  )
}

read_har_entries <- function(path) {
  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  entries <- payload$log$entries
  if (is.null(entries) || !length(entries)) {
    stop(sprintf("No HAR entries found in %s", path), call. = FALSE)
  }

  rows <- lapply(entries, entry_row)
  out <- do.call(rbind, rows)
  out$file <- normalizePath(path, mustWork = FALSE)
  out$scenario <- detect_scenario(path)
  out
}

metric_row <- function(entries, category) {
  rows <- entries[entries$category == category, , drop = FALSE]
  data.frame(
    request_count = nrow(rows),
    transfer_bytes = sum(rows$transfer_bytes, na.rm = TRUE),
    request_time_ms = sum(rows$request_time_ms, na.rm = TRUE),
    ttfb_ms = sum(rows$ttfb_ms, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

top_requests <- function(entries, limit = 10L) {
  rows <- entries[order(-entries$transfer_bytes, -entries$request_time_ms), , drop = FALSE]
  head(rows[, c("status", "transfer_bytes", "request_time_ms", "ttfb_ms", "category", "url")], limit)
}

summarize_har <- function(path) {
  entries <- read_har_entries(path)
  document_rows <- entries[entries$category == "document", , drop = FALSE]
  document_row <- if (nrow(document_rows)) {
    document_rows[order(document_rows$started), , drop = FALSE][1, , drop = FALSE]
  } else {
    entries[order(entries$started), , drop = FALSE][1, , drop = FALSE]
  }

  metrics <- list(
    file = normalizePath(path, mustWork = FALSE),
    scenario = detect_scenario(path),
    document_url = document_row$normalized_url[[1]],
    document_ttfb_ms = document_row$ttfb_ms[[1]],
    total_requests = nrow(entries),
    total_transferred_bytes = sum(entries$transfer_bytes, na.rm = TRUE),
    css = metric_row(entries, "css"),
    js = metric_row(entries, "js"),
    image = metric_row(entries, "image"),
    bootstrap = metric_row(entries, "bootstrap"),
    top_requests = top_requests(entries, limit = 10L)
  )
  metrics
}

print_metric_block <- function(label, metrics) {
  cat(sprintf("%-12s requests: %3d | bytes: %10s | total ms: %8.1f | TTFB ms: %8.1f\n",
              paste0(label, ":"),
              metrics$request_count[[1]],
              format_bytes(metrics$transfer_bytes[[1]]),
              metrics$request_time_ms[[1]],
              metrics$ttfb_ms[[1]]))
}

print_summary <- function(summary) {
  cat(sprintf("\n== %s ==\n", basename(summary$file)))
  cat(sprintf("scenario:            %s\n", summary$scenario))
  cat(sprintf("document:            %s\n", summary$document_url))
  cat(sprintf("document TTFB:       %.1f ms\n", summary$document_ttfb_ms))
  cat(sprintf("total requests:      %d\n", summary$total_requests))
  cat(sprintf("total transferred:   %s\n", format_bytes(summary$total_transferred_bytes)))

  print_metric_block("css", summary$css)
  print_metric_block("js", summary$js)
  print_metric_block("image", summary$image)
  print_metric_block("bootstrap", summary$bootstrap)

  cat("\ntop requests by transferred bytes:\n")
  rows <- summary$top_requests
  if (!nrow(rows)) {
    cat("  (no requests)\n")
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    cat(sprintf(
      "  %2d. [%3d] %-10s %10s | %7.1f ms | %s\n",
      i,
      row$status[[1]],
      row$category[[1]],
      format_bytes(row$transfer_bytes[[1]]),
      row$request_time_ms[[1]],
      row$url[[1]]
    ))
  }
}

scenario_medians <- function(summaries) {
  scenarios <- unique(vapply(summaries, `[[`, character(1), "scenario"))
  scenarios <- scenarios[scenarios != "unlabeled"]
  if (!length(scenarios)) {
    return(invisible(NULL))
  }

  cat("\n== Scenario Medians ==\n")
  for (scenario in scenarios) {
    subset <- summaries[vapply(summaries, function(x) identical(x$scenario, scenario), logical(1))]
    if (!length(subset)) {
      next
    }

    metric_vector <- function(path) {
      vapply(subset, function(x) path(x), numeric(1))
    }

    cat(sprintf("\n%s (%d runs)\n", scenario, length(subset)))
    cat(sprintf("  document TTFB:     %.1f ms\n", median(metric_vector(function(x) x$document_ttfb_ms))))
    cat(sprintf("  total requests:    %.0f\n", median(metric_vector(function(x) x$total_requests))))
    cat(sprintf("  total transferred: %s\n", format_bytes(median(metric_vector(function(x) x$total_transferred_bytes)))))

    print_scenario_block <- function(label, extractor) {
      cat(sprintf(
        "  %-12s requests: %3.0f | bytes: %10s | total ms: %8.1f | TTFB ms: %8.1f\n",
        paste0(label, ":"),
        median(metric_vector(function(x) extractor(x)$request_count[[1]])),
        format_bytes(median(metric_vector(function(x) extractor(x)$transfer_bytes[[1]]))),
        median(metric_vector(function(x) extractor(x)$request_time_ms[[1]])),
        median(metric_vector(function(x) extractor(x)$ttfb_ms[[1]]))
      ))
    }

    print_scenario_block("css", function(x) x$css)
    print_scenario_block("js", function(x) x$js)
    print_scenario_block("image", function(x) x$image)
    print_scenario_block("bootstrap", function(x) x$bootstrap)
  }
}

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  usage()
}

paths <- normalizePath(args, mustWork = FALSE)
missing_paths <- paths[!file.exists(paths)]
if (length(missing_paths)) {
  stop(sprintf("HAR file(s) not found: %s", paste(missing_paths, collapse = ", ")), call. = FALSE)
}

summaries <- lapply(paths, summarize_har)
for (summary in summaries) {
  print_summary(summary)
}
scenario_medians(summaries)
