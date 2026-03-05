`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) {
    return(rhs)
  }
  lhs
}

trim_chr <- function(x) {
  trimws(as.character(x))
}

read_metadata_xlsx <- function(path) {
  tmp_dir <- tempfile("metadata_xlsx_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  needed <- c("xl/sharedStrings.xml", "xl/worksheets/sheet1.xml")
  utils::unzip(path, files = needed, exdir = tmp_dir)

  ns <- c(d1 = "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
  shared_strings <- character(0)

  shared_path <- file.path(tmp_dir, "xl", "sharedStrings.xml")
  if (file.exists(shared_path)) {
    shared_xml <- xml2::read_xml(shared_path)
    si_nodes <- xml2::xml_find_all(shared_xml, ".//d1:si", ns = ns)
    shared_strings <- vapply(
      si_nodes,
      function(node) {
        paste(xml2::xml_text(xml2::xml_find_all(node, ".//d1:t", ns = ns)), collapse = "")
      },
      character(1)
    )
  }

  sheet_path <- file.path(tmp_dir, "xl", "worksheets", "sheet1.xml")
  if (!file.exists(sheet_path)) {
    return(data.frame(old = character(0), new = character(0), stringsAsFactors = FALSE))
  }

  sheet_xml <- xml2::read_xml(sheet_path)
  all_ref_nodes <- xml2::xml_find_all(sheet_xml, ".//d1:sheetData/d1:row/d1:c", ns = ns)
  all_cols <- sub("[0-9]+$", "", xml2::xml_attr(all_ref_nodes, "r"))
  all_cols <- unique(all_cols[!is.na(all_cols) & nzchar(all_cols)])
  col_to_num <- function(col) {
    chars <- strsplit(toupper(col), "")[[1]]
    total <- 0L
    for (ch in chars) {
      total <- total * 26L + (match(ch, LETTERS) - 1L + 1L)
    }
    total
  }
  if (!length(all_cols)) {
    return(data.frame(old = character(0), new = character(0), stringsAsFactors = FALSE))
  }
  all_cols <- all_cols[order(vapply(all_cols, col_to_num, integer(1)))]
  target_cols <- all_cols[seq_len(min(2L, length(all_cols)))]

  row_nodes <- xml2::xml_find_all(sheet_xml, ".//d1:sheetData/d1:row", ns = ns)

  old_vals <- character(0)
  new_vals <- character(0)

  for (row_node in row_nodes) {
    row_vals <- c(NA_character_, NA_character_)
    cell_nodes <- xml2::xml_find_all(row_node, "./d1:c", ns = ns)
    for (cell_node in cell_nodes) {
      ref <- xml2::xml_attr(cell_node, "r") %||% ""
      col <- sub("[0-9]+$", "", ref)
      col_idx <- match(col, target_cols)
      if (is.na(col_idx)) {
        next
      }

      cell_type <- xml2::xml_attr(cell_node, "t")
      value <- NA_character_

      if (!is.na(cell_type) && identical(cell_type, "inlineStr")) {
        value <- paste(xml2::xml_text(xml2::xml_find_all(cell_node, ".//d1:t", ns = ns)), collapse = "")
      } else {
        value_node <- xml2::xml_find_first(cell_node, "./d1:v", ns = ns)
        if (inherits(value_node, "xml_missing")) {
          next
        }
        raw_val <- xml2::xml_text(value_node)
        value <- raw_val
        if (!is.na(cell_type) && identical(cell_type, "s")) {
          idx <- suppressWarnings(as.integer(raw_val)) + 1L
          if (!is.na(idx) && idx >= 1L && idx <= length(shared_strings)) {
            value <- shared_strings[[idx]]
          }
        }
      }

      row_vals[col_idx] <- value
    }

    old_vals <- c(old_vals, row_vals[1])
    new_vals <- c(new_vals, row_vals[2])
  }

  data.frame(old = old_vals, new = new_vals, stringsAsFactors = FALSE)
}

normalize_metadata_rules <- function(raw_df) {
  if (is.null(raw_df) || nrow(raw_df) == 0 || ncol(raw_df) < 2) {
    return(data.frame(old = character(0), new = character(0), stringsAsFactors = FALSE))
  }

  rules <- data.frame(
    old = trim_chr(raw_df[[1]]),
    new = trim_chr(raw_df[[2]]),
    stringsAsFactors = FALSE
  )

  keep <- !is.na(rules$old) & nzchar(rules$old)
  rules <- rules[keep, , drop = FALSE]
  rules$new[is.na(rules$new) | !nzchar(rules$new)] <- NA_character_
  rules <- rules[!(tolower(rules$old) == "remove" & is.na(rules$new)), , drop = FALSE]

  key <- tolower(rules$old)
  rules <- rules[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  rownames(rules) <- NULL
  rules
}

read_metadata_rules <- function(base_dir = "www") {
  csv_path <- file.path(base_dir, "metadata.csv")
  xlsx_path <- file.path(base_dir, "metadata.xlsx")

  raw_df <- NULL
  if (file.exists(csv_path)) {
    raw_df <- tryCatch(
      utils::read.csv(csv_path, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
  } else if (file.exists(xlsx_path)) {
    raw_df <- tryCatch(
      read_metadata_xlsx(xlsx_path),
      error = function(e) {
        warning(sprintf("Unable to read metadata overrides from '%s': %s", xlsx_path, e$message))
        NULL
      }
    )
  }

  normalize_metadata_rules(raw_df)
}

get_metadata_overrides <- local({
  cache <- NULL
  function(force = FALSE, base_dir = "www") {
    if (isTRUE(force) || is.null(cache)) {
      cache <<- read_metadata_rules(base_dir = base_dir)
    }
    cache
  }
})

lookup_metadata_override <- function(id, rules) {
  if (is.null(id) || is.na(id) || !nzchar(id) || is.null(rules) || nrow(rules) == 0) {
    return(NA_character_)
  }

  map <- stats::setNames(rules$new, tolower(rules$old))
  keys <- c(id)
  if (grepl("\\.[0-9]+$", id)) {
    keys <- c(keys, sub("\\.[0-9]+$", "", id))
  }
  if (grepl("s$", id)) {
    keys <- c(keys, sub("s$", "", id))
  } else {
    keys <- c(keys, paste0(id, "s"))
  }

  keys <- unique(tolower(keys))
  for (key in keys) {
    val <- unname(map[key])
    if (length(val) && !is.na(val) && nzchar(val)) {
      return(val[[1]])
    }
  }

  NA_character_
}

apply_metadata_overrides_to_conf <- function(conf, rules = NULL) {
  if (is.null(rules)) {
    rules <- get_metadata_overrides()
  }

  if (is.null(conf) || nrow(conf) == 0 || !"ID" %in% names(conf) || !"UI" %in% names(conf) || nrow(rules) == 0) {
    return(conf)
  }

  if (inherits(conf, "data.table")) {
    conf <- data.table::copy(conf)
  } else {
    conf <- as.data.frame(conf, stringsAsFactors = FALSE)
  }

  for (i in seq_len(nrow(conf))) {
    if ("dimred" %in% names(conf) && !is.na(conf$dimred[i]) && conf$dimred[i]) {
      next
    }

    id <- as.character(conf$ID[i])
    if (is.na(id) || !nzchar(id)) {
      next
    }

    override <- lookup_metadata_override(id, rules)
    if (is.na(override) || !nzchar(override)) {
      next
    }

    if (tolower(override) == "remove") {
      conf$UI[i] <- NA_character_
      if ("grp" %in% names(conf)) {
        conf$grp[i] <- FALSE
      }
    } else {
      conf$UI[i] <- override
    }
  }

  conf
}

get_cellinfo_choices <- function(conf, grouped_only = FALSE, include_dimred = FALSE) {
  if (is.null(conf) || nrow(conf) == 0 || !"UI" %in% names(conf)) {
    return(character(0))
  }

  mask <- !is.na(conf$UI) & nzchar(as.character(conf$UI))
  if (!include_dimred && "dimred" %in% names(conf)) {
    mask <- mask & !(!is.na(conf$dimred) & conf$dimred)
  }
  if (grouped_only && "grp" %in% names(conf)) {
    mask <- mask & !is.na(conf$grp) & conf$grp
  }

  unique(as.character(conf$UI[mask]))
}

resolve_ui_from_id <- function(conf, preferred_ids, fallback = NULL) {
  if (is.null(conf) || nrow(conf) == 0 || !"ID" %in% names(conf) || !"UI" %in% names(conf)) {
    return(fallback %||% "")
  }

  for (id in preferred_ids) {
    idx <- which(conf$ID == id & !is.na(conf$UI) & nzchar(as.character(conf$UI)))
    if (length(idx)) {
      return(as.character(conf$UI[idx[1]]))
    }

    alt_ids <- if (grepl("s$", id)) sub("s$", "", id) else paste0(id, "s")
    idx <- which(conf$ID %in% alt_ids & !is.na(conf$UI) & nzchar(as.character(conf$UI)))
    if (length(idx)) {
      return(as.character(conf$UI[idx[1]]))
    }
  }

  grouped <- get_cellinfo_choices(conf, grouped_only = TRUE, include_dimred = FALSE)
  if (length(grouped)) {
    return(grouped[[1]])
  }

  fallback %||% ""
}

apply_metadata_overrides_to_def <- function(def, conf, rules = NULL) {
  if (is.null(def) || !is.list(def)) {
    return(def)
  }

  if (is.null(rules)) {
    rules <- get_metadata_overrides()
  }

  map_default <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return(value)
    }
    current <- as.character(value[[1]])
    if (is.na(current) || !nzchar(current)) {
      return(value)
    }
    override <- lookup_metadata_override(current, rules)
    if (is.na(override) || !nzchar(override)) {
      return(value)
    }
    if (tolower(override) == "remove") {
      return(NA_character_)
    }
    override
  }

  for (field in c("meta1", "meta2", "grp1", "grp2")) {
    if (field %in% names(def)) {
      def[[field]] <- map_default(def[[field]])
    }
  }

  grouped_choices <- get_cellinfo_choices(conf, grouped_only = TRUE, include_dimred = FALSE)
  pick_valid <- function(value, choices) {
    if (!length(choices)) {
      return(value)
    }
    current <- as.character(value[[1]])
    if (!is.na(current) && nzchar(current) && current %in% choices) {
      return(current)
    }
    choices[[1]]
  }

  for (field in c("meta1", "meta2", "grp1", "grp2")) {
    if (field %in% names(def)) {
      def[[field]] <- pick_valid(def[[field]], grouped_choices)
    }
  }

  def
}
