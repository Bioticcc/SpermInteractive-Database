# Helper utilities for lazily loading large app assets. Each loader memoizes the
# first successfully read object so we avoid re-reading the same file across the
# session.

first_existing <- function(paths) {
  for (path in paths) {
    if (file.exists(path)) {
      return(path)
    }
  }
  stop(
    sprintf("None of the candidate files exist: %s", paste(paths, collapse = ", ")),
    call. = FALSE
  )
}

memoized_loader <- function(paths, label, reader, postprocess = identity) {
  cache <- NULL
  function() {
    if (is.null(cache)) {
      path <- first_existing(paths)
      cache <<- postprocess(reader(path))
    }
    cache
  }
}

memoized_path <- function(paths, label = NULL) {
  cached_path <- NULL
  function() {
    if (is.null(cached_path)) {
      cached_path <<- first_existing(paths)
    }
    cached_path
  }
}

normalize_gene_index <- function(gene_data) {
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
    names(data)[idx[1]]
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
    if (!length(idx)) {
      idx <- seq_along(genes)
    }
    if (length(idx) != length(genes)) {
      idx <- seq_along(genes)
    }
    if (anyDuplicated(genes)) {
      genes <- make.unique(genes)
    }
    names(idx) <- genes
    idx
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
    gene_data <- tryCatch(as.data.frame(gene_data, stringsAsFactors = FALSE), error = function(e) NULL)
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

read_rds_file <- function(path) readRDS(path)
read_csv_file <- function(path) utils::read.csv(path, check.names = FALSE)

get_final_staged_object <- memoized_loader(
  c("final_staged_object_slim.rds", "final_staged_object.rds"),
  "final staged Seurat object",
  read_rds_file
)

get_specific_obj <- memoized_loader(
  c("specificCellID_slim.rds", "specificCellID.rds"),
  "specificCellID Seurat object",
  read_rds_file
)

get_sc1conf <- memoized_loader(
  c("sc1conf_slim.rds", "sc1conf.rds"),
  "ShinyCell config (sc1)",
  read_rds_file
)
get_sc1def <- memoized_loader(
  c("sc1def_slim.rds", "sc1def.rds"),
  "ShinyCell defaults (sc1)",
  read_rds_file
)
get_sc1gene <- memoized_loader(
  c("sc1gene_slim.rds", "sc1gene.rds"),
  "ShinyCell gene index (sc1)",
  read_rds_file,
  normalize_gene_index
)
get_sc1meta <- memoized_loader(
  c("sc1meta_slim.rds", "sc1meta.rds"),
  "ShinyCell metadata (sc1)",
  read_rds_file
)
get_sc1gexpr_path <- memoized_path(
  c("sc1gexpr_slim.h5", "sc1gexpr.h5"),
  "ShinyCell expression matrix (sc1)"
)

get_sc2conf <- memoized_loader(
  c("sc2conf_slim.rds", "sc2conf.rds"),
  "ShinyCell config (sc2)",
  read_rds_file
)
get_sc2def <- memoized_loader(
  c("sc2def_slim.rds", "sc2def.rds"),
  "ShinyCell defaults (sc2)",
  read_rds_file
)
get_sc2gene <- memoized_loader(
  c("sc2gene_slim.rds", "sc2gene.rds"),
  "ShinyCell gene index (sc2)",
  read_rds_file,
  normalize_gene_index
)
get_sc2meta <- memoized_loader(
  c("sc2meta_slim.rds", "sc2meta.rds"),
  "ShinyCell metadata (sc2)",
  read_rds_file
)
get_sc2gexpr_path <- memoized_path(
  c("sc2gexpr_slim.h5", "sc2gexpr.h5"),
  "ShinyCell expression matrix (sc2)"
)

get_sc3conf <- memoized_loader(
  c("sc3conf_slim.rds", "sc3conf.rds"),
  "ShinyCell config (sc3)",
  read_rds_file
)
get_sc3def <- memoized_loader(
  c("sc3def_slim.rds", "sc3def.rds"),
  "ShinyCell defaults (sc3)",
  read_rds_file
)
get_sc3gene <- memoized_loader(
  c("sc3gene_slim.rds", "sc3gene.rds"),
  "ShinyCell gene index (sc3)",
  read_rds_file,
  normalize_gene_index
)
get_sc3meta <- memoized_loader(
  c("sc3meta_slim.rds", "sc3meta.rds"),
  "ShinyCell metadata (sc3)",
  read_rds_file
)
get_sc3gexpr_path <- memoized_path(
  c("sc3gexpr_slim.h5", "sc3gexpr.h5"),
  "ShinyCell expression matrix (sc3)"
)

get_cellchat_scores <- memoized_loader(
  c(
    "cellchat_scores_slim.rds",
    "CellChat_all_stage_communication_score_LR_reverse.rds",
    "CellChat_all_stage_communication_score_LR_reverse.csv"
  ),
  "CellChat ligand-receptor scores",
  function(path) {
    if (grepl("\\.rds$", path, ignore.case = TRUE)) {
      read_rds_file(path)
    } else {
      read_csv_file(path)
    }
  },
  function(df) {
    if (!("lr_pair" %in% names(df))) {
      if ("X" %in% names(df)) {
        df$lr_pair <- df$X
      }
    }
    rownames(df) <- df$lr_pair
    df
  }
)

get_gene_metadata <- memoized_loader(
  c("www/gene_data.json"),
  "gene metadata",
  function(path) jsonlite::fromJSON(path)
)
