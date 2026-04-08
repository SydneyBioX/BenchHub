# Reproduce SimBench data-property supporting evidence for BenchHub.
#
# This script computes the real-data summaries used as supporting evidence for
# the SimBench "Data property estimation" task and builds Trio objects. It does
# not evaluate simulation methods.

data_property_packages <- c(
  "Matrix",
  "sparseMatrixStats",
  "edgeR"
)

simbench_packages <- c(
  "AnnotationHub",
  "ExperimentHub",
  "SingleCellExperiment",
  "SummarizedExperiment"
)

assert_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    stop(
      "Install the following packages before running this script: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

as_dgCMatrix <- function(x) {
  if (inherits(x, "dgCMatrix")) {
    return(x)
  }

  methods::as(x, "dgCMatrix")
}

get_counts_matrix <- function(obj, dataset_name) {
  if (inherits(obj, "Seurat")) {
    if (!requireNamespace("SeuratObject", quietly = TRUE)) {
      stop(
        "Dataset ", dataset_name, " is a Seurat object, but SeuratObject is not installed.",
        call. = FALSE
      )
    }

    counts <- tryCatch(
      SeuratObject::GetAssayData(obj, layer = "counts"),
      error = function(e) {
        SeuratObject::GetAssayData(obj, slot = "counts")
      }
    )
  } else if (inherits(obj, "SingleCellExperiment")) {
    counts <- SummarizedExperiment::assay(obj, "counts")
  } else {
    stop(
      sprintf(
        "Unsupported object type for dataset %s: %s",
        dataset_name,
        paste(class(obj), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  as_dgCMatrix(counts)
}

sample_numeric <- function(x, size = 10000L, seed = 1L) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]

  if (length(x) <= size) {
    return(x)
  }

  set.seed(seed)
  sample(x, size)
}

sample_pairwise_cor <- function(x, margin = c("row", "col"), size = 10000L, seed = 1L) {
  margin <- match.arg(margin)
  n <- if (identical(margin, "row")) nrow(x) else ncol(x)

  if (n < 2L || size < 1L) {
    return(numeric())
  }

  set.seed(seed)
  first <- sample.int(n, size = size, replace = TRUE)
  second <- sample.int(n, size = size, replace = TRUE)
  same <- first == second
  second[same] <- (second[same] %% n) + 1L

  res <- vapply(seq_len(size), function(i) {
    if (identical(margin, "row")) {
      x1 <- as.numeric(x[first[i], ])
      x2 <- as.numeric(x[second[i], ])
    } else {
      x1 <- as.numeric(x[, first[i]])
      x2 <- as.numeric(x[, second[i]])
    }

    suppressWarnings(stats::cor(x1, x2, method = "pearson"))
  }, FUN.VALUE = numeric(1))

  res[is.finite(res)]
}

default_ids <- function(ids, prefix, n) {
  if (!is.null(ids)) {
    return(ids)
  }

  paste0(prefix, "_", seq_len(n))
}

compute_data_property_evidence <- function(
  real_counts,
  pearson_sample_size = 10000L,
  seed = 1L
) {
  real_counts <- as_dgCMatrix(real_counts)
  gene_ids <- default_ids(rownames(real_counts), "gene", nrow(real_counts))
  cell_ids <- default_ids(colnames(real_counts), "cell", ncol(real_counts))

  frac_zero_genes <- Matrix::rowMeans(real_counts == 0)
  frac_zero_cells <- Matrix::colMeans(real_counts == 0)
  names(frac_zero_genes) <- gene_ids
  names(frac_zero_cells) <- cell_ids

  lib_size_cells <- log1p(Matrix::colSums(real_counts))
  names(lib_size_cells) <- cell_ids

  # Kept aligned with the source metric script, where effective library size is
  # computed as log1p(library size).
  efflib_size_cells <- log1p(Matrix::colSums(real_counts))
  names(efflib_size_cells) <- cell_ids

  real_dge <- edgeR::DGEList(counts = real_counts)
  tmm_cells <- edgeR::calcNormFactors(
    real_dge,
    method = "TMM"
  )$samples$norm.factors
  names(tmm_cells) <- cell_ids

  scaled_var_cells <- scale(sparseMatrixStats::colVars(real_counts))
  scaled_mean_cells <- scale(Matrix::colMeans(real_counts))
  scaled_var_cells <- stats::setNames(as.numeric(scaled_var_cells), cell_ids)
  scaled_mean_cells <- stats::setNames(as.numeric(scaled_mean_cells), cell_ids)

  lib_fraczero_cells <- data.frame(
    lib = lib_size_cells,
    fraczero = frac_zero_cells,
    row.names = cell_ids
  )

  pearson_cells <- sample_pairwise_cor(
    real_counts,
    margin = "col",
    size = pearson_sample_size,
    seed = seed
  )
  names(pearson_cells) <- paste0("cell_pair_", seq_along(pearson_cells))

  scaled_var_genes <- scale(sparseMatrixStats::rowVars(real_counts))
  scaled_mean_genes <- scale(Matrix::rowMeans(real_counts))
  scaled_var_genes <- stats::setNames(as.numeric(scaled_var_genes), gene_ids)
  scaled_mean_genes <- stats::setNames(as.numeric(scaled_mean_genes), gene_ids)

  pearson_genes <- sample_pairwise_cor(
    real_counts,
    margin = "row",
    size = pearson_sample_size,
    seed = seed + 1L
  )
  names(pearson_genes) <- paste0("gene_pair_", seq_along(pearson_genes))

  mean_var_genes <- data.frame(
    mean = Matrix::rowMeans(real_counts),
    var = sparseMatrixStats::rowVars(real_counts),
    row.names = gene_ids
  )

  mean_fraczero_genes <- data.frame(
    mean = Matrix::rowMeans(real_counts),
    fraczero = frac_zero_genes,
    row.names = gene_ids
  )

  list(
    frac_zero_genes = frac_zero_genes,
    frac_zero_cells = frac_zero_cells,
    lib_size_cells = lib_size_cells,
    efflib_size_cells = efflib_size_cells,
    tmm_cells = tmm_cells,
    scaled_var_cells = scaled_var_cells,
    scaled_mean_cells = scaled_mean_cells,
    lib_fraczero_cells = lib_fraczero_cells,
    pearson_cells = pearson_cells,
    scaled_var_genes = scaled_var_genes,
    scaled_mean_genes = scaled_mean_genes,
    pearson_genes = pearson_genes,
    mean_var_genes = mean_var_genes,
    mean_fraczero_genes = mean_fraczero_genes
  )
}

as_trio_evidence <- function(evidence, metric_name = "KDE") {
  lapply(
    evidence,
    function(x) {
      list(
        evidence = x,
        metrics = metric_name
      )
    }
  )
}

build_data_property_trio <- function(
  counts_mat,
  dataset_id,
  dataset_name = dataset_id,
  description = NULL,
  pearson_sample_size = 10000L,
  seed = 1L
) {
  assert_packages(data_property_packages)

  if (!exists("Trio", inherits = TRUE) || !exists("kdeMetric", inherits = TRUE)) {
    stop(
      "Load BenchHub before building Trio objects, for example with ",
      "`pkgload::load_all('.')` from the package root.",
      call. = FALSE
    )
  }

  counts_mat <- as_dgCMatrix(counts_mat)
  evidence <- compute_data_property_evidence(
    counts_mat,
    pearson_sample_size = pearson_sample_size,
    seed = seed
  )
  trio_evidence <- as_trio_evidence(evidence, metric_name = "KDE")

  if (is.null(description)) {
    description <- paste(
      "SimBench real dataset for data-property estimation with",
      "14 supporting evidence summaries."
    )
  }

  trio <- Trio$new(
    data = counts_mat,
    evidence = trio_evidence,
    metrics = list(KDE = kdeMetric),
    datasetID = dataset_id,
    name = dataset_name,
    description = description
  )

  trio$dataSource <- "experimenthub"
  trio$dataSourceID <- dataset_id

  trio
}

fetch_simbench_datasets <- function() {
  eh <- ExperimentHub::ExperimentHub()
  AnnotationHub::query(eh, "SimBenchData")
}

list_simbench_datasets <- function() {
  assert_packages(c("AnnotationHub", "ExperimentHub"))

  alldata <- fetch_simbench_datasets()
  metadata <- as.data.frame(
    AnnotationHub::mcols(alldata)[, c("title", "rdataclass")]
  )
  data.frame(
    id = rownames(metadata),
    title = metadata$title,
    rdataclass = metadata$rdataclass,
    row.names = NULL
  )
}

simbench_dataset_map <- function(drop_ids = c("EH5384")) {
  metadata <- list_simbench_datasets()
  metadata <- metadata[!metadata$id %in% drop_ids, , drop = FALSE]
  stats::setNames(metadata$title, metadata$id)
}

default_dataset_map <- function(alldata) {
  stats::setNames(names(alldata), names(alldata))
}

build_simbench_data_property_trios <- function(
  dataset_map = NULL,
  drop_ids = c("EH5384"),
  pearson_sample_size = 10000L,
  seed = 1L
) {
  assert_packages(c(data_property_packages, simbench_packages))

  alldata <- fetch_simbench_datasets()

  if (is.null(dataset_map)) {
    dataset_map <- default_dataset_map(alldata)
  }

  dataset_map <- dataset_map[!names(dataset_map) %in% drop_ids]

  trios <- list()

  for (id in names(dataset_map)) {
    ds_name <- dataset_map[[id]]
    message("Building data-property Trio for ", ds_name, " (", id, ")")

    obj <- alldata[[id]]
    counts_mat <- get_counts_matrix(obj, ds_name)

    trios[[ds_name]] <- build_data_property_trio(
      counts_mat,
      dataset_id = id,
      dataset_name = ds_name,
      pearson_sample_size = pearson_sample_size,
      seed = seed
    )
  }

  trios
}

if (sys.nframe() == 0L) {
  build_simbench_data_property_trios()
}
