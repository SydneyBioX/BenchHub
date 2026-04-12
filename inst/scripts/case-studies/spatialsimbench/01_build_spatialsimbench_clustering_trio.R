# Build one SpatialSimBench Trio with SimBench-style data-property evidence
# plus spatial clustering evidence.

source("inst/scripts/case-studies/simbench/01_compute_data_property_evidence.R")

spatialsimbench_packages <- c(
  data_property_packages,
  "SingleCellExperiment"
)

get_spatial_domain_labels <- function(trio, label_col = "spatial.cluster") {
  obj <- trio$data

  if (inherits(obj, "SingleCellExperiment")) {
    metadata <- as.data.frame(obj@colData)
  } else if (inherits(obj, "Seurat")) {
    metadata <- obj[[]]
  } else {
    stop(
      "Unsupported object type for spatial clustering evidence: ",
      paste(class(obj), collapse = ", "),
      call. = FALSE
    )
  }

  if (!label_col %in% colnames(metadata)) {
    stop(
      "Could not find spatial clustering labels in column `", label_col, "`.",
      call. = FALSE
    )
  }

  labels <- as.factor(metadata[[label_col]])
  names(labels) <- rownames(metadata)
  labels
}

ARImetric <- function(evidence, predicted) {
  evidence <- as.factor(evidence)
  predicted <- as.factor(predicted)

  if (length(evidence) != length(predicted)) {
    stop("ARI requires evidence and predicted to have the same length.", call. = FALSE)
  }

  tab <- table(evidence, predicted)
  n <- sum(tab)

  if (n <= 1L) {
    return(1)
  }

  choose2 <- function(x) {
    x * (x - 1) / 2
  }

  sum_ij <- sum(choose2(tab))
  sum_i <- sum(choose2(rowSums(tab)))
  sum_j <- sum(choose2(colSums(tab)))
  total_pairs <- choose2(n)

  expected <- (sum_i * sum_j) / total_pairs
  max_index <- 0.5 * (sum_i + sum_j)
  denominator <- max_index - expected

  if (isTRUE(all.equal(denominator, 0))) {
    return(1)
  }

  (sum_ij - expected) / denominator
}

NMImetric <- function(evidence, predicted) {
  evidence <- as.factor(evidence)
  predicted <- as.factor(predicted)

  if (length(evidence) != length(predicted)) {
    stop("NMI requires evidence and predicted to have the same length.", call. = FALSE)
  }

  tab <- table(evidence, predicted)
  n <- sum(tab)

  if (n == 0L) {
    return(NA_real_)
  }

  p_ij <- tab / n
  p_i <- rowSums(p_ij)
  p_j <- colSums(p_ij)

  nz <- p_ij > 0
  mi <- sum(p_ij[nz] * log(p_ij[nz] / (p_i[row(p_ij)][nz] * p_j[col(p_ij)][nz])))

  entropy <- function(p) {
    p <- p[p > 0]
    -sum(p * log(p))
  }

  h_i <- entropy(p_i)
  h_j <- entropy(p_j)
  denominator <- sqrt(h_i * h_j)

  if (isTRUE(all.equal(denominator, 0))) {
    return(1)
  }

  mi / denominator
}

precisionMetric <- function(evidence, predicted) {
  truth <- unique(as.character(evidence))
  pred <- unique(as.character(predicted))

  if (length(pred) == 0L) {
    return(0)
  }

  sum(pred %in% truth) / length(pred)
}

recallMetric <- function(evidence, predicted) {
  truth <- unique(as.character(evidence))
  pred <- unique(as.character(predicted))

  if (length(truth) == 0L) {
    return(0)
  }

  sum(truth %in% pred) / length(truth)
}

generate_svg_sparkx <- function(sce) {
  if (!requireNamespace("SPARK", quietly = TRUE)) {
    stop(
      "Install the SPARK package before running generate_svg_sparkx().",
      call. = FALSE
    )
  }

  if (!inherits(sce, "SingleCellExperiment")) {
    stop(
      "generate_svg_sparkx() expects a SingleCellExperiment object.",
      call. = FALSE
    )
  }

  sp_count <- SingleCellExperiment::counts(sce)
  location <- as.matrix(as.data.frame(sce@colData)[, c("col", "row"), drop = FALSE])
  rownames(location) <- colnames(sp_count)

  sp_count <- sp_count[!grepl("^(MT|mt)-", rownames(sp_count)), ]

  SPARK::sparkx(
    sp_count,
    location,
    numCores = 1,
    option = "mixture"
  )
}

generate_moransI <- function(sce) {
  if (!requireNamespace("spots", quietly = TRUE)) {
    stop(
      "Install the spots package before running generate_moransI().",
      call. = FALSE
    )
  }

  if (!inherits(sce, "SingleCellExperiment")) {
    stop(
      "generate_moransI() expects a SingleCellExperiment object.",
      call. = FALSE
    )
  }

  counts <- SingleCellExperiment::counts(sce)
  logcounts <- Matrix::t(log1p(counts))
  loc <- as.matrix(as.data.frame(sce@colData)[, c("row", "col"), drop = FALSE])

  weights <- 1 / as.matrix(stats::dist(loc))
  weights[!is.finite(weights)] <- 0
  diag(weights) <- 0

  spots::BivariateMoransI(X = logcounts, W = weights)
}

generate_cosine <- function(real, sim) {
  if (!requireNamespace("lsa", quietly = TRUE)) {
    stop(
      "Install the lsa package before running generate_cosine().",
      call. = FALSE
    )
  }

  real_mi <- as.vector(real$Morans.I)
  sim_mi <- as.vector(sim$Morans.I)
  keep <- !is.na(real_mi) & !is.na(sim_mi)

  if (!any(keep)) {
    return(NA_real_)
  }

  similarity <- lsa::cosine(
    lsa::as.textmatrix(cbind(real_mi[keep], sim_mi[keep]))
  )

  mean(similarity)
}

generate_mantel <- function(real, sim) {
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop(
      "Install the vegan package before running generate_mantel().",
      call. = FALSE
    )
  }

  real_mi <- real$Morans.I
  sim_mi <- sim$Morans.I

  if (is.null(dim(real_mi))) {
    real_mi <- stats::dist(as.vector(real_mi))
  }

  if (is.null(dim(sim_mi))) {
    sim_mi <- stats::dist(as.vector(sim_mi))
  }

  vegan::mantel(real_mi, sim_mi, na.rm = TRUE, method = "pearson")$statistic
}

CARD_processing <- function(
  sp_sce,
  sc_sce,
  cell_type_col = "cell_type",
  donor_col = "sampleInfo"
) {
  if (!requireNamespace("CARD", quietly = TRUE)) {
    stop(
      "Install the CARD package before running CARD_processing().",
      call. = FALSE
    )
  }

  if (!inherits(sp_sce, "SingleCellExperiment") ||
      !inherits(sc_sce, "SingleCellExperiment")) {
    stop(
      "CARD_processing() expects both spatial and single-cell inputs to be SingleCellExperiment objects.",
      call. = FALSE
    )
  }

  sc_coldata <- as.data.frame(sc_sce@colData)
  sp_coldata <- as.data.frame(sp_sce@colData)

  if (!donor_col %in% colnames(sc_coldata)) {
    donor_candidates <- c("sampleInfo", "donor_id", "donor", "sample", "Sample")
    donor_match <- donor_candidates[donor_candidates %in% colnames(sc_coldata)][1]
    if (!is.na(donor_match)) {
      donor_col <- donor_match
    }
  }

  if (!cell_type_col %in% colnames(sc_coldata)) {
    celltype_candidates <- c("cell_type", "cellType", "celltype", "CellType")
    celltype_match <- celltype_candidates[celltype_candidates %in% colnames(sc_coldata)][1]
    if (!is.na(celltype_match)) {
      cell_type_col <- celltype_match
    }
  }

  missing_sc_cols <- c(cell_type_col, donor_col)[!c(cell_type_col, donor_col) %in% colnames(sc_coldata)]
  if (length(missing_sc_cols) > 0L) {
    stop(
      "Missing required single-cell metadata column(s): ",
      paste(missing_sc_cols, collapse = ", "),
      call. = FALSE
    )
  }

  spatial_count <- SingleCellExperiment::counts(sp_sce)
  spatial_location <- data.frame(
    x = as.numeric(sp_coldata$col),
    y = as.numeric(sp_coldata$row)
  )
  sc_count <- SingleCellExperiment::counts(sc_sce)
  sc_meta <- data.frame(
    cellID = colnames(sc_sce),
    cellType = sc_coldata[[cell_type_col]],
    sampleInfo = sc_coldata[[donor_col]]
  )

  rownames(sc_meta) <- sc_meta$cellID
  rownames(spatial_location) <- colnames(spatial_count)

  card_obj <- CARD::createCARDObject(
    sc_count = sc_count,
    sc_meta = sc_meta,
    spatial_count = spatial_count,
    spatial_location = spatial_location,
    ct.varname = "cellType",
    ct.select = unique(sc_meta$cellType),
    sample.varname = "sampleInfo",
    minCountGene = 100,
    minCountSpot = 5
  )
  card_obj <- CARD::CARD_deconvolution(CARD_object = card_obj)

  as.matrix(card_obj@Proportion_CARD)
}

generate_jds <- function(real, sim) {
  if (!requireNamespace("philentropy", quietly = TRUE)) {
    stop(
      "Install the philentropy package before running generate_jds().",
      call. = FALSE
    )
  }

  common_row_names <- intersect(rownames(real), rownames(sim))
  real_common <- real[common_row_names, , drop = FALSE]
  sim_common <- sim[common_row_names, , drop = FALSE]

  if (length(common_row_names) == 0L) {
    return(NA_real_)
  }

  jsd_values <- vapply(seq_len(nrow(real_common)), function(i) {
    x_count <- rbind(as.vector(real_common[i, ]), as.vector(sim_common[i, ]))
    philentropy::JSD(x_count, est.prob = "empirical")
  }, FUN.VALUE = numeric(1))

  mean(jsd_values)
}

generate_rmse <- function(real, sim) {
  if (!requireNamespace("Metrics", quietly = TRUE)) {
    stop(
      "Install the Metrics package before running generate_rmse().",
      call. = FALSE
    )
  }

  common_row_names <- intersect(rownames(real), rownames(sim))
  real_common <- real[common_row_names, , drop = FALSE]
  sim_common <- sim[common_row_names, , drop = FALSE]

  if (length(common_row_names) == 0L) {
    return(NA_real_)
  }

  rmse_values <- vapply(seq_len(nrow(real_common)), function(i) {
    Metrics::rmse(as.vector(real_common[i, ]), as.vector(sim_common[i, ]))
  }, FUN.VALUE = numeric(1))

  mean(rmse_values)
}

extract_svg_truth <- function(
  sparkx_result,
  pval_col = "adjustedPval",
  pval_cutoff = 0.05
) {
  real_svg <- sparkx_result

  if (is.list(real_svg) && "res_mtest" %in% names(real_svg)) {
    real_svg <- real_svg$res_mtest
  }

  candidate_cols <- c(pval_col, "adjustedPval", "adj.pvalue", "padj", "p.adjust")
  selected_col <- candidate_cols[candidate_cols %in% colnames(real_svg)][1]

  if (is.na(selected_col)) {
    stop(
      "Could not identify an adjusted p-value column in the SPARK-X output.",
      call. = FALSE
    )
  }

  svg_hits <- real_svg[real_svg[[selected_col]] < pval_cutoff, , drop = FALSE]

  if (!is.null(rownames(svg_hits))) {
    return(rownames(svg_hits))
  }

  if ("gene" %in% colnames(svg_hits)) {
    return(as.character(svg_hits[["gene"]]))
  }

  stop(
    "Filtered SPARK-X results must have row names or a `gene` column to define the SVG gene set.",
    call. = FALSE
  )
}

build_spatialsimbench_trio <- function(
  figshare_id,
  dataset_name = figshare_id,
  description = NULL,
  cache_path = FALSE,
  matched_sc_sce = NULL,
  label_col = "spatial.cluster",
  cell_type_col = "cell_type",
  donor_col = "sampleInfo",
  pearson_sample_size = 10000L,
  seed = 1L,
  pval_cutoff = 0.05
) {
  assert_packages(spatialsimbench_packages)

  if (!exists("Trio", inherits = TRUE) || !exists("kdeMetric", inherits = TRUE)) {
    stop(
      "Load BenchHub before building Trio objects, for example with ",
      "`pkgload::load_all('.')` from the package root.",
      call. = FALSE
    )
  }

  trio <- Trio$new(
    paste0("figshare:", figshare_id),
    cachePath = cache_path
  )

  if (!inherits(trio$data, "SingleCellExperiment")) {
    stop(
      "SpatialSimBench trio builder currently expects a SingleCellExperiment object.",
      call. = FALSE
    )
  }

  sce <- trio$data
  counts_mat <- get_counts_matrix(sce, dataset_name = dataset_name)

  data_property_evidence <- compute_data_property_evidence(
    real_counts = counts_mat,
    pearson_sample_size = pearson_sample_size,
    seed = seed
  )

  clustering_labels <- get_spatial_domain_labels(trio, label_col = label_col)
  if (!identical(names(clustering_labels), colnames(counts_mat))) {
    names(clustering_labels) <- colnames(counts_mat)
  }

  sparkx_result <- generate_svg_sparkx(sce)
  svg_truth <- extract_svg_truth(
    sparkx_result = sparkx_result,
    pval_cutoff = pval_cutoff
  )
  real_moransI <- generate_moransI(sce)
  real_deconvolution <- NULL
  if (!is.null(matched_sc_sce)) {
    real_deconvolution <- CARD_processing(
      sp_sce = sce,
      sc_sce = matched_sc_sce,
      cell_type_col = cell_type_col,
      donor_col = donor_col
    )
  }

  if (is.null(description)) {
    description <- paste(
      "SpatialSimBench spatial transcriptomics dataset with 14",
      "data-property supporting evidence summaries, spatial clustering",
      "labels, spatially variable gene truth from SPARK-X, and",
      "spatial cross-correlation evidence from bivariate Moran's I."
    )
  }

  trio$evidence <- c(
    as_trio_evidence(data_property_evidence, metric_name = "KDE"),
    list(
      spatial_clustering = list(
        evidence = clustering_labels,
        metrics = c("ARI", "NMI")
      )
    ),
    list(
      spatial_variable_gene = list(
        evidence = svg_truth,
        metrics = c("recall", "precision")
      )
    ),
    list(
      spatial_cross_correlation = list(
        evidence = real_moransI,
        metrics = c("Mantel statistics", "cosine similarity")
      )
    ),
    if (is.null(real_deconvolution)) NULL else list(
      spatial_deconvolution = list(
        evidence = real_deconvolution,
        metrics = c("JSD", "RMSE")
      )
    )
  )

  trio$metrics <- list(
    KDE = kdeMetric,
    ARI = ARImetric,
    NMI = NMImetric,
    recall = recallMetric,
    precision = precisionMetric,
    `Mantel statistics` = generate_mantel,
    `cosine similarity` = generate_cosine,
    JSD = generate_jds,
    RMSE = generate_rmse
  )
  trio$name <- dataset_name
  trio$description <- description

  trio
}

if (sys.nframe() == 0L) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".")
  } else {
    library(BenchHub)
  }

  trio <- build_spatialsimbench_trio(
    figshare_id = "26054188/47115889",
    dataset_name = "MOBNEW",
    pearson_sample_size = 100L
  )

  print(trio)
  print(names(trio$evidence))
}
