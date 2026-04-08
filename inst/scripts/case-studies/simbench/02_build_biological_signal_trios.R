# Reproduce SimBench biological-signal supporting evidence for BenchHub.
#
# This script computes the real-data biological-signal proportions used as
# supporting evidence and builds Trio objects. It does not evaluate simulation
# methods.

source("inst/scripts/case-studies/simbench/01_compute_data_property_evidence.R")

biological_signal_packages <- c(
  "Matrix",
  "limma"
)

propDiffMetric <- function(evidence, predicted) {
  abs(as.numeric(predicted) - as.numeric(evidence))
}

get_celltype <- function(obj, dataset_name) {
  celltype <- NULL

  if (inherits(obj, "Seurat")) {
    celltype <- obj$celltype
  } else if (inherits(obj, "SingleCellExperiment")) {
    celltype <- SummarizedExperiment::colData(obj)$celltype
  }

  if (is.null(celltype)) {
    stop(
      "Could not find a `celltype` field for dataset ",
      dataset_name,
      ".",
      call. = FALSE
    )
  }

  celltype
}

subset_top_two_celltypes <- function(exprs_mat, celltype) {
  celltype <- as.character(celltype)
  total_celltypes <- unique(celltype)

  if (length(total_celltypes) < 2L) {
    stop("Not enough cell types.", call. = FALSE)
  }

  selected_celltypes <- names(sort(table(celltype), decreasing = TRUE)[1:2])
  keep_cells <- celltype %in% selected_celltypes

  list(
    exprs = exprs_mat[, keep_cells, drop = FALSE],
    celltype = droplevels(factor(celltype[keep_cells], levels = selected_celltypes)),
    selected_celltypes = selected_celltypes
  )
}

normalise_counts_for_signals <- function(counts_mat) {
  exprs_mat <- as_dgCMatrix(counts_mat)
  exprs_mat@x <- log1p(exprs_mat@x)
  exprs_mat
}

do_limma_signal <- function(exprs_mat, celltype, exprs_pct = 0.05) {
  celltype <- droplevels(as.factor(celltype))
  tmp_celltype <- ifelse(celltype == levels(celltype)[1], 1, 0)
  design <- stats::model.matrix(~tmp_celltype)

  mean_exprs <- do.call(cbind, lapply(c(0, 1), function(i) {
    Matrix::rowMeans(exprs_mat[, tmp_celltype == i, drop = FALSE])
  }))

  mean_pct <- do.call(cbind, lapply(c(0, 1), function(i) {
    Matrix::rowSums(exprs_mat[, tmp_celltype == i, drop = FALSE] > 0) /
      sum(tmp_celltype == i)
  }))

  keep <- mean_pct[, 2] > exprs_pct

  y <- methods::new("EList")
  y$E <- exprs_mat[keep, , drop = FALSE]
  fit <- limma::lmFit(y, design = design)
  fit <- limma::eBayes(fit, trend = TRUE, robust = TRUE)
  tt <- limma::topTable(fit, n = Inf, adjust.method = "BH", coef = 2)

  if (!is.null(tt$ID)) {
    tt <- tt[!duplicated(tt$ID), ]
    rownames(tt) <- tt$ID
  }

  tt$meanExprs.1 <- mean_exprs[rownames(tt), 1]
  tt$meanExprs.2 <- mean_exprs[rownames(tt), 2]
  tt$meanPct.1 <- mean_pct[rownames(tt), 1]
  tt$meanPct.2 <- mean_pct[rownames(tt), 2]

  tt
}

do_dv_signal <- function(exprs_mat, celltype) {
  celltype <- droplevels(as.factor(celltype))
  tmp_celltype <- ifelse(celltype == levels(celltype)[1], 1, 0)

  mean_pct <- do.call(cbind, lapply(c(0, 1), function(i) {
    Matrix::rowSums(exprs_mat[, tmp_celltype == i, drop = FALSE] > 0) /
      sum(tmp_celltype == i)
  }))

  pos_neg <- (mean_pct[, 2] - mean_pct[, 1]) > 0.05
  exprs_mat_filt <- exprs_mat[pos_neg, , drop = FALSE]

  tt <- apply(exprs_mat_filt, 1, function(x) {
    df <- data.frame(gene = x, celltype = as.factor(tmp_celltype))
    stats::bartlett.test(gene ~ celltype, df)$p.value
  })

  stats::p.adjust(tt, method = "BH")
}

do_dd_signal <- function(exprs_mat, celltype) {
  celltype <- droplevels(as.factor(celltype))
  tmp_celltype <- ifelse(celltype == levels(celltype)[1], 1, 0)

  mean_pct <- do.call(cbind, lapply(c(0, 1), function(i) {
    Matrix::rowSums(exprs_mat[, tmp_celltype == i, drop = FALSE] > 0) /
      sum(tmp_celltype == i)
  }))

  pos_neg <- (mean_pct[, 2] - mean_pct[, 1]) > 0.05
  exprs_mat_filt <- exprs_mat[pos_neg, , drop = FALSE]

  tt <- apply(exprs_mat_filt, 1, function(x) {
    x1 <- x[tmp_celltype == 0]
    x2 <- x[tmp_celltype == 1]
    stats::ks.test(x1, x2, alternative = "greater")$p.value
  })

  stats::p.adjust(tt, method = "BH")
}

do_dp_signal <- function(exprs_mat, celltype, threshold = 1) {
  celltype <- droplevels(as.factor(celltype))
  tmp_celltype <- ifelse(celltype == levels(celltype)[1], 1, 0)
  zeros_mat <- ifelse(as.matrix(exprs_mat) > threshold, 1, 0)

  tt <- apply(zeros_mat, 1, function(x) {
    tab <- NULL
    for (i in c(0, 1)) {
      tmp <- factor(x[tmp_celltype == i], levels = c(0, 1))
      tab <- rbind(tab, table(tmp))
    }

    suppressWarnings(stats::chisq.test(tab)$p.value)
  })

  stats::p.adjust(tt, method = "BH")
}

do_bd_signal <- function(exprs_mat, celltype) {
  celltype <- droplevels(as.factor(celltype))
  tmp_celltype <- ifelse(celltype == levels(celltype)[1], 1, 0)
  pi <- table(tmp_celltype) / length(tmp_celltype)

  agg_mean <- do.call(cbind, lapply(c(0, 1), function(i) {
    Matrix::rowMeans(exprs_mat[, tmp_celltype == i, drop = FALSE])
  }))

  agg_sd2 <- do.call(cbind, lapply(c(0, 1), function(i) {
    apply(exprs_mat[, tmp_celltype == i, drop = FALSE], 1, stats::var)
  }))

  bi <- abs(agg_mean[, 2] - agg_mean[, 1]) / sqrt(
    pi[1] * agg_sd2[, 1] + pi[2] * agg_sd2[, 2]
  )
  bi <- unlist(bi)
  names(bi) <- rownames(exprs_mat)
  bi[order(bi, decreasing = TRUE)]
}

compute_biological_signal_evidence <- function(
  counts_mat,
  celltype,
  p_sig = 0.1,
  bd_threshold = 0.3
) {
  assert_packages(biological_signal_packages)

  exprs_mat <- normalise_counts_for_signals(counts_mat)
  prepared <- subset_top_two_celltypes(exprs_mat, celltype)
  exprs_mat <- prepared$exprs
  celltype <- prepared$celltype
  n_genes <- nrow(exprs_mat)

  de <- do_limma_signal(exprs_mat, celltype)
  dv <- do_dv_signal(exprs_mat, celltype)
  dd <- do_dd_signal(exprs_mat, celltype)
  dp <- do_dp_signal(exprs_mat, celltype)
  bd <- do_bd_signal(exprs_mat, celltype)

  evidence <- c(
    DE = sum(de$adj.P.Val < p_sig, na.rm = TRUE) / n_genes,
    DV = sum(dv < p_sig, na.rm = TRUE) / n_genes,
    DD = sum(dd < p_sig, na.rm = TRUE) / n_genes,
    DP = sum(dp < p_sig, na.rm = TRUE) / n_genes,
    BD = sum(bd > bd_threshold, na.rm = TRUE) / n_genes
  )

  stats::setNames(as.list(evidence), names(evidence))
}

as_biological_signal_trio_evidence <- function(evidence) {
  Map(
    function(x, evidence_name) {
      names(x) <- evidence_name
      list(
        evidence = x,
        metrics = "Proportion Difference"
      )
    },
    evidence,
    names(evidence)
  )
}

build_biological_signal_trio <- function(
  counts_mat,
  celltype,
  dataset_id,
  dataset_name = dataset_id,
  description = NULL,
  p_sig = 0.1,
  bd_threshold = 0.3
) {
  if (!exists("Trio", inherits = TRUE)) {
    stop(
      "Load BenchHub before building Trio objects, for example with ",
      "`pkgload::load_all('.')` from the package root.",
      call. = FALSE
    )
  }

  counts_mat <- as_dgCMatrix(counts_mat)
  evidence <- compute_biological_signal_evidence(
    counts_mat = counts_mat,
    celltype = celltype,
    p_sig = p_sig,
    bd_threshold = bd_threshold
  )

  if (is.null(description)) {
    description <- paste(
      "SimBench real dataset for biological-signal evaluation with",
      "DE, DV, DD, DP, and BD supporting evidence."
    )
  }

  trio <- Trio$new(
    data = counts_mat,
    evidence = as_biological_signal_trio_evidence(evidence),
    metrics = list(`Proportion Difference` = propDiffMetric),
    datasetID = dataset_id,
    name = dataset_name,
    description = description
  )

  trio$dataSource <- "experimenthub"
  trio$dataSourceID <- dataset_id

  trio
}

build_simbench_biological_signal_trios <- function(
  dataset_map = NULL,
  drop_ids = c("EH5384"),
  p_sig = 0.1,
  bd_threshold = 0.3
) {
  assert_packages(c(biological_signal_packages, simbench_packages))

  alldata <- fetch_simbench_datasets()

  if (is.null(dataset_map)) {
    dataset_map <- default_dataset_map(alldata)
  }

  dataset_map <- dataset_map[!names(dataset_map) %in% drop_ids]
  trios <- list()

  for (id in names(dataset_map)) {
    ds_name <- dataset_map[[id]]
    message("Building biological-signal Trio for ", ds_name, " (", id, ")")

    obj <- alldata[[id]]
    counts_mat <- get_counts_matrix(obj, ds_name)
    celltype <- get_celltype(obj, ds_name)

    trios[[ds_name]] <- build_biological_signal_trio(
      counts_mat = counts_mat,
      celltype = celltype,
      dataset_id = id,
      dataset_name = ds_name,
      p_sig = p_sig,
      bd_threshold = bd_threshold
    )
  }

  trios
}

if (sys.nframe() == 0L) {
  build_simbench_biological_signal_trios()
}
