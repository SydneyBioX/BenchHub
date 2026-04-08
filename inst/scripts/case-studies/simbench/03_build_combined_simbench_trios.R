# Build combined SimBench Trios for BenchHub.
#
# Each Trio contains one real dataset plus both SimBench data-property and
# biological-signal supporting evidence. It does not evaluate simulation methods.

source("inst/scripts/case-studies/simbench/01_compute_data_property_evidence.R")
source("inst/scripts/case-studies/simbench/02_build_biological_signal_trios.R")

build_combined_simbench_trio <- function(
  counts_mat,
  celltype,
  dataset_id,
  dataset_name = dataset_id,
  description = NULL,
  pearson_sample_size = 10000L,
  seed = 1L,
  p_sig = 0.1,
  bd_threshold = 0.3
) {
  assert_packages(c(data_property_packages, biological_signal_packages))

  if (!exists("Trio", inherits = TRUE) || !exists("kdeMetric", inherits = TRUE)) {
    stop(
      "Load BenchHub before building Trio objects, for example with ",
      "`pkgload::load_all('.')` from the package root.",
      call. = FALSE
    )
  }

  counts_mat <- as_dgCMatrix(counts_mat)

  data_property_evidence <- compute_data_property_evidence(
    real_counts = counts_mat,
    pearson_sample_size = pearson_sample_size,
    seed = seed
  )
  biological_signal_evidence <- compute_biological_signal_evidence(
    counts_mat = counts_mat,
    celltype = celltype,
    p_sig = p_sig,
    bd_threshold = bd_threshold
  )
  biological_signal_evidence <- stats::setNames(
    biological_signal_evidence,
    c(
      DE = "differentially_expression",
      DV = "differentially_variable",
      DD = "differentially_distributed",
      DP = "differential_prop",
      BD = "bimodally_distributed"
    )[names(biological_signal_evidence)]
  )

  trio_evidence <- c(
    as_trio_evidence(data_property_evidence, metric_name = "KDE"),
    as_biological_signal_trio_evidence(biological_signal_evidence)
  )

  if (is.null(description)) {
    description <- paste(
      "SimBench real dataset with data-property and biological-signal",
      "supporting evidence."
    )
  }

  trio <- Trio$new(
    data = counts_mat,
    evidence = trio_evidence,
    metrics = list(
      KDE = kdeMetric,
      `Proportion Difference` = propDiffMetric
    ),
    datasetID = dataset_id,
    name = dataset_name,
    description = description
  )

  trio$dataSource <- "experimenthub"
  trio$dataSourceID <- dataset_id

  trio
}

build_simbench_combined_trios <- function(
  dataset_map = NULL,
  drop_ids = c("EH5384"),
  pearson_sample_size = 10000L,
  seed = 1L,
  p_sig = 0.1,
  bd_threshold = 0.3
) {
  assert_packages(c(data_property_packages, biological_signal_packages, simbench_packages))

  alldata <- fetch_simbench_datasets()

  if (is.null(dataset_map)) {
    dataset_map <- default_dataset_map(alldata)
  }

  dataset_map <- dataset_map[!names(dataset_map) %in% drop_ids]
  trios <- list()

  for (id in names(dataset_map)) {
    ds_name <- dataset_map[[id]]
    message("Building combined SimBench Trio for ", ds_name, " (", id, ")")

    obj <- alldata[[id]]
    counts_mat <- get_counts_matrix(obj, ds_name)
    celltype <- get_celltype(obj, ds_name)

    trios[[ds_name]] <- build_combined_simbench_trio(
      counts_mat = counts_mat,
      celltype = celltype,
      dataset_id = id,
      dataset_name = ds_name,
      pearson_sample_size = pearson_sample_size,
      seed = seed,
      p_sig = p_sig,
      bd_threshold = bd_threshold
    )
  }

  trios
}

if (sys.nframe() == 0L) {
  build_simbench_combined_trios()
}
