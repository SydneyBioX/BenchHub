# Shared submission helpers for SpatialSimBench Trios.
#
# These helpers mirror the SimBench submission flow but cover the combined
# SpatialSimBench Trio, which includes upstream data-property evidence and
# downstream spatial tasks.

spatialsimbench_evidence_task_names <- c(
  frac_zero_genes = "fraction zero per gene",
  frac_zero_cells = "fraction zero per spot",
  lib_size_cells = "library size per spot",
  efflib_size_cells = "effective library size per spot",
  tmm_cells = "weighted trimmed mean of M-values per spot",
  scaled_var_cells = "scaled variance per spot",
  scaled_mean_cells = "scaled mean per spot",
  lib_fraczero_cells = "library size vs fraction zero per spot",
  pearson_cells = "pearson correlation per spot",
  scaled_var_genes = "scaled variance per gene",
  scaled_mean_genes = "scaled mean per gene",
  pearson_genes = "pearson correlation per gene",
  mean_var_genes = "mean vs variance per gene",
  mean_fraczero_genes = "mean vs fraction zero per gene",
  spatial_clustering = "spatial clustering",
  spatial_variable_gene = "spatially variable genes",
  spatial_cross_correlation = "spatial cross-correlation",
  spatial_deconvolution = "spatial deconvolution"
)

spatialsimbench_task_defaults <- function(trio) {
  evidence_names <- names(trio$evidence)
  task_name <- unname(spatialsimbench_evidence_task_names[evidence_names])

  if (any(is.na(task_name))) {
    missing <- evidence_names[is.na(task_name)]
    stop(
      "Missing SpatialSimBench task default(s) for evidence: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  task_stage <- ifelse(
    evidence_names %in% c(
      "spatial_clustering",
      "spatial_variable_gene",
      "spatial_cross_correlation",
      "spatial_deconvolution"
    ),
    "downstream",
    "upstream"
  )

  task_type <- vapply(evidence_names, function(x) {
    switch(
      x,
      spatial_clustering = "spatial_domain_detection",
      spatial_variable_gene = "biomarker_discovery",
      spatial_cross_correlation = "neighbourhood_analysis",
      spatial_deconvolution = "cell_composition_analysis",
      "feature_quantification"
    )
  }, FUN.VALUE = character(1))

  list(
    taskStage = unname(task_stage),
    taskType = unname(task_type),
    taskName = task_name
  )
}

spatialsimbench_evidence_defaults <- function(trio) {
  evidence_names <- names(trio$evidence)
  task_name <- spatialsimbench_evidence_task_names[evidence_names]

  if (any(is.na(task_name))) {
    missing <- evidence_names[is.na(task_name)]
    stop(
      "Missing SpatialSimBench evidence default(s) for evidence: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    taskName = unname(task_name),
    evidenceType = rep("derived_proxy", length(evidence_names))
  )
}

spatialsimbench_metric_defaults <- function(trio) {
  metric_type <- c(
    KDE = "distribution_based",
    ARI = "label_based",
    NMI = "label_based",
    recall = "error_based",
    precision = "error_based",
    `Mantel statistics` = "spatial_structure",
    `cosine similarity` = "spatial_structure",
    JSD = "error_based",
    RMSE = "error_based"
  )

  list(
    metricType = unname(metric_type[names(trio$metrics)])
  )
}

spatialsimbench_dataset_defaults <- function(
  trio,
  organism = NA_character_,
  tissue = "mixed",
  status = "other",
  doi = NA_character_,
  technology = "spatial transcriptomics"
) {
  list(
    name = trio$name,
    dataType = "omics",
    dataModality = "transcriptomics",
    technology = technology,
    description = trio$description,
    doi = doi,
    organism = organism,
    tissue = tissue,
    status = status
  )
}

prepare_spatialsimbench_submission <- function(
  trio,
  dataset_defaults = spatialsimbench_dataset_defaults(trio),
  output_dir = file.path("ignore", "spatialsimbench", "submission", trio$dataSourceID),
  upload_custom_metrics = TRUE,
  githubPat = Sys.getenv("GITHUB_PAT"),
  review = TRUE,
  submit = FALSE,
  url = NULL,
  submittedBy = NULL,
  build_payload = TRUE,
  build_json = TRUE
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  writeSubmission(
    trio = trio,
    n_tasks = length(trio$evidence),
    dataset_defaults = dataset_defaults,
    task_defaults = spatialsimbench_task_defaults(trio),
    evidence_defaults = spatialsimbench_evidence_defaults(trio),
    metric_defaults = spatialsimbench_metric_defaults(trio),
    prepare_files = TRUE,
    file_args = list(
      outputDir = output_dir,
      useExistingSource = TRUE,
      saveData = FALSE,
      saveEvidence = TRUE
    ),
    upload_custom_metrics = upload_custom_metrics,
    githubPat = githubPat,
    review = review,
    submit = submit,
    url = url,
    submittedBy = submittedBy,
    build_payload = build_payload,
    build_json = build_json
  )
}
