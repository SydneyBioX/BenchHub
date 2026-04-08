# Shared submission helpers for SimBench Trios.
#
# These helpers keep the repeated submission metadata in one place. They are
# intended for repeated manual submission of SimBench Trios that share the same
# tasks, evidence type, and metric classifications.

simbench_biological_signal_names <- c(
  "differentially_expression",
  "differentially_variable",
  "differentially_distributed",
  "differential_prop",
  "bimodally_distributed"
)

simbench_evidence_task_names <- c(
  frac_zero_genes = "fraction zero per gene",
  frac_zero_cells = "fraction zero per cell",
  lib_size_cells = "library size per cell",
  efflib_size_cells = "effective library size per cell",
  tmm_cells = "weighted trimmed mean of M-values per cell",
  scaled_var_cells = "scaled variance per cell",
  scaled_mean_cells = "scaled mean per cell",
  lib_fraczero_cells = "library size vs fraction zero per cell",
  pearson_cells = "pearson correlation per cell",
  scaled_var_genes = "scaled variance per gene",
  scaled_mean_genes = "scaled mean per gene",
  pearson_genes = "pearson correlation per gene",
  mean_var_genes = "mean vs variance per gene",
  mean_fraczero_genes = "mean vs fraction zero per gene",
  differentially_expression = "differential expression gene",
  differentially_variable = "differentially variable genes",
  differentially_distributed = "differentially distributed genes",
  differential_prop = "differential proportion genes",
  bimodally_distributed = "bimodally distributed genes"
)

simbench_task_defaults <- function(trio) {
  evidence_names <- names(trio$evidence)
  task_names <- unname(simbench_evidence_task_names[evidence_names])

  if (any(is.na(task_names))) {
    missing <- evidence_names[is.na(task_names)]
    stop(
      "Missing SimBench task default(s) for evidence: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    taskStage = rep("upstream", length(task_names)),
    taskType = rep("feature_quantification", length(task_names)),
    taskName = task_names
  )
}

simbench_evidence_defaults <- function(trio) {
  evidence_names <- names(trio$evidence)
  task_name <- simbench_evidence_task_names[evidence_names]

  if (any(is.na(task_name))) {
    missing <- evidence_names[is.na(task_name)]
    stop(
      "Missing SimBench task default(s) for evidence: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    taskName = unname(task_name),
    evidenceType = rep("derived_proxy", length(evidence_names))
  )
}

simbench_metric_defaults <- function(trio) {
  metric_type <- c(
    KDE = "distribution_based",
    `Proportion Difference` = "error_based"
  )

  list(
    metricType = unname(metric_type[names(trio$metrics)])
  )
}

simbench_dataset_defaults <- function(
  trio,
  organism = NA_character_,
  tissue = "mixed",
  status = "other",
  doi = NA_character_,
  technology = "single-cell RNA-seq"
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

prepare_simbench_submission <- function(
  trio,
  dataset_defaults = simbench_dataset_defaults(trio),
  output_dir = file.path("ignore", "simbench", "submission", trio$dataSourceID),
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
    task_defaults = simbench_task_defaults(trio),
    evidence_defaults = simbench_evidence_defaults(trio),
    metric_defaults = simbench_metric_defaults(trio),
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
