# Interactively prepare a Trio submission bundle

This high-level helper provides a `writeCTD()`-style console workflow
for the new five-table submission path. It collects dataset, task,
evidence, and metric metadata; optionally prepares local files for
upload and verifies a Figshare article; optionally uploads custom
metrics to a GitHub gist; and returns all submission tables ready for
review before calling
[`submitTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/submitTrioSubmission.md).

## Usage

``` r
writeSubmission(
  trio,
  n_tasks = NULL,
  dataset_defaults = list(),
  task_defaults = list(),
  evidence_defaults = list(),
  metric_defaults = list(),
  prepare_files = TRUE,
  file_args = list(),
  upload_custom_metrics = FALSE,
  githubPat = Sys.getenv("GITHUB_PAT"),
  gistPublic = TRUE,
  review = TRUE,
  submit = NULL,
  url = submission_webapp_url,
  submittedBy = NULL,
  build_payload = TRUE,
  build_json = FALSE
)
```

## Arguments

- trio:

  A `Trio` object.

- n_tasks:

  Optional number of tasks to define. If `NULL`, interactive sessions
  prompt for it and non-interactive sessions require it through
  `task_defaults` or `n_tasks`.

- dataset_defaults:

  Optional named list passed into
  [`collectDatasetSubmissionInfo()`](https://sydneybiox.github.io/BenchHub/reference/collectDatasetSubmissionInfo.md).

- task_defaults:

  Optional named list passed into
  [`collectTaskSubmissionInfo()`](https://sydneybiox.github.io/BenchHub/reference/collectTaskSubmissionInfo.md).

- evidence_defaults:

  Optional named list passed into
  [`collectEvidenceSubmissionInfo()`](https://sydneybiox.github.io/BenchHub/reference/collectEvidenceSubmissionInfo.md).

- metric_defaults:

  Optional named list passed into
  [`collectMetricSubmissionInfo()`](https://sydneybiox.github.io/BenchHub/reference/collectMetricSubmissionInfo.md).

- prepare_files:

  Logical; if `TRUE`, prepare dataset/evidence files or reuse an
  existing source.

- file_args:

  Optional named list passed into
  [`prepareTrioSubmissionFiles()`](https://sydneybiox.github.io/BenchHub/reference/prepareTrioSubmissionFiles.md).

- upload_custom_metrics:

  Logical; if `TRUE`, upload custom metrics to a GitHub gist.

- githubPat:

  Optional GitHub personal access token. Defaults to the current
  `GITHUB_PAT` environment variable.

- gistPublic:

  Logical; whether any created gist should be public.

- review:

  Logical; if `TRUE`, print a compact review summary before returning.

- submit:

  Logical; if `TRUE`, submit the built payload at the end. If `NULL`,
  interactive sessions ask whether to submit after the review step.

- url:

  Optional Google Apps Script endpoint URL used when `submit = TRUE`.

- submittedBy:

  Optional submitter email or identifier used when `submit = TRUE`.

- build_payload:

  Logical; if `TRUE`, attach the nested payload object.

- build_json:

  Logical; if `TRUE`, attach the JSON string.

## Value

A named list containing collected arguments, optional prepared file and
metric metadata, the final `submission`, and optional `payload`/`json`.

## Examples

``` r
data <- data.frame(feature = c(1, 2, 3), row.names = paste0("sample", 1:3))
labels <- factor(c("A", "B", "A"))
names(labels) <- rownames(data)
trio <- Trio$new(
  data = data,
  evidence = list(class_labels = list(
    evidence = labels,
    metrics = "macroF1Metric"
  )),
  metrics = list(macroF1Metric = macroF1Metric),
  name = "example_dataset",
  description = "A small example dataset."
)
result <- writeSubmission(
  trio = trio,
  n_tasks = 1,
  dataset_defaults = list(
    dataType = "omics",
    dataModality = "transcriptomics",
    technology = "RNA-seq",
    tissue = "blood",
    status = "healthy"
  ),
  task_defaults = list(
    taskStage = "prediction",
    taskType = "classification",
    taskName = "class_prediction"
  ),
  evidence_defaults = list(
    taskName = "class_prediction",
    evidenceType = "experimental_ground_truth"
  ),
  metric_defaults = list(metricType = "label_based"),
  prepare_files = FALSE,
  build_json = FALSE,
  review = FALSE
)
names(result)
#> [1] "dataset_args"  "task_args"     "evidence_args" "metric_args"  
#> [5] "files"         "metrics"       "submission"    "payload"      
```
