# Build a pre-submit Trio submission bundle

This is the higher-level R interface for the new submission workflow. It
can prepare dataset/evidence files, prepare metric metadata, build the
five-table submission object, and return the payload/JSON for inspection
before calling
[`submitTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/submitTrioSubmission.md).

## Usage

``` r
prepareTrioSubmissionBundle(
  trio,
  dataset_args,
  task_args,
  evidence_task_map,
  prepare_files = FALSE,
  file_args = list(),
  prepare_metrics = FALSE,
  metric_args = list(),
  build_payload = TRUE,
  build_json = FALSE
)
```

## Arguments

- trio:

  A `Trio` object.

- dataset_args:

  Named list passed to
  [`buildDatasetSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildDatasetSubmission.md).

- task_args:

  Named list passed to
  [`buildDatasetTaskSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildDatasetTaskSubmission.md).

- evidence_task_map:

  Named character vector mapping Trio evidence names to submission task
  names.

- prepare_files:

  Logical; if `TRUE`, call
  [`prepareTrioSubmissionFiles()`](https://sydneybiox.github.io/BenchHub/reference/prepareTrioSubmissionFiles.md).

- file_args:

  Named list of additional arguments for
  [`prepareTrioSubmissionFiles()`](https://sydneybiox.github.io/BenchHub/reference/prepareTrioSubmissionFiles.md),
  excluding `trio`.

- prepare_metrics:

  Logical; if `TRUE`, call
  [`prepareTrioSubmissionMetrics()`](https://sydneybiox.github.io/BenchHub/reference/prepareTrioSubmissionMetrics.md).

- metric_args:

  Named list of additional arguments for
  [`prepareTrioSubmissionMetrics()`](https://sydneybiox.github.io/BenchHub/reference/prepareTrioSubmissionMetrics.md),
  excluding `trio`.

- build_payload:

  Logical; if `TRUE`, attach the nested payload structure.

- build_json:

  Logical; if `TRUE`, attach the JSON string.

## Value

A named list containing the built `submission`, plus optional `files`,
`metrics`, `payload`, and `json`.

## Examples

``` r
trio <- BenchHub:::private_example_submission_trio()
#> Error: object 'private_example_submission_trio' not found
bundle <- prepareTrioSubmissionBundle(
  trio = trio,
  dataset_args = BenchHub:::private_example_dataset_args(),
  task_args = BenchHub:::private_example_task_args(),
  evidence_task_map = c(class_labels = "class_prediction")
)
#> Error: object 'trio' not found
names(bundle)
#> Error: object 'bundle' not found
```
