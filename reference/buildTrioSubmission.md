# Build a combined Trio submission object

This helper assembles the five Trio-focused submission tables into a
single list so it can be inspected locally before any upload step is
added.

## Usage

``` r
buildTrioSubmission(trio, dataset_args, task_args, evidence_task_map)
```

## Arguments

- trio:

  A `Trio` object.

- dataset_args:

  Named list of arguments passed to
  [`buildDatasetSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildDatasetSubmission.md),
  excluding `trio`.

- task_args:

  Named list of arguments passed to
  [`buildDatasetTaskSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildDatasetTaskSubmission.md),
  excluding `trio`.

- evidence_task_map:

  Character vector mapping Trio evidence names to the corresponding task
  names in the submission. Names must be evidence names from
  `trio$evidence`, values must be task names supplied in `task_args`.

## Value

A named list containing `Dataset`, `DatasetTask`, `DatasetEvidence`,
`Metric`, `DatasetTaskMetric`, and `submission_links`.
