# Build DatasetTaskMetric submission rows from a Trio object

This helper creates one `DatasetTaskMetric` row for each task-metric
pairing. It does not upload anything and does not modify the existing
writeCTD() path.

## Usage

``` r
buildDatasetTaskMetricSubmission(
  trio,
  datasetTaskID,
  taskMetrics = NULL,
  evidenceTaskID = NULL,
  evidenceName = names(trio$evidence)
)
```

## Arguments

- trio:

  A `Trio` object.

- datasetTaskID:

  Task identifier(s) to link metrics to. Defaults to `NA` generated task
  IDs when not supplied.

- taskMetrics:

  A named list mapping each `datasetTaskID` to its metric IDs. If
  omitted, metrics are inferred from the evidence assigned to each task
  via `evidenceTaskID`.

- evidenceTaskID:

  Character vector mapping each Trio evidence entry to a
  `datasetTaskID`. Used when `taskMetrics` is not supplied.

- evidenceName:

  Character vector of Trio evidence names corresponding to
  `evidenceTaskID`. Defaults to all evidence in `trio`.

## Value

A `data.frame` matching the proposed `DatasetTaskMetric` table schema.
`datasetTaskMetricID` and `metricID` are left as `NA`.
