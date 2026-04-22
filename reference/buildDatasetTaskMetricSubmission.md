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
buildDatasetTaskMetricSubmission(
  trio,
  datasetTaskID = "task_001",
  evidenceTaskID = "task_001"
)
#>   datasetTaskMetricID datasetTaskID      metricID
#> 1                <NA>      task_001 macroF1Metric
```
