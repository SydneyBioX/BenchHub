# Collect DatasetTaskMetric submission rows from Trio assignments

This helper builds the `DatasetTaskMetric` linking table automatically
from the evidence-to-task assignments and the metric links already
stored inside the `Trio` object.

## Usage

``` r
collectDatasetTaskMetricSubmission(trio, evidence_args, task_args)
```

## Arguments

- trio:

  A `Trio` object.

- evidence_args:

  Named list returned by
  [`collectEvidenceSubmissionInfo()`](https://sydneybiox.github.io/BenchHub/reference/collectEvidenceSubmissionInfo.md).

- task_args:

  Named list returned by
  [`collectTaskSubmissionInfo()`](https://sydneybiox.github.io/BenchHub/reference/collectTaskSubmissionInfo.md).

## Value

A `data.frame` matching the `DatasetTaskMetric` schema.

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
collectDatasetTaskMetricSubmission(
  trio,
  evidence_args = list(
    datasetTaskID = "task_001",
    evidenceName = "class_labels"
  ),
  task_args = list(
    taskStage = "prediction",
    taskType = "classification",
    taskName = "class_prediction"
  )
)
#> [1] datasetTaskMetricID datasetTaskID       metricID           
#> <0 rows> (or 0-length row.names)
```
