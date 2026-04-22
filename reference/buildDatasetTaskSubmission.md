# Build DatasetTask submission rows from a Trio object

This helper creates one `DatasetTask` row per supporting evidence name
in the Trio object. It does not upload anything and does not modify the
existing writeCTD() path.

## Usage

``` r
buildDatasetTaskSubmission(
  trio,
  datasetID = NA_character_,
  taskStage,
  taskType,
  taskName
)
```

## Arguments

- trio:

  A `Trio` object.

- datasetID:

  Dataset identifier to link tasks to. Defaults to `NA` so the backend
  can fill it later.

- taskStage:

  A character vector giving the task stage for each task row.

- taskType:

  A character vector giving the task type for each task row.

- taskName:

  A character vector giving the task name for each task row.

## Value

A `data.frame` matching the proposed `DatasetTask` table schema.
`datasetTaskID` is left as `NA`.

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
buildDatasetTaskSubmission(
  trio,
  taskStage = "prediction",
  taskType = "classification",
  taskName = "class_prediction"
)
#>   datasetTaskID datasetID  taskStage       taskType         taskName
#> 1          <NA>      <NA> prediction classification class_prediction
```
