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
trio <- BenchHub:::private_example_submission_trio()
#> Error: object 'private_example_submission_trio' not found
buildDatasetTaskSubmission(
  trio,
  taskStage = "prediction",
  taskType = "classification",
  taskName = "class_prediction"
)
#> Error: object 'trio' not found
```
