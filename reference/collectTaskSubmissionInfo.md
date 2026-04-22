# Collect DatasetTask submission metadata interactively

This helper gathers the task-level fields needed for the `DatasetTask`
table. It does not modify the `Trio` object.

## Usage

``` r
collectTaskSubmissionInfo(trio, defaults = list(), n_tasks = NULL)
```

## Arguments

- trio:

  A `Trio` object.

- defaults:

  Optional named list of pre-filled task values. Supported entries are
  `taskStage`, `taskType`, and `taskName`. Each entry may be length 1 or
  length `n_tasks`.

- n_tasks:

  Number of tasks to define. If `NULL`, interactive sessions prompt for
  this value, while non-interactive sessions infer it from the longest
  defaults vector.

## Value

A named list ready to pass as `task_args` to
[`buildDatasetTaskSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildDatasetTaskSubmission.md).

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
collectTaskSubmissionInfo(
  trio,
  defaults = list(
    taskStage = "prediction",
    taskType = "classification",
    taskName = "class_prediction"
  )
)
#> $taskStage
#> [1] "prediction"
#> 
#> $taskType
#> [1] "classification"
#> 
#> $taskName
#> [1] "class_prediction"
#> 
```
