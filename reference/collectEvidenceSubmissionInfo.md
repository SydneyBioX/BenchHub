# Collect DatasetEvidence submission metadata interactively

This helper gathers the evidence-to-task assignment and evidence type
for each supporting evidence item in a `Trio`.

## Usage

``` r
collectEvidenceSubmissionInfo(trio, task_args, defaults = list())
```

## Arguments

- trio:

  A `Trio` object.

- task_args:

  Named list returned by
  [`collectTaskSubmissionInfo()`](https://sydneybiox.github.io/BenchHub/reference/collectTaskSubmissionInfo.md).

- defaults:

  Optional named list with entries `taskName` and `evidenceType`. Each
  entry may be length 1 or length equal to the number of supporting
  evidence items in `trio`.

## Value

A named list with `datasetTaskID`, `evidenceName`, `evidenceType`, and
`evidence_task_map`.

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
task_args <- list(
  taskStage = "prediction",
  taskType = "classification",
  taskName = "class_prediction"
)
collectEvidenceSubmissionInfo(
  trio,
  task_args = task_args,
  defaults = list(
    taskName = "class_prediction",
    evidenceType = "experimental_ground_truth"
  )
)
#> $datasetTaskID
#> [1] "SUBMISSION_TASK_1"
#> 
#> $evidenceName
#> [1] "class_labels"
#> 
#> $evidenceType
#> [1] "experimental_ground_truth"
#> 
#> $evidence_task_map
#>       class_labels 
#> "class_prediction" 
#> 
```
