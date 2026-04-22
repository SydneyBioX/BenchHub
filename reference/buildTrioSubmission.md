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
dataset_args <- list(
  dataType = "omics",
  dataModality = "transcriptomics",
  technology = "RNA-seq",
  tissue = "blood",
  status = "healthy"
)
task_args <- list(
  taskStage = "prediction",
  taskType = "classification",
  taskName = "class_prediction"
)
submission <- buildTrioSubmission(
  trio = trio,
  dataset_args = dataset_args,
  task_args = task_args,
  evidence_task_map = c(class_labels = "class_prediction")
)
names(submission)
#> [1] "Dataset"           "DatasetTask"       "DatasetEvidence"  
#> [4] "Metric"            "DatasetTaskMetric" "submission_links" 
```
