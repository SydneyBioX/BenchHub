# Convert a combined Trio submission to payload structure

This helper converts the output of
[`buildTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildTrioSubmission.md)
into the nested list structure used by the JSON payload in the prototype
submission script.

## Usage

``` r
buildTrioSubmissionPayload(submission)
```

## Arguments

- submission:

  A submission object returned by
  [`buildTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildTrioSubmission.md).

## Value

A named list with a top-level `payload` entry ready for
`jsonlite::toJSON(..., auto_unbox = TRUE)`.

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
submission <- buildTrioSubmission(
  trio = trio,
  dataset_args = list(
    dataType = "omics",
    dataModality = "transcriptomics",
    technology = "RNA-seq",
    tissue = "blood",
    status = "healthy"
  ),
  task_args = list(
    taskStage = "prediction",
    taskType = "classification",
    taskName = "class_prediction"
  ),
  evidence_task_map = c(class_labels = "class_prediction")
)
payload <- buildTrioSubmissionPayload(submission)
names(payload)
#> [1] "payload"          "submission_links"
```
