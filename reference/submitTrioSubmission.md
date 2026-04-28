# Submit a Trio submission payload to Google Apps Script

This helper posts the Trio submission payload to a Google Apps Script
endpoint using the same redirect-tolerant approach as the prototype
script.

## Usage

``` r
submitTrioSubmission(submission, url, submittedBy, submittedType = "Trio")
```

## Arguments

- submission:

  A submission object returned by
  [`buildTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildTrioSubmission.md).

- url:

  Google Apps Script endpoint URL.

- submittedBy:

  Submitter email or identifier.

- submittedType:

  Submission type label. Defaults to `"Trio"`.

## Value

A list containing request status information and response text.

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

if (interactive() && curl::has_internet()) {
  response <- submitTrioSubmission(
    submission,
    url = "https://script.google.com/macros/s/example/exec",
    submittedBy = "researcher@example.org"
  )
  names(response)
}
```
