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
