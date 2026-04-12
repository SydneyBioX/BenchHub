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
