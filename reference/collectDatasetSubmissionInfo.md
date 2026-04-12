# Collect Dataset submission metadata interactively

This helper gathers the dataset-level fields needed for the `Dataset`
table. It reuses `Trio` metadata when already available and only prompts
for the remaining values in interactive sessions.

## Usage

``` r
collectDatasetSubmissionInfo(trio, defaults = list())
```

## Arguments

- trio:

  A `Trio` object.

- defaults:

  Optional named list of pre-filled values. Any non-`NULL` value here
  bypasses the interactive prompt for that field.

## Value

A named list ready to pass as `dataset_args` to
[`buildDatasetSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildDatasetSubmission.md).
