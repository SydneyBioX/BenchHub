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
trio <- BenchHub:::private_example_submission_trio()
#> Error: object 'private_example_submission_trio' not found
collectTaskSubmissionInfo(
  trio,
  defaults = BenchHub:::private_example_task_args()
)
#> Error: object 'trio' not found
```
