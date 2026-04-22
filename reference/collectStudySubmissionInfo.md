# Collect Study submission metadata

Collect Study submission metadata

## Usage

``` r
collectStudySubmissionInfo(
  study,
  datasetIDs = NULL,
  available_datasets = NULL,
  existing_studies = NULL,
  ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
  defaults = list()
)
```

## Arguments

- study:

  A `BenchmarkStudy` object.

- datasetIDs:

  Optional character vector of dataset IDs. When `NULL`, datasets can be
  inferred from `study$trios` or selected interactively.

- available_datasets:

  Optional data frame of available Dataset rows.

- existing_studies:

  Optional data frame of current Study rows.

- ss:

  Submission spreadsheet ID or URL.

- defaults:

  Optional named list with entries such as `datasetIDs`, `version`,
  `type`, `protocolGist`, and `mappingFunctions`.

## Value

A named list ready to pass into
[`buildStudySubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildStudySubmission.md).

## Examples

``` r
study <- BenchHub:::private_example_study()
#> Error: object 'private_example_study' not found
collectStudySubmissionInfo(
  study,
  datasetIDs = "dataset_001",
  available_datasets = BenchHub:::private_example_available_datasets(),
  existing_studies = BenchHub:::private_example_existing_studies()
)
#> Error: object 'study' not found
```
