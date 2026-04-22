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
study <- BenchmarkStudy$new(name = "example_study")
study$description <- "A small example benchmark study."
available_datasets <- data.frame(
  datasetID = "dataset_001",
  name = "example_dataset",
  stringsAsFactors = FALSE
)
existing_studies <- data.frame(
  studyID = character(0),
  studyName = character(0),
  version = character(0),
  description = character(0),
  type = character(0),
  protocolGist = character(0),
  mappingFunctions = character(0),
  stringsAsFactors = FALSE
)
collectStudySubmissionInfo(
  study,
  datasetIDs = "dataset_001",
  available_datasets = available_datasets,
  existing_studies = existing_studies
)
#> $datasetIDs
#> [1] "dataset_001"
#> 
#> $type
#> [1] "original"
#> 
#> $version
#> [1] "0.0.1"
#> 
#> $protocolGist
#> [1] NA
#> 
#> $mappingFunctions
#> [1] NA
#> 
```
