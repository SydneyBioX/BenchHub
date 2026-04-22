# Build Study and StudyDataset submission tables

Build Study and StudyDataset submission tables

## Usage

``` r
buildStudySubmission(
  study,
  datasetIDs,
  existing_studies = NULL,
  ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
  version = NULL,
  type = NULL,
  protocolGist = "",
  mappingFunctions = ""
)
```

## Arguments

- study:

  A `BenchmarkStudy` object.

- datasetIDs:

  Character vector of existing dataset IDs to link.

- existing_studies:

  Optional data frame of current Study rows.

- ss:

  Submission spreadsheet ID or URL. Used when `existing_studies` is not
  supplied.

- version:

  Optional version override.

- type:

  Optional type override. Must be `"original"` or `"update"` when
  provided.

- protocolGist:

  Optional protocol gist URL.

- mappingFunctions:

  Optional mapping functions gist URL.

## Value

A named list containing `Study` and `StudyDataset`.

## Examples

``` r
study <- BenchmarkStudy$new(name = "example_study")
study$description <- "A small example benchmark study."
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
buildStudySubmission(
  study,
  datasetIDs = "dataset_001",
  existing_studies = existing_studies
)
#> $Study
#>   studyID     studyName version                      description     type
#> 1    <NA> example_study   0.0.1 A small example benchmark study. original
#>   protocolGist mappingFunctions
#> 1         <NA>             <NA>
#> 
#> $StudyDataset
#>   studyDatasetID studyID   datasetID
#> 1           <NA>    <NA> dataset_001
#> 
```
