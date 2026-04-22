# Get one existing Study row by studyID

Get one existing Study row by studyID

## Usage

``` r
getSubmissionStudy(
  studyID,
  studies = NULL,
  ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg"
)
```

## Arguments

- studyID:

  Existing Study identifier.

- studies:

  Optional data frame of Study rows.

- ss:

  Submission spreadsheet ID or URL.

## Value

A one-row data frame for the requested Study.

## Examples

``` r
studies <- data.frame(
  studyID = "study_001",
  studyName = "example_study",
  version = "0.0.1",
  description = "A small example benchmark study.",
  type = "original",
  protocolGist = "",
  mappingFunctions = "",
  stringsAsFactors = FALSE
)
getSubmissionStudy(
  "study_001",
  studies = studies
)
#>     studyID     studyName version                      description     type
#> 1 study_001 example_study   0.0.1 A small example benchmark study. original
#>   protocolGist mappingFunctions
#> 1                              
```
