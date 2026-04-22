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
getSubmissionStudy(
  "study_001",
  studies = BenchHub:::private_example_existing_study_rows()
)
#> Error in private_resolve_submission_studies(studies = studies, ss = ss): object 'private_example_existing_study_rows' not found
```
