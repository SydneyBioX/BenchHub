# Get linked StudyDataset rows by studyID

Get linked StudyDataset rows by studyID

## Usage

``` r
getSubmissionStudyDatasets(
  studyID,
  study_datasets = NULL,
  ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg"
)
```

## Arguments

- studyID:

  Existing Study identifier.

- study_datasets:

  Optional data frame of StudyDataset rows.

- ss:

  Submission spreadsheet ID or URL.

## Value

A data frame of StudyDataset rows linked to the supplied studyID.
