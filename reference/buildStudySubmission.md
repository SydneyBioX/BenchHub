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
