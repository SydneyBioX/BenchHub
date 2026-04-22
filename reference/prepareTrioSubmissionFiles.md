# Prepare Trio files and download metadata for submission

This helper reuses the good parts of the old `writeCTD()` workflow for
the new submission path. It can either save dataset/evidence files
locally so the user can upload them, or reuse an existing downloadable
source already attached to the `Trio`.

## Usage

``` r
prepareTrioSubmissionFiles(
  trio,
  name = trio$name,
  outputDir = ".",
  saveData = NULL,
  saveEvidence = NULL,
  useExistingSource = NULL,
  figshareUrl = NULL,
  datasetFileName = NULL,
  evidenceFileName = NULL,
  verifyFigshare = FALSE,
  skipMd5Check = FALSE
)
```

## Arguments

- trio:

  A `Trio` object.

- name:

  Dataset name used for generated filenames. Defaults to `trio$name`.

- outputDir:

  Directory for generated `.rds` files. Defaults to `"."`.

- saveData:

  Logical indicating whether to save `trio$data` to an RDS file. If
  `NULL` and interactive, the user is prompted.

- saveEvidence:

  Logical indicating whether to save all supporting evidence to a single
  RDS file. If `NULL` and interactive, defaults to `TRUE`.

- useExistingSource:

  Logical indicating whether to keep using
  `trio$dataSource`/`trio$dataSourceID` rather than preparing uploaded
  files. If `NULL` and interactive, the user is prompted when an
  existing source is available.

- figshareUrl:

  Optional Figshare URL to verify against prepared files.

- datasetFileName:

  Optional dataset filename to match inside the Figshare article.

- evidenceFileName:

  Optional evidence filename to match inside the Figshare article.

- verifyFigshare:

  Logical; if `TRUE`, verify the prepared files against the Figshare
  article metadata.

- skipMd5Check:

  Logical; if `TRUE`, allow verification without MD5 matching.

## Value

A named list describing the saved files and/or reusable download sources
for dataset and evidence.

## Examples

``` r
trio <- BenchHub:::private_example_submission_trio()
#> Error: object 'private_example_submission_trio' not found
files <- prepareTrioSubmissionFiles(
  trio,
  outputDir = tempdir(),
  saveData = TRUE,
  saveEvidence = TRUE,
  useExistingSource = FALSE
)
#> Error: object 'trio' not found
names(files)
#> Error: object 'files' not found
```
