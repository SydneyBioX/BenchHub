# Download a BenchmarkStudy from the submission database

Download a BenchmarkStudy from the submission database

## Usage

``` r
downloadSubmissionStudy(
  studyID,
  name = NULL,
  version = NULL,
  ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
  cachePath = tempdir()
)
```

## Arguments

- studyID:

  Study identifier from the `Study` table.

- name:

  Optional study name fallback for compatibility. Prefer `studyID` for
  new code.

- version:

  Optional version string used only when loading by `name`.

- ss:

  Google Sheets spreadsheet ID containing the submission tables.

- cachePath:

  Directory for downloaded files. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

A populated `BenchmarkStudy` object.

## Examples

``` r
if (interactive()) {
  study <- downloadSubmissionStudy(
    studyID =  "ST005", 
    cachePath = tempdir()
  )
  study
}
```
