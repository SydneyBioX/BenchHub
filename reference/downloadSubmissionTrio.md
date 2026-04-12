# Download a Trio from the five-table submission database

Download a Trio from the five-table submission database

## Usage

``` r
downloadSubmissionTrio(
  datasetID,
  ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
  cachePath = tempdir()
)
```

## Arguments

- datasetID:

  Dataset identifier from the `Dataset` table.

- ss:

  Google Sheets spreadsheet ID containing the five database tables.

- cachePath:

  Directory for downloaded files. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

A populated `Trio` object.
