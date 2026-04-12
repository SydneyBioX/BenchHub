# Prepare Trio metric metadata for submission

This helper aligns the new submission workflow with the older
`writeCTD()` metric handling. Internal package metrics are recorded
directly. Custom metrics can optionally be uploaded to a GitHub gist and
linked from the resulting `Metric` table rows.

## Usage

``` r
prepareTrioSubmissionMetrics(
  trio,
  name = trio$name,
  uploadCustom = FALSE,
  githubPat = NULL,
  gistPublic = TRUE
)
```

## Arguments

- trio:

  A `Trio` object.

- name:

  Dataset name used for the gist filename. Defaults to `trio$name`.

- uploadCustom:

  Logical; if `TRUE`, custom metrics are uploaded to a GitHub gist.

- githubPat:

  Optional GitHub personal access token with gist scope.

- gistPublic:

  Logical; whether the created gist should be public.

## Value

A list with entries `Metric`, `gist`, and `custom_metric_lines`.
