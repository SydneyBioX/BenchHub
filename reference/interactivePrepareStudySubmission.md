# Interactively prepare a Study submission

Interactively prepare a Study submission

## Usage

``` r
interactivePrepareStudySubmission(
  study,
  ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
  githubPat = Sys.getenv("GITHUB_PAT"),
  gistPublic = TRUE,
  url = submission_webapp_url,
  review = TRUE,
  build_payload = TRUE,
  build_json = TRUE
)
```

## Arguments

- study:

  A `BenchmarkStudy` object.

- ss:

  Submission spreadsheet ID or URL.

- githubPat:

  Optional GitHub personal access token.

- gistPublic:

  Logical; whether uploaded gists should be public.

- url:

  Google Apps Script endpoint URL. Defaults to the package submission
  web app.

- review:

  Whether to print a short submission summary. Defaults to `TRUE`.

- build_payload:

  Whether to include the nested payload. Defaults to `TRUE`.

- build_json:

  Whether to include JSON output. Defaults to `TRUE`.

## Value

A named list containing the Study submission bundle.
