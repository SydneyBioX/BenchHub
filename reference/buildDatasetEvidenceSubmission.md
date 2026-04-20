# Build DatasetEvidence submission rows from a Trio object

This helper creates one `DatasetEvidence` row per supporting evidence
entry in the Trio object. It does not upload anything and does not
modify the existing writeCTD() path.

## Usage

``` r
buildDatasetEvidenceSubmission(
  trio,
  datasetTaskID,
  evidenceName = names(trio$evidence),
  evidenceType = NA_character_
)
```

## Arguments

- trio:

  A `Trio` object.

- datasetTaskID:

  Task identifier to link evidence rows to. Defaults to generated task
  IDs when not supplied.

- evidenceName:

  Character vector of Trio evidence names to include. Defaults to all
  evidence in `trio`.

- evidenceType:

  Either `"columns"` or `"figshare"` indicating where to find the
  evidence.

## Value

A `data.frame` matching the proposed `DatasetEvidence` table schema.
`evidenceID` is left as `NA`.
