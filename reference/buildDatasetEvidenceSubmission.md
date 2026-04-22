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

## Examples

``` r
data <- data.frame(feature = c(1, 2, 3), row.names = paste0("sample", 1:3))
labels <- factor(c("A", "B", "A"))
names(labels) <- rownames(data)
trio <- Trio$new(
  data = data,
  evidence = list(class_labels = list(
    evidence = labels,
    metrics = "macroF1Metric"
  )),
  metrics = list(macroF1Metric = macroF1Metric),
  name = "example_dataset",
  description = "A small example dataset."
)
buildDatasetEvidenceSubmission(
  trio,
  datasetTaskID = "task_001",
  evidenceType = "experimental_ground_truth"
)
#>              evidenceID datasetTaskID supportingEvidence is_in_data
#> class_labels       <NA>      task_001       class_labels       TRUE
#>              evidenceSourceID              evidenceType
#> class_labels             <NA> experimental_ground_truth
```
