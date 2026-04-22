# Collect Dataset submission metadata interactively

This helper gathers the dataset-level fields needed for the `Dataset`
table. It reuses `Trio` metadata when already available and only prompts
for the remaining values in interactive sessions.

## Usage

``` r
collectDatasetSubmissionInfo(trio, defaults = list())
```

## Arguments

- trio:

  A `Trio` object.

- defaults:

  Optional named list of pre-filled values. Any non-`NULL` value here
  bypasses the interactive prompt for that field.

## Value

A named list ready to pass as `dataset_args` to
[`buildDatasetSubmission()`](https://sydneybiox.github.io/BenchHub/reference/buildDatasetSubmission.md).

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
collectDatasetSubmissionInfo(
  trio,
  defaults = list(
    dataType = "omics",
    dataModality = "transcriptomics",
    technology = "RNA-seq",
    tissue = "blood",
    status = "healthy"
  )
)
#> $name
#> [1] "example_dataset"
#> 
#> $dataType
#> [1] "omics"
#> 
#> $dataModality
#> [1] "transcriptomics"
#> 
#> $technology
#> [1] "RNA-seq"
#> 
#> $description
#> [1] "A small example dataset."
#> 
#> $doi
#> [1] NA
#> 
#> $organism
#> [1] NA
#> 
#> $tissue
#> [1] "blood"
#> 
#> $status
#> [1] "healthy"
#> 
```
