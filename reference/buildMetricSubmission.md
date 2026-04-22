# Build Metric submission rows from a Trio object

This helper creates one `Metric` row per metric in the Trio object. It
does not upload anything and does not modify the existing writeCTD()
path.

## Usage

``` r
buildMetricSubmission(trio)
```

## Arguments

- trio:

  A `Trio` object.

## Value

A `data.frame` matching the proposed `Metric` table schema.

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
buildMetricSubmission(trio)
#>               metricID    metricName wrapper_r metricType metricSourceType
#> macroF1Metric     <NA> macroF1Metric      <NA>       <NA>             gist
#>               metricKey wrapper_py gist_url
#> macroF1Metric      <NA>       <NA>     <NA>
```
