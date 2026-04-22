# Collect Metric submission metadata interactively

This helper gathers user-supplied metric classification for each metric
in a `Trio`. The metric names and wrappers still come from the Trio
object and the package helper logic.

## Usage

``` r
collectMetricSubmissionInfo(trio, defaults = list())
```

## Arguments

- trio:

  A `Trio` object.

- defaults:

  Optional named list with entry `metricType`. It may be length 1 or
  length equal to the number of metrics in the Trio.

## Value

A named list ready to merge into the `Metric` submission table.

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
collectMetricSubmissionInfo(
  trio,
  defaults = list(metricType = "label_based")
)
#> $metricName
#> [1] "macroF1Metric"
#> 
#> $metricType
#> macroF1Metric 
#> "label_based" 
#> 
```
