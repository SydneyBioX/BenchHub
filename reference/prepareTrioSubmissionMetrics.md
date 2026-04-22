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
prepareTrioSubmissionMetrics(trio)
#> $Metric
#>               metricID    metricName     wrapper_r metricType metricSourceType
#> macroF1Metric     <NA> macroF1Metric macroF1Metric       <NA>             gist
#>               metricKey wrapper_py gist_url
#> macroF1Metric      <NA>       <NA>     <NA>
#> 
#> $gist
#> NULL
#> 
#> $custom_metric_lines
#> [1] "macroF1Metric <- function (evidence, predicted) "                                    
#> [2] "{"                                                                                   
#> [3] "    2 * macroPrecMetric(evidence, predicted) * macroRecMetric(evidence, "            
#> [4] "        predicted)/(macroPrecMetric(evidence, predicted) + macroRecMetric(evidence, "
#> [5] "        predicted))"                                                                 
#> [6] "}"                                                                                   
#> [7] "macroF1MetricArgs <- NULL"                                                           
#> 
```
