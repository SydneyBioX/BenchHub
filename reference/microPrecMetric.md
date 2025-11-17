# Micro Precision Metric

Computes the micro precision of the predictions.

## Usage

``` r
microPrecMetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The micro precision.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
microPrecMetric(evidence, predicted)
#> [1] 0.75
```
