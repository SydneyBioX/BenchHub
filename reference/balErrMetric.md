# Balanced Error Metric

Computes the balanced error of the predictions.

## Usage

``` r
balErrMetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The balanced error.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
balErrMetric(evidence, predicted)
#> [1] 0.25
```
