# Macro Precision Metric

Computes the macro precision of the predictions.

## Usage

``` r
macroPrecMetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The macro precision.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
macroPrecMetric(evidence, predicted)
#> [1] 0.8333333
```
