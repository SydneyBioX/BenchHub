# Balanced Accuracy Metric

Computes the balanced accuracy of the predictions.

## Usage

``` r
balAccMetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The balanced accuracy.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
balAccMetric(evidence, predicted)
#> [1] 0.75
```
