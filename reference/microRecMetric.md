# Micro Recall Metric

Computes the micro recall of the predictions.

## Usage

``` r
microRecMetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The micro recall.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
microRecMetric(evidence, predicted)
#> [1] 0.75
```
