# Micro F1 Score Metric

Computes the micro F1 score of the predictions.

## Usage

``` r
microF1Metric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The micro F1 score.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
microF1Metric(evidence, predicted)
#> [1] 0.75
```
