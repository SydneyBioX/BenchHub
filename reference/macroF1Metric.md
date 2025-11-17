# Macro F1 Score Metric

Computes the macro F1 score of the predictions.

## Usage

``` r
macroF1Metric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The macro F1 score.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
macroF1Metric(evidence, predicted)
#> [1] 0.7894737
```
