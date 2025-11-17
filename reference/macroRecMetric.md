# Macro Recall Metric

Computes the macro recall of the predictions.

## Usage

``` r
macroRecMetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The macro recall.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
macroRecMetric(evidence, predicted)
#> [1] 0.75
```
