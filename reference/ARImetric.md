# Adjusted Rand Index (ARI) Metric

Computes the adjusted Rand index between two cluster labelings.

## Usage

``` r
ARImetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The adjusted Rand index.

## Examples

``` r
evidence <- factor(c("A", "A", "B", "B"))
predicted <- factor(c("A", "A", "B", "B"))
ARImetric(evidence, predicted)
#> [1] 1
```
