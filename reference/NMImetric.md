# Normalized Mutual Information (NMI) Metric

Computes the normalized mutual information between two cluster
labelings.

## Usage

``` r
NMImetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The normalized mutual information.

## Examples

``` r
evidence <- factor(c("A", "A", "B", "B"))
predicted <- factor(c("A", "A", "B", "B"))
NMImetric(evidence, predicted)
#> [1] 1
```
