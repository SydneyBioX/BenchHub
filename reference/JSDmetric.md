# Jensen-Shannon Divergence (JSD) Metric

Computes the Jensen-Shannon divergence between two non-negative numeric
vectors after normalizing them to probability distributions.

## Usage

``` r
JSDmetric(evidence, predicted)
```

## Arguments

- evidence:

  The true values.

- predicted:

  The predicted values.

## Value

The Jensen-Shannon divergence.

## Examples

``` r
evidence <- c(0.2, 0.3, 0.5)
predicted <- c(0.1, 0.4, 0.5)
JSDmetric(evidence, predicted)
#> [1] 0.01742578
```
