# Kernel Density Estimation (KDE) Metric

Computes the kernel density estimation test statistic.

## Usage

``` r
kdeMetric(evidence, predicted)
```

## Arguments

- evidence:

  The true values.

- predicted:

  The predicted values.

## Value

The KDE test statistic.

## Examples

``` r
evidence <- c(1, 2, 3, 4)
predicted <- c(1.1, 2.1, 2.9, 4.2)
kdeMetric(evidence, predicted)
#> [1] -9.594744
```
