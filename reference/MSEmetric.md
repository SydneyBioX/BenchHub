# Mean Squared Error (MSE) Metric

Computes the mean squared error of the predictions.

## Usage

``` r
MSEmetric(evidence, predicted)
```

## Arguments

- evidence:

  The true values.

- predicted:

  The predicted values.

## Value

The mean squared error.

## Examples

``` r
evidence <- c(1, 2, 3, 4)
predicted <- c(1.1, 2.1, 2.9, 4.2)
MSEmetric(evidence, predicted)
#> [1] 0.0175
```
