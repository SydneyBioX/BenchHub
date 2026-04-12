# Root Mean Squared Error (RMSE) Metric

Computes the root mean squared error of the predictions.

## Usage

``` r
RMSEmetric(evidence, predicted)
```

## Arguments

- evidence:

  The true values.

- predicted:

  The predicted values.

## Value

The root mean squared error.

## Examples

``` r
evidence <- c(1, 2, 3, 4)
predicted <- c(1.1, 2.1, 2.9, 4.2)
RMSEmetric(evidence, predicted)
#> [1] 0.1322876
```
