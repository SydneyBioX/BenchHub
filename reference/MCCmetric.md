# Matthews Correlation Coefficient (MCC) Metric

Computes the Matthews Correlation Coefficient (MCC) of the predictions.

## Usage

``` r
MCCmetric(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

The MCC.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
MCCmetric(evidence, predicted)
#>         B 
#> 0.5773503 
```
