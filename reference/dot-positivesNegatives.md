# Compute Positives and Negatives

Computes the true positives, false positives, false negatives, and true
negatives.

## Usage

``` r
.positivesNegatives(evidence, predicted)
```

## Arguments

- evidence:

  The true labels.

- predicted:

  The predicted labels.

## Value

A list containing the true positives, false positives, false negatives,
and true negatives.

## Examples

``` r
evidence <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
.positivesNegatives(evidence, predicted)
#> $TP
#> A B 
#> 2 1 
#> 
#> $FP
#> A B 
#> 1 0 
#> 
#> $FN
#> A B 
#> 0 1 
#> 
#> $TN
#> A B 
#> 1 2 
#> 
```
