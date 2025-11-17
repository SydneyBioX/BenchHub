# Brier Score Metric

Computes the Brier score for survival analysis.

## Usage

``` r
brierScoreMetric(evidence, predicted)
```

## Arguments

- evidence:

  The true survival times and event indicators.

- predicted:

  The predicted survival times.

## Value

The Brier score.

## Examples

``` r
# More realistic training dataset (8 patients)
evidence <- list(
  survival::Surv(time = c(5, 10, 15, 20, 25, 30, 35, 40), 
  event = c(1, 1, 0, 1, 0, 1, 1, 0)),  # Training
  survival::Surv(time = c(12, 18, 25, 32),
  event = c(1, 0, 1, 0))  # Testing
)
# Predicted risk scores
predicted <- list(
  c(0.5142118, 0.3902035, 0.9057381, 0.4469696, 
  0.8360043, 0.7375956, 0.8110551, 0.3881083),  # Training predictions
  c(0.685169729, 0.003948339, 0.832916080, 0.007334147)  # Testing predictions
)
brierScoreMetric(evidence, predicted)
#> [1] 0.01057958 0.04156804 0.17674010 0.16438778 0.24591007 0.15448003 0.03400678
#> [8] 0.03400678
```
