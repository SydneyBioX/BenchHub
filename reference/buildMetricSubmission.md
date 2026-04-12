# Build Metric submission rows from a Trio object

This helper creates one `Metric` row per metric in the Trio object. It
does not upload anything and does not modify the existing writeCTD()
path.

## Usage

``` r
buildMetricSubmission(trio)
```

## Arguments

- trio:

  A `Trio` object.

## Value

A `data.frame` matching the proposed `Metric` table schema.
