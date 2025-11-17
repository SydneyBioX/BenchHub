# A RecSys object

An object containing a recommender system for data-driven specific task.

## Value

A RecSys object.

## Public fields

- `evalSummary`:

  The evaluation summary is stored by dataframe, where each row is the
  compared identifier, each column is the metric used in the evaluation
  task and related information.

- `metadata`:

  A dataframe to store metadata for the benchmark.

## Methods

### Public methods

- [`RecSys$clone()`](#method-RecSys-clone)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RecSys$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
RecSys$new()
#> <RecSys>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     evalSummary: NULL
#>     metadata: NULL
```
