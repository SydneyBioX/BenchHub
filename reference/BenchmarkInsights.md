# A BenchmarkInsights object

An object containing a benchmark result for evaluating analytical tasks.

## Value

A BenchmarkInsights object.

## Public fields

- `evalSummary`:

  The evaluation summary is stored by dataframe, where each row is the
  methodd identifier, each column is the metric used in the evaluation
  task and related information.

- `metadata`:

  A dataframe to store metadata for the benchmark.

## Methods

### Public methods

- [`BenchmarkInsights$new()`](#method-BenchmarkInsights-new)

- [`BenchmarkInsights$addevalSummary()`](#method-BenchmarkInsights-addevalSummary)

- [`BenchmarkInsights$addMetadata()`](#method-BenchmarkInsights-addMetadata)

- [`BenchmarkInsights$getHeatmap()`](#method-BenchmarkInsights-getHeatmap)

- [`BenchmarkInsights$getLineplot()`](#method-BenchmarkInsights-getLineplot)

- [`BenchmarkInsights$getScatterplot()`](#method-BenchmarkInsights-getScatterplot)

- [`BenchmarkInsights$getBoxplot()`](#method-BenchmarkInsights-getBoxplot)

- [`BenchmarkInsights$getCorplot()`](#method-BenchmarkInsights-getCorplot)

- [`BenchmarkInsights$getForestplot()`](#method-BenchmarkInsights-getForestplot)

- [`BenchmarkInsights$clone()`](#method-BenchmarkInsights-clone)

------------------------------------------------------------------------

### Method `new()`

Create a BenchmarkInsights object

#### Usage

    BenchmarkInsights$new(evalResult = NULL)

#### Arguments

- `evalResult`:

  A dataframe containing initial evaluation results with columns such as
  datasetID, evidence, metric, and result.

------------------------------------------------------------------------

### Method `addevalSummary()`

Add additional evaluation summary to the existing evalSummary

#### Usage

    BenchmarkInsights$addevalSummary(additional_evalResult)

#### Arguments

- `additional_evalResult`:

  A dataframe containing additional evaluation results to be appended.

------------------------------------------------------------------------

### Method `addMetadata()`

Add metadata to the BenchmarkInsights object

#### Usage

    BenchmarkInsights$addMetadata(metadata)

#### Arguments

- `metadata`:

  A dataframe containing metadata information.

------------------------------------------------------------------------

### Method `getHeatmap()`

Creates a heatmap from the evaluation summary by averaging results
across datasets.

#### Usage

    BenchmarkInsights$getHeatmap(evalSummary)

#### Arguments

- `evalSummary`:

  A dataframe containing the evaluation summary.

#### Returns

A heatmap object.

------------------------------------------------------------------------

### Method `getLineplot()`

Creates a line plot for the given x and y variables, with an optional
grouping and fixed x order.

#### Usage

    BenchmarkInsights$getLineplot(evalResult, order = NULL, metricVariable)

#### Arguments

- `evalResult`:

  subset of evaluation summary.

- `order`:

  An optional vector specifying the order of x-axis values.

- `metricVariable`:

  Specify subset value in metric column.

#### Returns

A ggplot2 line plot object.

------------------------------------------------------------------------

### Method `getScatterplot()`

Creates a scatter plot for the same evidence, with an two methodd
metrics.

#### Usage

    BenchmarkInsights$getScatterplot(evalResult, variables)

#### Arguments

- `evalResult`:

  subset of evaluation summary, only include two different metrics, all
  evidence should be same

- `variables`:

  A character vector of length two specifying the metric names to be
  used for the x and y axes.

#### Returns

A ggplot2 line plot object.

------------------------------------------------------------------------

### Method `getBoxplot()`

Creates boxplot plots for the mutiple evidence, different method, one
metric.

#### Usage

    BenchmarkInsights$getBoxplot(evalResult, metricVariable, evidenceVariable)

#### Arguments

- `evalResult`:

  subset of evaluation summary, only include two different metrics, all
  evidence should be same.

- `metricVariable`:

  Specify subset value in metric column.

- `evidenceVariable`:

  Specify subset value in evidence column.

#### Returns

A ggplot2 line plot object.

------------------------------------------------------------------------

### Method `getCorplot()`

Creates a correlation plot based on the provided evaluation summary and
the specified input type (either "evidence", "metric", or "method"). The
correlation plot shows the pairwise correlation between results for
different categories (evidence, metric, or method).

#### Usage

    BenchmarkInsights$getCorplot(evalResult, input_type)

#### Arguments

- `evalResult`:

  A subset of the evaluation summary. It must include columns relevant
  to the input type (evidence, metric, method) and the result values.

- `input_type`:

  A string that specifies the input type for generating the correlation
  plot. It must be either "evidence", "metric", or "method".

#### Returns

A ggplot2 correlation plot object. The plot visualizes the correlation
matrix using ggcorrplot with aesthetic enhancements like labeled values
and angled axis text.

------------------------------------------------------------------------

### Method `getForestplot()`

This function generates a forest plot using linear models based on the
comparison between groups in the provided evaluation summary. The plot
is created using dotwhisker and broom packages, with custom grouping and
labeling.

#### Usage

    BenchmarkInsights$getForestplot(evalResult, input_group, input_model)

#### Arguments

- `evalResult`:

  A data frame containing the evaluation summary.

- `input_group`:

  A string specifying the grouping variable (only "datasetID", "method",
  or "evidence" allowed).

- `input_model`:

  A string specifying the model variable (only "datasetID", "method", or
  "evidence" allowed).

#### Returns

A forest plot showing the comparison of models across groups.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BenchmarkInsights$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
BenchmarkInsights$new()
#> <BenchmarkInsights>
#>   Public:
#>     addMetadata: function (metadata) 
#>     addevalSummary: function (additional_evalResult) 
#>     clone: function (deep = FALSE) 
#>     evalSummary: data.frame
#>     getBoxplot: function (evalResult = NULL, metricVariable, evidenceVariable) 
#>     getCorplot: function (evalResult = NULL, input_type) 
#>     getForestplot: function (evalResult = NULL, input_group, input_model) 
#>     getHeatmap: function (evalSummary = NULL) 
#>     getLineplot: function (evalResult = NULL, order = NULL, metricVariable) 
#>     getScatterplot: function (evalResult = NULL, variables) 
#>     initialize: function (evalResult = NULL) 
#>     metadata: NULL
```
