# A Trio object

An object containing a dataset and methods for evaluating analytical
tasks against ground truths for the dataset.

## Value

A `Trio` object

## Public fields

- `data`:

  The data

- `evidence`:

  The supporting evidence for the data

- `metrics`:

  The metric for evaluating tasks against the gold standards

- `cachePath`:

  The path to the data cache

- `dataSource`:

  The data repository that the data were retrieved from

- `dataSourceID`:

  The dataset ID for `dataSource`

- `evidenceSource`:

  The data repository that the supporting evidence was retrieved from.

- `evidenceSourceID`:

  The dataset ID for `evidenceSource`.

- `splitIndices`:

  Indices for cross-validation

- `splitSeed`:

  The seed used to generate the split indices

- `verbose`:

  Set the verbosity of Trio. Defaults to `FALSE`.

- `description`:

  A description of the dataset.

- `name`:

  The name of the Trio object, as defined in Curated Trio Datasets.

## Methods

### Public methods

- [`Trio$new()`](#method-Trio-new)

- [`Trio$addEvidence()`](#method-Trio-addEvidence)

- [`Trio$addMetric()`](#method-Trio-addMetric)

- [`Trio$getMetrics()`](#method-Trio-getMetrics)

- [`Trio$getEvidence()`](#method-Trio-getEvidence)

- [`Trio$evaluate()`](#method-Trio-evaluate)

- [`Trio$split()`](#method-Trio-split)

- [`Trio$print()`](#method-Trio-print)

- [`Trio$writeCTD()`](#method-Trio-writeCTD)

- [`Trio$clone()`](#method-Trio-clone)

------------------------------------------------------------------------

### Method `new()`

Create a Trio object

#### Usage

    Trio$new(
      datasetID = NULL,
      data = NULL,
      dataLoader = NULL,
      evidenceID = NULL,
      evidence = NULL,
      evidenceColumns = NULL,
      evidenceLoader = NULL,
      task = NULL,
      metrics = NULL,
      cachePath = FALSE,
      verbose = FALSE,
      description = NULL,
      name = NULL
    )

#### Arguments

- `datasetID`:

  A string specifying a dataset, either a name from curated-trio-data or
  a format string of the form `source`:`source_id`.

- `data`:

  An object to use as the Trio dataset.

- `dataLoader`:

  A custom loading function that takes the path of a downloaded file and
  returns a single dataset, ready to be used in evaluation tasks.

- `evidenceID`:

  If `datasetID` is not an ID from the curated trio datasets
  spreadsheet, then a format string of the form `source`:`source_id`
  indicating the file to obtain the supporting evidence from.

- `evidence`:

  A named list of lists. The top-level list is named by task type. The
  lower-level list is of length-two and named `"evidence"` and
  `"metrics"`. The `"evidence"` component has supporting evidence and
  the `"metrics"` component has a character vector of metric names
  (corresponding to the names of the list provided to the `metrics`
  parameter).

- `evidenceColumns`:

  If specified, extract supporting evidence from columns in the loaded
  dataset.

- `evidenceLoader`:

  Alternative to `evidence` and `evidenceColumns`. Extract the evidence
  in a flexible way.

- `task`:

  If `evidenceColumns` or `evidenceLoader` specified, a character vector
  of length 1 naming the task the evidence is for.

- `metrics`:

  A named list of metric functions.

- `cachePath`:

  The path to the data cache

- `verbose`:

  Set the verbosity of Trio. Defaults to `FALSE`.

- `description`:

  A description of the dataset.

- `name`:

  The name of the Trio object, as defined in Curated Trio Datasets.

------------------------------------------------------------------------

### Method `addEvidence()`

Add supporting evidence to the Trio.

#### Usage

    Trio$addEvidence(name, evidence, metrics, args = NULL)

#### Arguments

- `name`:

  A string specifying the name of the supporting evidence.

- `evidence`:

  The supporting evidence. An object to be compared or a function to be
  run on the data.

- `metrics`:

  A list of one or more metrics names used to compare gs with the input
  to evaluate.

- `args`:

  A named list of parameters and values to be passed to the function.

------------------------------------------------------------------------

### Method `addMetric()`

Add a metric to the Trio.

#### Usage

    Trio$addMetric(name, metric, args = NULL)

#### Arguments

- `name`:

  A string specifying the name of the metric.

- `metric`:

  The metric. A function to be run on the input to evaluate to compare
  it with the gold standard. Should be of the form f(x, y, ...). Where
  `x` is the "truth" and `y` is the output to be evaluated. Otherwise
  input a wrapper function of the desired metric.

- `args`:

  A named list of parameters and values to be passed to the function.

------------------------------------------------------------------------

### Method `getMetrics()`

Get metrics by supporting evidence name.

#### Usage

    Trio$getMetrics(evidenceName)

#### Arguments

- `evidenceName`:

  A string specifying the name of the supporting evidence.

------------------------------------------------------------------------

### Method `getEvidence()`

Get supporting evidence by name.

#### Usage

    Trio$getEvidence(name)

#### Arguments

- `name`:

  A string specifying the name of the supporting evidence.

------------------------------------------------------------------------

### Method `evaluate()`

Evaluate against gold standards

#### Usage

    Trio$evaluate(input)

#### Arguments

- `input`:

  A named list of objects to be evaluated against gold standards.

------------------------------------------------------------------------

### Method [`split()`](https://rdrr.io/r/base/split.html)

Create cross-validation indices.

#### Usage

    Trio$split(
      y,
      n_fold = 5L,
      n_repeat = 1L,
      stratify = TRUE,
      seed = 23624482,
      overwrite = FALSE,
      ...
    )

#### Arguments

- `y`:

  A variable to use for stratified sampling (e.g. supporting evidence).
  If `stratify` is false, a vector the length of the data.

- `n_fold`:

  Number of folds. Defaults to `5L`.

- `n_repeat`:

  Number of repeats. Defaults to `1L`.

- `stratify`:

  If `TRUE`, uses stratified sampling. Defaults to `TRUE`.

- `seed`:

  An `integer` of lenth 1. Defaults to 23624482, which is the text
  "BenchHub" in vanity number form.

- `overwrite`:

  If `TRUE`, overwrites the current split. Defaults to `FALSE`.

- `...`:

  Additional arguments passed to
  [`splitTools::create_folds`](https://rdrr.io/pkg/splitTools/man/create_folds.html).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method to display key information about the Trio object.

#### Usage

    Trio$print()

------------------------------------------------------------------------

### Method `writeCTD()`

Write the Trio Metadata to Curated Trio Datasets sheet.

#### Usage

    Trio$writeCTD(
      name,
      email = NULL,
      githubPat = NULL,
      description = NULL,
      figshareUrl = NULL,
      datasetFileName = NULL,
      evidenceFileName = NULL,
      dataType = NULL,
      skipMd5Check = FALSE
    )

#### Arguments

- `name`:

  The name of the dataset to be added.

- `email`:

  Required. Email address of the contributor for dataset update
  notifications.

- `githubPat`:

  Optional GitHub Personal Access Token. If not provided and not set in
  environment, will prompt user.

- `description`:

  Optional description of the dataset. If not provided and not set, will
  prompt user.

- `figshareUrl`:

  Optional URL to the Figshare dataset. If not provided, will prompt
  user.

- `datasetFileName`:

  Optional name of the dataset file in Figshare. If not provided, will
  prompt user for selection.

- `evidenceFileName`:

  Optional name of the evidence file in Figshare. If not provided, will
  prompt user for selection.

- `dataType`:

  Optional type of data. Must be one of: "omics", "clinical", "spatial",
  "other". If not provided, will prompt user.

- `skipMd5Check`:

  Optional boolean to skip MD5 verification. Defaults to FALSE.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Trio$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
trio <- Trio$new("figshare:26054188/47112109", cachePath = tempdir())
#> has no supporting evidence for this dataset.
#> ℹ Please add your own supporting evidence for evaluation.
```
