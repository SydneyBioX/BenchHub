# BenchmarkStudy Class

This class manages a collection of benchmark trios and mapping
functions. It allows adding new trios, mapping functions, and running
mappings on data.

## Value

A `BenchmarkStudy` object.

## Public fields

- `name`:

  A character string to name the study.

- `trios`:

  A list to store benchmark trios.

- `mappingFunctions`:

  A list to store mapping functions with metadata.

- `description`:

  A character string describing the study.

- `version`:

  Integer specifying the version of the study.

## Methods

### Public methods

- [`BenchmarkStudy$new()`](#method-BenchmarkStudy-new)

- [`BenchmarkStudy$addTrio()`](#method-BenchmarkStudy-addTrio)

- [`BenchmarkStudy$addMappingFunction()`](#method-BenchmarkStudy-addMappingFunction)

- [`BenchmarkStudy$runMapping()`](#method-BenchmarkStudy-runMapping)

- [`BenchmarkStudy$getMappingFunctionDocumentation()`](#method-BenchmarkStudy-getMappingFunctionDocumentation)

- [`BenchmarkStudy$printMappingFunctionDocumentation()`](#method-BenchmarkStudy-printMappingFunctionDocumentation)

- [`BenchmarkStudy$listMappingFunctions()`](#method-BenchmarkStudy-listMappingFunctions)

- [`BenchmarkStudy$generateVignetteTemplate()`](#method-BenchmarkStudy-generateVignetteTemplate)

- [`BenchmarkStudy$evaluate()`](#method-BenchmarkStudy-evaluate)

- [`BenchmarkStudy$writeBenchmarkStudy()`](#method-BenchmarkStudy-writeBenchmarkStudy)

- [`BenchmarkStudy$print()`](#method-BenchmarkStudy-print)

- [`BenchmarkStudy$clone()`](#method-BenchmarkStudy-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    BenchmarkStudy$new(
      name = NULL,
      trios = list(),
      fetchFromCtd = FALSE,
      version = NULL
    )

#### Arguments

- `name`:

  A character string to name the study. If fetchFromCtd is TRUE, this
  name will be used to fetch the study from the submission database.

- `trios`:

  A list of Trio objects to initialize the study.

- `fetchFromCtd`:

  Logical indicating whether to fetch study details from the submission
  database.

- `version`:

  Optional integer specifying which version of the study to fetch (when
  fetchFromCtd is TRUE).

------------------------------------------------------------------------

### Method `addTrio()`

Add a new trio to the study

#### Usage

    BenchmarkStudy$addTrio(trioObject)

#### Arguments

- `trioObject`:

  A Trio object to be added.

------------------------------------------------------------------------

### Method `addMappingFunction()`

Add a mapping function with metadata

#### Usage

    BenchmarkStudy$addMappingFunction(
      name,
      func,
      inputDescription,
      outputDescription,
      exampleUsage = NULL
    )

#### Arguments

- `name`:

  A character string to name the mapping function.

- `func`:

  A function that takes data as input and returns transformed data.

- `inputDescription`:

  A character string describing the input data.

- `outputDescription`:

  A character string describing the output data.

- `exampleUsage`:

  An optional character string showing example usage of the function.

------------------------------------------------------------------------

### Method `runMapping()`

Apply a mapping function to data

#### Usage

    BenchmarkStudy$runMapping(mappingName, data)

#### Arguments

- `mappingName`:

  A character string naming the mapping function to apply.

- `data`:

  The data to which the mapping function will be applied.

#### Returns

The transformed data after applying the mapping function.

------------------------------------------------------------------------

### Method `getMappingFunctionDocumentation()`

Documentation getter for mapping function

#### Usage

    BenchmarkStudy$getMappingFunctionDocumentation(mappingName)

#### Arguments

- `mappingName`:

  A character string naming the mapping function.

#### Returns

A list containing the input description, output description, and example
usage.

------------------------------------------------------------------------

### Method `printMappingFunctionDocumentation()`

Print the documentation for a mapping function in a human-readable
format.

#### Usage

    BenchmarkStudy$printMappingFunctionDocumentation(mappingName)

#### Arguments

- `mappingName`:

  A character string naming the mapping function.

#### Returns

Prints inputDescription, outputDescription, exampleUsage

------------------------------------------------------------------------

### Method `listMappingFunctions()`

available options

#### Usage

    BenchmarkStudy$listMappingFunctions()

#### Returns

A character vector of mapping function names.

------------------------------------------------------------------------

### Method `generateVignetteTemplate()`

Generate R Markdown vignette template

#### Usage

    BenchmarkStudy$generateVignetteTemplate(
      outputPath = "benchmark_study_template.Rmd"
    )

#### Arguments

- `outputPath`:

  A character string specifying the path to save the vignette template.

------------------------------------------------------------------------

### Method `evaluate()`

Evaluate a trio with input data

#### Usage

    BenchmarkStudy$evaluate(trioName, input)

#### Arguments

- `trioName`:

  A character string naming the trio to evaluate.

- `input`:

  The input data to evaluate the trio against.

#### Returns

The evaluation result from the trio.

------------------------------------------------------------------------

### Method `writeBenchmarkStudy()`

Write the BenchmarkStudy metadata to Curated Trio Datasets sheet.

#### Usage

    BenchmarkStudy$writeBenchmarkStudy()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method to display key information about the BenchmarkStudy object.

#### Usage

    BenchmarkStudy$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BenchmarkStudy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
