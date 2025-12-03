# 4 BenchmarkStudy

``` r
library(BenchHub)
```

## Overview

The `BenchHubStudy` object is designed to encapsulate all necessary
components in a benchmarking study, including the data and functions
associated. It provides a unified structure for benchmark developers to
share their work and for method developers to interact with an existing
benchmark study.

- **Benchmark developers** can store *Trio* objects (containing the
  input data, metrics, and supporting evidence), any mapping functions
  and distribute a ready-to-use study object.  
- **Method developers** can apply their methods to the provided data and
  evaluate their outputs using the built-in metrics.

This vignette provides guide for both usage.

## For Benchmark Developer

This section demonstrates how to create a `BenchmarkStudy` object from a
benchmarking study.

### Initialising the Study

We begin by creating an empty `BenchmarkStudy` object.

``` r
study <- BenchmarkStudy$new()
```

### Adding Trio

Assume the benchmark developer has created a trio object for one of the
data used for the benchmarking. Here, we use a Trio from the curated
trio dataset.

``` r
# Download the Trio from the database using its name 
example_trio <- suppressMessages( Trio$new("benchhub_vignette_example", cachePath = TRUE) )

# Add to study
study$addTrio(example_trio)
```

### Define mapping function

A mapping function is a helper function that process the data into a
format that can then be input into the evaluation metrics.

For example, in simulation studies, the output from a simulator is a
cell x gene count matrix. Suppose we want to compare the sparsity of
genes between the simulated data and an experimental data, we need a
mapping function that calculates the sparsity of genes from the count
matrix.

We define two examples of mapping functions.

Example 1: calculate the sparsity (proportion of zero counts) per gene.

``` r
# Define the mapping function 
proportion_zero_gene <- function(data) {
  return(colMeans(counts(data) == 0))
}

# Add the mapping function
study$addMappingFunction(
  name = "Fraction Zero Genes",
  func = proportion_zero_gene,
  inputDescription = "SingleCellExperiment or matrix with gene expression counts.",
  outputDescription = "Numeric vector of fraction of zero counts per gene.",
  exampleUsage = paste(
    "## Minimal example",
    "#mat <- matrix(rpois(100, lambda = 1), nrow = 10, ncol = 10)",
    "#mat <- SingleCellExperiment(assays = list(counts = mat)",
    "#res <- study$runMapping('Fraction.Zero.Genes', mat)",
    "#head(res)",
    sep = "\n"
  )
)
```

    ## Warning: Mapping function name has been modified to be a valid R variable name.
    ## ℹ Original name: Fraction Zero Genes
    ## ℹ Modified name: Fraction.Zero.Genes

Example 2: calculate the normalized library size per cell.

``` r
# Define the mapping function 
norm_lib_size <- function(data) {
  lib_size <- log1p(rowSums(counts(data)))
  dge <- edgeR::DGEList(counts = Matrix::t(counts(data)))
  norm_factors <- edgeR::calcNormFactors(dge, method = "TMM")$samples$norm.factors
  return(unname(lib_size * norm_factors))
}

# Add the mapping function, it is optional but recommended to add example usage 
study$addMappingFunction(
  name = "normalized library size",
  func = norm_lib_size,
  inputDescription = "SingleCellExperiment or matrix with gene expression counts.",
  outputDescription = "Numeric vector of normalised library size per cell."
)
```

    ## Warning: Mapping function name has been modified to be a valid R variable name.
    ## ℹ Original name: normalized library size
    ## ℹ Modified name: normalized.library.size

### Uploading to Curated Trio Datasets

Once the BenchmarkStudy object includes: - A study name and
description  
- At least three Trio objects  
- Mapping functions \[optional\]

The BenchmarkStudy object can be uploaded to the database.

``` r
# because we need at least three Trio 
# pretend we have added three unique Trios 
study$addTrio(example_trio) 
study$addTrio(example_trio)

# Note that each Trio needs to be already exist in the database. 
# Trios can be uploaded to the database using the `writeCTD()` function. 
# For more details, see the vignette on Trio construction.
# example_trio$writeCTD("example_trio")

# Set name and description manually
study$name <- "Benchhubstudy vignette"
study$description <- "This study compares simulated spatial transcriptomics data."
```

The object can be uploaded using the `writeBenchmarkStudy()` function.

``` r
# We comment it out in the vignette so that it does not get repetitively added
# study$writeBenchmarkStudy()
```

## For Method Developer

This section illustrates how a method developer can use the benchmark
study object created by another user, apply their method, and evaluate
its performance.

### Loading the Study

A BenchmarkStudy object can be downloaded from the database through its
name.

``` r
study  <- suppressMessages( BenchmarkStudy$new("Benchhubstudy vignette", fetchFromCtd = TRUE))
```

Inspect the list of available trios, and available mapping functions

We see that this study has three Trios. Each Trio has supporting
evidence that we can compare with.

``` r
length(study$trios)
```

    ## [1] 3

``` r
study$trios[[1]]
```

    ## 
    ## ── Trio Object ─────────────────────────────────────────────────────────────────
    ## 
    ## ── Dataset 
    ## Dataset Details:
    ##   Formal class 'SingleCellExperiment' [package "SingleCellExperiment"] with 9
    ##   slots
    ## Data Source: "figshare"
    ## Dataset ID: "29565947/57477553"
    ## Cache Path: "/home/runner/.cache/R/TrioR"
    ## Split Indices: "None"
    ## 
    ## ── Supporting Evidence 
    ## Number of Supporting Evidence: 3
    ## Names of Supporting Evidence: "Fraction zero genes", "Fraction zero cells", and
    ## "normalized library size"
    ## 
    ## ── Metrics 
    ## Number of Metrics: 1
    ## Names of Metrics: "KS_stats"

This study provides two mapping function to process the data into a
format that can be used for evaluation.

Each mapping function has documentation.

``` r
study$listMappingFunctions()
```

    ## [1] "Fraction.Zero.Genes"     "normalized.library.size"

``` r
doc <- study$getMappingFunctionDocumentation("Fraction.Zero.Genes" )
 
doc 
```

    ## $inputDescription
    ## [1] "SingleCellExperiment or matrix with gene expression counts."
    ## 
    ## $outputDescription
    ## [1] "Numeric vector of fraction of zero counts per gene."
    ## 
    ## $exampleUsage
    ## [1] "## Minimal example\nmat <- matrix(rpois(100, lambda = 1), nrow = 10, ncol = 10)\nmat <- SingleCellExperiment(assays = list(counts = mat)\nres <- study$runMapping('Fraction.Zero.Genes', mat)\nhead(res)"

``` r
# Because the example Usage is a multiple line vector, need to use cat to print it nicely.
cat(doc$exampleUsage, "\n")
```

    ## ## Minimal example
    ## mat <- matrix(rpois(100, lambda = 1), nrow = 10, ncol = 10)
    ## mat <- SingleCellExperiment(assays = list(counts = mat)
    ## res <- study$runMapping('Fraction.Zero.Genes', mat)
    ## head(res)

### Preparing for evaluation

This benchmark study wants to assess the quality of simulated data.

Suppose the method developer has generated a simulated data with their
method, and they want to compare with the real data to see how realistic
the simulated data is.

``` r
# Suppose this is a simulated data 
set.seed(0)
sim <- mockSCE(ncells =  4744, ngenes = 1000)
```

The method developer can apply the mapping functions to the simulated
data to generate the relevant features to evaluate such as the sparsity
of the genes and the library size.

``` r
mydata_prop_zero_gene <- study$runMapping("Fraction.Zero.Genes", sim)
mydata_lib_size <- study$runMapping( "normalized.library.size", sim)
```

### Evaluate

Now we can compare the simulated data against an experimental data using
the `evaluate` function.

The evaluate function is in the format of
`study$evaluate(trio_name, list( supporting evidence = output to compare with ))`.

In the function below, the `Fraction zero genes` and
`normalized library size` are the names of the supporting evidence that
can be found in the `example_trio_1` Trio object.

``` r
result <- study$evaluate("benchhub_vignette_example",  # name of the trio to compare with  , can be accessed by study$trios[[1]]$name
  list("Fraction zero genes" = mydata_prop_zero_gene  , # name of the supporting evidence 
   "normalized library size" = mydata_prop_zero_gene )) # name of the supporting evidence   

# 
 
result
```

    ## # A tibble: 2 × 4
    ##   datasetID         evidence                metric   result
    ##   <chr>             <chr>                   <chr>     <dbl>
    ## 1 29565947/57477553 Fraction zero genes     KS_stats  268. 
    ## 2 29565947/57477553 normalized library size KS_stats   98.1
