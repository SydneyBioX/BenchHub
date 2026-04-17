# 5 BenchmarkStudy

``` r
library(BenchHub)
```

## Overview

The `BenchmarkStudy` object is designed to encapsulate all necessary
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

For example, in single-cell RNA-sequencing simulation studies, the
output from a simulator is often a cell by gene count matrix. When
benchmarking, we often map the matrix output to derived statistics
(e.g., gene-level sparsity or cell-level library sizes) that can be
compared against a reference (e.g., real single-cell RNA-seq matrix).

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

Example 2: calculate the normalized library size per cell.

``` r
library(edgeR)
# Define the mapping function 
norm_lib_size <-  function(data) {
  dge <- SE2DGEList(data)
  dge <- normLibSizes(dge)
  return(getNormLibSizes(dge))
}

# Add the mapping function, it is optional but recommended to add example usage 
study$addMappingFunction(
  name = "normalized library size",
  func = norm_lib_size,
  inputDescription = "SingleCellExperiment or matrix with gene expression counts.",
  outputDescription = "Numeric vector of normalised library size per cell."
)
```

### Uploading to Curated Trio Datasets

Once the BenchmarkStudy object includes:  
- A study name and description  
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
    ## Cache Path: "/home/runner/.cache/R/BenchHub"
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
# list the names of the mapping function
study$listMappingFunctions()
```

    ## [1] "Fraction.Zero.Genes"     "normalized.library.size"

``` r
# choose one to print the documentation 
study$printMappingFunctionDocumentation("Fraction.Zero.Genes")
```

    ## Input:
    ##   SingleCellExperiment or matrix with gene expression counts.
    ## Output:
    ##   Numeric vector of fraction of zero counts per gene.
    ## Example:
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
# Use scuttle to simulate a data 
set.seed(0)
sim <- scuttle::mockSCE(ncells =  4744, ngenes = 1000)
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

result
```

    ## # A tibble: 2 × 4
    ##   datasetID         evidence                metric   result
    ##   <chr>             <chr>                   <chr>     <dbl>
    ## 1 29565947/57477553 Fraction zero genes     KS_stats  268. 
    ## 2 29565947/57477553 normalized library size KS_stats   98.1

## Summary

This vignette demonstrated two ways that users can interact with the
BenchmarkStudy framework:

- Benchmark developers: create or update a `BenchmarkStudy` by adding
  `Trio` objects and optional mapping functions with clear
  documentation, then upload the study to the Trio Database.

- Method developers: load an existing `BenchmarkStudy` from the Trio
  Database, use the `Trio` objects to execute benchmarking methods of
  interest, use the mapping functions to convert method outputs where
  needed, evaluate those outputs against the study’s supporting evidence
  using the `evaluate()` function.

## Session Info

``` r
sessionInfo()
```

    ## R version 4.5.3 (2026-03-11)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats4    stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] edgeR_4.8.2                 limma_3.66.0               
    ##  [3] BenchHub_0.99.10            scuttle_1.20.0             
    ##  [5] SingleCellExperiment_1.32.0 SummarizedExperiment_1.40.0
    ##  [7] Biobase_2.70.0              GenomicRanges_1.62.1       
    ##  [9] Seqinfo_1.0.0               IRanges_2.44.0             
    ## [11] S4Vectors_0.48.1            BiocGenerics_0.56.0        
    ## [13] generics_0.1.4              MatrixGenerics_1.22.0      
    ## [15] matrixStats_1.5.0           R6_2.6.1                   
    ## [17] BiocStyle_2.38.0           
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] httr2_1.2.2            gridExtra_2.3          rlang_1.2.0           
    ##   [4] magrittr_2.0.5         compiler_4.5.3         survAUC_1.4-0         
    ##   [7] reshape2_1.4.5         systemfonts_1.3.2      vctrs_0.7.3           
    ##  [10] stringr_1.6.0          pkgconfig_2.0.3        fastmap_1.2.0         
    ##  [13] backports_1.5.1        XVector_0.50.0         utf8_1.2.6            
    ##  [16] ggstance_0.3.7         rmarkdown_2.31         pracma_2.4.6          
    ##  [19] ragg_1.5.2             purrr_1.2.2            xfun_0.57             
    ##  [22] cachem_1.1.0           beachmat_2.26.0        jsonlite_2.0.0        
    ##  [25] DelayedArray_0.36.1    BiocParallel_1.44.0    broom_1.0.12          
    ##  [28] parallel_4.5.3         cluster_2.1.8.2        bslib_0.10.0          
    ##  [31] stringi_1.8.7          RColorBrewer_1.1-3     rpart_4.1.24          
    ##  [34] jquerylib_0.1.4        cellranger_1.1.0       Rcpp_1.1.1            
    ##  [37] bookdown_0.46          knitr_1.51             base64enc_0.1-6       
    ##  [40] parameters_0.28.3      splines_4.5.3          Matrix_1.7-4          
    ##  [43] nnet_7.3-20            tidyselect_1.2.1       rstudioapi_0.18.0     
    ##  [46] abind_1.4-8            yaml_2.3.12            codetools_0.2-20      
    ##  [49] curl_7.0.0             plyr_1.8.9             lattice_0.22-9        
    ##  [52] tibble_3.3.1           ks_1.15.1              withr_3.0.2           
    ##  [55] bayestestR_0.17.0      S7_0.2.1               evaluate_1.0.5        
    ##  [58] marginaleffects_0.32.0 foreign_0.8-91         survival_3.8-6        
    ##  [61] desc_1.4.3             mclust_6.1.2           pillar_1.11.1         
    ##  [64] BiocManager_1.30.27    KernSmooth_2.23-26     checkmate_2.3.4       
    ##  [67] insight_1.5.0          ggplot2_4.0.2          scales_1.4.0          
    ##  [70] glue_1.8.0             Hmisc_5.2-5            tools_4.5.3           
    ##  [73] data.table_1.18.2.1    locfit_1.5-9.12        mvtnorm_1.3-6         
    ##  [76] fs_2.0.1               grid_4.5.3             tidyr_1.3.2           
    ##  [79] datawizard_1.3.0       colorspace_2.1-2       googlesheets4_1.1.2   
    ##  [82] patchwork_1.3.2        performance_0.16.0     htmlTable_2.4.3       
    ##  [85] googledrive_2.1.2      splitTools_1.0.1       Formula_1.2-5         
    ##  [88] cli_3.6.6              rappdirs_0.3.4         textshaping_1.0.5     
    ##  [91] gargle_1.6.1           S4Arrays_1.10.1        dplyr_1.2.1           
    ##  [94] gtable_0.3.6           ggcorrplot_0.1.4.1     ggsci_4.3.0           
    ##  [97] sass_0.4.10            digest_0.6.39          SparseArray_1.10.10   
    ## [100] ggrepel_0.9.8          htmlwidgets_1.6.4      farver_2.1.2          
    ## [103] htmltools_0.5.9        pkgdown_2.2.0          lifecycle_1.0.5       
    ## [106] httr_1.4.8             statmod_1.5.1          dotwhisker_0.8.4
