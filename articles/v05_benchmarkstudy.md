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

    ## Warning: Mapping function name has been modified to be a valid R variable name.
    ## ℹ Original name: Fraction Zero Genes
    ## ℹ Modified name: Fraction.Zero.Genes

Example 2: calculate the normalized library size per cell.

``` r
# Define the mapping function 
norm_lib_size <-  function(data) {
  dge <- edgeR::SE2DGEList(data)
  dge <- edgeR::normLibSizes(dge)
  return(edgeR::getNormLibSizes(dge))
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

# 
 
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

    ## R version 4.5.2 (2025-10-31)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
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
    ##  [1] scuttle_1.20.0              SingleCellExperiment_1.32.0
    ##  [3] SummarizedExperiment_1.40.0 Biobase_2.70.0             
    ##  [5] GenomicRanges_1.62.1        Seqinfo_1.0.0              
    ##  [7] IRanges_2.44.0              S4Vectors_0.48.0           
    ##  [9] BiocGenerics_0.56.0         generics_0.1.4             
    ## [11] MatrixGenerics_1.22.0       matrixStats_1.5.0          
    ## [13] R6_2.6.1                    BenchHub_0.99.9            
    ## [15] BiocStyle_2.38.0           
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] gridExtra_2.3          httr2_1.2.2            rlang_1.1.7           
    ##   [4] magrittr_2.0.4         compiler_4.5.2         survAUC_1.4-0         
    ##   [7] systemfonts_1.3.1      vctrs_0.6.5            reshape2_1.4.5        
    ##  [10] stringr_1.6.0          pkgconfig_2.0.3        fastmap_1.2.0         
    ##  [13] XVector_0.50.0         backports_1.5.0        utf8_1.2.6            
    ##  [16] ggstance_0.3.7         rmarkdown_2.30         pracma_2.4.6          
    ##  [19] ragg_1.5.0             purrr_1.2.1            xfun_0.55             
    ##  [22] beachmat_2.26.0        cachem_1.1.0           jsonlite_2.0.0        
    ##  [25] DelayedArray_0.36.0    BiocParallel_1.44.0    parallel_4.5.2        
    ##  [28] broom_1.0.11           cluster_2.1.8.1        bslib_0.9.0           
    ##  [31] stringi_1.8.7          RColorBrewer_1.1-3     limma_3.66.0          
    ##  [34] rpart_4.1.24           jquerylib_0.1.4        cellranger_1.1.0      
    ##  [37] Rcpp_1.1.1             bookdown_0.46          knitr_1.51            
    ##  [40] base64enc_0.1-3        parameters_0.28.3      Matrix_1.7-4          
    ##  [43] splines_4.5.2          nnet_7.3-20            tidyselect_1.2.1      
    ##  [46] abind_1.4-8            rstudioapi_0.17.1      yaml_2.3.12           
    ##  [49] codetools_0.2-20       curl_7.0.0             lattice_0.22-7        
    ##  [52] tibble_3.3.1           plyr_1.8.9             ks_1.15.1             
    ##  [55] withr_3.0.2            bayestestR_0.17.0      S7_0.2.1              
    ##  [58] evaluate_1.0.5         marginaleffects_0.31.0 foreign_0.8-90        
    ##  [61] desc_1.4.3             survival_3.8-3         mclust_6.1.2          
    ##  [64] pillar_1.11.1          BiocManager_1.30.27    KernSmooth_2.23-26    
    ##  [67] checkmate_2.3.3        insight_1.4.4          ggplot2_4.0.1         
    ##  [70] scales_1.4.0           glue_1.8.0             Hmisc_5.2-5           
    ##  [73] tools_4.5.2            data.table_1.18.0      locfit_1.5-9.12       
    ##  [76] mvtnorm_1.3-3          fs_1.6.6               grid_4.5.2            
    ##  [79] tidyr_1.3.2            datawizard_1.3.0       edgeR_4.8.2           
    ##  [82] colorspace_2.1-2       googlesheets4_1.1.2    patchwork_1.3.2       
    ##  [85] performance_0.15.3     htmlTable_2.4.3        googledrive_2.1.2     
    ##  [88] splitTools_1.0.1       Formula_1.2-5          cli_3.6.5             
    ##  [91] rappdirs_0.3.3         textshaping_1.0.4      S4Arrays_1.10.1       
    ##  [94] gargle_1.6.0           dplyr_1.1.4            gtable_0.3.6          
    ##  [97] ggcorrplot_0.1.4.1     ggsci_4.2.0            sass_0.4.10           
    ## [100] digest_0.6.39          SparseArray_1.10.8     ggrepel_0.9.6         
    ## [103] htmlwidgets_1.6.4      farver_2.1.2           htmltools_0.5.9       
    ## [106] pkgdown_2.2.0          lifecycle_1.0.5        httr_1.4.7            
    ## [109] statmod_1.5.1          dotwhisker_0.8.4
