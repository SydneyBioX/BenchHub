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

This vignette provides a guide for both use cases under the current
BenchHub submission workflow.

## For Benchmark Developer

This section demonstrates how to create a `BenchmarkStudy` object from a
benchmarking study.

### Initialising the Study

We begin by creating an empty `BenchmarkStudy` object.

``` r
study <- BenchmarkStudy$new()
```

``` r
# Download an existing Trio from the submission database
example_trio <- downloadSubmissionTrio("D001", cachePath = tempdir())

example_trio
```

### Define mapping function and protocol function

A mapping function is a helper function that processes method output
into a format that can then be compared with the supporting evidence
stored in the reference `Trio`. There are three ways to contribute the
mapping function:

1.  Leave blank: if you don’t want to contribute now
2.  Use existing GitHub repository: if your mapping function has been
    uploaded to the GitHub repository in the published paper
3.  Upload mapping functions stored in the study object
4.  Upload local mapping function to gist: define the mapping function
    locally

In this toy spatial transcriptomics example, the `Trio` contains the
following supporting evidence:

- `annotated_domain`
- `celltype_proportions`

We therefore define two mapping functions that extract those objects
from a method result.

Example 1: extract predicted spatial domains.

``` r
# Define the mapping function 
extract_domains <- function(result) {
  if (is.data.frame(result) && "annotated_domain" %in% colnames(result)) {
    return(result$annotated_domain)
  }
  if (is.list(result) && "annotated_domain" %in% names(result)) {
    return(result$annotated_domain)
  }
  stop("Could not find 'annotated_domain' in the method output.")
}

# Add the mapping function
study$addMappingFunction(
  name = "annotated_domain",
  func = extract_domains,
  inputDescription = "Method output containing one predicted spatial domain label per spot.",
  outputDescription = "A vector of predicted spatial domain labels aligned to spots.",
  exampleUsage = paste(
    "## Minimal example",
    "#result <- list(annotated_domain = c('domain_1', 'domain_1', 'domain_2', 'domain_2'))",
    "#res <- study$runMapping('annotated_domain', result)",
    "#head(res)",
    sep = "\n"
  )
)
```

Example 2: extract predicted cell type proportions.

``` r
# Define the mapping function 
extract_celltype_props <- function(result) {
  if (is.data.frame(result) && "celltype_proportions" %in% names(result)) {
    return(result$celltype_proportions)
  }
  if (is.list(result) && "celltype_proportions" %in% names(result)) {
    return(result$celltype_proportions)
  }
  if (is.matrix(result) || is.data.frame(result)) {
    mat <- as.matrix(result)
    rs <- rowSums(mat)
    rs[rs == 0] <- 1
    return(mat / rs)
  }
  stop("Could not extract cell type proportions from the method output.")
}

# Add the mapping function, it is optional but recommended to add example usage 
study$addMappingFunction(
  name = "celltype_proportions",
  func = extract_celltype_props,
  inputDescription = "Method output containing cell type proportions per spot.",
  outputDescription = "A matrix or data frame of cell type proportions aligned to spots.",
  exampleUsage = paste(
    "## Minimal example",
    "#props <- matrix(c(0.9, 0.1, 0.8, 0.2, 0.2, 0.8, 0.1, 0.9), ncol = 2, byrow = TRUE)",
    "#study$runMapping('celltype_proportions', props)",
    sep = "\n"
  )
)
```

Similar as mapping functions, the protocol function is the full workflow
of benchmarking study. There are three ways to contribute the protocol
function:

1.  Leave blank: if you don’t want to contribute now
2.  Use existing protocol gist URL: if your protocol function has been
    uploaded to the GitHub repository in the published paper
3.  Upload local protocol file to gist

### Upload Study

Once the BenchmarkStudy object includes:

1.  A study name and description
2.  One or more Trio objects already represented in the submission
    database
3.  Mapping functions \[optional\]
4.  Protocol functions \[optional\]

the recommended next step is to an interactive console workflow via
`interactivePrepareStudySubmission(study)`.

``` r
# Set name and description manually
study <- BenchmarkStudy$new(name = "ST toy study")
study$description <- "Toy spatial transcriptomics study."

interactivePrepareStudySubmission(study)
```

In that interactive workflow, BenchHub will guide you through:

- selecting or confirming dataset IDs to link to the study,
- entering the study description,
- optionally providing a protocol gist,
- optionally providing a mapping-functions gist or uploading mapping
  functions already stored in the `study` object,
- reviewing the submission bundle, and
- optionally submitting the Study immediately.

## For Method Developer

This section illustrates how a method developer can use the benchmark
study object created by another user, apply their method, and evaluate
its performance.

### Loading the Study

A `BenchmarkStudy` object can be downloaded from the submission database
through its `studyID`.

``` r
loaded_study <- downloadSubmissionStudy(studyID = "ST005", cachePath = tempdir())
```

This returns a populated `BenchmarkStudy` object. For example:

``` r
loaded_study
loaded_study$name
loaded_study$description
loaded_study$version
length(loaded_study$trios)
```

Inspect the list of available trios, and available mapping functions

Each entry of `loaded_study$trios` is a `Trio` object with supporting
evidence that can be used for evaluation.

``` r
length(loaded_study$trios)

loaded_study$trios[[1]]
```

This study provides mapping functions to process method outputs into a
format that can be used for evaluation.

Each mapping function has documentation.

``` r
# list the names of the mapping function
loaded_study$listMappingFunctions()

# choose one to print the documentation 
loaded_study$printMappingFunctionDocumentation("annotated_domain")
```

### Preparing for evaluation

This benchmark study aims to assess predicted spatial domains and cell
type proportions.

Suppose the method developer has run a method and obtained predicted
domain labels and cell type proportions for each spot.

``` r
method_output <- list(
  annotated_domain = c("domain_1", "domain_1", "domain_2", "domain_2"),
  celltype_proportions = data.frame(
    celltype_A = c(0.9, 0.8, 0.2, 0.1),
    celltype_B = c(0.1, 0.2, 0.8, 0.9)
  )
)
```

The method developer can apply the mapping functions to the method
output to generate the objects required for evaluation.

``` r
domain_pred <- loaded_study$runMapping("annotated_domain", method_output)
prop_pred <- loaded_study$runMapping("celltype_proportions", method_output)
```

### Evaluate

Now we can compare the simulated data against an experimental dataset
using the `evaluate` function.

The evaluate function is in the format of
`study$evaluate(trio_name, list(supporting evidence = output to compare with))`.

In the function below, the names in the list correspond to supporting
evidence stored in the reference `Trio`.

``` r
result <- loaded_study$evaluate(loaded_study$trios[[1]]$name,  # name of the Trio to compare with
  list(
    "annotated_domain" = domain_pred,
    "celltype_proportions" = prop_pred
  ))

result
```

## Summary

This vignette demonstrated two ways that users can interact with the
`BenchmarkStudy` framework:

- Benchmark developers: create or update a `BenchmarkStudy` by adding
  `Trio` objects and optional mapping functions with clear
  documentation, then prepare and submit the study through the current
  Study submission workflow.

- Method developers: load an existing `BenchmarkStudy` from the
  submission database, use the `Trio` objects to execute benchmarking
  methods of interest, use the mapping functions to convert method
  outputs where needed, and evaluate those outputs against the study’s
  supporting evidence using the `evaluate()` function.

## Session Info

``` r
sessionInfo()
```

    ## R version 4.6.0 (2026-04-24)
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
    ##  [1] BenchHub_0.99.15            scuttle_1.21.6             
    ##  [3] SingleCellExperiment_1.33.2 SummarizedExperiment_1.41.1
    ##  [5] Biobase_2.71.0              GenomicRanges_1.63.2       
    ##  [7] Seqinfo_1.1.0               IRanges_2.45.0             
    ##  [9] S4Vectors_0.49.3            BiocGenerics_0.57.1        
    ## [11] generics_0.1.4              MatrixGenerics_1.23.0      
    ## [13] matrixStats_1.5.0           R6_2.6.1                   
    ## [15] BiocStyle_2.39.0           
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr2_1.2.2            gridExtra_2.3          rlang_1.2.0           
    ##  [4] magrittr_2.0.5         compiler_4.6.0         survAUC_1.4-0         
    ##  [7] reshape2_1.4.5         systemfonts_1.3.2      vctrs_0.7.3           
    ## [10] stringr_1.6.0          pkgconfig_2.0.3        fastmap_1.2.0         
    ## [13] backports_1.5.1        XVector_0.51.0         ggstance_0.3.7        
    ## [16] rmarkdown_2.31         ragg_1.5.2             purrr_1.2.2           
    ## [19] xfun_0.57              cachem_1.1.0           beachmat_2.27.5       
    ## [22] jsonlite_2.0.0         DelayedArray_0.37.1    BiocParallel_1.45.0   
    ## [25] broom_1.0.12           parallel_4.6.0         cluster_2.1.8.2       
    ## [28] bslib_0.10.0           stringi_1.8.7          RColorBrewer_1.1-3    
    ## [31] rpart_4.1.27           jquerylib_0.1.4        cellranger_1.1.0      
    ## [34] Rcpp_1.1.1-1.1         bookdown_0.46          knitr_1.51            
    ## [37] base64enc_0.1-6        parameters_0.28.3      splines_4.6.0         
    ## [40] Matrix_1.7-5           nnet_7.3-20            tidyselect_1.2.1      
    ## [43] rstudioapi_0.18.0      abind_1.4-8            yaml_2.3.12           
    ## [46] codetools_0.2-20       curl_7.1.0             plyr_1.8.9            
    ## [49] lattice_0.22-9         tibble_3.3.1           withr_3.0.2           
    ## [52] bayestestR_0.17.0      S7_0.2.2               evaluate_1.0.5        
    ## [55] marginaleffects_0.32.0 foreign_0.8-91         survival_3.8-6        
    ## [58] desc_1.4.3             pillar_1.11.1          BiocManager_1.30.27   
    ## [61] checkmate_2.3.4        insight_1.5.0          ggplot2_4.0.3         
    ## [64] scales_1.4.0           glue_1.8.1             Hmisc_5.2-5           
    ## [67] tools_4.6.0            data.table_1.18.2.1    fs_2.1.0              
    ## [70] grid_4.6.0             tidyr_1.3.2            datawizard_1.3.1      
    ## [73] colorspace_2.1-2       googlesheets4_1.1.2    patchwork_1.3.2       
    ## [76] performance_0.16.0     htmlTable_2.5.0        googledrive_2.1.2     
    ## [79] splitTools_1.0.1       Formula_1.2-5          cli_3.6.6             
    ## [82] rappdirs_0.3.4         textshaping_1.0.5      gargle_1.6.1          
    ## [85] S4Arrays_1.11.1        dplyr_1.2.1            gtable_0.3.6          
    ## [88] ggcorrplot_0.1.4.1     ggsci_5.0.0            sass_0.4.10           
    ## [91] digest_0.6.39          SparseArray_1.11.13    ggrepel_0.9.8         
    ## [94] htmlwidgets_1.6.4      farver_2.1.2           htmltools_0.5.9       
    ## [97] pkgdown_2.2.0          lifecycle_1.0.5        dotwhisker_0.8.4
