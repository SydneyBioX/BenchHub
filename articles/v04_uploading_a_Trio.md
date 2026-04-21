# 4 Preparing and Submitting a Trio

## Preparing and Submitting a Trio

This guide provides step-by-step instructions for preparing and
submitting a `Trio` object to the [BenchHub
Datasets](https://docs.google.com/spreadsheets/d/1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg/edit?usp=sharing).
The recommended workflow is to first build a submission bundle with
[`writeSubmission()`](https://sydneybiox.github.io/BenchHub/reference/writeSubmission.md),
review the collected metadata, and then submit it.

The workflow has two common entry points:

- [`writeSubmission()`](https://sydneybiox.github.io/BenchHub/reference/writeSubmission.md):
  interactive helper for preparing and optionally submitting a Trio.
- [`submitTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/submitTrioSubmission.md):
  submit Trio to [BenchHub
  Datasets](https://docs.google.com/spreadsheets/d/1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg/edit?usp=sharing).

``` r
library(BenchHub)
```

### Step 1: GitHub Personal Access Token (PAT)

A GitHub personal access token is needed for uploading custom metrics to
a GitHub gist.

1.  Log in to your GitHub account.
2.  Navigate to **Settings** \> **Developer settings** \> **Personal
    access tokens** \> **Tokens (classic)**.
3.  Click **Generate new token**.
4.  Select the following scopes:
    - `gist` (for creating and managing gists)
5.  Click **Generate token** and copy the token.

#### Set the PAT in R

Add the token to your environment in R:

``` r
Sys.setenv(GITHUB_PAT = "your_personal_access_token")
```

Replace `"your_personal_access_token"` with the token you copied.

### Step 2: Build the submission bundle

1.  In R, create a `Trio` object and ensure it is properly populated
    with data, supporting evidence, and metrics.
2.  Use
    [`writeSubmission()`](https://sydneybiox.github.io/BenchHub/reference/writeSubmission.md)
    to prepare the Trio submission:

``` r
bundle <- writeSubmission(trio)
```

During the interactive workflow,
[`writeSubmission()`](https://sydneybiox.github.io/BenchHub/reference/writeSubmission.md)
may prompt you for:

- dataset metadata such as the dataType, technology and description
- task information for the Trio
- mappings between supporting evidence and tasks
- metric metadata where needed
- file preparation details, including whether to verify a Figshare
  article, and
- optional submission details if you choose to submit immediately.

### Step 3: Submit the Trio

After reviewing the generated bundle, you can submit it with
[`submitTrioSubmission()`](https://sydneybiox.github.io/BenchHub/reference/submitTrioSubmission.md):

``` r
response <- submitTrioSubmission(
  submission = bundle$submission,
  submittedBy = "your.name@example.org"
)
```

If you prefer a single interactive step, you can also let
[`writeSubmission()`](https://sydneybiox.github.io/BenchHub/reference/writeSubmission.md)
submit at the end:

``` r
bundle <- writeSubmission(
  trio,
  submittedBy = "your.name@example.org",
  submit = TRUE
)
```

### Step 4: Verify the submission

Once you upload the Trio, it will be first uploaded in to
Submission_Master sheet. After BenchHub team completed the review and
approved, the Trio information will update to Dataset, DatasetTask,
DatasetEvidence, DatasetTaskMetric and Metric table. Check the [BenchHub
Datasets](https://docs.google.com/spreadsheets/d/1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg/edit?usp=sharing)
to ensure your dataset has been added under the submission tables.

### Step 5: Downloading an existing Trio

You can also download a previously submitted Trio by its dataset ID
under Dataset sheet:

``` r
trio <- downloadSubmissionTrio("datasetID", cachePath = tempdir())
```

### Notes

- Ensure you have an active internet connection during the upload
  process.
- If you encounter any issues, check that your Figshare URL and
  submission details are correctly set.

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
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] BenchHub_0.99.12 BiocStyle_2.38.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1       dplyr_1.2.1            farver_2.1.2          
    ##  [4] S7_0.2.1-1             fastmap_1.2.0          bayestestR_0.17.0     
    ##  [7] digest_0.6.39          rpart_4.1.24           lifecycle_1.0.5       
    ## [10] cluster_2.1.8.2        survival_3.8-6         magrittr_2.0.5        
    ## [13] compiler_4.5.3         rlang_1.2.0            Hmisc_5.2-5           
    ## [16] sass_0.4.10            tools_4.5.3            yaml_2.3.12           
    ## [19] data.table_1.18.2.1    knitr_1.51             htmlwidgets_1.6.4     
    ## [22] curl_7.0.0             ggstance_0.3.7         plyr_1.8.9            
    ## [25] RColorBrewer_1.1-3     foreign_0.8-91         withr_3.0.2           
    ## [28] purrr_1.2.2            desc_1.4.3             nnet_7.3-20           
    ## [31] grid_4.5.3             datawizard_1.3.0       googledrive_2.1.2     
    ## [34] colorspace_2.1-2       ggplot2_4.0.2          scales_1.4.0          
    ## [37] insight_1.5.0          cli_3.6.6              rmarkdown_2.31        
    ## [40] dotwhisker_0.8.4       ragg_1.5.2             generics_0.1.4        
    ## [43] rstudioapi_0.18.0      performance_0.16.0     reshape2_1.4.5        
    ## [46] parameters_0.28.3      ggcorrplot_0.1.4.1     cachem_1.1.0          
    ## [49] stringr_1.6.0          splines_4.5.3          BiocManager_1.30.27   
    ## [52] cellranger_1.1.0       base64enc_0.1-6        marginaleffects_0.32.0
    ## [55] vctrs_0.7.3            Matrix_1.7-4           jsonlite_2.0.0        
    ## [58] bookdown_0.46          patchwork_1.3.2        ggrepel_0.9.8         
    ## [61] Formula_1.2-5          htmlTable_2.4.3        systemfonts_1.3.2     
    ## [64] tidyr_1.3.2            jquerylib_0.1.4        splitTools_1.0.1      
    ## [67] glue_1.8.1             pkgdown_2.2.0          survAUC_1.4-0         
    ## [70] stringi_1.8.7          gtable_0.3.6           tibble_3.3.1          
    ## [73] pillar_1.11.1          rappdirs_0.3.4         htmltools_0.5.9       
    ## [76] R6_2.6.1               httr2_1.2.2            textshaping_1.0.5     
    ## [79] evaluate_1.0.5         lattice_0.22-9         backports_1.5.1       
    ## [82] googlesheets4_1.1.2    broom_1.0.12           ggsci_5.0.0           
    ## [85] gargle_1.6.1           bslib_0.10.0           Rcpp_1.1.1-1          
    ## [88] gridExtra_2.3          checkmate_2.3.4        xfun_0.57             
    ## [91] fs_2.1.0               pkgconfig_2.0.3
