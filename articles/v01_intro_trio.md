# 1 Introduction to the Trio Class

``` r
library(BenchHub)
```

## Motivation

BenchHub is an R ecosystem built to make benchmarking easier.

BenchHub contains two key components: `Trio` and `BenchmarkInsight`:  
- `Trio` object constructs benchmarking data by organising data,
evaluation metrics, and supporting evidence (gold standards).  
- `BenchmarkInsight` is a visualization tool that helps interpret the
results of benchmarking studies.

With BenchHub, researchers can quickly compare new methods, gain
insights, and produce trustworthy their benchmarking studies.

## Creating Trio object

The Trio class is designed to facilitate the storing and sharing of
benchmarking datasets. Each Trio is structured around a single dataset
but can include multiple metrics and multiple pieces of supporting
evidence (such as references or gold standards).

Trio objects can be created using the `Trio$new` constructor. There are
3 ways to create a Trio object:

- Curated Trio Datasets
- Source and ID
- Load an object directly

If the dataset can’t be loaded using Trio’s inbuilt loader, a custom
loader can be provided.

### Method 1: Curated Trio Datasets

You can directly use the name from the Curated Trio Datasets sheet to
initialise a Trio object populated with some metrics and supporting
evidence. This method is useful when you want to quickly start with a
predefined dataset.

``` r
tempCache <- tempdir()
trio <- Trio$new("Veteran_data", cachePath = tempCache)
trio
```

    ## 
    ## ── Trio Object ─────────────────────────────────────────────────────────────────
    ## 
    ## ── Dataset 
    ## Dataset Details:
    ##   Classes 'data.table' and 'data.frame': 137 obs. of 9 variables:
    ##   $ V1 : int 1 2 3 4 5 6 7 8 9 10 ...
    ##   $ trt : int 1 1 1 1 1 1 1 1 1 1 ...
    ##   $ celltype: int 1 1 1 1 1 1 1 1 1 1 ...
    ##   $ time : int 72 411 228 126 118 10 82 110 314 100 ...
    ##   $ status : int 1 1 1 1 1 1 1 1 1 0 ...
    ##   $ karno : int 60 70 60 60 70 20 40 80 50 70 ...
    ##   $ diagtime: int 7 5 3 9 11 5 10 29 18 6 ...
    ##   $ age : int 69 64 38 63 65 49 69 68 43 70 ...
    ##   $ prior : int 0 10 0 10 10 0 10 0 0 0 ...
    ##   ... (truncated)
    ## Data Source: "figshare"
    ## Dataset ID: "26142922/47361073"
    ## Cache Path: "/tmp/RtmpDgyV07"
    ## Split Indices: "None"
    ## 
    ## ── Supporting Evidence 
    ## Number of Supporting Evidence: 1
    ## Names of Supporting Evidence: "survival_data"
    ## 
    ## ── Metrics 
    ## Number of Metrics: 6
    ## Names of Metrics: "harrell_cindex", "begg_cindex", "uno_cindex", "gh_cindex",
    ## "brier_score", and "time_dep_auc"

The above output shows that we have a Trio with a dataset, metrics, and
supporting evidence. The dataset contains 137 rows and 9 columns, and
the metrics and supporting evidence are already populated and printed.

This Trio is ready for use in survival prediction evaluation.

### Method 2: Source and ID

Trio objects can be created by specifying an ID from a source with a
valid trio downloader. This method is useful when you have a specific
dataset ID from a supported source like Figshare, GEO, or ExperimentHub.

For example, if you have a dataset ID from Figshare, GEO, or
ExperimentHub, you can create a Trio object as follows:

- figshare: `Trio$new("figshare:figshareID[/fileID]")`
  - `fileID` can optionally be provided to specify a specific file in
    the collection.
- GEO: `Trio$new("geo:GSEID[/Supplementary_filename]")`
  - `Supplementary_filename` can optionally be provided to specify a
    specific supplementary file in the series.
- experimenthub: `Trio$new("experimenthub:experimenthubID")`

The example below shows how to create a Trio object using a Figshare
dataset with a `datasetID`.

``` r
trioA <- Trio$new("figshare:26142922/47361079",
                  evidenceColumns = c("time", "status"),
                  task = "Risk Estimation",
                  metrics = list("Harrell C-index" = harrelCIndexMetric, "Begg C-index" = beggCIndexMetric),
                  cachePath = tempCache)
trioA
```

    ## 
    ## ── Trio Object ─────────────────────────────────────────────────────────────────
    ## 
    ## ── Dataset 
    ## Dataset Details:
    ##   Classes 'data.table' and 'data.frame': 58 obs. of 19820 variables:
    ##   $ V1 : chr "GSM746861" "GSM746862" "GSM746863" "GSM746864" ...
    ##   $ A1BG : num 6.16 5.75 5.9 6 6.98 ...
    ##   $ A1BG-AS1 : num 6.53 7.21 6.71 6.79 7.21 ...
    ##   $ A1CF : num 3.92 4.65 3.95 4.2 4.57 ...
    ##   $ A2M : num 7.21 6.94 7.34 8.22 7.09 ...
    ##   $ A2M-AS1 : num 4.62 6.07 4.04 4.78 4.33 ...
    ##   $ A2ML1 : num 4.17 3.66 4.16 3.94 3.73 ...
    ##   $ A4GNT : num 3.99 4.48 4.43 4.13 4.61 ...
    ##   $ AAAS : num 7.12 7.14 6.27 7.6 6.95 ...
    ##   ... (truncated)
    ## Data Source: "figshare"
    ## Dataset ID: "26142922/47361079"
    ## Cache Path: "/tmp/RtmpDgyV07"
    ## Split Indices: "None"
    ## 
    ## ── Supporting Evidence 
    ## Number of Supporting Evidence: 1
    ## Names of Supporting Evidence: "survival_data"
    ## 
    ## ── Metrics 
    ## Number of Metrics: 6
    ## Names of Metrics: "harrell_cindex", "begg_cindex", "uno_cindex", "gh_cindex",
    ## "brier_score", and "time_dep_auc"

### Method 3: Load an object directly

Trio can also be created by passing an object directly into the
constructor. This method is useful when you already have a dataset
loaded in your R environment and want to use it with Trio.

If you have your own dataset, you can easily create a trio object as
well. Below is an example using a microbiome dataset.

``` r
data("lubomski_microbiome_data", package = "BenchHub")
trioB <- Trio$new(data = x, evidence = list(`Diagnosis` = list(evidence = lubomPD, metrics = "Balanced Accuracy")),
                  metrics = list(`Balanced Accuracy` = balAccMetric),
                  datasetID = "lubomski_microbiome")
```

    ## Warning: No sample IDs found on evidence. Assuming same order as data
    ## and adding them.

``` r
trioB
```

    ## 
    ## ── Trio Object ─────────────────────────────────────────────────────────────────
    ## 
    ## ── Dataset 
    ## Dataset Details:
    ##   num [1:575, 1:1192] 0 0.00448 0 0.02983 0 ...
    ##   - attr(*, "dimnames")=List of 2
    ## Data Source:
    ## Dataset ID: "lubomski_microbiome"
    ## Cache Path:
    ## Split Indices: "None"
    ## 
    ## ── Supporting Evidence 
    ## Number of Supporting Evidence: 1
    ## Names of Supporting Evidence: "Diagnosis"
    ## 
    ## ── Metrics 
    ## Number of Metrics: 1
    ## Names of Metrics: "Balanced Accuracy"

### Bonus: Using a custom loader

Trio supports custom loaders for data formats not directly supported by
Trio. A loader is any function that takes in a path, provided by a
downloader, and returns an object to be loaded into Trio.

Below, we use an anonymous function to wrap
[`GEOquery::getGEO`](http://seandavi.github.io/GEOquery/reference/getGEO.md)
and
[`Biobase::phenoData`](https://rdrr.io/pkg/Biobase/man/phenoData.html),
and provide them with the path of the downloaded file to extract both
the gene expression values and the patient classes from different
tables.

``` r
trioGEO <- Trio$new(
  "GEO:GSE46474",
  dataLoader = \(path) Biobase::exprs(GEOquery::getGEO(filename = path)),
  task = "Rejection Prediction",
  evidenceLoader = \(path) Biobase::phenoData(GEOquery::getGEO(filename = path))[["procedure status:ch1"]],
  metrics = list(`Balanced Accuracy` = balAccMetric),
  cachePath = tempdir()
)
trioGEO
```

    ## 
    ## ── Trio Object ─────────────────────────────────────────────────────────────────
    ## 
    ## ── Dataset 
    ## Dataset Details:
    ##   num [1:54613, 1:40] 5.7 4.82 7.95 7.64 2.83 ...
    ##   - attr(*, "dimnames")=List of 2
    ## Data Source: "geo"
    ## Dataset ID: "GSE46474"
    ## Cache Path: "/tmp/RtmpDgyV07"
    ## Split Indices: "None"
    ## 
    ## ── Supporting Evidence 
    ## Number of Supporting Evidence: 0
    ## Names of Supporting Evidence:
    ## 
    ## ── Metrics 
    ## Number of Metrics: 0
    ## Names of Metrics:

## Adding Components to Trio Sequentially

### Adding metrics

In benchmarking studies, a metric refers to the measurement used to
evaluate a specific task. In this example, we define a task called
survival model prediction.

In Trio, a metric is any pairwise function of the form
`f(expected, predicted)` which returns a single value.

We can also add metrics that have additional arguments by passing a list
of arguments to the args parameter.

``` r
eq <- \(expected, predicted, inequality = FALSE) {
  if (inequality) {
    return(!expected == predicted)
  }

  expected == predicted
}

trio$addMetric("equality", eq)

# Trio also supports passing through arguments to a metric
# Note: parameter names added for clarity
trio$addMetric(
  name = "inequality", metric = eq, args = list(inequality = TRUE)
)
```

In the above example, we added two metrics based on the same function:
“equality” and “inequality”. The “equality” metric checks if the
expected and predicted values are equal, while the “inequality” metric
checks if they are not equal.

Underneeth the hood, Trio creates a wrapper function that calls the
metric function with the specified arguments.

``` r
trio$metrics$inequality
```

    ## function (evidence, to_eval) 
    ## {
    ##     do.call(metric, append(list(evidence, to_eval), args))
    ## }
    ## <bytecode: 0x564af91dbfd0>
    ## <environment: 0x564af2b5f428>

## Other Features

### Caching

Trio uses caching to avoid lengthy downloads after the first time a data
set is accessed. The `cachePath` parameter specifies the path to the
cache directory. If not specified, the cache directory defaults to
`~/.cache/R/TrioR/`.

### Data Splitting

Trio supports data splitting for cross-validation. The `split` method
splits the data into training and test sets for cross-validation. The
`splitIndices` attribute stores the indices for each sample.

Indices are generated using the `splitTools` package. The `split` method
takes in the outcome variable and the number of folds and repeats. The
`stratify` parameter can be used to stratify the outcome variable.

``` r
trio$split(y = 1:137, n_fold = 2, n_repeat = 5, seed = 1234, stratify = FALSE)
trio$splitIndices
```

    ## $Fold1.Rep1
    ##  [1]   2   3   4   5   6   7   8   9  10  11  12  13  18  21  22  24  25  26  30
    ## [20]  31  34  36  37  42  45  48  49  55  58  59  60  61  64  66  68  70  71  73
    ## [39]  76  77  81  82  87  89  90  91  92  93  96  97  98 102 103 104 105 108 109
    ## [58] 110 111 115 122 125 126 130 132 133 134 135
    ## 
    ## $Fold2.Rep1
    ##  [1]   1  14  15  16  17  19  20  23  27  28  29  32  33  35  38  39  40  41  43
    ## [20]  44  46  47  50  51  52  53  54  56  57  62  63  65  67  69  72  74  75  78
    ## [39]  79  80  83  84  85  86  88  94  95  99 100 101 106 107 112 113 114 116 117
    ## [58] 118 119 120 121 123 124 127 128 129 131 136 137
    ## 
    ## $Fold1.Rep2
    ##  [1]   3   4   5   6   7   8  13  15  21  22  23  24  28  29  30  31  32  34  35
    ## [20]  42  47  48  51  52  54  57  59  60  61  63  64  66  67  73  74  77  78  79
    ## [39]  82  84  85  89  90  92  93  94  96  98  99 102 103 108 109 110 112 114 116
    ## [58] 122 123 124 125 126 128 131 132 133 135 136
    ## 
    ## $Fold2.Rep2
    ##  [1]   1   2   9  10  11  12  14  16  17  18  19  20  25  26  27  33  36  37  38
    ## [20]  39  40  41  43  44  45  46  49  50  53  55  56  58  62  65  68  69  70  71
    ## [39]  72  75  76  80  81  83  86  87  88  91  95  97 100 101 104 105 106 107 111
    ## [58] 113 115 117 118 119 120 121 127 129 130 134 137
    ## 
    ## $Fold1.Rep3
    ##  [1]   2   5   6   8   9  11  13  14  17  19  20  21  24  26  27  30  31  32  34
    ## [20]  36  37  38  41  42  44  46  47  48  52  53  54  58  59  60  62  65  66  69
    ## [39]  70  71  72  75  76  77  78  80  84  85  86  88  89  90  91  92  93  94  99
    ## [58] 100 103 104 109 111 112 113 121 126 127 128 129
    ## 
    ## $Fold2.Rep3
    ##  [1]   1   3   4   7  10  12  15  16  18  22  23  25  28  29  33  35  39  40  43
    ## [20]  45  49  50  51  55  56  57  61  63  64  67  68  73  74  79  81  82  83  87
    ## [39]  95  96  97  98 101 102 105 106 107 108 110 114 115 116 117 118 119 120 122
    ## [58] 123 124 125 130 131 132 133 134 135 136 137
    ## 
    ## $Fold1.Rep4
    ##  [1]   1   2   5   8  10  14  16  17  19  22  23  24  25  27  28  31  33  34  37
    ## [20]  39  40  42  43  44  48  53  54  56  58  61  65  67  68  70  71  72  74  77
    ## [39]  78  80  81  82  83  84  85  87  89  91  93  95  97 102 104 111 113 119 120
    ## [58] 122 123 125 126 127 129 131 132 133 134 135
    ## 
    ## $Fold2.Rep4
    ##  [1]   3   4   6   7   9  11  12  13  15  18  20  21  26  29  30  32  35  36  38
    ## [20]  41  45  46  47  49  50  51  52  55  57  59  60  62  63  64  66  69  73  75
    ## [39]  76  79  86  88  90  92  94  96  98  99 100 101 103 105 106 107 108 109 110
    ## [58] 112 114 115 116 117 118 121 124 128 130 136 137
    ## 
    ## $Fold1.Rep5
    ##  [1]   2   3   5   6   7   8   9  10  11  13  14  15  18  19  21  23  29  31  33
    ## [20]  35  37  40  42  43  45  46  47  51  52  54  56  57  60  61  62  66  67  68
    ## [39]  69  72  76  79  81  86  87  93  94  97 100 101 102 103 106 107 108 111 115
    ## [58] 116 117 118 119 123 126 127 128 129 132 134
    ## 
    ## $Fold2.Rep5
    ##  [1]   1   4  12  16  17  20  22  24  25  26  27  28  30  32  34  36  38  39  41
    ## [20]  44  48  49  50  53  55  58  59  63  64  65  70  71  73  74  75  77  78  80
    ## [39]  82  83  84  85  88  89  90  91  92  95  96  98  99 104 105 109 110 112 113
    ## [58] 114 120 121 122 124 125 130 131 133 135 136 137

## Conclusion

In this vignette, we introduced the Trio class and demonstrated how to
create a Trio object using curated datasets, source and ID, or loading
an object directly. We also showed how to add metrics and supporting
evidence to a Trio object and evaluate the performance of different
methods using these metrics and supporting evidence. We hope this
vignette helps you get started with Trio and conduct benchmarking
studies more effectively.

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
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] BenchHub_0.99.5  BiocStyle_2.38.0
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] gridExtra_2.3               httr2_1.2.1                
    ##   [3] rlang_1.1.6                 magrittr_2.0.4             
    ##   [5] matrixStats_1.5.0           compiler_4.5.2             
    ##   [7] survAUC_1.4-0               systemfonts_1.3.1          
    ##   [9] vctrs_0.6.5                 reshape2_1.4.5             
    ##  [11] stringr_1.6.0               pkgconfig_2.0.3            
    ##  [13] fastmap_1.2.0               XVector_0.50.0             
    ##  [15] backports_1.5.0             ggstance_0.3.7             
    ##  [17] rmarkdown_2.30              tzdb_0.5.0                 
    ##  [19] ragg_1.5.0                  purrr_1.2.0                
    ##  [21] xfun_0.54                   cachem_1.1.0               
    ##  [23] jsonlite_2.0.0              DelayedArray_0.36.0        
    ##  [25] broom_1.0.10                cluster_2.1.8.1            
    ##  [27] R6_2.6.1                    bslib_0.9.0                
    ##  [29] stringi_1.8.7               RColorBrewer_1.1-3         
    ##  [31] limma_3.66.0                rpart_4.1.24               
    ##  [33] GenomicRanges_1.62.0        jquerylib_0.1.4            
    ##  [35] cellranger_1.1.0            Rcpp_1.1.0                 
    ##  [37] Seqinfo_1.0.0               bookdown_0.45              
    ##  [39] SummarizedExperiment_1.40.0 knitr_1.50                 
    ##  [41] R.utils_2.13.0              base64enc_0.1-3            
    ##  [43] parameters_0.28.2           readr_2.1.6                
    ##  [45] IRanges_2.44.0              rentrez_1.2.4              
    ##  [47] Matrix_1.7-4                splines_4.5.2              
    ##  [49] nnet_7.3-20                 tidyselect_1.2.1           
    ##  [51] abind_1.4-8                 rstudioapi_0.17.1          
    ##  [53] yaml_2.3.10                 curl_7.0.0                 
    ##  [55] lattice_0.22-7              tibble_3.3.0               
    ##  [57] plyr_1.8.9                  Biobase_2.70.0             
    ##  [59] withr_3.0.2                 bayestestR_0.17.0          
    ##  [61] S7_0.2.1                    evaluate_1.0.5             
    ##  [63] marginaleffects_0.31.0      foreign_0.8-90             
    ##  [65] desc_1.4.3                  survival_3.8-3             
    ##  [67] xml2_1.5.0                  pillar_1.11.1              
    ##  [69] BiocManager_1.30.27         MatrixGenerics_1.22.0      
    ##  [71] checkmate_2.3.3             stats4_4.5.2               
    ##  [73] insight_1.4.2               generics_0.1.4             
    ##  [75] hms_1.1.4                   S4Vectors_0.48.0           
    ##  [77] ggplot2_4.0.1               scales_1.4.0               
    ##  [79] glue_1.8.0                  Hmisc_5.2-4                
    ##  [81] tools_4.5.2                 data.table_1.17.8          
    ##  [83] GEOquery_2.78.0             XML_3.99-0.20              
    ##  [85] fs_1.6.6                    grid_4.5.2                 
    ##  [87] tidyr_1.3.1                 datawizard_1.3.0           
    ##  [89] colorspace_2.1-2            googlesheets4_1.1.2        
    ##  [91] patchwork_1.3.2             performance_0.15.2         
    ##  [93] htmlTable_2.4.3             googledrive_2.1.2          
    ##  [95] splitTools_1.0.1            Formula_1.2-5              
    ##  [97] cli_3.6.5                   rappdirs_0.3.3             
    ##  [99] textshaping_1.0.4           S4Arrays_1.10.0            
    ## [101] gargle_1.6.0                dplyr_1.1.4                
    ## [103] gtable_0.3.6                ggcorrplot_0.1.4.1         
    ## [105] R.methodsS3_1.8.2           ggsci_4.1.0                
    ## [107] sass_0.4.10                 digest_0.6.39              
    ## [109] BiocGenerics_0.56.0         SparseArray_1.10.2         
    ## [111] ggrepel_0.9.6               htmlwidgets_1.6.4          
    ## [113] farver_2.1.2                R.oo_1.27.1                
    ## [115] htmltools_0.5.8.1           pkgdown_2.2.0              
    ## [117] lifecycle_1.0.4             httr_1.4.7                 
    ## [119] statmod_1.5.1               dotwhisker_0.8.4
