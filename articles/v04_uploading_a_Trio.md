# 4 Uploading a Trio to Curated Trio Datasets

## Instructions for Uploading a Trio to Curated Trio Datasets

This guide provides step-by-step instructions for uploading a Trio
object to the [Curated Trio Datasets
sheet](https://docs.google.com/spreadsheets/d/1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY/edit).
Curated Trio Dataset is a list of datasets with proven utility. It
includes creating a GitHub Personal Access Token (PAT) with gist write
access and uploading data to Figshare.

### Step 1: Create a GitHub Personal Access Token (PAT)

To upload metrics as a GitHub Gist, you need a GitHub PAT with gist
write access.

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

#### Ensure `googlesheets4` is set up

1.  Install the `googlesheets4` package if you haven’t already:

    ``` r
    install.packages("googlesheets4")
    ```

2.  Authenticate with your Google account:

    ``` r
    googlesheets4::gs4_auth()
    ```

### Step 2: Add the Trio to Curated Trio Datasets

1.  In R, create a `Trio` object and ensure it is properly populated
    with data, supporting evidence, and metrics.
2.  Use the `writeCTD()` method to upload the Trio metadata to the
    Curated Trio Datasets sheet:

``` r
trio$writeCTD(name = "Your Dataset Name")
```

3.  Follow the prompts to:
    - Save the dataset and supporting evidence locally (optional).
    - Provide the Figshare URL. For more detailed instructions on
      uploading to Figshare, see below.
    - Select the data type (e.g., omics, clinical, spatial, other).
    - Confirm the upload of supporting evidence to Figshare.

### Step 3: Verify the Upload

1.  Check the Curated Trio Datasets Google Sheet to ensure your dataset
    has been added: [Curated Trio
    Datasets](https://docs.google.com/spreadsheets/d/1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY/)

2.  Verify that the metrics have been uploaded as a GitHub Gist.

### Notes

- Ensure you have an active internet connection during the upload
  process.
- If you encounter any issues, check that your GitHub PAT and Figshare
  URL are correctly set.

#### Uploading Data to Figshare

1.  Log in to your Figshare account.
2.  Click **Create a new item**.
3.  Fill in the required metadata fields (e.g., title, description,
    tags).
4.  Upload your dataset files (e.g., `.rds` files for the dataset and
    supporting evidence).
    - NOTE: `writeCTD` can create the
5.  Publish the item to make it publicly accessible.
6.  Copy the Figshare URL of the published item.

Happy uploading!

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
    ## [1] BiocStyle_2.38.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] digest_0.6.39       desc_1.4.3          R6_2.6.1           
    ##  [4] bookdown_0.46       fastmap_1.2.0       xfun_0.57          
    ##  [7] cachem_1.1.0        knitr_1.51          htmltools_0.5.9    
    ## [10] rmarkdown_2.31      lifecycle_1.0.5     cli_3.6.5          
    ## [13] sass_0.4.10         pkgdown_2.2.0       textshaping_1.0.5  
    ## [16] jquerylib_0.1.4     systemfonts_1.3.2   compiler_4.5.3     
    ## [19] tools_4.5.3         ragg_1.5.2          bslib_0.10.0       
    ## [22] evaluate_1.0.5      yaml_2.3.12         BiocManager_1.30.27
    ## [25] jsonlite_2.0.0      rlang_1.2.0         fs_2.0.1           
    ## [28] htmlwidgets_1.6.4
