
# BenchHub

<!-- badges: start -->
<!-- badges: end -->

## Installation instruction

```r
 devtools::install_github("SydneyBioX/TrioR")
```

## Goals

- Clean and simple user interface (API), with clear information and error messages using modern CLI outputs (progress bars, stack tracing, colours, …).
- Data handling, including caching and data integrity verification (MD5 checksum?).
- Simplify novel method evaluation by providing sample code and inputs for the evaluate function.
- Ensure that it is easy and highly flexible to add new datasets from various sources.
	- Use YAML to encode dataset metadata to ensure language interoperability.

## Finding Datasets

We could find datasets by datatype, task and patterns.

```r
library(TrioR)

findData(dataType = "SST", task = "Celltype Classification")
#outputs a data frame with metadata about the matched datasets

findData(name = "*IMC*") # match datasets which have IMC in their name
```

## Getting Data

Get data from various sources.

```r
trio <- getTrio("openproblems_v1/tenx_1k_pbmc") # some automatic caching, etc.
trio$tasks()
# outputs information about tasks, metrics and "gold standards" of the included data
```

## Method Evaluation

Evaluate your method.

```r
myCellTypes <- myMethod1(trio$getSCE())
mySegementation <- myMethod2(trio$getRaster())

outputs <- list("cellTypes" = myCellTypes, "segmentation" = mySegementation)

evaluation <- trio$evaluate(outputs) 
```

### Get an evaluation template

Generate a temple of what an input to `trio$evaluate()` would have to look like for a successful evaluation.

```r
template <- trio$getTemplate(task = c("Celltype Classification", "Cell Segmentation"))

names(template) # [1] "celltype"     "segmentation"
length(template$celltype) # [1] 1000
typeof(template$celltype[1]) # [1] "character"
unique(template$celltype) # [1] "a" "b"
```

Generate an assertion that checks the format of the method output.

```r
checkInput <- trio$checkInput(myCelltypes, task = "Celltype Classification")
# informative errors that allow the user to make their outputs conform to the input of trio$evaluate()
```

## Potential Features

### Multiple Datasets

Get multiple datasets in one trio and perform the evaluation on all relevant dataset within the trio.

```r
trio <- getTrio(c("openproblems_v1/tenx_1k_pbmc", "openproblems_v1/tenx_5k_pbmc"))
```

### User Defined Cache Directory

Allow users to define a cache directory for the datasets (i.e., in our case, `biostat` folder).
