
# BenchHub

<!-- badges: start -->
<!-- badges: end -->

<img src="https://raw.githubusercontent.com/SydneyBioX/BenchHub/devel/inst/benchhub_sticker.png" align="right" width="200">


## Installation instruction

```r
 devtools::install_github("SydneyBioX/BenchHub")
```


## About 

BenchHub is a data storage framework implemented in R language to facilitate living benchmarks. It aims to enhance reproducibility and accessibility of benchmarking studies by making it easier to store, analyse and share benchmarking data.  

The three components currently in BenchHub are:  
- **Trio**: A data structure consisting of Dataset, Evidence and Metric to faciliate the sharing of benchmarking datasets within the community.    
- **BenchmarkInsight**: A data structure for storing benchmarking results and provides a collection of visualisations to faciliate the analysis of benchmarking results. 
- **BenchmarkInsight**: A data structure for storing collections of Trio objects and helper functions for specific benchmarking tasks. 


BenchHub aims to make benchmarking easier for multiple groups of users in the community: 
- **Benchmark Developers**: BenchHub offers a central storage for contributing benchmarking studies with the community
- **Method Contributors**: BenchHub allows evaluation of new tools against established benchmarks in the database without starting from scratch  
- **Benchmark Consumers**: BenchHub allows exploring and interpretation of results to make informed method selections



### Trio


Trio is built around three key components:

**Data**: Data used by the methods to generate output.   
**Supporting Evidence**: Metadata to compare with the output of methods, such as cell type, patient outcome, disease pathway.      
**Metric**: Evaluation metrics used to compare output of methods with supporting evidence. 

 
Trio is implemented as R6 object with fields to store each of the components. 
 


<p align="center">
  <img src="https://raw.githubusercontent.com/SydneyBioX/BenchHub/devel/inst/triooverview.png" width="35%" />
  <img src="https://raw.githubusercontent.com/SydneyBioX/BenchHub/devel/inst/triodatastructure.png" width="55%" />
</p>


### BenchmarkInsight 

BenchmarkInsight serves as a visualisation and analysis tool for benchmarking results. It contains multiple visualisation techniques to help researchers analyse benchmarking results in terms of data, methods and metrics. 

 
Once results are evaluated using Trio, the output can be directly passed into the benchmarkInsight object. BenchmarkInsight currently supports the following plot types. 
 

<p align="center">
  <img src="https://raw.githubusercontent.com/SydneyBioX/BenchHub/devel/inst/benchmarkinsight1.png" width="55%" />
  <img src="https://raw.githubusercontent.com/SydneyBioX/BenchHub/devel/inst/benchmarkinsight2.png" width="35%" />
</p>

### BenchmarkStudy

Benchmarking often involve large amount of datasets and processing scripts. BenchmarkStudy serves as the organisation framework for such objects. It stores references to collections of Trio objects and helper functions that standardise method outputs into a common form to evaluation. 
 

## Vignettes 

We provide a comprehensive list of vignettes for every key step of BenchHub.  
Please refer to the [website](https://sydneybiox.github.io/BenchHub/) to see all vignettes listed below:             

| Vignette                                                                                                      | Description                                                                                                                                                                               |
| ------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1  [Introduction to the Trio Class](https://sydneybiox.github.io/BenchHub/articles/v01_intro_trio.html)       | </li><li>  How to set up trio  <br> </li><li>  How to add evidence <br>  </li><li> How to add metric <br>  </li><li> Evaluation example                                                   |
| 2 [Evaluation using TrioR](https://sydneybiox.github.io/BenchHub/articles/v02_Evaluation_using_Trio.html)     | </li><li>  Cross-validation with Trio  <br> </li><li>  Evaluation without cross-validation    <br> </li><li>  Importing result to benchmarkInsights                                       |
| 3 [Introduction of benchmarkInsight Class](https://sydneybiox.github.io/BenchHub/articles/v03_intro_bmi.html) | </li><li>  Creating bnechmarkInsights Class <br> </li><li>  Collection of visualisations    <br> </li><li>  Collection of analytical questions that can be answered with benchmarkInsight |
| 4 [Uploading a Trio to Curated Trio Datasets](https://sydneybiox.github.io/BenchHub/articles/v04_uploading_a_Trio.html) | <ul><li>Creating GitHub Personal Access Token</li><li>Setting up googlesheets4 authentication</li><li>Uploading datasets to Figshare</li><li>Contributing Trios to the database</li></ul> |
| 5 [Working with BenchmarkStudy](https://sydneybiox.github.io/BenchHub/articles/v05_benchmarkstudy.html) | <ul><li>Creating and organizing benchmark studies</li><li>Adding Trios and mapping functions</li><li>For benchmark developers: uploading studies</li><li>For method developers: evaluating new methods against existing benchmarks</li></ul> |
