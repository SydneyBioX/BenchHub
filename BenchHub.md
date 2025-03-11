# BenchHub

## Installation instruction

`devtools::install_github("SydneyBioX/BenchHub")`


## About 

BenchHub is a data storage framework to facilitate living benchmark​s. It aims to enhance reproducibility and accessibility of benchmarking studies by making it easier to store, analyse and share benchmarking data.  

The two components currently in BenchHub are:  
- **Trio**: A data structure consisting of Dataset, Auxiliary and Metric to faciliate the sharing of benchmarking datasets within the community.    
- **BenchmarkInsight**: A data structure for storing benchmarking results and provides a collection of visualisations to faciliate the analysis of benchmarking results. 


### Trio


Trio is built around three key components:

Dataset: Data used by the methods to generate output.   
Auxiliary Data: Metadata to compare with the output of methods, such as cell type, patient outcome, disease pathway.      
Metric: Evaluation metrics used to compare output of methods with auxiliary data. 


![](/Users/yue/Downloads/triooverview.png)

Trio is implemented as R6 object with fields to store each of the components. 
 

![](/Users/yue/Downloads/triodatastructure.png)

### BenchmarkInsight 

BenchmarkInsight serves as a visualisation and analysis tool for benchmarking results. It contains multiple visualisation techniques to help researchers analyse benchmarking results in terms of data, methods and metrics. 

![](/Users/yue/Downloads/benchmarkinsight1.png)


It currently supports the following plot types. 

![](/Users/yue/Downloads/benchmarkinsight2.png)





## Vignettes 

We provide a comprehensive list of vignettes for every key step of BenchHub.  
Please refer to the [website](https://sydneybiox.github.io/BenchHub/) to see all vignettes listed below:             

| Vignette       | Description |
| -------------  | ------------- |
| 1  Introduction to the Trio Class    |    </li><li>  How to set up trio  <br> </li><li>  How to add auxData <br>  </li><li> How to add metric <br>  </li><li> Evaluation example
| 2 Evaluation using TrioR      |      </li><li>  Cross-validation with Trio  <br> </li><li>  Evaluation without cross-validation    <br> </li><li>  Importing result to BenchmarkInsights  |

