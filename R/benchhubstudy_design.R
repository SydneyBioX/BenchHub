
library(SingleCellExperiment)
library(R6)


# implement function to install dependency automatically 
# 

BenchHubStudy <- R6Class("BenchHubStudy",
                         public = list(
                           # Fields
                           name = NULL, 
                           trios = NULL,
                           mapping_functions = NULL,
                       
                           # Constructor
                           initialize = function( name = NULL) {
                             self$name <- name  
                             self$trios <- list()
                             self$mapping_functions <- list()
                           },
                           
                           # Add a new trio
                           add_trio = function(name, trio_object) {
                             if (!is.character(name)) stop("Name must be a character string.")
                             self$trios[[name]] <- trio_object
                           },
                           
                           # Add a mapping function with metadata
                           add_mapping_function = function(name, func, input_description, output_description,  example_usage = NULL) {
                             if (!is.function(func)) stop("Mapping function must be a function.")
                             self$mapping_functions[[name]] <- list(
                               func = func,
                               doc = list(
                                 input_description = input_description,
                                 output_description = output_description,
                                 example_usage = example_usage
                               )
                             )
                           },
                           
                           # Apply a mapping function to data
                           run_mapping = function(mapping_name, data) {
                             if (!(mapping_name %in% names(self$mapping_functions))) {
                               stop(paste0("Mapping function '", mapping_name, "' not found."))
                             }
                             func <- self$mapping_functions[[mapping_name]]$func
                             return(func(data))
                           },
                           
                          #  documentation retriever for mapping function 
                           get_mapping_function_documentation = function(mapping_name) {
                             if (!(mapping_name %in% names(self$mapping_functions))) {
                               stop(paste0("Mapping function '", mapping_name, "' not found."))
                             }
                             return(self$mapping_functions[[mapping_name]]$doc)
                           }, 
                           
                          # print out names of all mapping functions so user can see
                          #  list_mapping_functions = function() {
                          #   return(names(self$mapping_functions))
                          # }, 
                          # 
                          
                          # print out names of all mapping functions so user can see
                          list_mapping_functions = function(as_tibble = FALSE) {
                            if (length(self$mapping_functions) == 0) {
                              message("No mapping functions available.")
                              return(NULL)
                            }
                            
                            mapping_info <- lapply(names(self$mapping_functions), function(name) {
                              doc <- self$mapping_functions[[name]]$doc
                              list(
                                Name = name,
                                Input = ifelse(!is.null(doc$input_description), doc$input_description, NA),
                                Output = ifelse(!is.null(doc$output_description), doc$output_description, NA) 
                              )
                            })
                            
                            df <- do.call(rbind, lapply(mapping_info, as.data.frame))
                            
                            if (as_tibble && requireNamespace("tibble", quietly = TRUE)) {
                              return(tibble::as_tibble(df))
                            } else {
                              return(df)
                            }
                          }, 
                          
                          
                          
                           # Generate R Markdown vignette template
                           generate_vignette_template = function(output_path = "benchmark_study_template.Rmd") {
                             vignette_text <- "
---
title: \"Benchmark Study Report\"
output: html_document
---

# Introduction

Describe the benchmark task and dataset.


"
                             writeLines(vignette_text, con = output_path)
                             message(paste("Vignette template written to:", output_path))
                           }
                         )
)







# starting from a benchmarker perspective 

# Initialize study
study <- BenchHubStudy$new("example study")


# Add a trio
# here assume the benchmark already created a trio 
example_trio <- readRDS("example_trio.rds")
study$add_trio("example_trio_1", example_trio)


 
# Define a mapping function
proportion_zero_gene <- function( data ){
  return ( colMeans(counts(  data )  == 0) ) 
}

# Add this mapping function to benchhubstudy
study$add_mapping_function(
  name = "Fraction Zero Genes",
  func = proportion_zero_gene,
  input_description = "SingleCellExperiment or Matrix object with gene expression counts.",
  output_description = "Numeric vector of fraction of zero counts per gene." 
)


# Define another mapping function
norm_lib_size <- function(real) {
  
  lib_size <- log1p(rowSums(counts( real )))
  
  real_dge <- edgeR::DGEList(counts = Matrix::t(counts( real )))
  norm_factors <- edgeR::calcNormFactors(real_dge, method = "TMM")
  norm_factors <-   norm_factors$samples$norm.factors
  result <-  unname( lib_size *  norm_factors )
  
  return (   result )
}

 


# Add this mapping function to benchhubstudy
study$add_mapping_function(
  name = "normalized library size",
  func = norm_lib_size,
  input_description = "SingleCellExperiment or Matrix object with gene expression counts.",
  output_description = "Numeric vector of normalised library size per cell." 
)

saveRDS(study, "example_benchmarkstudy.rds")



# Generate vignette template for user to fill in 
study$generate_vignette_template()

 
# starting the user perspective 
study <- readRDS("example_benchmarkstudy.rds")

study$name

# first they need to see what trios are available
study$trios

# suppose the user has generated a new data from a method 
# here I'm just using exactly the same data from the trio as an example output
# can think of this is benchmarking a positive control 
new_data <- study$trios$example_trio_1$data

# next, the user also needs to see what are the mapping functions available  
study$list_mapping_functions()


# suppose they are interested in the function "Fraction Zero Genes" 
# print out the mapping documentation so the user know what the input and output are
study$get_mapping_function_documentation("Fraction Zero Genes")
 

# Run mapping
mydata_prop_zero_gene <- study$run_mapping("Fraction Zero Genes", new_data)
mydata_lib_size <- study$run_mapping( "normalized library size", new_data)

 

# Evaluate by comparing to the ground truth of a trio 
example_trio_1 <- study$trios$example_trio_1
result <-  example_trio_1$evaluate(list("Fraction zero genes" = mydata_prop_zero_gene ))
result <-  example_trio_1$evaluate(list("normalized library size" = mydata_lib_size))


# Question for Nick, I wonder if this is possible
# ie, instead of having to take the trio out of the study to evaluate 
# Can I just specify the name of the trio that I am comparing to inside the benchmarkstudy object 
result <- study$evaluate("example_trio_1", (list("Fraction zero genes" = mydata_prop_zero_gene )))

# Questions for Nick
# I want to include a description field into Trio
# And add this function to benchhubstudy
# so instead of printing out everything (support evidence, metric) about every Trio
# I just print out the trio name and an overall description 
study$list_trios()


