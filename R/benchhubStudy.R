#' BenchHubStudy Class
#' @description
#' This class manages a collection of benchmark trios and mapping functions.
#' It allows adding new trios, mapping functions, and running mappings on data.
#' @param trios A list to store benchmark trios.
#' @param mapping_functions A list to store mapping functions with metadata.
#' @export
BenchHubStudy <- R6Class(
  "BenchHubStudy",
  public = list(
    # Fields
    trios = list(),
    mapping_functions = list(),

    #` @desctription Create a new BenchHubStudy object
    #' @param trios A list of Trio objects to initialize the study.
    initialize = function(trios = list()) {
      self$trios <- trios
      if (!length(trios) == 0 && !all(sapply(trios, inherits, "Trio"))) {
        stop("All trios must be Trio objects.")
      }
    },
    #' @description
    # Add a new trio to the study
    #' @param name A character string to name the trio.
    #' @param trio_object A Trio object to be added.
    add_trio = function(name, trio_object) {
      if (!is.character(name)) stop("Name must be a character string.")
      self$trios[[name]] <- trio_object
    },

    #' @description
    # Add a mapping function with metadata
    #' @param name A character string to name the mapping function.
    #' @param func A function that takes data as input and returns transformed data.
    #' @param input_description A character string describing the input data.
    #' @param output_description A character string describing the output data.
    #' @param example_usage An optional character string showing example usage of the function.
    add_mapping_function = function(
      name,
      func,
      input_description,
      output_description,
      example_usage = NULL
    ) {
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

    #' @description
    # Apply a mapping function to data
    #' @param mapping_name A character string naming the mapping function to apply.
    #' @param data The data to which the mapping function will be applied.
    #' @return The transformed data after applying the mapping function.
    run_mapping = function(mapping_name, data) {
      if (!(mapping_name %in% names(self$mapping_functions))) {
        stop(paste0("Mapping function '", mapping_name, "' not found."))
      }
      func <- self$mapping_functions[[mapping_name]]$func
      return(func(data))
    },

    #' @description
    # Documentation getter for mapping function
    #' @param mapping_name A character string naming the mapping function.
    #' @return A list containing the input description, output description, and example usage.
    get_mapping_function_documentation = function(mapping_name) {
      if (!(mapping_name %in% names(self$mapping_functions))) {
        stop(paste0("Mapping function '", mapping_name, "' not found."))
      }
      return(self$mapping_functions[[mapping_name]]$doc)
    },

    #' @description
    # Print out names of all mapping functions so user can see
    #' available options
    #' @return A character vector of mapping function names.
    list_mapping_functions = function() {
      return(names(self$mapping_functions))
    },

    #' @description
    # Generate R Markdown vignette template
    #' @param output_path A character string specifying the path to save the vignette template.
    generate_vignette_template = function(
      output_path = "benchmark_study_template.Rmd"
    ) {
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
    },

    evaluate = function(trioName, mappingName, input) {
      if (!(trioName %in% names(self$trios))) {
        stop(paste0("Trio '", trioName, "' not found."))
      }
      if (!(mappingName %in% names(self$mapping_functions))) {
        stop(paste0("Mapping function '", mappingName, "' not found."))
      }
      trio <- self$trios[[trioName]]
      mapping_func <- self$mapping_functions[[mappingName]]$func
      trio$evaluate(mapping_func, input)
    }
  )
)
