#' BenchHubStudy Class
#' @description
#' This class manages a collection of benchmark trios and mapping functions.
#' It allows adding new trios, mapping functions, and running mappings on data.
#' @field name A character string to name the study.
#' @field trios A list to store benchmark trios.
#' @field mapping_functions A list to store mapping functions with metadata.
#' @export
BenchHubStudy <- R6Class(
  "BenchHubStudy",
  public = list(
    # Fields
    name = NULL,
    trios = list(),
    mapping_functions = list(),

    #` @description Create a new BenchHubStudy object
    #' @param name A character string to name the study.
    #' @param trios A list of Trio objects to initialize the study.
    initialize = function(name = NULL, trios = list()) {
      self$name <- name
      self$trios <- trios
      if (!length(trios) == 0 && !all(sapply(trios, inherits, "Trio"))) {
        stop("All trios must be Trio objects.")
      }
    },
    #' @description
    #' Add a new trio to the study
    #' @param name A character string to name the trio.
    #' @param trio_object A Trio object to be added.
    add_trio = function(name, trio_object) {
      if (!is.character(name)) {
        stop("Name must be a character string.")
      }
      self$trios[[name]] <- trio_object
    },

    #' @description
    #' Add a mapping function with metadata
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
      if (!is.function(func)) {
        stop("Mapping function must be a function.")
      }
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
    #' Apply a mapping function to data
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
    #' Documentation getter for mapping function
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
    #' Generate R Markdown vignette template
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

    #' @description
    #' Evaluate a trio with input data
    #' @param trioName A character string naming the trio to evaluate.
    #' @param input The input data to evaluate the trio against.
    #' @return The evaluation result from the trio.
    evaluate = function(trioName, input) {
      if (!(trioName %in% names(self$trios))) {
        stop(paste0("Trio '", trioName, "' not found."))
      }
      trio <- self$trios[[trioName]]
      trio$evaluate(input)
    },
    #' @description
    #' Write the BenchHubStudy metadata to Curated Trio Datasets sheet.
    writeBenchHubStudy = function() {
      if (!curl::has_internet()) {
        cli::cli_warn(c(
          "Couldn't write to Curated Trio Datasets.",
          "Check your internet connection and try again."
        ))
        return(NULL)
      }

      # check if GITHUB_PAT is set and ask the user to set it if not
      if (is.null(Sys.getenv("GITHUB_PAT"))) {
        cli::cli_inform(c(
          "The GITHUB_PAT environment variable is not set.",
          "Please set it to your GitHub personal access token with gist access."
        ))
        if (interactive()) {
          set_github_pat <- utils::askYesNo(
            "Do you want to set the GITHUB_PAT environment variable?"
          )
          if (set_github_pat) {
            pat <- readline("Enter your GitHub personal access token: ")
            Sys.setenv(GITHUB_PAT = pat)
          } else {
            cli::cli_abort(c(
              "The GITHUB_PAT environment variable is not set.",
              "i" = "Please set it to your GitHub personal access token with gist access."
            ))
          }
        } else {
          cli::cli_abort(c(
            "This function must be run interactively.",
          ))
        }
      }

      # Read existing studies
      studies <- googlesheets4::read_sheet(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        sheet = "Studies"
      )

      # Calculate the next studyID
      if (!"studyID" %in% names(studies)) {
        studyID <- "0001"
      } else {
        studyID <- formatC(
          max(as.integer(studies$studyID), na.rm = TRUE) + 1,
          width = 4,
          flag = "0"
        )
      }

      # Check if the name is already in the studies
      if (name %in% studies$name) {
        cli::cli_abort(c(
          "The study name {.val {name}} is already in the studies sheet.",
          "i" = "Please choose a different name."
        ))
      }

      # Prompt for description if not set
      if (is.null(self$description)) {
        self$description <- readline(
          prompt = "Please provide a description for the study: "
        )
      }

      # Prompt for version
      version <- readline(
        prompt = "Enter the version for the study (e.g., 1.0.0): "
      )

      studyType <- studyTypes[studyType]

      # Prompt for related datasets (comma-separated)
      relatedDatasets <- readline(
        prompt = "Enter related dataset IDs (comma-separated, or leave blank): "
      )

      # Optionally, upload study protocol or code as a gist
      protocolText <- NULL
      uploadProtocol <- utils::askYesNo(
        "Do you want to upload a study protocol or code as a GitHub Gist?"
      )
      gist_url <- ""
      if (uploadProtocol) {
        protocolFile <- readline("Enter the path to the protocol/code file: ")
        if (file.exists(protocolFile)) {
          protocolText <- readLines(protocolFile)
          gist <- gistr::gist_create(
            code = protocolText,
            description = paste0("Protocol for BenchHubStudy ", name),
            public = TRUE,
            filename = basename(protocolFile)
          )
          gist_url <- gist$html_url
        } else {
          cli::cli_warn("File not found. Skipping protocol upload.")
        }
      }

      # Write to the Studies sheet
      googlesheets4::sheet_append(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        data = data.frame(
          studyID = studyID,
          name = name,
          description = self$description,
          version = version,
          studyType = studyType,
          relatedDatasets = relatedDatasets,
          protocol_gist = gist_url,
          validated = FALSE,
          stringsAsFactors = FALSE
        ),
        sheet = "Studies"
      )

      cli::cli_inform(c(
        "Added the study to the Curated Trio Datasets sheet.",
        "i" = paste0(
          "Please check the details at ",
          "{.href [this link](https://docs.google.com/spreadsheets/d/1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY/)}"
        )
      ))
    },

    #' @description
    #' Print method to display key information about the BenchHubStudy object.
    print = function() {
      msg <- cli::cli_fmt({
        cli::cli_h1("BenchHub Study")

        # Basic information
        cli::cli_h3("Study Information")
        if (!is.null(self$name)) {
          cli::cli_text("{.strong Name}: {.val {self$name}}")
        }
        if (!is.null(self$description)) {
          cli::cli_text("{.strong Description}: {.val {self$description}}")
        }

        # Trios information
        cli::cli_h3("Trios")
        cli::cli_text("{.strong Number of Trios}: {.val {length(self$trios)}}")
        if (length(self$trios) > 0) {
          trio_names <- sapply(self$trios, function(t) t$name)
          trio_names <- trio_names[!sapply(trio_names, is.null)]
          if (length(trio_names) > 0) {
            cli::cli_text("{.strong Trio Names}: {.val {paste(trio_names, collapse = ', ')}}")
          }
        }

        # Mapping functions
        cli::cli_h3("Mapping Functions")
        cli::cli_text(
          "{.strong Number of Mapping Functions}: {.val {length(self$mapping_functions)}}"
        )
        if (length(self$mapping_functions) > 0) {
          cli::cli_text(
            "{.strong Function Names}: {.val {paste(names(self$mapping_functions), collapse = ', ')}}"
          )
        }
      })

      cat(msg, sep = "\n")
    }
  )
)
