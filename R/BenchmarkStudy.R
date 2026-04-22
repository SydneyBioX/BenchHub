#' BenchmarkStudy Class
#' @description
#' This class manages a collection of benchmark trios and mapping functions.
#' It allows adding new trios, mapping functions, and running mappings on data.
#' @field name A character string to name the study.
#' @field trios A list to store benchmark trios.
#' @field mappingFunctions A list to store mapping functions with metadata.
#' @field description A character string describing the study.
#' @field version Integer specifying the version of the study.
#' @examples
#' study <- BenchmarkStudy$new(name = "example_study")
#' study$description <- "A small example benchmark study."
#' study$name
#' @export
#' @return A \code{BenchmarkStudy} object.
BenchmarkStudy <- R6Class(
  "BenchmarkStudy",
  public = list(
    # Fields
    name = NULL,
    trios = list(),
    description = NULL,
    mappingFunctions = list(),
    version = NULL,

    #` @description Create a new BenchmarkStudy object
    #' @param name A character string to name the study. If fetchFromCtd is TRUE, this name will be used to fetch the study from the submission database.
    #' @param trios A list of Trio objects to initialize the study.
    #' @param fetchFromCtd Logical indicating whether to fetch study details from the submission database.
    #' @param version Optional integer specifying which version of the study to fetch (when fetchFromCtd is TRUE).
    initialize = function(
      name = NULL,
      trios = list(),
      fetchFromCtd = FALSE,
      version = NULL
    ) {
      if (fetchFromCtd && !is.null(name)) {
        loaded_study <- downloadSubmissionStudy(
          name = name,
          version = version,
          cachePath = tempdir()
        )
        self$name <- loaded_study$name
        self$trios <- loaded_study$trios
        self$description <- loaded_study$description
        self$mappingFunctions <- loaded_study$mappingFunctions
        self$version <- loaded_study$version
      } else {
        self$name <- name
        self$trios <- trios
        if (!length(trios) == 0 && !all(vapply(trios, inherits, FUN.VALUE = logical(1), "Trio"))) {
          stop("All trios must be Trio objects.")
        }
      }
    },
    #' @description
    #' Add a new trio to the study
    #' @param trioObject A Trio object to be added.
    addTrio = function(trioObject) {
      self$trios[[length(self$trios) + 1]] <- trioObject
    },

    #' @description
    #' Add a mapping function with metadata
    #' @param name A character string to name the mapping function.
    #' @param func A function that takes data as input and returns transformed data.
    #' @param inputDescription A character string describing the input data.
    #' @param outputDescription A character string describing the output data.
    #' @param exampleUsage An optional character string showing example usage of the function.
    addMappingFunction = function(
      name,
      func,
      inputDescription,
      outputDescription,
      exampleUsage = NULL
    ) {
      if (!is.function(func)) {
        stop("Mapping function must be a function.")
      }
      # Convert name to valid R variable name
      validName <- make.names(name)
      if (validName != name) {
        cli::cli_warn(c(
          "Mapping function name has been modified to be a valid R variable name.",
          "i" = "Original name: {name}",
          "i" = "Modified name: {validName}"
        ))
      }
      self$mappingFunctions[[validName]] <- list(
        func = func,
        doc = list(
          inputDescription = inputDescription,
          outputDescription = outputDescription,
          exampleUsage = exampleUsage
        )
      )
    },

    #' @description
    #' Apply a mapping function to data
    #' @param mappingName A character string naming the mapping function to apply.
    #' @param data The data to which the mapping function will be applied.
    #' @return The transformed data after applying the mapping function.
    runMapping = function(mappingName, data) {
      if (!(mappingName %in% names(self$mappingFunctions))) {
        stop("Mapping function '", mappingName, "' not found.")
      }
      func <- self$mappingFunctions[[mappingName]]$func
      return(func(data))
    },

    #' @description
    #' Documentation getter for mapping function
    #' @param mappingName A character string naming the mapping function.
    #' @return A list containing the input description, output description, and example usage.
    getMappingFunctionDocumentation = function(mappingName) {
      if (!(mappingName %in% names(self$mappingFunctions))) {
        stop("Mapping function '", mappingName, "' not found.")
      }
      return(self$mappingFunctions[[mappingName]]$doc)
    },
    
    #' @description
    #' Print the documentation for a mapping function in a human-readable format.
    #' @param mappingName A character string naming the mapping function.
    #' @return Prints inputDescription, outputDescription, exampleUsage
    printMappingFunctionDocumentation = function(mappingName) {
      
      if (!(mappingName %in% names(self$mappingFunctions))) {
        stop("Mapping function '", mappingName, "' not found.")
      }
      
      doc <- self$getMappingFunctionDocumentation(mappingName)
      cat("Input:\n  ", doc$inputDescription, "\n", sep = "")
      cat("Output:\n  ", doc$outputDescription, "\n", sep = "")
      if (!is.null(doc$exampleUsage)) {
        cat("Example:\n")
        cat(doc$exampleUsage, "\n", sep = "")
      }
      invisible(doc)
    },
    

    #' @description
    # Print out names of all mapping functions so user can see
    #' available options
    #' @return A character vector of mapping function names.
    listMappingFunctions = function() {
      return(names(self$mappingFunctions))
    },

    #' @description
    #' Generate R Markdown vignette template
    #' @param outputPath A character string specifying the path to save the vignette template.
    generateVignetteTemplate = function(
      outputPath = "benchmark_study_template.Rmd"
    ) {
      vignetteText <- "
---
title: \"Benchmark Study Report\"
output: html_document
---

# Introduction

Describe the benchmark task and dataset.


"
      writeLines(vignetteText, con = outputPath)
      message("Vignette template written to:", outputPath)
    },

    #' @description
    #' Evaluate a trio with input data
    #' @param trioName A character string naming the trio to evaluate.
    #' @param input The input data to evaluate the trio against.
    #' @return The evaluation result from the trio.
    evaluate = function(trioName, input) {
      # Find trio by name field
      trioIndex <- which(vapply(self$trios, function(t) t$name == trioName, FUN.VALUE = logical(1)))
      if (length(trioIndex) == 0) {
        stop("Trio '", trioName, "' not found.")
      }
      trio <- self$trios[[trioIndex[1]]] # Use first match if multiple exist
      trio$evaluate(input)
    },
    #' @description
    #' Write the BenchmarkStudy metadata to Curated Trio Datasets sheet.
    writeBenchmarkStudy = function() {
      if (!curl::has_internet()) {
        cli::cli_warn(c(
          "Couldn't write to Curated Trio Datasets.",
          "Check your internet connection and try again."
        ))
        return(NULL)
      }

      # check that all trios with the study are available on Curated Trio Datasets
      if (length(self$trios) < 2) {
        cli::cli_abort(c(
          "Insufficient in the study.",
          "Please add at least three trios before writing the study."
        ))
      } else if (!all(vapply(self$trios, inherits, "Trio", FUN.VALUE = logical(1)))) {
        cli::cli_abort(c(
          "All trios must be Trio objects.",
          "Please check the trios in the study."
        ))
      } else if (any(vapply(self$trios, function(trio) is.null(trio$name), FUN.VALUE = logical(1)))) {
        unnamed <- paste0(
          unlist(lapply(self$trios, function(trio) trio$name[is.null(trio$name)])),
          collapse = ", "
        )
        cli::cli_abort(c(
          "All trios must be available on Curated Trio Datasets.",
          "Please upload for each trio before writing the study.",
          "You can use `Trio$writeCTD(name)` to upload a trio.",
          "Please upload the following trios: {unnamed}",
        ))
      }

      # check if GITHUB_PAT is set in environment or needs to be obtained
      githubPat <- Sys.getenv("GITHUB_PAT")
      if (githubPat == "") {
        cli::cli_inform(c(
          "The GITHUB_PAT environment variable is not set.",
          "Please set it to your GitHub personal access token with gist access."
        ))
        if (interactive()) {
          setGithubPat <- utils::askYesNo(
            "Do you want to provide your GitHub personal access token?"
          )
          if (setGithubPat) {
            githubPat <- readline("Enter your GitHub personal access token: ")
          } else {
            cli::cli_abort(c(
              "The GITHUB_PAT is required to write the study.",
              "Please set the GITHUB_PAT environment variable or provide it when prompted."
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
      if (nrow(studies) == 0) {
        studyID <- "0001"
      } else {
        studyID <- formatC(
          max(as.integer(studies$studyID), na.rm = TRUE) + 1,
          width = 4,
          flag = "0"
        )
      }

      # Check if the name is already in the studies
      if (self$name %in% studies$studyName) {
        # ask the user if they are contributing a new verision of the study
        cli::cli_inform(c(
          "The study name `{self$name}` already exists in the Curated Trio Datasets.",
          "i" = "Are you contributing a new version of this study?"
        ))
        type <- utils::askYesNo(
          "Is this a new version of an existing study?"
        )
        if (type) {
          type <- "version"
        } else {
          cli::cli_abort(c(
            "The study name `{self$name}` already exists.",
            "Please choose a different name or update the existing study."
          ))
        }

        # get the previous study version and increment it
        # NOTE: versions are whole integers
        previousVersion <- studies$version[studies$studyName == self$name]
        if (length(previousVersion) == 0) {
          cli::cli_abort(c(
            "No previous version found for the study `{self$name}`.",
            "Please check the Curated Trio Datasets sheet."
          ))
        } else {
          previousVersion <- as.integer(max(previousVersion, na.rm = TRUE))
          version <- previousVersion + 1
          self$version <- version
        }
        type <- "update"
      } else {
        version <- 1
        self$version <- version
        type <- "new"
      }

      # Prompt for description if not set
      if (is.null(self$description)) {
        if (type == "new") {
          self$description <- readline(
            prompt = "Please provide a description for the study: "
          )
        } else {
          # If updating, use the existing description
          existingDescription <- studies$description[
            studies$studyName == self$name
          ]
          if (length(existingDescription) == 0) {
            cli::cli_abort(c(
              "No existing description found for the study `{self$name}`.",
              "Please provide a new description."
            ))
          } else {
            self$description <- existingDescription[1]
          }
        }
      }

      # Prompt for related datasets (comma-separated)
      relatedTrios <- paste0(
        lapply(
          self$trios,
          function(trio) {
            trio$name
          }
        ),
        collapse = ":"
      )

      # Optionally, upload study protocol or code as a gist
      protocolText <- NULL
      uploadProtocol <- utils::askYesNo(
        "Do you want to upload a study protocol or code as a GitHub Gist?"
      )
      gistUrl <- ""
      protocolText <- ""
      if (uploadProtocol) {
        protocolFile <- readline("Enter the path to the protocol/code file: ")
        if (file.exists(protocolFile)) {
          protocolText <- readLines(protocolFile)
          gist <- createGist(
            content = protocolText,
            filename = basename(protocolFile),
            description = paste0("Protocol for BenchmarkStudy ", self$name),
            pat = githubPat
          )
          gistUrl <- gist$html_url
        } else {
          cli::cli_abort("File not found. Please check the path and try again.")
        }
      }

      # Upload mapping functions as a gist if they exist
      mappingFunctionsGistUrl <- ""
      if (length(self$mappingFunctions) > 0) {
        cli::cli_inform("Uploading mapping functions to GitHub Gist...")

        # Create R code containing all mapping functions
        mappingFunctionsCode <- character()
        for (funcName in names(self$mappingFunctions)) {
          func <- self$mappingFunctions[[funcName]]
          # Add function documentation as comments
          mappingFunctionsCode <- c(
            mappingFunctionsCode,
            paste0("# Function: ", funcName),
            paste0("# Input: ", func$doc$inputDescription),
            paste0("# Output: ", func$doc$outputDescription),
            if (!is.null(func$doc$exampleUsage)) {
              paste0("# Example: ", func$doc$exampleUsage)
            },
            "",
            paste0(
              funcName,
              " <- ",
              paste(deparse(func$func), collapse = "\n")
            ),
            "\n"
          )
        }

        # Create the gist
        mappingGist <- createGist(
          content = mappingFunctionsCode,
          filename = paste0(
            "mapping_functions_",
            self$name,
            "_v",
            self$version,
            ".R"
          ),
          description = paste0(
            "Mapping Functions for BenchmarkStudy ",
            self$name,
            " v",
            self$version
          ),
          pat = githubPat
        )
        mappingFunctionsGistUrl <- mappingGist$html_url
      }

      # Write to the Studies sheet
      googlesheets4::sheet_append(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        data = tibble::tibble(
          studyID = studyID,
          studyName = self$name,
          version = version,
          description = self$description,
          type = type,
          nTrios = length(self$trios),
          relatedTrios = relatedTrios,
          protocolGist = gistUrl,
          mappingFunctions = mappingFunctionsGistUrl,
          validated = FALSE,
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
    #' Print method to display key information about the BenchmarkStudy object.
    print = function() {
      msg <- cli::cli_fmt({
        cli::cli_h1("BenchHub Study")

        # Basic information
        cli::cli_h3("Study Information")
        if (!is.null(self$name)) {
          cli::cli_text("{.strong Name}: {.val {self$name}}")
          if (!is.null(self$version)) {
            cli::cli_text("{.strong Version}: {.val {self$version}}")
          }
        }
        if (!is.null(self$description)) {
          cli::cli_text("{.strong Description}: {.val {self$description}}")
        }

        # Trios information
        cli::cli_h3("Trios")
        cli::cli_text("{.strong Number of Trios}: {.val {length(self$trios)}}")
        if (length(self$trios) > 0) {
          trioNames <- vapply(self$trios, function(t) t$name, FUN.VALUE = character(1))
          trioNames <- trioNames[!vapply(trioNames, is.null, FUN.VALUE = logical(1))]
          if (length(trioNames) > 0) {
            cli::cli_text(
              "{.strong Trio Names}: {.val {paste(trioNames, collapse = ', ')}}"
            )
          }
        }

        # Mapping functions
        cli::cli_h3("Mapping Functions")
        cli::cli_text(
          "{.strong Number of Mapping Functions}: {.val {length(self$mappingFunctions)}}"
        )
        if (length(self$mappingFunctions) > 0) {
          cli::cli_text(
            "{.strong Function Names}: {.val {paste(names(self$mappingFunctions), collapse = ', ')}}"
          )
        }
      })

      cat(msg, sep = "\n")
    }
  )
)

private_load_study_mapping_functions <- function(study, gist_url) {
  if (is.na(gist_url) || !nzchar(gist_url)) {
    return(invisible(study))
  }

  cli::cli_inform("Loading mapping functions from gist...")

  tryCatch(
    {
      temp_file <- downloadGist(gist_url)
      on.exit(unlink(temp_file), add = TRUE)
      mapping_code <- readLines(temp_file)

      temp_env <- new.env()
      eval(parse(text = paste(mapping_code, collapse = "\n")), envir = temp_env)

      current_func <- NULL
      current_doc <- list(input = NULL, output = NULL, example = NULL)
      current_field <- NULL

      add_current_function <- function() {
        if (is.null(current_func)) {
          return(invisible(NULL))
        }

        func_name <- ls(
          envir = temp_env,
          pattern = paste0("^", current_func, "$")
        )
        if (length(func_name) > 0) {
          study$addMappingFunction(
            name = current_func,
            func = get(func_name, envir = temp_env),
            inputDescription = current_doc$input,
            outputDescription = current_doc$output,
            exampleUsage = current_doc$example
          )
        }

        invisible(NULL)
      }

      for (line in mapping_code) {
        trimmed <- trimws(line)

        if (startsWith(trimmed, "# Function:")) {
          add_current_function()
          current_func <- sub("^# Function:\\s*", "", trimmed)
          current_doc <- list(input = NULL, output = NULL, example = NULL)
          current_field <- NULL
        } else if (startsWith(trimmed, "# Input:")) {
          current_doc$input <- sub("^# Input:\\s*", "", trimmed)
          current_field <- "input"
        } else if (startsWith(trimmed, "# Output:")) {
          current_doc$output <- sub("^# Output:\\s*", "", trimmed)
          current_field <- "output"
        } else if (startsWith(trimmed, "# Example:")) {
          current_doc$example <- sub("^# Example:\\s*", "", trimmed)
          current_field <- "example"
        } else if (startsWith(trimmed, "#")) {
          continuation <- sub("^#\\s?", "", trimmed)
          if (!is.null(current_field) && nzchar(continuation)) {
            previous <- current_doc[[current_field]]
            if (is.null(previous) || !nzchar(previous)) {
              current_doc[[current_field]] <- continuation
            } else {
              current_doc[[current_field]] <- paste(previous, continuation, sep = "\n")
            }
          }
        } else {
          current_field <- NULL
        }
      }

      add_current_function()

      cli::cli_inform(c(
        "v" = "Successfully loaded mapping functions",
        "i" = "Loaded {length(study$mappingFunctions)} functions"
      ))
    },
    error = function(e) {
      cli::cli_warn(c(
        "Failed to load mapping functions from gist",
        "x" = "Error: {conditionMessage(e)}"
      ))
    }
  )

  invisible(study)
}
#' List the curated Trio studies
#' @param name_filter
#'   A string to filter studies by name (case-insensitive partial match)
#' @return A data frame with the study names and IDs.
#' @export
listCuratedTrioStudies <- function(
  name_filter = NULL
) {
  if (!curl::has_internet()) {
    cli::cli_warn(c(
      "Couldn't list Curated Trio Studies.",
      "Check your internet connection and try again."
    ))
    return(NULL)
  }
  studies <- read_public_sheet(
    ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
    sheet = "Studies"
  ) |>
    dplyr::select(studyName, studyID, version, description) |>
    dplyr::arrange(studyName)

  # Apply filters if provided
  if (!is.null(name_filter)) {
    studies <- studies |>
      dplyr::filter(grepl(name_filter, studyName, ignore.case = TRUE))
  }

  studies
}
