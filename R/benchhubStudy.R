#' BenchHubStudy Class
#' @description
#' This class manages a collection of benchmark trios and mapping functions.
#' It allows adding new trios, mapping functions, and running mappings on data.
#' @field name A character string to name the study.
#' @field trios A list to store benchmark trios.
#' @field mappingFunctions A list to store mapping functions with metadata.
#' @field description A character string describing the study.
#' @export
BenchHubStudy <- R6Class(
  "BenchHubStudy",
  public = list(
    # Fields
    name = NULL,
    trios = list(),
    description = NULL,
    mappingFunctions = list(),

    #` @description Create a new BenchHubStudy object
    #' @param name A character string to name the study. If fetchFromCtd is TRUE, this name will be used to fetch the study from Curated Trio Datasets.
    #' @param trios A list of Trio objects to initialize the study.
    #' @param fetchFromCtd Logical indicating whether to fetch study details from Curated Trio Datasets.
    #' @param version Optional integer specifying which version of the study to fetch (when fetchFromCtd is TRUE).
    initialize = function(name = NULL, trios = list(), fetchFromCtd = FALSE, version = NULL) {
      if (fetchFromCtd && !is.null(name)) {
        # Read existing studies from the sheet
        studies <- googlesheets4::read_sheet(
          ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
          sheet = "Studies"
        )
        
        # Find the study by name
        studyRows <- studies$studyName == name
        if (!any(studyRows)) {
          stop(paste0("Study '", name, "' not found in Curated Trio Datasets."))
        }
        
        # If version is specified, filter for that version
        if (!is.null(version)) {
          studyRows <- studyRows & studies$version == version
          if (!any(studyRows)) {
            stop(paste0("Version ", version, " of study '", name, "' not found."))
          }
        } else {
          # If no version specified, use the latest
          latestVersion <- max(studies$version[studyRows])
          studyRows <- studyRows & studies$version == latestVersion
        }
        
        studyData <- studies[studyRows, ][1,]
        self$name <- study_data$studyName
        self$description <- study_data$description
        
        # Parse and load related trios
        if (!is.na(studyData$relatedTrios) && studyData$relatedTrios != "") {
          trioNames <- strsplit(studyData$relatedTrios, ":")[[1]]
          self$trios <- list()
          for (trioName in trioNames) {
            # Create new Trio object and add it to the list
            trio <- Trio$new(trioName, cachePath = TRUE)
            self$addTrio(trio)
          }
        }
      } else {
        self$name <- name
        self$trios <- trios
        if (!length(trios) == 0 && !all(sapply(trios, inherits, "Trio"))) {
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
      self$mappingFunctions[[name]] <- list(
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
        stop(paste0("Mapping function '", mappingName, "' not found."))
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
        stop(paste0("Mapping function '", mappingName, "' not found."))
      }
      return(self$mappingFunctions[[mappingName]]$doc)
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
    generate_vignette_template = function(
      outputPath = "benchmark_study_template.Rmd"
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

      # check that all trios with the study are available on Curated Trio Datasets
      if (length(self$trios) < 2) {
        cli::cli_abort(c(
          "Insufficient in the study.",
          "Please add at least three trios before writing the study."
        ))
      } else if (!all(sapply(self$trios, inherits, "Trio"))) {
        cli::cli_abort(c(
          "All trios must be Trio objects.",
          "Please check the trios in the study."
        ))
      } else if (any(sapply(self$trios, function(trio) is.null(trio$name)))) {
        unnamed <- paste0(
          sapply(self$trios, function(trio) trio$name[is.null(trio$name)]),
          collapse = ", "
        )
        cli::cli_abort(c(
          "All trios must be available on Curated Trio Datasets.",
          "Please upload for each trio before writing the study.",
          "You can use `Trio$writeCTD(name)` to upload a trio.",
          "Please upload the following trios: {unnamed}",
        ))
      }

      # check if GITHUB_PAT is set and ask the user to set it if not
      if (Sys.getenv("GITHUB_PAT") == "") {
        cli::cli_inform(c(
          "The GITHUB_PAT environment variable is not set.",
          "Please set it to your GitHub personal access token with gist access."
        ))
        if (interactive()) {
          setGithubPat <- utils::askYesNo(
            "Do you want to set the GITHUB_PAT environment variable?"
          )
          if (setGithubPat) {
            pat <- readline("Enter your GitHub personal access token: ")
            Sys.setenv(GITHUB_PAT = pat)
          } else {
            cli::cli_abort(c(
              "The GITHUB_PAT environment variable is not set.",
              "Please set it to your GitHub personal access token with gist access."
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
        previous_version <- studies$version[studies$studyName == self$name]
        if (length(previous_version) == 0) {
          cli::cli_abort(c(
            "No previous version found for the study `{self$name}`.",
            "Please check the Curated Trio Datasets sheet."
          ))
        } else {
          previous_version <- as.integer(max(previous_version, na.rm = TRUE))
          version <- previous_version + 1
        }
        type <- "update"
      } else {
        version <- 1
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
          existing_description <- studies$description[
            studies$studyName == self$name
          ]
          if (length(existing_description) == 0) {
            cli::cli_abort(c(
              "No existing description found for the study `{self$name}`.",
              "Please provide a new description."
            ))
          } else {
            self$description <- existing_description[1]
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
          gist <- gistr::gist_create(
            code = protocolText,
            description = paste0("Protocol for BenchHubStudy ", self$name),
            public = TRUE,
            filename = basename(protocolFile)
          )
          gistUrl <- gist$html_url
        } else {
          cli::cli_abort("File not found. Please check the path and try again.")
        }
      }

      # Write to the Studies sheet
      googlesheets4::sheet_append(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        data = data.frame(
          studyID = studyID,
          studyName = self$name,
          version = version,
          description = self$description,
          type = type,
          nTrios = length(self$trios),
          relatedTrios = relatedTrios,
          protocolGist = gistUrl,
          mappingFunctions = "", #TODO: Add mapping functions if needed
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
