# Silence check notes about R6 class
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
NULL

#' A Trio object
#' @description An object containing a dataset and methods for evaluating
#'   analytical tasks against ground truths for the dataset.
#' @field data The data
#' @field evidence The supporting evidence for the data
#' @field metrics The metric for evaluating tasks against the gold standards
#' @field cachePath The path to the data cache
#' @field dataSource The data repository that the data were retrieved from
#' @field dataSourceID The dataset ID for `dataSource`
#' @field evidenceSource The data repository that the supporting evidence was
#'   retrieved from.
#' @field evidenceSourceID The dataset ID for `evidenceSource`.
#' @field splitIndices Indices for cross-validation
#' @field splitSeed The seed used to generate the split indices
#' @field verbose Set the verbosity of Trio. Defaults to `FALSE`.
#' @field description A description of the dataset.
#' @field name The name of the Trio object, as defined in Curated Trio Datasets.
#'
#' @examples
#' trio <- Trio$new("figshare:26054188/47112109", cachePath = tempdir())
#' @return A Trio object
#' @export
#' @importFrom googlesheets4 read_sheet
Trio <- R6::R6Class(
  "Trio",
  public = list(
    cachePath = NULL,
    data = NULL,
    evidence = list(),
    metrics = list(),
    dataSource = NULL,
    dataSourceID = NULL,
    evidenceSource = list(),
    evidenceSourceID = list(),
    splitIndices = NULL,
    splitSeed = NULL,
    verbose = FALSE,
    description = NULL,
    name = NULL,

    #' @description
    #' Create a Trio object
    #' @param datasetID
    #'   A string specifying a dataset, either a name from curated-trio-data or
    #'   a format string of the form `source`:`source_id`.
    #' @param data An object to use as the Trio dataset.
    #' @param dataLoader
    #'   A custom loading function that takes the path of a downloaded file and
    #'   returns a single dataset, ready to be used in evaluation tasks.
    #' @param evidenceID
    #'   If `datasetID` is not an ID from the curated trio datasets spreadsheet,
    #'   then a format string of the form `source`:`source_id` indicating the file to
    #'   obtain the supporting evidence from.
    #' @param evidence
    #'   A named list of lists. The top-level list is named by task type. The lower-level list
    #'   is of length-two and named \code{"evidence"} and \code{"metrics"}. The \code{"evidence"}
    #'   component has supporting evidence and the \code{"metrics"} component has a character vector
    #'   of metric names (corresponding to the names of the list provided to the \code{metrics} parameter).
    #' @param evidenceColumns
    #'   If `evidenceID` is not `NULL`, then the columns of the table containing
    #'   the supporting evidence.
    #' @param evidenceLoader
    #'   Alternative to `evidence` and `evidenceColumns`. Extract the evidence in a flexible way.
    #' @param task
    #'   If `evidenceColumns` or `evidenceLoader` specified, a character vector of length 1 naming the task the evidence is for.
    #' @param metrics A named list of metric functions.
    #' @param cachePath The path to the data cache
    #' @param verbose Set the verbosity of Trio. Defaults to `FALSE`.
    #' @param description A description of the dataset.
    initialize = function(
      datasetID = NULL,
      data = NULL,
      dataLoader = NULL,
      evidenceID = NULL,
      evidence = NULL,
      evidenceColumns = NULL,
      evidenceLoader = NULL,
      task = NULL,
      metrics = NULL,
      cachePath = FALSE,
      verbose = FALSE
    ) {
      if (!interactive()) {
        googlesheets4::gs4_deauth()
      }
      if (!is.logical(verbose)) {
        cli::cli_abort(c(
          "The {.var verbose} parameter must be a {.cls logical}."
        ))
      }
      self$verbose <- verbose
      # if users have their own data without datasetID
      if (!is.null(data)) {
        if (interactive()) {
          self$description <- readline("Briefly describe the dataset: ")
          self$dataSourceID <- ifelse(
            is.null(datasetID) || datasetID == "",
            readline("Name the dataset: "),
            datasetID
          )
        } else {
          self$dataSourceID <- ifelse(
            is.null(datasetID) || datasetID == "",
            "local_data",
            datasetID
          )
        }
        self$data <- data
        # If any evidence is missing sample IDs.
        missingNames <- sapply(lapply(evidence, "[[", "evidence"), function(evidenceData)
        {
          if(isTabular(evidenceData)) is.null(rownames(evidenceData)) 
          else is.null(names(evidenceData))
        })
        if(any(missingNames))
        {
          cli::cli_warn("No sample IDs found on evidence. Assuming same order as data and adding them.")
          evidence[missingNames] <- lapply(evidence, function(oneEvidence)
          {
            if(isTabular(oneEvidence$evidence)) rownames(oneEvidence$evidence) <- rownames(data) else names(oneEvidence$evidence) <- rownames(data)
            oneEvidence
          })
        }
        
        self$evidence <- evidence
        self$metrics <- metrics
        return(NULL)
      }

      if (is.null(datasetID) || datasetID == "") {
        if (!interactive()) {
          cli::cli_abort(
            paste0(
              "When Trio is initialised non-interactively, a",
              " {.val datasetID} must be specified."
            )
          )
        }
        # prompt users to input their own new datasetID
        datasetID <- readline(
          prompt = paste0(
            "If you don't have a Figshare/GEO/ExperimentHub datasetID",
            ", please provide a new datasetID: "
          )
        )
      }
      # parse user input and set dataSource and dataSourceID
      private$parseIDString(datasetID)

      self$cachePath <- getTrioCachePath(cachePath)
      self$data <- private$getData(
        self$dataSource,
        self$dataSourceID,
        self$cachePath,
        dataLoader
      )
      private$populateTrio(evidenceID, evidence, evidenceColumns, evidenceLoader, task, metrics)
    },

    #' @description
    #' Add supporting evidence to the Trio.
    #' @param name A string specifying the name of the supporting evidence.
    #' @param evidence
    #'   The supporting evidence. An object to be compared or a function to be run on
    #'   the data.
    #' @param metrics
    #'   A list of one or more metrics names used to compare gs with the input
    #'   to evaluate.
    #' @param args
    #'   A named list of parameters and values to be passed to the function.
    addEvidence = function(name, evidence, metrics, args = NULL) {
      if (name %in% names(self$evidence)) {
        cli::cli_warn(c(
          paste0(
            "Supporting evidence `{name}` is already present in this Trio,",
            " overwriting."
          )
        ))
      }

      if (methods::is(evidence, "function")) {
        # Assign a wrapper function that adds args applies to
        # self$data, returning the result.
        self$evidence[[name]] <- list(
          "evidence" = function(data) {
            do.call(evidence, append(list(data), args))
          },
          "metrics" = metrics
        )
      } else {
        if(isTabular(evidence))
        {
          if(is.null(rownames(evidence)))
          {
            cli::cli_warn("No sample IDs found on evidence. Assuming same order as data and adding them.")
            rownames(evidence) <- rownames(self$data)
          }
        } else {
          if(is.null(names(evidence)))
          {
            cli::cli_warn("No sample IDs found on evidence. Assuming same order as data and adding them.")
            names(evidence) <- rownames(self$data)
          }
        }
        self$evidence[[name]] <- list(
          "evidence" = evidence,
          "metrics" = metrics
        )
      }
    },

    #' @description
    #' Add a metric to the Trio.
    #' @param name A string specifying the name of the metric.
    #' @param metric
    #'   The metric. A function to be run on the input to evaluate to compare it
    #'   with the gold standard. Should be of the form f(x, y, ...). Where `x`
    #'   is the "truth" and `y` is the output to be evaluated. Otherwise input
    #'   a wrapper function of the desired metric.
    #' @param args
    #'   A named list of parameters and values to be passed to the function.
    addMetric = function(name, metric, args = NULL) {
      if (!methods::is(metric, "function")) {
        cli::cli_abort(c(
          paste0(
            "{.var metric} should be a {.cls function}, not a",
            " {.cls {class(metric)}}."
          )
        ))
      }
      if (name %in% names(self$metrics)) {
        cli::cli_warn(c(
          "A metric `{name}` is already present in this Trio, overwriting."
        ))
      }
      # Validate metric function signature
      # metric functions should follow this format (evidence, to_eval)
      metric_args <- names(formals(metric))
      if (length(metric_args) < 2) {
        cli::cli_abort(c(
          "Metric functions must have at least two arguments.",
          "i" = "The first two arguments should be {.var evidence} and {.var to_eval}."
        ))
      }

      self$metrics[[name]] <- function(evidence, to_eval) {
        do.call(metric, append(list(evidence, to_eval), args))
      }
    },

    #' @description
    #' Get metrics by supporting evidence name.
    #' @param evidenceName A string specifying the name of the supporting evidence.
    getMetrics = function(evidenceName) {
      if (!evidenceName %in% names(self$evidence)) {
        cli::cli_abort(c(
          "{.val {evidenceName} is not supporting evidence in this object.}",
          "i" = "Choose one of {.val {names(self$evidence)}}"
        ))
      }
      purrr::pluck(self$evidence, evidenceName, "metrics")
    },

    #' @description
    #' Get supporting evidence by name.
    #' @param name A string specifying the name of the supporting evidence.
    getEvidence = function(name) {
      if (length(self$evidence) == 0) {
        cli::cli_abort(c(
          "There is no supporting evidence in this Trio!",
          "i" = "Add some using {.code Trio$addEvidence(...)}."
        ))
      }
      if (!name %in% names(self$evidence)) {
        evidenceNames <- names(self$evidence)
        cli::cli_abort(c(
          "Supporting evidence {.val {name}} could not be found.",
          "i" = paste0(
            "Add it using {.code Trio$addEvidence(.)} or choose one of ",
            "{.val {evidenceNames}}"
          )
        ))
      }

      evidence <- self$evidence[[name]]$evidence

      if (!methods::is(evidence, "function")) {
        return(evidence)
      }

      evidence(self$data)
    },

    #' @description
    #' Evaluate against gold standards
    #' @param input A named list of objects to be evaluated against gold
    #'   standards.
    evaluate = function(input) {
      # check if the requested evidence is available
      evidenceAvail <- names(input) %in% names(self$evidence)

      # if input list contains no evidence names, check if first sub-list
      # contains evidence names and set separateMethods based on this
      if (all(!evidenceAvail)) {
        if (any(names(input[[1]]) %in% names(self$evidence))) {
          if (self$verbose) {
            cli::cli_inform(c(
              "Evidence names found in sublist.",
              "i" = "Evaluating as separate methods."
            ))
          }
          separateMethods <- TRUE
        }
      } else {
        separateMethods <- FALSE
      }

      # check if supporting evidence is available for each element of the input.
      if (separateMethods) {
        evalList <- lapply(input, self$evaluate)
        return(
          purrr::list_rbind(evalList, names_to = "method") %>%
            dplyr::select(datasetID, dplyr::everything())
        )
      } else {
        # if none of the evidence is available
        if (all(!evidenceAvail)) {
          evidenceNames <- names(self$evidence)
          cli::cli_abort(c(
            "None of the specified supporting evidence is available in the object.",
            "i" = paste0(
              "Add it using {.code Trio$addEvidence(.)} or choose",
              " from {.val {evidenceNames}}"
            )
          ))
        }

        # if some of the evidence is missing
        if (any(!evidenceAvail)) {
          unavail <- names(input)[!evidenceAvail]
          if (self$verbose) {
            cli::cli_inform(c(
              paste0(
                "Supporting evidence {.val {unavail}} from {.var input} ",
                "{?is/are} not available in this object. Passing through as ",
                "unevaluated benchmark data."
              ),
              "i" = paste0(
                "Evaluating the following:",
                " {.var {names(input)[evidenceAvail]}}"
              )
            ))
          }
        }

        # compute/retrieve supporting evidence
        evidence <- setNames(
          lapply(names(input[evidenceAvail]), self$getEvidence),
          names(input[evidenceAvail])
        )
        isComputed <- sapply(names(input[evidenceAvail]), function(ID) is.function(self$evidence[[ID]]$evidence))

        # get a list of metrics to compute for each gold standard in the data
        metrics <- setNames(
          lapply(names(input[evidenceAvail]), self$getMetrics),
          names(input[evidenceAvail])
        )

        # get a flat list of available metrics
        allMetrics <- metrics |>
          unlist() |>
          unique()

        # find metrics that are not available in the trio
        unavailMetrics <- allMetrics[!allMetrics %in% names(self$metrics)]

        if (length(unavailMetrics) == length(allMetrics)) {
          cli::cli_abort(c(
            paste0(
              "None of the metrics related to the supporting evidence being ",
              "evaluated are available in the object."
            ),
            "i" = "Add some of the following: {.val {allMetrics}}."
          ))
        }

        if (length(unavailMetrics) > 0) {
          cli::cli_warn(c(
            paste0(
              "{.val {unavailMetrics}} metric{?s} {?is/are} not available in",
              " the object."
            ),
            "They will be skipped during evaluation."
          ))

          # remove unavailable metrics from the nested list
          metrics <- lapply(
            metrics,
            \(evidenceMetrics) {
              Filter(\(x) !x %in% unavailMetrics, evidenceMetrics)
            }
          )
        }
 
        # subset evidence based on prediction's names
        if (!is.null(self$splitIndices)) {
            evidence <- mapply(function(oneEvidence, predictions, computed) {
            if(!computed) # data needs to match the order of predictions.
            {
                testIDs <- names(predictions)
                if (isTabular(oneEvidence)) {
                  return(oneEvidence[testIDs, , drop = FALSE])
                } else if (is.vector(oneEvidence) || is.factor(oneEvidence) || is.list(oneEvidence)) {
                  return(oneEvidence[testIDs])
                } else {
                  cli::cli_abort(c(
                    "Unsupported data type.",
                    "x" = paste0(
                      "Only vectors and tabular data are supported for",
                      " evidence subsetting."
                    ),
                    "i" = paste0(
                      "Try adding pre-subsetted evidence to the Trio for",
                      " evaluation."
                    )
                  ))
                }
            } else oneEvidence
          }, evidence, input[evidenceAvail], isComputed, SIMPLIFY = FALSE)
        }
        
        # compute each metric for each input
        res <- purrr::imap(input, function(to_eval, evidenceName) {
          if (is.null(metrics[[evidenceName]])) {
            return(to_eval)
          }
          res <- lapply(
            metrics[[evidenceName]],
            function(x) {
              if(is.function(self$evidence[[evidenceName]]$evidence))
                to_eval <- self$evidence[[evidenceName]]$evidence(to_eval)
              metric_res <- self$metrics[[x]](to_eval, evidence[[evidenceName]])
              if (length(metric_res) > 1) {
                cli::cli_abort(c(
                  "The result for the {.val {x}} metric is not a single value.",
                  "i" = paste0(
                    "Please ensure that all your metrics only output",
                    " a single value."
                  )
                ))
              }
              metric_res
            }
          )
          setNames(res, metrics[[evidenceName]])
        })
        purrr::map(names(res), function(metric_name) {
          metricValues <- res[[metric_name]]

          # Create a data frame for each metric
          tibble::tibble(
            datasetID = self$dataSourceID,
            evidence = metric_name,
            metric = names(metricValues),
            result = unlist(metricValues)
          )
        }) %>%
          purrr::list_rbind()
      }
    },

    #' @description
    #' Create cross-validation indices.
    #' @param y
    #'   A variable to use for stratified sampling (e.g. supporting evidence). If `stratify` is false, a
    #'   vector the length of the data.
    #' @param n_fold Number of folds. Defaults to `5L`.
    #' @param n_repeat Number of repeats. Defaults to `1L`.
    #' @param stratify If `TRUE`, uses stratified sampling. Defaults to `TRUE`.
    #' @param overwrite
    #'   If `TRUE`, overwrites the current split. Defaults to `FALSE`.
    #' @param seed
    #'   An optional seed for split generation. Defaults to `NULL`. If `NULL`,
    #'   the seed is set to the current time.
    #' @param ... Additional arguments passed to `splitTools::create_folds`.
    #' @importFrom splitTools create_folds
    #' @importFrom cli cli_inform
    #' @importFrom utils askYesNo
    split = function(
      y,
      n_fold = 5L,
      n_repeat = 1L,
      stratify = TRUE,
      seed = NULL,
      overwrite = FALSE,
      ...
    ) {
      # choose a seed if not provided
      if (is.null(seed)) {
        seed <- as.integer(Sys.time()) * sample(c(1, -1), 1)
      }

      if (!overwrite && !is.null(self$splitIndices)) {
        if (self$verbose) {
          cli::cli_inform(c(
            "Not overwriting, keeping the existing split indices.",
            "i" = "Use {.code trio$split(..., overwrite = TRUE)} to overwrite.",
            "i" = "To get current indices, access {.code trio$splitIndices}"
          ))
        }
        return(NULL)
      }

      # save split indices and seed
      self$splitSeed <- seed
      self$splitIndices <- splitTools::create_folds(
        y,
        k = n_fold,
        type = dplyr::if_else(stratify, "stratified", "basic"),
        m_rep = n_repeat,
        seed = seed,
        ...
      )
    },

    #' @description
    #' Print method to display key information about the Trio object.
    print = function() {
      data_str <- capture.output(str(self$data, max.level = 1))
      if (length(data_str) > 10) {
        data_str <- c(data_str[1:10], "... (truncated)")
      }
      data_str <- setNames(data_str, rep(" ", times = length(data_str)))
      split_ind <- ifelse(is.null(self$splitIndices), "None", "Available")

      msg <- cli::cli_fmt({
        cli::cli_h1("Trio Object")

        cli::cli_h3("Dataset")
        cli::cli_text("{.strong Dataset Details}:")
        cli::cli_bullets(data_str)
        cli::cli_text("{.strong Data Source}: {.val {self$dataSource}}")
        cli::cli_text("{.strong Dataset ID}: {.val {self$dataSourceID}}")
        cli::cli_text("{.strong Cache Path}: {.val {self$cachePath}}")
        cli::cli_text("{.strong Split Indices}: {.val {split_ind}}")

        cli::cli_h3("Supporting Evidence")
        cli::cli_text(
          "{.strong Number of Supporting Evidence}: {.val {length(self$evidence)}}"
        )
        cli::cli_text(
          "{.strong Names of Supporting Evidence}: {.val {names(self$evidence)}}"
        )

        cli::cli_h3("Metrics")
        cli::cli_text(
          "{.strong Number of Metrics}: {.val {length(self$metrics)}}"
        )
        cli::cli_text(
          "{.strong Names of Metrics}: {.val {names(self$metrics)}}"
        )

        if (!is.null(self$splitIndices)) {
          cli::cli_h3("CV Split Indices")
          cli::cli_text("{.strong Seed}: {.val {self$splitSeed}}")
          cli::cli_text(
            "{.strong Number of Folds}: {.val {length(self$splitIndices)}}"
          )
          cli::cli_text(
            paste0(
              "{.strong Number of Repeats}:",
              " {.val {length(self$splitIndices[[1]])}}"
            )
          )
        }
      })

      cat(msg, sep = "\n")
    },

    #' @description
    #' Write the Trio Metadata to Curated Trio Datasets sheet.
    #' @param name The name of the dataset to be added.
    #' @param githubPat Optional GitHub Personal Access Token. If not provided and not set in environment, will prompt user.
    #' @param description Optional description of the dataset. If not provided and not set, will prompt user.
    #' @param figshareUrl Optional URL to the Figshare dataset. If not provided, will prompt user.
    #' @param datasetFileName Optional name of the dataset file in Figshare. If not provided, will prompt user for selection.
    #' @param evidenceFileName Optional name of the evidence file in Figshare. If not provided, will prompt user for selection.
    #' @param dataType Optional type of data. Must be one of: "omics", "clinical", "spatial", "other". If not provided, will prompt user.
    #' @param skipMd5Check Optional boolean to skip MD5 verification. Defaults to FALSE.
    writeCTD = function(
      name,
      githubPat = NULL,
      description = NULL,
      figshareUrl = NULL,
      datasetFileName = NULL,
      evidenceFileName = NULL,
      dataType = NULL,
      skipMd5Check = FALSE
    ) {
      # Initialize state list
      state <- list(
        name = name,
        md5 = "",
        save = FALSE,
        saveEvidence = FALSE,
        evidenceFilename = paste0(name, "_evidence.rds"),
        evidenceMd5 = "",
        dataSource = self$dataSource,
        dataSourceID = self$dataSourceID,
        evidenceSource = self$evidenceSource,
        evidenceSourceID = self$evidenceSourceID,
        datasetID = private$datasetID,
        dataType = dataType,
        githubPat = githubPat,
        description = description,
        figshareUrl = figshareUrl,
        datasetFileName = datasetFileName,
        evidenceFileName = evidenceFileName,
        skipMd5Check = skipMd5Check
      )

      # Perform initial validation checks
      state <- private$validateWriteCTD(state)

      # Handle GitHub PAT
      state <- private$handleGitHubPAT(state)

      # Handle data saving
      state <- private$handleDataSaving(state)

      # Validate dataset availability
      state <- private$validateDatasetAvailability(state)

      # Save supporting evidence
      state <- private$saveSupportingEvidence(state)

      # Verify Figshare upload
      state <- private$verifyFigshareUpload(state)
      self$dataSource <- state$dataSource
      self$dataSourceID <- state$dataSourceID
      self$evidenceSource <- state$evidenceSource
      self$evidenceSourceID <- state$evidenceSourceID

      # Handle dataset description
      state <- private$handleDatasetDescription(state)

      # Process Google Sheets data
      state <- private$processGoogleSheetsData(state)
      state$dataType <- state$dataType

      # Add dataset to sheets
      state <- private$addDatasetToSheets(state)

      # Add evidence to sheets
      state <- private$addEvidenceToSheets(state)

      # Process metrics and tasks
      state <- private$processMetricsAndTasks(state)

      private$datasetID <- state$datasetID
      self$name <- state$name

      cli::cli_inform(c(
        "Added the dataset to the Curated Trio Datasets sheet.",
        "i" = paste0(
          "Please check the details at ",
          "{.href [this link]({private$CTDlink})}"
        )
      ))
    }
  ),
  private = list(
    datasetID = NULL,
    CTDlink = "{.href [Curated Trio Datasets](https://docs.google.com/spreadsheets/d/1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY/)}",

    validateWriteCTD = function(state) {
      if (length(self$evidence) == 0) {
        cli::cli_abort(c(
          "There is no {.var evidence} in this Trio!",
          "i" = "Add some using {.code Trio$addEvidence(...)}."
        ))
      }
      if (!curl::has_internet()) {
        cli::cli_abort(c(
          "Couldn't write to Curated Trio Datasets.",
          "Check your internet connection and try again."
        ))
      }
      if (!interactive()) {
        cli::cli_abort(c(
          "This function must be run interactively.",
          "i" = "Please run it in an interactive session."
        ))
      }
      return(state)
    },

    handleGitHubPAT = function(state) {
      # Use provided PAT if available
      if (!is.null(state$githubPat) && state$githubPat != "") {
        Sys.setenv(GITHUB_PAT = state$githubPat)
        return(state)
      }

      # check if GITHUB_PAT is set and ask the user to set it if not
      if (Sys.getenv("GITHUB_PAT") == "") {
        cli::cli_inform(c(
          "The GITHUB_PAT environment variable is not set.",
          "Please set it to your GitHub personal access token with gist access."
        ))
        setGithubPat <- utils::askYesNo(
          "Do you want to set the GITHUB_PAT environment variable?"
        )
        if (setGithubPat) {
          pat <- readline("Enter your GitHub personal access token: ")
          Sys.setenv(GITHUB_PAT = pat)
        } else {
          cli::cli_abort(c(
            "The GITHUB_PAT environment variable is not set.",
            "i" = paste0(
              "Please set it to your GitHub personal access token",
              " with gist access."
            )
          ))
        }
      }
      return(state)
    },

    handleDataSaving = function(state) {
      # Save dataset if needed
      if (!state$save) {
        state$save <- utils::askYesNo(
          "Do you want to save the data to an RDS file in the current dir?"
        )
        if (state$save) {
          filename <- paste0(state$name, "_dataset.rds")
          saveRDS(self$data, file = filename, compress = "xz")
          state$md5 <- tools::md5sum(filename)
          cli::cli_inform(c(
            "Saved the dataset to {.file {filename}}."
          ))
        }
      }
      return(state)
    },

    validateDatasetAvailability = function(state) {
      if (!state$save && is.null(self$dataSourceID) && is.null(self$dataSource)) {
        # Ask the user if the dataset is available in one of the databases
        # with an implemented downloader in downloaders.R
        # Extract availableSources from downloaders.R dynamically
        availableSources <- stringr::str_remove(
          grep("Dl$", ls("package:BenchHub"), value = TRUE),
          "Dl$"
        )
        # Convert to lowercase to match user input expectations
        availableSources <- tolower(availableSources)
        cli::cli_inform(c(
          "In order to write to the Curated Trio Datasets, the dataset must be available for download.",
          "i" = "Supported data sources: {.val {availableSources}}"
        ))

        hasDownloader <- utils::askYesNo(
          "Is your dataset available in one of these databases with a known ID?"
        )

        if (hasDownloader) {
          sourceChoice <- utils::menu(
            availableSources,
            title = "Select the data source for your dataset:"
          )

          if (sourceChoice == 0) {
            cli::cli_abort(c(
              "No data source was selected.",
              "i" = "Please save the dataset to an RDS file and upload it to Figshare."
            ))
          }

          selectedSource <- availableSources[sourceChoice]
          sourceID <- readline(
            prompt = paste0(
              "Please provide the ",
              selectedSource,
              " ID for your dataset: "
            )
          )

          if (nchar(sourceID) == 0) {
            cli::cli_abort(c(
              "A valid ID is required for the selected data source.",
              "i" = "Please save the dataset to an RDS file and upload it to Figshare."
            ))
          }

          # Set the data source and ID
          self$dataSource <- selectedSource
          self$dataSourceID <- sourceID
        } else {
          cli::cli_abort(c(
            "In order to write to the Curated Trio Datasets, the dataset must be available for download.",
            "i" = "Please save the dataset to an RDS file and upload it to Figshare."
          ))
        }
      }
      return(state)
    },

    saveSupportingEvidence = function(state) {
      # Save all evidence as a single file (required for CTD)
      cli::cli_inform(c(
        "Saving supporting evidence to RDS file..."
      ))
      state$saveEvidence <- TRUE
      saveRDS(self$evidence, file = state$evidenceFilename, compress = "xz")
      state$evidenceMd5 <- tools::md5sum(state$evidenceFilename)
      cli::cli_inform(c(
        "Saved all evidence to {.file {state$evidenceFilename}}."
      ))
      return(state)
    },

    verifyFigshareUpload = function(state) {
      # Upload verification using md5
      attempts <- 0
      maxAttempts <- 4
      verified <- FALSE

      while (attempts < maxAttempts && !verified) {
        attempts <- attempts + 1

        # Use provided URL if available, otherwise prompt
        url <- if (!is.null(state$figshareUrl) && state$figshareUrl != "") {
          state$figshareUrl
        } else {
          cli::cli_inform(c(
            paste0(
              "Please upload the data and/or supporting evidence to Figshare and provide",
              " the URL"
            )
          ))
          readline("Dataset/Evidence Figshare URL: ")
        }

        if (!grepl("figshare", url)) {
          cli::cli_inform(c(
            "The provided URL is not a Figshare URL.",
            "i" = "Please provide a Figshare URL."
          ))
          next()
        }
        id <- stringr::str_extract(url, "(?<=/)[0-9]+")
        if (is.na(id)) {
          cli::cli_inform(c(
            "The provided URL does not contain a valid Figshare ID.",
            "i" = "Please provide a Figshare URL with a valid ID."
          ))
          next()
        }
        fileDF <- figshareListFiles(id)
        fileNames <- fileDF$name

        if (nrow(fileDF) == 0) {
          cli::cli_inform("No files found in the Figshare article.")
          next()
        }

        # Check dataset file
        datasetUploaded <- FALSE
        datasetFileName <- NULL
        # Compute md5 if not present
        if (is.null(state$md5) || state$md5 == "") {
          localFile <- paste0(state$name, "_dataset.rds")
          if (file.exists(localFile)) {
            state$md5 <- tools::md5sum(localFile)
          } else {
            # Create temporary file if local doesn't exist
            tmpFile <- file.path(tempdir(), paste0(state$name, "_dataset.rds"))
            saveRDS(self$data, file = tmpFile, compress = "xz")
            state$md5 <- tools::md5sum(tmpFile)
            unlink(tmpFile) # Clean up temporary file
          }
        }
        if (!is.null(self$dataSource) && !is.null(self$dataSourceID)) {
          datasetUploaded <- TRUE
        } else {
          # Use provided dataset filename or let user choose
          if (!is.null(state$datasetFileName)) {
            if (!(state$datasetFileName %in% fileNames)) {
              cli::cli_inform(c(
                "Provided dataset file name not found in Figshare article.",
                "i" = "Available files: {.val {fileNames}}"
              ))
              state$figshareUrl <- NULL # Reset URL to try again
              next()
            }
            datasetFileName <- state$datasetFileName
          } else {
            # Let user choose the dataset file from available files
            cli::cli_inform(
              "Please select the dataset file from the list above."
            )
            datasetChoice <- utils::menu(fileNames)
            if (datasetChoice == 0) {
              cli::cli_inform("No dataset file selected. Please try again.")
              state$figshareUrl <- NULL # Reset URL to try again
              next()
            }
            datasetFileName <- fileNames[datasetChoice]
          }

          idx <- which(fileNames == datasetFileName)[1]
          fileID <- fileDF$id[idx]
          # Check md5
          fileMd5 <- fileDF$computed_md5[idx]
          if (!is.null(state$md5) && state$md5 == fileMd5) {
            datasetUploaded <- TRUE
            state$dataSource <- "figshare"
            state$dataSourceID <- paste0(id, "/", fileID)
          } else {
            cli::cli_inform(c(
              "The uploaded dataset file md5 does not match.",
              "i" = "Please re-upload the correct file.",
              "i" = "Otherwise, set self$dataSource and self$dataSourceID."
            ))
            # Ask user if they want to override MD5 verification
            if (!state$skipMd5Check) {
              overrideMD5 <- utils::askYesNo(
                "Do you want to override the MD5 verification for the dataset file?"
              )
            } else {
              overrideMD5 <- TRUE
            }
            if (overrideMD5) {
              cli::cli_inform("MD5 verification overridden for dataset file.")
              datasetUploaded <- TRUE
              state$dataSource <- "figshare"
              state$dataSourceID <- paste0(id, "/", fileID)
            } else {
              state$figshareUrl <- NULL # Reset URL to try again
              next()
            }
          }
        }
        # Check evidence file
        evidenceUploaded <- FALSE
        evidenceFileName <- NULL
        if (state$saveEvidence) {
          # Use provided evidence filename or let user choose
          if (!is.null(state$evidenceFileName)) {
            if (!(state$evidenceFileName %in% fileNames)) {
              cli::cli_inform(c(
                "Provided evidence file name not found in Figshare article.",
                "i" = "Available files: {.val {fileNames}}"
              ))
              state$figshareUrl <- NULL # Reset URL to try again
              next()
            }
            evidenceFileName <- state$evidenceFileName
          } else {
            # Let user choose the evidence file from available files
            cli::cli_inform(
              "Please select the evidence file from the list above."
            )
            evidenceChoice <- utils::menu(fileNames, title = "Evidence file")
            if (evidenceChoice == 0) {
              cli::cli_inform("No evidence file selected. Please try again.")
              state$figshareUrl <- NULL # Reset URL to try again
              next()
            }
            evidenceFileName <- fileNames[evidenceChoice]
          }

          idxEv <- which(fileNames == evidenceFileName)[1]
          if (!is.na(idxEv)) {
            fileIDev <- fileDF$id[idxEv]
            fileMd5ev <- fileDF$computed_md5[idxEv]
            if (!is.null(state$evidenceMd5) && state$evidenceMd5 == fileMd5ev) {
              evidenceUploaded <- TRUE
              state$evidenceSource <- "figshare"
              state$evidenceSourceID <- paste0(id, "/", fileIDev)
            } else {
              cli::cli_inform(c(
                "The uploaded evidence file md5 does not match.",
                "i" = "Please re-upload the correct evidence file."
              ))
              # Ask user if they want to override MD5 verification
              if (!state$skipMd5Check) {
                overrideMD5Ev <- utils::askYesNo(
                  "Do you want to override the MD5 verification for the evidence file?"
                )
              } else {
                overrideMD5Ev <- TRUE
              }
              if (overrideMD5Ev) {
                cli::cli_inform(
                  "MD5 verification overridden for evidence file."
                )
                evidenceUploaded <- TRUE
                state$evidenceSource <- "figshare"
                state$evidenceSourceID <- paste0(id, "/", fileIDev)
              } else {
                state$figshareUrl <- NULL # Reset URL to try again
                next()
              }
            }
          } else {
            cli::cli_inform(c(
              "The selected evidence file is not found in the Figshare article.",
              "i" = "Please try again."
            ))
            next()
          }
        }
        verified <- datasetUploaded && (evidenceUploaded || !state$saveEvidence)
      }
      if (!verified) {
        cli::cli_abort(c(
          "Failed to verify upload to Figshare after 4 attempts.",
          "i" = "Please ensure the files are uploaded and try again."
        ))
      }
      return(state)
    },

    handleDatasetDescription = function(state) {
      # Use provided description if available
      if (!is.null(state$description)) {
        self$description <- state$description
        return(state)
      }

      # if the dataset doesn't have a description, prompt the user to input one
      if (is.null(self$description)) {
        self$description <- readline(
          prompt = "Please provide a description for the dataset: "
        )
      }
      return(state)
    },

    processGoogleSheetsData = function(state) {
      # read the existing datasets
      datasets <- googlesheets4::read_sheet(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        sheet = "Datasets"
      )

      if (!is.null(state$datasetID)) {
        # ask for confirmation if they want to upload this dataset again
        if (state$datasetID %in% datasets$datasetID) {
          cli::cli_inform(c(
            "The dataset ID {.val {state$datasetID}} already exists in the datasets sheet."
          ))
          overwrite <- utils::askYesNo("Do you want to add it again?")
          if (!overwrite) {
            cli::cli_abort(c(
              "Dataset upload cancelled.",
            ))
          } else {
            state$datasetID <- NULL
            private$datasetID <- NULL
          }
        }
      }

      state$datasetID <- formatC(
        max(as.integer(datasets$datasetID)) + 1,
        width = 4,
        flag = "0"
      )

      # check if the name is already in the datasets
      if (state$name %in% datasets$name) {
        cli::cli_abort(c(
          "The dataset name {.val {state$name}} is already in the datasets sheet.",
          "i" = "Please choose a different name."
        ))
      }

      dataTypes <- c(
        "omics",
        "clinical",
        "spatial",
        "other"
      )
      # Use provided data type or prompt user
      if (!is.null(state$dataType)) {
        if (!state$dataType %in% dataTypes) {
          cli::cli_abort(c(
            "Invalid data type provided.",
            "i" = "Must be one of: {.val {dataTypes}}"
          ))
        }
      } else {
        # prompt the user to input the data type
        dataTypeChoice <- utils::menu(
          dataTypes,
          title = "Select the data type of the dataset:"
        )
        if (dataTypeChoice == 0) {
          cli::cli_abort(c(
            "No data type was selected.",
            "i" = "Please select a data type."
          ))
        }
        state$dataType <- dataTypes[dataTypeChoice]
      }

      return(state)
    },

    addDatasetToSheets = function(state) {
      googlesheets4::sheet_append(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        data = data.frame(
          datasetID = state$datasetID,
          name = state$name,
          source = state$dataSource,
          sourceID = as.character(state$dataSourceID), # Avoid mixed types causing import as list.
          md5 = state$md5,
          dataType = state$dataType,
          description = self$description,
          validated = FALSE
        ),
        sheet = "Datasets"
      )
      return(state)
    },

    addEvidenceToSheets = function(state) {
      # add the evidence to the sheet
      if (state$saveEvidence) {
        evidenceMetaData <- data.frame(
          datasetID = rep(state$datasetID, times = length(self$evidence)),
          Supporting_Evidence = names(self$evidence),
          is_in_data = rep(FALSE, times = length(self$evidence)),
          type = unlist(state$evidenceSource),
          sourceID = unlist(state$evidenceSourceID),
          name = rep("", times = length(self$evidence)),
          on_load = rep("", times = length(self$evidence)),
          validated = rep(FALSE, times = length(self$evidence)),
          stringsAsFactors = FALSE
        )
        googlesheets4::sheet_append(
          ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
          data = evidenceMetaData,
          sheet = "Dataset-Evidence"
        )
      }
      return(state)
    },

    processMetricsAndTasks = function(state) {
      # create a character vector of the metric functions
      metricText <- lapply(names(self$metrics), function(metric_name) {
        metric_func <- self$metrics[[metric_name]]
        # access the environment of the metric function
        metric_env <- environment(metric_func)

        # Extract the `metric` function and `args` from the environment
        metric <- metric_env$metric
        args <- metric_env$args

        # Deparse the `metric` function
        metric_deparsed <- deparse(metric)
        metric_deparsed[1] <- paste0(metric_name, " <- ", metric_deparsed[1])

        # Create the `metricArgs` list as a deparsed character vector
        args_deparsed <- paste0(metric_name, "Args <- ", deparse(args))

        # Combine the deparsed function and args into the desired format
        c(metric_deparsed, args_deparsed)
      }) |>
        unlist()

      # create a gist of the metrics
      gist <- gistr::gist_create(
        code = metricText,
        description = paste0("Metrics for Trio ", state$name),
        public = TRUE,
        filename = paste0(state$name, "_metrics.R")
      )

      # add the metrics to the sheet
      metricsMetaData <- data.frame(
        MetricID = names(self$metrics),
        wrapper.r = rep("", times = length(self$metrics)),
        `Metric Type` = rep("gist", times = length(self$metrics)),
        r_deps = rep("", times = length(self$metrics)),
        wrapper.py = rep("", times = length(self$metrics)),
        py_deps = rep("", times = length(self$metrics)),
        gist_url = rep(gist$html_url, times = length(self$metrics)),
        validated = rep(FALSE, times = length(self$metrics)),
        stringsAsFactors = FALSE
      )

      googlesheets4::sheet_append(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        data = metricsMetaData,
        sheet = "Metrics"
      )

      # get the next task ID
      # read the existing datasets
      tasks <- googlesheets4::read_sheet(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        sheet = "Task-Evidence Type-Metric"
      )

      taskID <- formatC(
        max(as.integer(substring(tasks$`Task ID`, 2))) + 1,
        width = 4,
        flag = "0"
      )

      # create a table of evidence-metric relationships for each evidence
      taskEvidenceMetaData <- tibble::tibble(
        `Task ID` = paste0("T", taskID),
        `Task Name` = state$name,
        Topic = paste0(state$name, "Tasks"),
        `Evidence Type` = lapply(names(self$evidence), \(evidenceName) {
          metrics <- self$getMetrics(evidenceName)
          rep(evidenceName, times = length(metrics))
        }) |>
          unlist(),
        MetricID = lapply(names(self$evidence), self$getMetrics) |>
          unlist(),
        validated = FALSE
      )

      googlesheets4::sheet_append(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        data = taskEvidenceMetaData,
        sheet = "Task-Evidence Type-Metric"
      )
      return(state)
    },
    parseIDString = function(userInput, IDtype = c("data", "evidence")) {
      IDtype <- match.arg(IDtype)
      parsed <- unlist(stringr::str_split(userInput, ":"))

      if (length(parsed) == 1) {
        datasets <- googlesheets4::read_sheet(
          ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
          sheet = "Datasets"
        )

        if (!userInput %in% datasets$name) {
          # Tell the user how to list the available datasets
          cli::cli_abort(c(
            "Specified dataset ({.val {userInput}}) is not available.",
            "i" = "To see a list of available datasets, run {.code listCuratedTrioDatasets()}."
          ))
        }

        self$name <- userInput

        sourceName <- datasets |>
          dplyr::filter(name == userInput) |>
          dplyr::select(source) |>
          purrr::pluck(1)

        id <- datasets |>
          dplyr::filter(name == userInput) |>
          dplyr::select(sourceID) |>
          purrr::pluck(1)

        private$datasetID <- datasets |>
          dplyr::filter(name == userInput) |>
          dplyr::select(datasetID) |>
          purrr::pluck(1)
      } else if (length(parsed) == 2) {
        sourceName <- tolower(parsed[1])
        id <- parsed[2]
      } else {
        cli::cli_abort(c(
          "Unsupported data specification string",
          "i" = paste0(
            "Input a dataset name or a string like",
            " {.emph source}:{.emph ID}"
          )
        ))
      }

      if (!exists(paste0(sourceName, "Dl"))) {
        supported <- stringr::str_remove(
          grep("Dl", ls("package:BenchHub"), value = TRUE),
          "Dl"
        )
        cli::cli_abort(c(
          "Downloading form {.emph {sourceName}} is not supported.",
          "i" = "Choose one of the following: {supported}"
        ))
      }

      if(IDtype == "data")
      {
        self$dataSource <- sourceName
        self$dataSourceID <- id
      } else {
        self$evidenceSource <- sourceName
        self$evidenceSourceID <- id
      }
        
    },
    # Send the ID to the appropriate downloader and load the file, if possible.
    getData = function(sourceName, id, cachePath, dataLoader) {
      files <- do.call(
        paste0(sourceName, "Dl"),
        list("ID" = id, "cachePath" = cachePath)
      )

      if (length(files) > 1) {
        if (self$verbose) {
          cli::cli_inform("Select a file to load as the dataset:")
        }
        files <- files[utils::menu(files)]
      }

      if (is.null(dataLoader)) {
        return(loadFile(files))
      }

      if (!is.function(dataLoader)) {
        cli::cli_abort(c(
          "The provided {.var dataLoader} is not a function!",
          "i" = "Ensure the dataloader is a function with one argument."
        ))
      }

      if (length(formals(dataLoader)) != 1) {
        cli::cli_abort(c(
          "The provided dataLoader must have one argument!"
        ))
      }

      dataLoader(files)
    },
    populateTrio = function(evidenceID, evidence, evidenceColumns, evidenceLoader, task, metrics) {
      if (!curl::has_internet()) {
        cli::cli_warn(c(
          "Couldn't populate Trio from Curated Trio Datasets.",
          "Check your internet connection and try again."
        ))
        return(NULL)
      }

      # get the gold standard metadata from curated trio datasets
      evidenceMetaData <- suppressMessages(
        googlesheets4::read_sheet(
          ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
          sheet = "Dataset-Evidence",
        ) |>
          dplyr::filter(sourceID == self$dataSourceID)
      )
      
      if (nrow(evidenceMetaData) == 0 && is.null(evidenceLoader) && is.null(evidenceColumns) && is.null(evidence)) {
        cli::cli_warn(c(
          paste0(self$CTDlink, " has no supporting evidence for this dataset."),
          "i" = "Please add your own supporting evidence for evaluation."
        ))
        return(NULL)
      }

      if(nrow(evidenceMetaData) == 0) # Not curated. Getting it directly from a source.
      {
        if(!is.null(evidence))
        {
          self$evidence <- evidence
          self$metrics <- metrics
        } else if(!is.null(evidenceID))
        {
          self$evidence <- list(task = list(evidence = private$getData(
            self$evidenceSource,
            self$evidenceSourceID,
            self$cachePath,
            evidenceLoader$evidence
          ), metrics = names(metrics)), metrics = metrics)
        } else if(!is.null(evidenceColumns)) { # evidenceID is NULL, so evidence is in columns of data table.
          self$evidence <- list(list(evidence = self$data[, evidenceColumns], metrics = names(metrics)))
          names(self$evidence) <- task
          self$metrics <- metrics
          self$evidenceSourceID <- self$dataSourceID
          self$data <- self$data[, -match(evidenceColumns, colnames(self$data))]
          # Evidence extracted and removed from data to avoid use as covariate.
        } else if (!is.null(evidenceLoader))  { # Evidence is extracted from data object using dataLoader.
          self$evidence <- list(list(evidence = private$getData(
            self$dataSource,
            self$dataSourceID,
            self$cachePath,
            evidenceLoader
          ), metrics = names(metrics)))
          names(self$evidence) <- task
          self$metrics <- metrics
        }
      }

      evidence <- evidenceMetaData |> purrr::pluck("Supporting Evidence")

      # get the relevant metrics and respective information from the sheet.
      metrics <- suppressMessages(
        googlesheets4::read_sheet(
          ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
          sheet = "Task-Evidence Type-Metric",
        ) |>
          dplyr::filter(`Evidence Type` %in% evidence) %>%
          dplyr::left_join(
            .,
            googlesheets4::read_sheet(
              ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
              sheet = "Metrics",
            )
          ) |>
          dplyr::distinct(MetricID, .keep_all = TRUE)
      )

      # create metrics inside the object
      if(nrow(metrics) > 0)
      {
        apply(metrics, 1, \(metric) {
          if (metric["Metric Type"] == "internal") {
            self$addMetric(
              name = metric["MetricID"][[1]],
              metric = match.fun(metric["wrapper.r"][[1]])
            )
          } else if (metric["Metric Type"] == "gist") {
            # Handle external metrics from gists
            gist_url <- metric["gist_url"][[1]]
            if (!is.na(gist_url) && gist_url != "") {
              path <- tempdir()
              # Download the gist
              temp_file <- gistr::gist(
                gist_url,
                quiet = TRUE
              ) |>
                gistr::gist_save(
                  path = path
                ) |>
                purrr::pluck(1)
  
              # Create a new environment for the metric functions
              metric_env <- new.env()
  
              # Source the file in the new environment
              sys.source(temp_file, envir = metric_env)
  
              # Add the metric function from the new environment
              metric_name <- metric["MetricID"][[1]]
              if (exists(metric_name, envir = metric_env)) {
                self$addMetric(
                  name = metric_name,
                  metric = get(metric_name, envir = metric_env)
                )
              }
            }
          } else {
            # TODO: Support other external metrics.
              cli::cli_abort(c(
              "External metrics of type {metric['Metric Type']} are not yet supported."
              ))
            }
        })
      }

      # Check if evidence was saved via writeCTD (all evidence in one file)
      # This is indicated by multiple evidence items with the same sourceID
      evidenceSourceIDs <- evidenceMetaData |> purrr::pluck("sourceID")
      uniqueSourceIDs <- unique(evidenceSourceIDs)

      # If there's only one unique sourceID and multiple evidence items,
      # it's likely that all evidence was saved together via writeCTD
      if (
        !is.na(uniqueSourceIDs) &&
          length(uniqueSourceIDs) == 1 &&
          nrow(evidenceMetaData) > 1
      ) {
        # Download the combined evidence file once
        sourceID <- uniqueSourceIDs[1]
        tempCachePath <- tempdir()

        tryCatch(
          {
            filePath <- figshareDl(sourceID, tempCachePath)

            # Load all evidence from the combined file
            combinedEvidence <- loadFile(filePath)

            # Add each evidence item with its respective metrics
            for (i in seq_len(nrow(unique(evidenceMetaData)))) {
              evidenceRow <- unique(evidenceMetaData)[i, ]
              evidenceName <- unlist(evidenceRow["Supporting Evidence"])

              # Check if the evidence exists in the combined evidence
              if (evidenceName %in% names(combinedEvidence)) {
                self$addEvidence(
                  name = evidenceName,
                  evidence = combinedEvidence[[evidenceName]]$evidence,
                  metrics = combinedEvidence[[evidenceName]]$metrics
                )
              } else {
                cli::cli_warn(c(
                  "Evidence `{evidenceName}` not found in the combined evidence file.",
                  "Skipping this evidence item."
                ))
              }
            }
          },
          error = function(e) {
            cli::cli_abort(c(
              "Failed to download or load combined evidence from figshare.",
              "Error: {e$message}"
            ))
          }
        )
      } else {
        # Handle evidence items individually (original behavior)
        if(nrow(evidenceMetaData) > 0)
        {
          apply(evidenceMetaData, 1, \(evidenceRow) {
            evidenceName <- evidenceRow["Supporting Evidence"]
  
            # Handle evidence from different sources
            if (evidenceRow["is_in_data"]) {
              # Evidence is in the main data
              if (evidenceRow["type"] == "columns") {
                evidenceCols <- unlist(strsplit(evidenceRow["name"], ", ", TRUE))
                self$addEvidence(
                  name = evidenceName,
                  evidence = self$data[, evidenceCols],
                  metrics = metrics |>
                    dplyr::filter(`Evidence Type` == evidenceName) |>
                    purrr::pluck("MetricID")
                )
              } else {
                cli::cli_abort(c(
                  "Accessors for non-tabular data types aren't supported yet."
                ))
              }
            } else if (evidenceRow["type"] == "figshare") {
              # Evidence is stored separately
              # Use the figshareDl function to download the evidence file
              sourceID <- evidenceRow["sourceID"]
  
              # Create a temporary cache path for downloading
              tempCachePath <- tempdir()
  
              # Download the evidence file using figshareDl
              tryCatch(
                {
                  filePath <- figshareDl(sourceID, tempCachePath)
  
                  # Load the evidence data
                  evidenceData <- loadFile(filePath)
  
                  self$addEvidence(
                    name = evidenceName,
                    evidence = evidenceData,
                    metrics = metrics |>
                      dplyr::filter(`Evidence Type` == evidenceName) |>
                      purrr::pluck("MetricID")
                  )
                },
                error = function(e) {
                  cli::cli_abort(c(
                    "Failed to download or load evidence {evidenceName} from figshare.",
                    "Error: {e$message}"
                  ))
                }
              )
            } else if (evidenceRow["type"] == "function") {
              # Evidence is a function to be applied to the data
              on_load_code <- evidenceRow["on_load"]
              if (!is.na(on_load_code) && on_load_code != "") {
                # Evaluate the function
                evidenceFunc <- eval(parse(text = on_load_code))
                if (is.function(evidenceFunc)) {
                  self$addEvidence(
                    name = evidenceName,
                    evidence = evidenceFunc,
                    metrics = metrics |>
                      dplyr::filter(`Evidence Type` == evidenceName) |>
                      purrr::pluck("MetricID")
                  )
                } else {
                  cli::cli_abort(c(
                    "on_load code for evidence {evidenceName} does not evaluate to a function."
                  ))
                }
              } else {
                cli::cli_abort(c(
                  "No on_load code provided for evidence {evidenceName} of type function."
                ))
              }
            } else {
              cli::cli_abort(c(
                "Evidence type {evidenceRow['type']} is not supported."
              ))
            }
          }
        )}
      }
    }
  )
)

#' List the curated Trio datasets
#' @param name_filter
#'   A string to filter datasets by name (case-insensitive partial match)
#' @param
#'   source_filter A string or vector of strings to filter datasets by source
#' @param dataType_filter
#'   A string or vector of strings to filter datasets by data type
#' @return A data frame with the dataset names and IDs.
#' @export
listCuratedTrioDatasets <- function(
  name_filter = NULL,
  dataType_filter = NULL
) {
  if (!curl::has_internet()) {
    cli::cli_warn(c(
      "Couldn't list Curated Trio Datasets.",
      "Check your internet connection and try again."
    ))
    return(NULL)
  }
  datasets <- googlesheets4::read_sheet(
    ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
    sheet = "Datasets"
  ) |>
    dplyr::select(name, datasetID, source, sourceID, dataType) |>
    dplyr::arrange(name)

  # Apply filters if provided
  if (!is.null(name_filter)) {
    datasets <- datasets |>
      dplyr::filter(grepl(name_filter, name, ignore.case = TRUE))
  }

  if (!is.null(dataType_filter)) {
    datasets <- datasets |>
      dplyr::filter(dataType %in% dataType_filter)
  }

  datasets
}
