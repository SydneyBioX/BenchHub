# Silence check notes about R6 class
#' @importFrom R6 R6Class
NULL

#' A Trio object
#' @description An object containing a dataset and methods for evaluating
#'   analytical tasks against ground truths for the dataset.
#' @field data The data
#' @field auxData The auxiliary data in the data
#' @field metrics The metric for evaluating tasks against the gold standards
#' @field cachePath The path to the data cache
#' @field dataSource The data repository that the data were retrieved from
#' @field dataSourceID The dataset ID for `dataSouce`
#' @field splitIndices Indices for cross-validation
#'
#' @examples
#' trio <- Trio$new("figshare:26054188/47112109", cachePath = tempdir())
#' @export
#' @importFrom googlesheets4 read_sheet
Trio <- R6::R6Class(
  "Trio",
  public = list(
    cachePath = NULL,
    data = NULL,
    auxData = list(),
    metrics = list(),
    dataSource = NULL,
    dataSourceID = NULL,
    splitIndices = NULL,

    # TODO: Implement Trio$sources() (Issue #2)

    #' @description
    #' Create a Trio object
    #' @param datasetID
    #'   A string specifying a dataset, either a name from curated-trio-data or
    #'   a format string of the form `source`:`source_id`.
    #' @param cachePath The path to the data cache
    initialize = function(datasetID = NULL, cachePath = FALSE) {
      # if users have their own data without datasetID
      if (is.null(datasetID)) {
        # prompt users to input their own new datasetID
        datasetID <- readline(prompt = "If you don't have a Figshare/GEO/ExperimentHub datasetID, please provide a new datasetID: ")
      } else {
        # parse user input and set dataSource and dataSourceID
        private$parseIDString(datasetID)

        self$cachePath <- getTrioCachePath(cachePath)
        self$data <- private$getData(
          self$dataSource, self$dataSourceID, self$cachePath
        )
        if (!is.null(private$datasetID)) {
          # private$populateTrio()
        }
      }
    },

    #' @description
    #' Add a gold standard to the Trio.
    #' @param name A string specifying the name of the gold standard.
    #' @param auxData
    #'   The auxiliary data. An object to be compared or a function to be run on
    #'   the data.
    #' @param metrics
    #'   A list of one or more metrics names used to campare gs with the input
    #'   to evaluate.
    #' @param args
    #'   A named list of parameters and values to be passed to the function.
    addAuxData = function(name, auxData, metrics, args = NULL) {
      if (name %in% names(self$auxData)) {
        cli::cli_warn(c(
          paste0(
            "Auxiliary data `{name}` is already present in this Trio,",
            " overwriting."
          )
        ))
      }

      if (methods::is(auxData, "function")) {
        # Assign a wrapper function that adds args applies to
        # self$data, returning the result.
        self$auxData[[name]] <- list(
          "auxData" = function(data) {
            do.call(auxData, append(list(data), args))
          },
          "metrics" = metrics
        )
      } else {
        # TODO: Validate the gold standard objects.
        self$auxData[[name]] <- list(
          "auxData" = auxData,
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
      # TODO: Validate metric!!
      # metric functions should follow this format (auxData, to_eval)
      self$metrics[[name]] <- function(auxData, to_eval) {
        do.call(metric, append(list(auxData, to_eval), args))
      }
    },

    #' @description
    #' Get metrics by gold standard name.
    #' @param auxDataName A string specifying the name of the gold standard.
    getMetrics = function(auxDataName) {
      if (!auxDataName %in% names(self$auxData)) {
        cli::cli_abort(c(
          "{.val {auxDataName} is not auxiliary data in this object.}",
          "i" = "Choose one of {.val {names(self$auxData)}}"
        ))
      }
      purrr::pluck(self$auxData, auxDataName, "metrics")
    },

    #' @description
    #' Get auxiliary data by name.
    #' @param name A string specifying the name of the auxiliary data.
    getAuxData = function(name) {
      if (length(self$auxData) == 0) {
        cli::cli_abort(c(
          "There is no auxiliary data in this Trio!",
          "i" = "Add some using {.code Trio$addAuxData(...)}."
        ))
      }
      if (!name %in% names(self$auxData)) {
        auxDataNames <- names(self$auxData)
        cli::cli_abort(c(
          "Auxiliary data {.val {name}} could not be found.",
          "i" = paste0(
            "Add it using {.code Trio$addAuxDataS(.)} or choose one of ",
            "{.val {auxDataNames}}"
          )
        ))
      }

      auxData <- self$auxData[[name]]$auxData

      if (!methods::is(auxData, "function")) {
        return(auxData)
      }

      auxData(self$data)
    },

    #' @description
    #' Evalute against gold standards
    #' @param input A named list of objects to be evaluated against gold
    #'   standards.
    #' @param separateMethods If `input` contains separate sublists to evaluate
    #'   for each method.
    evaluate = function(input, separateMethods = FALSE) {
      # check if auxiliary data is available for each element of the input.
      if (separateMethods) {
        evalList <- lapply(input, self$evaluate)
        return(evalList)
      } else {
        # check if all the requested auxData is available
        auxDataAvail <- names(input) %in% names(self$auxData)

        # if none of the auxData are available
        if (all(!auxDataAvail)) {
          auxDataNames <- names(self$auxData)
          cli::cli_abort(c(
            "None of the specified auxiliary data are available in this object.",
            "i" = (
              "Add it using {.code Trio$addAuxData(.)} or choose from {.val {auxDataNames}}"
            )
          ))
        }

        # if some of the auxData are missing
        if (any(!auxDataAvail)) {
          unavail <- names(input)[!auxDataAvail]
          cli::cli_inform(c(
            paste0(
              "Auxiliary data{?s} {.val {unavail}} from {.var input} {?is/are} ",
              "not available in this object."
            ),
            "i" = "Evaluating the following: {.var {names(input)[auxDataAvail]}}"
          ))
        }

        # subset to inputs with available auxData
        input <- input[auxDataAvail]

        # compute/retrive auxiliary data
        auxData <- setNames(
          lapply(names(input), self$getAuxData),
          names(input)
        )

        # get a list of metrics to compute for each gold standard in the data
        metrics <- setNames(lapply(names(input), self$getMetrics), names(input))

        # get a flat list of available metrics
        # TODO: only get metrics for current evaluation task
        allMetrics <- metrics |>
          unlist() |>
          unique()

        # find metrics that are not available in the trio
        unavailMetrics <- allMetrics[!allMetrics %in% names(self$metrics)]

        if (length(unavailMetrics) == length(allMetrics)) {
          cli::cli_abort(c(
            paste0(
              "None of the metrics related to the auxiliary data being evalutaed",
              " are available in the object."
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
            \(auxDataMetrics) Filter(\(x) !x %in% unavailMetrics, auxDataMetrics)
          )
        }

        # compute each metric for each input
        purrr::imap(input, function(to_eval, auxDataName) {
          res <- lapply(
            metrics[[auxDataName]],
            function(x) self$metrics[[x]](to_eval, auxData[[auxDataName]])
          )
          setNames(res, metrics[[auxDataName]])
        })
      }
    },

    #' @description
    #' Create a cross-validation indices.
    #' @param y
    #'   A variable to use for statified sampling. If `stratify` is false, a
    #'   vector the length of the data.
    #' @param n_fold Number of folds. Defaults to `5L`.
    #' @param n_repeat Number of repeats. Defaults to `1L`.
    #' @param stratify If `TRUE`, uses stratified sampling. Defaults to `TRUE`.
    split = function(y,
                     n_fold = 5L, n_repeat = 1L, stratify = TRUE) {
      # If indices already exist.
      if (!is.null(self$splitIndices)) {
        # ask user to confirm that they want to overwrite the current split
        overwrite <- utils::askYesNo(
          "A split already exists. Would you like to overwrite it? (yes/[no])",
          default = FALSE
        )
        if (!overwrite) {
          cli::cli_inform(c(
            "i" = "Not overwriting, to get current indices, access {.code trio$splitIndices}"
          ))
          invisible(NULL)
        }
      }

      self$splitIndices <- splitTools::create_folds(
        y,
        k = n_fold,
        type = if_else(stratify, "stratified", "basic"),
        m_rep = n_repeat
      )
    }
  ),
  private = list(
    datasetID = NULL,
    parseIDString = function(userInput) {
      parsed <- unlist(stringr::str_split(userInput, ":"))

      if (length(parsed) == 1) {
        datasets <- googlesheets4::read_sheet(
          ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
          sheet = "Datasets"
        )

        if (!userInput %in% datasets$name) {
          # TODO: Tell the user how to list the available datasets
          cli::cli_abort(c(
            "Specified dataset ({.val {userInput}}) is not avaiable."
          ))
        }

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
          "i" = (
            "Input a dataset name or a string like {.emph source}:{.emph ID}"
          )
        ))
      }

      if (!exists(paste0(sourceName, "Dl"))) {
        supported <- stringr::str_remove( # nolint
          grep("Dl", ls("package:TrioR"), value = TRUE), "Dl"
        )
        cli::cli_abort(c(
          "Downloading form {.emph {sourceName}} is not supported.",
          "i" = "Choose one of the following: {supported}"
        ))
      }

      self$dataSource <- sourceName
      self$dataSourceID <- id
    },
    # Send the ID to the appropriate downloader and load the file, if possible.
    getData = function(sourceName, id, cachePath) {
      # TODO: allow the user to input a custom loading function.
      files <- do.call(
        paste0(sourceName, "Dl"),
        list("ID" = id, "cachePath" = cachePath)
      )

      if (length(files) > 1) {
        cli::cli_inform("Select a file to load as the dataset:")
        files <- files[utils::menu(files)]
      }

      loadFile(files)
    },
    populateTrio = function() {
      # get the gold standard metadata from curated trio datasets
      gSMetaData <- suppressMessages(googlesheets4::read_sheet(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        sheet = "Dataset-Gold Standard",
      ) |>
        dplyr::filter(datasetID == private$datasetID))

      auxData <- gSMetaData |> purrr::pluck("Auxiliary Data")

      metrics <- suppressMessages(googlesheets4::read_sheet(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        sheet = "Task-GS Type-Metric",
      ) |>
        dplyr::filter(`GS Type` %in% auxData) |>
        dplyr::select(`GS Type`, `MetricID`))

      # add each gold standard with it's respective metrics
      apply(gSMetaData, 1, \(auxData) {
        if (auxData["is_in_data"]) {
          if (auxData["type"] == "column") {
            self$addAuxData(
              name = auxData["Auxiliary Data"],
              auxData = \() self$data[[1]][auxData["name"]],
              metrics = metrics |>
                dplyr::filter(`GS Type` == auxData["Auxiliary Data"]) |>
                purrr::pluck("MetricID")
            )
          } else {
            cli::cli_abort(c(
              "Accessors for non-tabular data types aren't supported yet."
            ))
          }
        } else {
          cli::cli_abort(c(
            "Gold standards that are not in the data are not supoorted yet."
          ))
        }
      })
    }
  )
)
