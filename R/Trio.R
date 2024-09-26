# Silence check notes about R6 class
#' @importFrom R6 R6Class
NULL

#' A Trio object
#' @description An object containing a dataset and methods for evaluating
#'   analytical tasks against ground truths for the dataset.
#' @field data The data
#' @field goldStandards The gold standards in the data
#' @field metrics The metric for evaluating tasks against the gold standards
#' @field cachePath The path to the data cache
#' @field dataSource The data repository that the data were retrieved from
#' @field dataSourceID The dataset ID for `dataSouce`
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
    goldStandards = list(),
    metrics = list(),
    dataSource = NULL,
    dataSourceID = NULL,

    # TODO: Implement Trio$sources() (Issue #2)

    #' @description
    #' Create a Trio object
    #' @param datasetID
    #'   A string specifying a dataset, either a name from curated-trio-data or
    #'   a format string of the form `source`:`source_id`.
    #' @param cachePath The path to the data cache
    initialize = function(datasetID, cachePath = FALSE) {
      # parse user input and set dataSource and dataSourceID
      private$parseIDString(datasetID)

      self$cachePath <- getTrioCachePath(cachePath)
      self$data <- private$getData(
        self$dataSource, self$dataSourceID, self$cachePath
      )
      if (!is.null(private$datasetID)) {
        #private$populateTrio()
      }
    },

    #' @description
    #' Add a gold standard to the Trio.
    #' @param name A string specifying the name of the gold standard.
    #' @param gs
    #'   The goldstandard. An object to be compared or a function to be run on
    #'   the data.
    #' @param metrics
    #'   A list of one or more metrics names used to campare gs with the input
    #'   to evaluate.
    #' @param args
    #'   A named list of parameters and values to be passed to the function.
    addGS = function(name, gs, metrics, args = NULL) {
      if (name %in% names(self$goldStandards)) {
        cli::cli_warn(c(
          paste0(
            "A gold standard `{name}` is already present in this Trio,",
            " overwriting."
          )
        ))
      }

      if (methods::is(gs, "function")) {
        # Assign a function that adds args and applies to each element in
        # self$data, returning the result.
        self$goldStandards[[name]] <- list(
          "gs" = function(data) {
            lapply(
              data, # a list of datasets to apply `gs` to
              \(x) do.call(gs, append(list(x), args))
            )
          },
          "metrics" = metrics
        )
      } else {
        # TODO: Validate the gold standard objects.
        self$goldStandards[[name]] <- list(
          "gs" = gs,
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
      # metric functions should follow this format (gs, to_eval)
      self$metrics[[name]] <- function(gs, to_eval) {
        do.call(metric, append(list(gs, to_eval), args))
      }
    },

    #' @description
    #' Get metrics by gold standard name.
    #' @param gsName A string specifying the name of the gold standard.
    getMetrics = function(gsName) {
      if (!gsName %in% names(self$goldStandards)) {
        cli::cli_abort(c(
          "{.val {gsName} is not a gold standard in this object.}",
          "i" = "Choose one of {.val {names(self$goldStandards)}}"
        ))
      }
      purrr::pluck(self$goldStandards, gsName, "metrics")
    },

    #' @description
    #' Get a gold standard by name.
    #' @param name A string specifying the name of the gold standard.
    getGS = function(name) {
      if (length(self$goldStandards) == 0) {
        cli::cli_abort(c(
          "There are no gold standards in this Trio!",
          "i" = "Add some using {.code Trio$addGS(...)}."
        ))
      }
      if (!name %in% names(self$goldStandards)) {
        gsNames <- names(self$goldStandards)
        cli::cli_abort(c(
          "Gold standard {.val {name}} could not be found.",
          "i" = paste0(
            "Add it using {.code Trio$addGS(.)} or choose one of ",
            "{.val {gsNames}}"
          )
        ))
      }

      gs <- self$goldStandards[[name]]$gs

      if (!methods::is(gs, "function")) {
        return(gs)
      }

      gs(self$data)
    },

    #' @description
    #' Evalute against gold standards
    #' @param input A named list of objects to be evaluated against gold
    #'   standards.
    #' @param separateMethods If `input` contains separate sublists to evaluate
    #'   for each method.
    evaluate = function(input, separateMethods = FALSE) {
      # check if a gold standard is available for each element of the input.
      if (separateMethods) {
        evalList <- lapply(input, self$evaluate)
        return(evalList)
      } else {
        gsAvail <- names(input) %in% names(self$goldStandards)
        if (all(!gsAvail)) {
          gsNames <- names(self$goldStandards)
          cli::cli_abort(c(
            "None of the specified gold standards are available in this object.",
            "i" = (
              "Add it using {.code Trio$addGS(.)} or choose from {.val {gsNames}}"
            )
          ))
        }
        if (any(!gsAvail)) {
          unavail <- names(input)[!gsAvail]
          cli::cli_inform(c(
            paste0(
              "Gold standard{?s} {.val {unavail}} from {.var input} {?is/are} ",
              "not available in this object."
            ),
            "i" = "Evaluating the following: {.var {names(input)[gsAvail]}}"
          ))
        }

        # subset to inputs with available GS
        input <- input[gsAvail]

        # compute/retrive gold standards
        gs <- setNames(
          lapply(names(input), self$getGS) |> unlist(recursive = FALSE),
          names(input)
        )

        # get a list of metrics to compute for each gold standard in the data
        metrics <- setNames(lapply(names(input), self$getMetrics), names(input))

        # get a flat list of available metrics
        allMetrics <- metrics |>
          unlist() |>
          unique()
        # find metrics that are not available in the trio
        unavailMetrics <- allMetrics[!allMetrics %in% names(self$metrics)]

        if (length(unavailMetrics) == length(allMetrics)) {
          cli::cli_abort(c(
            paste0(
              "None of the metrics related to the gold standards being evalutaed",
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
            \(gsMetrics) Filter(\(x) !x %in% unavailMetrics, gsMetrics)
          )
        }

        # compute each metric for each input
        purrr::imap(input, function(to_eval, gsName) {
          res <- lapply(
            metrics[[gsName]],
            function(x) self$metrics[[x]](to_eval, gs[[gsName]])
          )
          setNames(res, metrics[[gsName]])
        })
      }
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
          # TODO: Tell the user how to list the avaiable datasets
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

      lapply(files, loadFile)
    },
    populateTrio = function() {
      # get the gold standard metadata from curated trio datasets
      gSMetaData <- suppressMessages(googlesheets4::read_sheet(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        sheet = "Dataset-Gold Standard",
      ) |>
        dplyr::filter(datasetID == private$datasetID))

      goldStandards <- gSMetaData |> purrr::pluck("Gold Standard")

      metrics <- suppressMessages(googlesheets4::read_sheet(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        sheet = "Task-GS Type-Metric",
      ) |>
        dplyr::filter(`GS Type` %in% goldStandards) |>
        dplyr::select(`GS Type`, `MetricID`))

      # add each gold standard with it's respective metrics
      apply(gSMetaData, 1, \(gs) {
        if (gs["is_in_data"]) {
          if (gs["type"] == "column") {
            self$addGS(
              name = gs["Gold Standard"],
              gs = \() self$data[[1]][gs["name"]],
              metrics = metrics |>
                dplyr::filter(`GS Type` == gs["Gold Standard"]) |>
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
