# Silence check notes about R6 class
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
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
#' @return A Trio object
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
    #' @param data An object to use as the Trio dataset.
    #' @param dataLoader
    #'   A custom loading fuction that takes the path of a downloaded file and
    #'   returns a single dataset, ready to be used in evaluation tasks.
    #' @param cachePath The path to the data cache
    #' @param verbose Set the verbosity of Trio
    initialize = function(datasetID = NULL,
                          data = NULL,
                          dataLoader = NULL,
                          cachePath = FALSE,
                          verbose = FALSE) {
      googlesheets4::gs4_deauth()

      if (!verbose) {
        options(rlib_message_verbosity = "quiet")
      } else {
        options(rlib_message_verbosity = "default")
      }

      # if users have their own data without datasetID
      if (!is.null(data)) {
        if (is.null(datasetID) && interactive()) {
          self$dataSourceID <- readline("Choose a name for this data: ")
        } else if (is.null(datasetID) && !interactive()) {
          cli::cli_abort(c(
            "No {.var datasetID} was provided.",
            "i" = "Please pass datasetID when creating Trio non-interactively."
          ))
        } else if (!is.null(datasetID)) {
          self$dataSourceID <- datasetID
        } else {
          cli::cli_abort(c(
            "No {.var datasetID} was provided.",
            "i" = "Please pass a datasetID when creating Trio with local data."
          ))
        }
        self$data <- data
        return(NULL)
      }

      if (is.null(datasetID)) {
        if (!interactive()) {
          cli::cli_abort(
            "When Trio is initialised non-interactively, a {.val datasetID} must be specified."
          )
        }
        # prompt users to input their own new datasetID
        datasetID <- readline(
          prompt = "If you don't have a Figshare/GEO/ExperimentHub datasetID, please provide a new datasetID: "
        )
      }
      # parse user input and set dataSource and dataSourceID
      private$parseIDString(datasetID)

      self$cachePath <- getTrioCachePath(cachePath)
      self$data <- private$getData(
        self$dataSource, self$dataSourceID, self$cachePath, dataLoader
      )
      if (!is.null(private$datasetID)) {
        private$populateTrio()
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
    #' @param splitIndex
    #'   An optional index for subsetting data during evaluation using the
    #'   indices created by the split method.
    evaluate = function(input, splitIndex = NULL) {
      # check if splitIndex is provided but splitIndices is not present
      if (!is.null(splitIndex) && is.null(self$splitIndices)) {
        cli::cli_abort(c(
          "splitIndex is provided but self$splitIndices is NULL.",
          "i" = "Try running {.code trio$split(...)} to generate splits first."
        ))
      }

      # check if the requested auxData are available
      auxDataAvail <- names(input) %in% names(self$auxData)


      # if input list contains no auxData names, check if first sublist
      # contains auxData names and set separateMethods based on this
      if (all(!auxDataAvail)) {
        if (any(names(input[[1]]) %in% names(self$auxData))) {
          cli::cli_inform(c(
            "AuxData names found in sublist.",
            "i" = "Evaluating as separate methods."
          ))
          separateMethods <- TRUE
        }
      } else {
        separateMethods <- FALSE
      }

      # check if auxiliary data is available for each element of the input.
      if (separateMethods) {
        evalList <- lapply(input, self$evaluate, splitIndex = splitIndex)
        return(
          purrr::list_rbind(evalList, names_to = "method") %>%
            dplyr::select(datasetID, dplyr::everything())
        )
      } else {
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
              "not available in this object. Passing through as unevaluated ",
              "benchmark data."
            ),
            "i" = "Evaluating the following: {.var {names(input)[auxDataAvail]}}"
          ))
        }

        # compute/retrieve auxiliary data
        auxData <- setNames(
          lapply(names(input[auxDataAvail]), self$getAuxData),
          names(input[auxDataAvail])
        )

        # get a list of metrics to compute for each gold standard in the data
        metrics <- setNames(
          lapply(names(input[auxDataAvail]), self$getMetrics),
          names(input[auxDataAvail])
        )

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
              "None of the metrics related to the auxiliary data being evaluated",
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

        # subset auxiliary data based on splitIndex if provided
        if (!is.null(splitIndex) && !is.null(self$splitIndices)) {
          auxData <- lapply(
            auxData, function(data) {
              indices <- self$splitIndices[[splitIndex]]
              if (is.data.frame(data) || is.matrix(data) || is(data, "DataFrame")) {
                return(data[-indices, , drop = FALSE])
              } else if (is.vector(data) || is.factor(data) || is.list(data)) {
                return(data[-indices])
              } else {
                cli::cli_abort(c(
                  "Unsupported data type.",
                  "x" = "Only vectors and tabular data are supported for auxData subsetting.",
                  "i" = "Try adding pre-subsetted auxData to the Trio for evaluation."
                ))
              }
            }
          )
        }
        # compute each metric for each input
        res <- purrr::imap(input, function(to_eval, auxDataName) {
          if (is.null(metrics[[auxDataName]])) {
            return(to_eval)
          }
          res <- lapply(
            metrics[[auxDataName]],
            function(x) {
              metric_res <- self$metrics[[x]](to_eval, auxData[[auxDataName]])
              if (length(metric_res) > 1) {
                cli::cli_abort(c(
                  "The result for the {.val {x}} metric is not a single value.",
                  "i" = "Please ensure that all your metrics only output a single value."
                ))
              }
              metric_res
            }
          )
          setNames(res, metrics[[auxDataName]])
        })
        purrr::map(names(res), function(metric_name) {
          metricValues <- res[[metric_name]]

          # Create a data frame for each metric
          tibble::tibble(
            datasetID = self$dataSourceID,
            auxData = metric_name,
            metric = names(metricValues),
            result = unlist(metricValues)
          )
        }) %>% purrr::list_rbind()
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
    #' @importFrom splitTools create_folds
    #' @importFrom cli cli_inform
    #' @importFrom utils askYesNo
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
        type = dplyr::if_else(stratify, "stratified", "basic"),
        m_rep = n_repeat
      )
    },

    #' @description
    #' Print method to display key information about the Trio object.
    print = function() {
      data_str <- capture.output(str(self$data, max.level = 1))
      data_str <- setNames(data_str, rep(" ", times = length(data_str)))
      split_ind <- ifelse(is.null(self$splitIndices), "None", "Available")

      msg <- cli::cli_fmt({
        cli::cli_h1("Trio Object")

        cli::cli_h3("Dataset")
        cli::cli_text("{.strong Dataset Details}:")
        cli::cli_bullets(data_str)
        cli::cli_text("{.strong Dataset ID}: {.val {self$dataSourceID}}")
        cli::cli_text("{.strong Data Source}: {.val {self$dataSource}}")
        cli::cli_text("{.strong Cache Path}: {.val {self$cachePath}}")
        cli::cli_text("{.strong Split Indices}: {.val {split_ind}}")

        cli::cli_h3("Auxilliary Data")
        cli::cli_text("{.strong Number of Auxiliary Data}: {.val {length(self$auxData)}}")
        cli::cli_text("{.strong Names of Auxiliary Data}: {.val {names(self$auxData)}}")

        cli::cli_h3("Metrics")
        cli::cli_text("{.strong Number of Metrics}: {.val {length(self$metrics)}}")
        cli::cli_text("{.strong Names of Metrics}: {.val {names(self$metrics)}}")
      })

      cat(msg, sep = "\n")
    }
  ),
  private = list(
    datasetID = NULL,
    CTDlinl = "{.href [Curated Trio Datasets](https://docs.google.com/spreadsheets/d/1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY/)}",
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
          grep("Dl", ls("package:BenchHub"), value = TRUE), "Dl"
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
    getData = function(sourceName, id, cachePath, dataLoader) {
      # TODO: allow the user to input a custom loading function.
      files <- do.call(
        paste0(sourceName, "Dl"),
        list("ID" = id, "cachePath" = cachePath)
      )

      if (length(files) > 1) {
        cli::cli_inform("Select a file to load as the dataset:")
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
    populateTrio = function() {
      if (!curl::has_internet()) {
        cli::cli_warn(c(
          "Couldn't populate Trio from Curated Trio Datasets.",
          "Check your internet connection and try again."
        ))
        return(NULL)
      }
      # get the gold standard metadata from curated trio datasets
      auxDataMetaData <- suppressMessages(googlesheets4::read_sheet(
        ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
        sheet = "Dataset-AuxData",
      ) |>
        dplyr::filter(datasetID == private$datasetID))

      if (nrow(auxDataMetaData) == 0) {
        cli::cli_warn(c(
          paste0(self$CTDlink, " has no auxData for this dataset."),
          "i" = "Please add your own auxData for evaluation."
        ))
        return(NULL)
      }

      auxData <- auxDataMetaData |> purrr::pluck("Auxiliary Data")

      # get the relevant metrics and respective informaiton from the sheet.
      metrics <- suppressMessages(
        googlesheets4::read_sheet(
          ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
          sheet = "Task-AuxData Type-Metric",
        ) |>
          dplyr::filter(`AuxData Type` %in% auxData) %>%
          dplyr::left_join(
            .,
            googlesheets4::read_sheet(
              ss = "1zEyB5957aXYq6LvI9Ma65Z7GStpjIDWL16frru73qiY",
              sheet = "Metrics",
            )
          )
      )

      # create metrics inside the object
      apply(metrics, 1, \(metric) {
        if (metric["Metric Type"] == "internal") {
          self$addMetric(
            name = metric["MetricID"][[1]],
            metric = match.fun(metric["wrapper.r"][[1]])
          )
        } else {
          # TODO: Support external metrics.
          cli::cli_abort(c(
            "External metrics are not yet supported."
          ))
        }
      })

      # add each gold standard with it's respective metrics
      apply(auxDataMetaData, 1, \(auxData) {
        if (auxData["is_in_data"]) {
          if (auxData["type"] == "columns") {
            auxDataCols <- unlist(strsplit(auxData["name"], ", ", TRUE))
            self$addAuxData(
              name = auxData["Auxiliary Data"],
              # TODO: make it so the data is accessed by, rather stored in the
              #       auxData
              auxData = self$data[, auxDataCols],
              metrics = metrics |>
                dplyr::filter(`AuxData Type` == auxData["Auxiliary Data"]) |>
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
