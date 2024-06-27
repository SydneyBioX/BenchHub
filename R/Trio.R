#' A Trio object
#' @description An object containing a dataset and methods for evaluating
#'   analytical tasks against ground truths for the dataset.
#' @field data The data
#' @field goldStandards The gold standards in the data
#' @field metrics The metric for evaluating tasks against the gold standards
#'
#' @examples
#' trio <- Trio$new("figshare:26054188/47112109")
#' @export
Trio <- R6::R6Class(
  "Trio",
  public = list(
    cachePath = NULL,
    data = NULL,
    goldStandards = list(),
    metrics = list(),

    # TODO: Implement Trio$sources() (Issue #2)

    #' @description
    #' Create a Trio object
    #' @param datasetID
    #'   A string specifying a dataset, either a name from curated-trio-data or
    #'   a format string of the form `source`:`source_id`.
    initialize = function(datasetID, cachePath = NULL) {
      self$cachePath <- getTrioCachePath(cachePath)
      self$data <- getData(datasetID)
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
    evaluate = function(input) {
      # check if a gold standard is available for each element of the input.
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
      imap(input, function(to_eval, gsName) {
        res <- lapply(
          metrics[[gsName]],
          function(x) self$metrics[[x]](to_eval, gs[[gsName]])
        )
        setNames(res, metrics[[gsName]])
      })
    }
  )
)
