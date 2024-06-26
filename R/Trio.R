#' A Trio object
#' @description An object containing a dataset and methods for evaluating
#'   analytical task against ground truths in the dataset.
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
    data = NULL,
    goldStandards = list(),
    metrics = list(),

    # TODO: Implement Trio$sources() (Issue #2)

    #' @description
    #' Create a Trio object
    #' @param datasetID
    #'   A string specifying a dataset, either a name from curated-trio-data or
    #'   a format string of the form `source`:`source_id`.
    initialize = function(datasetID) {
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
          "A gold standard `{name}` is already present in this Trio, overwriting."
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
    addMetric = function(name, metric, args) {
      if (!methods::is(metric, "function")) {
        cli::cli_abort(c(
          "{.var metric} should be a {.cls function}, not a {.cls {class(metric)}}."
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
          "i" = "Add it using {.code Trio$addGS(.)} or choose one of {.val {gsNames}}"
        ))
      }

      gs <- self$goldStandards[[name]]$gs

      if (!methods::is(gs, "function")) {
        return(gs)
      }

      gs(self$data)
    }
  )
)
