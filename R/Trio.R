Trio <- R6::R6Class(
  "Trio",
  public = list(
    data = NULL,
    goldStandards = list(),
    metrics = list(),
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
    #'   A list of metrics names used to campaere gs with the input to evaluate.
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
          "gs" = function(gs, args) {
            purrr::lmap(
              self$data,
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
    }
  )
)
