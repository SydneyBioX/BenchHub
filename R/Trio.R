Trio <- R6::R6Class(
  "Trio",
  public = list(
    data = NULL,
    initialize = function(datasetID) {
      self$data <- getData(datasetID)
    }
  )
)
