#' @importFrom R6 R6Class
NULL

#' A RecSys object
#' @description An object containing a recommender system for data-driven 
#'   specific task.
#' @field evalSummary The evaluation summary is stored by dataframe, where
#'   each row is the compared identifier, each column is the metric used in 
#'   the evaluation task and related information.
#' @field metadata A dataframe to store metadata for the benchmark.
#'   
#' @examples
#' # TODO
#' @export
RecSys <- R6::R6Class(
  classname = "RecSys",
  
  public = list(
    evalSummary = NULL,   # Placeholder for evalSummary
    metadata = NULL

  )
)
