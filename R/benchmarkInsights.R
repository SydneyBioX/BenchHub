#' @importFrom R6 R6Class
NULL

#' A benchmarkInsights object
#' @description An object containing a benchmark result for evaluating
#'   analytical tasks.
#' @field evalSummary The evaluation summary is stored by dataframe, where
#'   each row is the compared identifier, each column is the metric used in 
#'   the evaluation task.
#' @field metadata A dataframe to store metadata for the benchmark.
#'   
#' @examples
#' TODO
#' @export
benchmarkInsights <- R6::R6Class(
  classname = "benchmarkInsights",
  
  public = list(
    evalSummary = NULL,   # Placeholder for evalSummary
    metadata = NULL,      # Placeholder for metadata
    
    #' @description Adds evaluation results to evalSummary.
    #' @param evalResult A list containing evaluation results.
    #' @param dataID A character string representing the dataset ID.
    addevalSummary = function(evalResult, dataID) {
      # Convert the evalResult list into a data frame
      result_df <- do.call(rbind, lapply(names(evalResult), function(compare) {
        do.call(rbind, lapply(names(evalResult[[compare]]), function(GS) {
          do.call(rbind, lapply(names(evalResult[[compare]][[GS]]), function(expected) {
            metric_name <- names(evalResult[[compare]][[GS]][[expected]])
            result_value <- evalResult[[compare]][[GS]][[expected]][[metric_name]]
            data.frame(datasetID = dataID, GS = GS, Compare = compare, metric = metric_name, result = result_value, stringsAsFactors = FALSE)
          }))
        }))
      }))
      
      # Ensure result_df is always a data frame
      result_df <- as.data.frame(result_df, stringsAsFactors = FALSE)
      
      # If evalSummary is NULL, initialize it as the new result_df
      if (is.null(self$evalSummary)) {
        self$evalSummary <- result_df
      } else {
        # Append new results to evalSummary
        self$evalSummary <- rbind(self$evalSummary, result_df)
      }
    },
    
    #' @description Adds metadata to the metadata field.
    #' @param metadata A dataframe containing metadata information.
    addMetadata = function(metadata) {
      # Ensure the input is a dataframe
      if (!is.data.frame(metadata)) {
        stop("Metadata must be a dataframe.")
      }
      
      # If metadata is NULL, initialize it as the new metadata dataframe
      if (is.null(self$metadata)) {
        self$metadata <- metadata
      } else {
        # Append the new metadata to the existing metadata
        self$metadata <- rbind(self$metadata, metadata)
      }
    }
  )
)
