#' @importFrom R6 R6Class
NULL

#' A benchmarkInsights object
#' @description An object containing a benchmark result for evaluating
#'   analytical tasks.
#' @field evalSummary The evaluation summary is stored by dataframe, where
#'   each row is the compared identifier, each columns are the metric used in 
#'   the evaluation task
#'   
#' @examples
#' TODO
#' @export

benchmarkInsights <- R6::R6Class(
  classname = "benchmarkInsights",
  
  public = list(
    evalSummary = NULL,  # Placeholder for evalSummary
    
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
        # Convert existing evalSummary to a data frame if necessary
        self$evalSummary <- as.data.frame(self$evalSummary, stringsAsFactors = FALSE)
        
        # Append new results to evalSummary
        self$evalSummary <- rbind(self$evalSummary, result_df)
      }
    }
  ),
  
  active = list(
    # Active binding for evalSummary that just returns it
    evalSummaryActive = function() {
      if (is.null(self$evalSummary)) {
        return(NULL)
      }
      return(self$evalSummary)
    }
  )
)
