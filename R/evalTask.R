eval_task_factory = R6Class(
  "evalTask",
  
  private = list(
    ..gsResult = NULL
  ),

  public = list(
    taskName = NULL,
    bestScore = NULL,
    worstScore = NULL,
    initialize = function(task_name, gs_result, best_score, worst_score) {
      if (missing(gs_result)) {
        stop("gs_result is missing!")
      } 
      private$..gsResult = gs_result
      
      self$taskName = task_name
      self$bestScore = best_score
      self$worstScore = worst_score
    },
    
    # get a subset of gsResult based on user input subset ids for stability measures
    gsResultSubset = function(subset_id) {
      private$..gsResult[subset_id]
    },
    
    evalMetric = function(task_fun, ...) {
      message("Note: task_arg_list must be a list of arguments to task_function.")
      
      if (!is.function(task_fun)) {
        stop("task_fun is not a function! Please provide an appropriate task function.")
      }
      
      do.call(task_fun, ...)
    }
  ),
  
  active = list(
    gsResult = function() {
      private$..gsResult
    }
  )
)
