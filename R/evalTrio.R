eval_trio_factory <- R6Class(
  "evalTrio",
  private = list(
    ..evalData = NULL
  ),
  public = list(
    evalTaskList = NULL,
    metaInfo = NULL,
    initialize = function(eval_data, eval_task_list, meta_info) {
      if (!is.list(eval_task_list)) {
        stop("eval_task_list must be a list of evalTask objects!")
      }

      if (length(eval_task_list) == 0) {
        stop("There is no evalTask object in eval_task_list! Check your list of evalTask objects!")
      } else {
        for (i in 1:length(eval_task_list)) {
          if (class(eval_task_list[[i]])[[1]] != "evalTask") {
            stop("eval_task_list must be a list of evalTask class objects!")
          }
        }
      }

      if (!is.list(meta_info)) {
        stop("meta_info must be a list of meta information about eval_data!")
      } else {
        if (length(meta_info) == 0) {
          warning("There is no meta information in meta_info! Meta information about evalData will not be displayed.")
        }
      }

      private$..evalData <- eval_data
      self$evalTaskList <- eval_task_list
      self$metaInfo <- meta_info
    }
  ),
  active = list(
    evalData = function() {
      private$..evalData
    }
  )
)
