#' @importFrom R6 R6Class
NULL

#' A benchmarkInsights object
#' @description An object containing a benchmark result for evaluating
#'   analytical tasks.
#' @field evalSummary The evaluation summary is stored by dataframe, where
#'   each row is the compared identifier, each column is the metric used in 
#'   the evaluation task and related information.
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
    
    addevalSummary = function(evalResult, dataID) {
      result_df <- do.call(rbind, lapply(names(evalResult), function(compare) {
        do.call(rbind, lapply(names(evalResult[[compare]]), function(GS) {
          do.call(rbind, lapply(names(evalResult[[compare]][[GS]]), function(expected) {
            metric_name <- names(evalResult[[compare]][[GS]][[expected]])
            result_value <- evalResult[[compare]][[GS]][[expected]][[metric_name]]
            data.frame(datasetID = dataID, GS = GS, Compare = compare, metric = metric_name, result = result_value, stringsAsFactors = FALSE)
          }))
        }))
      }))
      
      result_df <- as.data.frame(result_df, stringsAsFactors = FALSE)
      
      if (is.null(self$evalSummary)) {
        self$evalSummary <- result_df
      } else {
        self$evalSummary <- rbind(self$evalSummary, result_df)
      }
    },
    
    addMetadata = function(metadata) {
      if (!is.data.frame(metadata)) {
        stop("Metadata must be a dataframe.")
      }
      
      if (is.null(self$metadata)) {
        self$metadata <- metadata
      } else {
        self$metadata <- rbind(self$metadata, metadata)
      }
    },
    
    #' @description Creates a heatmap from the evaluation summary by averaging results across datasets.
    #' @param evalSummary A dataframe containing the evaluation summary.
    #' @return A heatmap object.
    getHeatmap = function(evalSummary) {
      if (is.null(evalSummary)) {
        stop("Evaluation summary is required to generate heatmap.")
      }
      
      # Average results across datasets by GS, Compare, and metric
      averaged_df <- evalSummary %>%
        dplyr::group_by(GS, Compare, metric) %>%
        dplyr::summarise(avg_result = mean(result, na.rm = TRUE)) %>%
        dplyr::ungroup()
      
      # Detect if there are any duplicated GS names across different metrics
      averaged_df <- averaged_df %>%
        dplyr::mutate(
          # Check if there are duplicate GS names, and if so, combine GS and metric to distinguish them
          GS_metric = ifelse(duplicated(GS) | duplicated(GS, fromLast = TRUE), 
                             paste(GS, metric, sep = "_"), 
                             GS)
        )
      
      # Reshape the data into a wide format where Compare is the row and GS_metric is the column
      reshaped_df <- reshape2::dcast(averaged_df, Compare ~ GS_metric, value.var = "avg_result")
      
      # Set Compare as rownames and remove the Compare column
      rownames(reshaped_df) <- reshaped_df$Compare
      reshaped_df$Compare <- NULL
      heatmap <- funkyheatmap::funky_heatmap(reshaped_df)
      
      return(heatmap)
    },
    
    #' @description Creates a scatterplot for the given x and y variables, with an optional grouping.
    #' @param data A pre-processed dataframe containing the x, y, and optional group variables.
    #' @param x The x-axis variable (e.g., GS).
    #' @param y The y-axis variable (e.g., metric).
    #' @param group A grouping variable for coloring points (e.g., Compare).
    #' @return A ggplot2 scatterplot object.
    getScatterplot = function(minievalSummary) {
      if (!is.data.frame(minievalSummary)) {
        stop("Input data must be a dataframe.")
      }
      
      th <- theme(text=element_text(size=12),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(colour = "black", size=0.2, fill=NA))
      
      minievalSummary_aggreate <- minievalSummary %>%
        group_by(GS, Compare, metric) %>%
        summarise(average_result = mean(result, na.rm = TRUE)) %>%
        ungroup()
      
      plot <- ggplot(minievalSummary_aggreate, 
                     aes(x = minievalSummary_aggreate$GS, 
                         y = minievalSummary_aggreate$average_result, 
                         group = minievalSummary_aggreate$Compare, 
                         color = minievalSummary_aggreate$Compare)) +
                      geom_point() +
                      geom_line() +
                      th
    
      return(plot)
    }
    
  )
)
