#' @importFrom R6 R6Class
#' @import ggplot2
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
#' # TODO
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
#' @importFrom reshape2 dcast    
#' @return A heatmap object.
  
    
    getHeatmap = function(evalSummary) {
      if (!requireNamespace("reshape2", quietly = TRUE)) {
        cli::cli_abort(c(
        "Install {.pkg reshape2}.",
        "i" = "You can get it by running: {.code install.packages('reshape2')}"
        ))
      }
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
    
    #' @description Creates a line plot for the given x and y variables, with an optional grouping and fixed x order.
    #' @param minievalSummary subset of evaluation summary
    #' @param order An optional vector specifying the order of x-axis values.
    #' @return A ggplot2 line plot object.
    getLineplot = function(minievalSummary, order = NULL) {
      if (!is.data.frame(minievalSummary)) {
        stop("Input data must be a dataframe.")
      }
      
      th <- ggplot2::theme(text=element_text(size=12),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(colour = "black", linewidth = 0.2, fill = NA))

      minievalSummary_aggreate <- minievalSummary %>%
        group_by(GS, Compare, metric) %>%
        summarise(average_result = mean(result, na.rm = TRUE)) %>%
        ungroup()

      if (!is.null(order)) {
        minievalSummary_aggreate$GS <- factor(minievalSummary_aggreate$GS, levels = order)
      }
      
      # Create the line plot
      plot <- ggplot(minievalSummary_aggreate, 
                     aes(x = GS, 
                         y = average_result, 
                         group = Compare, 
                         color = Compare)) +
        labs(x = "gold standard", y = "average_value", fill = "compare") +
        geom_point() +
        geom_line() +
        th
      
      return(plot)
    },
    
    #' @description Creates a scatter plot for the same GS, with an two compared metrics.
    #' @param minievalSummary subset of evaluation summary, only include two different metrics, all GS should be same
    #' @return A ggplot2 line plot object.
    #' @importFrom ggrepel geom_label_repel
    getScatterplot = function(minievalSummary) {
      if (!is.data.frame(minievalSummary)) {
        stop("Input data must be a dataframe.")
      }
    
      minievalSummary_aggreate <- minievalSummary %>%
        group_by(Compare, metric) %>%
        summarise(average_result = mean(result, na.rm = TRUE)) %>%
        ungroup()
      
      metric_types <- unique(minievalSummary_aggreate$metric)
      
      result <- minievalSummary_aggreate %>%
        tidyr::pivot_wider(names_from = metric, values_from = average_result) %>%
        rename(metric_a = !!metric_types[1], metric_b = !!metric_types[2])
      
      plot <- ggplot(result, aes(x = metric_a, y = metric_b, label = Compare)) +
              geom_point(alpha = 0.4) + 
              ggrepel::geom_label_repel(size = 3, show.legend = FALSE, aes(colour = Compare)) +
              coord_fixed(ratio = 1, xlim = c(0, NA), ylim = c(0, NA)) + 
              scale_x_continuous(expand = c(0, 0)) +
              scale_y_continuous(expand = c(0, 0)) +
              theme_minimal() +
              ylab(metric_types[2]) + 
              xlab(metric_types[1])
      
      return(plot)
    },
    
    #' @description Creates boxplot plots for the mutiple GS, different Compare, one metric.
    #' @param minievalSummary subset of evaluation summary, only include two different metrics, all GS should be same
    #' @return A ggplot2 line plot object.
    #' @importFrom ggsci scale_fill_npg
    getBoxplot = function(minievalSummary) {
      if (!is.data.frame(minievalSummary)) {
        stop("Input data must be a dataframe.")
      }
      
      p1 <- ggplot(minievalSummary, aes(x=Compare, y=result, fill=Compare)) + 
        geom_boxplot() +
        facet_wrap(~GS, scale="free") +
        theme(text=element_text(size=12 ),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(colour = "black", linewidth = 0.2, fill = NA)) +
        ggsci::scale_fill_npg()

       return(p1)
    },
    
    #' @description Creates a correlation plot based on the provided evaluation summary and the specified input type (either "GS", "metric", or "Compare").
    #' The correlation plot shows the pairwise correlation between results for different categories (GS, metric, or Compare).
    #' @param minievalSummary A subset of the evaluation summary. It must include columns relevant to the input type (GS, metric, Compare) and the result values.
    #' @param input_type A string that specifies the input type for generating the correlation plot. It must be either "GS", "metric", or "Compare".
    #' @return A ggplot2 correlation plot object. The plot visualizes the correlation matrix using ggcorrplot with aesthetic enhancements like labeled values and angled axis text.
    #' @importFrom ggcorrplot ggcorrplot
    getCorplot = function(minievalSummary, input_type) {
      
      if (!is.data.frame(minievalSummary)) {
        stop("Input data must be a dataframe.")
      }
      
      if (!input_type %in% c("GS", "metric", "Compare")) {
        stop("Invalid input_type. Must be 'GS', 'metric', or 'Compare'.")
      }
      
      df <- minievalSummary
      
      if (input_type == "metric") {
        pivot_df <- df %>%
          select(datasetID, Compare, metric, result) %>%
          tidyr::pivot_wider(names_from = metric, values_from = result, values_fn = mean)
        
      } else if (input_type == "GS") {
        pivot_df <- df %>%
          select(datasetID, Compare, GS, result) %>%
          tidyr::pivot_wider(names_from = GS, values_from = result, values_fn = mean)
        
      } else if (input_type == "Compare") {
        pivot_df <- df %>%
          select(datasetID, GS, Compare, result) %>%
          tidyr::pivot_wider(names_from = Compare, values_from = result, values_fn = mean)
      }
      
      cor_matrix <- pivot_df %>%
        select_if(is.numeric) %>%
        cor(use = "pairwise.complete.obs")
      
      p1 <- ggcorrplot::ggcorrplot(cor_matrix, method = "square", 
                                   type = "lower", 
                                   lab = TRUE, 
                                   colors = c("white", "grey", "black")) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
              panel.grid.major = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks = element_blank(),
              legend.justification = c(0, 1),   
              legend.direction = "horizontal") +
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", 
                                     title.hjust = 0.5))
      return(p1)
    },
    
    #' @description This function generates a forest plot using linear models based on the 
    #' comparison between groups in the provided evaluation summary. The plot is created
    #' using dotwhisker and broom packages, with custom grouping and labeling.
    #' @param minievalSummary A data frame containing the evaluation summary.
    #' @param input_group A string specifying the grouping variable (only "datasetID", "Compare", or "GS" allowed).
    #' @param input_model A string specifying the model variable (only "datasetID", "Compare", or "GS" allowed).
    #' @return A forest plot showing the comparison of models across groups.
    #' @importFrom broom tidy
    #' @importFrom dotwhisker relabel_predictors
    getForestplot = function(minievalSummary, input_group, input_model) {
      
      allowed_values <- c("datasetID", "Compare", "GS", "metric")
      if (!input_group %in% allowed_values) {
        stop("Invalid input_group. Must be 'datasetID', 'Compare', 'GS' or 'metric'.")
      }
      if (!input_model %in% allowed_values) {
        stop("Invalid input_model. Must be 'datasetID', 'Compare', 'GS' or 'metric'.")
      }
      
      # minievalSummary <- benchmark$evalSummary
      # input_group <- "metric"
      # input_model <- "Compare"
      
      to_plot <- minievalSummary %>%
        group_by(!!sym(input_group)) %>%
        do(broom::tidy(lm(result ~ !!sym(input_model), data = .))) %>%
        rename(model = !!sym(input_group))
      
      predictor_labels <- to_plot$term %>%
        unique() %>%
        rlang::set_names(., .)
      
      if ('(Intercept)' %in% predictor_labels) {
        predictor_labels['(Intercept)'] <- paste0(input_model, " (Intercept)")
      }
      
      to_plot <- dotwhisker::relabel_predictors(to_plot, predictor_labels)
      
      g <- dotwhisker::dwplot(to_plot,
                              vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
        theme_minimal() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(colour = "black", linewidth = 1, fill = NA))
      
      return(g)
    }
  )
)
