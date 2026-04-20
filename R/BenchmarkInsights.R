#' @importFrom R6 R6Class
#' @import ggplot2
NULL

#' A BenchmarkInsights object
#' @description An object containing a benchmark result for evaluating
#'   analytical tasks.
#' @field evalSummary The evaluation summary is stored by dataframe, where
#'   each row is the methodd identifier, each column is the metric used in
#'   the evaluation task and related information.
#' @field metadata A dataframe to store metadata for the benchmark.
#'
#' @return A BenchmarkInsights object.
#' @examples
#' BenchmarkInsights$new()
#' @export
BenchmarkInsights <- R6::R6Class(
  classname = "BenchmarkInsights",

  public = list(
    evalSummary = NULL,
    metadata = NULL,

    #' @description
    #' Create a BenchmarkInsights object
    #' @param evalResult
    #'   A dataframe containing initial evaluation results with columns such as datasetID, evidence, metric, and result.
    initialize = function(evalResult = NULL) {
      if (!is.null(evalResult)) {
        if (!is.data.frame(evalResult)) {
          stop("evalResult must be a dataframe.")
        }
        self$evalSummary <- evalResult
      } else {
        self$evalSummary <- data.frame()
      }
    },

    #' @description
    #' Add additional evaluation summary to the existing evalSummary
    #' @param additional_evalResult
    #'   A dataframe containing additional evaluation results to be appended.
    addevalSummary = function(additional_evalResult) {
      if (!is.data.frame(additional_evalResult)) {
        stop("additional_evalResult must be a dataframe.")
      }

      if (is.null(self$evalSummary) || nrow(self$evalSummary) == 0) {
        self$evalSummary <- additional_evalResult
      } else {
        self$evalSummary <- rbind(self$evalSummary, additional_evalResult)
      }
    },

    #' @description
    #' Add metadata to the BenchmarkInsights object
    #' @param metadata
    #'   A dataframe containing metadata information.
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
    #' @importFrom reshape2 dcast
    #' @return A heatmap object.

    getHeatmap = function() {
      if (!requireNamespace("reshape2", quietly = TRUE)) {
        cli::cli_abort(c(
          "Install {.pkg reshape2}.",
          "i" = "You can get it by running: {.code install.packages('reshape2')}"
        ))
      }
      evalSummary <- self$evalSummary
      if (is.null(evalSummary) || nrow(evalSummary) == 0) {
        stop("Evaluation summary is required to generate heatmap.")
      }
      # Average results across datasets by evidence, method, and metric

      evalSummary$result <- -evalSummary$result
      averaged_df <- evalSummary %>%
        dplyr::group_by(evidence, method, metric) %>%
        dplyr::summarise(avg_result = mean(result, na.rm = TRUE)) %>%
        dplyr::ungroup()

      # Detect if there are any duplicated evidence names across different metrics
      averaged_df <- averaged_df %>%
        dplyr::mutate(
          # Check if there are duplicate evidence names, and if so, combine evidence and metric to distinguish them
          evidence_metric = ifelse(
            duplicated(evidence) | duplicated(evidence, fromLast = TRUE),
            paste(evidence, metric, sep = "_"),
            evidence
          )
        )

      # Reshape the data into a wide format where method is the row and evidence_metric is the column
      reshaped_df <- reshape2::dcast(
        averaged_df,
        method ~ evidence_metric,
        value.var = "avg_result"
      ) %>%
        replace(is.na(.), 0)
      
      

      # Set method as rownames and remove the method column
      rownames(reshaped_df) <- reshaped_df$method
      reshaped_df <- reshaped_df %>%
        dplyr::rename(id = method)
      
      cinfo <- tibble::tibble(
        id = colnames(reshaped_df),
        group = c(NA_character_, rep("", ncol(reshaped_df) - 1)),
        options = replicate(ncol(reshaped_df), rlang::list2(), simplify = FALSE)
      )
      
      cinfo$palette <- c(NA, rep("performance_score", ncol(reshaped_df) - 1))
      
      palettes <- list(performance_score = "Blues")
      
      heatmap <- suppressMessages(suppressWarnings(funkyheatmap::funky_heatmap(
        reshaped_df, column_info = cinfo, palettes = palettes
      )))
      

      return(heatmap)
    },

    #' @description Creates a line plot for the given x and y variables, with an optional grouping and fixed x order.
    #' @param order An optional vector specifying the order of x-axis values.
    #' @param metricVariable Specify subset value in metric column.
    #' @return A ggplot2 line plot object.
    getLineplot = function(order = NULL, metricVariable) {
      evalResult <- self$evalSummary
      if (!is.data.frame(evalResult)) {
        stop("Input data must be a dataframe.")
      }

      evalResult <- evalResult[evalResult$metric == metricVariable, ]

      th <- ggplot2::theme(
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(
          colour = "black",
          linewidth = 0.2,
          fill = NA
        )
      )

      evalResult_aggreate <- evalResult %>%
        dplyr::group_by(evidence, method) %>%
        dplyr::summarise(average_result = mean(result, na.rm = TRUE)) %>%
        dplyr::ungroup()

      if (!is.null(order)) {
        evalResult_aggreate$evidence <- factor(
          evalResult_aggreate$evidence,
          levels = order
        )
      }

      evalResult_aggreate <- evalResult_aggreate %>%
        dplyr::mutate(evidence_numeric = as.numeric(evidence)) %>%
        dplyr::arrange(evidence_numeric) %>%
        dplyr::mutate(
          evidence = factor(
            evidence,
            levels = unique(evidence[order(evidence_numeric)])
          )
        )

      plot <- ggplot(
        evalResult_aggreate,
        aes(x = evidence, y = average_result, group = method, color = method)
      ) +
        labs(x = "dataset size", y = metricVariable, fill = "method") +
        geom_point() +
        geom_line() +
        th

      return(plot)
    },

    #' @description Creates a scatter plot for the same evidence, with an two method metrics.
    #' @param variables A character vector of length two specifying the metric names to be used for the x and y axes.
    #' @return A ggplot2 line plot object.
    #' @importFrom ggrepel geom_label_repel
    getScatterplot = function(variables) {
      evalResult <- self$evalSummary
      if (!is.data.frame(evalResult)) {
        stop("Input data must be a dataframe.")
      }

      th <- ggplot2::theme(
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(
          colour = "black",
          linewidth = 0.2,
          fill = NA
        )
      )

      evalResult_aggreate <- evalResult %>%
        dplyr::group_by(method, metric) %>%
        dplyr::summarise(average_result = mean(result, na.rm = TRUE)) %>%
        dplyr::ungroup()

      metric_types <- unique(evalResult_aggreate$metric)

      result <- evalResult_aggreate %>%
        tidyr::pivot_wider(names_from = metric, values_from = average_result)

      plot <- #ggplot(result, aes(x = sensitivity, y = specificity, label = method)) +
        ggplot(
          result,
          aes(x = get(variables[1]), y = get(variables[2]), label = method)
        ) +
        geom_point(alpha = 0.4) +
        ggrepel::geom_label_repel(
          size = 3,
          show.legend = FALSE,
          aes(colour = method)
        ) +
        coord_fixed(ratio = 1, xlim = c(0, NA), ylim = c(0, NA)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        th +
        ylab(variables[2]) +
        xlab(variables[1])

      return(plot)
    },
    #' @description Creates boxplot plots for the mutiple evidence, different method, one metric.
    #' @param metricVariable Specify subset value in metric column.
    #' @param evidenceVariable Specify subset value in evidence column.
    #' @return A ggplot2 line plot object.
    #' @importFrom ggsci scale_fill_npg
    getBoxplot = function(metricVariable, evidenceVariable) {
        evalResult <- self$evalSummary
      if (!is.data.frame(evalResult)) {
        stop("Input data must be a dataframe.")
      }

      subsetData <- evalResult[
        evalResult$metric == metricVariable &
          evalResult$evidence == evidenceVariable,
      ]

      if (nrow(subsetData) == 0) {
        stop("No data available for the given metric and evidence values.")
      }

      p1 <- ggplot(subsetData, aes(x = method, y = result, fill = method)) +
        geom_boxplot() +
        labs(
          x = "Method",
          y = metricVariable
        ) +
        facet_wrap(~evidence, scale = "free") +
        theme(
          text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            colour = "black",
            linewidth = 0.2,
            fill = NA
          )
        )

      return(p1)
    },

    #' @description Creates a correlation plot based on the provided evaluation summary and the specified input type (either "evidence", "metric", or "method").
    #' The correlation plot shows the pairwise correlation between results for different categories (evidence, metric, or method).
    #' @param input_type A string that specifies the input type for generating the correlation plot. It must be either "evidence", "metric", or "method".
    #' @return A ggplot2 correlation plot object. The plot visualizes the correlation matrix using ggcorrplot with aesthetic enhancements like labeled values and angled axis text.
    #' @importFrom ggcorrplot ggcorrplot
    getCorplot = function(input_type) {
      evalResult <- self$evalSummary
      if (!is.data.frame(evalResult)) {
        stop("Input data must be a dataframe.")
      }

      if (!input_type %in% c("evidence", "metric", "method")) {
        stop("Invalid input_type. Must be 'evidence', 'metric', or 'method'.")
      }

      df <- evalResult

      if (input_type == "metric") {
        pivot_df <- df %>%
          dplyr::select(datasetID, method, metric, result) %>%
          tidyr::pivot_wider(
            names_from = metric,
            values_from = result,
            values_fn = mean
          )
      } else if (input_type == "evidence") {
        pivot_df <- df %>%
          dplyr::select(datasetID, method, evidence, result) %>%
          tidyr::pivot_wider(
            names_from = evidence,
            values_from = result,
            values_fn = mean
          )
      } else if (input_type == "method") {
        pivot_df <- df %>%
          dplyr::select(datasetID, evidence, method, result) %>%
          tidyr::pivot_wider(
            names_from = method,
            values_from = result,
            values_fn = mean
          )
      }

      cor_matrix <- pivot_df %>%
        select_if(is.numeric) %>%
        cor(use = "pairwise.complete.obs")

      p1 <- suppressWarnings(ggcorrplot::ggcorrplot(
        cor_matrix,
        method = "square",
        type = "lower",
        lab = TRUE
      )) +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(0, 1),
          legend.direction = "horizontal"
        ) +
        guides(
          fill = guide_colorbar(
            barwidth = 7,
            barheight = 1,
            title.position = "top",
            title.hjust = 0.5
          )
        )
      return(p1)
    },

    #' @description This function generates a forest plot using linear models based on the
    #' comparison between groups in the provided evaluation summary. The plot is created
    #' using dotwhisker and broom packages, with custom grouping and labeling.
    #' @param input_group A string specifying the grouping variable (only "datasetID", "method", or "evidence" allowed).
    #' @param input_model A string specifying the model variable (only "datasetID", "method", or  "evidence" allowed).
    #' @return A forest plot showing the comparison of models across groups.
    #' @importFrom broom tidy
    #' @importFrom dotwhisker relabel_predictors
    getForestplot = function(input_group, input_model) {
      evalResult <- self$evalSummary
      allowed_values <- c("datasetID", "method", "evidence", "metric")
      if (!input_group %in% allowed_values) {
        stop(
          "Invalid input_group. Must be 'datasetID', 'method', 'evidence' or 'metric'."
        )
      }
      if (!input_model %in% allowed_values) {
        stop(
          "Invalid input_model. Must be 'datasetID', 'method', 'evidence' or 'metric'."
        )
      }

      to_plot <- evalResult %>%
        group_by(!!sym(input_group)) %>%
        dplyr::do(broom::tidy(lm(result ~ !!sym(input_model), data = .)))

      colnames(to_plot)[1] <- "model"

      predictor_labels <- to_plot$term %>%
        unique() %>%
        rlang::set_names(., .)

      if ('(Intercept)' %in% predictor_labels) {
        predictor_labels['(Intercept)'] <- paste0(input_model, " (Intercept)")
      }

      to_plot <- dotwhisker::relabel_predictors(to_plot, predictor_labels)

      g <- suppressWarnings(dotwhisker::dwplot(
        to_plot,
        vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)
      )) +
        labs(
          x = "Regression coefficient"
        ) +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            colour = "black",
            linewidth = 1,
            fill = NA
          )
        )

      return(g)
    }
  )
)
