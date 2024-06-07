
source("./mpop1.R")
source("./mpop2.R")
source("./mpop3.R")
source("./mpop model.R")
source("./predict_mpop.R")
source("./evalTrio.R")
source("./evalTask.R")

# evalTask Object Generator
## a function that generates evalTask objects using the evalTask class
evalTask_obj_generator = function(task_name, gs_result) {
  # get both best and worst evaluation metric scores for classification tasks
  class_best_score = get_best_score(task_name)
  class_worst_score = get_worst_score(task_name)
  
  # generate evalTask objects using the inputs above
  eval_task_obj = eval_task_factory$new(task_name = task_name,
                                        gs_result = gs_result,
                                        best_score = class_best_score,
                                        worst_score = class_worst_score)
  
  return(eval_task_obj)
}


# evalTrio Object Generator
## a function that generates evalTrio objects using the evalTrio class (eval_trio_factory)
evalTrio_obj_generator = function(eval_data, data_name, organism, 
                                  research_area = NULL, ref = NULL, eval_task_list) {
  # create a nice named evalTask list
  list = list()
  for (i in 1:length(eval_task_list)) {
    task_name = eval_task_list[[i]]$taskName
    list[[task_name]] = eval_task_list[[i]]
  }
  
  # create metaInfo
  meta_info = list(Data = data_name,
                   Organism = organism,
                   `Research Area` = research_area,
                   Reference = ref,
                   `evalTrio Date` = Sys.Date())
  
  # generate an evalTrio object using the inputs above
  eval_trio_obj = eval_trio_factory$new(eval_data = eval_data,
                                        eval_task_list = list,
                                        meta_info = meta_info)
  
  return(eval_trio_obj)
}


# evalStudy Function
evalStudy = function(eval_trio_list, method_list, task_name, task_fun, args_list) {
  eval_result_ls = list()
  # for each data
  for (i in 1:length(eval_trio_list)) {
    data_name = names(eval_trio_list)[i] # data name
    eval_task_list = eval_trio_list[[data_name]]$evalTaskList
    eval_result = data.frame()
    
    # for each iteration
    for (j in 1:length(method_list)) {
      if (names(method_list)[j] == data_name) {
        # if the task name doesn't exist among the list of evalTask objects
        if (!task_name %in% names(eval_task_list)) {
          stop("task_name doesn't exist in the list of evalTask objects!")
        } else {
          eval_task = eval_task_list[[task_name]] # get the right evalTask object for the given task
          
          # for each method
          for (k in 1:length(method_list[[j]])) {
            # calculate accuracy evaluation metrics
            eval_metric = eval_task$evalMetric(task_fun = task_fun, args_list[[j]][[k]])
            
            # method name
            method_name = names(method_list[[j]])[k]
            
            eval_metric_df = tibble::as_tibble(eval_metric) %>% 
              dplyr::mutate(Method = method_name) %>% 
              dplyr::relocate(Method)
            
            eval_result = rbind(eval_result, eval_metric_df)
          }
        }
      }
    }
    
    eval_result_ls[[i]] = eval_result
    
    # # get and print an evaluation result table for each data
    # eval_result_tab = eval_result
    # eval_result_kable = eval_result_tab %>% 
    #   dplyr::group_by(Method) %>% 
    #   dplyr::summarize_if(is.numeric, mean, na.rm = TRUE, .names = "Mean_{.col}") %>% 
    #   kableExtra::kbl(digits = 4,
    #                   caption = paste(data_name, "Accuracy Performance Table"),
    #                   align = rep("c", ncol(eval_result_tab))) %>% 
    #   kableExtra::kable_classic_2(full_width = TRUE, 
    #                               html_font = "Times New Roman Bold", 
    #                               font_size = 15) %>% 
    #   kableExtra::row_spec(0, bold = T)
    # 
    # print(eval_result_kable)
    # 
    # p = eval_result_tab %>% 
    #   ggplot() +
    #   aes(x = Method, y = F1, fill = Method) +
    #   geom_boxplot(alpha = 0.7) +
    #   labs(x = "Method", y = "F1 Score", 
    #        title = paste(data_name, "F1 Score")) +
    #   scale_fill_viridis(discrete = TRUE,
    #                      option = "plasma") +
    #   theme(legend.position = "bottom")
    # 
    # print(p)
  }
  return(eval_result_ls)
}


# Best Score Generator
## get the best score of accuracy metrics
get_best_score = function(task_name) {
  if (tolower(task_name) == "classification") {
    best_score = list(`Best Accuracy` = 1,
                      `Best Sensitivity` = 1,
                      `Best Specificity` = 1,
                      `Best F1` = 1,
                      `Best Balanced Accuracy` = 1,
                      `Best AUC` = 1)
  } else if (tolower(task_name) == "clustering") {
    best_score = list(`Best Adjusted Rand Index` = 1,
                      `Best Average Silhouette Score` = 1,
                      `Best Calinski-Harabasz Index` = Inf)
  } else {
    best_score = list(`Best # of False Positives` = 0,
                      `Best Absolute Mean CPM SNR` = 0,
                      `Absolute SD CPM SNR` = 0,
                      `Absolute CV CPM SNR` = 0)
  }
  return(best_score)
}


# Worst Score Generator
## get the worst score of accuracy metrics
get_worst_score = function(task_name) {
  if (tolower(task_name) == "classification") {
    worst_score = list(`Worst Accuracy` = 0,
                       `Worst Sensitivity` = 0,
                       `Worst Specificity` = 0,
                       `Worst F1` = 0,
                       `Worst Balanced Accuracy` = 0,
                       `Worst AUC` = 0)
  } else if (tolower(task_name) == "clustering") {
    worst_score = list(`Worst Adjusted Rand Index` = 0,
                       `Worst Average Silhouette Score` = -1,
                       `Worst Calinski-Harabasz Index` = 0)
  } else {
    worst_score = list(`Best # of False Positives` = Inf,
                       `Best Absolute Mean CPM SNR` = 10,
                       `Absolute SD CPM SNR` = 10,
                       `Absolute CV CPM SNR` = 10)
  }
  return(worst_score)
}


# Classification Accuracy Metric Calculator
## class_acc_calculator computes and returns a list of accuracy metrics for classification tasks
class_acc_calculator = function(pred, gs_result){
  #  make sure the prediction result is a factor vector
  if (!is.factor(pred)) {
    pred = as.factor(pred)
  }
  
  cm = caret::confusionMatrix(pred, as.factor(gs_result), positive = "1")
  roc = pROC::roc(gs_result, as.numeric(pred))
  
  acc = unname(cm$overall[1])
  sens = unname(cm$byClass[1])
  spec = unname(cm$byClass[2])
  f1 = unname(cm$byClass[7])
  balanced_acc = unname(cm$byClass[11])
  auc = pROC::auc(roc)[1]
  
  eval_result = list(Accuracy = acc,
                     Sensitivity = sens,
                     Specificity = spec,
                     F1 = f1,
                     `Balanced Accuracy` = balanced_acc,
                     AUC = auc)
  
  return(eval_result)
}


# Clustering Accuracy Metric Calculator
## cluster_acc_calculator computes and returns a list of accuracy metrics for clustering tasks
cluster_acc_calculator = function(pred, gs_result, dist){
  if (!is.numeric(pred)) {
    pred = as.numeric(pred)
  }
  
  ari = mclust::adjustedRandIndex(pred, gs_result)
  ass = mean(cluster::silhouette(pred, dist))
  chi = fpc::cluster.stats(dist, pred)$ch
  
  eval_result = list(`Adjusted Rand Index` = ari,
                     `Average Silhouette Score` = ass,
                     `Calinski-Harabasz Index` = chi)
  
  return(eval_result)
}

# stability measure calculator
## stab_calculator computes the standard deviation of evaluation metrics
stab_calculator = function(eval_result) {
  stab_result = eval_result %>% 
    dplyr::group_by(Method) %>% 
    dplyr::summarize(dplyr::across(is.numeric, sd, na.rm = TRUE, .names = "SD_{.col}"))
  
  return(stab_result)
}


# DE False Positive Calculator
## de_fp_calculator computes and returns the number of DE genes detected in a null dataset (false positives)
de_fp_calculator = function(exprs_mat, pred) {
  fp = length(pred)
  
  if (fp >= 5) {
    cpm_mat = Seurat::RelativeCounts(exprs_mat, scale = 1e6, verbose = TRUE)
    cpm_mat = as(cpm_mat, "matrix")
    
    de_mat = cpm_mat[rownames(cpm_mat) %in% pred, ]
    non_de_mat = cpm_mat[!rownames(cpm_mat) %in% pred, ]
    
    de_avg_cpm = rowMeans(de_mat)
    de_var_cpm = apply(de_mat, 1, var)
    de_cv_cpm = sqrt(de_var_cpm)/de_avg_cpm
    
    non_de_avg_cpm = rowMeans(non_de_mat)
    non_de_var_cpm = apply(non_de_mat, 1, var)
    non_de_cv_cpm = sqrt(non_de_var_cpm)/non_de_avg_cpm
    
    snr_avg = (mean(log2(de_avg_cpm)) - mean(log2(non_de_avg_cpm)))/(sd(log2(de_avg_cpm)) - sd(log2(non_de_avg_cpm)))
    snr_var = (mean(log2(de_var_cpm)) - mean(log2(non_de_var_cpm)))/(sd(log2(de_var_cpm)) - sd(log2(non_de_var_cpm)))
    snr_cv = (mean(de_cv_cpm) - mean(non_de_cv_cpm))/(sd(de_cv_cpm) - sd(non_de_cv_cpm))
    
    eval_result = list(`# of False Positives` = fp,
                       `Absolute Mean CPM SNR` = abs(snr_avg),
                       `Absolute Variance CPM SNR` = abs(snr_sd),
                       `Absolute CV CPM SNR` = abs(snr_cv))
  } else {
    eval_result = list(`# of False Positives` = fp)
  }
  return(eval_result)
}