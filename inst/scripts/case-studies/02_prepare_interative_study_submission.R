# Interactive example for preparing a BenchmarkStudy submission.
#
# This script mirrors the workflow shown in vignette 05:
# 1. create a BenchmarkStudy object
# 2. optionally add existing Trio objects
# 3. add mapping functions
# 4. run the interactive Study submission helper
# 5. download and inspect an existing submitted Study

study <- BenchmarkStudy$new(name = "ST toy study")
study$description <- "Toy spatial transcriptomics study."


extract_domains <- function(result) {
  if (is.data.frame(result) && "annotated_domain" %in% colnames(result)) {
    return(result$annotated_domain)
  }
  if (is.list(result) && "annotated_domain" %in% names(result)) {
    return(result$annotated_domain)
  }
  stop("Could not find 'annotated_domain' in the method output.")
}

study$addMappingFunction(
  name = "annotated_domain",
  func = extract_domains,
  inputDescription = "Method output containing one predicted domain label per spot.",
  outputDescription = "A vector of predicted spatial domain labels aligned to spots.",
  exampleUsage = paste(
    "result <- list(annotated_domain = c('A', 'A', 'B', 'B'))",
    "study$runMapping('annotated_domain', result)",
    sep = "\n"
  )
)
extract_celltype_props <- function(result) {
  if (is.data.frame(result) && "celltype_proportions" %in% names(result)) {
    return(result$celltype_proportions)
  }
  if (is.list(result) && "celltype_proportions" %in% names(result)) {
    return(result$celltype_proportions)
  }
  if (is.matrix(result) || is.data.frame(result)) {
    mat <- as.matrix(result)
    rs <- rowSums(mat)
    rs[rs == 0] <- 1
    return(mat / rs)
  }
  stop("Could not extract cell type proportions from the method output.")
}

study$addMappingFunction(
  name = "celltype_proportions",
  func = extract_celltype_props,
  inputDescription = "Method output containing cell type proportions per spot.",
  outputDescription = "A matrix or data frame of cell type proportions aligned to spots.",
  exampleUsage = paste(
    "props <- matrix(c(0.9, 0.1, 0.8, 0.2, 0.2, 0.8, 0.1, 0.9), ncol = 2, byrow = TRUE)",
    "study$runMapping('celltype_proportions', props)",
    sep = "\n"
  )
)

bundle <- interactivePrepareStudySubmission(study)

loaded_study <- downloadSubmissionStudy(studyID = "ST005", cachePath = tempdir())

loaded_study
loaded_study$name
loaded_study$description
loaded_study$version
length(loaded_study$trios)

loaded_study$trios[[1]]
loaded_study$listMappingFunctions()
loaded_study$printMappingFunctionDocumentation("annotated_domain")

method_output <- list(
  annotated_domain = c("domain_1", "domain_1", "domain_2", "domain_2"),
  celltype_proportions = data.frame(
    celltype_A = c(0.9, 0.8, 0.2, 0.1),
    celltype_B = c(0.1, 0.2, 0.8, 0.9)
  )
)

domain_pred <- loaded_study$runMapping("annotated_domain", method_output)
prop_pred <- loaded_study$runMapping("celltype_proportions", method_output)

result <- loaded_study$evaluate(
  loaded_study$trios[[1]]$name,
  list(
    "annotated_domain" = domain_pred,
    "celltype_proportions" = prop_pred
  )
)

