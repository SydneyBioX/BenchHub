# Build one combined SimBench Trio.
#
# Run from the BenchHub package root:
#   Rscript inst/scripts/case-studies/simbench/04_run_one_simbench_trio.R
#
# Or run interactively line by line if you want to inspect intermediate objects.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  library(BenchHub)
}

simbench_metadata <- list_simbench_datasets()
dataset_map <- simbench_dataset_map(drop_ids = c("EH5384"))

print(simbench_metadata)
alldata <- fetch_simbench_datasets()

# Start with one mixed-cell dataset. EH5384 is excluded because it is a 293T
# cell-line dataset and may not have enough cell types for biological signals.
id <- "EH5385"
metadata <- showMetaData()
metadata[metadata$ExperimentHub_ID == "EH5385", ]

# Keep this small for the first smoke test. Increase to 10000L for a fuller
# reproduction run.
pearson_sample_size <- 100L

if (!id %in% names(dataset_map)) {
  stop("Dataset ID ", id, " is not available in dataset_map.", call. = FALSE)
}

obj <- alldata[[id]]
dataset_name <- dataset_map[[id]]

counts_mat <- get_counts_matrix(obj, dataset_name = dataset_name)
celltype <- get_celltype(obj, dataset_name = dataset_name)

trio <- build_combined_simbench_trio(
  counts_mat = counts_mat,
  celltype = celltype,
  dataset_id = id,
  dataset_name = dataset_name,
  pearson_sample_size = pearson_sample_size
)

print(trio)

Sys.unsetenv("GITHUB_PAT")
res <- writeSubmission(
  trio = trio,
  n_tasks = 19,
  prepare_files = TRUE,
  upload_custom_metrics = TRUE,
  githubPat = Sys.getenv("GITHUB_PAT"),
  review = TRUE,
  submit = TRUE,
  url = "https://script.google.com/macros/s/AKfycbx2kgx2N0lbAlr0Q35PEwYsy3sFKvnWZVYEmjRsHDSRFEIWB-TLFM3r4HEd09TNfFxO/exec",
  submittedBy = "xiaoqi.liang@sydney.edu.au",
  build_payload = TRUE,
  build_json = TRUE
)

