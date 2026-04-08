# Manually prepare one SimBench Trio submission.
#
# Edit `id` and the dataset metadata below for each dataset. This script avoids
# batching on purpose because organism, tissue, status, DOI, and technology may
# need manual checking.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  library(BenchHub)
}

source("inst/scripts/case-studies/simbench/03_build_combined_simbench_trios.R")
source("inst/scripts/case-studies/simbench/05_submission_helpers.R")

id <- "EH5387"
pearson_sample_size <- 100L
metadata <- showMetaData()
print(metadata[metadata$ExperimentHub_ID == id, , drop = FALSE])

dataset_metadata <- list(
  organism = "Homo sapiens",
  tissue = "Cell line",
  status = "diseased",
  doi = "10.1016/j.cell.2018.05.060",
  technology = "10x Genomics"
)

submission_url <- "https://script.google.com/macros/s/AKfycbx2kgx2N0lbAlr0Q35PEwYsy3sFKvnWZVYEmjRsHDSRFEIWB-TLFM3r4HEd09TNfFxO/exec"
submitted_by <- "xiaoqi.liang@sydney.edu.au"

dataset_map <- simbench_dataset_map(drop_ids = c("EH5384"))


if (!id %in% names(dataset_map)) {
  stop("Dataset ID ", id, " is not available in dataset_map.", call. = FALSE)
}

alldata <- fetch_simbench_datasets()
obj <- alldata[[id]]
dataset_name <- dataset_map[[id]]

counts_mat <- get_counts_matrix(obj, dataset_name = dataset_name)
celltype <- get_celltype(obj, dataset_name = dataset_name)

if (length(unique(as.character(celltype))) >= 2L) {
  trio <- build_combined_simbench_trio(
    counts_mat = counts_mat,
    celltype = celltype,
    dataset_id = id,
    dataset_name = dataset_name,
    pearson_sample_size = pearson_sample_size
  )
} else {
  message(
    "Dataset ", id, " has fewer than two cell types; building data-property-only Trio."
  )
  trio <- build_data_property_trio(
    counts_mat = counts_mat,
    dataset_id = id,
    dataset_name = dataset_name,
    pearson_sample_size = pearson_sample_size
  )
}

print(trio)

res <- prepare_simbench_submission(
  trio = trio,
  dataset_defaults = simbench_dataset_defaults(
    trio = trio,
    organism = dataset_metadata$organism,
    tissue = dataset_metadata$tissue,
    status = dataset_metadata$status,
    doi = dataset_metadata$doi,
    technology = dataset_metadata$technology
  ),
  upload_custom_metrics = TRUE,
  githubPat = Sys.getenv("GITHUB_PAT"),
  review = TRUE,
  submit = TRUE,
  url = submission_url,
  submittedBy = submitted_by
)

res
