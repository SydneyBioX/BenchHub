# Manually prepare one SpatialSimBench Trio submission.
#
# Edit the Figshare IDs and dataset metadata below for each dataset. This script
# is intentionally manual because the matched single-cell reference and dataset
# metadata may vary across datasets.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  library(BenchHub)
}

source("inst/scripts/case-studies/spatialsimbench/01_build_spatialsimbench_clustering_trio.R")
source("inst/scripts/case-studies/spatialsimbench/05_submission_helpers.R")

spatial_figshare_id <- "26054188/47115877"
dataset_name <- "MCATUMOR"
pearson_sample_size <- 100L

# Provide the matched single-cell reference as a loaded SingleCellExperiment.
# Example:
# matched_sc_sce <- readRDS("path/to/matched_sc_reference.rds")
matched_sc_sce <- readRDS("/Users/cabiria/Downloads/MCATUMOR_sc.rds")

dataset_metadata <- list(
  organism = "Mus musculus", #Mus musculus Homo sapiens
  tissue = "fibrosarcoma",
  status = "diseased",
  doi = "10.1038/s41587-022-01272-8",
  technology = "10x Visium"
)

submission_url <- "https://script.google.com/macros/s/AKfycbx2kgx2N0lbAlr0Q35PEwYsy3sFKvnWZVYEmjRsHDSRFEIWB-TLFM3r4HEd09TNfFxO/exec"
submitted_by <- "xiaoqi.liang@sydney.edu.au"

trio <- build_spatialsimbench_trio(
  figshare_id = spatial_figshare_id,
  dataset_name = dataset_name,
  matched_sc_sce = matched_sc_sce,
  pearson_sample_size = pearson_sample_size
)

trio$description <- "Multi-resolution deconvolution of spatial transcriptomics data"

print(trio)

res <- prepare_spatialsimbench_submission(
  trio = trio,
  dataset_defaults = spatialsimbench_dataset_defaults(
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


res$metric_args
