# Small local example for the combined SimBench Trio builder.
#
# Run this from the BenchHub package root after loading the package code:
#   pkgload::load_all(".")
#   source("inst/scripts/case-studies/simbench/example_small_combined_simbench_trio.R")

source("inst/scripts/case-studies/simbench/03_build_combined_simbench_trios.R")

set.seed(3)

toy_counts <- Matrix::Matrix(
  stats::rpois(100L * 40L, lambda = 1),
  nrow = 100L,
  ncol = 40L,
  sparse = TRUE
)
rownames(toy_counts) <- paste0("gene_", seq_len(nrow(toy_counts)))
colnames(toy_counts) <- paste0("cell_", seq_len(ncol(toy_counts)))

toy_celltype <- rep(c("celltype_a", "celltype_b"), each = 20L)
toy_counts[seq_len(10L), toy_celltype == "celltype_b"] <-
  toy_counts[seq_len(10L), toy_celltype == "celltype_b"] + 4L

toy_trio <- build_combined_simbench_trio(
  counts_mat = toy_counts,
  celltype = toy_celltype,
  dataset_id = "toy_simbench_combined",
  dataset_name = "Toy SimBench combined",
  description = "Toy count matrix for testing combined SimBench Trio construction.",
  pearson_sample_size = 100L
)

stopifnot(inherits(toy_trio, "Trio"))
stopifnot(length(toy_trio$evidence) == 19L)
stopifnot(identical(toy_trio$getMetrics("frac_zero_genes"), "KDE"))
stopifnot(identical(
  toy_trio$getMetrics("differentially_expression"),
  "Proportion Difference"
))

print(toy_trio)
