# Small local example for the SimBench biological-signal Trio builder.
#
# Run this from the BenchHub package root after loading the package code:
#   pkgload::load_all(".")
#   source("inst/scripts/case-studies/simbench/example_small_biological_signal_trio.R")

source("inst/scripts/case-studies/simbench/02_build_biological_signal_trios.R")

set.seed(2)

toy_counts <- Matrix::Matrix(
  stats::rpois(100L * 40L, lambda = 1),
  nrow = 100L,
  ncol = 40L,
  sparse = TRUE
)
rownames(toy_counts) <- paste0("gene_", seq_len(nrow(toy_counts)))
colnames(toy_counts) <- paste0("cell_", seq_len(ncol(toy_counts)))

toy_celltype <- rep(c("celltype_a", "celltype_b"), each = 20L)

# Add a small signal so the tests have something non-trivial to compute.
toy_counts[seq_len(10L), toy_celltype == "celltype_b"] <-
  toy_counts[seq_len(10L), toy_celltype == "celltype_b"] + 4L

toy_trio <- build_biological_signal_trio(
  counts_mat = toy_counts,
  celltype = toy_celltype,
  dataset_id = "toy_simbench_biological_signal",
  dataset_name = "Toy SimBench biological signal",
  description = "Toy count matrix for testing SimBench biological-signal Trio construction."
)

stopifnot(inherits(toy_trio, "Trio"))
stopifnot(identical(sort(names(toy_trio$evidence)), sort(c("DE", "DV", "DD", "DP", "BD"))))
stopifnot(identical(toy_trio$getMetrics("DE"), "Proportion Difference"))

toy_sim_counts <- toy_counts
toy_sim_counts[seq_len(5L), toy_celltype == "celltype_b"] <-
  toy_sim_counts[seq_len(5L), toy_celltype == "celltype_b"] + 2L

toy_predicted <- compute_biological_signal_evidence(
  counts_mat = toy_sim_counts,
  celltype = toy_celltype
)

toy_result <- toy_trio$evaluate(
  list(
    toy_simulator = toy_predicted
  )
)

print(toy_trio)
print(toy_result)
