# Small local example for the SimBench data-property Trio builder.
#
# Run this from the BenchHub package root after loading the package code:
#   pkgload::load_all(".")
#   source("inst/scripts/case-studies/simbench/example_small_data_property_trio.R")

source("inst/scripts/case-studies/simbench/01_compute_data_property_evidence.R")

set.seed(1)

toy_counts <- Matrix::Matrix(
  sample(c(0L, 0L, 0L, 1L, 2L, 5L), size = 20L * 40L, replace = TRUE),
  nrow = 20L,
  ncol = 40L,
  sparse = TRUE
)
rownames(toy_counts) <- paste0("gene_", seq_len(nrow(toy_counts)))
colnames(toy_counts) <- paste0("cell_", seq_len(ncol(toy_counts)))

toy_trio <- build_data_property_trio(
  counts_mat = toy_counts,
  dataset_id = "toy_simbench_counts",
  dataset_name = "Toy SimBench counts",
  description = "Toy count matrix for testing SimBench data-property Trio construction.",
  pearson_sample_size = 100L
)

stopifnot(inherits(toy_trio, "Trio"))
stopifnot(length(toy_trio$evidence) == 14L)
stopifnot(identical(toy_trio$getMetrics("frac_zero_genes"), "KDE"))

# Optional single-evidence evaluation smoke test. This avoids the
# two-dimensional evidence objects until BenchHub's KDE metric has explicit
# matrix/data-frame support.
if (requireNamespace("ks", quietly = TRUE)) {
  toy_sim_counts <- toy_counts
  toy_sim_counts[1L, 1L] <- toy_sim_counts[1L, 1L] + 1L

  toy_result <- toy_trio$evaluate(
    list(
    toy_simulator = list(
      frac_zero_genes = Matrix::rowMeans(toy_sim_counts == 0)
    )
  )
)

  print(toy_result)
} else {
  message("Skipping KDE evaluation smoke test because the ks package is not installed.")
}

print(toy_trio)
