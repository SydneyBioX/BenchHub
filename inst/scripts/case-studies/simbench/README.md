# SimBench Case Study

This folder contains reproduction scripts for building BenchHub objects from
SimBench resources.

## Extract SimBenchData

The SimBenchData resources are available through ExperimentHub. To inspect the
available datasets and build a named map from ExperimentHub IDs to dataset
titles:

```r
source("inst/scripts/case-studies/simbench/03_build_combined_simbench_trios.R")

simbench_metadata <- list_simbench_datasets()
dataset_map <- simbench_dataset_map(drop_ids = c("EH5384"))
```

To extract the count matrix and cell-type labels for one dataset:

```r
alldata <- fetch_simbench_datasets()

id <- "EH5385"
obj <- alldata[[id]]
counts_mat <- get_counts_matrix(obj, dataset_name = dataset_map[[id]])
celltype <- get_celltype(obj, dataset_name = dataset_map[[id]])
```

`counts_mat` is the gene-by-cell count matrix used as Trio `data`, and
`celltype` is the cell-type vector used to derive biological-signal supporting
evidence.

## Data Property Estimation

`01_compute_data_property_evidence.R` computes the 14 real-data summaries used
as supporting evidence for the SimBench data-property estimation task and builds
Trio objects.

For one count matrix, use:

```r
source("inst/scripts/case-studies/simbench/01_compute_data_property_evidence.R")

trio <- build_data_property_trio(
  counts_mat = counts_mat,
  dataset_id = "simbench_dataset_id",
  dataset_name = "SimBench dataset name"
)
```

For all SimBenchData ExperimentHub resources, use:

```r
trios <- build_simbench_data_property_trios(
  dataset_map = dataset_map,
  drop_ids = c("EH5384")
)
```

Each Trio contains:

- `dataset_id`: ExperimentHub resource ID.
- `dataset_name`: Dataset label used for file naming and later Trio metadata.
- `data`: Real count matrix.
- `evidence`: 14 data-property supporting evidence summaries.
- `metrics`: `KDE`, using BenchHub's `kdeMetric`.

The script does not evaluate simulator outputs.

## Small Example

Run this from the package root after loading BenchHub:

```r
pkgload::load_all(".")
source("inst/scripts/case-studies/simbench/example_small_data_property_trio.R")
```

The small example builds a toy sparse count matrix, constructs one Trio, checks
that it contains 14 evidence objects, and runs one univariate evaluation smoke
test for `frac_zero_genes`.

## Biological Signals

`02_build_biological_signal_trios.R` computes real-data biological-signal
proportions and builds Trio objects. It currently includes:

- `DE`: limma differential expression, using `adj.P.Val < p_sig`.
- `DV`: Bartlett test differential variability, using adjusted p-value `< p_sig`.
- `DD`: K-S test differential distribution, using adjusted p-value `< p_sig`.
- `DP`: chi-square differential expression proportion, using adjusted p-value `< p_sig`.
- `BD`: bimodality index, using value `> bd_threshold`.

Each supporting evidence value is a scalar proportion of genes and uses a local
`Proportion Difference` metric:

```r
source("inst/scripts/case-studies/simbench/02_build_biological_signal_trios.R")

trio <- build_biological_signal_trio(
  counts_mat = counts_mat,
  celltype = celltype,
  dataset_id = "simbench_dataset_id",
  dataset_name = "SimBench dataset name"
)
```

For all SimBenchData ExperimentHub resources, use:

```r
trios <- build_simbench_biological_signal_trios(
  dataset_map = dataset_map,
  drop_ids = c("EH5384")
)
```

Small example:

```r
pkgload::load_all(".")
source("inst/scripts/case-studies/simbench/example_small_biological_signal_trio.R")
```

## Combined Trio

`03_build_combined_simbench_trios.R` builds one Trio per SimBench real dataset
with both evidence groups:

- 14 data-property evidence objects using `KDE`.
- 5 biological-signal evidence objects using `Proportion Difference`.

Biological-signal evidence names are expanded in the combined Trio:

- `DE` becomes `differentially_expression`
- `DV` becomes `differentially_variable`
- `DD` becomes `differentially_distributed`
- `DP` becomes `differential_prop`
- `BD` becomes `bimodally_distributed`

For one count matrix, use:

```r
source("inst/scripts/case-studies/simbench/03_build_combined_simbench_trios.R")

trio <- build_combined_simbench_trio(
  counts_mat = counts_mat,
  celltype = celltype,
  dataset_id = "simbench_dataset_id",
  dataset_name = "SimBench dataset name"
)
```

For all SimBenchData ExperimentHub resources, use:

```r
trios <- build_simbench_combined_trios(
  dataset_map = dataset_map,
  drop_ids = c("EH5384")
)
```

Small example:

```r
pkgload::load_all(".")
source("inst/scripts/case-studies/simbench/example_small_combined_simbench_trio.R")
```

To build one real SimBenchData Trio, start with:

```r
source("inst/scripts/case-studies/simbench/04_run_one_simbench_trio.R")
```

The runner currently uses `EH5385` and leaves the combined Trio in an object
called `trio`.

## Prepare Submission

After checking one combined Trio, prepare a review bundle with:

```r
source("inst/scripts/case-studies/simbench/05_prepare_one_simbench_submission.R")
```

This uses `writeSubmission()` with 19 tasks: one task per supporting evidence
object. Shared metadata such as task stage, task type, evidence type, and
metric type is filled from `05_submission_helpers.R` so you do not need to
re-enter the same choices for every Trio.

The script reuses the ExperimentHub source for the dataset, saves supporting
evidence under `ignore/simbench/submission/`, builds the submission payload and
JSON for review, and does not submit.

To run the full submit flow, edit `submitted_by` in
`06_submit_one_simbench_trio.R`, make sure `GITHUB_PAT` is available for custom
metric gist upload, and run:

```r
source("inst/scripts/case-studies/simbench/06_submit_one_simbench_trio.R")
```
