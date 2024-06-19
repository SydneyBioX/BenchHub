getData <- function(datasetID) {
  parsed <- unlist(stringr::str_split(datasetID, ":"))

  if (length(parsed) == 1) {
    # TODO: Validate named datasets.
    cli::cli_abort(c(
      "Getting datasets by name is not supported yet :(",
      "i" = "Use a format string instead: {.emph source}:{.emph ID}"
    ))
  } else if (length(parsed) == 2) {
    sourceName <- tolower(parsed[1])
    id <- parsed[2]

    if (!exists(paste0(sourceName, "Dl"))) {
      supported <- stringr::str_remove( # nolint
        grep("Dl", ls("package:TrioR"), value = TRUE), "Dl"
      )
      cli::cli_abort(c(
        "Downloading form {.emph {sourceName}} is not supported.",
        "i" = "Choose one of the following: {supported}"
      ))
    }
    files <- do.call(paste0(sourceName, "Dl"), list("ID" = id))
  } else {
    cli::cli_abort(c(
      "Unsupported data specification string",
      "i" = "Input a datasetID or a string formatted {.emph source}:{.emph ID}"
    ))
  }

  lapply(files, loadFile)
}


loadFile <- function(filePath) {
  ext <- tools::file_ext(filePath)

  if (tolower(ext) == "rds") {
    readRDS(filePath)
  } else if (tolower(ext) == "zip") {
    directory <- dirname(filePath)
    decompressedPath <- utils::unzip(
      filePath,
      exdir = fs::path_join(c(directory, "decompressed"))
    )
    decompressedPath
  } else if (tolower(ext) == "h5ad") {
    if (!requireNamespace("anndata", quietly = TRUE)) {
      cli::cli_abort(c(
        "Reading H5AD files requires the {.pkg anndata} package.",
        "i" = "Check {.url https://anndata.dynverse.org/} for instuctions."
      ))
    }

    anndata::read_h5ad(filePath)
  } else {
    cli::cli_abort(c(
      "File format {.file .{ext}} is not currently supported."
    ))
  }
}
