# Download interfaces for each source that Trio supports

#' Download files from Gene Expression Omnibus
#' @description Download main or supplementary files from GEO.
#' @param ID
#'   The ID, formatted either "ARTICLE_ID" for the main file or
#'   "ARTICLE_ID/FILE_ID" for a specific file.
#' @param cachePath
#'   The path to store the downloaded file.
#' @return The path to the downloaded file.
#' @keywords internal
figshareDl <- function(ID, cachePath) {
  # get file ID from ID if it is available
  splitID <- unlist(stringr::str_split(ID, "/"))

  if (length(splitID) == 1) {
    articleID <- ID
    fileID <- NULL
  } else if (length(splitID) == 2) {
    articleID <- splitID[1]
    fileID <- splitID[2]
  } else {
    cli::cli_abort(c(
      "Invalid dataset ID for figshare: {ID}",
      "i" = "Should be either: ",
      "i" = "\t{.strong ARTICLE_ID} for all files",
      "i" = "\t{.strong ARTICLE_ID}/{.strong FILE_ID} for a single file"
    ))
  }

  fileData <- figshareListFiles(articleID, fileID = fileID)

  # for files with the same name, get the most recent ID (deals with versions)
  # TODO: Deal with files that have been deleted in newer versions
  datasets <- fileData |>
    dplyr::arrange(dplyr::desc(id)) |>
    dplyr::group_by(name) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(c("name", "size", "download_url", "computed_md5", "mimetype"))

  if (nrow(datasets) > 1) {
    # Try to auto-select an appropriate file when multiple files present.
    # Preference order:
    # 1) any file ending with _dataset.rds (case-insensitive)
    # 2) any file ending with _evidence.rds (case-insensitive)
    # If multiple candidates exist, pick the largest by size. If none found,
    # fall back to interactive selection.
    df <- datasets |>
      dplyr::select(name, size) |>
      dplyr::mutate(size = paste0(round(size / 1e6, 2), " MB"))

    lb <- tolower(datasets$name)
    ds_idx <- which(stringr::str_detect(lb, "_dataset\\.rds$"))
    ev_idx <- which(stringr::str_detect(lb, "_evidence\\.rds$"))

    chosen_row <- NULL
    if (length(ds_idx) > 0) {
      # prefer the largest dataset file among matches
      sizes <- datasets$size[ds_idx]
      chosen_row <- ds_idx[which.max(sizes)][1]
      cli::cli_inform(c(
        "Multiple files found in Figshare article.",
        "i" = paste0("Auto-selected {.val ", datasets$name[chosen_row], "} (ends with `_dataset.rds`).")
      ))
    } else if (length(ev_idx) > 0) {
      sizes <- datasets$size[ev_idx]
      chosen_row <- ev_idx[which.max(sizes)][1]
      cli::cli_inform(c(
        "Multiple files found in Figshare article.",
        "i" = paste0("Auto-selected {.val ", datasets$name[chosen_row], "} (ends with `_evidence.rds`)."),
        "i" = "If this was unexpected, verify the Figshare article ID."
      ))
    }

    if (!is.null(chosen_row)) {
      datasets <- datasets[chosen_row, , drop = FALSE]
    } else {
      # interactively select one of multiple datasets
      cli::cli_inform("Select a dataset to download:")
      datasets <- datasets[utils::menu(apply(df, 1, paste, collapse = "  ")), ]
    }
  }

  dlPath <- fs::path_join(c(cachePath, paste0("figshare_", articleID)))
  if (!fs::dir_exists(dlPath)) fs::dir_create(dlPath)

  dlLocation <- fs::path_join(c(dlPath, datasets$name))

  # check if files already exist
  alreadyDl <- datasets$name %in% list.files(dlPath)

  # if the files exist, check their md5 hashes
  # delete files that need redownloading
  if (alreadyDl) {
    validDl <- cli::hash_file_md5(
      dlLocation[alreadyDl]
    ) == datasets$computed_md5[alreadyDl]
    alreadyDl <- alreadyDl && validDl

    if (!validDl) {
      fs::file_delete(dlLocation)
    }
  }

  # download datasets which are not available locally
  if (!alreadyDl) {
    curl::curl_download(
      datasets$download_url, dlLocation
    )
  }
  # Check md5 checksums
  MD5equal <- cli::hash_file_md5(dlLocation) == datasets$computed_md5

  if (!MD5equal) {
    cli::cli_warn(c(
      "Not all MD5 hashes of downloaded data are as expected!",
      "i" = "Reinitiallising the Trio will redownload corrupted data."
    ))
  }

  dlLocation
}

#' Download files from geo
#' @description Download main or supplementary files from GEO.
#' @param ID
#'   The ID, formatted either "GSEXXXXXX" for the main file or
#'   "GSEXXXXXX/SupFile.tar.gz" or a supplementary file.
#' @param cachePath
#'   The path to store the downloaded file.
#' @return The path to the downloaded file.
#' @keywords internal
geoDl <- function(ID, cachePath) {
  if (!requireNamespace("GEOquery", quietly = TRUE)) {
    cli::cli_abort(c(
      "Install {.pkg GEOquery} to get data from {.url ncbi.nlm.nih.gov/geo}.",
      "i" = "You can get it by running: {.code BiocManager::install('GEOquery')}"
    ))
  }

  # get file ID from ID if it is available
  splitID <- unlist(stringr::str_split(ID, "/"))

  dlPath <- fs::path_join(c(cachePath, splitID[1]))
  if (!fs::dir_exists(dlPath)) fs::dir_create(dlPath)

  if (length(splitID) == 1) {
    # download GEO data
    tryCatch(
      {
        GEOquery::getGEO(GEO = ID, destdir = dlPath)
        dlLocation <- file.path(dlPath, paste(ID, "series_matrix.txt.gz", sep = '_'))
      },
      error = function(e) {
        cli::cli_abort(c(
          "Failed to download GEO data: {ID}",
          "i" = "Check the GEO ID and try again.",
          "Error message: {e$message}"
        ))
      }
    )
  } else {
    mainID <- splitID[1]
    suppID <- splitID[2]

    # download GEO supplementary data
    tryCatch(
      {
        dlLocation <- GEOquery::getGEOSuppFiles(
          GEO = mainID, makeDirectory = FALSE,
          baseDir = dlPath, filter_regex = suppID
        ) |>
          rownames() |>
          purrr::pluck(1)
      },
      error = function(e) {
        cli::cli_abort(c(
          "Failed to download GEO data: {ID}",
          "i" = "Check the GEO ID and try again.",
          "Error message: {e$message}"
        ))
      }
    )
  }

  if (length(dlLocation) == 0) {
    cli::cli_warn(c(
      "No files found for GEO ID: {ID}",
      "i" = "Ensure that the GEO ID is correct and the data is available."
    ))
  }

  dlLocation
}

#' Download files from ExperimentHub
#' @description Get the a dataset from ExperimentHub
#' @param ID
#'   The ID, a string, with "EH" followed by a series of numbers (e.g. EH119)
#' @param cachePath
#'   The path to store the downloaded file.
#' @return The path to the downloaded file.
#' @keywords internal
experimenthubDl <- function(ID, cachePath) {
  if (!requireNamespace("ExperimentHub", quietly = TRUE)) {
    cli::cli_abort(c(
      "Install {.pkg ExperimentHub} to get data from ExperimentHub.",
      "i" = "You can get it by running: {.code BiocManager::install('ExperimentHub')}"
    ))
  }

  # load ExperimentHub
  eh <- ExperimentHub::ExperimentHub()

  # check if the ID is valid
  if (!ID %in% names(eh)) {
    cli::cli_abort(c(
      "Invalid ExperimentHub ID: {ID}",
      "i" = "Check the ID and try again."
    ))
  }

  # create a download path
  dlPath <- fs::path_join(c(cachePath, paste("ExperimentHub_", ID)))
  if (!fs::dir_exists(dlPath)) fs::dir_create(dlPath)

  dlLocation <- fs::path_join(c(dlPath, ID))

  # check if the file already exists
  alreadyDl <- ID %in% list.files(dlPath)

  # download data if not already downloaded
  if (!alreadyDl) {
    cli::cli_inform("Downloading data for ID: {ID}...")
    # download ExperimentHub data
    data <- eh[[ID]]

    data_name <- paste(paste("ExperimentHub_", ID), ".rds")

    # manually save the EH data as an RDS file in the working directory
    saveRDS(data, file = fs::path_join(c(dlLocation, data_name)))
  } else {
    cli::cli_inform("File already exists in cache. No download needed.")
  }

  if (length(dlLocation) == 0) {
    cli::cli_warn(c(
      "No files found for ExperimentHub ID: {ID}",
      "i" = "Ensure that the ExperimentHub ID is correct and the data is available."
    ))
  }

  dlLocation
}

#' Download files from Zenodo
#' @description Get a dataset from Zenodo
#' @param ID
#'   The ID, a string, with D.O.I., optionally followed by a / and a file name.
#' @param cachePath
#'   The path to store the downloaded file.
#' @return The path to the downloaded file or containing folder if multiple files.
#' @keywords internal
zenodoDl <- function(ID, cachePath) {
  if (!requireNamespace("zen4R", quietly = TRUE)) {
    cli::cli_abort(c(
      "Install {.pkg zen4R} to get data from Zenodo",
      "i" = "You can get it by running: {.code install.packages('zen4R')}"
    ))
  }
  
  splitID <- unlist(stringr::str_split(ID, "/"))
  if(length(splitID) == 3) specificFile <- splitID[3] else specificFile <- list()
  DOI <- paste(splitID[1:2], collapse = '/')
  
  zenodoManager <- ZenodoManager$new()
  requestedRecord <- zenodoManager$getRecordByDOI(DOI)
  if(is.null(requestedRecord))
    cli::cli_abort(c(
      "Invalid Zenodo ID: {ID}",
      "i" = "Check the ID and try again."
    ))
  
  # create a download path
  dlPath <- fs::path_join(c(cachePath, paste0("Zenodo_", splitID[1])))
  if (!fs::dir_exists(dlPath)) fs::dir_create(dlPath)
  
  # check if the file already exists
  if(length(splitID) == 3) allFiles <- splitID[3] else allFiles <- names(requestedRecord$files)
  alreadyDl <- all(allFiles %in% list.files(dlPath))
  
  # download data if not already downloaded
  if (!alreadyDl) {
    cli::cli_inform("Downloading data for ID: {DOI}...")
    # download Zenodo data
    zen4R::download_zenodo(DOI, dlPath, specificFile, timeout = Inf)
  } else {
    cli::cli_inform("File already exists in cache. No download needed.")
  }
  
  if(length(splitID) == 3) dlLocation <- fs::path_join(c(dlPath, specificFile)) else dlLocation <- dlPath
  dlLocation
}

#' Download a Trio from the five-table submission database
#'
#' @param datasetID Dataset identifier from the `Dataset` table.
#' @param ss Google Sheets spreadsheet ID containing the five database tables.
#' @param cachePath Directory for downloaded files. Defaults to `tempdir()`.
#' @return A populated `Trio` object.
#' @export
downloadSubmissionTrio <- function(
    datasetID,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
    cachePath = tempdir()
) {
  if (missing(datasetID) || is.null(datasetID) ||
      length(datasetID) != 1 || is.na(datasetID) || !nzchar(datasetID)) {
    cli::cli_abort("{.arg datasetID} must be a single non-empty string.")
  }

  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection available.")
  }

  tables <- private_read_submission_database_tables(ss)

  dataset_row <- tables$Dataset[tables$Dataset$datasetID == datasetID, , drop = FALSE]
  if (nrow(dataset_row) == 0) {
    cli::cli_abort(c(
      "Could not find {.arg datasetID} in the Dataset table.",
      "i" = "Checked datasetID: {.val {datasetID}}"
    ))
  }
  if (nrow(dataset_row) > 1) {
    cli::cli_abort("Dataset table contains multiple rows for datasetID {.val {datasetID}}.")
  }

  dataset_source <- private_submission_db_chr(dataset_row$source[[1]])
  dataset_source_id <- private_submission_db_chr(dataset_row$datasourceID[[1]])

  data <- private_download_submission_object(
    source = dataset_source,
    source_id = dataset_source_id,
    cachePath = cachePath,
    label = "dataset"
  )

  task_rows <- tables$DatasetTask[tables$DatasetTask$datasetID == datasetID, , drop = FALSE]
  if (nrow(task_rows) == 0) {
    cli::cli_abort("No DatasetTask rows found for datasetID {.val {datasetID}}.")
  }

  evidence_rows <- tables$DatasetEvidence[
    tables$DatasetEvidence$datasetTaskID %in% task_rows$datasetTaskID,
    ,
    drop = FALSE
  ]
  if (nrow(evidence_rows) == 0) {
    cli::cli_abort("No DatasetEvidence rows found for datasetID {.val {datasetID}}.")
  }

  task_metric_rows <- tables$DatasetTaskMetric[
    tables$DatasetTaskMetric$datasetTaskID %in% task_rows$datasetTaskID,
    ,
    drop = FALSE
  ]
  if (nrow(task_metric_rows) == 0) {
    cli::cli_abort("No DatasetTaskMetric rows found for datasetID {.val {datasetID}}.")
  }

  metric_rows <- tables$Metric[
    tables$Metric$metricID %in% task_metric_rows$metricID,
    ,
    drop = FALSE
  ]
  if (nrow(metric_rows) == 0) {
    cli::cli_abort("No Metric rows found for datasetID {.val {datasetID}}.")
  }

  metrics <- private_reconstruct_submission_metrics(metric_rows)
  evidence <- private_reconstruct_submission_evidence(
    evidence_rows = evidence_rows,
    task_metric_rows = task_metric_rows,
    metric_rows = metric_rows,
    cachePath = cachePath
  )

  trio <- Trio$new(
    datasetID = datasetID,
    data = data,
    evidence = evidence,
    metrics = metrics,
    cachePath = cachePath,
    description = private_submission_db_chr(dataset_row$description[[1]]),
    name = private_submission_db_chr(dataset_row$name[[1]])
  )

  trio$dataSource <- dataset_source
  trio$dataSourceID <- dataset_source_id
  trio$evidenceSourceID <- stats::setNames(
    evidence_rows$evidenceSourceID,
    evidence_rows$supportingEvidence
  )

  trio
}

private_read_submission_database_tables <- function(ss) {
  list(
    Dataset = private_read_submission_database_sheet(ss, "Dataset"),
    DatasetTask = private_read_submission_database_sheet(ss, "DatasetTask"),
    DatasetEvidence = private_read_submission_database_sheet(ss, "DatasetEvidence"),
    Metric = private_read_submission_database_sheet(ss, "Metric"),
    DatasetTaskMetric = private_read_submission_database_sheet(ss, "DatasetTaskMetric")
  )
}

private_read_submission_database_sheet <- function(ss, sheet) {
  suppressMessages(
    googlesheets4::read_sheet(
      ss = ss,
      sheet = sheet
    )
  ) |>
    as.data.frame()
}

private_download_submission_object <- function(source, source_id, cachePath, label) {
  if (is.na(source) || is.na(source_id)) {
    cli::cli_abort("Cannot download {label}: source and source ID are required.")
  }

  downloader_name <- paste0(tolower(source), "Dl")
  if (!exists(downloader_name, mode = "function")) {
    cli::cli_abort(c(
      "No downloader is available for source {.val {source}}.",
      "i" = "Expected function: {.fn {downloader_name}}"
    ))
  }

  path <- do.call(
    downloader_name,
    list(ID = source_id, cachePath = cachePath)
  )

  loadFile(path)
}

private_reconstruct_submission_evidence <- function(
    evidence_rows,
    task_metric_rows,
    metric_rows,
    cachePath
) {
  evidence <- list()
  evidence_cache <- new.env(parent = emptyenv())

  for (i in seq_len(nrow(evidence_rows))) {
    evidence_row <- evidence_rows[i, , drop = FALSE]
    evidence_name <- private_submission_db_chr(evidence_row$supportingEvidence[[1]])
    evidence_source_id <- private_submission_db_chr(evidence_row$evidenceSourceID[[1]])

    evidence_data <- private_load_submission_evidence_data(
      evidence_name = evidence_name,
      evidence_source_id = evidence_source_id,
      cachePath = cachePath,
      cache = evidence_cache
    )

    metric_ids <- unique(task_metric_rows$metricID[
      task_metric_rows$datasetTaskID == evidence_row$datasetTaskID[[1]]
    ])
    metric_names <- metric_rows$metricName[match(metric_ids, metric_rows$metricID)]
    metric_names <- metric_names[!is.na(metric_names) & nzchar(metric_names)]

    evidence[[evidence_name]] <- list(
      evidence = evidence_data,
      metrics = metric_names
    )
  }

  evidence
}

private_load_submission_evidence_data <- function(
    evidence_name,
    evidence_source_id,
    cachePath,
    cache
) {
  if (is.na(evidence_source_id)) {
    cli::cli_abort("Evidence {.val {evidence_name}} has no evidenceSourceID.")
  }

  if (!exists(evidence_source_id, envir = cache, inherits = FALSE)) {
    assign(
      evidence_source_id,
      private_download_submission_object(
        source = "figshare",
        source_id = evidence_source_id,
        cachePath = cachePath,
        label = paste("evidence", evidence_name)
      ),
      envir = cache
    )
  }

  loaded <- get(evidence_source_id, envir = cache, inherits = FALSE)

  if (is.list(loaded) &&
      evidence_name %in% names(loaded) &&
      is.list(loaded[[evidence_name]]) &&
      "evidence" %in% names(loaded[[evidence_name]])) {
    return(loaded[[evidence_name]]$evidence)
  }

  if (is.list(loaded) && evidence_name %in% names(loaded)) {
    return(loaded[[evidence_name]])
  }

  loaded
}

private_reconstruct_submission_metrics <- function(metric_rows) {
  metric_rows <- metric_rows[!duplicated(metric_rows$metricID), , drop = FALSE]
  metrics <- list()

  for (i in seq_len(nrow(metric_rows))) {
    metric_row <- metric_rows[i, , drop = FALSE]
    metric_name <- private_submission_db_chr(metric_row$metricName[[1]])
    wrapper_r <- private_submission_db_col_chr(metric_row, "wrapper_r")
    source_type <- if ("metricSourceType" %in% names(metric_row)) {
      private_submission_db_chr(metric_row$metricSourceType[[1]])
    } else {
      NA_character_
    }
    gist_url <- private_submission_db_col_chr(metric_row, "gist_url")

    metrics[[metric_name]] <- private_reconstruct_submission_metric(
      metric_name = metric_name,
      wrapper_r = wrapper_r,
      source_type = source_type,
      gist_url = gist_url
    )
  }

  metrics
}

private_reconstruct_submission_metric <- function(
    metric_name,
    wrapper_r,
    source_type,
    gist_url
) {
  if (!is.na(wrapper_r) &&
      (identical(source_type, "internal") || is.na(gist_url))) {
    return(match.fun(wrapper_r))
  }

  if (!is.na(gist_url)) {
    metric_env <- new.env(parent = baseenv())
    sys.source(downloadGist(gist_url), envir = metric_env)

    if (!is.na(wrapper_r) && exists(wrapper_r, envir = metric_env, inherits = FALSE)) {
      return(get(wrapper_r, envir = metric_env, inherits = FALSE))
    }

    if (exists(metric_name, envir = metric_env, inherits = FALSE)) {
      return(get(metric_name, envir = metric_env, inherits = FALSE))
    }
  }

  cli::cli_abort("Could not reconstruct metric {.val {metric_name}}.")
}

private_submission_db_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) {
    return(NA_character_)
  }

  as.character(x)
}

private_submission_db_col_chr <- function(row, column) {
  if (!column %in% names(row)) {
    return(NA_character_)
  }

  private_submission_db_chr(row[[column]][[1]])
}
