# Download inferfaces for each source that Trio supports


# ID: input of the form ARTICLE_ID or ARTICLE_ID/FILE_ID
figshareDl <- function(ID, cachePath) {
  API_URL <- "https://api.figshare.com/v2/"

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

  # Create request URL
  requestUrl <- glue::glue(
    API_URL, "articles/{articleID}/files",
    ifelse(!is.null(fileID), paste0("/", fileID), "")
  )

  # Execute request
  req <- httr2::request(requestUrl)
  resp <- req |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  body <- resp |> httr2::resp_body_json()

  # Informative error if API query fails
  if (resp |> httr2::resp_is_error()) {
    status <- paste(httr2::resp_status(resp), httr2::resp_status_desc(resp))
    apiMessage <- stringr::str_split(body$message, "\n")

    errorMessage <- paste0(
      cli::format_error(c(
        "API Request to figshare failed. Code: {status}",
        "i" = "Check the figshare article ID and try again.",
        "i" = "Figshare API error message:"
      )),
      "\n",
      stringr::str_flatten(
        unlist(lapply(apiMessage, \(x) paste0("> ", x))),
        collapse = "\n"
      )
    )
    rlang::abort(message = errorMessage)
  }

  # check if query returned a single item
  if (!is.null(fileID) || !is.null(names(body))) {
    body <- list(body)
  }
  # for files with the same name, get the most recent ID (deals with versions)
  # TODO: Deal with files that have been deleted in newer versions
  datasets <- do.call(rbind, lapply(body, data.frame)) |>
    dplyr::arrange(dplyr::desc(id)) |>
    dplyr::group_by(name) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(c("name", "size", "download_url", "computed_md5", "mimetype"))

  dlPath <- fs::path_join(c(cachePath, paste0("figshare_", articleID)))
  if (!fs::dir_exists(dlPath)) fs::dir_create(dlPath)

  dlLoacations <- paste(dlPath, datasets$name, sep = "/")

  # check if files already exist
  alreadyDl <- datasets$name %in% list.files(dlPath)

  # if the files exist, check their md5 hashes
  # delete files that need redownloading
  if (any(alreadyDl)) {
    validDl <- cli::hash_file_md5(
      dlLoacations[alreadyDl]
    ) == datasets$computed_md5[alreadyDl]
    alreadyDl[validDl] <- alreadyDl[validDl] & validDl

    toReDl <- fs::file_exists(dlLoacations[!alreadyDl])
    fs::file_delete(names(toReDl[toReDl]))
  }

  # download datasets which are not available locally
  if (any(!alreadyDl)) {
    status <- curl::multi_download(
      datasets$download_url[!alreadyDl], dlLoacations[!alreadyDl]
    )

    if (!all(status$success)) {
      # TODO: Deal with one failed download.
      cli::cli_abort(c(
        "One or more downloads failed.",
        "Dataset names: {datasets[!alreadyDl,][!status$success, 'name']}",
        "Dataset URLs: {datasets[!alreadyDl,][!status$success, 'download_url']}"
      ))
    }
  }
  # Check md5 checksums
  MD5equal <- cli::hash_file_md5(dlLoacations) == datasets$computed_md5

  if (!all(MD5equal)) {
    # TODO: Inform user on how to redownload the data.
    cli::cli_warn(c(
      "Not all MD5 hashes of downloaded data are as expected!",
      "i" = "Reinitiallising the Trio will redownload corrupted data."
    ))
  }

  dlLoacations
}

geoDl <- function(ID) {
  if (!requireNamespace("GEOquery", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg GEOquery} is required to get data from {.url ncbi.nlm.nih.gov/geo}.",
      "i" = "You can get it by running: {.code install.packages('GEOquery')}"
    ))
  }

  # ensure tempdir for downloads
  tempFolder <- tempdir()

  # download GEO data
  tryCatch(
    {
      dlLoacations <- GEOquery::getGEOfile(GEO = ID, destdir = tempFolder)
    },
    error = function(e) {
      cli::cli_abort(c(
        "Failed to download GEO data: {ID}",
        "i" = "Check the GEO ID and try again.",
        "Error message: {e$message}"
      ))
    }
  )

  if (length(dlLoacations) == 0) {
    cli::cli_warn(c(
      "No files found for GEO ID: {ID}",
      "i" = "Ensure that the GEO ID is correct and the data is available."
    ))
  }

  dlLoacations
}

experimenthubDl <- function(ID, cachePath) {
  cli::cli_abort(c(
    "ExperimentHub Downloads are not supported yet ☹"
  ))
}
