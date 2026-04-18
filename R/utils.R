#' Create a GitHub gist with provided content
#' @param content Character vector of content to include in the gist
#' @param filename Name of the file in the gist
#' @param description Description of the gist
#' @param public Boolean indicating if the gist should be public
#' @return List containing gist data including html_url
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @noRd
creatGist <- function(content, filename, description, public = TRUE, pat = NULL) {
  # check network connectivity
  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection available")
  } 
  # Use provided PAT or fall back to environment variable
  if (is.null(pat) || pat == "") {
    pat <- Sys.getenv("GITHUB_PAT")
  }
  if (pat == "") {
    cli::cli_abort("GITHUB_PAT environment variable must be set to create gists")
  }

  # Build the gist creation payload
  gist_content <- paste(content, collapse = "\n")
  gist_payload <- list(
    description = description,
    public = public,
    files = list()
  )
  gist_payload$files[[filename]] <- list(content = gist_content)

  # Create gist using GitHub API
  resp <- httr2::request("https://api.github.com/gists") |>
    httr2::req_headers(
      Accept = "application/vnd.github+json",
      Authorization = paste("Bearer", pat),
      "X-GitHub-Api-Version" = "2022-11-28"
    ) |>
    httr2::req_body_json(gist_payload) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)
}

#' Download content from a GitHub gist
#' @param gist_url URL of the gist to download
#' @return Path to temporary file containing the gist content
#' @importFrom httr2 request req_headers req_perform resp_body_json
#' @noRd
downloadGist <- function(gist_url) {
  # check network connectivity
  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection available")
  }
  # Extract gist ID from URL
  gist_id <- sub(".*/(\\w+)$", "\\1", gist_url)

  # Fetch gist content using GitHub API
  resp <- httr2::request(paste0("https://api.github.com/gists/", gist_id)) |>
    httr2::req_headers(
      Accept = "application/vnd.github+json",
      "X-GitHub-Api-Version" = "2022-11-28"
    ) |>
    httr2::req_perform()

  gist_data <- httr2::resp_body_json(resp)

  # Get the R file content
  r_files <- Filter(function(f) grepl("\\.R$", f), names(gist_data$files))
  if (length(r_files) == 0) {
    cli::cli_abort("No R files found in gist")
  }

  # Write content to temp file
  temp_file <- file.path(tempdir(), r_files[1])
  writeLines(gist_data$files[[r_files[1]]]$content, temp_file)
  temp_file
}

#' Load downloaded files.
#' @param filePath A path to the file to load.
#' @param context Optional label describing what is being loaded.
#' @importFrom withr with_output_sink
#' @importFrom fs path_join
#' @noRd
loadFile <- function(filePath, context = NULL) {
  ext <- tools::file_ext(filePath)

  if (tolower(ext) == "rds") {
    # silence all the annoying messages
    con <- file(tempfile(), open = "wt")

    withr::with_output_sink(
      new = con,
      code = {
        data <- readRDS(filePath)
      }
    )
    close.connection(con)
    return(data)
  } else if (tolower(ext) == "zip") {
    directory <- dirname(filePath)
    decompressedDir <- fs::path_join(c(directory, "decompressed"))
    decompressedPaths <- utils::unzip(
      filePath,
      exdir = decompressedDir
    )

    # Check what files were extracted
    if (length(decompressedPaths) == 1) {
      # If only one file was extracted, process it recursively
      return(loadFile(decompressedPaths, context = context))
    } else if (length(decompressedPaths) > 1) {
      # Check if there's exactly one supported file
      supportedExts <- c("rds", "h5ad", "csv")
      supportedFiles <- decompressedPaths[
        tolower(
          tools::file_ext(decompressedPaths)
        ) %in%
          supportedExts
      ]

      if (length(supportedFiles) == 1) {
        # If exactly one supported file, load it
        return(loadFile(supportedFiles, context = context))
      } else if (length(supportedFiles) > 1) {
        # Prefer .rds files for BenchHub downloads; otherwise take the first
        # supported file in a deterministic order to avoid interactive menus.
        supportedLower <- tolower(tools::file_ext(supportedFiles))
        rdsFiles <- supportedFiles[supportedLower == "rds"]

        if (length(rdsFiles) > 0) {
          chosenFile <- sort(rdsFiles)[[1]]
          cli::cli_inform(c(
            if (is.null(context) || is.na(context) || !nzchar(context)) {
              "The archive contains multiple supported files."
            } else {
              "The archive for {.val {context}} contains multiple supported files."
            },
            "i" = "Automatically selected {.file {basename(chosenFile)}}."
          ))
        } else {
          chosenFile <- sort(supportedFiles)[[1]]
          cli::cli_inform(c(
            if (is.null(context) || is.na(context) || !nzchar(context)) {
              "The archive contains multiple supported files."
            } else {
              "The archive for {.val {context}} contains multiple supported files."
            },
            "i" = "Automatically selected {.file {basename(chosenFile)}}."
          ))
        }

        return(loadFile(chosenFile, context = context))
      } else {
        # If there are no supported files, fall back to interactive selection.
        cli::cli_inform(c(
          if (is.null(context) || is.na(context) || !nzchar(context)) {
            "The archive contains multiple files."
          } else {
            "The archive for {.val {context}} contains multiple files."
          },
          "i" = "For more control over file loading, consider using the {.code dataLoader} parameter in {.code Trio$new()} (see {.code ?Trio} for details)."
        ))

        # List files for user selection
        if (is.null(context) || is.na(context) || !nzchar(context)) {
          cli::cli_inform("Select a file to load:")
        } else {
          cli::cli_inform("Select a file to load for {.val {context}}:")
        }
        selectedFile <- decompressedPaths[utils::menu(decompressedPaths)]
        return(loadFile(selectedFile, context = context))
      }
    } else {
      cli::cli_abort(c(
        "No files were extracted from the archive."
      ))
    }
  } else if (tolower(ext) == "h5ad") {
    if (!requireNamespace("anndata", quietly = TRUE)) {
      cli::cli_abort(c(
        "Reading H5AD files requires the {.pkg anndata} package.",
        "i" = "Check {.url https://anndata.dynverse.org/} for instructions."
      ))
    }

    anndata::read_h5ad(filePath)
  } else if (tolower(ext) == "csv") {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      url <- "https://rdatatable.gitlab.io/data.table/"
      cli::cli_abort(c(
        "Reading CSV files requires the {.pkg data.table} package.",
        "i" = "Check {.url {url}} for instructions."
      ))
    }

    data.table::fread(filePath)
  } else {
    cli::cli_abort(c(
      "File format {.file .{ext}} is not currently supported."
    ))
  }
}

#' Get an answer to a question.
#' @param m1 A message clarifying the context
#' @param m2 The y/n question to ask the user
#' @noRd
.getAnswer <- function(m1, m2) {
  while (TRUE) {
    cli::cli_inform(m1)
    answer <- tolower(readline(m2))
    if (answer %in% c("", "y")) {
      answer <- TRUE
      break
    } else if (answer == "n") {
      answer <- FALSE
      break
    } else {
      cli::cli_inform(
        "Invalid input {.val {answer}}, choose one of {.val {c('y','n')}}"
      )
    }
  }
  answer
}

#' Determine the cache path for the trio
#' @param cachePath
#'   Either a valid path or a boolean. If TRUE, will create then return the
#'   default Trio cache path without prompting the user.
#' @noRd
getTrioCachePath <- function(cachePath) {
  defaultPath <- FALSE

  # if cachePath is TRUE, use the default cache location without prompting user
  if (cachePath == TRUE) {
    cachePath <- tools::R_user_dir("BenchHub", which = "cache")
    if (!fs::dir_exists(cachePath)) {
      fs::dir_create(cachePath)
    }
    return(cachePath)
  }

  if (cachePath == FALSE) {
    cachePath <- tools::R_user_dir("BenchHub", which = "cache")
    defaultPath <- TRUE
  }
  cacheExists <- fs::dir_exists(cachePath)

  if (!defaultPath) {
    # if the user specified path does not exist, ensure it is correct
    if (!cacheExists) {
      create <- .getAnswer(
        cli::cli_text("Spicified path ({.path {cachePath}}) does not exit"),
        "Create it? ([y]/n) "
      )
      if (create) {
        keep <- TRUE
      } else {
        keep <- FALSE
      }
    } else {
      # if user specfied a cache path and it already exists, return it.
      return(cachePath)
    }
  } else if (defaultPath && cacheExists) {
    keep <- .getAnswer(
      cli::cli_text("Default cache was found at {.path {cachePath}}."),
      "Would you like to use this path? ([y]/n) "
    )
  } else {
    keep <- .getAnswer(
      cli::cli_text("Default cache path is {.path {cachePath}}."),
      "Would you like to cache downloaded datasets here? ([y]/n) "
    )
  }

  if (keep) {
    if (!cacheExists) {
      fs::dir_create(cachePath)
    }

    return(cachePath)
  }

  while (TRUE) {
    userPath <- readline("Where would you like to store your cache? ")

    if (fs::dir_exists(userPath)) {
      cli::cli_inform(
        "Creating data cache at {.path {fs::path_expand(userPath)}}"
      )
      cachePath <- userPath
      break
    } else {
      create <- .getAnswer(
        cli::cli_text("{.path {userPath}} does not exist."),
        "Would you like the create it? ([y]/n) "
      )
      if (create) {
        cli::cli_inform(
          "Creating data cache at {.path {fs::path_expand(userPath)}}"
        )
        fs::dir_create(userPath)
        cachePath <- userPath
        break
      }
    }
  }

  cachePath
}


assertSuggestAvail <- function(packages) {
  lapply(packages, \(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      cli::cli_abort(c(
        "{.pkg package} is required for this functionality.",
        "i" = "You can get it by running: {.code install.packages('{package}')}"
      ))
    }
  })
}


figshareListFiles <- function(articleID, fileID = NULL) {
  API_URL <- "https://api.figshare.com/v2/"

  # Create request URL
  # Assumption: No article will have more that 100 files...
  requestUrl <- glue::glue(
    API_URL,
    "articles/{articleID}/files",
    ifelse(!is.null(fileID), paste0("/", fileID), ""),
    "?page_size=100"
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

  do.call(rbind, lapply(body, data.frame))
}

isTabular <- function(x) inherits(x, c("data.frame", "DataFrame", "matrix"))
