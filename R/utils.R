getData <- function(datasetID, cachePath) {
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
    files <- do.call(
      paste0(sourceName, "Dl"),
      list("ID" = id, "cachePath" = cachePath)
    )
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


getTrioCachePath <- function(cachePath) {
  defaultPath <- FALSE

  # if cachePath is TRUE, use the default cache location without prompting user
  if (cachePath == TRUE) {
    cachePath <- fs::path_join(
      c(tools::R_user_dir("", which = "cache"), "TrioR")
    )
    if (!fs::dir_exists(cachePath)) fs::dir_create(cachePath)
    return(cachePath)
  }

  if (is.null(cachePath)) {
    cachePath <- fs::path_join(
      c(tools::R_user_dir("", which = "cache"), "TrioR")
    )
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
    if (!cacheExists) fs::dir_create(cachePath)

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


assertSuggestAvail <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg package} is required to to get data from {.url figshare.com}.",
      "i" = "You can get it by running: {.code install.packages('{package}')}"
    ))
  }
}
