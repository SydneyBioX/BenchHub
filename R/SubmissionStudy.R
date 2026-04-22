# Default Google Apps Script endpoint for submission workflows.
submission_webapp_url <- "https://script.google.com/macros/s/AKfycbx2kgx2N0lbAlr0Q35PEwYsy3sFKvnWZVYEmjRsHDSRFEIWB-TLFM3r4HEd09TNfFxO/exec"

#' List available datasets for Study submission
#'
#' @param ss Submission spreadsheet ID or URL.
#'
#' @return A data frame of existing Dataset rows.
#' @export
listSubmissionStudyDatasets <- function(
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg"
) {
  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection available.")
  }

  private_read_submission_database_sheet(ss = ss, sheet = "Dataset")
}

#' List existing Study rows
#'
#' @param ss Submission spreadsheet ID or URL.
#'
#' @return A data frame of existing Study rows.
#' @export
listSubmissionStudies <- function(
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg"
) {
  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection available.")
  }

  private_read_submission_database_sheet(ss = ss, sheet = "Study")
}

#' Get one existing Study row by studyID
#'
#' @param studyID Existing Study identifier.
#' @param studies Optional data frame of Study rows.
#' @param ss Submission spreadsheet ID or URL.
#'
#' @return A one-row data frame for the requested Study.
#' @examples
#' getSubmissionStudy(
#'   "study_001",
#'   studies = BenchHub:::private_example_existing_study_rows()
#' )
#' @export
getSubmissionStudy <- function(
    studyID,
    studies = NULL,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg"
) {
  study_id <- private_required_submission_chr(studyID, "studyID")
  studies <- private_resolve_submission_studies(studies = studies, ss = ss)

  if (!"studyID" %in% names(studies)) {
    cli::cli_abort("{.arg studies} must include a {.val studyID} column.")
  }

  matches <- private_submission_chr_vec(studies$studyID) == study_id
  if (!any(matches)) {
    cli::cli_abort("Study {.val {study_id}} was not found in the submission sheet.")
  }

  studies[matches, , drop = FALSE][1, , drop = FALSE]
}

#' Get linked StudyDataset rows by studyID
#'
#' @param studyID Existing Study identifier.
#' @param study_datasets Optional data frame of StudyDataset rows.
#' @param ss Submission spreadsheet ID or URL.
#'
#' @return A data frame of StudyDataset rows linked to the supplied studyID.
#' @examples
#' getSubmissionStudyDatasets(
#'   "study_001",
#'   study_datasets = BenchHub:::private_example_study_dataset_rows()
#' )
#' @export
getSubmissionStudyDatasets <- function(
    studyID,
    study_datasets = NULL,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg"
) {
  study_id <- private_required_submission_chr(studyID, "studyID")
  study_datasets <- private_resolve_submission_study_datasets(
    study_datasets = study_datasets,
    ss = ss
  )

  if (!"studyID" %in% names(study_datasets)) {
    cli::cli_abort("{.arg study_datasets} must include a {.val studyID} column.")
  }

  matches <- private_submission_chr_vec(study_datasets$studyID) == study_id
  study_datasets[matches, , drop = FALSE]
}

#' Collect Study submission metadata
#'
#' @param study A `BenchmarkStudy` object.
#' @param datasetIDs Optional character vector of dataset IDs. When `NULL`,
#'   datasets can be inferred from `study$trios` or selected interactively.
#' @param available_datasets Optional data frame of available Dataset rows.
#' @param existing_studies Optional data frame of current Study rows.
#' @param ss Submission spreadsheet ID or URL.
#' @param defaults Optional named list with entries such as `datasetIDs`,
#'   `version`, `type`, `protocolGist`, and `mappingFunctions`.
#'
#' @return A named list ready to pass into `buildStudySubmission()`.
#' @examples
#' study <- BenchHub:::private_example_study()
#' collectStudySubmissionInfo(
#'   study,
#'   datasetIDs = "dataset_001",
#'   available_datasets = BenchHub:::private_example_available_datasets(),
#'   existing_studies = BenchHub:::private_example_existing_studies()
#' )
#' @export
collectStudySubmissionInfo <- function(
    study,
    datasetIDs = NULL,
    available_datasets = NULL,
    existing_studies = NULL,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
    defaults = list()
) {
  if (!inherits(study, "BenchmarkStudy")) {
    cli::cli_abort("{.arg study} must be a {.cls BenchmarkStudy} object.")
  }

  if (!is.list(defaults)) {
    cli::cli_abort("{.arg defaults} must be a named list.")
  }

  if (is.null(available_datasets)) {
    if (!curl::has_internet()) {
      cli::cli_abort(
        "No internet connection available. Supply {.arg available_datasets} to collect Study submission information offline."
      )
    }
    available_datasets <- listSubmissionStudyDatasets(ss = ss)
  }

  if (is.null(existing_studies)) {
    if (!curl::has_internet()) {
      cli::cli_abort(
        "No internet connection available. Supply {.arg existing_studies} to collect Study submission information offline."
      )
    }
    existing_studies <- private_read_submission_database_sheet(ss = ss, sheet = "Study")
  }

  resolved_dataset_ids <- private_collect_study_dataset_ids(
    study = study,
    datasetIDs = if (is.null(datasetIDs)) defaults$datasetIDs else datasetIDs,
    available_datasets = available_datasets
  )

  study_name <- private_required_submission_chr(study$name, "study$name")
  existing_studies <- private_as_study_submission_df(existing_studies)

  type <- if (!is.null(defaults$type)) {
    private_required_submission_chr(defaults$type, "defaults$type")
  } else {
    private_study_submission_type(study_name, existing_studies)
  }
  private_validate_submission_choice(type, "type", c("original", "update"))

  version <- if (!is.null(defaults$version)) {
    private_required_submission_chr(defaults$version, "defaults$version")
  } else {
    private_study_submission_version(study_name, existing_studies, type = type)
  }

  list(
    datasetIDs = resolved_dataset_ids,
    type = type,
    version = version,
    protocolGist = private_collect_submission_text(
      value = defaults$protocolGist,
      prompt = "Protocol gist URL (optional)",
      required = FALSE
    ),
    mappingFunctions = private_collect_submission_text(
      value = defaults$mappingFunctions,
      prompt = "Mapping functions gist URL (optional)",
      required = FALSE
    )
  )
}

#' Build Study and StudyDataset submission tables
#'
#' @param study A `BenchmarkStudy` object.
#' @param datasetIDs Character vector of existing dataset IDs to link.
#' @param existing_studies Optional data frame of current Study rows.
#' @param ss Submission spreadsheet ID or URL. Used when `existing_studies` is
#'   not supplied.
#' @param version Optional version override.
#' @param type Optional type override. Must be `"original"` or `"update"` when
#'   provided.
#' @param protocolGist Optional protocol gist URL.
#' @param mappingFunctions Optional mapping functions gist URL.
#'
#' @return A named list containing `Study` and `StudyDataset`.
#' @examples
#' study <- BenchHub:::private_example_study()
#' buildStudySubmission(
#'   study,
#'   datasetIDs = "dataset_001",
#'   existing_studies = BenchHub:::private_example_existing_studies()
#' )
#' @export
buildStudySubmission <- function(
    study,
    datasetIDs,
    existing_studies = NULL,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
    version = NULL,
    type = NULL,
    protocolGist = "",
    mappingFunctions = ""
) {
  if (!inherits(study, "BenchmarkStudy")) {
    cli::cli_abort("{.arg study} must be a {.cls BenchmarkStudy} object.")
  }

  study_name <- private_required_submission_chr(study$name, "study$name")
  description <- private_required_submission_chr(study$description, "study$description")
  dataset_ids <- unique(private_required_submission_chr_vec(datasetIDs, "datasetIDs"))

  if (length(dataset_ids) == 0) {
    cli::cli_abort("At least one {.arg datasetIDs} value must be supplied.")
  }

  if (is.null(existing_studies)) {
    if (!curl::has_internet()) {
      cli::cli_abort(
        "No internet connection available. Supply {.arg existing_studies} to build a Study submission offline."
      )
    }
    existing_studies <- private_read_submission_database_sheet(ss = ss, sheet = "Study")
  }

  existing_studies <- private_as_study_submission_df(existing_studies)

  if (!is.null(type)) {
    private_validate_submission_choice(type, "type", c("original", "update"))
  } else {
    type <- private_study_submission_type(study_name, existing_studies)
  }

  if (!is.null(version)) {
    version <- private_required_submission_chr(version, "version")
  } else {
    version <- private_study_submission_version(study_name, existing_studies, type = type)
  }

  study_tbl <- data.frame(
    studyID = NA_character_,
    studyName = study_name,
    version = version,
    description = description,
    type = type,
    protocolGist = private_submission_chr(protocolGist),
    mappingFunctions = private_submission_chr(mappingFunctions),
    stringsAsFactors = FALSE
  )

  study_dataset_tbl <- data.frame(
    studyDatasetID = rep(NA_character_, length(dataset_ids)),
    studyID = rep(NA_character_, length(dataset_ids)),
    datasetID = dataset_ids,
    stringsAsFactors = FALSE
  )

  list(
    Study = study_tbl,
    StudyDataset = study_dataset_tbl
  )
}

#' Convert a Study submission to payload structure
#'
#' @param submission A submission object returned by `buildStudySubmission()`.
#'
#' @return A named list with a top-level `payload` entry.
#' @examples
#' submission <- BenchHub:::private_example_study_submission()
#' payload <- buildStudySubmissionPayload(submission)
#' names(payload)
#' @export
buildStudySubmissionPayload <- function(submission) {
  required_names <- c("Study", "StudyDataset")

  if (!is.list(submission)) {
    cli::cli_abort("{.arg submission} must be a submission list.")
  }

  missing_names <- setdiff(required_names, names(submission))
  if (length(missing_names) > 0) {
    cli::cli_abort(c(
      "The submission object is missing required tables.",
      "i" = "Missing: {.val {missing_names}}"
    ))
  }

  study_records <- private_submission_df_to_records(submission$Study)
  if (length(study_records) != 1) {
    cli::cli_abort("The Study submission must contain exactly one Study row.")
  }

  list(
    payload = list(
      Study = study_records[[1]],
      StudyDataset = private_submission_df_to_records(submission$StudyDataset)
    )
  )
}

#' Convert a Study submission to JSON
#'
#' @param submission A submission object returned by `buildStudySubmission()`.
#' @param pretty Whether to pretty-print the JSON. Defaults to `TRUE`.
#'
#' @return A JSON string.
#' @importFrom jsonlite toJSON
#' @examples
#' submission <- BenchHub:::private_example_study_submission()
#' json <- studySubmissionToJSON(submission)
#' substr(json, 1, 20)
#' @export
studySubmissionToJSON <- function(
    submission,
    pretty = TRUE
) {
  payload <- buildStudySubmissionPayload(submission = submission)

  jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    pretty = pretty,
    null = "null",
    na = "null"
  )
}

#' Submit a Study submission payload to Google Apps Script
#'
#' @param submission A submission object returned by `buildStudySubmission()`.
#' @param url Google Apps Script endpoint URL.
#' @param submittedBy Submitter email or identifier.
#'
#' @return A list containing request status information and response text.
#' @examples
#' \dontrun{
#' submission <- BenchHub:::private_example_study_submission()
#' submitStudySubmission(
#'   submission,
#'   url = "https://script.google.com/macros/s/example/exec",
#'   submittedBy = "researcher@example.org"
#' )
#' }
#' @export
submitStudySubmission <- function(
    submission,
    url,
    submittedBy
) {
  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection available.")
  }

  if (missing(url) || is.null(url) || !is.character(url) ||
      length(url) != 1 || is.na(url) || !nzchar(url)) {
    cli::cli_abort(
      "{.arg url} must be a single non-empty character string."
    )
  }

  if (missing(submittedBy) || is.null(submittedBy) ||
      !is.character(submittedBy) || length(submittedBy) != 1 ||
      is.na(submittedBy) || !nzchar(submittedBy)) {
    cli::cli_abort(
      "{.arg submittedBy} must be a single non-empty character string."
    )
  }

  payload <- buildStudySubmissionPayload(submission = submission)

  body <- list(
    submittedBy = submittedBy,
    submittedType = "Study",
    payload = payload
  )

  resp1 <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_options(followlocation = FALSE) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status1 <- httr2::resp_status(resp1)
  headers1 <- httr2::resp_headers(resp1)

  if (status1 %in% c(301, 302, 303, 307, 308) &&
      !is.null(headers1[["location"]])) {
    resp2 <- httr2::request(headers1[["location"]]) |>
      httr2::req_method("GET") |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    return(list(
      ok = httr2::resp_status(resp2) == 200,
      post_status = status1,
      final_status = httr2::resp_status(resp2),
      final_body = httr2::resp_body_string(resp2)
    ))
  }

  list(
    ok = status1 == 200,
    post_status = status1,
    final_status = status1,
    final_body = httr2::resp_body_string(resp1)
  )
}

#' Prepare a Study submission bundle
#'
#' @param study A `BenchmarkStudy` object.
#' @param datasetIDs Optional character vector of existing dataset IDs to link.
#' @param available_datasets Optional data frame of Dataset rows for selection.
#' @param existing_studies Optional data frame of Study rows.
#' @param defaults Optional named list for `collectStudySubmissionInfo()`.
#' @param protocolGist Optional protocol gist URL.
#' @param mappingFunctions Optional mapping functions gist URL.
#' @param protocolFile Optional local file path to upload as a protocol gist.
#' @param mappingFunctionsFile Optional local file path to upload as a mapping
#'   functions gist.
#' @param uploadProtocol Logical; whether to upload `protocolFile` to a gist.
#' @param uploadMappingFunctions Logical; whether to upload
#'   `mappingFunctionsFile` to a gist.
#' @param githubPat Optional GitHub personal access token.
#' @param gistPublic Logical; whether uploaded gists should be public.
#' @param ss Submission spreadsheet ID or URL.
#' @param build_payload Whether to include the nested payload. Defaults to
#'   `TRUE`.
#' @param build_json Whether to include JSON output. Defaults to `TRUE`.
#' @param review Whether to print a short submission summary. Defaults to
#'   `TRUE`.
#' @param submit Whether to submit immediately. Defaults to `FALSE`.
#' @param url Google Apps Script endpoint URL. Defaults to the package
#'   submission web app.
#' @param submittedBy Submitter email or identifier.
#'
#' @return A named list containing the Study submission bundle.
#' @examples
#' study <- BenchHub:::private_example_study()
#' result <- prepareStudySubmission(
#'   study,
#'   datasetIDs = "dataset_001",
#'   available_datasets = BenchHub:::private_example_available_datasets(),
#'   existing_studies = BenchHub:::private_example_existing_studies(),
#'   build_json = FALSE,
#'   review = FALSE
#' )
#' names(result)
#' @export
prepareStudySubmission <- function(
    study,
    datasetIDs = NULL,
    available_datasets = NULL,
    existing_studies = NULL,
    defaults = list(),
    protocolGist = NULL,
    mappingFunctions = NULL,
    protocolFile = NULL,
    mappingFunctionsFile = NULL,
    uploadProtocol = FALSE,
    uploadMappingFunctions = FALSE,
    githubPat = Sys.getenv("GITHUB_PAT"),
    gistPublic = TRUE,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
    build_payload = TRUE,
    build_json = TRUE,
    review = TRUE,
    submit = FALSE,
    url = submission_webapp_url,
    submittedBy = NULL
) {
  if (!inherits(study, "BenchmarkStudy")) {
    cli::cli_abort("{.arg study} must be a {.cls BenchmarkStudy} object.")
  }

  if (!is.list(defaults)) {
    cli::cli_abort("{.arg defaults} must be a named list.")
  }

  available_datasets <- if (is.null(available_datasets)) {
    listSubmissionStudyDatasets(ss = ss)
  } else {
    available_datasets
  }

  existing_studies <- if (is.null(existing_studies)) {
    private_read_submission_database_sheet(ss = ss, sheet = "Study")
  } else {
    existing_studies
  }

  study_info <- collectStudySubmissionInfo(
    study = study,
    datasetIDs = datasetIDs,
    available_datasets = available_datasets,
    existing_studies = existing_studies,
    ss = ss,
    defaults = defaults
  )

  final_protocol_gist <- private_submission_chr(protocolGist)
  if (is.na(final_protocol_gist)) {
    final_protocol_gist <- study_info$protocolGist
  }

  final_mapping_functions <- private_submission_chr(mappingFunctions)
  if (is.na(final_mapping_functions)) {
    final_mapping_functions <- study_info$mappingFunctions
  }

  if (isTRUE(uploadProtocol)) {
    final_protocol_gist <- private_upload_study_submission_gist(
      file = protocolFile,
      description = paste0("Protocol for BenchmarkStudy ", study$name),
      githubPat = githubPat,
      public = gistPublic
    )
  }

  if (isTRUE(uploadMappingFunctions)) {
    if (is.null(mappingFunctionsFile)) {
      mappingFunctionsFile <- private_write_study_mapping_functions_file(study)
    }

    final_mapping_functions <- private_upload_study_submission_gist(
      file = mappingFunctionsFile,
      description = paste0("Mapping functions for BenchmarkStudy ", study$name),
      githubPat = githubPat,
      public = gistPublic
    )
  }

  submission <- buildStudySubmission(
    study = study,
    datasetIDs = study_info$datasetIDs,
    existing_studies = existing_studies,
    ss = ss,
    version = study_info$version,
    type = study_info$type,
    protocolGist = final_protocol_gist,
    mappingFunctions = final_mapping_functions
  )

  result <- list(
    study_info = study_info,
    submission = submission
  )

  if (isTRUE(review)) {
    private_inform_study_submission_review(submission)
  }

  if (isTRUE(build_payload)) {
    result$payload <- buildStudySubmissionPayload(submission)
  }

  if (isTRUE(build_json)) {
    result$json <- studySubmissionToJSON(submission)
  }

  if (isTRUE(submit)) {
    if (is.null(url) || !nzchar(as.character(url))) {
      if (!interactive()) {
        cli::cli_abort("{.arg url} must be provided when {.arg submit = TRUE} in non-interactive mode.")
      }
      url <- private_collect_submission_text(
        prompt = "Google Apps Script URL",
        required = TRUE
      )
    }

    if (is.null(submittedBy) || !nzchar(as.character(submittedBy))) {
      if (!interactive()) {
        cli::cli_abort("{.arg submittedBy} must be provided when {.arg submit = TRUE} in non-interactive mode.")
      }
      submittedBy <- private_collect_submission_text(
        prompt = "Submitted by",
        required = TRUE
      )
    }

    result$submit_response <- submitStudySubmission(
      submission = submission,
      url = url,
      submittedBy = submittedBy
    )
  }

  result
}

#' Interactively prepare a Study submission
#'
#' @param study A `BenchmarkStudy` object.
#' @param ss Submission spreadsheet ID or URL.
#' @param githubPat Optional GitHub personal access token.
#' @param gistPublic Logical; whether uploaded gists should be public.
#' @param url Google Apps Script endpoint URL. Defaults to the package
#'   submission web app.
#' @param review Whether to print a short submission summary. Defaults to
#'   `TRUE`.
#' @param build_payload Whether to include the nested payload. Defaults to
#'   `TRUE`.
#' @param build_json Whether to include JSON output. Defaults to `TRUE`.
#'
#' @return A named list containing the Study submission bundle.
#' @export
interactivePrepareStudySubmission <- function(
    study,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
    githubPat = Sys.getenv("GITHUB_PAT"),
    gistPublic = TRUE,
    url = submission_webapp_url,
    review = TRUE,
    build_payload = TRUE,
    build_json = TRUE
) {
  if (!inherits(study, "BenchmarkStudy")) {
    cli::cli_abort("{.arg study} must be a {.cls BenchmarkStudy} object.")
  }

  if (!interactive()) {
    cli::cli_abort(
      "{.fn interactivePrepareStudySubmission} must be run interactively."
    )
  }

  available_datasets <- listSubmissionStudyDatasets(ss = ss)
  existing_studies <- private_read_submission_database_sheet(ss = ss, sheet = "Study")

  cli::cli_inform(c(
    "Preparing an interactive Study submission bundle.",
    "i" = "Study name: {.val {study$name}}"
  ))

  study$description <- private_collect_submission_text(
    value = private_submission_chr(study$description),
    prompt = "Please describe your benchmark study",
    fallback = private_submission_chr(study$description),
    required = TRUE
  )

  defaults <- list(
    protocolGist = "",
    mappingFunctions = ""
  )

  study_info <- collectStudySubmissionInfo(
    study = study,
    available_datasets = available_datasets,
    existing_studies = existing_studies,
    ss = ss,
    defaults = defaults
  )

  protocol_choice <- utils::menu(
    c(
      "Leave blank",
      "Use existing protocol gist URL",
      "Upload local protocol file to gist"
    ),
    title = "Protocol input"
  )
  if (protocol_choice == 0) {
    cli::cli_abort("A selection for protocol input is required.")
  }

  protocol_gist <- study_info$protocolGist
  upload_protocol <- FALSE
  protocol_file <- NULL
  if (protocol_choice == 2) {
    protocol_gist <- private_collect_submission_text(
      prompt = "Protocol gist URL",
      required = TRUE
    )
  } else if (protocol_choice == 3) {
    upload_protocol <- TRUE
    protocol_file <- private_collect_submission_text(
      prompt = "Path to protocol file",
      required = TRUE
    )
  }

  mapping_choice <- utils::menu(
    c(
      "Leave blank",
      "Use existing mapping functions gist URL",
      if (length(study$mappingFunctions) > 0) {
        "Upload mapping functions stored in the study object"
      },
      "Upload local mapping functions file to gist"
    ),
    title = "Mapping functions input"
  )
  if (mapping_choice == 0) {
    cli::cli_abort("A selection for mapping functions input is required.")
  }

  mapping_functions <- study_info$mappingFunctions
  upload_mapping_functions <- FALSE
  mapping_functions_file <- NULL
  if (mapping_choice == 2) {
    mapping_functions <- private_collect_submission_text(
      prompt = "Mapping functions gist URL",
      required = TRUE
    )
  } else if (length(study$mappingFunctions) > 0 && mapping_choice == 3) {
    upload_mapping_functions <- TRUE
  } else if ((length(study$mappingFunctions) > 0 && mapping_choice == 4) ||
             (length(study$mappingFunctions) == 0 && mapping_choice == 3)) {
    upload_mapping_functions <- TRUE
    mapping_functions_file <- private_collect_submission_text(
      prompt = "Path to mapping functions file",
      required = TRUE
    )
  }

  submit_now <- utils::askYesNo("Do you want to submit this Study now?")
  submit_url <- url
  submit_by <- NULL

  if (isTRUE(submit_now)) {
    submit_by <- private_collect_submission_text(
      prompt = "Submitted by",
      required = TRUE
    )
  }

  prepareStudySubmission(
    study = study,
    datasetIDs = study_info$datasetIDs,
    available_datasets = available_datasets,
    existing_studies = existing_studies,
    defaults = list(
      version = study_info$version,
      type = study_info$type,
      protocolGist = protocol_gist,
      mappingFunctions = mapping_functions
    ),
    protocolGist = protocol_gist,
    mappingFunctions = mapping_functions,
    protocolFile = protocol_file,
    mappingFunctionsFile = mapping_functions_file,
    uploadProtocol = upload_protocol,
    uploadMappingFunctions = upload_mapping_functions,
    githubPat = githubPat,
    gistPublic = gistPublic,
    ss = ss,
    build_payload = build_payload,
    build_json = build_json,
    review = review,
    submit = isTRUE(submit_now),
    url = submit_url,
    submittedBy = submit_by
  )
}

#' Prepare an update submission from an existing Study version
#'
#' @param studyID Existing Study identifier to use as the update baseline.
#' @param study Optional `BenchmarkStudy` object. When omitted, a new one is
#'   created from the existing Study row.
#' @param datasetIDs Optional replacement dataset IDs. Defaults to the linked
#'   datasets from the baseline Study.
#' @param description Optional replacement description.
#' @param protocolGist Optional replacement protocol gist URL.
#' @param mappingFunctions Optional replacement mapping functions gist URL.
#' @param studies Optional data frame of Study rows.
#' @param study_datasets Optional data frame of StudyDataset rows.
#' @param ss Submission spreadsheet ID or URL.
#' @param build_payload Whether to include the nested payload. Defaults to
#'   `TRUE`.
#' @param build_json Whether to include JSON output. Defaults to `TRUE`.
#' @param review Whether to print a short submission summary. Defaults to
#'   `TRUE`.
#' @param submit Whether to submit immediately. Defaults to `FALSE`.
#' @param url Google Apps Script endpoint URL. Defaults to the package
#'   submission web app.
#' @param submittedBy Submitter email or identifier.
#'
#' @return A named list containing the updated Study submission bundle.
#' @examples
#' result <- prepareStudyUpdateSubmission(
#'   studyID = "study_001",
#'   studies = BenchHub:::private_example_existing_study_rows(),
#'   study_datasets = BenchHub:::private_example_study_dataset_rows(),
#'   build_json = FALSE,
#'   review = FALSE
#' )
#' names(result)
#' @export
prepareStudyUpdateSubmission <- function(
    studyID,
    study = NULL,
    datasetIDs = NULL,
    description = NULL,
    protocolGist = NULL,
    mappingFunctions = NULL,
    studies = NULL,
    study_datasets = NULL,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
    build_payload = TRUE,
    build_json = TRUE,
    review = TRUE,
    submit = FALSE,
    url = submission_webapp_url,
    submittedBy = NULL
) {
  existing_study <- getSubmissionStudy(
    studyID = studyID,
    studies = studies,
    ss = ss
  )
  linked_datasets <- getSubmissionStudyDatasets(
    studyID = studyID,
    study_datasets = study_datasets,
    ss = ss
  )

  if (is.null(study)) {
    study <- BenchmarkStudy$new(name = private_required_submission_chr(
      existing_study$studyName[[1]],
      "studyName"
    ))
  }

  if (!inherits(study, "BenchmarkStudy")) {
    cli::cli_abort("{.arg study} must be a {.cls BenchmarkStudy} object.")
  }

  study$name <- private_required_submission_chr(existing_study$studyName[[1]], "studyName")
  study$description <- private_submission_chr(description)
  if (is.na(study$description)) {
    study$description <- private_required_submission_chr(
      existing_study$description[[1]],
      "description"
    )
  }

  default_dataset_ids <- if (is.null(datasetIDs)) {
    private_submission_chr_vec(linked_datasets$datasetID)
  } else {
    datasetIDs
  }

  default_protocol_gist <- private_submission_chr(protocolGist)
  if (is.na(default_protocol_gist) && "protocolGist" %in% names(existing_study)) {
    default_protocol_gist <- private_submission_chr(existing_study$protocolGist[[1]])
  }

  default_mapping_functions <- private_submission_chr(mappingFunctions)
  if (is.na(default_mapping_functions) && "mappingFunctions" %in% names(existing_study)) {
    default_mapping_functions <- private_submission_chr(existing_study$mappingFunctions[[1]])
  }

  prepareStudySubmission(
    study = study,
    datasetIDs = default_dataset_ids,
    existing_studies = private_resolve_submission_studies(studies = studies, ss = ss),
    defaults = list(
      type = "update",
      protocolGist = default_protocol_gist,
      mappingFunctions = default_mapping_functions
    ),
    protocolGist = default_protocol_gist,
    mappingFunctions = default_mapping_functions,
    ss = ss,
    build_payload = build_payload,
    build_json = build_json,
    review = review,
    submit = submit,
    url = url,
    submittedBy = submittedBy
  )
}

#' Interactively prepare a Study update submission
#'
#' @param studyID Optional existing Study identifier to update. When omitted,
#'   the user is prompted to choose one.
#' @param ss Submission spreadsheet ID or URL.
#' @param githubPat Optional GitHub personal access token.
#' @param gistPublic Logical; whether uploaded gists should be public.
#' @param url Google Apps Script endpoint URL. Defaults to the package
#'   submission web app.
#' @param review Whether to print a short submission summary. Defaults to
#'   `TRUE`.
#' @param build_payload Whether to include the nested payload. Defaults to
#'   `TRUE`.
#' @param build_json Whether to include JSON output. Defaults to `TRUE`.
#'
#' @return A named list containing the updated Study submission bundle.
#' @export
interactivePrepareStudyUpdateSubmission <- function(
    studyID = NULL,
    ss = "1H8hOxL8D0XTquao8vGZ2cr9-XeaFC48SWAdFn0M3fkg",
    githubPat = Sys.getenv("GITHUB_PAT"),
    gistPublic = TRUE,
    url = submission_webapp_url,
    review = TRUE,
    build_payload = TRUE,
    build_json = TRUE
) {
  if (!interactive()) {
    cli::cli_abort(
      "{.fn interactivePrepareStudyUpdateSubmission} must be run interactively."
    )
  }

  studies <- listSubmissionStudies(ss = ss)
  study_datasets <- private_read_submission_database_sheet(ss = ss, sheet = "StudyDataset")
  available_datasets <- listSubmissionStudyDatasets(ss = ss)

  chosen_study_id <- private_collect_existing_study_id(
    studyID = studyID,
    studies = studies
  )
  existing_study <- getSubmissionStudy(chosen_study_id, studies = studies, ss = ss)
  linked_datasets <- getSubmissionStudyDatasets(
    chosen_study_id,
    study_datasets = study_datasets,
    ss = ss
  )

  study <- BenchmarkStudy$new(name = private_required_submission_chr(
    existing_study$studyName[[1]],
    "studyName"
  ))
  study$description <- private_collect_submission_text(
    value = private_submission_chr(existing_study$description[[1]]),
    prompt = "Please describe your benchmark study",
    fallback = private_submission_chr(existing_study$description[[1]]),
    required = TRUE
  )

  cli::cli_inform(c(
    "Preparing an update from an existing Study version.",
    "i" = "Base studyID: {.val {chosen_study_id}}",
    "i" = "Base version: {.val {existing_study$version[[1]]}}"
  ))

  current_dataset_ids <- private_submission_chr_vec(linked_datasets$datasetID)
  dataset_choice <- utils::menu(
    c(
      paste0("Keep current linked datasets: ", paste(current_dataset_ids, collapse = ", ")),
      "Choose a new set of linked datasets"
    ),
    title = "StudyDataset update"
  )
  if (dataset_choice == 0) {
    cli::cli_abort("A selection for StudyDataset update is required.")
  }

  selected_dataset_ids <- if (dataset_choice == 1) {
    current_dataset_ids
  } else {
    private_select_study_dataset_ids(available_datasets)
  }

  protocol_choice <- utils::menu(
    c(
      "Keep current protocol gist",
      "Leave blank",
      "Use a different protocol gist URL",
      "Upload local protocol file to gist"
    ),
    title = "Protocol input"
  )
  if (protocol_choice == 0) {
    cli::cli_abort("A selection for protocol input is required.")
  }

  protocol_gist <- if ("protocolGist" %in% names(existing_study)) {
    private_submission_chr(existing_study$protocolGist[[1]])
  } else {
    NA_character_
  }
  upload_protocol <- FALSE
  protocol_file <- NULL
  if (protocol_choice == 2) {
    protocol_gist <- NA_character_
  } else if (protocol_choice == 3) {
    protocol_gist <- private_collect_submission_text(
      prompt = "Protocol gist URL",
      required = TRUE
    )
  } else if (protocol_choice == 4) {
    upload_protocol <- TRUE
    protocol_file <- private_collect_submission_text(
      prompt = "Path to protocol file",
      required = TRUE
    )
  }

  mapping_choice <- utils::menu(
    c(
      "Keep current mapping functions gist",
      "Leave blank",
      "Use a different mapping functions gist URL",
      "Upload local mapping functions file to gist"
    ),
    title = "Mapping functions input"
  )
  if (mapping_choice == 0) {
    cli::cli_abort("A selection for mapping functions input is required.")
  }

  mapping_functions <- if ("mappingFunctions" %in% names(existing_study)) {
    private_submission_chr(existing_study$mappingFunctions[[1]])
  } else {
    NA_character_
  }
  upload_mapping_functions <- FALSE
  mapping_functions_file <- NULL
  if (mapping_choice == 2) {
    mapping_functions <- NA_character_
  } else if (mapping_choice == 3) {
    mapping_functions <- private_collect_submission_text(
      prompt = "Mapping functions gist URL",
      required = TRUE
    )
  } else if (mapping_choice == 4) {
    upload_mapping_functions <- TRUE
    mapping_functions_file <- private_collect_submission_text(
      prompt = "Path to mapping functions file",
      required = TRUE
    )
  }

  submit_now <- utils::askYesNo("Do you want to submit this Study update now?")
  submit_by <- NULL
  if (isTRUE(submit_now)) {
    submit_by <- private_collect_submission_text(
      prompt = "Submitted by",
      required = TRUE
    )
  }

  result <- prepareStudyUpdateSubmission(
    studyID = chosen_study_id,
    study = study,
    datasetIDs = selected_dataset_ids,
    description = study$description,
    protocolGist = protocol_gist,
    mappingFunctions = mapping_functions,
    studies = studies,
    study_datasets = study_datasets,
    ss = ss,
    build_payload = build_payload,
    build_json = build_json,
    review = review,
    submit = isTRUE(submit_now),
    url = url,
    submittedBy = submit_by
  )

  if (upload_protocol) {
    result <- prepareStudySubmission(
      study = study,
      datasetIDs = result$submission$StudyDataset$datasetID,
      available_datasets = available_datasets,
      existing_studies = studies,
      defaults = list(
        version = result$submission$Study$version[[1]],
        type = result$submission$Study$type[[1]],
        protocolGist = protocol_gist,
        mappingFunctions = mapping_functions
      ),
      protocolGist = protocol_gist,
      mappingFunctions = mapping_functions,
      protocolFile = protocol_file,
      uploadProtocol = TRUE,
      githubPat = githubPat,
      gistPublic = gistPublic,
      ss = ss,
      build_payload = build_payload,
      build_json = build_json,
      review = review,
      submit = isTRUE(submit_now),
      url = url,
      submittedBy = submit_by
    )
  }

  if (upload_mapping_functions) {
    result <- prepareStudySubmission(
      study = study,
      datasetIDs = result$submission$StudyDataset$datasetID,
      available_datasets = available_datasets,
      existing_studies = studies,
      defaults = list(
        version = result$submission$Study$version[[1]],
        type = result$submission$Study$type[[1]],
        protocolGist = result$submission$Study$protocolGist[[1]],
        mappingFunctions = mapping_functions
      ),
      protocolGist = result$submission$Study$protocolGist[[1]],
      mappingFunctions = mapping_functions,
      mappingFunctionsFile = mapping_functions_file,
      uploadMappingFunctions = TRUE,
      githubPat = githubPat,
      gistPublic = gistPublic,
      ss = ss,
      build_payload = build_payload,
      build_json = build_json,
      review = review,
      submit = isTRUE(submit_now),
      url = url,
      submittedBy = submit_by
    )
  }

  result
}

private_required_submission_chr <- function(x, arg) {
  if (missing(x) || is.null(x) || length(x) != 1 || is.na(x) || !nzchar(as.character(x))) {
    cli::cli_abort(
      "The {.arg {arg}} argument must be a single non-empty string."
    )
  }

  as.character(x)
}

private_required_submission_chr_vec <- function(x, arg) {
  if (missing(x) || is.null(x)) {
    cli::cli_abort(
      "The {.arg {arg}} argument must be a non-empty character vector."
    )
  }

  x_chr <- private_submission_chr_vec(x)
  names(x_chr) <- names(x)
  x_chr <- x_chr[!is.na(x_chr)]

  if (length(x_chr) == 0) {
    cli::cli_abort(
      "The {.arg {arg}} argument must contain at least one non-empty value."
    )
  }

  x_chr
}

private_as_study_submission_df <- function(x) {
  if (is.null(x)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg existing_studies} must be a data.frame.")
  }

  x
}

private_study_submission_type <- function(study_name, existing_studies) {
  if (!"studyName" %in% names(existing_studies) || nrow(existing_studies) == 0) {
    return("original")
  }

  if (study_name %in% private_submission_chr_vec(existing_studies$studyName)) {
    return("update")
  }

  "original"
}

private_study_submission_version <- function(study_name, existing_studies, type) {
  private_validate_submission_choice(type, "type", c("original", "update"))

  if (identical(type, "original")) {
    return("0.0.1")
  }

  if (!all(c("studyName", "version") %in% names(existing_studies))) {
    cli::cli_abort(
      "{.arg existing_studies} must include {.val studyName} and {.val version} columns to calculate the next Study version."
    )
  }

  study_rows <- private_submission_chr_vec(existing_studies$studyName) == study_name
  prior_versions <- private_submission_chr_vec(existing_studies$version[study_rows])
  prior_versions <- prior_versions[!is.na(prior_versions)]

  if (length(prior_versions) == 0) {
    return("0.0.1")
  }

  latest_version <- prior_versions[[which.max(vapply(
    prior_versions,
    private_study_version_rank,
    numeric(1)
  ))]]

  private_study_bump_version(latest_version)
}

private_study_bump_version <- function(version) {
  version <- private_required_submission_chr(version, "version")
  parts <- strsplit(version, "\\.")[[1]]

  if (length(parts) != 3 || any(is.na(suppressWarnings(as.integer(parts))))) {
    cli::cli_abort(
      "Study version {.val {version}} must use the format {.val 0.0.1}."
    )
  }

  nums <- as.integer(parts)
  nums[[3]] <- nums[[3]] + 1L
  paste(nums, collapse = ".")
}

private_study_version_rank <- function(version) {
  parts <- strsplit(version, "\\.")[[1]]
  parts_num <- suppressWarnings(as.numeric(parts))

  if (length(parts_num) != 3 || any(is.na(parts_num))) {
    return(-Inf)
  }

  parts_num[[1]] * 1e6 + parts_num[[2]] * 1e3 + parts_num[[3]]
}

private_resolve_submission_studies <- function(studies = NULL, ss) {
  if (!is.null(studies)) {
    return(private_as_study_submission_df(studies))
  }

  if (!curl::has_internet()) {
    cli::cli_abort(
      "No internet connection available. Supply {.arg studies} to work with Study updates offline."
    )
  }

  private_read_submission_database_sheet(ss = ss, sheet = "Study")
}

private_resolve_submission_study_datasets <- function(study_datasets = NULL, ss) {
  if (!is.null(study_datasets)) {
    if (!is.data.frame(study_datasets)) {
      cli::cli_abort("{.arg study_datasets} must be a data.frame.")
    }
    return(study_datasets)
  }

  if (!curl::has_internet()) {
    cli::cli_abort(
      "No internet connection available. Supply {.arg study_datasets} to work with Study updates offline."
    )
  }

  private_read_submission_database_sheet(ss = ss, sheet = "StudyDataset")
}

private_collect_existing_study_id <- function(studyID = NULL, studies) {
  if (!is.null(studyID)) {
    return(private_required_submission_chr(studyID, "studyID"))
  }

  if (!all(c("studyID", "studyName", "version") %in% names(studies))) {
    cli::cli_abort(
      "{.arg studies} must include {.val studyID}, {.val studyName}, and {.val version} for interactive selection."
    )
  }

  choices <- paste0(
    studies$studyID,
    " - ",
    studies$studyName,
    " (v",
    studies$version,
    ")"
  )
  selected <- utils::menu(choices, title = "Choose the Study version to update")
  if (selected == 0) {
    cli::cli_abort("A Study selection is required.")
  }

  private_required_submission_chr(studies$studyID[[selected]], "studyID")
}

private_collect_study_dataset_ids <- function(study, datasetIDs, available_datasets) {
  if (!is.null(datasetIDs)) {
    return(unique(private_required_submission_chr_vec(datasetIDs, "datasetIDs")))
  }

  inferred_ids <- private_infer_study_dataset_ids(study, available_datasets)
  if (length(inferred_ids) > 0) {
    return(inferred_ids)
  }

  if (!interactive()) {
    cli::cli_abort(
      "Study dataset IDs could not be inferred automatically. Provide {.arg datasetIDs} in non-interactive mode."
    )
  }

  private_select_study_dataset_ids(available_datasets)
}

private_infer_study_dataset_ids <- function(study, available_datasets) {
  if (length(study$trios) == 0 || !"name" %in% names(available_datasets) || !"datasetID" %in% names(available_datasets)) {
    return(character(0))
  }

  trio_names <- unique(stats::na.omit(vapply(
    study$trios,
    function(trio) {
      private_submission_chr(trio$name)
    },
    character(1)
  )))

  if (length(trio_names) == 0) {
    return(character(0))
  }

  matches <- available_datasets$datasetID[available_datasets$name %in% trio_names]
  unique(stats::na.omit(as.character(matches)))
}

private_select_study_dataset_ids <- function(available_datasets) {
  if (!all(c("datasetID", "name") %in% names(available_datasets))) {
    cli::cli_abort(
      "{.arg available_datasets} must include {.val datasetID} and {.val name} columns for interactive selection."
    )
  }

  choices <- paste0(seq_len(nrow(available_datasets)), ": ",
    available_datasets$datasetID, " - ", available_datasets$name)
  cat(paste0(choices, collapse = "\n"), "\n")
  cli::cli_inform(c(
    "Choose datasets for this Study.",
    "i" = "Enter menu numbers or dataset IDs separated by commas, for example: 36,37,38 or D036,D037,D038"
  ))

  entered <- private_collect_submission_text(
    prompt = "Dataset selections",
    required = TRUE
  )
  selected_values <- unique(trimws(strsplit(entered, ",")[[1]]))
  selected_values <- selected_values[nzchar(selected_values)]

  selected_ids <- vapply(
    selected_values,
    function(value) {
      if (grepl("^[0-9]+$", value)) {
        idx <- as.integer(value)
        if (idx < 1 || idx > nrow(available_datasets)) {
          cli::cli_abort(c(
            "Invalid dataset selection number.",
            "i" = "Value {.val {value}} is outside the available range 1-{nrow(available_datasets)}."
          ))
        }
        return(as.character(available_datasets$datasetID[[idx]]))
      }

      value
    },
    character(1)
  )

  if (length(selected_ids) == 0) {
    cli::cli_abort("At least one datasetID must be selected.")
  }

  unknown_ids <- setdiff(selected_ids, as.character(available_datasets$datasetID))
  if (length(unknown_ids) > 0) {
    cli::cli_abort(c(
      "Unknown dataset IDs selected for Study submission.",
      "i" = "Unknown values: {.val {unknown_ids}}"
    ))
  }

  selected_ids
}

private_upload_study_submission_gist <- function(file, description, githubPat, public) {
  file <- private_required_submission_chr(file, "file")

  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.file {file}}")
  }

  content <- readLines(file, warn = FALSE)
  gist <- private_submission_create_gist(
    content = content,
    filename = basename(file),
    description = description,
    public = public,
    pat = githubPat
  )

  private_required_submission_chr(gist$html_url, "gist$html_url")
}

private_write_study_mapping_functions_file <- function(study) {
  if (!inherits(study, "BenchmarkStudy")) {
    cli::cli_abort("{.arg study} must be a {.cls BenchmarkStudy} object.")
  }

  if (length(study$mappingFunctions) == 0) {
    cli::cli_abort("No mapping functions are stored in the supplied Study object.")
  }

  mapping_functions_code <- character()

  for (func_name in names(study$mappingFunctions)) {
    func <- study$mappingFunctions[[func_name]]

    mapping_functions_code <- c(
      mapping_functions_code,
      paste0("# Function: ", func_name),
      paste0("# Input: ", func$doc$inputDescription),
      paste0("# Output: ", func$doc$outputDescription)
    )

    if (!is.null(func$doc$exampleUsage)) {
      example_lines <- strsplit(func$doc$exampleUsage, "\n", fixed = TRUE)[[1]]
      mapping_functions_code <- c(
        mapping_functions_code,
        paste0("# Example: ", example_lines[[1]])
      )
      if (length(example_lines) > 1) {
        mapping_functions_code <- c(
          mapping_functions_code,
          paste0("# ", example_lines[-1])
        )
      }
    }

    mapping_functions_code <- c(
      mapping_functions_code,
      "",
      paste0(
        func_name,
        " <- ",
        paste(deparse(func$func), collapse = "\n")
      ),
      "\n"
    )
  }

  out_file <- tempfile(pattern = "benchhub_mapping_functions_", fileext = ".R")
  writeLines(mapping_functions_code, out_file, useBytes = TRUE)
  out_file
}

private_inform_study_submission_review <- function(submission) {
  study_tbl <- submission$Study
  study_dataset_tbl <- submission$StudyDataset

  lines <- c(
    "Study submission review summary:",
    paste0("  Study name: ", study_tbl$studyName[[1]]),
    paste0("  Version: ", study_tbl$version[[1]]),
    paste0("  Type: ", study_tbl$type[[1]]),
    paste0("  Linked datasets: ", paste(study_dataset_tbl$datasetID, collapse = ", "))
  )

  if ("protocolGist" %in% names(study_tbl) && !is.na(study_tbl$protocolGist[[1]])) {
    lines <- c(lines, paste0("  Protocol gist: ", study_tbl$protocolGist[[1]]))
  }

  if ("mappingFunctions" %in% names(study_tbl) && !is.na(study_tbl$mappingFunctions[[1]])) {
    lines <- c(lines, paste0("  Mapping functions gist: ", study_tbl$mappingFunctions[[1]]))
  }

  cli::cli_inform(lines)
  invisible(NULL)
}
