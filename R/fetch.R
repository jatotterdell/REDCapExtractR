#' Save a REDCap bundle to disk
#'
#' Saves:
#' - labelled data as RDS
#' - metadata as RDS and JSON
#' - checkbox dictionary as CSV
#'
#' @param data Labelled data frame.
#' @param metadata REDCap metadata.
#' @param path Output directory.
#'
#' @return Invisibly returns `path`.
#' @export
save_redcap_bundle <- function(data, metadata, path) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || path == "") {
    rlang::abort("`path` must be a non-empty character scalar.")
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  saveRDS(data, file.path(path, "data_labelled.rds"))
  saveRDS(metadata, file.path(path, "metadata.rds"))
  jsonlite::write_json(
    metadata,
    path = file.path(path, "metadata.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  checkbox_dict <- get_checkbox_dictionary(metadata)
  readr::write_csv(checkbox_dict, file.path(path, "checkbox_dictionary.csv"))

  invisible(path)
}


#' Fetch, label, and optionally save REDCap data
#'
#' @param client A `redcap_client`.
#' @param records Optional record IDs.
#' @param forms Optional form names.
#' @param events Optional event names.
#' @param checkbox One of `"both"`, `"label"`, or `"collapse"`.
#' @param keep_checkbox_columns If `FALSE`, checkbox child columns are removed after collapsing.
#' @param checkbox_sep Separator used when collapsing checkbox selections.
#' @param path Optional output directory for saving the bundle.
#'
#' @return A list with elements `data`, `metadata`, and `checkbox_dictionary`.
#' @export
fetch_redcap_bundle <- function(
  client,
  forms = NULL,
  events = NULL,
  checkbox = c("both", "label", "collapse"),
  keep_checkbox_columns = TRUE,
  checkbox_sep = "; ",
  path = NULL
) {
  checkbox <- rlang::arg_match(checkbox)

  metadata <- export_metadata(client)
  data <- export_records(
    client = client,
    forms = forms,
    events = events,
    raw_or_label = "raw"
  )

  labelled_data <- apply_redcap_labels(
    data = data,
    metadata = metadata,
    checkbox = checkbox,
    keep_checkbox_columns = keep_checkbox_columns,
    checkbox_sep = checkbox_sep
  )

  bundle <- list(
    data = labelled_data,
    metadata = metadata,
    checkbox_dictionary = get_checkbox_dictionary(metadata)
  )

  class(bundle) <- "redcap_bundle"

  if (!is.null(path)) {
    save_redcap_bundle(
      data = bundle$data,
      metadata = bundle$metadata,
      path = path
    )
  }

  bundle
}

#' @export
print.redcap_bundle <- function(x, ...) {
  cat("<redcap_bundle>\n")
  cat("  rows: ", nrow(x$data), "\n", sep = "")
  cat("  cols: ", ncol(x$data), "\n", sep = "")
  cat("  metadata fields: ", nrow(x$metadata), "\n", sep = "")
  cat("  checkbox mappings: ", nrow(x$checkbox_dictionary), "\n", sep = "")
  invisible(x)
}
