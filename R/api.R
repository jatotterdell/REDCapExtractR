#' Perform a REDCap API call
#'
#' @param client A `redcap_client`.
#' @param content REDCap API content type.
#' @param ... Additional REDCap API form fields.
#'
#' @return Parsed JSON response.
#' @noRd
redcap_api_call <- function(client, content, ...) {
  if (!inherits(client, "redcap_client")) {
    rlang::abort("`client` must be a `redcap_client`.")
  }

  body <- c(
    list(
      token = client$token,
      content = content,
      format = "csv",
      returnFormat = "csv"
    ),
    list(...)
  )

  req <- httr2::request(client$url) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(!!!body) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3)

  resp <- httr2::req_perform(req)

  txt <- httr2::resp_body_string(resp)

  if (identical(txt, "") || is.na(txt)) {
    rlang::abort("REDCap API returned an empty response.")
  }

  # Default to col_character as readr may infer wrong type if there is a lack of data values
  # Will use metadata to better infer types as a processing step
  readr::read_csv(
    I(txt),

    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
}

#' Export the project info
#'
#' @description
#' This method allows you to export some of the basic attributes of a given REDCap project,
#' such as the project's title, if it is longitudinal, if surveys are enabled,
#' the time the project was created and moved to production, etc.
#'
#' @param client A `redcap_client`.
#'
#' @return A tibble
export_project_info <- function(client) {
  redcap_api_call(client, "project")
}

#' Export the events for a project
#'
#' @description
#' This method allows you to export the events for a project
#'
#' @param client A `redcap_client`.
#'
#' @return A tibble
export_events <- function(client) {
  redcap_api_call(client, "event")
}


export_repeating_events <- function(client) {
  redcap_api_call(client, "repeatingFormsEvents")
}

#' The export/import-specific version of field names for all fields
#'
#' @description
#' This method returns a list of the export/import-specific version of field names for all fields
#' (or for one field, if desired) in a project. This is mostly used for checkbox fields because during
#' data exports and data imports, checkbox fields have a different variable name used than the exact one
#' defined for them in the Online Designer and Data Dictionary, in which *each checkbox option* gets represented
#' as its own export field name in the following format: field_name + triple underscore + converted coded value
#' for the choice. For non-checkbox fields, the export field name will be exactly the same as the original field name.
#' Note: The following field types will be automatically removed from the list returned by this method since they
#' cannot be utilized during the data import process: 'calc', 'file', and 'descriptive'.
#'
#' The list that is returned will contain the three following attributes for each field/choice: 'original_field_name',
#' 'choice_value', and 'export_field_name'. The choice_value attribute represents the raw coded value for a
#' checkbox choice. For non-checkbox fields, the choice_value attribute will always be blank/empty.
#' The export_field_name attribute represents the export/import-specific version of that field name.
#'
#' @inheritParams export_events
#'
#' @return A tibble
export_field_names <- function(client) {
  redcap_api_call(client, "exportFieldNames")
}


#' Export the data collection instruments for a project
#'
#' @description
#' This method allows you to export a list of the data collection instruments for a project.
#' This includes their unique instrument name as seen in the second column of the Data Dictionary,
#' as well as each instrument's corresponding instrument label, which is seen on a project's left-hand
#' menu when entering data. The instruments will be ordered according to their order in the project.
#'
#' @inheritParams export_events
#'
#' @return A tibble
export_instruments <- function(client) {
  redcap_api_call(client, "instrument")
}


#' Export the instrument-event mappings for a project
#'
#' @description
#' This method allows you to export the instrument-event mappings for a project
#' (i.e., how the data collection instruments are designated for certain events in a longitudinal project).
#'
#' @inheritParams export_events
#'
#' @return A tibble
export_instrument_event_mappings <- function(client) {
  redcap_api_call(client, "formEventMapping")
}


#' Export the metadata for a project
#'
#' @description
#' This method allows you to export the metadata for a project
#'
#' @inheritParams export_events
#'
#' @return A tibble of REDCap metadata
export_metadata <- function(client) {
  redcap_api_call(client, "metadata")
}


#' Helper function to split metadata into form-specific tibbles
#'
#' @param metadata REDCap metadata tibble
#' @return A list of tibbles by form
split_metadata_by_form <- function(metadata) {
  metadata <- tibble::as_tibble(metadata)
  split(metadata, metadata$form_name)
}


#' Export the metadata for a project
#'
#' @description
#' This method allows you to export the metadata for a project
#'
#' @inheritParams export_events
#' @param forms A comma separated string giving the forms to export
#' @param fields A comma separated string giving the fields to export
#' @param events A comma separated string giving the events to export
#' @param raw_or_label Return raw or labelled field values
#'
#' @return A tibble
export_records <- function(
  client,
  forms,
  fields,
  events,
  raw_or_label = c("raw", "label")
) {
  redcap_api_call(
    client,
    content = "record",
    type = "flat",
    rawOrLabel = raw_or_label,
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "true",
    exportSurveyFields = "true",
    forms = forms,
    fields = fields,
    events = events
  )
}


#' Export all REDCap forms along with metadata
#'
#' The aim of this function is to export all of the relevant REDCap data.
#' In particular, we want:
#' - the raw REDCap records
#' - the data dictionary and other metadata necessary to determine field types and labels.
#'
#' @param client REDCap client
#' @param raw_or_label Export 'raw' or 'label' data from REDCap.
#'
#' @return A list of REDCap data.
#' @export
export_redcap <- function(client, raw_or_label = "raw") {
  project <- export_project_info(client)
  field_names <- export_field_names(client)
  metadata <- export_metadata(client)
  forms <- export_instruments(client)
  events <- export_events(client)
  form_by_event <- export_instrument_event_mappings(client)
  form_names <- forms$instrument_name

  # Expand field names metadata for use in type derivation
  # Makes checkbox handling more direct
  field_names <- field_names |>
    dplyr::left_join(
      dplyr::select(
        metadata,
        field_name,
        form_name,
        field_type,
        text_validation_type_or_show_slider_number
      ),
      dplyr::join_by(original_field_name == field_name)
    ) |>
    tidyr::fill(form_name) |>
    mutate(
      field_type = replace_when(field_type, is.na(field_type) & grepl("_complete", export_field_name) ~ "dropdown")
    )

  # Add metadata for the "form complete" fields
  metadata_complete <- field_names |>
    filter(grepl("_complete", original_field_name)) |>
    select(original_field_name, form_name, field_type) |>
    rename(field_name = original_field_name) |>
    mutate(
      field_label = "Form complete?",
      select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete"
    )
  metadata <- bind_rows(metadata, metadata_complete)

  # 'record ID' field is defined in REDCap to be the first variable in the project codebook
  # we always include this field as a reference key
  record_id_name <- metadata$field_name[1]

  # Which events is the form collected at
  form_events <- with(form_by_event, split(unique_event_name, form))[form_names]

  # Which fields are on the form
  form_fields <- with(metadata, split(field_name, form_name))[form_names]

  form_list <- vector("list", length(form_events))
  names(form_list) <- form_names
  for (i in form_names) {
    cli::cli_progress_step("Extracting {i} form", "Extracted {i}")
    form_list[[i]] <- export_records(
      client,
      forms = i,
      fields = paste(c(record_id_name, form_fields[[i]]), collapse = ", "),
      events = paste(form_events[[i]], collapse = ", "),
      raw_or_label = raw_or_label
    )
    cli::cli_progress_done()
  }
  cli::cli_alert_success("All REDCap forms extracted.")

  list(
    project = project,
    metadata = metadata,
    field_names = field_names,
    events = events,
    forms = forms,
    form_event_mapping = form_by_event,
    data = form_list
  )
}

apply_redcap_types_and_labels <- function(redcap) {
  form_metadata <- split_metadata_by_form(redcap$metadata)
  form_names <- names(redcap$data) |>
    purrr::set_names()
  redcap$data <- form_names |>
    purrr::map(~ convert_redcap_form_types(redcap$data[[.x]], redcap$field_names))
  redcap$data <- form_names |>
    purrr::map(~ apply_redcap_form_labels(redcap$data_typed[[.x]], form_metadata[[.x]]))
  redcap
}
