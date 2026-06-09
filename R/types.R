#' Map REDCap metadata to a readr collector
#'
#' @param field_type REDCap field type.
#' @param validation REDCap validation type.
#'
#' @return A readr collector.
#' @noRd
redcap_collector <- function(field_type, validation = NA_character_) {
  validation <- validation %||% NA_character_

  # Boolean-ish REDCap fields
  if (field_type %in% c("yesno", "truefalse", "checkbox")) {
    return(readr::col_integer())
  }

  # Calculated or slider values are usually numeric
  if (field_type %in% c("calc", "slider")) {
    return(readr::col_double())
  }

  # Text fields depend heavily on validation
  if (field_type == "text") {
    if (validation %in% c("integer")) {
      return(readr::col_integer())
    }

    if (validation %in% c("number", "number_1dp", "number_2dp", "number_3dp", "number_4dp")) {
      return(readr::col_double())
    }

    if (validation %in% c("date_ymd")) {
      return(readr::col_date(format = "%Y-%m-%d"))
    }

    if (validation %in% c("date_dmy")) {
      return(readr::col_date(format = "%Y-%m-%d"))
    }

    if (validation %in% c("date_mdy")) {
      return(readr::col_date(format = "%m-%d-%Y"))
    }

    if (validation %in% c("datetime_ymd")) {
      return(readr::col_datetime(format = "%Y-%m-%d %H:%M"))
    }

    if (validation %in% c("datetime_dmy")) {
      return(readr::col_datetime(format = "%Y-%m-%d %H:%M"))
    }

    if (validation %in% c("datetime_mdy")) {
      return(readr::col_datetime(format = "%m-%d-%Y %H:%M"))
    }

    if (validation %in% c("datetime_seconds_ymd")) {
      return(readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"))
    }

    if (validation %in% c("datetime_seconds_dmy")) {
      return(readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"))
    }

    if (validation %in% c("datetime_seconds_mdy")) {
      return(readr::col_datetime(format = "%m-%d-%Y %H:%M:%S"))
    }
  }

  # Default: keep as character
  readr::col_character()
}


#' Build a readr column specification from REDCap metadata
#'
#' @param field_metadata REDCap form metadata tibble
#' @param form_field_names The REDCap form fields
#'
#' @return A readr cols specification.
#' @export
build_redcap_form_col_types <- function(field_metadata, form_field_names) {
  field_metadata <- field_metadata |>
    filter(export_field_name %in% form_field_names)
  spec_list <- list()
  for (i in seq_len(nrow(field_metadata))) {
    field_name <- field_metadata$export_field_name[i]
    field_type <- field_metadata$field_type[i]
    validation <- field_metadata$text_validation_type_or_show_slider_number[i]
    spec_list[[field_name]] <- redcap_collector(
      field_type = field_type,
      validation = validation
    )
  }

  # REDCap system columns often absent from metadata
  if ("redcap_event_name" %in% form_field_names) {
    spec_list[["redcap_event_name"]] <- readr::col_character()
  }

  if ("redcap_repeat_instrument" %in% form_field_names) {
    spec_list[["redcap_repeat_instrument"]] <- readr::col_character()
  }

  if ("redcap_repeat_instance" %in% form_field_names) {
    spec_list[["redcap_repeat_instance"]] <- readr::col_integer()
  }

  timestamp_cols <- grep("timestamp", form_field_names, value = TRUE)
  if (length(timestamp_cols) > 0) {
    for (x in timestamp_cols) {
      spec_list[[x]] <- readr::col_datetime(format = "%Y-%m-%d %H:%M:%S")
    }
  }

  # Keep unspecified columns as character
  do.call(
    readr::cols,
    c(
      list(.default = readr::col_character()),
      spec_list
    )
  )
}


#' Convert REDCap export columns using metadata-driven types
#'
#' @param data A data frame, ideally with all columns as character.
#' @param field_metadata REDCap field name metadata tibble.
#' @param na Character vector of strings to treat as missing.
#'
#' @return A typed tibble.
#' @export
convert_redcap_form_types <- function(data, field_metadata, na = c("", "NA", "UNK")) {
  col_types <- build_redcap_form_col_types(
    field_metadata = field_metadata,
    form_field_names = colnames(data)
  )
  readr::type_convert(
    data,
    col_types = col_types,
    na = na
  )
}
