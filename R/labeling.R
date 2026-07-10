strip_html_tags <- function(x) {
  gsub("<.*?>", "", x)
}

#' Parse a REDCap choices string into a named vector
#'
#' REDCap typically stores choices in the form:
#' `"1, Yes | 2, No | 9, Unknown"`.
#'
#' @param x A REDCap choice string.
#'
#' @return A named character vector where names are codes and values are labels,
#'   or `NULL` if no choices are present.
parse_redcap_choices <- function(x) {
  if (length(x) != 1 || is.na(x) || x == "") {
    return(NULL)
  }

  parsed <- dplyr::tibble(part = stringr::str_split(x, "\\s*\\|\\s*")[[1]]) |>
    tidyr::separate_wider_delim(
      part,
      delim = ",",
      names = c("code", "label"),
      too_many = "merge"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_trim))
  parsed

  # parts <- stringr::str_split(x, "\\|", simplify = FALSE)[[1]]
  # parts <- stringr::str_trim(parts)

  # parsed <- purrr::map(parts, function(part) {
  #   m <- stringr::str_match(part, "^\\s*([^,]+)\\s*,\\s*(.*)\\s*$")
  #   if (is.na(m[1, 1])) {
  #     return(NULL)
  #   }
  #   tibble::tibble(
  #     code = m[1, 2],
  #     label = m[1, 3]
  #   )
  # })

  # parsed <- purrr::compact(parsed)

  # if (length(parsed) == 0) {
  #   return(NULL)
  # }

  # parsed <- dplyr::bind_rows(parsed)
  # stats::setNames(parsed$code, parsed$label)
}

metadata_add_parsed_labels <- function(metadata) {
  supported_types <- c(
    "dropdown",
    "radio",
    "yesno",
    "truefalse"
  )
  metadata |>
    dplyr::mutate(
      raw_labels = dplyr::replace_when(
        select_choices_or_calculations,
        field_type == "yesno" ~ "0, No | 1, Yes",
        !(field_type %in% supported_types) ~ NA_character_
      ),
      labels = purrr::map(raw_labels, parse_redcap_choices),
      .after = select_choices_or_calculations
    )
}


get_value_labels_dictionary <- function(metadata) {
  metadata |>
    metadata_add_parsed_labels() |>
    dplyr::select(field_name, labels) |>
    tidyr::unnest(labels)
}

#' Get variable labels from REDCap metadata
#'
#' @param metadata REDCap metadata tibble.
#'
#' @return Named character vector of variable labels.
get_variable_labels <- function(metadata) {
  metadata |>
    dplyr::filter(!(field_type %in% c("descriptive", "checkbox"))) |>
    dplyr::select(field_name, field_label) |>
    labelled::dictionary_to_variable_labels()
}

#' Get value labels from REDCap metadata
#'
#' Excludes checkbox fields because checkbox labels are handled separately.
#'
#' @param metadata REDCap metadata tibble.
#'
#' @return Named list of value label vectors.
get_value_labels <- function(data, value_labels_dict) {
  value_labels_dict |>
    dplyr::filter(field_name %in% colnames(data)) |>
    labelled::dictionary_to_value_labels(
      names_from = field_name,
      values_from = code,
      labels_from = label,
      data = data
    )
}

#' Apply REDCap metadata labels to a dataset
#'
#' Applies:
#' - variable labels
#' - value labels for non-checkbox fields
#' - checkbox labels
#' - optional checkbox collapsing
#'
#' @param data REDCap records.
#' @param metadata REDCap metadata.
#' @param checkbox One of `"label"`, `"collapse"`, or `"both"`.
#' @param keep_checkbox_columns If `FALSE`, remove checkbox child columns after collapse.
#' @param checkbox_sep Separator when collapsing checkbox selections.
#'
#' @return A labelled data frame.
apply_redcap_form_labels <- function(
  data,
  metadata,
  checkbox = c("both", "label", "collapse"),
  keep_checkbox_columns = TRUE,
  checkbox_sep = "; "
) {
  checkbox <- rlang::arg_match(checkbox)

  var_labels <- get_variable_labels(metadata)
  value_labels_dict <- get_value_labels_dictionary(metadata)
  if (nrow(value_labels_dict) > 0) {
    value_labels <- get_value_labels(data, value_labels_dict)
  } else {
    value_labels <- NULL
  }

  data <- data |>
    labelled::set_variable_labels(!!!var_labels, .strict = FALSE) |>
    labelled::set_value_labels(!!!value_labels)

  if (checkbox %in% c("label", "both")) {
    data <- label_checkbox_columns(data, metadata)
  }

  if (checkbox %in% c("collapse", "both")) {
    data <- collapse_checkbox_fields(
      data = data,
      metadata = metadata,
      sep = checkbox_sep,
      keep_checkbox_columns = keep_checkbox_columns
    )
  }

  data
}
