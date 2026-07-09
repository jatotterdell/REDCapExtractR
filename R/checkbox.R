#' Build a checkbox dictionary from REDCap metadata
#'
#' @param metadata REDCap metadata tibble.
#'
#' @return A tibble mapping checkbox parent fields to child columns and labels.
#' @export
get_checkbox_dictionary <- function(metadata) {
  checkbox_meta <- metadata |>
    dplyr::filter(field_type == "checkbox") |>
    dplyr::select(
      field_name,
      field_label,
      select_choices_or_calculations
    )

  if (nrow(checkbox_meta) == 0) {
    return(dplyr::tibble(
      field_name = character(),
      field_label = character(),
      choice_code = character(),
      choice_label = character(),
      checkbox_column = character()
    ))
  }

  out <- purrr::pmap_dfr(
    checkbox_meta,
    function(field_name, field_label, select_choices_or_calculations) {
      choices <- parse_redcap_choices(select_choices_or_calculations)

      if (is.null(choices)) {
        return(dplyr::tibble(
          field_name = character(),
          field_label = character(),
          choice_code = character(),
          choice_label = character(),
          checkbox_column = character()
        ))
      }

      dplyr::tibble(
        field_name = field_name,
        field_label = field_label,
        choice_code = choices$code,
        choice_label = choices$label,
        checkbox_column = paste0(field_name, "___", redcap_checkbox_suffix(choices$code))
      )
    }
  )

  out
}

#' Normalise a checkbox code to a likely REDCap checkbox suffix
#'
#' REDCap checkbox exports append a suffix to the field name. In many projects,
#' non-alphanumeric characters are replaced with underscores.
#'
#' @param x Choice code.
#'
#' @return Character vector of normalised suffixes.
#' @export
redcap_checkbox_suffix <- function(x) {
  x |>
    stringr::str_replace_all("[^A-Za-z0-9]", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

#' Coerce checkbox columns to integer 0/1 where possible
#'
#' @param data A data frame.
#' @param checkbox_dict Checkbox dictionary from `get_checkbox_dictionary()`.
#'
#' @return Modified data frame.
#' @export
coerce_checkbox_columns <- function(data, checkbox_dict) {
  cols <- intersect(checkbox_dict$checkbox_column, names(data))

  for (nm in cols) {
    vals <- data[[nm]]

    if (is.character(vals)) {
      vals <- dplyr::case_when(
        vals %in% c("0", "1") ~ as.integer(vals),
        vals == "" ~ NA_integer_,
        TRUE ~ suppressWarnings(as.integer(vals))
      )
    }

    data[[nm]] <- vals
  }

  data
}

#' Apply labels to checkbox child columns
#'
#' Each checkbox column will receive:
#' - a variable label of the form `"Field label: Choice label"`
#' - value labels `0 = "No"` and `1 = "Yes"`
#'
#' @param data A data frame.
#' @param metadata REDCap metadata.
#'
#' @return Modified labelled data frame.
#' @export
label_checkbox_columns <- function(data, metadata) {
  checkbox_dict <- get_checkbox_dictionary(metadata)

  if (nrow(checkbox_dict) == 0) {
    return(data)
  }

  data <- coerce_checkbox_columns(data, checkbox_dict)

  present <- checkbox_dict |>
    dplyr::filter(checkbox_column %in% names(data))

  if (nrow(present) == 0) {
    return(data)
  }

  for (i in seq_len(nrow(present))) {
    nm <- present$checkbox_column[i]
    vlab <- paste0(present$field_label[i], ": ", present$choice_label[i])

    labelled::var_label(data[[nm]]) <- vlab
    labelled::val_labels(data[[nm]]) <- c(No = 0, Yes = 1)
  }

  data
}

#' Collapse checkbox child columns into a single human-readable field
#'
#' For each checkbox field, selected options are collapsed into a single string.
#'
#' @param data A data frame.
#' @param metadata REDCap metadata.
#' @param sep Separator used when multiple choices are selected.
#' @param keep_checkbox_columns If `TRUE`, keep the original checkbox child columns.
#'
#' @return Modified data frame.
#' @export
collapse_checkbox_fields <- function(data, metadata, sep = "; ", keep_checkbox_columns = TRUE) {
  checkbox_dict <- get_checkbox_dictionary(metadata)

  if (nrow(checkbox_dict) == 0) {
    return(data)
  }

  data <- coerce_checkbox_columns(data, checkbox_dict)

  by_field <- split(checkbox_dict, checkbox_dict$field_name)

  for (field in names(by_field)) {
    dict <- by_field[[field]]
    cols <- intersect(dict$checkbox_column, names(data))

    if (length(cols) == 0) {
      next
    }

    dict_present <- dict[match(cols, dict$checkbox_column), , drop = FALSE]
    choice_labels <- dict_present$choice_label

    selected_text <- apply(
      X = as.data.frame(data[, cols, drop = FALSE]),
      MARGIN = 1,
      FUN = function(row_vals) {
        is_selected <- !is.na(row_vals) & row_vals == 1
        picked <- choice_labels[is_selected]

        if (length(picked) == 0) {
          return(NA_character_)
        }

        paste(picked, collapse = sep)
      }
    )

    data[[field]] <- selected_text
    labelled::var_label(data[[field]]) <- dict$field_label[1]

    if (!keep_checkbox_columns) {
      data[cols] <- NULL
    }
  }

  data
}
