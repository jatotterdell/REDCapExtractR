redcap_variable_labels <- function(metadata) {
  metadata |>
    dplyr::filter(!(field_type %in% c("descriptive", "checkbox"))) |>
    dplyr::select(field_name, field_label) |>
    labelled::dictionary_to_variable_labels()
}

redcap_form_value_labels <- function(metadata, formdata) {
  form_fields <- colnames(formdata)
  field_types <- c("radio", "dropdown", "checkbox", "yesno")
  metadata |>
    dplyr::filter(
      field_name %in% form_fields,
      field_type %in% field_types
    ) |>
    dplyr::mutate(
      # Hack to make delimiter more unique
      # Sometimes ", " is used in the value labels
      # So replace "1, A", by "1:|: A" under the assumption that ":|:" won't appear anywhere else
      select_choices_or_calculations = dplyr::replace_when(
        select_choices_or_calculations,
        field_type == "yesno" ~ "1, Yes | 0, No",
        select_choices_or_calculations == "1," ~ "1, NA",
      ),
      # select_choices_or_calculations = gsub("(?<=\\w),", ":|:", select_choices_or_calculations, perl = TRUE),
      select_choices_or_calculations = gsub("(^|\\|)\\s*[^,|]*\\K,", ":|:", select_choices_or_calculations, perl = TRUE)
    ) |>
    dplyr::select(field_name, select_choices_or_calculations, field_label) |>
    labelled::dictionary_to_value_labels(delim_entries = " | ", delim_value_label = ":|: ", data = formdata)
}


redcap_value_labels <- function(metadata, data) {}


set_redcap_labels <- function(dat, var_labels, val_labels) {
  dat |>
    labelled::set_variable_labels(!!!(var_labels)) |>
    labelled::set_value_labels(!!!(val_labels))
}
