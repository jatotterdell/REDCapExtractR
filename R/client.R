#' Create a REDCap API client
#'
#' @param url REDCap API URL.
#' @param token REDCap API token.
#'
#' @return An object of class `redcap_client`.
#' @export
redcap_client <- function(url, token) {
  rlang::check_installed("httr2")

  if (!is.character(url) || length(url) != 1 || is.na(url) || url == "") {
    rlang::abort("`url` must be a non-empty character scalar.")
  }

  if (!is.character(token) || length(token) != 1 || is.na(token) || token == "") {
    rlang::abort("`token` must be a non-empty character scalar.")
  }

  structure(
    list(
      url = url,
      token = token
    ),
    class = "redcap_client"
  )
}


#' Print a REDCap API client
#'
#' @param x A REDCap API client.
#' @param ... Other arguments
#' @export
print.redcap_client <- function(x, ...) {
  cat("<redcap_client>\n")
  cat("  url:   ", x$url, "\n", sep = "")
  cat("  token: ", paste0(substr(x$token, 1, 4), strrep("*", max(nchar(x$token) - 4, 0))), "\n", sep = "")
  invisible(x)
}
