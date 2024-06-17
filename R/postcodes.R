#' @title Clean UK postcodes and extract their relevant elements
#'
#' @description This function cleans UK postcode data before returning the relevant section (such as its first part of full postcode).
#' @param postcode_value The postcode data to be cleaned (in the form of 'N1 1AA', 'ME1 2RE' or 'TN12 0QS').
#' @param postcode_type The relevant section of the postcode to return. Only one section is permitted. Options are:
#' \describe{
#'   \item{full}{the full postcode (such as 'N1 1AA')}
#'   \item{region}{the first part of the first element of the postcode (such as 'ME' for 'ME1 2RE')}
#'   \item{area}{the first element of the postcode (such as 'ME1' for 'ME1 2RE')}
#'   \item{locale}{the area and the first part of the second element of the postcode (such as 'ME1 2' for 'ME1 2RE')}
#'   }
#' @keywords Postcodes
#' @export
#' @examples
#' postcodes(
#'   postcode_value = 'ME1 2re ',
#'   postcode_type = 'region'
#'   )
#'
#' @importFrom rlang .data

postcodes <- function(postcode_value, postcode_type = "full"){

  # Error checks ----

  if (!(rlang::is_scalar_character(postcode_value))){
    rlang::abort("postcode must be a single character value.")
  }

  rlang::arg_match(
    arg = postcode_type,
    values = c("full", "region", "area", "locale"),
    multiple = FALSE)


  # Check for oddities ----

  postcode_tidier <- postcode_value |>
    stringr::str_trim() |>
    stringr::str_to_upper()

  postcode_raw1 <- ifelse(
    postcode_tidier %in% c("N/A", "NA", "N-A"),
    NA_character_,
    postcode_tidier
    )

  postcode_raw2 <- ifelse(
    stringr::str_detect(postcode_raw1, pattern = "^[:alpha:]{1,2}[:digit:]{1,2}[:alpha:]?\\s[:digit:]"),
    postcode_raw1,
    NA_character_
    )

  postcode_raw <- ifelse(
    stringr::str_detect(postcode_raw2, pattern = "\\s[:digit:][:alpha:]{2}$"),
    postcode_raw2,
    NA_character_
    )

  # Main functions ----

  postcode_locale <- stringr::str_extract(postcode_raw, "^[:alpha:]{1,2}[:digit:]{1,2}[:alpha:]?\\s[:digit:]")
  postcode_area <- stringr::str_extract(postcode_raw, "^[:alpha:]{1,2}[:digit:]{1,2}[:alpha:]?")
  postcode_region <- stringr::str_extract(postcode_area, "^[:alpha:]+")

  postcode_output <- switch (
    postcode_type,
    "full" = postcode_raw,
    "region" = postcode_region,
    "area" = postcode_area,
    "locale" = postcode_locale
    )

  return(postcode_output)

}
