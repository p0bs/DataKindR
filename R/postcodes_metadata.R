#' @title Scrape geographic metadata about UK postcodes
#'
#' @description This function takes clean UK postcode data and returns key geographic metadata. Details include: LSOA, MSOA, Latitude, Longitude, Deprivation Index and Constituency.
#' @param postcode_value The postcode data to be cleaned (in the form of 'N1 1AA', 'ME1 2RE' or 'TN12 0QS'). To clean this data, you could pass it through the `postcodes` function in this package.
#' @keywords Postcodes
#' @export
#' @examples
#' \dontrun{
#' postcodes_metadata(
#'   postcode_value = 'ME1 2re ',
#'   )
#' }

postcodes_metadata <- function(postcode_value){

  # Error checks ----

  if (!(rlang::is_scalar_character(postcode_value))){
    rlang::abort("postcode must be a single character value.")
  }

  if (
    stringr::str_detect(
      string = postcode_value,
      pattern = "^[:alpha:]+[:digit:]+[:alpha:]?\\s[:digit:][:alpha:][:alpha:]",
      negate = TRUE
    )){
    rlang::abort("Inapplicable postcode format. Have you tried to clean it using the postcodes function in this package?")
  }

  # Main functions ----

  url_main <- "https://findthatpostcode.uk/postcodes/"
  postcode_html <- xml2::url_escape(postcode_value)

  req <- httr2::request(url_main) |>
    httr2::req_url_path_append(postcode_html) |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_user_agent("DataKindR (https://github.com/p0bs/DataKindR)") |>
    httr2::req_throttle(10 / 60, realm = "https://findthatpostcode.uk/")

  resp <- req |>
    httr2::req_perform()

  resp_json <- resp |>
    httr2::resp_body_json()

  output_postcodes_metadata <- tibble::tibble(
    pcd = purrr::map_chr(resp_json$data$attributes$pcd, as.character),
    rgn = purrr::map_chr(resp_json$data$attributes$rgn, as.character),
    rgn_name = purrr::map_chr(resp_json$data$attributes$rgn_name, as.character),
    pcon = purrr::map_chr(resp_json$data$attributes$pcon, as.character),
    pcon_name = purrr::map_chr(resp_json$data$attributes$pcon_name, as.character),
    lsoa21 = purrr::map_chr(resp_json$data$attributes$lsoa21, as.character),
    lsoa21_name = purrr::map_chr(resp_json$data$attributes$lsoa21_name, as.character),
    msoa21 = purrr::map_chr(resp_json$data$attributes$msoa21, as.character),
    msoa21_name = purrr::map_chr(resp_json$data$attributes$msoa21_name, as.character),
    ward = purrr::map_chr(resp_json$data$attributes$ward, as.character),
    ward_name = purrr::map_chr(resp_json$data$attributes$ward_name, as.character),
    oac11_code = purrr::map_chr(resp_json$data$attributes$oac11$code, as.character),
    oac11_group = purrr::map_chr(resp_json$data$attributes$oac11$group, as.character),
    oac11_subgroup = purrr::map_chr(resp_json$data$attributes$oac11$subgroup, as.character),
    oac11_supergroup = purrr::map_chr(resp_json$data$attributes$oac11$supergroup, as.character),
    imd = purrr::map_int(resp_json$data$attributes$imd, as.integer),
    lat = purrr::map_dbl(resp_json$data$attributes$location$lat, as.numeric),
    lon = purrr::map_dbl(resp_json$data$attributes$location$lon, as.numeric)
    )

  return(output_postcodes_metadata)

  }

