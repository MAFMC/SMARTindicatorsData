#' Scrape ecodata catalog page
#'
#' Pulls selected fields, paragraphs, and images from the NOAA-EDAB Indicator Catalog website
#' and returns a dataframe of the .
#' Code had to be tailored to this website because not all fields come under standard headers.
#'
#'@param url The url of the catalog page to be scraped
#'@param FieldList Optional, A character vector of fields to be scraped; must match case and spelling.
#'The default FieldList is c("Description", "Introduction", "Key Results",
#'                  "Spatial scale", "Temporal scale", "Implications",
#'                  "Point of contact", "ecodata name", "tech-doc link")
#'
#'@return a dataframe with variables from the FieldList and values from the catalog webpage. Columns:
#'\itemize{
#'  \item{\code{Indicator}, name of the indicator extracted from catalog page title}
#'  \item{\code{Source}, URL of the catalog page}
#'  \item{\code{Varname}, variable names corresponding to matches from the FieldList}
#'  \item{\code{Value}, text for each variable name from the catalog page}
#' }
#'
#'@examples
#'  url <- "https://noaa-edab.github.io/tech-doc/cold_pool.html"
#'  FieldList <- c("Description", "Data steward", "Data sources", "Data extraction",
#'  "Data analysis", "catalog link", "References")
#'  scrape_ecodata_catalog(url, FieldList)
#'
#'@export
scrape_ecodata_techdoc <- function(url, FieldList=NULL){

  # default FieldList for iterating with purrr:map
  if(is.null(FieldList)){
  FieldList <- c("Description", "Data steward", "Data sources", "Data extraction",
                 "Data analysis","catalog link", "References")
  }

  # page to xml
  page <- rvest::read_html(url)

  # indicator chapter number and name is in second level 1 header
  titles <- page |>
    rvest::html_elements("h1") |>
    rvest::html_text2()

  chnum <- as.numeric(gsub(".*?([0-9]+).*", "\\1", titles[2]))
  name <- stringr::str_trim(gsub("[0-9]", "", titles[2]))

  # need level 2 headers separately to see if there are multiple Methods sections
  h2fields <- page |>
    rvest::html_elements("h2") |>
    rvest::html_text2()

  # and I only want the numbered ones
  myh2fields <- h2fields[stringr::str_detect(h2fields, "^[0-9]+")]

  # if there is more than 1 methods section (numbered level 2 header) then we capture current method
  # but also record that methods have changed over time

  # get level 3 headings and paragraphs, our data are in both
  fields <- page |>
    rvest::html_elements("h3, p") |>
    rvest::html_text2()


  # filter by the fieldlist
  # need to take either the FIRST set of data sources, extraction, analysis or all
  matching_fields <- fields[stringr::str_detect(fields, paste(FieldList, collapse = "|"))]

  # capture text sections following an h3 field
  fieldind <- which(stringr::str_detect(fields, paste(FieldList, collapse = "|")))
  headind <- which(stringr::str_detect(fields, paste(myh2fields, collapse = "|")))
  nheadpar <-c(diff(headind)-1, 1)

  headoffset <- data.frame(headind, nheadpar)

  h2fieldind <- dplyr::filter(headoffset, headind %in% fieldind) |>
    dplyr::rowwise() |>
    dplyr::mutate(h2fieldtext = paste(fields[(headind+1):(headind+nheadpar)], collapse = " "))

  h2fieldvec <- rep(NA, length(matching_fields))

  # indices to map h2 names to h2fieldtext
  h2fieldvec[which(stringr::str_detect(matching_fields, "^[0-9]+"))] <- as.vector(h2fieldind$h2fieldtext)

  # split into variable names and values
  # three patterns: variable name : value
  # and section number variable name with value from fieldtext
  # and variable name with value as https string
  df <- as.data.frame(matching_fields) |>
    dplyr::mutate(Varname = dplyr::case_when(stringr::str_detect(matching_fields, ": ") ~ stringr::str_extract(matching_fields, "[^:]+"),
                                             stringr::str_detect(matching_fields, "^[0-9]+") ~ stringr::str_extract(matching_fields,"[^0-9.0-9 ].*"),
                                             stringr::str_detect(matching_fields, "tech-doc") ~ "tech-doc link"
                                             )
                  ) |>
    dplyr::mutate(Value = dplyr::case_when(stringr::str_detect(matching_fields, ": ") ~ stringr::str_trim(stringr::str_extract(matching_fields, "(?<=:)\ *(.*)*")),
                                           stringr::str_detect(matching_fields, "^[0-9]+") ~ h2fieldvec,
                                           stringr::str_detect(matching_fields, "tech-doc") ~ stringr::str_trim(stringr::str_extract(matching_fields, "(?<=link)\ *(.*)"))
                                           )
    )


  df <- df[-1]

  # return result with named columns
  result <- df |>
    dplyr::mutate(Indicator = name,
                  Source = url) |>
    dplyr::select(Indicator, Source, Varname, Value)

  return(result)

}

