#' Scrape ecodata catalog page
#'
#' Pulls selected fields, paragraphs, and images from the NOAA-EDAB Indicator Catalog website.
#' Code had to be tailored to this website because not all fields come under standard headers.
#'
#'@param url The url of the catalpg page to be scraped
#'@param FieldList
#'
#'@return a dataframe with columns from the FieldList
#'
#'@examples
#'  url <- "https://noaa-edab.github.io/catalog/trans_dates.html"
#'  FieldList <- c("Description", "Introduction", "Key Results",
#'                  "Spatial scale", "Temporal scale", "Implications",
#'                  "ecodata name", "tech-doc link")
#'  scrape_ecodata_catalog(url, FieldList)
#'
#'@export
scrape_ecodata_catalog <- function(url, FieldList){

  # page to xml
  page <- rvest::read_html(url)

  titles <- page |>
    rvest::html_elements("h1") |>
    rvest::html_text2()

  chnum <- as.numeric(gsub(".*?([0-9]+).*", "\\1", titles[2]))
  name <- stringr::str_trim(gsub("[0-9]", "", titles[2]))

  h2fields <- page |>
    rvest::html_elements("h2") |>
    rvest::html_text2()

  # get level 2 headings and paragraphs, our data are in both
  fields <- page |>
    rvest::html_elements("h2, p") |>
    rvest::html_text2()

  # filter by the fieldlist
  matching_fields <- fields[stringr::str_detect(fields, paste(FieldList, collapse = "|"))]

  # capture text sections following an h2 field
  fieldind <- which(stringr::str_detect(fields, paste(FieldList, collapse = "|")))
  headind <- which(stringr::str_detect(fields, paste(h2fields, collapse = "|")))
  h2fieldind <- intersect(fieldind, headind)
  fieldtext <- fields[h2fieldind+1]

  # split into column names and values
  # two patterns: column name : value
  # and section number column name with value from fieldtext


  # return result with named columns


}

