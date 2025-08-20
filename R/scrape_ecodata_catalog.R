#' Scrape ecodata catalog pages
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
#'                  "Spatial scale", "Temporal scale", "Implications".
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

  # get level 2 headings and paragraphs, our data are in both
  fields <- page |>
    rvest::html_elements("h2, p") |>
    rvest::html_text2()

  # filter by the fieldlist

  # return result with named columns


}

