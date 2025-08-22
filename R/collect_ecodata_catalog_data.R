#' Collect catalog data and create dataset
#'
#' Apply the `scrape_ecodata_catalog` function to all or a subset of catalog pages and save the dataset
#'
#'@param urllist Optional; the urls of the catalog page to be scraped, defaults to all pages
#'@param outfile Optional; name of output datafile without extension (.rds format), defaults to "catalogdat"
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
#'  urllist <- c("https://noaa-edab.github.io/catalog/trans_dates.html",
#'  "https://noaa-edab.github.io/catalog/SAV.html",
#'  "https://noaa-edab.github.io/catalog/zooplankton_index.html")
#'  outfile <- "testdat"
#'  collect_ecodata_catalog_data(urllist, outfile)
#'
#'@export
collect_ecodata_catalog_data <- function(urllist = NULL, outfile = NULL){

  if(!is.null(urllist)){
    catlist <- urllist
  }else{
    #create a list of all catalog webpages
    caturl <- "https://noaa-edab.github.io/catalog/"

    page <- rvest::read_html(caturl)

    # Extract all href attributes from <a> tags
    links <- page |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")

    # Remove NA values and empty strings
    links <- links[!is.na(links) & links != ""]

    # Convert relative URLs to absolute URLs
    absolute_links <- xml2::url_absolute(links, caturl)

    # Remove links that are not catalog pages
    catlist <- absolute_links[stringr::str_detect(absolute_links, caturl)]

    # Remove heading, glossary, references, and index pages
    catlist <- catlist[!stringr::str_detect(catlist, "#|glossary.html|references.html|index.html")]

  }

  # scrape all pages in the list
  catdat <- purrr::map_dfr(catlist, scrape_ecodata_catalog)


  # save the dataset
  if(!is.null(outfile)){
    saveRDS(catdat, here::here(paste0("data-raw/", outfile ,".rds")))
  }else{
    saveRDS(catdat, here::here("data-raw/catalogdat.rds"))
  }

}
