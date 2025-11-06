#' Collect ecodata techdoc data and create dataset
#'
#' Apply the `scrape_ecodata_techdoc` function to all or a subset of tech-doc pages and save the dataset
#'
#'@param urllist Optional; the urls of the tech-doc page to be scraped, defaults to all pages
#'@param outfile Optional; name of output datafile without extension (.rds format), defaults to "techdocdat"
#'
#'@return saves an R dataset (.rds) with variables from the FieldList and values from the tech-doc webpage. Columns:
#'\itemize{
#'  \item{\code{Indicator}, name of the indicator extracted from tech-doc page title}
#'  \item{\code{Source}, URL of the tech-doc page}
#'  \item{\code{Varname}, variable names corresponding to matches from the FieldList}
#'  \item{\code{Value}, text for each variable name from the tech-doc page}
#' }
#'
#'@examples
#'  urllist <- c("https://noaa-edab.github.io/tech-doc/trans_dates.html",
#'  "https://noaa-edab.github.io/tech-doc/SAV.html",
#'  "https://noaa-edab.github.io/tech-doc/zooplankton_index.html")
#'  outfile <- "testdat"
#'  collect_ecodata_techdoc_data(urllist, outfile)
#'
#'@export
collect_ecodata_techdoc_data <- function(urllist = NULL, outfile = NULL){

  if(!is.null(urllist)){
    catlist <- urllist
  }else{
    #create a list of all tech-doc webpages
    caturl <- "https://noaa-edab.github.io/tech-doc/"

    page <- rvest::read_html(caturl)

    # Extract all href attributes from <a> tags
    links <- page |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")

    # Remove NA values and empty strings
    links <- links[!is.na(links) & links != ""]

    # Convert relative URLs to absolute URLs
    absolute_links <- xml2::url_absolute(links, caturl)

    # Remove links that are not tech-doc pages
    catlist <- absolute_links[stringr::str_detect(absolute_links, caturl)]

    # Remove heading, glossary, references, and index pages (temporarily remove ecosystem overfishing as its headers differ)
    catlist <- catlist[!stringr::str_detect(catlist, "#|erddap.html|glossary.html|references.html|/index.html|ppr.html")]

    # Ensure unique pages
    catlist <- unique(catlist)

    # remove the root if its in there
    catlist <- catlist[!(catlist %in% "https://noaa-edab.github.io/tech-doc/")]

  }

  # scrape all pages in the list
  catdat <- purrr::map_dfr(catlist, scrape_ecodata_techdoc)


  # save the dataset
  if(!is.null(outfile)){
    saveRDS(catdat, here::here(paste0("data-raw/", outfile ,".rds")))
  }else{
    saveRDS(catdat, here::here("data-raw/techdocdat.rds"))
  }

}
