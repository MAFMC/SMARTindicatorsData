#' Scrape smart indicator data pages to make a dataset
#'
#' Pulls selected fields, paragraphs, and images from the SMART indicator reports website
#' and returns a dataframe of the information for each indicator.
#' This dataset is the input for the searchable shiny app/dashboard
#'
#'@param url The url of the SMART indicator page to be scraped
#'@param FieldList Optional, A character vector of fields to be scraped; must match spelling.
#'The default FieldList is c()
#'
#'@return a dataframe with variables from the FieldList and values from the SMART indicator webpage. Columns:
#'\itemize{
#'  \item{\code{Indicator}, name of the indicator extracted from SMART indicator page title}
#'  \item{\code{Source}, URL of the SMART indicator page}
#'  \item{\code{Varname}, variable names corresponding to matches from the FieldList}
#'  \item{\code{Value}, text for each variable name from the SMART indicator page}
#' }
#'
#'@examples
#'  url <- "https://mafmc.github.io/SMARTindicatorsData/Cold-Pool-Index.html"
#'  scrape_smartinddata(url)
#'
#'@export
scrape_smartinddata <- function(url){

  # page to xml
  page <- rvest::read_html(url)

  # indicator chapter number and name is in second level 1 header
  titles <- page |>
    rvest::html_elements("h1") |>
    rvest::html_text2()

  name <- stringr::str_trim(stringr::str_extract(titles[1], "(?<=:)\ *(.*)*"))

  # level 2 headers are the variables mostly
  h2heads <- page |>
    rvest::html_elements("h2") |>
    rvest::html_text2()

  h2descriptive <- h2heads[1:4]

  h2smart <- h2heads[5:6]

  # descriptive fields start with 1,  h2 paragraphs 1-4
  h2fields <- page |>
    rvest::html_elements("h2, p") |>
    rvest::html_text2()

  # descriptive keep all in vector before "2.1 Indicator documentation"
  desckey <- h2smart[1]
  descind <- which(h2fields==desckey)
  descfields <- h2fields[2:(descind-1)]

  # SMART fields in section 2, level 3 headings and paragraphs
  # need the headers for variables 5-7
  h34heads <- page |>
    rvest::html_elements("h3, h4") |>
    rvest::html_text2()

  h34fields <- page |>
    rvest::html_elements("h3, h4, p") |>
    rvest::html_text2()

  # SMART keep all including and after the first h34heading
  smartkey <- h34heads[1]
  smartind <- which(h34fields==smartkey)
  lastsmart <- length(h34fields)
  smartfields <- h34fields[smartind:lastsmart]


  ################ revise below

  # filter by the fieldlist
  # need to take either the FIRST set of data sources, extraction, analysis or all
  # get the number portion of Current method
  numCurrentmethod <- gsub("[^0-9.-]+", "", Currentmethod)
  h3fieldsCurr <- h3fields[stringr::str_detect(h3fields, numCurrentmethod)]
  h3fieldsOld <- h3fields[!(h3fields %in% h3fieldsCurr)]

  # also there is inconsistent capitalization
  allmatching_fields <- fields[stringr::str_detect(fields, stringr::regex(paste(FieldList, collapse = "|"), ignore_case = TRUE))]

  # everything but the old methods goes forward
  matching_fields <- allmatching_fields[!(allmatching_fields %in% h3fieldsOld)]

  # filter matching fields that aren't headers
  # if text matching h3fields current is found in a paragraph, dont include it in matching fields
  matching_fields <- grep(stringr::regex(paste(c(h3fieldsCurr,FieldList), collapse = "|")), matching_fields, value = TRUE)

  # capture text sections following an h3 field
  fieldind <- which(stringr::str_detect(fields, paste(matching_fields, collapse = "|")))
  headind <- which(stringr::str_detect(fields, paste(h3fieldsCurr, collapse = "|")))
  nheadpar <-c(diff(headind)-1, 1)

  headoffset <- data.frame(headind, nheadpar)

  h3fieldind <- dplyr::filter(headoffset, headind %in% fieldind) |>
    dplyr::rowwise() |>
    dplyr::mutate(h3fieldtext = paste(fields[(headind+1):(headind+nheadpar)], collapse = " "))

  h3fieldvec <- rep(NA, length(matching_fields))

  # indices to map h3 names to h3fieldtext
  h3fieldvec[which(stringr::str_detect(matching_fields, "^[0-9]+"))] <- as.vector(h3fieldind$h3fieldtext)

  # split into variable names and values
  # three patterns: variable name : value
  # and section number variable name with value from fieldtext
  # and variable name with value as https string

  # catalog link is not consistent sometimes it is catalog page
  catpattern <-  paste0("(?<=", "link", "|", "page", ")\ *(.*)")

  df <- as.data.frame(matching_fields) |>
    dplyr::mutate(Varname = dplyr::case_when(stringr::str_detect(matching_fields, ": ") ~ stringr::str_extract(matching_fields, "[^:]+"),
                                             stringr::str_detect(matching_fields, "^[0-9]+") ~ stringr::str_extract(matching_fields,"[^0-9.0-9 ].*"),
                                             stringr::str_detect(matching_fields, "catalog") ~ "catalog link"
                                             )
                  ) |>
    dplyr::mutate(Value = dplyr::case_when(stringr::str_detect(matching_fields, ": ") ~ stringr::str_trim(stringr::str_extract(matching_fields, "(?<=:)\ *(.*)*")),
                                           stringr::str_detect(matching_fields, "^[0-9]+") ~ h3fieldvec,
                                           stringr::str_detect(matching_fields, "catalog") ~ stringr::str_trim(stringr::str_extract(matching_fields, catpattern))
                                           )
    )

  # add whether methods have changed
  multmethods <- data.frame(matching_fields=NA, Varname = "MethodsChange", Value = Methodchange)
  df <- dplyr::bind_rows(df, multmethods)

  #df <- df[-1]

  # return result with named columns
  result <- df |>
    dplyr::mutate(Indicator = name,
                  Source = url) |>
    dplyr::select(Indicator, Source, Varname, Value) |>
    dplyr::filter(!is.na(Varname)) |>
    tibble::as_tibble()

  return(result)

}

