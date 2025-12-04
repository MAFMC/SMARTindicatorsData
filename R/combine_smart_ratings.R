#' Combine individual indicator SMART ratings from rmd rendering and create dataset
#'
#' SMART ratings include those for individual attributes as well as summary ratings by attribute and across all SMART elements
#'
#'@param csvlist Optional; list of smart rating csvs created by rendering indicator rmd files, defaults to all csvs in `data-raw/indratings`
#'@param outfile Optional; name of output datafile without extension (.rds format), defaults to "smartratingdat"
#'
#'@return saves an R dataset (.rds) with smart elements, attributes, and rankings. Columns:
#'\itemize{
#'  \item{\code{Category}, Character string; indicator category from the SMART page}
#'  \item{\code{Indicator}, Character string; name of the indicator}
#'  \item{\code{Element}, Character string; SMART element: Specific, Measurable, Achievable, Relevant, Time-bound, or All}
#'  \item{\code{Attribute}, Character string; attributes of each SMART element, or denoting element summary ("ElementRating")
#'  or summary across all elements ("OverallRating")}
#'  \item{\code{Rating}, Numeric; rating evaluating how well the indicator meets attribute criteria; range from 0 does not meet to 1 fully meets.
#'  Ratings for each element "ElementRating" are an average across all attributes within an element. "OverallRating" is an average across
#'  all "ElementRating".}
#' }
#'
#'@examples
#'  csvlist <- c(here::here("data-raw/indratings/Gulf Stream Index_ratingsumm.csv"),
#'  here::here("data-raw/indratings/Forage Fish Index_ratingsumm.csv"),
#'  here::here("data-raw/indratings/Harbor Porpoise Bycatch_ratingsumm.csv"))
#'  outfile <- "test_smartratingdat"
#'  combine_smart_ratings(csvlist, outfile)
#'
#'@export
combine_smart_ratings <- function(csvlist = NULL, outfile = NULL){

  if(!is.null(csvlist)){
    ratlist <- csvlist
  }else{
    #create a list of all csvs in the default directory
    ratlist <- list.files(here::here("data-raw/indratings"), full.names = TRUE)
  }

  readsmartcsv <- function(csvfile){
    onesmart <- read.csv(csvfile)

    allatts <- onesmart |>
      dplyr::select(-c(X, ElementRating:OverallRating))

    elrates <- onesmart |>
      dplyr::select(Category, Indicator, Element, ElementRating) |>
      dplyr::distinct() |>
      tidyr::pivot_longer(ElementRating, names_to = "Attribute", values_to = "Rating")

    overall <- onesmart |>
      dplyr::select(Category, Indicator, OverallRating) |>
      dplyr::distinct() |>
      dplyr::mutate(Element = "All",
                    Attribute = "OverallRating") |>
      dplyr::select(Category, Indicator, Element, Attribute, Rating = OverallRating)

    onelongsmart <- dplyr::bind_rows(allatts, elrates, overall)

    return(onelongsmart)
  }

  # combine all data in the list
  ratdat <- purrr::map_dfr(ratlist, readsmartcsv)


  # save the dataset
  if(!is.null(outfile)){
    saveRDS(ratdat, here::here(paste0("data-raw/", outfile ,".rds")))
  }else{
    saveRDS(ratdat, here::here("data-raw/smartratingdat.rds"))
  }

}
