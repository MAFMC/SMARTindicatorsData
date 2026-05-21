#' Create and fill .Rmd templates for multiple indicators
#'
#' Apply the `create_template_marea` function to all or a subset of marea ecosystem indicators
#'
#'@param indlist Optional; the names of indicators to create templates for, defaults to all in the marea dataset created by `collect_marea_indicator_stats`
#'@param overwrite Logical. Defaults to FALSE. If TRUE, output will overwrite any existing template or google doc for chosen species.
#'
#'@return .Rmd templates for each indicator in the indlist with the naming convention `indicator_name.Rmd`.
#'
#'
#'@examples
#'  indlist <- c("Atlantic MultiDecadal Oscillation Index", "Arctic Oscillation Index", "North Atlantic Oscillation Index")
#'  fill_marea_templates(indlist, overwrite = TRUE)
#'
#'@export
fill_marea_templates <- function(indlist = NULL, overwrite = FALSE){

  if(!is.null(indlist)){
    templist <- indlist
  }else{
    #create a list of all indicators from the stored marea dataset
    mareadat <- readRDS(here::here("data-raw/mareadat.rds"))

    templist <- mareadat |>
      dplyr::select(Indicator) |>
      dplyr::distinct()

    templist <- unlist(unname(as.vector(templist)))
  }

  # make filenames with no spaces for the Rmds?

  # create templates and save to outdir with filenames
  if(overwrite) purrr::map(templist, ~create_template_marea(., overwrite = TRUE))
  if(!overwrite) purrr::map(templist, create_template_marea)

}
