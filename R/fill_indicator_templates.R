#' Create and fill .Rmd templates for multiple indicators
#'
#' Apply the `create_template` function to all or a subset of ecosystem indicators
#'
#'@param indlist Optional; the names of indicators to create templates for, defaults to all in the catalog dataset created by `collect_ecodata_catalog_data`
#'@param overwrite Logical. Defaults to FALSE. If TRUE, output will overwrite any existing template or google doc for chosen species.
#'
#'@return .Rmd templates for each indicator in the indlist with the naming convention `indicator_name.Rmd`.
#'
#'
#'@examples
#'  indlist <- c("Transition Dates","Submerged Aquatic Vegetation","Zooplankton Indices")
#'  fill_indicator_templates(indlist, overwrite = TRUE)
#'
#'@export
fill_indicator_templates <- function(indlist = NULL, overwrite = FALSE){

  if(!is.null(indlist)){
    templist <- indlist
  }else{
    #create a list of all indicators from the stored catalog dataset
    catalogdat <- readRDS(here::here("data-raw/catalogdat.rds"))

    ecodatadat <- readRDS(here::here("data-raw/ecodatadat.rds"))

    ecodatasets <- unique(ecodatadat$Dataset)

    # check if indicators have ecodata datasets, only use those that do for now
    # also some ecodata sets were filtered out for nonconforming, so remove these too
    # otherwise template will fail
    templist <- catalogdat |>
      dplyr::filter(Varname == "ecodata name") |>
      dplyr::select(Indicator, Dataset = Value) |>
      dplyr::distinct() |>
      dplyr::filter(!Dataset %in% c("No dataset"),
                    Dataset %in% ecodatasets) |>
      dplyr::select(Indicator)

    templist <- unlist(unname(as.vector(templist)))
  }

  # make filenames with no spaces for the Rmds?

  # create templates and save to outdir with filenames
  if(overwrite) purrr::map(templist, ~create_template_SOE(., overwrite = TRUE))
  if(!overwrite) purrr::map(templist, create_template)

}
