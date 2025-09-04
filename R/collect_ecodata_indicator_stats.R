#' Collect indicator stats and create dataset
#'
#' Apply `ecodata_indicator_stats` to all or a subset of ecodata package datasets and save collated dataset
#'
#'@param datalist Optional, vector of ecodata indicator dataset names to be summarized in
#'ecodata::dataset format. Default is all datasets in the package
#'#'@param outfile Optional; name of output datafile without extension (.rds format), defaults to "ecodatadat"
#'
#'@return a dataframe with basic stats calculated from the indicator(s). Columns:
#'\itemize{
#'  \item{\code{Indicator}, name of the variable summarized from the ecodata indicator dataset}
#'  \item{\code{Dataset}, the ecodata indicator dataset name = "ecodata name" collected from catalog}
#'  \item{\code{Units}, the units of the indicator}
#'  \item{\code{EPU}, the Ecosystem Production Unit where the indicator is measured}
#'  \item{\code{Varname}, basic indicator statistics; StartYear is the first year of the time series,
#'  EndYear is the last year of the time series, EstYrs is the total possible years between the start
#'  and end years, ActualYrs is the count of years to determine if any are missing, TSMean is the time
#'  series mean value, TSmin is the time series minimum value, TSmax is the time series maximum value,
#'  YrTSmin is the year of the time series minimum value, YrTSmax is the year of the time series maximum value}
#'  \item{\code{Value}, value of the statistic for the indicator}
#' }
#'
#'@examples
#'  datalist <- c("ecodata::trans_dates",
#'  "ecodata::SAV",
#'  "ecodata::zooplankton_index")
#'  outfile <- "testecodat"
#'  collect_ecodata_indicator_stats(datalist, outfile)
#'
#'@export
collect_ecodata_indicator_stats <- function(datalist = NULL, outfile = NULL){

  if(!is.null(datalist)){
    ecolist <- datalist
  }else{
    #create a list of all ecodata datasets

    d <- data(package = "ecodata", envir = environment())

    #ecolist <- d$results[, "Item"]
    ecolist <- paste0("ecodata::",d$results[, "Item"])

    ecolist <- gsub(" .*$", "", ecolist)#remove anything after a space in the dataset name

    #temporarily remove nonconforming datasets, need to address later
    ecolist <- (ecolist[!stringr::str_detect(ecolist, "ESP_|abc.acl|habitat_vulnerability|cetacean|ch_bay|bay_s|coast|_sf|stock_status")])

  }

  # calculate stats for all datasets in the list
  ecodat <- purrr::map_dfr(ecolist, ecodata_indicator_stats)

  # save the dataset
  if(!is.null(outfile)){
    saveRDS(ecodat, here::here(paste0("data-raw/", outfile ,".rds")))
  }else{
    saveRDS(ecodat, here::here("data-raw/ecodatadat.rds"))
  }


}
