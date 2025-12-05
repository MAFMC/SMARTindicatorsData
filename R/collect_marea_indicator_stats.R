#' Collect indicator stats and create dataset
#'
#' Apply `marea_indicator_stats` to all or a subset of marea package datasets and save collated dataset
#'
#'@param datalist Optional, vector of marea indicator dataset names to be summarized in
#'ecodata::dataset format. Default is all datasets in the package
#'#'@param outfile Optional; name of output datafile without extension (.rds format), defaults to "mareadat"
#'
#'@return a dataframe with basic stats calculated from the indicator(s). Columns:
#'\itemize{
#'  \item{\code{Indicator}, Indicator name}
#'  \item{\code{Dataset}, the marea indicator dataset name}
#'  \item{\code{Var}, name of the indicator variable summarized from the marea indicator dataset}
#'  \item{\code{Source}, the source of the indicator}
#'  \item{\code{Units}, the units of the indicator}
#'  \item{\code{Time}, periodicity of the indicator: annual or annual and monthly}
#'  \item{\code{Space}, spatial description}
#'  \item{\code{EPU}, the Ecosystem Production Unit or Region where the indicator is measured}
#'  \item{\code{Varname}, basic indicator statistics; StartYear is the first year of the time series,
#'  EndYear is the last year of the time series, EstYrs is the total possible years between the start
#'  and end years, ActualYrs is the count of years to determine if any are missing, TSMean is the time
#'  series mean value, TSmin is the time series minimum value, TSmax is the time series maximum value,
#'  YrTSmin is the year of the time series minimum value, YrTSmax is the year of the time series maximum value}
#'  \item{\code{Value}, value of the statistic for the indicator}
#' }
#'
#'@examples
#'  datalist <- c("amo",
#'  "ao",
#'  "nao")
#'  outfile <- "testmarea"
#'  collect_marea_indicator_stats(datalist, outfile)
#'
#'@export
collect_marea_indicator_stats <- function(datalist = NULL, outfile = NULL){

  if(!is.null(datalist)){
    ecolist <- datalist
  }else{
    #create a list of all marea datasets

    d <- data(package = "marea", envir = environment())

    ecolist <- d$results[, "Item"]

    ecolist <- gsub(" .*$", "", ecolist)#remove anything after a space in the dataset name

    #remove spatial and Scotian shelf specific datasets
    ecolist <- (ecolist[!stringr::str_detect(ecolist, "coastline|glorys_|satellite_|azmp_|eco_|grey_")])

  }

  # calculate stats for all datasets in the list
  ecodat <- purrr::map_dfr(ecolist, marea_indicator_stats)

  # save the dataset
  if(!is.null(outfile)){
    saveRDS(ecodat, here::here(paste0("data-raw/", outfile ,".rds")))
  }else{
    saveRDS(ecodat, here::here("data-raw/mareadat.rds"))
  }


}
