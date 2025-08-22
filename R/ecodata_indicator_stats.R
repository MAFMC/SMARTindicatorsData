#' Summarize and visualize ecodata indicators
#'
#' Retrieve attributes of an ecodata indicator and return as a dataframe, and plot the indicator.
#'
#'@param dataset The ecodata indicator dataset name (may contain one or multiple indicator variables)
#'@param StatList Optional, A character vector of attributes and statistics to be calculated.
#'The default StatList is c("Units", "StartYear", "EndYear", "TSMean", "TS95CI")
#'
#'@return a dataframe with variables from the StatList and values calculated from the indicator(s). Columns:
#'\itemize{
#'  \item{\code{Indicator}, name of the variable summarized from the ecodata indicator dataset}
#'  \item{\code{Dataset}, the ecodata indicator dataset name = "ecodata name" collected from catalog}
#'  \item{\code{Varname}, variable names corresponding the StatList}
#'  \item{\code{Value}, value of the extracted or calculated variable}
#' }
#'
#'@examples
#'  dataset <- ecodata::forage_index
#'  StatList <- c("Units", "EPUs", "StartYear", "EndYear", "TSMean", "TSMin", "TSMax", "YrMin", "YrMax")
#'  ecodata_indicator_stats(dataset, StatList)
#'
#'@export
ecodata_indicator_stats <- function(dataset, StatList = NULL){

  # default StatList for iterating with purrr:map
  if(is.null(StatList)){
    StatList <- c("Units", "EPUs", "StartYear", "EndYear", "TSMean", "TSMin", "TSMax", "YrMin", "YrMax")
  }

  # load the dataset
  dat <- dataset

  # check names and force to a standard
  names(dat) <- stringr::str_to_title(names(dat))

  # for each variable get name and units
  summ <- data.frame(Indicator = unique(dat$Var),
                     Dataset = deparse(substitute(dataset)))

#  if("Units" %in% StatList){

  if(!"Units" %in% names(dat)) dat$Units <- "no Units field"

  Units <- dat |>
    dplyr::group_by(Var) |>
    dplyr::select(Var, Units) |>
    dplyr::distinct() |>
    dplyr::ungroup()
#  }

  # spatial

#  if("EPUs" %in% StatList){

  if(!"EPU" %in% names(dat)) dat$EPU <- "no EPU field"

  EPUs <- dat |>
    dplyr::group_by(Var) |>
    dplyr::select(Var, EPU) |>
    dplyr::distinct()

#  }

  # for each variable get start and end year or time
  if(!"Time" %in% names(dat)) dat$Time <- "no Time field"

  StartEnd <- dat |>
    dplyr::group_by(Var) |>
    dplyr::select(Var, Time) |>
    dplyr::summarise(StartYear = min(Time, na.rm = T),
                     EndYear = max(Time, na.rm = T),
                     EstYrs = EndYear-StartYear+1,
                     ActualYrs = dplyr::n_distinct(Time))

  # mean and range of time series with min and max value years
  TSstats <- dat |>
    dplyr::group_by(Var, EPU) |>
    dplyr::select(Var, Time, Value) |>
    dplyr::summarise(TSMean = mean(Value, na.rm = T),
                     TSMin = min(Value, na.rm = T),
                     TSMax = max(Value, na.rm = T),
                     YrMin = Time[which(Value == TSMin)],
                     YrMax = Time[which(Value == TSMax)])

  # collate all this and return a dataframe
  sumstats <- dplyr::left_join(summ, Units, by=c("Indicator"="Var")) |>
    dplyr::left_join(EPUs, by=c("Indicator"="Var"))

  return(sumstats)
}

