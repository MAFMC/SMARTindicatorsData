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
#'  StatList <- c("Units", "StartYear", "EndYear", "TSMean", "TS95CI")
#'  ecodata_indicator_stats(dataset, StatList)
#'
#'@export
ecodata_indicator_stats <- function(dataset, StatList = NULL){

  # default StatList for iterating with purrr:map
  if(is.null(StatList)){
    StatList <- c("Units", "StartYear", "EndYear", "TSMean", "TS95CI")
  }

  # load the dataset
  dat <- dataset

  # for each variable get name and units
  summ <- data.frame(Indicator = unique(dat$Var),
                     Dataset = deparse(substitute(dataset)))

  Units <- dat |>
    dplyr::group_by(Var) |>
    dplyr::select(Units) |>
    dplyr::distinct()

  # for each variable get start and end year or time
  StartEnd <- dat |>
    dplyr::group_by(Var) |>
    dplyr::select(Time) |>
    dplyr::summarise(StartYear = min(Time),
                     EndYear = max(Time),
                     EstYrs = EndYear-StartYear+1,
                     ActualYrs = dplyr::n_distinct(Time))

  # mean and 95% CI of time series
  TSstats <- dat |>
    dplyr::group_by(Var) |>



  # test for trends or time blocks?

  # collate all this and return a dataframe

  return(summ)
}

