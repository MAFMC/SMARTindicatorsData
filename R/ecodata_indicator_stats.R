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
#'  dataset <- "forage_index"
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

  # for each variable get name and units

  # for each variable get start and end year or time

  # are all years in the time series?

  # mean and 95% CI of time series

  # test for trends or time blocks?

  # collate all this and return a dataframe


}
