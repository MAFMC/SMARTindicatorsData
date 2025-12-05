#' Summarize and visualize marea indicators
#'
#' Retrieve attributes of a marea indicator and return as a dataframe, and plot the indicator.
#'
#'@param dataset The marea indicator dataset name in ecodata::dataset format (may contain one or multiple indicator variables)
#'
#'@return a dataframe with variables from the StatList and values calculated from the indicator(s). Columns:
#'\itemize{
#'  \item{\code{Indicator}, name of the variable summarized from the marea indicator dataset}
#'  \item{\code{Dataset}, the marea indicator dataset name = "marea name" collected from catalog}
#'  \item{\code{Units}, the units of the indicator}
#'  \item{\code{EPU}, the Ecosystem Production Unit or Region where the indicator is measured}
#'  \item{\code{Varname}, basic indicator statistics; StartYear is the first year of the time series,
#'  EndYear is the last year of the time series, EstYrs is the total possible years between the start
#'  and end years, ActualYrs is the count of years to determine if any are missing, TSMean is the time
#'  series mean value, TSmin is the time series minimum value, TSmax is the time series maximum value,
#'  YrTSmin is the year of the time series minimum value, YrTSmax is the year of the time series maximum value}
#'  \item{\code{Value}, value of the statistic for the indicator}
#' }
#'
#'
#'
#'@examples
#'  marea_indicator_stats(dataset = "amo")
#'
#'@export
marea_indicator_stats <- function(dataset){

  # load the dataset
  data(list = dataset, package = "marea", envir = environment())

  dat <- get(dataset)

  # datasets have dat@meta = metadata and dat@data = data

  # count number of value columns
  length(dat@meta$original_value_col)

  # for each variable get name and units
  summ <- data.frame(Indicator = dat@meta$data_type,
                     Dataset = dataset,
                     IndVar = dat@meta$original_value_col)
  # units
  Units <- dat@meta$units

  # spatial
  EPUs <- dat@meta$region

  # for each variable get start and end year or time

  StartYear  <-  min(dat@data$year, na.rm = TRUE)

  EndYear <- max(dat@data$year, na.rm = TRUE)

  # are year and month fields present?
  datcols <- names(dat@data)

  # make the dataset long in case there is more than on variable


  # mean and range of time series with min and max value years
  TSstats <- dat |>
    dplyr::group_by(Var, EPU) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::mutate(Value = as.numeric(Value)) |>
    dplyr::select(Indicator = Var, EPU, Time, Value) |>
    dplyr::summarise(TSMean = mean(Value, na.rm = T),
                     TSMin = min(Value, na.rm = T),
                     TSMax = max(Value, na.rm = T),
                     YrTSMin = Time[which(Value == TSMin)][1],  #take first if >1
                     YrTSMax = Time[which(Value == TSMax)][1]) #take first if >1

  # collate all this and return a dataframe
  columns <- dplyr::left_join(summ, Units) |>
    dplyr::left_join(EPUs)

  stats <- merge(StartEnd, TSstats) |>
    tidyr::pivot_longer(-c(Indicator, EPU), names_to = "Varname", values_to = "Value")

  sumstats <- merge(columns, stats) |>
    dplyr::select(Indicator, Dataset, Units, EPU, Varname, Value) |>
    dplyr::mutate(Indicator = as.character(Indicator))

  return(sumstats)
}

