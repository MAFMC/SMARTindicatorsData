#' Summarize and visualize ecodata indicators
#'
#' Retrieve attributes of an ecodata indicator and return as a dataframe, and plot the indicator.
#'
#'@param dataset The ecodata indicator dataset name in ecodata::dataset format (may contain one or multiple indicator variables)
#'
#'@return a dataframe with variables from the StatList and values calculated from the indicator(s). Columns:
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
#'  ecodata_indicator_stats(dataset = ecodata::forage_index)
#'
#'@export
ecodata_indicator_stats <- function(dataset){

  # These will be hardcoded for now
  # # default StatList for iterating with purrr:map
  # if(is.null(StatList)){
  #   StatList <- c("Units", "EPUs", "StartYear", "EndYear", "TSMean", "TSMin", "TSMax", "YrMin", "YrMax")
  # }

  # load the dataset
  dat <- eval(parse(text = dataset))

  if(!"Var" %in% names(dat)) dat$Var <- "no Var field"

  # for each variable get name and units
  summ <- data.frame(Indicator = unique(dat$Var),
                     Dataset = dataset)

  if(!"Units" %in% names(dat)) dat$Units <- "no Units field"

  Units <- dat |>
    dplyr::group_by(Var) |>
    dplyr::select(Indicator = Var, Units) |>
    dplyr::filter(!is.na(Units)) |>
    dplyr::distinct() |>
    dplyr::ungroup()


  # spatial
  if(!"EPU" %in% names(dat)) dat$EPU <- "no EPU field"

  EPUs <- dat |>
    dplyr::group_by(Var) |>
    dplyr::select(Indicator = Var, EPU) |>
    dplyr::distinct()

  # for each variable get start and end year or time
  # if no Time column stop here
  if(!"Time" %in% names(dat)){
    dat$Time <- "no Time field"
    notime <- dat |>
      dplyr::group_by(Var) |>
      dplyr::select(Indicator = Var, Time) |>
      dplyr::distinct()

    columns <- dplyr::left_join(summ, Units) |>
      dplyr::left_join(EPUs) |>
      dplyr::left_join(notime) |>
      dplyr::mutate(Varname = "no Time Variables",
                    Value = NA) |>
      dplyr::select(Indicator, Dataset, Units, EPU, Varname, Value) |>
      dplyr::mutate(Indicator = as.character(Indicator))

    return(columns)
  }

  # if time is not numeric--chl_pp et al--pull out the year
  if(is.character(dat$Time)){
    dat$Time <- as.numeric(gsub("[^0-9.-]", "", dat$Time))
  }

  StartEnd <- dat |>
    dplyr::group_by(Var, EPU) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::select(Indicator = Var, EPU, Time) |>
    dplyr::summarise(StartYear = min(Time, na.rm = T),
                     EndYear = max(Time, na.rm = T),
                     EstYrs = EndYear-StartYear+1,
                     ActualYrs = dplyr::n_distinct(Time))

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

