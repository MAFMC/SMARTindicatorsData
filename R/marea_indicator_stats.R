#' Summarize and visualize marea indicators
#'
#' Retrieve attributes of a marea indicator and return as a dataframe, and plot the indicator.
#'
#'@param dataset The marea indicator dataset name in ecodata::dataset format (may contain one or multiple indicator variables)
#'
#'@return a dataframe with variables from the StatList and values calculated from the indicator(s). Columns:
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

  # count number of value columns, needed?
  #length(dat@meta$original_value_col)

  # for each variable get name, units, region
  # start and end year or time
  summ <- data.frame(Indicator = dat@meta$data_type,
                     Dataset = dataset,
                     Var = dat@meta$original_value_col,
                     Source = dat@meta$source_citation,
                     Units = dat@meta$units,
                     Space = dat@meta$location_descriptor,
                     EPU = dat@meta$region,
                     StartYear = min(dat@data$year, na.rm = TRUE),
                     EndYear = max(dat@data$year, na.rm = TRUE))

  # are year and month fields present?
  summ$Time <- if(any(stringr::str_detect(names(dat@data), "year")) &
             any(stringr::str_detect(names(dat@data), "month"))){
    "Annual and Monthly"
  }else if(any(stringr::str_detect(names(dat@data), "year"))){
    "Annual"
  }else{
    "No time dimension"
  }

  # make the dataset long in case there is more than on variable
  datlong <- dat@data |>
    tidyr::pivot_longer(cols = dplyr::ends_with("_value"),
                        names_to = "Var", values_to = "Value") |>
    dplyr::mutate(Var = stringr::str_remove(Var, "_value"))

  # mean and range of time series with min and max value years
  TSstats <- datlong |>
    dplyr::group_by(Var) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::mutate(Value = as.numeric(Value)) |>
    dplyr::select(year, Var, Value) |>
    dplyr::summarise(TSMean = mean(Value, na.rm = T),
                     TSMin = min(Value, na.rm = T),
                     TSMax = max(Value, na.rm = T),
                     YrTSMin = year[which(Value == TSMin)][1],  #take first if >1
                     YrTSMax = year[which(Value == TSMax)][1]) #take first if >1

  # collate all this and return a dataframe
  sumstats <- merge(summ, TSstats) |>
    dplyr::select(Indicator, Dataset, Var, Source, Units, Time, Space, EPU, dplyr::everything()) |>
    tidyr::pivot_longer(-c(Indicator:EPU), names_to = "Varname", values_to = "Value")

  sumstats[sumstats == ""] <- NA

  return(sumstats)
}

