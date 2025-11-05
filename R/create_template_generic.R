#'#' Create a SMART indicator bookdown template for an indicator of interest.
#'
#' Inspired by NOAA-EDAB Ecosystem Context for Stock Assessment workflow
#' original code: https://github.com/NOAA-EDAB/ECSA/blob/master/R/create_template.R
#'
#' @param indicator_name Character string. An indicator name (e.g. "Phytoplankton")
#' @param indicator_varname Character string. The variable name for indicators with multiple variables (e.g. "ANNUAL_PPD_MEDIAN"), defaults to all indicator variables, which can be a lot
#' @param template Character string. The path to the desired template, defaults to "templates/SMART_template.rmd"
#' @param output_dir Character string. The output directory for the compiled bookdown HTML document and supporting files. Default "docs" folder in current working directory
#' @param send_to_google_doc Logical. Defaults to TRUE. If TRUE, the generated template will render a google document into the chosen output directory for hand editing.
#' If FALSE, the template .Rmd file will be generated in the directory output_dir/ folder
#' @param overwrite Logical. Defaults to FALSE. If TRUE, output will overwrite any existing template or google doc for chosen species.
#'
#' @return A .Rmd file and/or google document populated with figures
#'
#' @examples
#'
#' create_template(indicator_name = "Phytoplankton", indicator_varname = "ANNUAL_PPD_MEDIAN",
#' overwrite = F, output_dir = getwd(), send_to_google_doc = T)
#'
#' @export


create_template_generic <- function(indicator_name,
                            indicator_varname = NULL,
                            template = here::here("templates/SMART_template_generic.rmd"),
                            output_dir = here::here("docs"),
                            send_to_google_doc = TRUE,
                            overwrite = FALSE) {

  # check for stuff that needs escaping in indicator_name and escape it so detection works
  esc_indicator_name <- stringr::str_escape(indicator_name)

  if(!is.null(indicator_varname)){ #no matching needed if doing them all
    esc_indicator_varname <- stringr::str_escape(indicator_varname)
  }


  #Create .Rmd file to be written to book
  #dat <- readLines(here::here("templates","SMART_template.rmd"))
  dat <- readLines(template)
  dat <- gsub("\\{\\{INDICATOR_NAME\\}\\}", esc_indicator_name, dat)
  if(!is.null(indicator_varname)){
    dat <- gsub("\\{\\{INDICATOR_VAR\\}\\}", esc_indicator_varname, dat)
  }else{
    dat <- gsub("\\{\\{INDICATOR_VAR\\}\\}", "ALL", dat)
  }

  # cat(dat,sep = "\n" )
  safename <- gsub("[() ]","-", esc_indicator_name)
  file_name <- sprintf("%s.rmd", safename)
  folder_name <- sprintf("%s",output_dir)
  #output_dir <- sprintf("%s_book", clean_names$stock_name)

  # create the output directory if missing
  if(!dir.exists(folder_name)) {
    dir.create(folder_name,recursive = TRUE)
  }

  #Check to make sure existing file is not over-written
  if(file.exists(sprintf("%s/%s",folder_name, file_name)) &  !overwrite){
    stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'", file_name))
  } else {

    # writes generic template after species specific substitutions to .rmd
    file_connection <- file(sprintf("%s/%s", folder_name, file_name))
    writeLines(dat, file_connection)
    close(file_connection)

    message(sprintf("SMART indicator template written locally: %s\n",
                    sprintf("%s/%s", folder_name, file_name)))

  }

  if(send_to_google_doc) {

    gdoc_name  <- gsub(".rmd$", "", file_name)
    gdoc_exist <- googledrive::drive_get(sprintf("Projects/MAFMC Indicators/IndicatorReport/%s", gdoc_name))

    #Check to make sure existing file is not over-written
    if(nrow(gdoc_exist) > 0 &
       !overwrite) {
      stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'", gdoc_name))
    } else {
      message("Now to render to Google Drive...\n")

      markdrive::gdoc_render(filename = sprintf("%s/%s", folder_name, file_name),
                             gdoc_name = gsub(".rmd$", "", file_name),
                             gdoc_path = "Projects/MAFMC Indicators/IndicatorReport/")

      gdoc_link <- googledrive::drive_link(sprintf("Projects/MAFMC Indicators/IndicatorReport/%s", file_name))

      message(sprintf("SMART indicator template written as a google doc:%s\n",
                      gdoc_link))
    }
  }
}

