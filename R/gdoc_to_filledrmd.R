#'#' Create filled SMART template Rmd from an edited google doc for further processing.
#' @details
#' Inspired by ECSA merge_to_bookdown https://github.com/NOAA-EDAB/ECSA/blob/master/R/merge_to_bookdown.R
#' But only takes a filled gdoc back to rmd to use render_smartind and renderall for the full book
#'
#' @param indicator_name Character string. An indicator name (e.g. "Phytoplankton")
#' @param input_dir Character string. The original draft rmd that produced the unedited gdoc is in this directory. Defaults to "drafts" folder in current working directory
#' @param output_dir Character string. The output directory for the compiled bookdown HTML document and supporting files. Default "docs" folder in current working directory
#' @param overwrite Logical. If TRUE, output will overwrite any existing template for chosen species.
#'
#' @return An .Rmd file populated with text and figures
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
#' create_template(indicator_name = "smooth-dogfish", overwrite = T, output_dir = getwd())
#'
#' @export


gdoc_to_filledrmd <- function(indicator_name,
                              input_dir = here::here("drafts"),
                              output_dir = here::here("docs"),
                              overwrite = FALSE) {

  # `%>%` <- magrittr::`%>%`
  # library(readr)
  # library(googledrive)

  ### Helper functions

  ## Function to mustache the section names
  grab_text <- function(object, name) {
    pattern <- sprintf(".*\\{\\{%sStart\\}\\}(.*?)\\{\\{%sEnd\\}\\}", name, name)
    regmatches(object, regexec(pattern, object))[[1]][2]
  }


  # indicator_name <- "scup"
  # stock_file <- sprintf("%s.rmd", here::here("docs", indicator_name))

  ### Load the necessary files
  ## Download the edited google doc
  tmp_txt <- tempfile(pattern = indicator_name, fileext = ".txt")

  gdoc_exist <- googledrive::drive_get(sprintf("Projects/MAFMC Indicators/IndicatorReport/%s", indicator_name))

  if(nrow(gdoc_exist) < 1) {
    stop(sprintf("Can't find the google doc at:\nProjects/MAFMC Indicators/IndicatorReport/%s", indicator_name))
  }
  if(nrow(gdoc_exist) >= 1) {
    googledrive::drive_download(
      sprintf("Projects/MAFMC Indicators/IndicatorReport/%s", indicator_name),
      path = tmp_txt,
      overwrite = TRUE
    )
  }
  # docs_text <- paste(readLines(tmp_txt, encoding = "UTF-8", warn = F), collapse = " ")
  docs_text <- readr::read_file(tmp_txt)
  ## remove readme
  docs_text <- gsub("\\{\\{READMEStart\\}\\}(.*?)\\{\\{READMEEnd\\}\\}\r\n", "", docs_text)
  ## Fix the methods links
  docs_text <- gsub("#methods(.*?)\\)", "[methods](#methods\\1))", docs_text)


  ## Download the draft rmd
  rmd_text <- readr::read_file(sprintf("%s.rmd", file.path(input_dir, indicator_name)))
  ## remove readme
  rmd_text <- gsub("\r", "\n", rmd_text)
  rmd_text <- gsub("\\{\\{READMEStart\\}\\}(.*?)\\{\\{READMEEnd\\}\\}\n\n", "", rmd_text)

  ### Replace the text

  ## Extract the section names from the edited google doc
  doc_names <- stringr::str_extract_all(docs_text, "\\{\\{(.*)Start\\}\\}")[[1]]
  doc_names <- gsub("\\{\\{(.*?)Start\\}\\}", "\\1", doc_names)

  ## Extract the text from the edited google doc
  text_list <- lapply(doc_names, grab_text, object = docs_text)
  names(text_list) <- doc_names

  ## Pattern used to find and replace sections
  pattern <- sprintf("\\{\\{%sStart\\}\\}(.*?)\\{\\{%sEnd\\}\\}", names(text_list), names(text_list))

  ##
  new_text <- rmd_text
  for(i in 1:length(text_list)) {
    new_text <- gsub(pattern[i], text_list[[i]], new_text)
  }

  new_text <- stringr::str_replace(new_text, " \\x{030A}| \\x{00B0}",
                           "`r degree`")

  new_text <- stringr::str_replace(new_text, "oC",
                           "`r paste0(degree,'C')`")

 ## Remove extra brackets
 new_text <- gsub("\n\\{\\{.*\\}\\}\n", "", new_text)

 # new_text <- gsub("---(.*?)---",
 #  sprintf("---\n%s---", yaml::as.yaml(yml)), new_text)

  ##Create .Rmd file to be written to book
  file_name <- sprintf("%s.rmd", indicator_name)
  folder_name <- sprintf("%s",output_dir)

  # create the output directory if missing
  if(!dir.exists(folder_name)) {
    dir.create(folder_name,recursive = T)
  }


  #Check to make sure existing file is not over-written
  if(file.exists(sprintf("%s/%s",folder_name,file_name)) &  !overwrite){
    stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'", file_name))
  }



  # writes generic template after species specific substitutions to .rmd
  file_connection <- file(sprintf("%s/%s", folder_name, file_name))
  writeLines(new_text, file_connection, sep = "")


  message(sprintf("SMART Indicator template written to %s",
                  sprintf("%s/%s", folder_name, file_name)))

}

