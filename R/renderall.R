#' Render all .Rmd templates in a given directory using bookdown
#'
#'
#'@param moddir Optional; the path to the dirctory containing .Rmd files to be rendered, defaults to `here::here("docs")`
#'@param overwrite Logical. Defaults to FALSE. If TRUE, output will overwrite any existing html.
#'
#'@return .html for each indicator in the indlist with the naming convention `indicator_name.html`.
#'
#'
#'@examples
#'  rmddir <- here::here("test")
#'  renderall(rmddir, overwrite = TRUE)
#'
#'@export
renderall <- function(rmddir=NULL, overwrite = FALSE){

  # Directory containing Rmd files
  if(!is.null(rmddir)){
    rmd_directory <- rmddir
  }else{
    rmd_directory <- here::here("docs")
  }

  # List all Rmd files in the directory
  rmd_files <- list.files(path = rmd_directory, pattern = "\\.rmd$", full.names = FALSE)

  if(!overwrite){
    html_files <- list.files(path = rmd_directory, pattern = "\\.html$", full.names = FALSE)
    html_names <- gsub(".html", "", html_files)
    #html_match <- gsub("[() ]", " ", html_names)

    rmd_files <- rmd_files[!stringr::str_detect(rmd_files, paste(html_names, collapse = "|"))]
  }

  # Apply rmarkdown::render
  purrr::map(rmd_files, render_smartind)

  # if(output=="html") purrr::map(rmd_files, render_smartind)
  # #if(output=="html") purrr::map(rmd_files, ~rmarkdown::render(., output_dir = rmd_directory)) #bookdown specified in _output.yml
  # if(output=="pdf") purrr::map(rmd_files, ~rmarkdown::render(., output_dir = rmd_directory, output_format = "pdf_document"))
  # if(output=="word") purrr::map(rmd_files, ~rmarkdown::render(., output_dir = rmd_directory, output_format = "word_document"))
  # if(output=="all") purrr::map(rmd_files, ~rmarkdown::render(., output_dir = rmd_directory, output_format = "all"))

}
