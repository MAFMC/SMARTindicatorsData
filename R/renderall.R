#' Render all .Rmd templates in a given directory
#'
#'
#'@param moddir Optional; the path to the dirctory containing .Rmd files to be rendered, defaults to `here::here("docs")`
#'@param output Optional; Defaults to "html". Other options are "pdf" and "word" but your mileage will vary.
#'@param overwrite Logical. Defaults to FALSE. If TRUE, output will overwrite any existing html.
#'
#'@return .Rmd templates for each indicator in the indlist with the naming convention `indicator_name.Rmd`.
#'
#'
#'@examples
#'  rmddir <- here::here("test")
#'  renderall(rmddir, overwrite = TRUE)
#'
#'@export
renderall <- function(rmddir=NULL, output="html", overwrite = FALSE){

  # Directory containing Rmd files
  if(!is.null(rmddir)){
    rmd_directory <- rmddir
  }else{
    rmd_directory <- here::here("docs")
  }

  # List all Rmd files in the directory
  rmd_files <- list.files(path = rmd_directory, pattern = "\\.rmd$", full.names = TRUE)

  if(!overwrite){
    html_files <- list.files(path = rmd_directory, pattern = "\\.html$")
    html_names <- gsub(".html", "", html_files)
    html_match <- gsub("-", " ", html_names)

    rmd_files <- rmd_files[!stringr::str_detect(rmd_files, paste(html_match, collapse = "|"))]
  }

  # Apply rmarkdown::render
  if(output=="html") purrr::map(rmd_files, ~rmarkdown::render(., output_format = "html_document"))

}
