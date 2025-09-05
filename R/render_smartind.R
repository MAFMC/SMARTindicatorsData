#' Render single .Rmd template using bookdown format
#'
#'
#'@param input_file file name to be rendered,  in `here::here("docs")`
#'
#'@return .html rendered using bookdown
#'
#'
#'@examples
#'  render_smartind("Aquaculture-Production.rmd")
#'
#'@export
render_smartind <- function(input_file){

  #Extract Rmd from ECSA project directory
  prev_wd <- here::here()
  # rmd <- readr::read_lines(file.path(prev_wd, paste0("docs/",input_file)))
  # output <- readr::read_lines(file.path(prev_wd,"docs/_output.yml"))

  #Create a temporary directory to build the book
  wd <- tempdir()

  file.remove(list.files(wd, full.names = T))

  if (!dir.exists(paste0(wd,"/images"))) dir.create(paste0(wd,"/images"))

  image_dir <- paste0(wd, "/images")

  file.copy(from = here::here("docs",input_file),
            to = file.path(wd, input_file), overwrite = TRUE)

  file.copy(from = here::here("docs/_output.yml"),
            to = file.path(wd,"_output.yml"), overwrite = TRUE)

  # file.copy(from = here::here("docs/images/favicon/favicon.ico"),
  #           to = file.path(wd,"favicon.ico"), overwrite = TRUE)

  file.copy(from = here::here("docs/smartind.bib"),
            to = file.path(wd,"smartind.bib"), overwrite = TRUE)

  if (!dir.exists(paste0(wd,"/data-raw"))) dir.create(paste0(wd,"/data-raw"))

  file.copy(from = here::here("data-raw/catalogdat.rds"),
            to = file.path(wd,"data-raw/catalogdat.rds"), overwrite = TRUE)

  file.copy(from = here::here("data-raw/ecodatadat.rds"),
            to = file.path(wd,"data-raw/ecodatadat.rds"), overwrite = TRUE)

  image_files <- list.files(here::here("docs/images"), full.names = TRUE)

  file.copy(file.path(image_files), image_dir, overwrite = T)

  #bookdown::render_book() requires that your input_file is in the wd
  #NOTE: Bookdown will try to knit all Rmds into one (and break) if you try to knit
  #directly from /docs
  setwd(wd)
  on.exit(setwd(prev_wd))


  #Pull out name of file without extension
  indicator_filename <- stringr::str_remove(input_file, "\\.rmd") #|\\.Rmd") only index is upper case
  #html_indicator_name <- gsub("[() ]","-", indicator_name)

  #Creates yml file in temp directory. Important for getting download button
  temp_book_yml <- paste0('book_filename: "',paste0(indicator_filename),'_.rmd"\ndelete_merged_file: true')

  #Write critical files for creating book with downloadable pdfs
  readr::write_lines(temp_book_yml, file.path(wd, "_bookdown.yml"))
  # write(rmd, file.path(wd, input_file))
  # write(output, file.path(wd,"_output.yml"))
  bookdown::render_book(input = input_file,
                        output_format = "html_document",
                        encoding = "UTF-8")#,
                        # preview = TRUE,
                        #output_dir = file.path(prev_wd,"docs")) #not sending to this directory

  #Simplifies filenames by removing extra underscores. These underscores are necessary to build
  #the pdf
  # file.rename(from = file.path(prev_wd,"docs",paste0(indicator_name,"_.pdf")),
  #             to = file.path(prev_wd,"docs",paste0(indicator_name,".pdf")))

  file.rename(from = file.path(wd,paste0(indicator_filename,"_.html")),
              to = file.path(prev_wd,"docs",paste0(indicator_filename,".html")))

  #Read in html and alter pdf filename for downloading

  # html <- readr::read_lines(file.path(prev_wd, "docs",paste0(indicator_name,".html")))
  # html <- gsub(paste0(indicator_name,'_\\.pdf'),
  #              paste0(indicator_name,'\\.pdf'),
  #              html)
  # html <- readr::write_lines(html, file.path(prev_wd, "docs",paste0(indicator_name,".html")))

  #Get back to the original directory
  file.remove(list.files(wd, full.names = T))


  message(paste(input_file, "rendered to "), prev_wd)

}
