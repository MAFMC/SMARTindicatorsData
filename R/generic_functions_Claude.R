# generic website scrape code from Claude
# Claude's mistakes
#   * Don't use %>% without loading magrittr, I replaced with native pipe |>
#   * Uses %||% symbol from package infix without loading it

# Required packages (install if needed):
# install.packages(c("rvest", "httr", "dplyr", "stringr"))

# Function to scrape text from similar headings
scrape_headings <- function(url, heading_selector) {
  # Read the webpage
  page <- rvest::read_html(url)

  # Extract headings based on CSS selector or XPath
  headings <- page |>
    rvest::html_nodes(heading_selector) |>
    rvest::html_text(trim = TRUE)

  # Clean up the text (remove extra whitespace, special characters if needed)
  headings <- stringr::str_trim(headings)
  headings <- headings[headings != ""] # Remove empty strings

  return(headings)
}

# Function to scrape headings with associated content
scrape_headings_with_content <- function(url, heading_selector, content_selector = NULL) {
  page <- rvest::read_html(url)

  # Extract headings
  headings <- page |>
    rvest::html_nodes(heading_selector) |>
    rvest::html_text(trim = TRUE)

  # If content selector is provided, extract associated content
  if (!is.null(content_selector)) {
    content <- page |>
      rvest::html_nodes(content_selector) |>
      rvest::html_text(trim = TRUE)

    # Create a data frame
    result <- data.frame(
      heading = headings,
      content = content[1:length(headings)], # Match lengths
      stringsAsFactors = FALSE
    )
  } else {
    result <- data.frame(
      heading = headings,
      stringsAsFactors = FALSE
    )
  }

  return(result)
}

# Function to fill and submit a form
fill_form <- function(form_url, form_data, form_selector = "form", method = "POST") {
  # Start a session to maintain cookies
  session <- rvest::session(form_url)

  # Get the form
  form <- session |>
    rvest::html_form()

  # If multiple forms exist, select the first one or specify by index
  if (length(form) > 1) {
    form <- form[[1]]  # Select first form, adjust as needed
    warning("Multiple forms found. Using the first one.")
  } else if (length(form) == 1) {
    form <- form[[1]]
  } else {
    stop("No forms found on the page.")
  }

  # Fill the form with scraped data
  filled_form <- form

  # Map form_data to form fields
  for (field_name in names(form_data)) {
    if (field_name %in% names(filled_form$fields)) {
      filled_form$fields[[field_name]]$value <- form_data[[field_name]]
    } else {
      warning(paste("Field", field_name, "not found in form"))
    }
  }

  # Submit the form
  result <- rvest::session_submit(session, filled_form)

  return(result)
}

# Example usage function
example_scrape_and_fill <- function() {
  # Example 1: Scrape headings from a news website
  url <- "https://example-news-site.com"

  # Scrape all h2 headings
  headings <- scrape_headings(url, "h2")
  print("Scraped headings:")
  print(headings)

  # Example 2: Scrape headings with content
  headings_content <- scrape_headings_with_content(
    url,
    "h2",           # heading selector
    "h2 + p"        # content selector (paragraph following h2)
  )
  print("Headings with content:")
  print(headings_content)

  # Example 3: Fill a form with scraped data
  form_url <- "https://example-form-site.com/contact"

  # Prepare form data using scraped content
  form_data <- list(
    title = headings[1],
    description = headings_content$content[1],
    category = "News"
  )

  # Fill and submit the form
  tryCatch({
    result <- fill_form(form_url, form_data)
    print("Form submitted successfully!")
    print(result$url)  # URL after form submission
  }, error = function(e) {
    print(paste("Error submitting form:", e$message))
  })
}

# Advanced function: Pattern-based heading extraction
scrape_headings_by_pattern <- function(url, pattern, heading_levels = c("h1", "h2", "h3", "h4", "h5", "h6")) {
  page <- rvest::read_html(url)

  results <- list()

  for (level in heading_levels) {
    headings <- page |>
      rvest::html_nodes(level) |>
      rvest::html_text(trim = TRUE)

    # Filter headings that match the pattern
    matching_headings <- headings[stringr::str_detect(headings, pattern)]

    if (length(matching_headings) > 0) {
      results[[level]] <- matching_headings
    }
  }

  return(results)
}

# Function to handle forms with CSRF tokens or hidden fields
fill_form_advanced <- function(form_url, form_data, submit_button = NULL) {
  # Start session
  session <- rvest::session(form_url)

  # Get all forms
  forms <- session |>
    rvest::html_form()

  if (length(forms) == 0) {
    stop("No forms found on the page")
  }

  # Use first form or find form by attributes
  form <- forms[[1]]

  # Automatically handle hidden fields (like CSRF tokens)
  filled_form <- form

  # Fill visible fields with provided data
  for (field_name in names(form_data)) {
    if (field_name %in% names(filled_form$fields)) {
      filled_form$fields[[field_name]]$value <- form_data[[field_name]]
    }
  }

  # Submit form
  if (!is.null(submit_button)) {
    result <- rvest::session_submit(session, filled_form, submit = submit_button)
  } else {
    result <- rvest::session_submit(session, filled_form)
  }

  return(result)
}

# Utility function to preview form fields
preview_form_fields <- function(url) {
  page <- rvest::read_html(url)
  forms <- rvest::html_form(page)

  if (length(forms) == 0) {
    print("No forms found on the page")
    return(NULL)
  }

  for (i in seq_along(forms)) {
    cat("Form", i, ":\n")
    form <- forms[[i]]

    cat("Action:", form$action, "\n")
    cat("Method:", form$method, "\n")
    cat("Fields:\n")

    for (field_name in names(form$fields)) {
      field <- form$fields[[field_name]]
      cat("  -", field_name, "(", field$type, ")\n")
    }
    cat("\n")
  }
}

# Example usage with error handling
safe_scrape_and_fill <- function(scrape_url, form_url, heading_selector, field_mapping) {
  tryCatch({
    # Scrape data
    cat("Scraping data from:", scrape_url, "\n")
    scraped_data <- scrape_headings(scrape_url, heading_selector)

    if (length(scraped_data) == 0) {
      stop("No data scraped from the webpage")
    }

    # Prepare form data based on field mapping
    form_data <- list()
    for (form_field in names(field_mapping)) {
      data_index <- field_mapping[[form_field]]
      if (data_index <= length(scraped_data)) {
        form_data[[form_field]] <- scraped_data[data_index]
      }
    }

    # Preview form before filling
    cat("Previewing form fields:\n")
    preview_form_fields(form_url)

    # Fill form
    cat("Filling form with scraped data...\n")
    result <- fill_form_advanced(form_url, form_data)

    cat("Success! Form submitted.\n")
    return(result)

  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# Alternative function using httr for more control over HTTP requests
fill_form_with_httr <- function(form_url, form_data, headers = NULL) {
  # Get the form page first to extract any hidden fields
  page_response <- httr::GET(form_url)
  page_content <- httr::content(page_response, "text")
  page <- rvest::read_html(page_content)

  # Extract form details
  forms <- rvest::html_form(page)
  if (length(forms) == 0) {
    stop("No forms found on the page")
  }

  form <- forms[[1]]

  # Prepare POST data including hidden fields
  post_data <- list()

  # Add existing hidden fields
  for (field_name in names(form$fields)) {
    field <- form$fields[[field_name]]
    if (field$type == "hidden" && !is.null(field$value)) {
      post_data[[field_name]] <- field$value
    }
  }

  # Add user-provided data
  post_data <- c(post_data, form_data)

  # Determine form action URL
  action_url <- form$action
  if (action_url == "") {
    action_url <- form_url
  } else if (!stringr::str_detect(action_url, "^https?://")) {
    # Relative URL, make it absolute
    parsed_url <- httr::parse_url(form_url)
    base_url <- paste0(parsed_url$scheme, "://", parsed_url$hostname)
    if (!is.null(parsed_url$port)) {
      base_url <- paste0(base_url, ":", parsed_url$port)
    }
    action_url <- paste0(base_url, action_url)
  }

  # Submit form using httr
  if (is.null(headers)) {
    headers <- list(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      "Referer" = form_url
    )
  }

  response <- httr::POST(
    action_url,
    body = post_data,
    encode = "form",
    httr::add_headers(.headers = headers)
  )

  return(response)
}

# Run example (uncomment to test)
# example_scrape_and_fill()

# Install packages if needed
#install.packages(c("rvest", "httr", "dplyr", "stringr"))

# Use the functions directly
#headings <- scrape_headings("https://example.com", "h2")
#result <- fill_form("https://example.com/form", list(title = headings[1]))

# my tests

scrape_headings_by_pattern("https://noaa-edab.github.io/catalog/trans_dates.html", "Key Results and Visualizations") # returns 45.2
scrape_headings_by_pattern("https://noaa-edab.github.io/catalog/index.html", "Key Results and Visualizations") # empty

scrape_headings_with_content("https://noaa-edab.github.io/catalog/trans_dates.html", "h2", "h2+p") # misaligns, first paragraph only
scrape_headings_with_content("https://noaa-edab.github.io/catalog/trans_dates.html", "h2", "body") # everything in first heading

url <- "https://noaa-edab.github.io/catalog/trans_dates.html"

html <- rvest::read_html(url)

html |> rvest::html_elements("h2") # want these but not first and last

html |> rvest::html_elements("p")

scrape_headings(url, "h2")

# combine pattern heading with content

scrape_headings_pattern_content <- function(url, pattern, elements) {

  page <- rvest::read_html(url)

  results <- list()

    headings <- page |>
      rvest::html_elements(elements) |>
      rvest::html_text(trim = TRUE)

    # Filter headings that match the pattern
    matching_headings <- headings[stringr::str_detect(headings, pattern)]

      # Create a data frame
      result <- data.frame(
        heading = matching_headings,
        content = content[1:length(matching_headings)], # Match lengths
        stringsAsFactors = FALSE
      )

    return(result)
}

#scrape_headings_pattern_content(url, "Key", "h2, p")

extract_links_rvest <- function(url) {
  # Read the webpage
  page <- rvest::read_html(url)

  # Extract all href attributes from <a> tags
  links <- page |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  # Remove NA values and empty strings
  links <- links[!is.na(links) & links != ""]

  # Convert relative URLs to absolute URLs
  absolute_links <- xml2::url_absolute(links, url)

  return(absolute_links)
}

#######################################################################################
# R Code to Find Documents on Web Page Containing Specific String

# Required packages
library(rvest)
library(httr)
library(stringr)
library(purrr)
library(dplyr)
library(infix)

# Step 1: Extract all document links from webpage
extract_document_links <- function(url, doc_extensions = c("pdf", "doc", "docx", "txt", "xls", "xlsx", "ppt", "pptx")) {

  # Read the webpage
  page <- rvest::read_html(url)

  # Extract all href attributes
  all_links <- page |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  # Remove NA values
  all_links <- all_links[!is.na(all_links)]

  # Convert relative URLs to absolute
  all_links <- xml2::url_absolute(all_links, url)

  # Filter for document extensions
  doc_pattern <- base::paste0("\\.(", base::paste(doc_extensions, collapse = "|"), ")($|\\?)", collapse = "")
  doc_links <- all_links[stringr::str_detect(all_links, stringr::regex(doc_pattern, ignore_case = TRUE))]

  return(doc_links)
}

# Step 2: Download and check document content
check_document_for_string <- function(doc_url, search_string, temp_dir = tempdir()) {

  tryCatch({
    # Determine file extension
    file_ext <- stringr::str_extract(doc_url, "\\.[^.\\?]+(?=\\?|$)")
    file_ext <- stringr::str_remove(file_ext, "^\\.")

    # Create temporary file path
    temp_file <- base::file.path(temp_dir, base::paste0("temp_doc.", file_ext))

    # Download the document
    response <- httr::GET(doc_url, httr::write_disk(temp_file, overwrite = TRUE))

    if (httr::status_code(response) != 200) {
      return(list(url = doc_url, found = FALSE, error = "Download failed"))
    }

    # Extract text based on file type
    text_content <- extract_text_from_file(temp_file, file_ext)

    # Check if string exists in content
    found <- stringr::str_detect(text_content, stringr::fixed(search_string), ignore.case = TRUE)

    # Clean up temp file
    base::unlink(temp_file)

    return(list(
      url = doc_url,
      found = found,
      file_type = file_ext,
      content_length = base::nchar(text_content),
      error = NULL
    ))

  }, error = function(e) {
    return(list(url = doc_url, found = FALSE, error = e$message))
  })
}

# Step 3: Extract text from different file types
extract_text_from_file <- function(file_path, file_ext) {

  text_content <- ""

  if (file_ext %in% c("txt", "csv", "log")) {
    # Plain text files
    text_content <- base::readLines(file_path, warn = FALSE) |>
      base::paste(collapse = "\n")

  } else if (file_ext == "pdf") {
    # PDF files (requires pdftools package)
    if (base::requireNamespace("pdftools", quietly = TRUE)) {
      text_content <- pdftools::pdf_text(file_path) |>
        base::paste(collapse = "\n")
    } else {
      warning("pdftools package required for PDF files")
    }

  } else if (file_ext %in% c("doc", "docx")) {
    # Word documents (requires textreadr package)
    if (base::requireNamespace("textreadr", quietly = TRUE)) {
      text_content <- textreadr::read_document(file_path)
    } else if (base::requireNamespace("officer", quietly = TRUE)) {
      # Alternative using officer package
      doc <- officer::read_docx(file_path)
      text_content <- officer::docx_summary(doc)$text |>
        base::paste(collapse = "\n")
    } else {
      warning("textreadr or officer package required for Word documents")
    }

  } else if (file_ext %in% c("xls", "xlsx")) {
    # Excel files (requires readxl package)
    if (base::requireNamespace("readxl", quietly = TRUE)) {
      # Read all sheets and combine
      sheets <- readxl::excel_sheets(file_path)
      all_data <- purrr::map(sheets, ~readxl::read_excel(file_path, sheet = .x))
      text_content <- all_data |>
        purrr::map(~base::paste(base::unlist(.x), collapse = " ")) |>
        base::paste(collapse = "\n")
    } else {
      warning("readxl package required for Excel files")
    }

  } else if (file_ext %in% c("ppt", "pptx")) {
    # PowerPoint files (requires officer package)
    if (base::requireNamespace("officer", quietly = TRUE)) {
      ppt <- officer::read_pptx(file_path)
      text_content <- officer::pptx_summary(ppt)$text |>
        base::paste(collapse = "\n")
    } else {
      warning("officer package required for PowerPoint files")
    }
  }

  return(text_content)
}

# Step 4: Main function to find all documents containing string
find_documents_with_string <- function(webpage_url, search_string,
                                       doc_extensions = c("pdf", "doc", "docx", "txt", "xls", "xlsx"),
                                       parallel = FALSE, max_workers = 4) {

  cat("Step 1: Extracting document links from webpage...\n")
  doc_links <- extract_document_links(webpage_url, doc_extensions)

  if (base::length(doc_links) == 0) {
    cat("No document links found on the webpage.\n")
    return(data.frame())
  }

  cat(base::paste("Found", base::length(doc_links), "document links\n"))

  cat("Step 2: Checking documents for search string...\n")

  if (parallel && base::requireNamespace("parallel", quietly = TRUE)) {
    # Parallel processing
    cl <- parallel::makeCluster(base::min(max_workers, parallel::detectCores() - 1))
    parallel::clusterEvalQ(cl, {
      library(httr)
      library(stringr)
    })

    results <- parallel::parLapply(cl, doc_links, check_document_for_string,
                                   search_string = search_string)
    parallel::stopCluster(cl)
  } else {
    # Sequential processing with progress
    results <- purrr::map(doc_links, ~{
      cat(".")
      check_document_for_string(.x, search_string)
    })
  }

  cat("\nStep 3: Compiling results...\n")

  # Convert results to data frame
  results_df <- purrr::map_dfr(results, ~{
    data.frame(
      url = .x$url,
      found = .x$found %||% FALSE,
      file_type = .x$file_type %||% "unknown",
      content_length = .x$content_length %||% 0,
      error = .x$error %||% NA,
      stringsAsFactors = FALSE
    )
  })

  # Summary
  found_count <- base::sum(results_df$found, na.rm = TRUE)
  error_count <- base::sum(!is.na(results_df$error))

  cat(base::paste("Search complete!\n"))
  cat(base::paste("- Total documents checked:", base::nrow(results_df), "\n"))
  cat(base::paste("- Documents containing '", search_string, "':", found_count, "\n"))
  cat(base::paste("- Documents with errors:", error_count, "\n"))

  return(results_df)
}

# Step 5: Helper function to display results
display_results <- function(results_df, show_errors = FALSE) {

  # Documents containing the string
  found_docs <- results_df[results_df$found == TRUE, ]
  if (base::nrow(found_docs) > 0) {
    cat("\n=== DOCUMENTS CONTAINING SEARCH STRING ===\n")
    for (i in 1:base::nrow(found_docs)) {
      cat(base::paste(i, ".", found_docs$url[i],
                      "(", found_docs$file_type[i], ")\n"))
    }
  }

  # Show errors if requested
  if (show_errors) {
    error_docs <- results_df[!is.na(results_df$error), ]
    if (base::nrow(error_docs) > 0) {
      cat("\n=== DOCUMENTS WITH ERRORS ===\n")
      for (i in 1:base::nrow(error_docs)) {
        cat(base::paste(i, ".", error_docs$url[i], "-", error_docs$error[i], "\n"))
      }
    }
  }
}

# Step 6: Advanced search with multiple strings
find_documents_multiple_strings <- function(webpage_url, search_strings,
                                            match_all = FALSE, doc_extensions = c("pdf", "doc", "docx", "txt")) {

  doc_links <- extract_document_links(webpage_url, doc_extensions)

  results <- purrr::map_dfr(doc_links, function(doc_url) {

    # Check each document for all search strings
    matches <- purrr::map_lgl(search_strings, function(search_string) {
      result <- check_document_for_string(doc_url, search_string)
      return(result$found %||% FALSE)
    })

    # Determine if document matches criteria
    if (match_all) {
      overall_match <- base::all(matches)
    } else {
      overall_match <- base::any(matches)
    }

    data.frame(
      url = doc_url,
      overall_match = overall_match,
      base::setNames(matches, base::paste0("contains_", search_strings)),
      stringsAsFactors = FALSE
    )
  })

  return(results)
}

# Example usage function
example_usage <- function() {
  cat("=== EXAMPLE USAGE ===\n\n")
  cat("# Basic usage:\n")
  cat('results <- find_documents_with_string("https://example.com", "climate change")\n')
  cat('display_results(results)\n\n')

  cat("# With specific file types:\n")
  cat('results <- find_documents_with_string("https://example.com", "budget", \n')
  cat('                                     doc_extensions = c("pdf", "xlsx"))\n\n')

  cat("# Multiple search strings:\n")
  cat('results <- find_documents_multiple_strings("https://example.com", \n')
  cat('                                          c("sustainability", "environment"))\n\n')

  cat("# Required packages for full functionality:\n")
  cat("# install.packages(c('pdftools', 'textreadr', 'readxl', 'officer'))\n")
}

# Load required packages check
check_required_packages <- function() {
  required <- c("rvest", "httr", "stringr", "purrr", "dplyr")
  optional <- c("pdftools", "textreadr", "readxl", "officer", "parallel")

  cat("Required packages:\n")
  for (pkg in required) {
    status <- if (base::requireNamespace(pkg, quietly = TRUE)) "✓" else "✗"
    cat(base::paste(" ", status, pkg, "\n"))
  }

  cat("\nOptional packages (for full file support):\n")
  for (pkg in optional) {
    status <- if (base::requireNamespace(pkg, quietly = TRUE)) "✓" else "✗"
    cat(base::paste(" ", status, pkg, "\n"))
  }
}

# cat("Document search functions loaded!\n")
# cat("Run check_required_packages() to see package status\n")
# cat("Run example_usage() to see usage examples\n")

# Search for documents containing "climate change"
results <- find_documents_with_string(
  webpage_url = "https://www.mafmc.org/",
  search_string = "fisheries",
  doc_extensions = c("pdf", "doc", "docx")
)

# Display results
display_results(results)
