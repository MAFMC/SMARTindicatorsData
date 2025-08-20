# generic website scrape code from Claude
# Claude's mistakes
#   * Don't use %>% without loading magrittr, I replaced with native pipe |>

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
install.packages(c("rvest", "httr", "dplyr", "stringr"))

# Use the functions directly
headings <- scrape_headings("https://example.com", "h2")
result <- fill_form("https://example.com/form", list(title = headings[1]))


