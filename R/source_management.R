# R/source_management.R
#
# these functions handle a complex task - just taking the info already available
# in PDFs in the sources folder, and just making a bib file...
#   the world's hardest task despite journals and formats being "organized" and
#   "Portable"
#
#
### ----------------------------------------------------- ###


#' Fix PDF encoding issues
#'
#' @param text Text to fix
#' @return Fixed text with proper UTF-8 encoding
fix_pdf_encoding <- function(text) {
  if(is.null(text) || is.na(text)) return(NULL)

  # Remove null bytes first
  text <- gsub("\\x00", "", text, fixed = TRUE)

  # Try to detect and fix encoding
  tryCatch({
    # First attempt: assume UTF-8 is correct
    if(Encoding(text) == "UTF-8") {
      # It might already be correct
      if(!grepl("Ã|Â|Ä|Å|Ã¼|Ã¤|Ã¶", text)) {
        return(text)
      }
    }

    # Try different encoding conversions
    attempts <- list(
      # UTF-8 double encoding (most common in PDFs)
      function(x) {
        x <- iconv(x, from = "UTF-8", to = "latin1", sub = "")
        iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
      },
      # Windows-1252 to UTF-8
      function(x) iconv(x, from = "windows-1252", to = "UTF-8", sub = ""),
      # Latin-1 to UTF-8
      function(x) iconv(x, from = "latin1", to = "UTF-8", sub = ""),
      # MacRoman to UTF-8
      function(x) iconv(x, from = "macintosh", to = "UTF-8", sub = "")
    )

    for(attempt in attempts) {
      result <- tryCatch(attempt(text), error = function(e) NA)
      if(!is.na(result) && nchar(result) > 0 && !grepl("Ã|Â(?!\\s)", result, perl = TRUE)) {
        return(result)
      }
    }

    # Manual fixes for common PDF encoding errors
    fixed <- text

    # Fix specific character patterns
    replacements <- list(
      # Turkish/Special Latin characters
      "ÃÂ¼" = "ü",
      "ÃÂ£" = "ã",
      "GÃ¶" = "Gö",
      "Ã¶" = "ö",
      "Ã¤" = "ä",
      "Ã¼" = "ü",
      "Ã©" = "é",
      "Ã¨" = "è",
      "Ã " = "à",
      "Ã§" = "ç",
      "Ã±" = "ñ",
      "Ã" = "í",
      "Ã³" = "ó",
      # Common UTF-8 double encoding patterns
      "Ã¢â‚¬â„¢" = "'",
      'Ã¢â‚¬Å"' = '"',     # Using single to keep from breaking the function
      'Ã¢â‚¬Âº' = '"',
      "Ã¢â‚¬â€œ" = "–",
      'Ã¢â‚¬â€"' = "—",
      "Â " = " ",
      "â€™" = "'",
      'â€œ' = '"',
      'â€�' = '"',
      'â€"' = "–",
      'â€"' = "—"
    )

    for(pattern in names(replacements)) {
      fixed <- gsub(pattern, replacements[[pattern]], fixed, fixed = TRUE)
    }

    # Clean up any remaining odd characters
    fixed <- gsub("Ã.{1,2}", "", fixed)  # Remove remaining encoding artifacts
    fixed <- gsub("Â(?=\\s|$)", "", fixed, perl = TRUE)  # Remove standalone Â

    # If we still have issues, try transliteration
    if(grepl("[^\x01-\x7F]", fixed) && grepl("Ã|Â", fixed)) {
      fixed <- iconv(fixed, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    }

    return(fixed)

  }, error = function(e) {
    # If all else fails, return original
    return(text)
  })
}

# try to parse DOI
find_doi <- function(text) {
  if(is.null(text) || length(text) == 0) return(NULL)

  # Combine all text
  full_text <- paste(text, collapse = " ")

  # First, try to find DOI URLs and extract the DOI
  url_patterns <- c(
    # Full HTTPS URLs
    "https://doi\\.org/(10\\.\\d{4,}/[^\\s]+)",
    "http://doi\\.org/(10\\.\\d{4,}/[^\\s]+)",
    "https://dx\\.doi\\.org/(10\\.\\d{4,}/[^\\s]+)",
    "http://dx\\.doi\\.org/(10\\.\\d{4,}/[^\\s]+)",
    # URLs that might be broken across lines
    "doi\\.org/(10\\.\\d{4,}/[^\\s]+)"
  )

  # Try URL patterns first
  for(pattern in url_patterns) {
    matches <- regmatches(full_text, gregexpr(pattern, full_text, perl = TRUE))[[1]]

    if(length(matches) > 0) {
      # Extract just the DOI part (10.xxxx/xxxx)
      doi_match <- regmatches(matches[1], regexec(pattern, matches[1]))[[1]]
      if(length(doi_match) > 1) {
        # Clean trailing punctuation
        doi <- gsub("[.,;:!?)\\]]+$", "", doi_match[2])
        return(doi)
      }
    }
  }

  # Then try standard DOI patterns
  doi_patterns <- c(
    # Standard DOI format
    "10\\.\\d{4,}/[-._;()/:A-Za-z0-9]+",
    # DOI with doi: prefix
    "doi:\\s*10\\.\\d{4,}/[-._;()/:A-Za-z0-9]+",
    # DOI with spaces
    "10\\.\\d{4,}\\s*/\\s*[-._;()/:A-Za-z0-9]+",
    # Common variations
    "DOI\\s+10\\.\\d{4,}/[-._;()/:A-Za-z0-9]+"
  )

  for(pattern in doi_patterns) {
    matches <- regmatches(full_text, gregexpr(pattern, full_text, ignore.case = TRUE, perl = TRUE))[[1]]

    if(length(matches) > 0) {
      # Clean up the DOI
      doi <- matches[1]
      doi <- gsub("^doi:\\s*", "", doi, ignore.case = TRUE)
      doi <- gsub("^DOI\\s+", "", doi, ignore.case = TRUE)
      doi <- gsub("\\s+", "", doi)  # Remove any spaces
      doi <- gsub("[.,;:!?)\\]]+$", "", doi)

      # Validate it starts with 10.
      if(grepl("^10\\.", doi)) {
        return(doi)
      }
    }
  }

  return(NULL)
}


#' Extract year from CrossRef data or text
#'
#' @param text CrossRef data (list) or text to search
#' @return Year as character string or NULL
extract_year <- function(text) {
  if(is.null(text)) return(NULL)

  # For CrossRef data
  if(is.list(text)) {
    # Try multiple date fields
    date_fields <- c("published-print", "published-online", "issued", "created", "deposited")

    # Loop through each field
    for(field in date_fields) {
      if(field %in% names(text) && !is.null(text[[field]])) {
        # Handle different date structures
        date_data <- text[[field]]

        # Check if it's a list with date-parts
        if(is.list(date_data) && "date-parts" %in% names(date_data)) {
          if(!is.null(date_data[["date-parts"]][[1]][[1]])) {
            year <- as.character(date_data[["date-parts"]][[1]][[1]])
            if(!is.na(year) && as.numeric(year) >= 1900 && as.numeric(year) <= as.numeric(format(Sys.Date(), "%Y")) + 2) {
              return(year)
            }
          }
        }
        # Check if it's a direct year value
        else if(is.numeric(date_data) || (is.character(date_data) && grepl("^\\d{4}$", date_data))) {
          year <- as.character(date_data)
          if(as.numeric(year) >= 1900 && as.numeric(year) <= as.numeric(format(Sys.Date(), "%Y")) + 2) {
            return(year)
          }
        }
      }
    }
  }

  # For text extraction - be more careful about what we accept as years
  text_str <- if(is.character(text)) paste(text, collapse = " ") else as.character(text)


  # Priority 1: Look for copyright years (very reliable for books/chapters)
  copyright_patterns <- c(
    "Copyright\\s*[©Â©]\\s*(\\d{4})",  # Copyright © 2018
    "©\\s*(\\d{4})",                     # © 2018
    "\\(c\\)\\s*(\\d{4})",               # (c) 2018
    "Copyright\\s+(\\d{4})"              # Copyright 2018
  )

  for(pattern in copyright_patterns) {
    matches <- regmatches(text_str, gregexpr(pattern, text_str, ignore.case = TRUE))[[1]]
    if(length(matches) > 0) {
      year <- gsub(".*?(\\d{4}).*", "\\1", matches[1])
      year_num <- as.numeric(year)
      if(year_num >= 1800 && year_num <= as.numeric(format(Sys.Date(), "%Y")) + 2) {
        return(year)
      }
    }
  }

  # Priority 2: Conference/Workshop patterns
  conf_patterns <- c(
    "Conference.*?(19|20)\\d{2}",       # Conference, ICA 2009
    "Workshop.*?(19|20)\\d{2}",         # Workshop 2023
    "Symposium.*?(19|20)\\d{2}",        # Symposium 2023
    "Proceedings.*?(19|20)\\d{2}",      # Proceedings 2023
    "\\b[A-Z]{2,}\\s+(19|20)\\d{2}\\b", # ICA 2009, ICML 2023, etc.
    "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+(19|20)\\d{2}" # March 2009
  )

  for(pattern in conf_patterns) {
    matches <- regmatches(text_str, gregexpr(pattern, text_str, ignore.case = TRUE))[[1]]
    if(length(matches) > 0) {
      year <- gsub(".*((19|20)\\d{2}).*", "\\1", matches[1])
      year_num <- as.numeric(year)
      if(year_num >= 1800 && year_num <= as.numeric(format(Sys.Date(), "%Y")) + 2) {
        return(year)
      }
    }
  }

  # Look for years in parentheses first (most reliable)
  paren_years <- regmatches(text_str, gregexpr("\\((19|20)\\d{2}\\)", text_str))[[1]]
  if(length(paren_years) > 0) {
    year <- gsub("[^0-9]", "", paren_years[1])
    year_num <- as.numeric(year)
    if(year_num >= 1800 && year_num <= as.numeric(format(Sys.Date(), "%Y")) + 2) {
      return(year)
    }
  }

  # Look for "Year:" or "Published:" patterns
  year_label_patterns <- c(
    "(Year|Published|Publication Date|Date)\\s*:?\\s*(19|20)\\d{2}",
    "Published\\s+in\\s+(19|20)\\d{2}",
    "Publication\\s+year\\s*:?\\s*(19|20)\\d{2}"
  )

  for(pattern in year_label_patterns) {
    matches <- regmatches(text_str, gregexpr(pattern, text_str, ignore.case = TRUE))[[1]]
    if(length(matches) > 0) {
      year <- gsub(".*((19|20)\\d{2}).*", "\\1", matches[1])
      year_num <- as.numeric(year)
      if(year_num >= 1900 && year_num <= as.numeric(format(Sys.Date(), "%Y")) + 2) {
        return(year)
      }
    }
  }

  header_patterns <- c(
    "Vol\\.?\\s*\\d+.*?(19|20)\\d{2}",  # Vol. 123 (2018)
    "Volume\\s*\\d+.*?(19|20)\\d{2}",   # Volume 123, 2018
    "\\d+\\s*\\((19|20)\\d{2}\\)",      # 123 (2018)
    "pp?\\.?\\s*\\d+[-–]\\d+.*?(19|20)\\d{2}"  # pp. 123-456, 2018
  )

  for(pattern in header_patterns) {
    matches <- regmatches(text_str, gregexpr(pattern, text_str, ignore.case = TRUE))[[1]]
    if(length(matches) > 0) {
      year <- gsub(".*((19|20)\\d{2}).*", "\\1", matches[1])
      year_num <- as.numeric(year)
      if(year_num >= 1900 && year_num <= as.numeric(format(Sys.Date(), "%Y")) + 2) {
        return(year)
      }
    }
  }
  # Last resort: Look for 4-digit years but be strict
  year_matches <- unlist(regmatches(text_str, gregexec("(?<!\\d)(19[5-9]\\d|20[0-3]\\d)(?!\\d)", text_str, perl = TRUE)))

  if(length(year_matches) > 1) {  # First element is the full match
    year_matches <- year_matches[-1]  # Remove the full match, keep captures

    for(year in year_matches) {
      # Check context around the year to avoid ISBN parts
      year_context_pattern <- paste0(".{0,30}", year, ".{0,30}")
      context_matches  <- regmatches(text_str, regexpr(year_context_pattern, text_str))

      # Skip if it looks like part of an ISBN or identifier
      if(length(context_matches) == 1) {
        context <- context_matches[1]

        # Skip if it looks like part of an ISBN or identifier
        if(grepl("978-\\d+-\\d+-\\d+-\\d+|979-\\d+|ISBN|ISSN", context)) {
          next
        }

        # Skip if the year is part of a longer number
        if(grepl(paste0("\\d", year, "|", year, "\\d"), context)) {
          next
        }

        # Good contexts for years
        if(grepl("publish|copyright|©|\\(c\\)|date|year|journal|volume|vol\\.|conference|proceedings|march|april|may",
                 context, ignore.case = TRUE)) {
          return(year)
        }
      }
    }
  }

  return(NULL)
}

#' Extract authors from PDF first page
#'
#' Attempts to extract author names from the first page of a PDF
#'
#' @param pdf_text Character vector of PDF pages
#' @return Character string of authors or NULL
extract_authors_from_pdf <- function(pdf_text) {
  if(is.null(pdf_text) || length(pdf_text) == 0) return(NULL)

  # Focus on first page
  first_page <- pdf_text[1]

  # Split into lines
  lines <- strsplit(first_page, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]

  # Look for author patterns
  # Authors often appear:
  # 1. After the title and before affiliations
  # 2. In lines with "and" connecting names
  # 3. As names with typical patterns (First Last, F. Last, etc.)

  potential_authors <- character()

  # Pattern 1: Lines that look like author names
  name_pattern <- "^[A-Z][a-z]+\\s+[A-Z][a-z]+(\\s+[A-Z][a-z]+)*$|^[A-Z]\\.\\s*[A-Z][a-z]+$"

  for(i in seq_along(lines)) {
    line <- lines[i]

    # Skip if too long (probably not an author line)
    if(nchar(line) > 100) next

    # Check if line contains "and" (common in author lists)
    if(grepl("\\band\\b", line, ignore.case = TRUE)) {
      # This might be an author line
      potential_authors <- c(potential_authors, line)
    }
    # Check if line matches name pattern
    else if(grepl(name_pattern, line)) {
      potential_authors <- c(potential_authors, line)
    }
    # Check for lines with multiple comma-separated names
    else if(grepl("^[A-Z][^,]+,\\s*[A-Z][^,]+", line)) {
      potential_authors <- c(potential_authors, line)
    }
  }

  # If we found potential authors, clean and combine them
  if(length(potential_authors) > 0) {
    # Combine lines that might be part of the same author list
    author_text <- paste(potential_authors, collapse = " ")

    # Clean up
    author_text <- gsub("\\s+", " ", author_text)
    author_text <- gsub("\\s*,\\s*and\\s*", " and ", author_text, ignore.case = TRUE)

    # Don't return if it's too long or contains suspicious words
    if(nchar(author_text) < 200 &&
       !grepl("University|Department|Institute|Center|School|Faculty", author_text, ignore.case = TRUE)) {
      return(author_text)
    }
  }

  return(NULL)
}


#' Extract bibliographic information from PDF first page
#'
#' Attempts to extract title, authors, year, journal, and DOI from the first page
#'
#' @param pdf_text Character vector of PDF pages (at least first page)
#' @return List with extracted metadata
extract_metadata_from_first_page <- function(pdf_text) {
  if(is.null(pdf_text) || length(pdf_text) == 0) return(list())

  # Get first two pages (sometimes metadata spills over)
  text <- paste(pdf_text[1:min(2, length(pdf_text))], collapse = "\n")

  # Split into lines for analysis
  lines <- strsplit(text, "\n")[[1]]
  lines <- trimws(lines)

  metadata <- list()

  # 1. Extract DOI
  metadata$doi <- find_doi(pdf_text)

  # 2. Extract title (usually the largest text block near the top)
  # Titles are often:
  # - In the first 10-15 lines
  # - Multiple words, properly capitalized
  # - May span multiple lines
  # - Not in all caps (that's usually journal name)

  potential_titles <- character()
  title_region <- lines[1:min(20, length(lines))]

  skip_patterns <- c(
    "^https?://",
    "^doi:",
    "^DOI:",
    "\\d+\\s*[-–]\\s*\\d+$",
    "^[A-Z]+,\\s*\\d{4}",
    "journal homepage",
    "^Downloaded from",
    "^\\d+$",
    "University|Institute|Department|Center|School",
    "@"
  )

  for(i in seq_along(title_region)) {
    line <- title_region[i]

    # Skip empty or very short lines
    if(nchar(line) < 10) next

    # Skip lines matching skip patterns
    skip_this <- FALSE
    for(pattern in skip_patterns) {
      if(grepl(pattern, line, ignore.case = TRUE)) {
        skip_this <- TRUE
        break
      }
    }
    if(skip_this) next

    # Skip if it's all caps (usually journal name or section header)
    if(line == toupper(line) && !grepl("REM|DNA|RNA|COVID|AIDS|HIV", line)) next

    # Skip if it looks like author names (has numbers or many commas)
    if(grepl("\\d+,\\d+|[,].*[,].*[,]", line)) next

    if(nchar(line) > 20) {
      # Skip lines that are clearly not titles
      if(grepl("^[A-Z]\\.", line) ||                    # Author initials
         grepl("\\d+,\\s*\\d+", line) ||                # Author affiliations
         grepl("^\\s*\\d+\\s*$", line) ||               # Just numbers
         grepl("^(Chapter|CHAPTER)\\s+\\d+", line)) {   # Chapter headers
        next
      }

      # Check if this could be a title
      # Titles often have:
      # - Mix of upper and lowercase (but not all uppercase)
      # - Multiple words
      # - No email addresses or URLs
      # - Reasonable length

      # Count uppercase vs lowercase to detect all-caps
      n_upper <- nchar(gsub("[^A-Z]", "", line))
      n_lower <- nchar(gsub("[^a-z]", "", line))

      # Skip if all caps (unless it contains special acronyms)
      if(n_upper > 0 && n_lower == 0 && !grepl("REM|DNA|RNA|COVID|AIDS|HIV|MRI|CT|PET", line)) {
        next
      }

      # This looks like it could be a title
      combined_title <- line
      j <- i + 1

      # Keep adding lines that look like title continuation
      while(j <= length(title_region) && j <= i + 4) {  # Max 5 lines for title
        if(j > length(title_region)) break

        next_line <- title_region[j]

        # Stop if we hit an empty line or clear metadata
        if(nchar(trimws(next_line)) == 0) break

        # Stop if we hit author names (with affiliations or "and")
        if(grepl("^[A-Z][a-z]+\\s+[A-Z][a-z]+\\d+", next_line) ||
           grepl("\\d+,\\s*\\d+", next_line) ||
           grepl("@", next_line)) {
          break
        }

        # Stop if we hit keywords like University (likely author affiliation)
        if(grepl("University|Department|Institute|^a\\s+", next_line, ignore.case = TRUE)) {
          break
        }

        # Add line if it looks like part of the title
        if(nchar(trimws(next_line)) > 5) {
          combined_title <- paste(combined_title, trimws(next_line))
          j <- j + 1
        } else {
          break
        }
      }

      # Clean up the title
      combined_title <- gsub("\\s+", " ", combined_title)
      combined_title <- trimws(combined_title)

      # Final validation - make sure it's a reasonable title
      if(nchar(combined_title) > 30 && nchar(combined_title) < 500) {
        # Remove any trailing punctuation except question marks
        combined_title <- gsub("[.,;:]+$", "", combined_title)

        potential_titles <- c(potential_titles, combined_title)
      }
    }

    # Take the first good title candidate
    if(length(potential_titles) > 0) {
      # Prefer titles that contain certain keywords
      title_keywords <- c("effect", "analysis", "study", "review", "assessment",
                          "evaluation", "comparison", "investigation", "development",
                          "application", "method", "approach", "model", "system")

      for(title in potential_titles) {
        if(any(sapply(title_keywords, function(k) grepl(k, title, ignore.case = TRUE)))) {
          metadata$title <- title
          break
        }
      }

      # If no keyword match, take the first one
      if(is.null(metadata$title)) {
        metadata$title <- potential_titles[1]
      }
    }
  }


  # 3. Extract authors
  # Look for lines with author patterns after the title
  author_lines <- character()
  start_looking <- 3  # Default

  if(length(potential_titles) > 0 && !is.null(metadata$title)) {
    # Find the last line that's part of the title
    # Look for where the title text ends in the lines
    title_words <- strsplit(metadata$title, " ")[[1]]

    # Look for the last few words of the title
    if(length(title_words) > 3) {
      last_words <- paste(tail(title_words, 3), collapse = " ")
      for(k in 1:length(lines)) {
        if(grepl(last_words, lines[k], fixed = TRUE)) {
          start_looking <- k + 1
          break
        }
      }
    }
  }

  if(!is.na(start_looking)) {
    for(i in start_looking:min(start_looking + 10, length(lines))) {
      if(i > length(lines)) break

      line <- lines[i]

      # Author patterns:
      # - Names with "and" between them
      # - Comma-separated names
      # - Names with superscripts (1,2,3 or symbols)
      # - Names in format "First Last" or "F. Last"

      # Skip if it's an affiliation line
      if(grepl("University|Department|Institute|School|Hospital|Center|Laboratory", line, ignore.case = TRUE)) {
        break  # Usually affiliations come after authors
      }

      # Check for author-like patterns
      if(grepl("\\band\\b", line) ||
         grepl("^[A-Z][a-z]+(\\s+[A-Z]\\.?\\s*)?[A-Z][a-z]+", line) ||
         grepl("^[A-Z]\\.", line) ||
         grepl("[A-Z][a-z]+\\s*[,，]\\s*[A-Z]", line)) {

        # Remove superscripts and symbols
        clean_line <- gsub("[0-9,*†‡§¶∥¹²³⁴⁵⁶⁷⁸⁹]+", "", line)
        clean_line <- trimws(clean_line)

        if(nchar(clean_line) > 3) {
          author_lines <- c(author_lines, clean_line)
        }
      }
    }
  }

  if(length(author_lines) > 0) {
    # Combine author lines
    authors <- paste(author_lines, collapse = " ")
    # Standardize "and" separators
    authors <- gsub("\\s+and\\s+", " and ", authors, ignore.case = TRUE)
    authors <- gsub("\\s*[,，]\\s*", ", ", authors)
    # Clean up
    authors <- gsub("\\s+", " ", authors)
    authors <- gsub(",$", "", authors)

    metadata$author <- authors
  }

  # 4. Extract year
  year <- extract_year(text)
  if(!is.null(year)) {
    metadata$year <- year
  }

  # 5. Extract journal name
  # Usually in header/footer or near the top in a specific format
  journal_patterns <- c(
    # Journal abbreviation at the very top (like SLEEPJ)
    "^\\s*([A-Z]{3,}[A-Z0-9]*),?\\s+\\d{4}",          # SLEEPJ, 2020
    # Common journal citation formats
    "^([A-Z][A-Za-z\\s&]+)\\s+\\d+\\s+\\(\\d{4}\\)",  # Journal Name 123 (2024)
    "^([A-Z][A-Za-z\\s&]+)\\s+[Vv]ol\\.?\\s*\\d+",    # Journal Name Vol. 123
    "^([A-Z][A-Za-z\\s&]+)\\s+\\d+:\\d+",              # Journal Name 12:345-678
    # Header/footer patterns
    "^([A-Z][A-Za-z\\s&]+)\\s*/\\s*[A-Za-z]",          # Journal Name / Author
    "^([A-Z][A-Za-z\\s&]+)\\s+\\d+\\s+\\(\\d+\\)"      # Journal Name 186 (2025)
  )

  for(pattern in journal_patterns) {
    for(line in lines[1:min(10, length(lines))]) {
      if(grepl(pattern, line)) {
        journal <- gsub(pattern, "\\1", line)
        journal <- trimws(journal)
        # Make sure it's not too long and doesn't contain weird stuff
        if(nchar(journal) < 100 && !grepl("\\d{4}|DOI|http", journal)) {
          metadata$journal <- journal
          break
        }
      }
    }
    if(!is.null(metadata$journal)) break
  }

  # 6. Extract volume/pages if visible
  vol_pattern <- "[Vv]ol\\.?\\s*(\\d+)"
  if(grepl(vol_pattern, text)) {
    metadata$volume <- gsub(paste0(".*", vol_pattern, ".*"), "\\1",
                            regmatches(text, regexpr(vol_pattern, text)))
  }

  pages_pattern <- "(\\d+)[–-](\\d+)"
  if(grepl(pages_pattern, text)) {
    matches <- regmatches(text, regexpr(pages_pattern, text))
    if(length(matches) > 0) {
      metadata$pages <- gsub("-", "--", matches[1])  # Use en-dash
    }
  }

  return(metadata)
}



#' Tag PDFs for organization
#'
#' @param pdf_files Character vector of PDF filenames or "all"
#' @param tags Character vector of tags (e.g., "methods", "background")
#' @export
tag_pdfs <- function(pdf_files = "all", tags) {
  paths <- get_project_paths()
  proman_data <- read_proman()

  # Initialize PDF metadata if doesn't exist
  if(is.null(proman_data$pdf_metadata)) {
    proman_data$pdf_metadata <- list()
  }

  # Get PDF files
  if(identical(pdf_files, "all")) {
    pdf_files <- list.files(paths$sources, pattern = "\\.pdf$",
                            full.names = FALSE, ignore.case = TRUE)
  }

  # Tag each PDF
  for(pdf in pdf_files) {
    if(is.null(proman_data$pdf_metadata[[pdf]])) {
      proman_data$pdf_metadata[[pdf]] <- list()
    }

    existing_tags <- null_coalesce(proman_data$pdf_metadata[[pdf]]$tags, character(0))
    proman_data$pdf_metadata[[pdf]]$tags <- unique(c(existing_tags, tags))
  }

  write_proman(proman_data)

  cat(paste("✓ Tagged", length(pdf_files), "PDFs with:", paste(tags, collapse = ", "), "\n"))
  invisible(pdf_files)
}


#' Generate bibliography from PDFs with smart metadata extraction
#'
#' Creates a .bib file from PDFs using:
#' 1. Data parsed from the first page of the article
#' 2. DOI lookup via CrossRef (if DOI found)
#' 3. PDF metadata (if available)
#' 4. Filename parsing (fallback - may just remove that)
#'
#' @param pdf_subset Specific PDFs to process (default: all)
#' @param tag Filter by tag (e.g., "methods" for methods papers only)
#' @param show_progress Show progress messages (default TRUE)
#' @export
generate_bibliography <- function(pdf_subset = NULL,
                                  tag = NULL,
                                  show_progress = TRUE) {

  paths <- get_project_paths()
  proman_data <- read_proman()
  project_code <- proman_data$project_code

  # Get PDFs to process
  if(!is.null(pdf_subset)) {
    pdf_files <- file.path(paths$sources, pdf_subset)
  } else if(!is.null(tag)) {
    all_pdfs <- list.files(paths$sources, pattern = "\\.pdf$",
                           full.names = FALSE, ignore.case = TRUE)

    tagged_pdfs <- character()
    for(pdf in all_pdfs) {
      pdf_tags <- null_coalesce(proman_data$pdf_metadata[[pdf]]$tags, character(0))
      if(tag %in% pdf_tags) {
        tagged_pdfs <- c(tagged_pdfs, pdf)
      }
    }
    pdf_files <- file.path(paths$sources, tagged_pdfs)
  } else {
    # Get all PDFs
    pdf_files <- list.files(paths$sources, pattern = "\\.pdf$",
                            full.names = TRUE, ignore.case = TRUE)
  }

  if(length(pdf_files) == 0) {
    cat("No PDFs found in sources folder\n")
    return(NULL)
  }

  cat(paste("📚 Found", length(pdf_files), "PDFs in sources folder\n\n"))

  # Track what worked
  entries <- list()
  summary <- list(doi_crossref = 0, first_page_only = 0, pdf_metadata = 0,
                  needs_attention = character())

  for(i in seq_along(pdf_files)) {
    if(show_progress) {
      cat(paste0("\r[", i, "/", length(pdf_files), "] ",
                 basename(pdf_files[i]), " ... "))
    }

    filename <- tools::file_path_sans_ext(basename(pdf_files[i]))
    metadata <- list()
    extraction_method <- NULL

    # Extract PDF text first (we'll need it for everything)
    pdf_text <- NULL
    tryCatch({
      pdf_text <- pdftools::pdf_text(pdf_files[i])
    }, error = function(e) {
      if(show_progress) cat("PDF read error ... ")
    })

    # Skip if we can't read the PDF
    if(is.null(pdf_text) || length(pdf_text) == 0) {
      summary$needs_attention <- c(summary$needs_attention, basename(pdf_files[i]))
      next
    }

    # STEP 1: Extract all possible info from first page
    first_page_data <- extract_metadata_from_first_page(pdf_text)

    # STEP 2: If DOI found, try CrossRef for complete metadata
    if(!is.null(first_page_data$doi)) {
      if(show_progress) cat("DOI found ... ")

      crossref_data <- NULL
      result <- tryCatch({
        rcrossref::cr_works(dois = first_page_data$doi)
      }, error = function(e) {
        if(show_progress) cat("CrossRef error ... ")
        NULL
      })

      if(!is.null(result$data) && nrow(result$data) > 0) {
        cr_data <- result$data

        # Parse CrossRef authors with better error handling
        authors <- NULL
        if("author" %in% names(cr_data)) {
          # Check if author is a list column
          if(is.list(cr_data$author)) {
            author_data <- cr_data$author[[1]]

            if(!is.null(author_data) && is.data.frame(author_data)) {
              # Author data is a data frame
              author_list <- list()
              for(j in 1:nrow(author_data)) {
                family <- null_coalesce(author_data$family[j], "")
                given <- null_coalesce(author_data$given[j], "")
                if(nchar(family) > 0 || nchar(given) > 0) {
                  author_list[[j]] <- paste(fix_pdf_encoding(family),
                                            fix_pdf_encoding(given), sep = ", ")
                }
              }
              author_list <- author_list[!sapply(author_list, is.null)]
              if(length(author_list) > 0) {
                authors <- paste(author_list, collapse = " and ")
              }
            } else if(is.list(author_data)) {
              # Author data is a list of lists
              author_list <- lapply(author_data, function(a) {
                if(is.list(a)) {
                  family <- fix_pdf_encoding(null_coalesce(a$family, ""))
                  given <- fix_pdf_encoding(null_coalesce(a$given, ""))
                  if(nchar(family) > 0 || nchar(given) > 0) {
                    paste(family, given, sep = ", ")
                  } else {
                    NULL
                  }
                } else {
                  NULL
                }
              })
              author_list <- author_list[!sapply(author_list, is.null)]
              if(length(author_list) > 0) {
                authors <- paste(author_list, collapse = " and ")
              }
            }
          }
        }

        # Build CrossRef metadata with safer column access
        crossref_data <- list(
          title = if("title" %in% names(cr_data) && !is.null(cr_data$title)) {
            fix_pdf_encoding(cr_data$title[[1]])
          } else NULL,
          author = authors,
          year = extract_year(cr_data),
          journal = if("container.title" %in% names(cr_data) && !is.null(cr_data$container.title)) {
            fix_pdf_encoding(cr_data$container.title[[1]])
          } else NULL,
          volume = if("volume" %in% names(cr_data) && !is.null(cr_data$volume)) {
            as.character(cr_data$volume[[1]])
          } else NULL,
          issue = if("issue" %in% names(cr_data) && !is.null(cr_data$issue)) {
            as.character(cr_data$issue[[1]])
          } else NULL,
          pages = if("page" %in% names(cr_data) && !is.null(cr_data$page)) {
            as.character(cr_data$page[[1]])
          } else NULL,
          doi = if("doi" %in% names(cr_data) && !is.null(cr_data$doi)) {
            cr_data$doi[[1]]
          } else if("DOI" %in% names(cr_data) && !is.null(cr_data$DOI)) {
            cr_data$DOI[[1]]
          } else first_page_data$doi,
          publisher = if("publisher" %in% names(cr_data) && !is.null(cr_data$publisher)) {
            cr_data$publisher[[1]]
          } else NULL
        )

        # Merge with first page data, preferring CrossRef for most fields
        # but keeping first page title if it looks better
        metadata$title <- crossref_data$title
        if(!is.null(first_page_data$title) &&
           (is.null(metadata$title) || nchar(first_page_data$title) > nchar(metadata$title))) {
          metadata$title <- first_page_data$title
        }

        metadata$author <- null_coalesce(crossref_data$author, first_page_data$author)
        metadata$year <- null_coalesce(crossref_data$year, first_page_data$year)
        metadata$journal <- null_coalesce(crossref_data$journal, first_page_data$journal)
        metadata$volume <- null_coalesce(crossref_data$volume, first_page_data$volume)
        metadata$issue <- crossref_data$issue
        metadata$pages <- null_coalesce(crossref_data$pages, first_page_data$pages)
        metadata$doi <- crossref_data$doi
        metadata$publisher <- crossref_data$publisher

        summary$doi_crossref <- summary$doi_crossref + 1
        extraction_method <- "doi_crossref"
      } else {
        # CrossRef failed, use first page data only
        metadata <- first_page_data
        summary$first_page_only <- summary$first_page_only + 1
        extraction_method <- "first_page"
      }
    } else {
      # No DOI found, use first page data
      metadata <- first_page_data
      summary$first_page_only <- summary$first_page_only + 1
      extraction_method <- "first_page"
    }

    # STEP 3: If still missing critical info, try PDF metadata as last resort
    if(is.null(metadata$title) || is.null(metadata$author)) {
      tryCatch({
        pdf_info <- pdftools::pdf_info(pdf_files[i])

        if(!is.null(pdf_info$keys)) {
          # Only use PDF metadata if it looks reasonable
          if(is.null(metadata$title)) {
            pdf_title <- fix_pdf_encoding(pdf_info$keys$Title)
            if(!is.null(pdf_title) &&
               nchar(pdf_title) > 5 &&
               !grepl("^Microsoft|^untitled|^document|\\.docx?$|\\.pdf$", pdf_title, ignore.case = TRUE)) {
              metadata$title <- pdf_title
              if(extraction_method == "first_page") {
                extraction_method <- "mixed"
              }
            }
          }

          if(is.null(metadata$author)) {
            pdf_author <- fix_pdf_encoding(pdf_info$keys$Author)
            if(!is.null(pdf_author) &&
               nchar(pdf_author) > 2 &&
               !grepl("^user$|^admin|^unknown", pdf_author, ignore.case = TRUE)) {
              metadata$author <- pdf_author
              if(extraction_method == "first_page") {
                extraction_method <- "mixed"
              }
            }
          }

          # Try to get year from creation date if missing
          if(is.null(metadata$year) && !is.null(pdf_info$keys$CreationDate)) {
            year <- extract_year(pdf_info$keys$CreationDate)
            if(!is.null(year)) {
              metadata$year <- year
            }
          }
        }
      }, error = function(e) {
        # PDF metadata extraction failed
      })

      if(extraction_method == "mixed") {
        summary$pdf_metadata <- summary$pdf_metadata + 1
      }
    }

    # Determine entry type
    entry_type <- "article"  # default

    # Check if it's a book chapter
    if(!is.null(metadata$doi) && grepl("/B978", metadata$doi)) {
      entry_type <- "incollection"
    } else if(!is.null(metadata$journal) &&
              grepl("Handbook|Series|Book|Proceedings", metadata$journal, ignore.case = TRUE)) {
      entry_type <- "incollection"
    } else if(!is.null(first_page_data$title) &&
              grepl("Conference|Symposium|Workshop|Proceedings", first_page_data$title, ignore.case = TRUE)) {
      entry_type <- "inproceedings"
    }

    # Track files that need attention
    if(is.null(metadata$author) || is.null(metadata$year) || is.null(metadata$title)) {
      summary$needs_attention <- c(summary$needs_attention, basename(pdf_files[i]))
    }

    # Generate citation key
    if(!is.null(metadata$author) && !is.null(metadata$year)) {
      first_author <- strsplit(metadata$author, " and ")[[1]][1]

      if(grepl(",", first_author)) {
        author_for_key <- trimws(strsplit(first_author, ",")[[1]][1])
      } else {
        words <- strsplit(trimws(first_author), " ")[[1]]
        author_for_key <- words[length(words)]
      }

      # Clean author name for key
      author_for_key <- iconv(author_for_key, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
      if(is.na(author_for_key) || author_for_key == "") {
        author_for_key <- gsub("[^A-Za-z]", "", first_author)
      }
      author_for_key <- gsub("[^a-zA-Z0-9]", "", author_for_key)

      key <- paste0(author_for_key, metadata$year)
    } else {
      # Use filename as key if no author/year
      key <- gsub("[^a-zA-Z0-9]", "", substring(filename, 1, 30))
    }

    # Build BibTeX entry
    entry <- c(paste0("@", entry_type, "{", key, ","))

    if(!is.null(metadata$title)) {
      entry <- c(entry, paste0("  title = {{", metadata$title, "}},"))
    }
    if(!is.null(metadata$author)) {
      entry <- c(entry, paste0("  author = {", metadata$author, "},"))
    }
    if(!is.null(metadata$year)) {
      entry <- c(entry, paste0("  year = {", metadata$year, "},"))
    }

    # For book chapters, use booktitle instead of journal
    if(entry_type == "incollection" && !is.null(metadata$journal)) {
      entry <- c(entry, paste0("  booktitle = {", metadata$journal, "},"))
    } else if(!is.null(metadata$journal)) {
      entry <- c(entry, paste0("  journal = {", metadata$journal, "},"))
    }

    if(!is.null(metadata$volume)) {
      entry <- c(entry, paste0("  volume = {", metadata$volume, "},"))
    }
    if(!is.null(metadata$issue)) {
      entry <- c(entry, paste0("  number = {", metadata$issue, "},"))
    }
    if(!is.null(metadata$pages)) {
      entry <- c(entry, paste0("  pages = {", metadata$pages, "},"))
    }
    if(!is.null(metadata$doi)) {
      entry <- c(entry, paste0("  doi = {", metadata$doi, "},"))
    }
    if(!is.null(metadata$publisher)) {
      entry <- c(entry, paste0("  publisher = {", metadata$publisher, "},"))
    }

    entry <- c(entry, paste0("  note = {PDF: ", filename, "}"))
    entry[length(entry)] <- gsub(",$", "", entry[length(entry)])
    entry <- c(entry, "}")

    entries[[length(entries) + 1]] <- entry

    if(show_progress) {
      cat(paste("✓", extraction_method, "\n"))
    }
  }

  if(show_progress) cat("\n")

  # Write bibliography
  bib_filename <- paste0("references_", project_code, ".bib")
  bib_path <- file.path(paths$sources, bib_filename)

  writeLines(c(unlist(entries)), bib_path)

  # Show summary
  cat("\n     SUMMARY:\n")
  cat(paste("✓ Complete entries (DOI + CrossRef):", summary$doi_crossref, "\n"))
  cat(paste("   First page extraction only:", summary$first_page_only, "\n"))
  cat(paste("   Used PDF metadata:", summary$pdf_metadata, "\n"))

  # Check for issues
  issues_found <- list()

  for(i in seq_along(entries)) {
    entry_lines <- entries[[i]]
    entry_text <- paste(entry_lines, collapse = " ")

    # Extract filename
    filename <- gsub(".*PDF: ([^}]+).*", "\\1", entry_text)

    # Quick checks for common problems
    has_issues <- FALSE

    if(grepl("title = \\{\\{.*(https?://|Copyright|Editors)", entry_text, ignore.case = TRUE)) {
      has_issues <- TRUE
    }
    if(grepl("author = \\{(and |Analysis and|\\})", entry_text)) {
      has_issues <- TRUE
    }
    if(grepl("doi = \\{10\\.[0-9]+/\\.?\\}", entry_text)) {
      has_issues <- TRUE
    }
    if(!grepl("year = ", entry_text)) {
      has_issues <- TRUE
    }

    if(has_issues) {
      issues_found <- c(issues_found, filename)
    }
  }

  # Simple report
  if(length(issues_found) > 0) {
    cat(paste("\n⚠️  Files needing review:", length(issues_found), "\n"))
    for(file in issues_found) {
      cat(paste("   •", file, "\n"))
    }
    cat("\n Run create_annotation_template() to fix issues or update bibliography\n")
  }

  cat(paste("\n ✓ Created bibliography:", bib_filename, "\n"))
  cat(paste("   Location:", paths$sources, "\n"))
  cat(paste("   Total entries:", length(entries), "\n"))

  return(invisible(bib_path))
}

#' Parse metadata from filename
#'
#' Attempts to extract author, year, title from common filename patterns
#'
#' @param filename Filename without extension
parse_filename_metadata <- function(filename) {
  metadata <- list()

  # Pattern 1: "Author_Year_Title" or "Author-Year-Title"
  if(grepl("^[A-Za-z]+[-_]\\d{4}[-_]", filename)) {
    parts <- strsplit(filename, "[-_]+")[[1]]
    parts <- parts[nchar(parts) > 0]
    metadata$author <- parts[1]
    metadata$year <- parts[2]
    if(length(parts) >= 3) {
      metadata$title <- paste(parts[3:length(parts)], collapse = " ")
    } else {
      metadata$title <- "Unknown"
    }

    # Pattern 2: "Author (Year) Title" or "Author, Year - Title"
  } else if(grepl("\\(\\d{4}\\)", filename)) {
    # Extract year in parentheses
    metadata$year <- gsub(".*\\((\\d{4})\\).*", "\\1", filename)
    # Everything before year is author
    metadata$author <- trimws(gsub("\\(\\d{4}\\).*", "", filename))
    # Everything after year is title
    metadata$title <- trimws(gsub(".*\\(\\d{4}\\)", "", filename))

    # Pattern 3: "Author et al Year"
  } else if(grepl("et al\\.?\\s+\\d{4}", filename)) {
    metadata$author <- gsub("\\s+et al\\.?.*", "", filename)
    metadata$year <- gsub(".*et al\\.?\\s+(\\d{4}).*", "\\1", filename)
    metadata$title <- gsub(".*\\d{4}\\s*[-–]?\\s*", "", filename)

    # Pattern 4: Just extract any 4-digit year
  } else if(grepl("\\d{4}", filename)) {
    metadata$year <- regmatches(filename, regexpr("\\d{4}", filename))
    # Try to guess author (first word that's capitalized)
    words <- strsplit(filename, "[^A-Za-z]+")[[1]]
    cap_words <- words[grepl("^[A-Z]", words)]
    if(length(cap_words) > 0) {
      metadata$author <- cap_words[1]
    }
    metadata$title <- filename
  } else {
    # No pattern matched
    metadata$title <- filename
  }

  # Clean up
  metadata <- lapply(metadata, function(x) {
    x <- gsub("_", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  })

  return(metadata)
}

#' Create a BibTeX entry
#'
#' Formats metadata into proper BibTeX format
#'
#' @param key Citation key
#' @param title Article title
#' @param authors Author names
#' @param year Publication year
#' @param journal Journal name (optional)
#' @param note Additional notes (optional)
create_bibtex_entry <- function(key, title, authors, year, journal = "", note = "") {

  # Clean the key (no spaces or special characters)
  key <- gsub("[^a-zA-Z0-9]", "", key)

  entry <- c(
    paste0("@article{", key, ","),
    paste0("  title = {{", title, "}},"),
    paste0("  author = {", authors, "},"),
    paste0("  year = {", year, "}")
  )

  if(nchar(journal) > 0) {
    entry <- c(entry, paste0(",", "\n", "  journal = {", journal, "}"))
  }

  if(nchar(note) > 0) {
    entry <- c(entry, paste0(",", "\n", "  note = {", note, "}"))
  }

  entry[length(entry)] <- paste0(entry[length(entry)], "\n}")
  entry <- c(entry, "")

  return(entry)
}


# Helper function for string count (to avoid stringr dependency)
str_count <- function(string, pattern) {
  length(regmatches(string, gregexpr(pattern, string))[[1]])
}

#' Update bibliography by merging all sources and incorporating annotations
#'
#' 1. Checks for annotation file and incorporates if found
#' 2. Merges any other .bib files in sources folder
#' 3. Removes duplicates
#' 4. Updates the main bibliography file
#'
#' @export
update_bibliography <- function() {

  paths <- get_project_paths()
  proman_data <- read_proman()
  project_code <- proman_data$project_code

  main_bib_file <- file.path(paths$sources, paste0("references_", project_code, ".bib"))
  annotation_file <- file.path(paths$personal_notes, paste0("annotate_", project_code, ".qmd"))

  # Step 1: Check if annotation file exists and parse annotations
  annotations <- NULL
  if(file.exists(annotation_file)) {
    cat("📝 Found annotation file - parsing annotations...\n")

    content <- readLines(annotation_file)

    # Parse annotations with max line limits to prevent infinite loops
    annotations <- list()
    current_key <- NULL
    in_annotation <- FALSE
    MAX_LINES_PER_FIELD <- 50

    for(i in seq_along(content)) {
      line <- content[i]

      if(grepl("<!-- ANNOTATION START:", line)) {
        current_key <- trimws(gsub("<!-- ANNOTATION START:|-->", "", line))
        in_annotation <- TRUE
        annotations[[current_key]] <- list()
      }

      if(grepl("<!-- ANNOTATION END:", line)) {
        in_annotation <- FALSE
        current_key <- NULL
      }

      if(in_annotation && !is.null(current_key)) {
        # Parse multi-line fields
        if(grepl("<!-- SUMMARY -->", line)) {
          j <- i + 1
          summary_lines <- character()
          lines_read <- 0
          while(j <= length(content) &&
                !grepl("<!-- /SUMMARY -->", content[j]) &&
                lines_read < MAX_LINES_PER_FIELD) {
            if(!grepl("^\\[.*\\]$", content[j])) {
              summary_lines <- c(summary_lines, content[j])
            }
            j <- j + 1
            lines_read <- lines_read + 1
          }
          annotations[[current_key]]$summary <- paste(summary_lines, collapse = " ")

        } else if(grepl("<!-- METHODS -->", line)) {
          j <- i + 1
          methods_lines <- character()
          lines_read <- 0
          while(j <= length(content) &&
                !grepl("<!-- /METHODS -->", content[j]) &&
                lines_read < MAX_LINES_PER_FIELD) {
            if(!grepl("^\\[.*\\]$", content[j])) {
              methods_lines <- c(methods_lines, content[j])
            }
            j <- j + 1
            lines_read <- lines_read + 1
          }
          annotations[[current_key]]$methods <- paste(methods_lines, collapse = " ")

        } else if(grepl("<!-- FINDINGS -->", line)) {
          j <- i + 1
          findings_lines <- character()
          lines_read <- 0
          while(j <= length(content) &&
                !grepl("<!-- /FINDINGS -->", content[j]) &&
                lines_read < MAX_LINES_PER_FIELD) {
            if(!grepl("^\\[.*\\]$", content[j])) {
              findings_lines <- c(findings_lines, content[j])
            }
            j <- j + 1
            lines_read <- lines_read + 1
          }
          annotations[[current_key]]$findings <- paste(findings_lines, collapse = " ")
        }

        # Single line fields
        if(grepl("<!-- RELEVANCE -->", line)) {
          value <- gsub(".*<!-- RELEVANCE -->\\s*\\[?([^\\]]+)\\]?.*<!-- /RELEVANCE -->.*", "\\1", line)
          annotations[[current_key]]$relevance <- trimws(value)
          }
      }
    }

    cat(paste("  Found annotations for", length(annotations), "entries\n"))
  }

  # Add this section to update_bibliography() after the annotation file check:

  # Step 1.5: Check for reading notes and extract annotations
  reading_dir <- file.path(paths$personal_notes, "reading")
  if(dir.exists(reading_dir)) {
    cat("📚 Found reading notes directory - extracting annotations...\n")

    note_files <- list.files(reading_dir, pattern = "\\.md$", full.names = TRUE)
    reading_annotations <- list()

    for(note_file in note_files) {
      content <- readLines(note_file, warn = FALSE)

      # Get citation key
      citation_line <- grep("\\*\\*Citation key:\\*\\*", content, value = TRUE)
      if(length(citation_line) == 0) next

      citation_key <- gsub(".*@([a-zA-Z0-9]+).*", "\\1", citation_line[1])

      # Extract summary from the Summary section
      summary_start <- grep("^## Summary", content)
      if(length(summary_start) > 0) {
        # Look for filled content (not template placeholders)
        summary_end <- grep("^##", content[(summary_start[1]+1):length(content)])[1]
        if(!is.na(summary_end)) {
          summary_lines <- content[(summary_start[1]+1):(summary_start[1]+summary_end-1)]
          # Filter out template placeholders
          summary_lines <- summary_lines[!grepl("^\\[.*\\]$", summary_lines)]
          summary_lines <- summary_lines[nchar(trimws(summary_lines)) > 0]

          if(length(summary_lines) > 0) {
            reading_annotations[[citation_key]] <- list()
            reading_annotations[[citation_key]]$summary <- paste(summary_lines, collapse = " ")
          }
        }
      }

    # Merge with existing annotations
    if(length(reading_annotations) > 0) {
      cat(paste("  Found annotations in", length(reading_annotations), "reading notes\n"))
      if(is.null(annotations)) {
        annotations <- reading_annotations
      } else {
        # Merge, preferring reading notes for conflicts
        for(key in names(reading_annotations)) {
          annotations[[key]] <- reading_annotations[[key]]
        }
      }
    }
    }
  }

  # Step 2: Collect all .bib files in sources folder
  all_bib_files <- list.files(paths$sources, pattern = "\\.bib$", full.names = TRUE)

  # Step 3: Read and merge all entries
  all_entries <- list()
  all_keys <- character()

  for(bib_file in all_bib_files) {
    cat(paste("📚 Reading:", basename(bib_file), "\n"))

    content <- readLines(bib_file)
    entries <- extract_bib_entries(content)

    # For the main file, we need to preserve structure for annotation insertion
    if(bib_file == main_bib_file && !is.null(annotations)) {
      # Process main file with annotations
      updated_content <- character()
      i <- 1

      while(i <= length(content)) {
        line <- content[i]

        if(grepl("^@", line)) {
          # Extract key
          key <- gsub("@\\w+\\{([^,]+),.*", "\\1", line)

          # Start building entry
          entry_lines <- c(line)
          i <- i + 1

          # Collect all lines of this entry
          brace_count <- 1
          original_lines <- character()

          while(i <= length(content) && brace_count > 0) {
            if(grepl("\\{", content[i])) {
              # Count opening braces
              brace_count <- brace_count + nchar(gsub("[^{]", "", content[i]))
            }
            if(grepl("\\}", content[i])) {
              # Count closing braces
              brace_count <- brace_count - nchar(gsub("[^}]", "", content[i]))
            }

            if(brace_count > 0) {
              original_lines <- c(original_lines, content[i])
            }
            i <- i + 1
          }


          # Check if we have annotations for this key
          if(key %in% names(annotations)) {
            ann <- annotations[[key]]

            # Remove any existing annotation fields to avoid duplicates
            original_lines <- original_lines[!grepl("^\\s*(relevance|annote|methods|findings)\\s*=\\s*\\{",
                                                    original_lines, ignore.case = TRUE)]

            # Add annotation fields before closing
            if(!is.null(ann$relevance) && nchar(ann$relevance) > 0) {
              original_lines <- c(original_lines, paste0("  relevance = {", ann$relevance, "},"))
            }
            if(!is.null(ann$summary) && nchar(ann$summary) > 0) {
              original_lines <- c(original_lines, paste0("  annote = {", ann$summary, "},"))
            }
            if(!is.null(ann$methods) && nchar(ann$methods) > 0) {
              original_lines <- c(original_lines, paste0("  methods = {", ann$methods, "},"))
            }
            if(!is.null(ann$findings) && nchar(ann$findings) > 0) {
              original_lines <- c(original_lines, paste0("  findings = {", ann$findings, "},"))
            }
          }

          # Build complete entry
          entry_lines <- c(entry_lines, original_lines, "}")

          # Add to collection if not duplicate
          if(!(key %in% all_keys)) {
            all_entries[[length(all_entries) + 1]] <- entry_lines
            all_keys <- c(all_keys, key)
          }
        } else {
          # Skip non-entry lines in individual files
          i <- i + 1
        }
      }
    } else {
      # For other .bib files, just add new entries
      for(j in seq_along(entries$keys)) {
        if(!(entries$keys[j] %in% all_keys)) {
          all_entries[[length(all_entries) + 1]] <- entries$entries[[j]]
          all_keys <- c(all_keys, entries$keys[j])
        }
      }
    }
  }

  # Step 4: Write the updated bibliography
  header <- c(
    paste("% Bibliography for project:", project_code),
    paste("% Last updated:", Sys.Date()),
    paste("% Total entries:", length(all_entries)),
    ifelse(!is.null(annotations),
           paste("% Includes annotations from Quarto document"),
           ""),
    "",
    ""
  )

  output_content <- header
  for(entry in all_entries) {
    output_content <- c(output_content, entry, "")
  }

  writeLines(output_content, main_bib_file)

  # Step 5: Summary report
  cat("\n✓ Bibliography updated successfully\n")
  cat(paste(" > Location:", paths$sources, "\n"))
  cat(paste(" > Total entries:", length(all_entries), "\n"))

  if(!is.null(annotations)) {
    cat(paste(" > Annotated entries:", length(annotations), "\n"))
  }

  # Count how many entries have each type of metadata
  final_content <- readLines(main_bib_file)
  n_with_doi <- length(grep("doi\\s*=", final_content, ignore.case = TRUE))
  n_with_annote <- length(grep("annote\\s*=", final_content, ignore.case = TRUE))

  cat(paste(" > Entries with DOI:", n_with_doi, "\n"))

  # Clean up other .bib files if requested
  if(length(all_bib_files) > 1 && interactive()) {
    cat("\n")
    other_bibs <- all_bib_files[all_bib_files != main_bib_file]
    cat(paste("Found", length(other_bibs), "other .bib files that have been merged.\n"))
    cat("Delete these files? (y/n): ")
    if(tolower(readline()) == "y") {
      file.remove(other_bibs)
      cat("✓ Cleaned up merged files\n")
    }
  }

  return(invisible(main_bib_file))

}

#' Find missing DOIs and update bibliography
#'
#' @export
find_missing_dois <- function() {

  paths <- get_project_paths()
  proman_data <- read_proman()
  project_code <- proman_data$project_code

  bib_file <- file.path(paths$sources, paste0("references_", project_code, ".bib"))

  if(!file.exists(bib_file)) {
    cat("No bibliography found. Run generate_bibliography() first.\n")
    return(invisible(NULL))
  }

  bib_content <- readLines(bib_file)
  entries <- parse_bib_entries(bib_content)

  found_count <- 0

  cat("🔍 Searching for missing DOIs...\n\n")

  for(i in seq_along(entries)) {
    entry <- entries[[i]]

    # Skip if already has DOI
    if(any(grepl("doi\\s*=", bib_content[entry$start:entry$end], ignore.case = TRUE))) {
      next
    }

    # Try to find DOI using title
    if(!is.null(entry$title) && nchar(entry$title) > 10) {
      cat(paste("Searching:", substr(entry$title, 1, 50), "... "))

      result <- tryCatch({
        rcrossref::cr_works(query = entry$title, limit = 1)
      }, error = function(e) NULL)

      if(!is.null(result$data) && nrow(result$data) > 0) {
        # Check if title is similar enough
        cr_title <- tolower(result$data$title[[1]])
        entry_title <- tolower(entry$title)

        # Simple similarity check
        if(grepl(substr(entry_title, 1, 20), cr_title, fixed = TRUE) ||
           grepl(substr(cr_title, 1, 20), entry_title, fixed = TRUE)) {

          doi <- result$data$DOI[[1]]
          cat(paste("✓ Found:", doi, "\n"))

          # Add DOI to entry
          # [Insert DOI into bib file logic]

          found_count <- found_count + 1
        } else {
          cat("✗ Title mismatch\n")
        }
      } else {
        cat("✗ Not found\n")
      }
    }
  }

  cat(paste("\n✅ Found DOIs for", found_count, "entries\n"))
}

#' Suggest PDF renames based on metadata
#'
#' @param apply If TRUE, apply the renames (default FALSE shows suggestions only)
#' @export
rename_pdfs <- function(apply = FALSE) {

  paths <- get_project_paths()
  proman_data <- read_proman()
  project_code <- proman_data$project_code

  # Generate bibliography to get metadata
  bib_file <- file.path(paths$sources, paste0("references_", project_code, ".bib"))

  if(!file.exists(bib_file)) {
    cat("Generating bibliography first...\n")
    generate_bibliography(show_progress = FALSE)
  }

  # Parse bibliography
  bib_content <- readLines(bib_file)
  entries <- parse_bib_entries(bib_content)

  suggestions <- list()

  for(entry in entries) {
    # Get current filename from note field
    note_lines <- grep("note.*PDF:", bib_content[entry$start:entry$end], value = TRUE)
    if(length(note_lines) > 0) {
      current_name <- gsub(".*PDF:\\s*([^}]+).*", "\\1", note_lines[1])
      current_name <- paste0(current_name, ".pdf")

      # Generate clean name
      if(!is.null(entry$authors) && !is.null(entry$year)) {
        first_author <- strsplit(entry$authors, " and ")[[1]][1]
        if(grepl(",", first_author)) {
          last_name <- trimws(strsplit(first_author, ",")[[1]][1])
        } else {
          words <- strsplit(first_author, " ")[[1]]
          last_name <- words[length(words)]
        }

        last_name <- gsub("[^A-Za-z]", "", last_name)

        # Shorten title
        if(!is.null(entry$title)) {
          title_words <- unlist(strsplit(entry$title, " "))
          # Remove common words
          title_words <- title_words[!tolower(title_words) %in%
                                       c("the", "a", "an", "of", "in", "on", "at", "to", "for", "and")]
          short_title <- paste(title_words[1:min(3, length(title_words))], collapse = "_")
          short_title <- gsub("[^A-Za-z0-9_]", "", short_title)
        } else {
          short_title <- "Unknown"
        }

        suggested <- paste0(last_name, "_", entry$year, "_", short_title, ".pdf")

        # Only suggest if different
        if(current_name != suggested) {
          suggestions[[current_name]] <- suggested
        }
      }
    }
  }

  if(length(suggestions) == 0) {
    cat("✅ All PDFs already have good names!\n")
    return(invisible(NULL))
  }

  cat("📄 PDF RENAMING SUGGESTIONS\n")
  cat(paste(rep("-", 60), collapse = ""), "\n\n")

  for(i in seq_along(suggestions)) {
    cat(paste("Current: ", names(suggestions)[i], "\n"))
    cat(paste("Suggest: ", suggestions[[i]], "\n\n"))
  }

  if(apply && interactive()) {
    cat("Apply these renames? (y/n): ")
    if(tolower(readline()) == "y") {
      for(i in seq_along(suggestions)) {
        file.rename(
          file.path(paths$sources, names(suggestions)[i]),
          file.path(paths$sources, suggestions[[i]])
        )
      }
      cat("✅ Files renamed\n")
      cat("⚠️  Remember to run generate_bibliography() again to update references\n")
    }
  } else if(apply && !interactive()) {
    # Non-interactive mode
    for(i in seq_along(suggestions)) {
      file.rename(
        file.path(paths$sources, names(suggestions)[i]),
        file.path(paths$sources, suggestions[[i]])
      )
    }
    cat("✅ Files renamed\n")
  }

  invisible(suggestions)
}

#' Export bibliography to other formats
#'
#' @param format Output format: "ris", "csv", or "endnote"
#' @export
translate_bibliography <- function(format = c("ris", "csv", "endnote")) {

  format <- match.arg(format)

  paths <- get_project_paths()
  proman_data <- read_proman()
  project_code <- proman_data$project_code

  bib_file <- file.path(paths$sources, paste0("references_", project_code, ".bib"))

  if(!file.exists(bib_file)) {
    cat("No bibliography found. Run generate_bibliography() first.\n")
    return(invisible(NULL))
  }

  # Parse bibliography
  bib_content <- readLines(bib_file)
  entries <- parse_bib_entries(bib_content)

  if(format == "csv") {
    # Create data frame
    df <- data.frame(
      authors = character(length(entries)),
      year = character(length(entries)),
      title = character(length(entries)),
      journal = character(length(entries)),
      doi = character(length(entries)),
      stringsAsFactors = FALSE
    )

    for(i in seq_along(entries)) {
      df$authors[i] <- null_coalesce(entries[[i]]$authors, "")
      df$year[i] <- null_coalesce(entries[[i]]$year, "")
      df$title[i] <- null_coalesce(entries[[i]]$title, "")
      df$journal[i] <- null_coalesce(entries[[i]]$journal, "")
      df$doi[i] <- null_coalesce(entries[[i]]$doi, "")
    }

    output_file <- file.path(paths$sources, paste0("references_", project_code, ".csv"))
    write.csv(df, output_file, row.names = FALSE)

  } else if(format == "ris") {
    # Convert to RIS format
    ris_content <- character()

    for(entry in entries) {
      ris_content <- c(ris_content, "TY  - JOUR")
      if(!is.null(entry$authors)) {
        authors <- strsplit(entry$authors, " and ")[[1]]
        for(author in authors) {
          ris_content <- c(ris_content, paste("AU  -", author))
        }
      }
      if(!is.null(entry$title)) ris_content <- c(ris_content, paste("TI  -", entry$title))
      if(!is.null(entry$journal)) ris_content <- c(ris_content, paste("JO  -", entry$journal))
      if(!is.null(entry$year)) ris_content <- c(ris_content, paste("PY  -", entry$year))
      if(!is.null(entry$doi)) ris_content <- c(ris_content, paste("DO  -", entry$doi))
      ris_content <- c(ris_content, "ER  -", "")
    }

    output_file <- file.path(paths$sources, paste0("references_", project_code, ".ris"))
    writeLines(ris_content, output_file)
  }

  cat(paste("✓ Exported to:", format, "\n"))
  cat(paste(" > File:", output_file, "\n"))

  invisible(output_file)
}

#' Parse BibTeX entries helper function
#'
#' Enhanced parser that includes start/end positions
#'
#' @param bib_content Lines from .bib file
parse_bib_entries <- function(bib_content) {
  entries <- list()
  entry_starts <- grep("^@", bib_content)

  for(i in seq_along(entry_starts)) {
    start <- entry_starts[i]
    end <- ifelse(i < length(entry_starts),
                  entry_starts[i+1] - 1,
                  length(bib_content))

    entry_lines <- bib_content[start:end]

    # Extract key fields
    key <- gsub("^@\\w+\\{([^,]+),?.*", "\\1", entry_lines[1])

    # Extract other fields
    title <- extract_bib_field(entry_lines, "title")
    authors <- extract_bib_field(entry_lines, "author")
    year <- extract_bib_field(entry_lines, "year")
    journal <- extract_bib_field(entry_lines, "journal")
    doi <- extract_bib_field(entry_lines, "doi")

    entries[[i]] <- list(
      key = key,
      title = title,
      authors = authors,
      year = year,
      journal = journal,
      doi = doi,
      start = start,  # Add start position
      end = end       # Add end position
    )
  }

  return(entries)
}

#' Extract field from BibTeX entry
#'
#' @param entry_lines Lines of a single BibTeX entry
#' @param field Field name to extract
extract_bib_field <- function(entry_lines, field) {
  # First try single-line pattern
  field_pattern <- paste0("^\\s*", field, "\\s*=\\s*\\{([^}]*)\\},?$")
  field_line <- grep(field_pattern, entry_lines, ignore.case = TRUE, value = TRUE)

  if(length(field_line) > 0) {
    value <- gsub(field_pattern, "\\1", field_line[1])
    # Remove extra braces
    value <- gsub("^\\{|\\}$", "", value)
    return(trimws(value))
  }

  # Try multi-line pattern
  field_start <- grep(paste0("^\\s*", field, "\\s*=\\s*\\{"), entry_lines, ignore.case = TRUE)
  if(length(field_start) > 0) {
    # Find closing brace
    value_lines <- character()
    brace_count <- 1
    i <- field_start[1]

    # Get first line content after opening brace
    first_line <- sub(paste0("^\\s*", field, "\\s*=\\s*\\{"), "", entry_lines[i], ignore.case = TRUE)
    if(nchar(first_line) > 0) {
      value_lines <- c(value_lines, first_line)
    }

    i <- i + 1
    while(i <= length(entry_lines) && brace_count > 0) {
      line <- entry_lines[i]

      # Count opening and closing braces
      open_braces <- length(regmatches(line, gregexpr("\\{", line))[[1]])
      close_braces <- length(regmatches(line, gregexpr("\\}", line))[[1]])
      brace_count <- brace_count + open_braces - close_braces

      if(brace_count > 0) {
        value_lines <- c(value_lines, line)
      } else {
        # Remove trailing }
        line <- sub("\\}.*$", "", line)
        if(nchar(line) > 0) {
          value_lines <- c(value_lines, line)
        }
      }
      i <- i + 1
    }

    value <- paste(value_lines, collapse = " ")
    value <- gsub("^\\{|\\}$", "", value)
    return(trimws(value))
  }

  return(NULL)
}

# Helper function to extract entries more robustly
extract_bib_entries <- function(content) {
  entries <- list()
  keys <- character()
  entry_starts <- grep("^@", content)

  for(i in seq_along(entry_starts)) {
    start <- entry_starts[i]

    # Find end of this entry
    if(i < length(entry_starts)) {
      end <- entry_starts[i + 1] - 1
    } else {
      end <- length(content)
    }

    # Extract the complete entry
    entry <- content[start:end]

    # Remove trailing empty lines
    while(length(entry) > 0 && nchar(trimws(entry[length(entry)])) == 0) {
      entry <- entry[-length(entry)]
    }

    # Extract key
    key <- gsub("@\\w+\\{([^,]+),.*", "\\1", entry[1])

    entries[[i]] <- entry
    keys[i] <- key
  }

  return(list(entries = entries, keys = keys))
}

#' Create annotated bibliography template in Quarto
#'
#' @param include_pdfs Link to PDFs if available
#' @param only_incomplete Only include entries that need attention
#' @export
annotated_bibliography <- function(include_pdfs = TRUE, only_incomplete = FALSE) {

  paths <- get_project_paths()
  proman_data <- read_proman()
  project_code <- proman_data$project_code

  # Check for bibliography
  bib_file <- file.path(paths$sources, paste0("references_", project_code, ".bib"))
  if(!file.exists(bib_file)) {
    cat("No bibliography found. Generating from PDFs first...\n")
    generate_bibliography()
  }

  # Parse bibliography
  bib_content <- readLines(bib_file)
  entries <- parse_bib_entries(bib_content)

  # Filter if requested
  if(only_incomplete) {
    # Check which entries need attention
    incomplete <- list()
    for(i in seq_along(entries)) {
      entry <- entries[[i]]
      # Check for common issues
      if(is.null(entry$authors) || is.null(entry$year) || is.null(entry$title) ||
         entry$title == "Unknown" ||
         grepl("https?://|Copyright|Editors", entry$title, ignore.case = TRUE) ||
         grepl("^and |Analysis and", entry$authors)) {
        incomplete[[length(incomplete) + 1]] <- entry
      }
    }
    entries <- incomplete
  }

  # Create Quarto template
  template <- c(
    "---",
    paste0("title: \"Annotated Bibliography: ", project_code, "\""),
    paste0("date: \"", Sys.Date(), "\""),
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 2",
    "---",
    "",
    "# Instructions",
    "",
    "Fill out the annotation fields for each reference below. When complete, run `update_bibliography()` to incorporate your corrections and notes into the .bib file.",
    "",
    "## Annotation Guidelines",
    "",
    "- **Relevance**: high/medium/low - How important is this for your project?",
    "- **Summary**: One sentence describing the paper's main contribution",
    "- **Methods Notes**: Key methodological approaches",
    "- **Key Findings**: Main results or insights relevant to your work",
    "",
    "You can also fix any errors in the title, authors, or year fields.",
    "",
    "---",
    "",
    "# References to Annotate",
    ""
  )

  # Add each entry
  for(i in seq_along(entries)) {
    entry <- entries[[i]]

    template <- c(template,
                  paste0("## ", i, ". @", entry$key),
                  "",
                  paste0("**Title:** ", null_coalesce(entry$title, "[Missing title]")),
                  paste0("**Authors:** ", null_coalesce(entry$authors, "[Missing authors]")),
                  paste0("**Year:** ", null_coalesce(entry$year, "[Missing year]"))
    )

    # Add journal if present
    if(!is.null(entry$journal)) {
      template <- c(template, paste0("**Journal:** ", entry$journal))
    }

    # Add PDF link if available
    if(include_pdfs) {
      # Try to find matching PDF from note field
      note_line <- grep("note.*PDF:", bib_content[entry$start:entry$end], value = TRUE)
      if(length(note_line) > 0) {
        pdf_name <- gsub(".*PDF:\\s*([^}]+).*", "\\1", note_line[1])
        template <- c(template,
                      paste0("**PDF:** [", pdf_name, ".pdf](../sources/", pdf_name, ".pdf)")
        )
      }
    }

    # Add annotation fields with HTML comments for parsing
    template <- c(template,
                  "",
                  "### Annotations",
                  "",
                  "<!-- ANNOTATION START: ", entry$key, " -->",
                  "",
                  "**Relevance:** <!-- RELEVANCE --> [medium] <!-- /RELEVANCE -->",
                  "",
                  "**Summary:** <!-- SUMMARY -->",
                  "[One sentence summary here]",
                  "<!-- /SUMMARY -->",
                  "",
                  "**Methods Notes:** <!-- METHODS -->",
                  "[Relevant methodological details]",
                  "<!-- /METHODS -->",
                  "",
                  "**Key Findings:** <!-- FINDINGS -->",
                  "[Main findings relevant to your work]",
                  "<!-- /FINDINGS -->",
                  "",
                  "<!-- ANNOTATION END: ", entry$key, " -->",
                  "",
                  "---",
                  ""
    )
  }

  # Add completion section
  template <- c(template,
                "# Annotation Summary",
                "",
                paste0("Total references: ", length(entries)),
                "",
                "Run `update_bibliography()` to:",
                "- Fix any errors in titles/authors",
                "- Add your annotations to the .bib file",
                "- Merge any other .bib files in the sources folder",
                ""
  )

  # Save template
  output_file <- paste0("annotate_", project_code, ".qmd")
  output_path <- file.path(paths$personal_notes, output_file)

  writeLines(template, output_path)

  cat(paste("✓ Created annotation template:", output_file, "\n"))
  cat(paste("   Location:", paths$personal_notes, "\n"))
  cat(paste("   References to annotate:", length(entries), "\n"))

  if(only_incomplete) {
    cat("   (Showing only entries that need attention)\n")
  }

  cat("\nOpen this file in RStudio/VS Code to add your annotations\n")

  return(output_path)
}

#' Check bibliography status
#'
#' @export
check_bibliography <- function() {

  paths <- get_project_paths()
  proman_data <- read_proman()
  project_code <- proman_data$project_code

  cat("   BIBLIOGRAPHY STATUS\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  auto_bib <- file.path(paths$sources, paste0("references_", project_code, ".bib"))

  if(file.exists(auto_bib)) {
    bib_content <- readLines(auto_bib)
    n_refs <- length(grep("^@", bib_content))
    cat(paste("✓ Bibliography file:", n_refs, "references\n"))
    cat(paste("  Location:", auto_bib, "\n"))

    # Check for annotations
    n_annotated <- length(grep("annote\\s*=", bib_content))
    if(n_annotated > 0) {
      cat(paste("  Annotated:", n_annotated, "entries\n"))
    }

    # Check for DOIs
    n_dois <- length(grep("doi\\s*=", bib_content))
    cat(paste("  With DOIs:", n_dois, "entries\n"))
  } else {
    cat("✗ No bibliography found\n")
    cat("  Run generate_bibliography() to create\n")
  }

  # Check for annotation template
  annotation_file <- file.path(paths$personal_notes, paste0("annotate_", project_code, ".qmd"))
  if(file.exists(annotation_file)) {
    cat("\n✓ Annotation template exists\n")
    cat(paste("  Location:", annotation_file, "\n"))
  }

  # Count PDFs
  pdf_count <- length(list.files(paths$sources, pattern = "\\.pdf$", ignore.case = TRUE))
  cat(paste("\n   PDFs in sources folder:", pdf_count, "\n"))

  if(pdf_count > 0 && !file.exists(auto_bib)) {
    cat("\nNext step: Run generate_bibliography()\n")
  } else if(file.exists(auto_bib) && !file.exists(annotation_file)) {
    cat("\nNext step: Run create_annotation_template() to review/annotate\n")
  }
}

#' Create reading note from PDF with automatic bibliography entry
#'
#' Creates a structured reading note in personal_notes/reading/ with auto-extracted
#' bibliography information from the PDF
#'
#' @param pdf_file PDF filename in sources folder (or "select" to choose interactively)
#' @param project_code Optional project code to link this note to
#' @param open_note Open the note after creation (default TRUE)
#' @export
create_pdf_reading_note <- function(pdf_file = "select", project_code = NULL, open_note = TRUE) {

  paths <- get_project_paths()
  proman_data <- read_proman()

  # Use current project code if not specified
  if(is.null(project_code)) {
    project_code <- proman_data$project_code
  }

  # Ensure reading notes directory exists
  reading_dir <- file.path(paths$personal_notes, "reading")
  if(!dir.exists(reading_dir)) {
    dir.create(reading_dir, showWarnings = FALSE)
    message("Created reading notes directory")
  }

  # Select PDF if not specified
  if(pdf_file == "select") {
    pdf_files <- list.files(paths$sources, pattern = "\\.pdf$", full.names = FALSE)
    if(length(pdf_files) == 0) {
      stop("No PDFs found in sources folder")
    }

    cat("Available PDFs:\n")
    for(i in seq_along(pdf_files)) {
      cat(paste0("[", i, "] ", pdf_files[i], "\n"))
    }
    cat("\nSelect PDF number: ")
    selection <- as.numeric(readline())

    if(selection < 1 || selection > length(pdf_files)) {
      stop("Invalid selection")
    }

    pdf_file <- pdf_files[selection]
  }

  pdf_path <- file.path(paths$sources, pdf_file)

  if(!file.exists(pdf_path)) {
    stop(paste("PDF not found:", pdf_file))
  }

  cat("📄 Extracting metadata from PDF...\n")

  # Use existing bibliography generation for this single PDF
  # This leverages all your sophisticated extraction logic
  temp_bib <- tempfile(fileext = ".bib")

  # Temporarily redirect the bibliography output
  original_bib <- file.path(paths$sources, paste0("references_", project_code, ".bib"))

  # Generate bibliography entry for just this PDF
  generate_bibliography(pdf_subset = pdf_file, show_progress = FALSE)

  # Read the generated entry
  if(file.exists(original_bib)) {
    bib_content <- readLines(original_bib)

    # Find the entry for this PDF
    # Look for the note field that contains the PDF filename
    pdf_marker <- paste0("PDF: ", tools::file_path_sans_ext(pdf_file))
    entry_lines <- character()
    in_entry <- FALSE
    citation_key <- NULL

    for(line in bib_content) {
      if(grepl("^@", line)) {
        # Start of new entry
        if(in_entry && length(entry_lines) > 0 && any(grepl(pdf_marker, entry_lines))) {
          # This was our entry
          break
        }
        # Extract citation key
        key <- gsub("@\\w+\\{([^,]+),.*", "\\1", line)
        entry_lines <- c(line)
        in_entry <- TRUE
        citation_key <- key
      } else if(in_entry) {
        entry_lines <- c(entry_lines, line)
        if(grepl("^\\}$", line)) {
          # End of entry
          if(any(grepl(pdf_marker, entry_lines))) {
            # This is our entry
            break
          }
          in_entry <- FALSE
          entry_lines <- character()
        }
      }
    }

    # Parse the entry to extract metadata
    metadata <- list()
    if(length(entry_lines) > 0) {
      metadata$citation_key <- citation_key
      metadata$title <- extract_bib_field(entry_lines, "title")
      metadata$author <- extract_bib_field(entry_lines, "author")
      metadata$year <- extract_bib_field(entry_lines, "year")
      metadata$journal <- extract_bib_field(entry_lines, "journal")
      metadata$doi <- extract_bib_field(entry_lines, "doi")
      metadata$bib_entry <- paste(entry_lines, collapse = "\n")
    }
  } else {
    stop("Could not generate bibliography entry")
  }

  # Create reading note content
  note_content <- c(
    paste0("# ", null_coalesce(metadata$title, "Reading Note")),
    "",
    paste0("**Date:** ", Sys.Date()),
    paste0("**PDF:** [[", pdf_file, "]](../../sources/", pdf_file, ")"),
    paste0("**Project:** [[", project_code, "]]"),
    paste0("**Citation key:** @", null_coalesce(metadata$citation_key, "unknown")),
    "",
    "## Bibliographic Information",
    "",
    paste0("- **Authors:** ", null_coalesce(metadata$author, "[Unknown]")),
    paste0("- **Year:** ", null_coalesce(metadata$year, "[Unknown]")),
    paste0("- **Journal:** ", null_coalesce(metadata$journal, "[Unknown]")),
    if(!is.null(metadata$doi)) paste0("- **DOI:** [", metadata$doi, "](https://doi.org/", metadata$doi, ")"),
    "",
    "```bibtex",
    null_coalesce(metadata$bib_entry, ""),
    "```",
    "",
    "---",
    "",
    "## Summary",
    "",
    "### Main Question/Purpose",
    "[What is the paper trying to answer or achieve?]",
    "",
    "### Key Methods",
    "[How did they approach the problem?]",
    "",
    "### Main Findings",
    "[What were the key results?]",
    "",
    "### Significance",
    "[Why does this matter to the field?]",
    "",
    "---",
    "",
    "## Relevance to My Work",
    "",
    "### Connection to Project ", project_code,
    "[How does this relate to my current project?]",
    "",
    "### Useful Methods/Approaches",
    "- [ ] [Method that could be adapted]",
    "",
    "---",
    "",
    "### Strengths",
    "- ",
    "",
    "### Limitations",
    "- ",
    "",
    "### Questions/Concerns",
    "- ",
    "",
    "---",
    "",
    "## Follow-up Actions",
    "",
    "---",
    "",
    "## Tags",
    "",
    "#reading-note #", project_code, " #",
    ""
  )

  # Create filename
  safe_key <- gsub("[^A-Za-z0-9-]", "", null_coalesce(metadata$citation_key, "unknown"))
  note_filename <- paste0(safe_key, "_", format(Sys.Date(), "%Y%m%d"), ".md")
  note_path <- file.path(reading_dir, note_filename)

  # Check if note already exists
  if(file.exists(note_path)) {
    cat("Note already exists for this paper. Overwrite? (y/n): ")
    if(tolower(readline()) != "y") {
      return(invisible(NULL))
    }
  }

  writeLines(note_content, note_path)

  # Update .proman to track reading notes
  if(is.null(proman_data$reading_notes)) {
    proman_data$reading_notes <- list()
  }

  proman_data$reading_notes[[safe_key]] <- list(
    pdf_file = pdf_file,
    note_file = note_filename,
    created_date = as.character(Sys.Date()),
    project_code = project_code,
    citation_key = metadata$citation_key
  )

  write_proman(proman_data)

  cat("\n✅ Created reading note!n")
  cat(paste("   Location: personal_notes/reading/", note_filename, "\n", sep = ""))
  cat(paste("   Citation: @", safe_key, "\n", sep = ""))

  # Log this activity
  log_blitz_activity("Reading Note Created", project_code,
                     notes = paste("PDF:", pdf_file))

  # Open in editor if requested
  if(open_note && interactive()) {
    file.edit(note_path)
  }

  return(invisible(list(
    note_path = note_path,
    citation_key = safe_key,
    metadata = metadata
  )))
}


#' Review and compile reading notes
#'
#' Shows summary of reading notes
#'
#' @param days_back Number of days to look back (NULL for all)
#' @param project_filter Filter by project code
#' @export
review_reading_notes <- function(days_back = NULL, project_filter = NULL) {

  paths <- get_project_paths()
  proman_data <- read_proman()

  reading_dir <- file.path(paths$personal_notes, "reading")

  if(!dir.exists(reading_dir)) {
    cat("No reading notes found.\n")
    return(invisible(NULL))
  }

  note_files <- list.files(reading_dir, pattern = "\\.md$", full.names = TRUE)

  if(length(note_files) == 0) {
    cat("No reading notes found.\n")
    return(invisible(NULL))
  }

  # Filter by date if requested
  if(!is.null(days_back)) {
    file_info <- file.info(note_files)
    note_files <- note_files[file_info$mtime >= (Sys.Date() - days_back)]
  }

  cat("📚 READING NOTES REVIEW\n")
  cat(paste(rep("-", 50), collapse = ""), "\n\n")

  # Parse each note for summary
  notes_data <- list()

  for(i in seq_along(note_files)) {
    content <- readLines(note_files[i], warn = FALSE)

    # Extract key information
    title_line <- grep("^# ", content, value = TRUE)[1]
    title <- gsub("^# ", "", title_line)

    project_line <- grep("\\*\\*Project:\\*\\*", content, value = TRUE)
    project <- if(length(project_line) > 0) {
      gsub(".*\\[\\[(.*)\\]\\].*", "\\1", project_line[1])
    } else NA

    citation_line <- grep("\\*\\*Citation key:\\*\\*", content, value = TRUE)
    citation <- if(length(citation_line) > 0) {
      gsub(".*@([a-zA-Z0-9]+).*", "\\1", citation_line[1])
    } else NA

    # Check if has substantial content (more than just template)
    has_content <- any(grepl("^\\[[^\\]]+[^\\]]\\]", content))  # Filled brackets

    notes_data[[i]] <- list(
      file = basename(note_files[i]),
      path = note_files[i],
      title = title,
      project = project,
      citation = citation,
      has_content = has_content,
      modified = file.info(note_files[i])$mtime
    )
  }

  # Convert to data frame for easier filtering
  notes_df <- do.call(rbind, lapply(notes_data, as.data.frame))

  # Apply project filter if specified
  if(!is.null(project_filter)) {
    notes_df <- notes_df[notes_df$project == project_filter, ]
  }

  # Summary statistics
  cat(paste("Total reading notes:", nrow(notes_df), "\n"))
  cat(paste("With content:", sum(notes_df$has_content), "\n"))

  if(!is.null(project_filter)) {
    cat(paste("Filtered to project:", project_filter, "\n"))
  }

  cat("\n")

  # Show recent notes
  recent <- notes_df[order(notes_df$modified, decreasing = TRUE), ]

  cat("RECENT NOTES:\n")
  for(i in 1:min(10, nrow(recent))) {
    cat(paste0("[", i, "] ", format(recent$modified[i], "%Y-%m-%d"), " - "))
    cat(paste0("@", recent$citation[i], " "))
    if(recent$has_content[i]) cat("✓ ") else cat("○ ")
    cat("\n")
  }

  cat("\nOPTIONS:\n")
  cat("1. Compile notes into bibliography\n")
  cat("2. Export notes for project\n")
  cat("0. Exit\n\n")

  choice <- readline("Select option (0-2): ")

  if(choice == "1") {
    compile_reading_notes_to_bibliography(notes_df)
  } else if(choice == "2") {
    export_project_reading_notes(project_filter)
  }

  return(invisible(notes_df))
}

#' Compile reading notes into annotated bibliography
#'
#' Extracts annotations from reading notes and updates bibliography
#'
#' @param notes_df Data frame of notes from review_reading_notes
compile_reading_notes_to_bibliography <- function(notes_df = NULL) {

  paths <- get_project_paths()
  proman_data <- read_proman()

  if(is.null(notes_df)) {
    # Get all notes
    reading_dir <- file.path(paths$personal_notes, "reading")
    note_files <- list.files(reading_dir, pattern = "\\.md$", full.names = TRUE)
  } else {
    note_files <- notes_df$path
  }

  cat("\n📖 Compiling reading notes to bibliography...\n")

  # For each note, extract annotation information
  annotations <- list()

  for(note_file in note_files) {
    content <- readLines(note_file, warn = FALSE)

    # Get citation key
    citation_line <- grep("\\*\\*Citation key:\\*\\*", content, value = TRUE)
    if(length(citation_line) == 0) next

    citation_key <- gsub(".*@([a-zA-Z0-9]+).*", "\\1", citation_line[1])

    # Extract sections
    summary_start <- grep("^### Main Question/Purpose", content)
    relevance_start <- grep("^## Relevance to My Work", content)

    if(length(summary_start) > 0) {
      # Extract summary content
      summary_lines <- content[(summary_start[1] + 1):(summary_start[1] + 10)]
      summary_lines <- summary_lines[!grepl("^\\[.*\\]$", summary_lines)]
      summary_lines <- summary_lines[nchar(summary_lines) > 0]

      if(length(summary_lines) > 0) {
        annotations[[citation_key]] <- list()
        annotations[[citation_key]]$summary <- paste(summary_lines, collapse = " ")
      }
    }
}

  # Update bibliography with annotations
  bib_file <- file.path(paths$sources, paste0("references_", proman_data$project_code, ".bib"))

  if(file.exists(bib_file) && length(annotations) > 0) {
    # This would integrate with your existing update_bibliography function
    cat(paste("Found annotations for", length(annotations), "entries\n"))

    # Create annotation file for update_bibliography to process
    annotation_content <- character()

    for(key in names(annotations)) {
      ann <- annotations[[key]]
      annotation_content <- c(annotation_content,
                              paste0("<!-- ANNOTATION START: ", key, " -->"),
                              paste0("<!-- SUMMARY -->"),
                              null_coalesce(ann$summary, "[No summary]"),
                              paste0("<!-- /SUMMARY -->"),
                              paste0("<!-- ANNOTATION END: ", key, " -->"),
                              ""
      )
    }

    # Save to annotation file
    ann_file <- file.path(paths$personal_notes, paste0("reading_annotations_",
                                                       format(Sys.Date(), "%Y%m%d"), ".qmd"))
    writeLines(annotation_content, ann_file)

    cat(paste("\n✓ Created annotation file:", basename(ann_file), "\n"))
    cat("Run update_bibliography() to incorporate these annotations\n")
  }
}


#' Export reading notes for a project
#'
#' Creates a compiled document of all reading notes for a project
#'
#' @param project_code Project code to export
export_project_reading_notes <- function(project_code = NULL) {

  paths <- get_project_paths()
  proman_data <- read_proman()

  if(is.null(project_code)) {
    project_code <- proman_data$project_code
  }

  # Get all reading notes for this project
  notes_data <- review_reading_notes(project_filter = project_code)

  if(nrow(notes_data) == 0) {
    cat("No reading notes found for project", project_code, "\n")
    return(invisible(NULL))
  }

  # Create compiled document
  output_content <- c(
    paste0("# Reading Notes for Project ", project_code),
    paste0("**Compiled:** ", Sys.Date()),
    paste0("**Total notes:** ", nrow(notes_data)),
    "",
    "---",
    ""
  )

  # Add each note's content
  for(i in 1:nrow(notes_data)) {
    note_content <- readLines(notes_data$path[i], warn = FALSE)

    # Add separator
    output_content <- c(output_content,
                        paste0("## ", i, ". ", notes_data$title[i]),
                        "",
                        note_content,
                        "",
                        "---",
                        ""
    )
  }

  # Save compiled document
  output_file <- paste0("compiled_reading_notes_", project_code, "_",
                        format(Sys.Date(), "%Y%m%d"), ".md")
  output_path <- file.path(paths$personal_notes, output_file)

  writeLines(output_content, output_path)

  cat(paste("\n✓ Exported reading notes to:", output_file, "\n"))
  cat(paste("Location:", paths$personal_notes, "\n"))

  return(invisible(output_path))
}

