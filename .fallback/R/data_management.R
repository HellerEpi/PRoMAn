
# R/data_management.R
#
# contains: select files by keyword, load by keyword (functions for working with file sets)

### --------------------------------------------------- ###

#' Select files by keyword pattern(s)
#'
#' Returns a list of files that contain specified keyword pattern(s)
#'
#' @param keywords Single keyword (string) or multiple keywords (vector). For multiple keywords, ALL must be present
#' @param folder_path Path to search in (default: data/source from current project)
#' @param file_extension File extension to filter by (default: "csv")
#' @param full_path Return full paths or just filenames (default: FALSE for filenames only)
#' @return Vector of matching filenames or full paths
#' @export
select_files_by_keyword <- function(keywords,
                                    folder_path = NULL,
                                    file_extension = "csv",
                                    full_path = FALSE) {

  # Get folder path if not specified - no redundant searching
  if(is.null(folder_path)) {
    proman_data <- read_proman()
    folder_path <- proman_data$.paths$data_source
  }

  # Get all files with the specified extension
  pattern <- paste0("\\.", file_extension, "$")
  all_files <- list.files(folder_path, pattern = pattern, ignore.case = TRUE)

  # Convert single keyword to vector for consistent processing
  if(length(keywords) == 1) {
    keywords <- c(keywords)
  }

  # Filter by all keywords (must contain ALL keywords)
  for(keyword in keywords) {
    all_files <- all_files[grepl(keyword, all_files, ignore.case = TRUE)]
  }

  # Return full paths or just filenames
  if(full_path) {
    return(file.path(folder_path, all_files))
  } else {
    return(all_files)
  }
}



### --------------------------------------------------- ###

#' Load multiple files by keyword pattern(s)
#'
#' Loads all files matching keyword pattern(s) into a named list
#'
#' @param keywords Single keyword (string) or multiple keywords (vector)
#' @param folder_path Path to search in (default: data/source)
#' @param file_extension File extension to filter by (default: "csv")
#' @return Named list of data frames
#' @export
load_files_by_keyword <- function(keywords,
                                  folder_path = NULL,
                                  file_extension = "csv") {

  # Get matching files
  files <- select_files_by_keyword(keywords, folder_path, file_extension, full_path = TRUE)

  if(length(files) == 0) {
    message(paste("No files found with keywords:", paste(keywords, collapse = ", ")))
    return(list())
  }

  # Load all files
  data_list <- list()

  for(file_path in files) {
    tryCatch({
      file_name <- basename(file_path)
      name <- tools::file_path_sans_ext(file_name)

      if(file_extension == "csv") {
        data_list[[name]] <- read.csv(file_path, stringsAsFactors = FALSE)
      } else if(file_extension == "rds") {
        data_list[[name]] <- readRDS(file_path)
      }

      message(paste("Loaded:", file_name))
    }, error = function(e) {
      warning(paste("Failed to load", basename(file_path), ":", e$message))
    })
  }

  return(data_list)
}

### --------------------------------------------------- ###
# this function cleans a vector of strings, either on its own or in a df column

#' Clean and standardize strings for matching/joining
#'
#' Removes leading/trailing spaces, converts to uppercase, and replaces internal spaces with underscores.
#' Works with both vectors and data frame columns.
#'
#' @param x Either a character vector OR a data frame
#' @param column_name If x is a data frame, the name of the column to clean (as string)
#' @param new_column_name If x is a data frame, optional new column name (default: overwrites original)
#' @return Either a cleaned character vector OR the data frame with cleaned column
#' @export
clean_strings <- function(x, column_name = NULL, new_column_name = NULL) {

  # If x is a data frame, work on the specified column
  if(is.data.frame(x)) {
    if(is.null(column_name)) {
      stop("When x is a data frame, column_name must be specified")
    }

    if(is.null(new_column_name)) {
      new_column_name <- column_name
    }

    # Clean the specified column
    x[[new_column_name]] <- clean_string_vector(x[[column_name]])
    return(x)

  } else {
    # x is a vector, clean it directly
    return(clean_string_vector(x))
  }
}

# Helper function (not exported) that does the actual cleaning
clean_string_vector <- function(x) {
  # Handle NAs
  if(any(is.na(x))) {
    result <- rep(NA_character_, length(x))
    non_na_idx <- !is.na(x)

    # Process only non-NA values
    clean_values <- trimws(x[non_na_idx])                    # Remove leading/trailing spaces
    clean_values <- toupper(clean_values)                    # Convert to uppercase
    clean_values <- gsub("\\s+", "_", clean_values)         # Replace spaces with underscores

    result[non_na_idx] <- clean_values
    return(result)
  } else {
    # No NAs, process everything
    x <- trimws(x)                    # Remove leading/trailing spaces
    x <- toupper(x)                   # Convert to uppercase
    x <- gsub("\\s+", "_", x)         # Replace internal spaces with underscores
    return(x)
  }
}


#' Move files to the old folder
#'
#' Moves specified files from their current location to the project's old folder
#'
#' @param filenames Vector of filenames to move
#' @param from_folder Source folder path (default: data/source)
#' @param create_date_folder Create a subfolder in old/ with today's date (default: TRUE)
#' @return Invisible vector of new file paths
#' @export
archive_files <- function(filenames, from_folder = NULL, create_date_folder = TRUE) {
  proman_data <- read_proman()

  if(is.null(from_folder)) {
    from_folder <- proman_data$.paths$data_source
  }

  # Create destination folder
  if(create_date_folder) {
    date_folder <- format(Sys.Date(), "%Y-%m-%d")
    dest_folder <- file.path(proman_data$.paths$old, date_folder)
  } else {
    dest_folder <- proman_data$.paths$old
  }

  if(!dir.exists(dest_folder)) {
    dir.create(dest_folder, recursive = TRUE)
  }

  # Move files
  moved_files <- character(length(filenames))

  for(i in seq_along(filenames)) {
    from_path <- file.path(from_folder, filenames[i])
    to_path <- file.path(dest_folder, filenames[i])

    if(file.exists(from_path)) {
      file.rename(from_path, to_path)
      moved_files[i] <- to_path
      message(paste("Moved:", filenames[i], "to old/"))
    } else {
      warning(paste("File not found:", filenames[i]))
    }
  }

  return(invisible(moved_files))
}

#' Save data with timestamp
#'
#' Saves data with automatic timestamp in filename
#'
#' @param data Data to save
#' @param base_filename Base filename without extension
#' @param folder_path Folder to save in (default: data/cleaned)
#' @param format File format: "csv" or "rds" (default: "csv")
#' @return Full path of saved file
#' @export
save_with_timestamp <- function(data, base_filename, folder_path = NULL, format = "csv") {
  if(is.null(folder_path)) {
    proman_data <- read_proman()
    folder_path <- proman_data$.paths$data_cleaned
  }

  # Create timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Create full filename
  filename <- paste0(base_filename, "_", timestamp, ".", format)
  full_path <- file.path(folder_path, filename)

  # Save file
  if(format == "csv") {
    write.csv(data, full_path, row.names = FALSE)
  } else if(format == "rds") {
    saveRDS(data, full_path)
  }

  message(paste("Saved:", filename))
  return(full_path)
}


#' Check if required files exist
#'
#' Verifies that all required files exist before running analysis
#'
#' @param filenames Vector of required filenames
#' @param folder_path Folder to check in (default: data/source)
#' @param stop_if_missing Stop execution if files are missing (default: TRUE)
#' @return Logical vector indicating which files exist
#' @export
check_required_files <- function(filenames, folder_path = NULL, stop_if_missing = TRUE) {
  if(is.null(folder_path)) {
    proman_data <- read_proman()
    folder_path <- proman_data$.paths$data_source
  }

  file_paths <- file.path(folder_path, filenames)
  files_exist <- file.exists(file_paths)

  missing_files <- filenames[!files_exist]

  if(length(missing_files) > 0) {
    message("Missing files:")
    for(file in missing_files) {
      message(paste("  -", file))
    }

    if(stop_if_missing) {
      stop("Required files are missing. Cannot proceed.")
    }
  } else {
    message("All required files found.")
  }

  return(files_exist)
}

#' Get a quick snapshot of dataset characteristics
#'
#' Returns basic info about data dimensions, types, and missing values
#'
#' @param data Data frame to inspect
#' @param save_summary Save summary to a file (default: FALSE)
#' @param filename If saving, filename for the summary
#' @return Data frame with column summaries
#' @export
data_snapshot <- function(data, save_summary = FALSE, filename = NULL) {

  # Basic dataset info
  cat("Dataset Overview:\n")
  cat(paste("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n"))

  # Column-by-column summary
  summary_df <- data.frame(
    column = names(data),
    type = sapply(data, function(x) class(x)[1]),
    non_missing = sapply(data, function(x) sum(!is.na(x))),
    missing = sapply(data, function(x) sum(is.na(x))),
    missing_pct = round(sapply(data, function(x) sum(is.na(x))/length(x) * 100), 1),
    unique_values = sapply(data, function(x) length(unique(x[!is.na(x)]))),
    stringsAsFactors = FALSE
  )

  print(summary_df)

  # Save if requested
  if(save_summary) {
    proman_data <- read_proman()
    if(is.null(filename)) {
      filename <- paste0("data_snapshot_", format(Sys.Date(), "%Y%m%d"), ".csv")
    }
    write.csv(summary_df, file.path(proman_data$.paths$personal_notes, filename), row.names = FALSE)
    message(paste("Summary saved to:", filename))
  }

  return(invisible(summary_df))
}

#' Compare two datasets
#'
#' Shows differences between two datasets - columns added, removed, or changed
#'
#' @param old_data Original dataset
#' @param new_data New dataset to compare against
#' @param id_column Optional ID column for row-level comparison
#' @return List with comparison results
#' @export
compare_datasets <- function(old_data, new_data, id_column = NULL) {

  old_cols <- names(old_data)
  new_cols <- names(new_data)

  # Column changes
  added_cols <- setdiff(new_cols, old_cols)
  removed_cols <- setdiff(old_cols, new_cols)
  common_cols <- intersect(old_cols, new_cols)

  cat("Dataset Comparison:\n")
  cat(paste("Old dataset:", nrow(old_data), "rows x", ncol(old_data), "columns\n"))
  cat(paste("New dataset:", nrow(new_data), "rows x", ncol(new_data), "columns\n\n"))

  if(length(added_cols) > 0) {
    cat("Added columns:", paste(added_cols, collapse = ", "), "\n")
  }

  if(length(removed_cols) > 0) {
    cat("Removed columns:", paste(removed_cols, collapse = ", "), "\n")
  }

  # Type changes in common columns
  type_changes <- character()
  for(col in common_cols) {
    old_type <- class(old_data[[col]])[1]
    new_type <- class(new_data[[col]])[1]
    if(old_type != new_type) {
      type_changes <- c(type_changes, paste0(col, ": ", old_type, " b ", new_type))
    }
  }

  if(length(type_changes) > 0) {
    cat("Type changes:\n")
    for(change in type_changes) {
      cat(paste("  ", change, "\n"))
    }
  }

  comparison <- list(
    added_columns = added_cols,
    removed_columns = removed_cols,
    type_changes = type_changes,
    common_columns = common_cols
  )

  return(invisible(comparison))
}


#' Get summary of folder contents
#'
#' Returns information about files in a folder including size and modification dates
#'
#' @param folder_path Path to folder (default: data/source)
#' @param sort_by Sort by: "name", "size", or "date" (default: "date")
#' @return Data frame with file information
#' @export
get_folder_summary <- function(folder_path = NULL, sort_by = "date") {
  if(is.null(folder_path)) {
    proman_data <- read_proman()
    folder_path <- proman_data$.paths$data_source
  }

  if(!dir.exists(folder_path)) {
    warning(paste("Folder does not exist:", folder_path))
    return(data.frame())
  }

  files <- list.files(folder_path, full.names = TRUE)

  if(length(files) == 0) {
    message("No files found in folder")
    return(data.frame())
  }

  file_info <- data.frame(
    filename = basename(files),
    size_kb = round(file.size(files) / 1024, 1),
    modified = file.mtime(files),
    stringsAsFactors = FALSE
  )

  # Sort as requested
  if(sort_by == "size") {
    file_info <- file_info[order(file_info$size_kb, decreasing = TRUE), ]
  } else if(sort_by == "date") {
    file_info <- file_info[order(file_info$modified, decreasing = TRUE), ]
  } else {
    file_info <- file_info[order(file_info$filename), ]
  }

  print(file_info)
  return(invisible(file_info))
}


#' Find files that haven't been modified recently
#'
#' Identifies stale files that might need cleanup
#'
#' @param days Number of days to consider "stale" (default: 30)
#' @param folder_path Folder to check (default: data/source)
#' @return Vector of stale filenames
#' @export
find_stale_files <- function(days = 30, folder_path = NULL) {
  if(is.null(folder_path)) {
    proman_data <- read_proman()
    folder_path <- proman_data$.paths$data_source
  }

  files <- list.files(folder_path, full.names = TRUE)
  cutoff_date <- Sys.time() - (days * 24 * 60 * 60)

  stale_files <- files[file.mtime(files) < cutoff_date]
  stale_names <- basename(stale_files)

  if(length(stale_names) > 0) {
    cat(paste("Files not modified in", days, "days:\n"))
    for(file in stale_names) {
      cat(paste("  ", file, "\n"))
    }
  } else {
    message(paste("No stale files found (older than", days, "days)"))
  }

  return(invisible(stale_names))
}

#' Find large files that might need cleanup
#'
#' Identifies files larger than specified size
#'
#' @param min_size_mb Minimum size in MB to flag (default: 10)
#' @param folder_path Folder to check (default: data/source)
#' @return Data frame with large files
#' @export
find_large_files <- function(min_size_mb = 10, folder_path = NULL) {
  if(is.null(folder_path)) {
    proman_data <- read_proman()
    folder_path <- proman_data$.paths$data_source
  }

  files <- list.files(folder_path, full.names = TRUE)
  min_size_bytes <- min_size_mb * 1024 * 1024

  large_files <- files[file.size(files) > min_size_bytes]

  if(length(large_files) > 0) {
    large_info <- data.frame(
      filename = basename(large_files),
      size_mb = round(file.size(large_files) / (1024 * 1024), 1),
      stringsAsFactors = FALSE
    )

    large_info <- large_info[order(large_info$size_mb, decreasing = TRUE), ]

    cat(paste("Files larger than", min_size_mb, "MB:\n"))
    print(large_info)
    return(invisible(large_info))
  } else {
    message(paste("No files larger than", min_size_mb, "MB found"))
    return(invisible(data.frame()))
  }
}



#' Flag suspicious values in dataset
#'
#' Identifies potentially problematic values that may represent missing data codes
#'
#' @param data Data frame to check
#' @param suspicious_codes Vector of codes to flag as suspicious
#' @return Data frame of flagged values
#' @export
flag_suspicious_values <- function(data, suspicious_codes = c(-999, -99, -9, 999, 9999)) {
  flagged <- data.frame(
    column = character(0),
    value = character(0),
    count = numeric(0),
    stringsAsFactors = FALSE
  )

  for(col in names(data)) {
    if(is.numeric(data[[col]])) {
      for(code in suspicious_codes) {
        count <- sum(data[[col]] == code, na.rm = TRUE)
        if(count > 0) {
          flagged <- rbind(flagged, data.frame(
            column = col,
            value = as.character(code),
            count = count,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  return(flagged)
}

#' Analyze missing data patterns
#'
#' Provides summary of missing data across all variables
#'
#' @param data Data frame to analyze
#' @param threshold Percentage threshold for flagging high missingness
#' @return Data frame with missing data summary
#' @export
missing_data_patterns <- function(data, threshold = 5) {
  missing_summary <- data.frame(
    column = names(data),
    missing_count = sapply(data, function(x) sum(is.na(x))),
    missing_pct = round(sapply(data, function(x) sum(is.na(x))/length(x) * 100), 2),
    stringsAsFactors = FALSE
  )

  missing_summary$high_missing <- missing_summary$missing_pct > threshold

  cat("Missing Data Analysis:\n")
  cat(paste("Columns with >", threshold, "% missing:\n"))
  flagged <- missing_summary[missing_summary$high_missing, ]
  if(nrow(flagged) > 0) {
    print(flagged[, c("column", "missing_count", "missing_pct")])
  } else {
    cat("None\n")
  }

  return(missing_summary)
}

#' Detect likely ID columns in dataset
#'
#' Identifies columns that appear to be identifier variables
#'
#' @param data Data frame to analyze
#' @return Vector of likely ID column names
#' @export
detect_id_columns <- function(data) {
  id_cols <- character(0)

  for(col in names(data)) {
    # Check if column name suggests it's an ID
    if(grepl("^id$|^ID$|_id$|_ID$|^.*_ID$|^.*id$", col)) {
      id_cols <- c(id_cols, col)
    }
    # Check if all values are unique and not missing (likely ID)
    else if(length(unique(data[[col]][!is.na(data[[col]])])) == sum(!is.na(data[[col]])) &&
            sum(!is.na(data[[col]])) > 0) {
      id_cols <- c(id_cols, col)
    }
  }

  return(id_cols)
}

#' Standardize variable names
#'
#' Converts variable names to consistent format
#'
#' @param data Data frame with variables to standardize
#' @param naming_convention Convention to use ("snake_case", "camelCase")
#' @param id_patterns Patterns that identify ID columns (currently unused)
#' @return Data frame with standardized names
#' @export
standardize_var_names <- function(data, naming_convention = "snake_case", id_patterns = NULL) {
  old_names <- names(data)
  new_names <- old_names

  if(naming_convention == "snake_case") {
    new_names <- tolower(new_names)
    new_names <- gsub("[^a-z0-9_]", "_", new_names)
    new_names <- gsub("_{2,}", "_", new_names)  # Remove multiple underscores
    new_names <- gsub("^_|_$", "", new_names)   # Remove leading/trailing underscores
  }

  names(data) <- new_names

  # Report changes
  changes <- sum(old_names != new_names)
  if(changes > 0) {
    cat("Standardized", changes, "variable names\n")
    for(i in which(old_names != new_names)) {
      cat(paste("  ", old_names[i], "->", new_names[i], "\n"))
    }
  }

  return(data)
}

#' Clean demographic variables
#'
#' Applies standard cleaning to common demographic variables
#'
#' @param data Data frame containing demographic variables
#' @param age_var Name of age variable
#' @param gender_var Name of gender variable
#' @param race_var Name of race/ethnicity variable
#' @return Data frame with cleaned demographics
#' @export
clean_demographics <- function(data, age_var = NULL, gender_var = NULL, race_var = NULL) {

  if(!is.null(age_var) && !is.na(age_var) && age_var %in% names(data)) {
    # Clean age variable - flag impossible values
    original_count <- sum(!is.na(data[[age_var]]))
    data[[age_var]][data[[age_var]] < 0 | data[[age_var]] > 120] <- NA
    cleaned_count <- original_count - sum(!is.na(data[[age_var]]))
    if(cleaned_count > 0) {
      cat("Cleaned", cleaned_count, "invalid age values (set to NA)\n")
    }
  }

  if(!is.null(gender_var) && !is.na(gender_var) && gender_var %in% names(data)) {
    # Standardize gender coding
    data[[gender_var]] <- clean_strings(data[[gender_var]])
    cat("Standardized gender variable\n")
  }

  if(!is.null(race_var) && !is.na(race_var) && race_var %in% names(data)) {
    # Standardize race/ethnicity coding
    data[[race_var]] <- clean_strings(data[[race_var]])
    cat("Standardized race/ethnicity variable\n")
  }

  return(data)
}

#' Generate comprehensive data dictionary
#'
#' Creates documentation for all variables in dataset
#'
#' @param data Data frame to document
#' @param filename Optional filename to save dictionary
#' @param include_stats Include descriptive statistics
#' @param guess_descriptions Attempt to guess variable descriptions
#' @return Data dictionary data frame
#' @export
generate_data_dictionary <- function(data, filename = NULL, include_stats = TRUE, guess_descriptions = FALSE) {

  dict <- data.frame(
    variable = names(data),
    type = sapply(data, function(x) class(x)[1]),
    description = "",
    stringsAsFactors = FALSE
  )

  if(include_stats) {
    dict$missing_count <- sapply(data, function(x) sum(is.na(x)))
    dict$missing_pct <- round(dict$missing_count / nrow(data) * 100, 1)
    dict$unique_values <- sapply(data, function(x) length(unique(x[!is.na(x)])))
  }

  if(guess_descriptions) {
    # Simple description guessing based on variable names
    dict$description <- sapply(dict$variable, function(x) {
      if(grepl("age", x, ignore.case = TRUE)) return("Age variable")
      if(grepl("gender|sex", x, ignore.case = TRUE)) return("Gender/sex variable")
      if(grepl("race|ethnicity", x, ignore.case = TRUE)) return("Race/ethnicity variable")
      if(grepl("id", x, ignore.case = TRUE)) return("Identifier variable")
      return("Description needed")
    })
  }

  if(!is.null(filename)) {
    proman_data <- read_proman()
    write.csv(dict, file.path(proman_data$.paths$personal_notes, filename), row.names = FALSE)
    cat("Data dictionary saved as:", filename, "\n")
  }

  return(dict)
}

#' Create a file set by adding a unique identifier to filenames
#'
#' Takes existing files and creates copies with a set identifier added to the filename.
#' Can accept multiple input types for maximum flexibility.
#'
#' @param files Can be:
#'   - Character vector of filenames
#'   - "interactive" to select files interactively
#'   - "recent:N" to select files from last N minutes (e.g., "recent:30")
#'   - Keywords to search for (if no files found with exact names)
#' @param set_name Name/identifier to add to the files
#' @param position Where to add identifier: "prefix", "suffix", or "before_extension"
#' @param source_folder Source folder (default: data/source)
#' @param dest_folder Destination folder (default: same as source)
#' @param copy_files If TRUE, copy files; if FALSE, rename them (default: TRUE)
#' @param separator Character to use between name parts (default: "_")
#' @param pattern Optional pattern to filter files (for interactive mode)
#' @return Vector of new filenames
#' @export
create_file_set <- function(files,
                            set_name,
                            position = "before_extension",
                            source_folder = NULL,
                            dest_folder = NULL,
                            copy_files = TRUE,
                            separator = "_",
                            pattern = NULL) {

  proman_data <- read_proman()

  # Set default folders
  if(is.null(source_folder)) {
    source_folder <- proman_data$.paths$data_source
  }

  if(is.null(dest_folder)) {
    dest_folder <- source_folder
  }

  # SMART FILE SELECTION
  selected_files <- character(0)

  # Case 1: Interactive selection
  if(length(files) == 1 && files == "interactive") {
    selected_files <- select_files_interactive(folder_path = source_folder,
                                               pattern = pattern)
    if(length(selected_files) == 0) {
      message("No files selected")
      return(invisible(character(0)))
    }
  }

  # Case 2: Recent files
  else if(length(files) == 1 && grepl("^recent:", files)) {
    minutes <- as.numeric(sub("recent:", "", files))
    if(is.na(minutes)) minutes <- 30  # default

    selected_files <- select_recent_files(minutes = minutes,
                                          folder_path = source_folder)
    if(length(selected_files) == 0) {
      message(paste("No files modified in last", minutes, "minutes"))
      return(invisible(character(0)))
    }
  }

  # Case 3: Check if files exist as given
  else {
    existing_files <- character(0)
    for(file in files) {
      if(file.exists(file.path(source_folder, file)) || file.exists(file)) {
        existing_files <- c(existing_files, file)
      }
    }

    # Case 4: If no files found, try as keywords
    if(length(existing_files) == 0) {
      message("No exact file matches found, trying as keywords...")
      selected_files <- select_files_by_keyword(files,
                                                folder_path = source_folder)
      if(length(selected_files) == 0) {
        stop("No files found matching: ", paste(files, collapse = ", "))
      }
      message(paste("Found", length(selected_files), "files matching keywords"))
    } else {
      selected_files <- existing_files
    }
  }

  # Confirm if interactive or many files
  if(length(selected_files) > 5 ||
     (length(files) == 1 && files %in% c("interactive", "recent"))) {
    cat("\nCreate set '", set_name, "' with these", length(selected_files), "files? (y/n): ")
    confirm <- readline()
    if(tolower(substr(confirm, 1, 1)) != "y") {
      message("Set creation cancelled")
      return(invisible(character(0)))
    }
  }

  # Ensure destination exists
  if(!dir.exists(dest_folder)) {
    dir.create(dest_folder, recursive = TRUE)
  }

  # Process each file
  new_files <- character(length(selected_files))

  for(i in seq_along(selected_files)) {
    file <- selected_files[i]

    # Get full path if not already
    if(!file.exists(file)) {
      file_path <- file.path(source_folder, file)
    } else {
      file_path <- file
    }

    if(!file.exists(file_path)) {
      warning(paste("File not found:", file))
      next
    }

    # Parse filename
    base_name <- tools::file_path_sans_ext(basename(file))
    extension <- tools::file_ext(basename(file))

    # Create new filename based on position
    if(position == "prefix") {
      new_name <- paste0(set_name, separator, base_name)
    } else if(position == "suffix") {
      new_name <- paste0(base_name, separator, set_name)
    } else {  # before_extension (default)
      new_name <- paste0(base_name, separator, set_name)
    }

    # Add extension back
    if(nchar(extension) > 0) {
      new_name <- paste0(new_name, ".", extension)
    }

    # Create full destination path
    dest_path <- file.path(dest_folder, new_name)

    # Copy or rename
    if(copy_files) {
      file.copy(file_path, dest_path, overwrite = FALSE)
      message(paste("Created:", new_name))
    } else {
      file.rename(file_path, dest_path)
      message(paste("Renamed to:", new_name))
    }

    new_files[i] <- new_name
  }

  # Report summary
  cat(paste("\nFile set '", set_name, "' created with",
            sum(new_files != ""), "files\n"))

  return(invisible(new_files))
}


#' List file sets with enhanced detection
#'
#' User-friendly wrapper for analyze_file_sets with better defaults
#'
#' @param folder_path Folder to analyze (default: both source and cleaned)
#' @param show_files Show individual files in each set (default: TRUE)
#' @export
list_file_sets <- function(folder_path = NULL, show_files = TRUE) {
  analyze_file_sets(folder_path = folder_path, show_files = show_files, return_data = FALSE)
}

#' Get file sets as data structure
#'
#' Returns file sets as a list for programmatic use
#'
#' @param folder_path Folder to analyze
#' @export
get_file_sets <- function(folder_path = NULL) {
  analyze_file_sets(folder_path = folder_path, show_files = FALSE, return_data = TRUE)
}

#' Add identifier to files from existing sets
#'
#' Gets all files from specified sets and adds another identifier
#'
#' @param set_names Names of sets to combine (as shown by list_file_sets)
#' @param new_identifier New identifier to add
#' @param source_folder Source folder (default: checks both data/source and data/cleaned)
#' @param archive_originals Whether to archive the original files after creating new set (default: FALSE)
#' @param ... Other parameters passed to create_file_set
#' @export
add_to_sets <- function(set_names, new_identifier, source_folder = NULL, archive_originals = TRUE, ...) {

  proman_data <- read_proman()

  # If no folder specified, we need to search for the sets
  if(is.null(source_folder)) {
    # Get sets from both folders
    all_sets <- get_file_sets()  # Gets both source and cleaned

    # Find which folder contains the requested sets
    all_files <- character(0)
    found_folder <- NULL

    for(folder_name in names(all_sets)) {
      folder_sets <- all_sets[[folder_name]]

      for(set_name in set_names) {
        if(set_name %in% names(folder_sets)) {
          all_files <- c(all_files, folder_sets[[set_name]])

          # Track which folder we found files in
          if(is.null(found_folder)) {
            if(folder_name == "Source Data") {
              found_folder <- proman_data$.paths$data_source
            } else if(folder_name == "Cleaned Data") {
              found_folder <- proman_data$.paths$data_cleaned
            }
          }

          cat("Found set '", set_name, "' in ", folder_name, " with ",
              length(folder_sets[[set_name]]), " files\n", sep = "")
        }
      }
    }

    # Use the folder where we found the sets
    if(!is.null(found_folder)) {
      source_folder <- found_folder
    } else {
      stop("No matching sets found in any folder")
    }

  } else {
    # Specific folder provided
    all_sets <- get_file_sets(folder_path = source_folder)

    # Find the folder key
    folder_key <- names(all_sets)[1]
    if(is.null(folder_key) || length(all_sets[[folder_key]]) == 0) {
      stop("No file sets found in ", source_folder)
    }

    available_sets <- all_sets[[folder_key]]
    all_files <- character(0)

    for(set_name in set_names) {
      if(set_name %in% names(available_sets)) {
        all_files <- c(all_files, available_sets[[set_name]])
        cat("Found set '", set_name, "' with ", length(available_sets[[set_name]]), " files\n", sep = "")
      } else {
        warning("Set '", set_name, "' not found in ", source_folder)
      }
    }
  }

  all_files <- unique(all_files)

  if(length(all_files) == 0) {
    stop("No files found in the specified sets")
  }

  cat("\nTotal files to process: ", length(all_files), "\n")
  cat("Using folder: ", source_folder, "\n\n")

  # Use create_file_set to add the new identifier
  new_files <- create_file_set(all_files, new_identifier, source_folder = source_folder, ...)

  # Archive originals if requested
  if(archive_originals && length(all_files) > 0) {
    cat("\n")

    # Ask for confirmation if interactive
    if(interactive()) {
      cat("Archive the original ", length(all_files), " files? (y/n): ", sep = "")
      response <- readline()

      if(tolower(substr(response, 1, 1)) == "y") {
        cat("\nArchiving original files...\n")
        archive_files(all_files, from_folder = source_folder, create_date_folder = TRUE)
        cat("✓ Original files archived\n")
      } else {
        cat("Skipping archive\n")
      }
    } else {
      # Non-interactive mode - just archive
      archive_files(all_files, from_folder = source_folder, create_date_folder = TRUE)
      cat("✓ Original files archived\n")
    }
  }

  return(invisible(new_files))
}


#' Select recently modified files
#'
#' Quick function to get files modified in last N minutes
#'
#' @param minutes How many minutes back to look (default: 30)
#' @param folder_path Folder to check (default: data/source)
#' @return Vector of recently modified filenames
#' @export
select_recent_files <- function(minutes = 30, folder_path = NULL) {
  if(is.null(folder_path)) {
    proman_data <- read_proman()
    folder_path <- proman_data$.paths$data_source
  }

  all_files <- list.files(folder_path, full.names = TRUE)
  if(length(all_files) == 0) return(character(0))

  cutoff_time <- Sys.time() - (minutes * 60)
  recent_mask <- file.mtime(all_files) > cutoff_time
  recent_files <- basename(all_files[recent_mask])

  if(length(recent_files) > 0) {
    cat("Files modified in last", minutes, "minutes:\n")
    for(f in recent_files) {
      cat("-", f, "\n")
    }
  } else {
    cat("No files modified in last", minutes, "minutes\n")
  }

  return(recent_files)
}
