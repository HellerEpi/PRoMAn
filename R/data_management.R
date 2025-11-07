
# R/data_management.R
#
# contains: select files by keyword, load by keyword (functions for working with file sets)

### --------------------------------------------------- ###

#' Unified file selection function
#'
#' One function to handle all file selection methods: pattern matching,
#' recent files, file sets, and interactive selection.
#'
#' @param pattern Keywords or regex pattern to match filenames
#' @param exclude Keywords to exclude from results (can be vector)
#' @param recent Time specification: "30m", "2h", "1d" etc
#' @param set_name Name of existing file set to retrieve
#' @param folder Folder to search: "source", "cleaned", etc (default: "source")
#' @param full_path Return full paths (default: TRUE)
#' @param interactive Use interactive selection (default: FALSE)
#' @param file_extension Filter by extension (default: NULL for all files)
#' @return Vector of file paths (full or relative based on full_path parameter)
#' @export
#' @examples
#' # Select by pattern
#' select_files(pattern = "baseline", folder = "cleaned")
#'
#' # Select recent files
#' select_files(recent = "2h", folder = "source")
#'
#' # Select with exclusions
#' select_files(pattern = "data", exclude = c("old", "temp"))
#'
#' # Get a file set
#' select_files(set_name = "baseline_series")
select_files <- function(pattern = NULL,
                         exclude = NULL,
                         recent = NULL,
                         set_name = NULL,
                         folder = "source",
                         full_path = TRUE,
                         interactive = FALSE,
                         file_extension = NULL) {

  # Get folder path using central helper
  folder_path <- .get_folder_path(folder)

  # Case 1: Interactive selection
  if(interactive) {
    all_files <- list.files(folder_path, pattern = file_extension)

    if(length(all_files) == 0) {
      message("No files found in ", folder)
      return(character(0))
    }

    # Show files with numbers
    cat("Available files in", folder, ":\n")
    for(i in seq_along(all_files)) {
      cat(sprintf("%3d: %s\n", i, all_files[i]))
    }

    cat("\nEnter file numbers to select (e.g., 1,3,5 or 1-5 or 'all'): ")
    selection <- readline()

    if(tolower(selection) == "all") {
      files <- all_files
    } else {
      # Parse selection
      selected_indices <- numeric(0)

      # Handle ranges (e.g., "1-5")
      if(grepl("-", selection)) {
        parts <- strsplit(selection, "-")[[1]]
        if(length(parts) == 2) {
          start <- as.numeric(parts[1])
          end <- as.numeric(parts[2])
          if(!is.na(start) && !is.na(end)) {
            selected_indices <- start:end
          }
        }
      } else {
        # Handle comma-separated (e.g., "1,3,5")
        indices <- strsplit(selection, ",")[[1]]
        selected_indices <- as.numeric(trimws(indices))
        selected_indices <- selected_indices[!is.na(selected_indices)]
      }

      # Validate indices
      selected_indices <- selected_indices[selected_indices >= 1 &
                                             selected_indices <= length(all_files)]

      if(length(selected_indices) == 0) {
        message("No valid files selected")
        return(character(0))
      }

      files <- all_files[selected_indices]
    }

    if(full_path) {
      return(file.path(folder_path, files))
    } else {
      return(files)
    }
  }

  # Case 2: Get existing file set
  if(!is.null(set_name)) {
    all_sets <- analyze_file_sets(folder_path = folder_path,
                                  show_files = FALSE,
                                  return_data = TRUE)

    for(folder_sets in all_sets) {
      if(set_name %in% names(folder_sets)) {
        files <- folder_sets[[set_name]]
        if(full_path) {
          return(file.path(folder_path, files))
        } else {
          return(files)
        }
      }
    }
    warning("File set '", set_name, "' not found in ", folder)
    return(character(0))
  }

  # Case 3: Recent files
  if(!is.null(recent)) {
    # Parse time spec
    if(grepl("d$", recent)) {
      days <- as.numeric(sub("d$", "", recent))
      cutoff <- Sys.time() - (days * 24 * 60 * 60)
      time_desc <- paste(days, "days")
    } else if(grepl("h$", recent)) {
      hours <- as.numeric(sub("h$", "", recent))
      cutoff <- Sys.time() - (hours * 60 * 60)
      time_desc <- paste(hours, "hours")
    } else if(grepl("m$", recent)) {
      minutes <- as.numeric(sub("m$", "", recent))
      cutoff <- Sys.time() - (minutes * 60)
      time_desc <- paste(minutes, "minutes")
    } else {
      # Try as plain minutes for backward compatibility
      minutes <- as.numeric(recent)
      if(is.na(minutes)) {
        stop("Invalid recent format. Use: '30m', '2h', '1d' or just minutes")
      }
      cutoff <- Sys.time() - (minutes * 60)
      time_desc <- paste(minutes, "minutes")
    }

    # Get files with optional extension filter
    if(!is.null(file_extension)) {
      pattern <- paste0("\\.", file_extension, "$")
      all_files <- list.files(folder_path, pattern = pattern,
                              full.names = TRUE, ignore.case = TRUE)
    } else {
      all_files <- list.files(folder_path, full.names = TRUE)
    }

    # Filter by modification time
    recent_files <- all_files[file.mtime(all_files) > cutoff]

    if(length(recent_files) == 0) {
      message("No files modified in last ", time_desc, " in ", folder)
      return(character(0))
    }

    # Sort by modification time (most recent first)
    recent_files <- recent_files[order(file.mtime(recent_files), decreasing = TRUE)]

    message("Found ", length(recent_files), " files modified in last ", time_desc)

    if(full_path) {
      return(recent_files)
    } else {
      return(basename(recent_files))
    }
  }

  # Case 4: Pattern matching (default)
  # Get all files with optional extension filter
  if(!is.null(file_extension)) {
    ext_pattern <- paste0("\\.", file_extension, "$")
    all_files <- list.files(folder_path, pattern = ext_pattern, ignore.case = TRUE)
  } else {
    all_files <- list.files(folder_path)
  }

  # If no pattern specified, return all files
  if(is.null(pattern)) {
    if(length(all_files) == 0) {
      message("No files found in ", folder)
      return(character(0))
    }

    if(full_path) {
      return(file.path(folder_path, all_files))
    } else {
      return(all_files)
    }
  }

  # Match pattern - handle multiple patterns as OR
  if(length(pattern) > 1) {
    pattern_regex <- paste(pattern, collapse = "|")
    matching <- grepl(pattern_regex, all_files, ignore.case = TRUE)
  } else {
    matching <- grepl(pattern, all_files, ignore.case = TRUE)
  }

  files <- all_files[matching]

  # Apply exclusions
  if(!is.null(exclude)) {
    for(excl in exclude) {
      files <- files[!grepl(excl, files, ignore.case = TRUE)]
    }
  }

  if(length(files) == 0) {
    message("No files found matching pattern '",
            paste(pattern, collapse = " | "), "' in ", folder)
    return(character(0))
  }

  message("Found ", length(files), " files matching criteria")

  if(full_path) {
    return(file.path(folder_path, files))
  } else {
    return(files)
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
                                  exclude_keywords = NULL,
                                  folder = "source",
                                  file_extension = "csv") {

  # Get matching files
  files <- select_files(pattern = keywords,
                        exclude = exclude_keywords,
                        folder = folder,
                        file_extension = file_extension,
                        full_path = TRUE)

  if(length(files) == 0) {
    message(paste("No files found with keywords:", paste(keywords, collapse = ", ")))
    if(!is.null(exclude_keywords)) {
      message(paste("Excluding:", paste(exclude_keywords, collapse = ", ")))
    }
    return(list())
  }

  # Load all files
  for(file_path in files) {
    tryCatch({
      file_name <- basename(file_path)
      name <- tools::file_path_sans_ext(file_name)

      if(file_extension == "csv") {
        data_list[[name]] <- read.csv(file_path, stringsAsFactors = FALSE)
      } else if(file_extension == "rds") {
        data_list[[name]] <- readRDS(file_path)
      } else if(file_extension %in% c("xlsx", "xls")) {
        if(requireNamespace("readxl", quietly = TRUE)) {
          data_list[[name]] <- readxl::read_excel(file_path)
        } else {
          warning("Package 'readxl' needed to read Excel files")
        }
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
      type_changes <- c(type_changes, paste0(col, ": ", old_type, " -> ", new_type))
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
    if(exists("select_files_interactive")) {
      selected_files <- select_files(interactive = TRUE, folder = source_folder, file_extension = pattern)

    } else {
      stop("Interactive selection not available. Please provide specific filenames.")
    }
    if(length(selected_files) == 0) {
      message("No files selected")
      return(invisible(character(0)))
    }
  }

  # Case 2: Recent files
  else if(length(files) == 1 && grepl("^recent:", files)) {
    time_spec <- sub("recent:", "", files)

    # Parse time specification
    if(grepl("d$", time_spec)) {
      # Days: recent:1d, recent:7d
      days <- as.numeric(sub("d$", "", time_spec))
      selected_files <- select_files(recent = paste0(minutes, "m"), folder = source_folder)
    } else if(grepl("h$", time_spec)) {
      # Hours: recent:1h, recent:24h
      hours <- as.numeric(sub("h$", "", time_spec))
      selected_files <- select_files(pattern = files, folder = source_folder)
    } else {
      # Default to minutes: recent:30, recent:60
      minutes <- as.numeric(time_spec)
      if(is.na(minutes)) minutes <- 30  # default
      selected_files <- select_recent_files(minutes = minutes,
                                            folder_path = source_folder)
    }

    if(length(selected_files) == 0) {
      return(invisible(character(0)))
    }
  }

  # Case 3: Direct file list or keywords
  else {
    # First try as exact filenames
    existing_files <- character(0)
    for(file in files) {
      # Check with full path
      if(file.exists(file)) {
        existing_files <- c(existing_files, basename(file))
      }
      # Check in source folder
      else if(file.exists(file.path(source_folder, file))) {
        existing_files <- c(existing_files, file)
      }
    }

    # If we found exact matches, use them
    if(length(existing_files) > 0) {
      selected_files <- existing_files
      message(paste("Found", length(selected_files), "exact file matches"))
    }
    # Otherwise try as keywords
    else {
      message("No exact file matches found, trying as keywords...")

      # If multiple items, first is include keywords, rest are exclude
      if(length(files) > 1) {
        selected_files <- select_files_by_keyword(
          keywords = files[1],
          exclude_keywords = files[-1],
          folder_path = source_folder,
          full_path = FALSE
        )
      } else {
        selected_files <- select_files_by_keyword(
          keywords = files,
          folder_path = source_folder,
          full_path = FALSE
        )
      }

      if(length(selected_files) == 0) {
        stop("No files found matching: ", paste(files, collapse = ", "))
      }
      message(paste("Found", length(selected_files), "files matching keywords"))
    }
  }

  # Show what we're about to process
  if(length(selected_files) <= 10) {
    cat("\nFiles to process:\n")
    for(f in selected_files) {
      cat("  -", f, "\n")
    }
  } else {
    cat("\nFiles to process: ", length(selected_files), " files\n", sep = "")
    cat("First 5:\n")
    for(i in 1:5) {
      cat("  -", selected_files[i], "\n")
    }
    cat("  ... and", length(selected_files) - 5, "more\n")
  }

  # Confirm if many files
  if(length(selected_files) > 5) {
    cat("\nCreate set '", set_name, "' with these ", length(selected_files), " files? (y/n): ", sep = "")
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
  new_files <- character(0)
  processed_count <- 0

  for(i in seq_along(selected_files)) {
    file <- selected_files[i]

    # Build full source path
    if(file.exists(file)) {
      file_path <- file
    } else {
      file_path <- file.path(source_folder, file)
    }

    if(!file.exists(file_path)) {
      warning(paste("File not found:", file_path))
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

    # Check if destination already exists
    if(file.exists(dest_path)) {
      warning(paste("Destination already exists, skipping:", new_name))
      next
    }

    # Copy or rename
    success <- FALSE
    if(copy_files) {
      success <- file.copy(file_path, dest_path, overwrite = FALSE)
      if(success) message(paste("Created:", new_name))
    } else {
      success <- file.rename(file_path, dest_path)
      if(success) message(paste("Renamed to:", new_name))
    }

    if(success) {
      new_files <- c(new_files, new_name)
      processed_count <- processed_count + 1
    }
  }

  # Report summary
  cat(paste("\n✓ File set '", set_name, "' created with ",
            processed_count, " files\n", sep = ""))

  if(processed_count < length(selected_files)) {
    cat("⚠ ", length(selected_files) - processed_count, " files were skipped\n", sep = "")
  }

  return(invisible(new_files))

}

#' List file sets
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




