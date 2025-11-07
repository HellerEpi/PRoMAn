# R/fallback.R
#
# This contains the functions that take snapshots of projects in case something goes wrong within a session or otherwise.
#  Currently thinking fallback() should be conducted when the user specifies, but not sure.
#  Want to avoid accidentally overriding snapshopts after mistakes.
#  This could mean the saves should occur at different times depending on the user's workflow.
#
#

#### -------------------------------------------------------------------- ####


#' Set a fallback checkpoint for project files
#'
#' @param path Project directory to snapshot (default: current directory)
#' @param verbose Print confirmation message
#'
#' @export
set_fallback <- function(path = ".", verbose = TRUE) {

  # Create hidden fallback directory
  fallback_dir <- file.path(path, ".fallback")
  if (!dir.exists(fallback_dir)) {
    dir.create(fallback_dir, showWarnings = FALSE)

    # Hide folder on Windows
    if (Sys.info()["sysname"] == "Windows") {
      system(paste("attrib +h", shQuote(fallback_dir)))
    }
  }

  # Clear any existing fallback
  if (length(list.files(fallback_dir)) > 0) {
    unlink(file.path(fallback_dir, "*"), recursive = TRUE)
  }

  # Get all files (excluding hidden)
  all_files <- list.files(path,
                          recursive = TRUE,
                          all.files = FALSE,
                          full.names = TRUE)

  # Filter out .fallback directory
  files_to_backup <- all_files[!grepl("^\\.fallback", all_files)]

  # Copy files maintaining directory structure
  for (file in files_to_backup) {
    rel_path <- sub(paste0("^", path, "/"), "", file)
    dest_file <- file.path(fallback_dir, rel_path)
    dest_dir <- dirname(dest_file)

    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    }

    file.copy(file, dest_file, overwrite = TRUE)
  }

  # Record fallback time in .proman file
  fallback_time <- Sys.time()
  proman_data <- read_proman()
  proman_data$last_fallback_time  <- fallback_time
  write_proman(proman_data)

  if (verbose) {
    cli::cli_alert_success("Fallback set at {format(fallback_time, '%Y-%m-%d %H:%M:%S')} ({length(files_to_backup)} files)")
  }

  invisible(TRUE)
}

#' Restore files from fallback checkpoint
#'
#' @param path Project directory (default: current directory)
#'
#' @export
fallback <- function(path = ".") {

  fallback_dir <- file.path(path, ".fallback")

  if (!dir.exists(fallback_dir) || length(list.files(fallback_dir)) == 0) {
    cli::cli_alert_danger("No fallback found. Run {.fn set_fallback} first.")
    return(invisible(FALSE))
  }

  if (interactive()) {
    response <- readline("Are you sure you want to restore? This will overwrite current files (y/n): ")
    if (tolower(substr(response, 1, 1)) != "y") {
      cli::cli_alert_info("Restore cancelled")
      return(invisible(FALSE))
    }
  }

  # Get fallback time from .proman
  proman_data <- read_proman()
  fallback_time <- proman_data$last_fallback_time

  if (!is.null(fallback_time)) {
    cli::cli_alert_info("Restoring from fallback set at {format(fallback_time, '%Y-%m-%d %H:%M:%S')}")
  }

  # Get all backed up files
  backup_files <- list.files(fallback_dir,
                             recursive = TRUE,
                             full.names = TRUE)

  # Restore each file
  restored_count <- 0

  for (backup_file in backup_files) {
    rel_path <- sub(paste0("^", fallback_dir, "/"), "", backup_file)
    dest_file <- file.path(path, rel_path)

    # Create directory if needed
    dest_dir <- dirname(dest_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    }

    # Copy file back
    file.copy(backup_file, dest_file, overwrite = TRUE)
    restored_count <- restored_count + 1
  }

  cli::cli_alert_success("Restored {restored_count} files")

  invisible(TRUE)
}

