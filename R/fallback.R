# R/fallback.R
#
# This contains the functions that take snapshots of projects in case something goes wrong within a session or otherwise.
#  Currently thinking fallback() should be conducted when the user specifies, but not sure.
#  Want to avoid accidentally overriding snapshopts after mistakes.
#  This could mean the saves should occur at different times depending on the user's workflow.
#
#

#### -------------------------------------------------------------------- ####

#' Resolve path to project root
#' @keywords internal
resolve_project_root <- function(path) {
  abs_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  tryCatch({
    find_project_root(abs_path)
  }, error = function(e) {
    if (file.exists(file.path(abs_path, ".proman"))) {
      return(abs_path)
    } else {
      stop("No .proman file found. Are you in a PRoMan project directory?")
    }
  })
}


#' Set a fallback checkpoint for project files
#'
#' @param path Project directory to snapshot (default: NULL uses current project)
#' @param verbose Print confirmation message
#'
#' @export
set_fallback <- function(path = NULL, verbose = TRUE) {

  # Use current project if no path specified
  if (is.null(path)) {
    if (!exists("current_project", envir = .proman_env)) {
      stop("No active project. Run set_project() or get_project_paths() first.")
    }
    project_root <- .proman_env$current_project
  } else {
    # If path is provided, resolve it
    project_root <- resolve_project_root(path)
  }

  # Get project info for display
  proman_data <- read_proman(project_root)
  project_name <- proman_data$project_name
  project_code <- proman_data$project_code

  # Show what project we're backing up
  if (verbose) {
    cli::cli_h2("Setting fallback for project: {project_name} ({project_code})")
    cli::cli_alert_info("Project location: {project_root}")
  }

  # Create hidden fallback directory
  fallback_dir <- file.path(project_root, ".fallback")
  if (!dir.exists(fallback_dir)) {
    dir.create(fallback_dir, showWarnings = FALSE)

    # Hide folder on Windows
    if (Sys.info()["sysname"] == "Windows") {
      system(paste("attrib +h", shQuote(fallback_dir)))
    }
  }

  # Clear any existing fallback
  existing_files <- list.files(fallback_dir)
  if (length(existing_files) > 0) {
    if (verbose) {
      cli::cli_alert_warning("Clearing existing fallback ({length(existing_files)} files)")
    }
    unlink(file.path(fallback_dir, "*"), recursive = TRUE)
  }

  # Get all files (excluding hidden)
  all_files <- list.files(project_root,
                          recursive = TRUE,
                          all.files = FALSE,
                          full.names = TRUE)

  # Filter out .fallback directory
  files_to_backup <- all_files[!grepl("^\\.fallback", all_files)]

  if (verbose) {
    cli::cli_alert_info("Backing up {length(files_to_backup)} files to: {fallback_dir}")
  }

  # Copy files maintaining directory structure
  for (file in files_to_backup) {
    rel_path <- sub(paste0("^", project_root, "/"), "", file)
    dest_file <- file.path(fallback_dir, rel_path)
    dest_dir <- dirname(dest_file)

    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    }

    file.copy(file, dest_file, overwrite = TRUE)
  }

  # Record fallback time in .proman file
  fallback_time <- Sys.time()
  proman_data$last_fallback_time <- fallback_time
  write_proman(proman_data)

  if (verbose) {
    cli::cli_alert_success("Fallback set at {format(fallback_time, '%Y-%m-%d %H:%M:%S')}")
    cli::cli_alert_success("Backed up {length(files_to_backup)} files for project: {project_name}")
    cli::cli_text("")  # Add blank line for readability
  }

  invisible(TRUE)
}


#' Restore files from fallback checkpoint
#'
#' @param path Project directory (default: NULL uses current project)
#'
#' @export
fallback <- function(path = NULL) {

  # Use current project if no path specified
  if (is.null(path)) {
    if (!exists("current_project", envir = .proman_env)) {
      stop("No active project. Run set_project() or get_project_paths() first.")
    }
    project_root <- .proman_env$current_project
  } else {
    # If path is provided, resolve it
    project_root <- resolve_project_root(path)
  }

  # Get project info
  proman_data <- read_proman(project_root)
  project_name <- proman_data$project_name
  project_code <- proman_data$project_code
  fallback_time <- proman_data$last_fallback_time

  cli::cli_h2("Restoring fallback for project: {project_name} ({project_code})")
  cli::cli_alert_info("Project location: {project_root}")

  fallback_dir <- file.path(project_root, ".fallback")

  if (!dir.exists(fallback_dir) || length(list.files(fallback_dir)) == 0) {
    cli::cli_alert_danger("No fallback found. Run {.fn set_fallback} first.")
    return(invisible(FALSE))
  }

  if (!is.null(fallback_time)) {
    cli::cli_alert_info("Fallback was created at: {format(fallback_time, '%Y-%m-%d %H:%M:%S')}")
  }

  if (interactive()) {
    cli::cli_alert_warning("This will overwrite current files in: {project_root}")
    response <- readline("Are you sure you want to restore? (y/n): ")
    if (tolower(substr(response, 1, 1)) != "y") {
      cli::cli_alert_info("Restore cancelled")
      return(invisible(FALSE))
    }
  }

  # Get all backed up files
  backup_files <- list.files(fallback_dir,
                             recursive = TRUE,
                             full.names = TRUE)

  cli::cli_progress_bar("Restoring files", total = length(backup_files))

  # Restore each file
  restored_count <- 0

  for (backup_file in backup_files) {
    rel_path <- sub(paste0("^", fallback_dir, "/"), "", backup_file)
    dest_file <- file.path(project_root, rel_path)

    # Create directory if needed
    dest_dir <- dirname(dest_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    }

    # Copy file back
    file.copy(backup_file, dest_file, overwrite = TRUE)
    restored_count <- restored_count + 1

    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_alert_success("Restored {restored_count} files to: {project_root}")
  cli::cli_text("")  # Blank line for readability

  invisible(TRUE)
}



#' Show information about current fallback
#'
#' @param path Project directory (default: NULL uses current project)
#' @param show_files Logical, whether to list all backed up files (default: FALSE)
#'
#' @export
fallback_info <- function(path = NULL, show_files = FALSE) {

  # Use current project if no path specified
  if (is.null(path)) {
    if (!exists("current_project", envir = .proman_env)) {
      stop("No active project. Run set_project() or get_project_paths() first.")
    }
    project_root <- .proman_env$current_project
  } else {
    project_root <- resolve_project_root(path)
  }

  # Get project info
  proman_data <- read_proman(project_root)
  project_name <- proman_data$project_name
  project_code <- proman_data$project_code

  cli::cli_h2("Fallback info for: {project_name} ({project_code})")

  fallback_dir <- file.path(project_root, ".fallback")

  if (!dir.exists(fallback_dir) || length(list.files(fallback_dir)) == 0) {
    cli::cli_alert_info("No fallback exists for this project")
    return(invisible(NULL))
  }

  # Get fallback details
  files <- list.files(fallback_dir, recursive = TRUE, full.names = TRUE)
  fallback_time <- proman_data$last_fallback_time

  # Calculate total size
  total_size <- sum(file.info(files)$size, na.rm = TRUE)
  size_mb <- round(total_size / 1024^2, 2)

  # Show summary
  cli::cli_alert_info("Fallback created: {format(fallback_time, '%Y-%m-%d %H:%M:%S')}")
  cli::cli_alert_info("Files backed up: {length(files)}")
  cli::cli_alert_info("Total size: {size_mb} MB")
  cli::cli_alert_info("Location: {fallback_dir}")

  # Group files by directory if showing files
  if (show_files && length(files) > 0) {
    cli::cli_text("")
    cli::cli_h3("Backed up files:")

    # Get relative paths and group by directory
    rel_paths <- sub(paste0("^", fallback_dir, "/"), "", files)
    dirs <- unique(dirname(rel_paths))

    for (dir in sort(dirs)) {
      dir_files <- rel_paths[dirname(rel_paths) == dir]
      cli::cli_text("{.file {dir}/}")
      for (f in sort(basename(dir_files))) {
        cli::cli_text("  └─ {f}")
      }
    }
  } else if (length(files) > 10) {
    cli::cli_text("")
    cli::cli_text("Use {.code fallback_info(show_files = TRUE)} to see all files")
  }

  # Return info invisibly
  info <- list(
    project_code = project_code,
    project_name = project_name,
    fallback_time = fallback_time,
    file_count = length(files),
    total_size_mb = size_mb,
    files = if(show_files) rel_paths else NULL
  )

  invisible(info)
}





