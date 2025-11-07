# R/project_setup.R
# Project setup functions
#
# includes:
# I.  create_project - creates a project with tracking system
# II. get_project_paths - gets paths with automatic folder tracking

### ------------------------------------------------------------ ###

#' Create a new project with standard folder structure
#'
#' Creates a standardized PRoMan project directory with all required folders,
#' tracking markers, and a .proman configuration file. Can be used interactively
#' with GUI folder selection or programmatically with specified path.
#'
#' @param project_code Unique project identifier (e.g., "P3.1")
#' @param project_name Optional human-readable project name (defaults to project_code)
#' @param description Optional project description
#' @param path Full path where project should be created. If NULL and use_gui=TRUE,
#'   opens folder selection dialog. If NULL and use_gui=FALSE, throws error.
#' @param use_gui Whether to use GUI for folder selection when path=NULL (default: TRUE in interactive sessions)
#'
#' @details
#' This function creates a complete project structure including:
#' \itemize{
#'   \item Project root directory with .proman configuration file
#'   \item data/ (with source/ and cleaned/ subdirectories)
#'   \item r_programs/ for R scripts
#'   \item personal_notes/ (with analyseblitz/ and figures/ subdirectories)
#'   \item sources/ for reference materials
#'   \item old/ for archived files
#' }
#'
#' Each folder contains a hidden .proman_(foldername) marker file that allows
#' PRoMan to track folders even if they are moved within the project.
#'
#' The .proman file in the project root contains:
#' \itemize{
#'   \item Project metadata (code, name, description)
#'   \item Folder registry for fast path lookups
#'   \item Project status and statistics
#' }
#'
#' @return Invisibly returns the project path. Also sets the working directory
#'   to the new project via set_project().
#'
#' @examples
#' \dontrun{
#' # Interactive mode with GUI folder selection
#' create_project("P3.1", project_name = "COVID Analysis")
#'
#' # Programmatic mode with specified path
#' create_project("P3.1",
#'                project_name = "COVID Analysis",
#'                description = "Analysis of COVID-19 data",
#'                path = "/Desktop/projects/P3.1")
#'
#' # Force non-GUI mode (will error if path not specified)
#' create_project("P3.1", use_gui = FALSE)  # Error!
#' create_project("P3.1", path = "/projects/P3.1", use_gui = FALSE)  # Works!
#' }
#'
#' @export
create_project <- function(project_code = NULL,
                           project_name = NULL,
                           description = NULL,
                           path = NULL,
                           use_gui = interactive()) {

  # GUI code for getting project information if not provided
  if (is.null(project_code) && use_gui) {
    if (requireNamespace("tcltk", quietly = TRUE)) {
      # Try to create a GUI dialog for project information
      # Create a tcltk dialog window
      tt <- tcltk::tktoplevel()
      tcltk::tkwm.title(tt, "Create New PRoMan Project")

      # Force window to front
      tcltk::tkraise(tt)
      tcltk::tkfocus(tt)
      tcltk::.Tcl(paste("wm attributes", tcltk::.Tk.ID(tt), "-topmost 1"))

      # Variables for input
      project_code_var <- tcltk::tclVar("")
      project_name_var <- tcltk::tclVar("")
      description_var <- tcltk::tclVar("")

      # Create input fields
      tcltk::tkgrid(tcltk::tklabel(tt, text = "Project Code (e.g., P3.1):"), row = 0, column = 0, sticky = "w")
      code_entry <- tcltk::tkentry(tt, textvariable = project_code_var, width = 30)
      tcltk::tkgrid(code_entry, row = 0, column = 1, padx = 5, pady = 5)

      tcltk::tkgrid(tcltk::tklabel(tt, text = "Project Name (optional):"), row = 1, column = 0, sticky = "w")
      name_entry <- tcltk::tkentry(tt, textvariable = project_name_var, width = 30)
      tcltk::tkgrid(name_entry, row = 1, column = 1, padx = 5, pady = 5)

      tcltk::tkgrid(tcltk::tklabel(tt, text = "Description (optional):"), row = 2, column = 0, sticky = "w")
      desc_entry <- tcltk::tkentry(tt, textvariable = description_var, width = 30)
      tcltk::tkgrid(desc_entry, row = 2, column = 1, padx = 5, pady = 5)

      # OK/Cancel buttons
      ok_pressed <- FALSE
      ok_fun <- function() {
        ok_pressed <<- TRUE
        tcltk::tkdestroy(tt)
      }
      cancel_fun <- function() {
        tcltk::tkdestroy(tt)
      }

      tcltk::tkgrid(tcltk::tkbutton(tt, text = "OK", command = ok_fun),
                    row = 3, column = 0, padx = 5, pady = 10)
      tcltk::tkgrid(tcltk::tkbutton(tt, text = "Cancel", command = cancel_fun),
                    row = 3, column = 1, padx = 5, pady = 10)

      # Wait for window to close
      tcltk::tkwait.window(tt)

      if (ok_pressed) {
        project_code <- tcltk::tclvalue(project_code_var)
        project_name <- tcltk::tclvalue(project_name_var)
        description <- tcltk::tclvalue(description_var)

        if (project_code == "") {
          stop("Project creation cancelled - no project code provided")
        }
        if (project_name == "") project_name <- NULL
        if (description == "") description <- NULL
      } else {
        stop("Project creation cancelled")
      }

    } else {
      # Fallback to console if tcltk not available
      cat("=== CREATE NEW PROMAN PROJECT ===\n\n")
      project_code <- readline(prompt = "Enter project code (e.g., P3.1): ")
      if (project_code == "") {
        stop("Project creation cancelled - no project code provided")
      }

      project_name <- readline(prompt = "Enter project name (optional, press Enter to skip): ")
      if (project_name == "") project_name <- NULL

      description <- readline(prompt = "Enter project description (optional, press Enter to skip): ")
      if (description == "") description <- NULL
    }

  } else if (is.null(project_code) && !use_gui) {
    stop("project_code must be specified when use_gui = FALSE")
  }

  # Determine project path
  if (is.null(path)) {
    if (use_gui && interactive()) {
      # GUI mode - prompt for folder selection
      if (requireNamespace("tcltk", quietly = TRUE)) {
        message("\nPlease select the parent folder where the project should be created...")
        parent_path <- tcltk::tk_choose.dir(
          default = getwd(),
          caption = paste("Select parent folder for project", project_code)
        )

        if (is.na(parent_path) || parent_path == "") {
          stop("Project creation cancelled - no folder selected")
        }

        path <- file.path(parent_path, project_code)

      } else {
        stop("GUI folder selection requires tcltk package. Please specify path directly or install tcltk.")
      }

      # Confirm the path
      cat("\nProject will be created at:\n", path, "\n")
      cat("Proceed? (y/n): ")
      if (tolower(substr(readline(), 1, 1)) != "y") {
        stop("Project creation cancelled")
      }

    } else {
      # Non-interactive mode - error if no path specified
      stop("Path must be specified when use_gui = FALSE")
    }
  }

  # Create project directory
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Define ONLY the folders that should be in the registry
  folder_registry <- list(
    data_source = file.path("data", "source"),
    data_cleaned = file.path("data", "cleaned"),
    r_programs = "r_programs",
    personal_notes = "personal_notes",
    sources = "sources",
    old = "old"
  )

  # Additional subdirectories to create (but not track with markers)
  additional_dirs <- c(
    file.path("personal_notes", "analyseblitz"),
    file.path("personal_notes", "figures")
  )

  folder_ids <- list()

  # Create tracked folders with markers
  for (folder_name in names(folder_registry)) {
    folder_path <- file.path(path, folder_registry[[folder_name]])

    # Create folder
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }

    # Generate unique ID for this folder
    folder_id <- paste0("proman_",
                        substr(folder_name, 1, 3), "_",
                        paste0(sample(c(0:9, letters[1:6]), 8, replace = TRUE),
                               collapse = ""))
    folder_ids[[folder_name]] <- folder_id

    # Use the create_folder_marker function for consistency
    create_folder_marker(folder_path, folder_name, folder_id, project_code)
  }

  # Create additional subdirectories without markers
  for (dir_path in additional_dirs) {
    full_path <- file.path(path, dir_path)
    if (!dir.exists(full_path)) {
      dir.create(full_path, recursive = TRUE)
    }
  }

  # Create .proman file with proper structure
  proman_data <- list(
    # Project metadata
    project_code = project_code,
    project_name = null_coalesce(project_name, project_code),
    description = null_coalesce(description, ""),
    created_date = as.character(Sys.Date()),
    created_by = Sys.info()["user"],
    proman_version = as.character(packageVersion("PRoMan")),

    # Folder registry with IDs and paths
    folders = list(
      registry = folder_registry,
      ids = folder_ids,
      last_verified = as.character(Sys.time())
    ),

    # Project status
    status = list(
      current_stage = "planning",
      last_activity = as.character(Sys.time()),
      is_active = TRUE
    ),

    # Statistics
    statistics = list(
      total_files = 0,
      total_figures = 0,
      total_scripts = 0,
      last_updated = as.character(Sys.Date())
    ),

    # Required by fallback system
    last_fallback_time = NULL,

    # For Blitzschreiben integration
    project_info = list()
  )

  # Save .proman file
  proman_file <- file.path(path, ".proman")
  jsonlite::write_json(proman_data, proman_file,
                       pretty = TRUE, auto_unbox = TRUE)

  # Make .proman file hidden on Windows
  if (.Platform$OS.type == "windows") {
    shell(paste("attrib +h", shQuote(normalizePath(proman_file))),
          intern = FALSE, wait = TRUE)
  }

  message("✓ Created PRoMan project: ", project_code)
  message("  Location: ", path)
  message("  Created ", length(folder_registry), " tracked folders")

  # Set as active project
  set_project(path)

  return(invisible(path))
}


# Define the null-coalescing operator
#' Null-coalescing operator
#'
#' @param x First value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
null_coalesce <- function(x, y) if (is.null(x)) y else x

### ------------------------------------------------------------ ###
# CORE INFRASTRUCTURE FOR .PROMAN FILE ACCESS

# Package environment for caching
.proman_env <- new.env(parent = emptyenv())

#' Read .proman file with caching
#'
#' Core function that all other functions use to access project data
#'
#' @param project_root Optional project root path. If NULL, uses find_project_root()
#' @param force_refresh Force re-reading from disk
#' @return List with complete project configuration
#' @keywords internal
read_proman <- function(project_root = NULL, force_refresh = FALSE) {
  # Determine project root
  if (is.null(project_root)) {
    project_root <- find_project_root()
  }

  # Check cache first
  cache_key <- paste0("proman_", project_root)
  if (!force_refresh && exists(cache_key, envir = .proman_env)) {
    return(.proman_env[[cache_key]])
  }

  # Read from file
  proman_file <- file.path(project_root, ".proman")
  if (!file.exists(proman_file)) {
    stop("No .proman file found at: ", project_root)
  }

  # Read JSON and enhance data
  proman_data <- jsonlite::read_json(proman_file)
  proman_data$.root <- project_root  # Add root for convenience
  proman_data$.file_path <- proman_file  # Add file path for updates

  # Build full paths for all folders
  proman_data$.paths <- list(root = project_root)
  for (folder_name in names(proman_data$folders$registry)) {
    proman_data$.paths[[folder_name]] <- file.path(
      project_root,
      proman_data$folders$registry[[folder_name]]
    )
  }

  # Cache the enhanced data
  .proman_env[[cache_key]] <- proman_data

  return(proman_data)
}

#' Write updates to .proman file
#'
#' Handles the complexity of updating hidden files on Windows
#'
#' @param proman_data The modified proman data to write
#' @param clear_cache Whether to clear the cache after writing
#' @keywords internal
write_proman <- function(proman_data, clear_cache = TRUE) {
  proman_file <- proman_data$.file_path

  # Remove our added fields before writing
  data_to_write <- proman_data
  data_to_write$.root <- NULL
  data_to_write$.file_path <- NULL
  data_to_write$.paths <- NULL

  # Update last activity
  data_to_write$status$last_activity <- as.character(Sys.time())

  # Handle Windows hidden files
  if (.Platform$OS.type == "windows") {
    shell(paste("attrib -h", shQuote(normalizePath(proman_file))),
          intern = FALSE, wait = TRUE)
  }

  # Write the JSON file
  jsonlite::write_json(data_to_write, proman_file,
                       pretty = TRUE, auto_unbox = TRUE)

  # Re-hide on Windows
  if (.Platform$OS.type == "windows") {
    shell(paste("attrib +h", shQuote(normalizePath(proman_file))),
          intern = FALSE, wait = TRUE)
  }

  # Clear cache if requested
  if (clear_cache) {
    cache_key <- paste0("proman_", proman_data$.root)
    if (exists(cache_key, envir = .proman_env)) {
      rm(list = cache_key, envir = .proman_env)
    }
  }
}

#' Get project paths with automatic structure checking
#'
#' Returns paths for the current project, with folder tracking and recovery
#'
#' @param project_path Optional path to project root. If NULL, searches from current working directory.
#' @return A named list containing absolute paths to all project directories
#' @export
get_project_paths <- function(project_path = NULL) {
  # Use read_proman for consistency
  proman_data <- read_proman(project_path)

  # Check for moved folders and update if needed
  updates_needed <- FALSE

  for (folder_name in names(proman_data$folders$registry)) {
    expected_path <- proman_data$.paths[[folder_name]]
    folder_id <- proman_data$folders$ids[[folder_name]]

    # Check if folder exists at expected location with correct marker
    if (!dir.exists(expected_path) || !verify_folder_marker(expected_path, folder_name, folder_id)) {
      # Search for moved folder
      found_path <- search_for_folder(proman_data$.root, folder_name, folder_id)

      if (!is.null(found_path)) {
        # Update the path
        rel_path <- gsub(paste0("^", gsub("\\\\", "/", normalizePath(proman_data$.root)), "/"), "",
                         gsub("\\\\", "/", normalizePath(found_path)))
        proman_data$folders$registry[[folder_name]] <- rel_path
        proman_data$.paths[[folder_name]] <- found_path
        updates_needed <- TRUE
        message(paste("Found moved folder:", folder_name, "at", rel_path))
      } else {
        # Recreate missing folder
        dir.create(expected_path, recursive = TRUE, showWarnings = FALSE)
        create_folder_marker(expected_path, folder_name, folder_id, proman_data$project_code)
        message(paste("Recreated missing folder:", folder_name))
      }
    }
  }

  # Save updates if needed
  if (updates_needed) {
    write_proman(proman_data)
  }

  # Add attributes for backward compatibility
  paths <- proman_data$.paths
  attr(paths, "project_code") <- proman_data$project_code
  attr(paths, "project_name") <- proman_data$project_name

  # Track project access
  if(exists("track_project_access", mode = "function")) {
    track_project_access(proman_data$.root)
  }

  return(paths)
}

#' Verify folder marker
#'
#' @param folder_path Path to folder
#' @param folder_name Expected folder name
#' @param folder_id Expected folder ID
#' @return Logical indicating if marker is valid
#' @keywords internal
verify_folder_marker <- function(folder_path, folder_name, folder_id) {
  marker_file <- file.path(folder_path, paste0(".proman_", folder_name))
  if (!file.exists(marker_file)) return(FALSE)

  tryCatch({
    marker_data <- jsonlite::read_json(marker_file)
    return(marker_data$folder_id == folder_id)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Create folder marker
#'
#' @param folder_path Path to folder
#' @param folder_name Folder name
#' @param folder_id Folder ID
#' @param project_code Project code
#' @keywords internal
create_folder_marker <- function(folder_path, folder_name, folder_id, project_code) {
  marker_data <- list(
    folder_type = folder_name,
    folder_id = folder_id,
    project_code = project_code,
    created_date = as.character(Sys.Date()),
    relative_path = folder_name
  )

  marker_file <- file.path(folder_path, paste0(".proman_", folder_name))
  jsonlite::write_json(marker_data, marker_file,
                       pretty = TRUE, auto_unbox = TRUE)

  # Hide marker on Windows
  if (.Platform$OS.type == "windows") {
    shell(paste("attrib +h", shQuote(normalizePath(marker_file))),
          intern = FALSE, wait = TRUE)
  }
}

#' Set working directory to project root
#'
#' Sets the working directory and initializes the project environment
#'
#' @param project_path Path to project (NULL to auto-detect)
#' @return Project paths (invisibly)
#' @export
set_project <- function(project_path = NULL) {
  if (is.null(project_path)) {
    project_path <- find_project_root()
  }

  # Normalize the path
  project_path <- normalizePath(project_path, winslash = "/", mustWork = TRUE)

  # Set working directory
  setwd(project_path)

  # Force refresh of cache to ensure we have current data
  proman_data <- read_proman(project_path, force_refresh = TRUE)

  # Set a "current project" marker in the environment
  .proman_env$current_project <- project_path

  # Track project access for session logging
  if(exists("track_project_access", mode = "function")) {
    track_project_access(project_path)
  }

  message("Working directory set to: ", project_path)
  message("Project: ", proman_data$project_name, " (", proman_data$project_code, ")")

  invisible(proman_data$.paths)
}

#' Find project root by searching for .proman file
#'
#' Searches upward from current directory to find .proman file
#'
#' @param start_path Starting directory (default: current working directory)
#' @return Path to project root
#' @keywords internal
find_project_root <- function(start_path = getwd()) {
  # Normalize the starting path
  current_path <- normalizePath(start_path, winslash = "/", mustWork = FALSE)

  # Search upward for .proman file
  while (TRUE) {
    proman_file <- file.path(current_path, ".proman")

    if (file.exists(proman_file)) {
      return(current_path)
    }

    # Get parent directory
    parent_path <- dirname(current_path)

    # Stop if we've reached the root directory
    if (parent_path == current_path) {
      break
    }

    current_path <- parent_path
  }

  # If not found, check if we're in a subdirectory with project name
  # This handles the case where start_path might be a project name
  if (basename(start_path) == start_path && start_path != getwd()) {
    # It's just a name, not a path - look for it as a sibling directory
    possible_path <- file.path(dirname(getwd()), start_path)
    if (dir.exists(possible_path)) {
      proman_file <- file.path(possible_path, ".proman")
      if (file.exists(proman_file)) {
        return(possible_path)
      }
    }
  }

  stop("No .proman file found. Are you in a PRoMan project directory?")
}

