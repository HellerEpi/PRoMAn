# R/project_logging.R

#' Generate a project status log
#'
#' Creates a comprehensive log of recent project activity and current state
#'
#' @param days_back How many days back to look for changes (default: 7)
#' @param save_to_file Save log to file instead of just printing (default: TRUE)
#' @param log_folder Folder to save logs (default: personal_notes/logs)
#' @param project_paths Optional: pre-computed project paths (for exit logging)
#' @return Path to saved log file (if saved)
#' @export
generate_project_log <- function(days_back = 7, save_to_file = TRUE, log_folder = NULL, project_paths = NULL) {

  # Get paths - either passed in or computed
  if(is.null(project_paths)) {
    proman_data <- read_proman()
    paths <- list(
      root = proman_data$.paths$root,
      data_source = proman_data$.paths$data_source,
      data_cleaned = proman_data$.paths$data_cleaned,
      r_programs = proman_data$.paths$r_programs,
      sources = proman_data$.paths$sources,
      personal_notes = proman_data$.paths$personal_notes,
      old = proman_data$.paths$old
    )
  } else {
    paths <- project_paths
  }

  if(is.null(log_folder)) {
    log_folder <- file.path(paths$personal_notes, "logs")
    if(!dir.exists(log_folder)) {
      dir.create(log_folder, recursive = TRUE)
    }
  }

  # Create log content
  log_content <- c()
  log_content <- c(log_content, paste("PROJECT STATUS LOG -", basename(paths$root)))
  log_content <- c(log_content, paste("Generated:", Sys.time()))
  log_content <- c(log_content, paste(rep("=", 50), collapse = ""))
  log_content <- c(log_content, "")

  # Recent file activity
  recent_activity <- analyze_recent_activity(days_back, paths)
  log_content <- c(log_content, recent_activity)

  # Last fallback info from .proman
  # If we already have proman_data from earlier, use it
  if(is.null(project_paths)) {
    # We already read proman_data at the beginning
    if(!is.null(proman_data$last_fallback_time)) {
      log_content <- c(log_content, "", paste("Last fallback:", proman_data$last_fallback_time))
    }
  } else {
    # Need to read it since paths were passed in
    proman_data_temp <- read_proman(paths$root)
    if(!is.null(proman_data_temp$last_fallback_time)) {
      log_content <- c(log_content, "", paste("Last fallback:", proman_data_temp$last_fallback_time))
    }
  }


  # Current project structure
  project_tree <- generate_project_tree(paths)
  log_content <- c(log_content, "", "CURRENT PROJECT STRUCTURE:")
  log_content <- c(log_content, paste(rep("-", 30), collapse = ""))
  log_content <- c(log_content, project_tree)

  # File sets summary
  file_sets <- analyze_file_sets(project_paths = paths)
  log_content <- c(log_content, "", "FILE SETS DETECTED:")
  log_content <- c(log_content, paste(rep("-", 20), collapse = ""))
  log_content <- c(log_content, file_sets)

  # Activity inference
  activity_summary <- infer_recent_activity(days_back, paths)
  log_content <- c(log_content, "", "RECENT ACTIVITY SUMMARY:")
  log_content <- c(log_content, paste(rep("-", 25), collapse = ""))
  log_content <- c(log_content, activity_summary)

  # Print to console
  cat(paste(log_content, collapse = "\n"))

  # Save to file if requested
  if(save_to_file) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_filename <- paste0("project_log_", timestamp, ".txt")
    log_path <- file.path(log_folder, log_filename)

    writeLines(log_content, log_path)
    message(paste("\nLog saved to:", log_filename))
    return(invisible(log_path))
  }

  return(invisible(NULL))
}

#' Analyze recent file activity
#'
#' Internal function to detect file changes, additions, etc.
#'
#' @param days_back Number of days to look back
#' @param paths Project paths (optional, will get if not provided)
#' @return Character vector with activity description
analyze_recent_activity <- function(days_back = 7, paths = NULL) {
  if(is.null(paths)) {
    proman_data <- read_proman()
    paths <- list(
      root = proman_data$.paths$root,
      data_source = proman_data$.paths$data_source,
      data_cleaned = proman_data$.paths$data_cleaned,
      r_programs = proman_data$.paths$r_programs,
      sources = proman_data$.paths$sources,
      personal_notes = proman_data$.paths$personal_notes,
      old = proman_data$.paths$old
    )
  }

  cutoff_time <- Sys.time() - (days_back * 24 * 60 * 60)

  activity <- c("RECENT FILE ACTIVITY:")
  activity <- c(activity, paste(rep("-", 25), collapse = ""))

  # Check each main folder
  folders_to_check <- list(
    "Data (source)" = paths$data_source,
    "Data (cleaned)" = paths$data_cleaned,
    "R Programs" = paths$r_programs,
    "Sources" = paths$sources,
    "Personal Notes" = paths$personal_notes
  )

  total_changes <- 0

  for(folder_name in names(folders_to_check)) {
    folder_path <- folders_to_check[[folder_name]]

    if(dir.exists(folder_path)) {
      files <- list.files(folder_path, full.names = TRUE, recursive = FALSE)

      if(length(files) > 0) {
        file_times <- file.mtime(files)
        recent_files <- files[!is.na(file_times) & file_times > cutoff_time]

        if(length(recent_files) > 0) {
          activity <- c(activity, paste("  ", folder_name, ":", length(recent_files), "files modified"))
          total_changes <- total_changes + length(recent_files)

          # Show a few specific files if not too many
          if(length(recent_files) <= 3) {
            for(file in recent_files) {
              mod_time <- format(file.mtime(file), "%m/%d %H:%M")
              activity <- c(activity, paste("    -", basename(file), "(", mod_time, ")"))
            }
          }
        }
      }
    }
  }

  if(total_changes == 0) {
    activity <- c(activity, "  No recent file changes detected")
  }

  return(activity)
}

#' Generate a simple project tree view
#'
#' Creates a text-based tree of project structure with accurate file counts
#'
#' @param paths Project paths (optional, will get if not provided)
#' @return Character vector representing the tree
generate_project_tree <- function(paths = NULL) {
  if(is.null(paths)) {
    proman_data <- read_proman()
    paths <- list(
      root = proman_data$.paths$root,
      data_source = proman_data$.paths$data_source,
      data_cleaned = proman_data$.paths$data_cleaned,
      r_programs = proman_data$.paths$r_programs,
      sources = proman_data$.paths$sources,
      personal_notes = proman_data$.paths$personal_notes,
      old = proman_data$.paths$old
    )
  }

  tree <- c()
  tree <- c(tree, basename(paths$root))

  # Main folders
  folders <- list(
    "data/" = c("source/", "cleaned/"),
    "r_programs/" = character(0),
    "personal_notes/" = "analyseblitz/",
    "sources/" = character(0),
    "old/" = character(0)
  )

  for(folder in names(folders)) {
    folder_path <- file.path(paths$root, folder)

    if(dir.exists(folder_path)) {
      # Count only actual files, not subdirectories
      all_items <- list.files(folder_path, recursive = FALSE, full.names = TRUE)

      if(length(all_items) > 0) {
        file_info <- file.info(all_items)
        file_count <- sum(!file_info$isdir, na.rm = TRUE)  # Count only non-directories
      } else {
        file_count <- 0
      }

      tree <- c(tree, paste("├──", folder, "(", file_count, "files)"))

      # Add subfolders with their file counts
      subfolders <- folders[[folder]]
      for(subfolder in subfolders) {
        subfolder_path <- file.path(folder_path, subfolder)
        if(dir.exists(subfolder_path)) {
          sub_items <- list.files(subfolder_path, recursive = FALSE, full.names = TRUE)

          if(length(sub_items) > 0) {
            sub_file_info <- file.info(sub_items)
            sub_count <- sum(!sub_file_info$isdir, na.rm = TRUE)
          } else {
            sub_count <- 0
          }

          tree <- c(tree, paste("│   └──", subfolder, "(", sub_count, "files)"))
        }
      }
    }
  }

  return(tree)
}

#' Analyze and detect file sets in the project
#'
#' Groups files by common patterns to identify data sets
#'
#' @param folder_path Specific folder to analyze (optional)
#' @param show_files Show individual files in each set (default: TRUE)
#' @param return_data Return data structure instead of just printing (default: FALSE)
#' @param project_paths Optional: pre-computed project paths
#' @return Character vector describing detected file sets (or list if return_data = TRUE)
#' @export
analyze_file_sets <- function(folder_path = NULL, show_files = TRUE, return_data = FALSE, project_paths = NULL) {
  if(is.null(project_paths)) {
    proman_data <- read_proman()
    paths <- list(
      root = proman_data$.paths$root,
      data_source = proman_data$.paths$data_source,
      data_cleaned = proman_data$.paths$data_cleaned,
      r_programs = proman_data$.paths$r_programs,
      sources = proman_data$.paths$sources,
      personal_notes = proman_data$.paths$personal_notes,
      old = proman_data$.paths$old
    )
  } else {
    paths <- project_paths
  }

  sets_info <- c()
  all_sets <- list()  # Store actual set data

  # Determine which folders to analyze
  if(!is.null(folder_path)) {
    folders_to_analyze <- list("Specified Folder" = folder_path)
  } else {
    folders_to_analyze <- list(
      "Source Data" = paths$data_source,
      "Cleaned Data" = paths$data_cleaned
    )
  }

  for(folder_name in names(folders_to_analyze)) {
    folder_path <- folders_to_analyze[[folder_name]]

    if(dir.exists(folder_path)) {
      files <- list.files(folder_path)

      if(length(files) > 0) {
        # Detect sets by common patterns
        file_sets <- detect_file_sets(files)

        if(length(file_sets) > 0) {
          sets_info <- c(sets_info, paste(folder_name, ":"))
          all_sets[[folder_name]] <- file_sets

          for(set_name in names(file_sets)) {
            count <- length(file_sets[[set_name]])
            sets_info <- c(sets_info, paste("  -", set_name, ":", count, "files"))

            if(show_files) {
              # Show examples if reasonable number
              if(count <= 5) {
                for(file in file_sets[[set_name]]) {
                  sets_info <- c(sets_info, paste("    •", file))
                }
              } else {
                # Show first 3 and last 1
                sets_info <- c(sets_info, paste("    •", file_sets[[set_name]][1]))
                sets_info <- c(sets_info, paste("    •", file_sets[[set_name]][2]))
                sets_info <- c(sets_info, paste("    •", file_sets[[set_name]][3]))
                sets_info <- c(sets_info, paste("    • ...", count-4, "more files"))
                sets_info <- c(sets_info, paste("    •", file_sets[[set_name]][count]))
              }
            }
          }
          sets_info <- c(sets_info, "")
        }
      }
    }
  }

  if(length(sets_info) == 0) {
    sets_info <- c("No clear file sets detected")
  }

  # Don't print here - just return for use in logging
  if(return_data) {
    return(all_sets)
  } else {
    return(sets_info)
  }
}

#' Detect file sets by pattern matching
#'
#' Internal function to group files by common naming patterns
#'
#' @param files Vector of filenames
#' @return Named list of file sets
detect_file_sets <- function(files) {

  # Remove extensions for pattern matching
  base_names <- tools::file_path_sans_ext(files)

  # Common patterns to look for
  patterns <- list(
    "State files" = "^.*_[A-Z]{2}(_.*)?$",  # Files ending with _XX (state codes)
    "Count files" = ".*count.*",
    "Summary files" = ".*summar.*",
    "Clean files" = ".*clean.*",
    "Raw files" = ".*raw.*",
    "Temp files" = ".*(temp|tmp).*"
  )

  detected_sets <- list()
  used_files <- character(0)

  # Look for each pattern
  for(pattern_name in names(patterns)) {
    pattern <- patterns[[pattern_name]]
    matching_files <- files[grepl(pattern, files, ignore.case = TRUE)]

    # Only count as a set if we have multiple files
    if(length(matching_files) > 1) {
      # Remove files already assigned to other sets
      new_matches <- setdiff(matching_files, used_files)
      if(length(new_matches) > 1) {
        detected_sets[[pattern_name]] <- new_matches
        used_files <- c(used_files, new_matches)
      }
    }
  }

  # Look for custom patterns (files with common prefixes)
  remaining_files <- setdiff(files, used_files)
  if(length(remaining_files) > 2) {
    prefix_sets <- detect_prefix_sets(remaining_files)
    detected_sets <- c(detected_sets, prefix_sets)
  }

  return(detected_sets)
}

#' Detect sets by common filename prefixes
#'
#' Groups remaining files by shared prefixes
#'
#' @param files Vector of filenames
#' @return Named list of prefix-based sets
detect_prefix_sets <- function(files) {

  prefix_sets <- list()

  # Extract potential prefixes (everything before first underscore or number)
  prefixes <- gsub("^([a-zA-Z]+)[_0-9].*", "\\1", tools::file_path_sans_ext(files))

  # Find prefixes that appear multiple times
  prefix_counts <- table(prefixes)
  common_prefixes <- names(prefix_counts)[prefix_counts > 1]

  for(prefix in common_prefixes) {
    if(nchar(prefix) > 2) {  # Only meaningful prefixes
      matching_files <- files[grepl(paste0("^", prefix), tools::file_path_sans_ext(files))]
      if(length(matching_files) > 1) {
        set_name <- paste0(prefix, " series")
        prefix_sets[[set_name]] <- matching_files
      }
    }
  }

  return(prefix_sets)
}

#' Infer what activities have been happening based on file changes
#'
#' Analyzes recent changes to guess what type of work has been done
#'
#' @param days_back Number of days to analyze
#' @param paths Project paths (optional, will get if not provided)
#' @return Character vector with activity inferences
infer_recent_activity <- function(days_back = 7, paths = NULL) {
  if(is.null(paths)) {
    proman_data <- read_proman()
    paths <- list(
      root = proman_data$.paths$root,
      data_source = proman_data$.paths$data_source,
      data_cleaned = proman_data$.paths$data_cleaned,
      r_programs = proman_data$.paths$r_programs,
      sources = proman_data$.paths$sources,
      personal_notes = proman_data$.paths$personal_notes,
      old = proman_data$.paths$old
    )
  }

  cutoff_time <- Sys.time() - (days_back * 24 * 60 * 60)

  inferences <- c()

  # Analyze each folder for activity type
  activity_indicators <- list()

  # Data source activity
  if(dir.exists(paths$data_source)) {
    source_files <- list.files(paths$data_source, full.names = TRUE)
    if(length(source_files) > 0) {
      recent_source <- source_files[file.mtime(source_files) > cutoff_time]
      if(length(recent_source) > 0) {
        activity_indicators[["data_input"]] <- length(recent_source)
      }
    }
  }

  # Data cleaned activity
  if(dir.exists(paths$data_cleaned)) {
    cleaned_files <- list.files(paths$data_cleaned, full.names = TRUE)
    if(length(cleaned_files) > 0) {
      recent_cleaned <- cleaned_files[file.mtime(cleaned_files) > cutoff_time]
      if(length(recent_cleaned) > 0) {
        activity_indicators[["data_processing"]] <- length(recent_cleaned)
      }
    }
  }

  # R programs activity
  if(dir.exists(paths$r_programs)) {
    r_files <- list.files(paths$r_programs, pattern = "\\.R$", full.names = TRUE)
    if(length(r_files) > 0) {
      recent_r <- r_files[file.mtime(r_files) > cutoff_time]
      if(length(recent_r) > 0) {
        activity_indicators[["analysis"]] <- length(recent_r)
      }
    }
  }

  # Sources activity (PDFs, references)
  if(dir.exists(paths$sources)) {
    source_docs <- list.files(paths$sources, pattern = "\\.(pdf|doc|docx)$", full.names = TRUE, ignore.case = TRUE)
    if(length(source_docs) > 0) {
      recent_docs <- source_docs[file.mtime(source_docs) > cutoff_time]
      if(length(recent_docs) > 0) {
        activity_indicators[["research"]] <- length(recent_docs)
      }
    }
  }

  # Personal notes activity
  if(dir.exists(paths$personal_notes)) {
    note_files <- list.files(paths$personal_notes, pattern = "\\.(qmd|rmd|txt|md)$", full.names = TRUE, ignore.case = TRUE)
    if(length(note_files) > 0) {
      recent_notes <- note_files[file.mtime(note_files) > cutoff_time]
      if(length(recent_notes) > 0) {
        activity_indicators[["documentation"]] <- length(recent_notes)
      }
    }
  }

  # Generate inferences based on activity patterns
  if(length(activity_indicators) == 0) {
    inferences <- c("No significant recent activity detected")
  } else {

    # Data work
    if(!is.null(activity_indicators[["data_input"]]) && !is.null(activity_indicators[["data_processing"]])) {
      inferences <- c(inferences, paste("[DATA] Data pipeline active:", activity_indicators[["data_input"]], "new source files,", activity_indicators[["data_processing"]], "processed files"))
    } else if(!is.null(activity_indicators[["data_input"]])) {
      inferences <- c(inferences, paste("[INPUT] Data collection phase:", activity_indicators[["data_input"]], "new source files added"))
    } else if(!is.null(activity_indicators[["data_processing"]])) {
      inferences <- c(inferences, paste("[PROCESS] Data processing active:", activity_indicators[["data_processing"]], "files processed"))
    }

    # Analysis work
    if(!is.null(activity_indicators[["analysis"]])) {
      inferences <- c(inferences, paste("[ANALYSIS] Analysis in progress:", activity_indicators[["analysis"]], "R scripts modified"))
    }

    # Research work
    if(!is.null(activity_indicators[["research"]])) {
      inferences <- c(inferences, paste("[RESEARCH] Research phase:", activity_indicators[["research"]], "new reference documents"))
    }

    # Documentation work
    if(!is.null(activity_indicators[["documentation"]])) {
      inferences <- c(inferences, paste("[DOCS] Documentation active:", activity_indicators[["documentation"]], "notes/reports updated"))
    }

    # Pattern-based insights
    if(!is.null(activity_indicators[["data_input"]]) && !is.null(activity_indicators[["analysis"]])) {
      inferences <- c(inferences, "[WORKFLOW] Full workflow active: collecting data AND running analysis")
    }

    if(!is.null(activity_indicators[["research"]]) && !is.null(activity_indicators[["analysis"]])) {
      inferences <- c(inferences, "[INSIGHT] Research-driven analysis: adding sources while analyzing")
    }
  }

  return(inferences)
}

#' Quick project status check
#'
#' Shortened version of the full log for quick status updates
#'
#' @export
quick_project_status <- function() {
  cat("Quick Project Status:\n")
  cat(paste(rep("=", 20), collapse = ""), "\n")

  # Just show recent activity and current file counts
  recent_activity <- analyze_recent_activity(3)  # Last 3 days
  activity_summary <- infer_recent_activity(3)

  # Show just the summary lines
  cat(paste(recent_activity[1:3], collapse = "\n"), "\n\n")
  cat("Recent work patterns:\n")
  cat(paste(activity_summary, collapse = "\n"), "\n")
}

#' Package startup hook
#'
#' Automatically sets up exit logging when PRoMAn is loaded
#'
#' @param libname Library name
#' @param pkgname Package name
.onAttach <- function(libname, pkgname) {

  if(interactive()) {
    tryCatch({

      # Initialize session project tracking (keep this!)
      assign(".proman_session", list(
        projects_worked_on = character(0),
        session_start = Sys.time(),
        last_project = NULL
      ), envir = .GlobalEnv)

      # Set up the smart exit logging (replaces the tcltk detection)
      assign(".Last", function() {
        PRoMAn:::enhanced_exit_logging_smart()
      }, envir = .GlobalEnv)

      packageStartupMessage("PRoMAn loaded | Project Management in R (Academic) v. 0.6.0.9000")

    }, error = function(e) {
      packageStartupMessage("PRoMAn loaded.")
    })
  }
}

#' Track project access for session logging
#'
#' Internal function called when project is accessed
#'
#' @param project_path Current project path
track_project_access <- function(project_path) {
  if(exists(".proman_session", envir = .GlobalEnv)) {
    session_info <- get(".proman_session", envir = .GlobalEnv)

    project_name <- basename(project_path)

    # Initialize project_paths if it doesn't exist
    if(is.null(session_info$project_paths)) {
      session_info$project_paths <- list()
    }

    # Store the ACTUAL path used, not just the name
    session_info$project_paths[[project_name]] <- project_path

    # Keep the old projects_worked_on for compatibility
    if(!project_name %in% session_info$projects_worked_on) {
      session_info$projects_worked_on <- c(session_info$projects_worked_on, project_name)
    }

    session_info$last_project <- project_name
    session_info$last_access_time <- Sys.time()

    assign(".proman_session", session_info, envir = .GlobalEnv)
  }
}

#' Enhanced text input GUI with multi-line prompt support
#'
#' Helper function to get text input from user via GUI
#'
#' @param title Dialog title
#' @param prompt Prompt text
#' @param width Entry width (default: 50)
#' @return User input string
get_text_input_gui <- function(title = "Input", prompt = "Enter text:", width = 50) {

  if(!requireNamespace("tcltk", quietly = TRUE)) {
    return("")
  }

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, title)

  text_var <- tcltk::tclVar("")
  result <- ""

  # Main frame with padding
  main_frame <- tcltk::tkframe(tt)
  tcltk::tkpack(main_frame, fill = "both", expand = TRUE, padx = 15, pady = 15)

  # Force window to front
  tcltk::tkraise(tt)
  tcltk::tkfocus(tt)
  tcltk::.Tcl(paste("wm attributes", tcltk::.Tk.ID(tt), "-topmost 1"))

  # Handle multi-line prompts
  prompt_lines <- strsplit(prompt, "\n")[[1]]
  for(line in prompt_lines) {
    if(nchar(line) > 0) {
      prompt_label <- tcltk::tklabel(main_frame, text = line)
      tcltk::tkpack(prompt_label, anchor = "w", pady = c(0, 2))
    }
  }

  # Add some space before the text entry
  tcltk::tkpack(tcltk::tklabel(main_frame, text = ""), pady = 5)

  # Text entry - make it bigger for notes
  entry_frame <- tcltk::tkframe(main_frame)
  tcltk::tkpack(entry_frame, fill = "both", expand = TRUE)

  # Use text widget instead of entry for multi-line input
  text_widget <- tcltk::tktext(entry_frame, width = width, height = 6, wrap = "word")
  tcltk::tkpack(text_widget, fill = "both", expand = TRUE)

  # Button frame
  button_frame <- tcltk::tkframe(main_frame)
  tcltk::tkpack(button_frame, side = "bottom", pady = c(10, 0))

  # Submit function
  submit_func <- function() {
    result <<- tcltk::tclvalue(tcltk::tkget(text_widget, "1.0", "end-1c"))
    tcltk::tkdestroy(tt)
  }

  # Skip function
  cancel_func <- function() {
    result <<- ""
    tcltk::tkdestroy(tt)
  }

  # Create buttons
  submit_btn <- tcltk::tkbutton(button_frame, text = "Save Note", command = submit_func, width = 12)
  tcltk::tkpack(submit_btn, side = "left", padx = 5)

  skip_btn <- tcltk::tkbutton(button_frame, text = "Skip", command = cancel_func, width = 12)
  tcltk::tkpack(skip_btn, side = "left", padx = 5)

  # Focus on text widget
  tcltk::tkfocus(text_widget)

  # Bind Ctrl+Enter to submit (since Enter adds new line in text widget)
  tcltk::tkbind(text_widget, "<Control-Return>", submit_func)
  tcltk::tkbind(tt, "<Escape>", cancel_func)

  tcltk::tkwait.window(tt)

  return(result)
}

#' Alternative: Simple console-based input for RStudio users
#'
#' Fallback function that uses console input when GUI has focus issues
#'
#' @param prompt Prompt text
#' @return User input string
get_text_input_console <- function(prompt = "Enter text:") {
  cat(paste(prompt, " "))
  return(readline())
}

#' Smart input function that chooses best method
#'
#' Automatically chooses GUI or console based on environment
#'
#' @param title Dialog title (GUI only)
#' @param prompt Prompt text
#' @param prefer_console Force console input (default: FALSE)
#' @return User input string
get_text_input_smart <- function(title = "Input", prompt = "Enter text:", prefer_console = FALSE) {

  # Try GUI first (unless explicitly told not to)
  if(!prefer_console && requireNamespace("tcltk", quietly = TRUE)) {
    tryCatch({
      return(get_text_input_gui(title, prompt))
    }, error = function(e) {
      # If GUI fails, fall back to console
      message("GUI unavailable, using console input:")
      return(readline(prompt = paste(prompt, "")))
    })
  } else {
    # Console input
    return(readline(prompt = paste(prompt, "")))
  }
}

#' Enhanced exit logging with smart input method
#'
#' Uses the best input method for the current environment
enhanced_exit_logging_smart <- function() {

  if(!exists(".proman_session", envir = .GlobalEnv)) {
    return(invisible(NULL))
  }

  session_info <- get(".proman_session", envir = .GlobalEnv)

  if(length(session_info$projects_worked_on) == 0) {
    return(invisible(NULL))
  }

  # Calculate session info
  session_duration <- round(as.numeric(difftime(Sys.time(), session_info$session_start, units = "hours")), 1)
  project_list <- paste(session_info$projects_worked_on, collapse = ", ")

  # Try GUI first
  use_gui <- TRUE

  if(use_gui && requireNamespace("tcltk", quietly = TRUE)) {
    tryCatch({
      # Create a more informative GUI dialog for the initial question
      response <- get_yes_no_gui(
        title = "R Session Ending - PRoMAn",
        message = paste0(
          "You worked on these projects:\n",
          paste("  * ", session_info$projects_worked_on, collapse = "\n"), "\n\n",
          "Session duration: ", session_duration, " hours\n\n",
          "Would you like to log your progress and next steps?"
        )
      )

      if(response) {
        # Process each project with GUI
        for(project_name in session_info$projects_worked_on) {

          # Get comment using GUI with better prompt
          comment <- get_text_input_gui(
            title = paste("PRoMAn -", project_name),
            prompt = paste0(
              "Project: ", project_name, "\n\n",
              "What did you accomplish?\n",
              "What are the next steps?\n",
              "Any ideas to remember?\n\n",
              "Enter your notes:")
          )

          # Generate log if we got a comment
          if(nchar(trimws(comment)) > 0) {
            generate_log_for_project(project_name, comment, session_info)
          }
        }

        # Show completion message
        show_completion_gui(session_info$projects_worked_on)
      }

    }, error = function(e) {
      # Fall back to console if GUI fails
      message("GUI failed, using console logging...")
      enhanced_exit_logging_console(session_info, session_duration, project_list)
    })
  } else {
    # Use console version
    enhanced_exit_logging_console(session_info, session_duration, project_list)
  }

  # Clean up session tracking
  remove(".proman_session", envir = .GlobalEnv)
}

# Console fallback version (the original logic)
enhanced_exit_logging_console <- function(session_info, session_duration, project_list) {

  cat("\n=== R SESSION ENDING ===\n")
  cat("Projects worked on this session:\n")
  for(i in seq_along(session_info$projects_worked_on)) {
    cat(paste(i, ".", session_info$projects_worked_on[i], "\n"))
  }
  cat(paste("Session duration:", session_duration, "hours\n\n"))

  cat("Generate project logs with comments? (y/n): ")
  response <- readline()

  if(tolower(substr(response, 1, 1)) == "y") {
    for(project_name in session_info$projects_worked_on) {
      cat(paste("\nNext steps/ideas for", project_name, ": "))
      comment <- readline()

      if(nchar(trimws(comment)) > 0) {
        generate_log_for_project(project_name, comment, session_info)
      }
    }
    cat("\n[COMPLETE] Multi-project logging completed!\n")
  }
}

# Helper function to generate log for a single project
generate_log_for_project <- function(project_name, comment, session_info) {

  # Get project path from session info or search for it
  project_path <- NULL

  if(!is.null(session_info$project_paths) && !is.null(session_info$project_paths[[project_name]])) {
    project_path <- session_info$project_paths[[project_name]]
  } else {
    # Search for project using find_project_root (which exists in another file)
    project_path <- find_project_root(project_name)
  }

  if(!is.null(project_path)) {
    tryCatch({
      # Use read_proman to get project data
      proman_data <- read_proman(project_path)
      paths <- proman_data$.paths

      # Generate the log with comment, passing the paths
      log_path <- generate_project_log_with_comment(
        days_back = 1,
        user_comment = comment,
        save_to_file = TRUE,
        project_paths = paths
      )

      # Return the log filename for GUI display
      return(basename(log_path))

    }, error = function(e) {
      message(paste("Error logging", project_name, ":", e$message))
      return(NULL)
    })
  } else {
    message(paste("Could not find project directory for", project_name))
    return(NULL)
  }
}

# GUI helper for yes/no dialog
get_yes_no_gui <- function(title = "Question", message = "Continue?") {

  if(!requireNamespace("tcltk", quietly = TRUE)) {
    return(FALSE)
  }

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, title)

  result <- FALSE

  # Message frame
  msg_frame <- tcltk::tkframe(tt)
  tcltk::tkpack(msg_frame, fill = "both", expand = TRUE, padx = 20, pady = 20)

  # Split message by newlines for better display
  msg_lines <- strsplit(message, "\n")[[1]]
  for(line in msg_lines) {
    label <- tcltk::tklabel(msg_frame, text = line)
    tcltk::tkpack(label, anchor = "w")
  }

  # Button frame
  btn_frame <- tcltk::tkframe(tt)
  tcltk::tkpack(btn_frame, side = "bottom", pady = 15)

  # Button functions
  yes_func <- function() {
    result <<- TRUE
    tcltk::tkdestroy(tt)
  }

  no_func <- function() {
    result <<- FALSE
    tcltk::tkdestroy(tt)
  }

  # Create buttons
  yes_btn <- tcltk::tkbutton(btn_frame, text = "Yes", command = yes_func, width = 10)
  no_btn <- tcltk::tkbutton(btn_frame, text = "No", command = no_func, width = 10)

  tcltk::tkpack(yes_btn, side = "left", padx = 5)
  tcltk::tkpack(no_btn, side = "left", padx = 5)

  # Bind Enter to Yes, Escape to No
  tcltk::tkbind(tt, "<Return>", yes_func)
  tcltk::tkbind(tt, "<Escape>", no_func)

  tcltk::tkfocus(yes_btn)
  tcltk::tkwait.window(tt)

  return(result)
}

# Show completion message with log filenames
show_completion_gui <- function(projects) {

  if(!requireNamespace("tcltk", quietly = TRUE)) {
    return()
  }

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "PRoMAn - Logging Complete")

  # Message frame
  msg_frame <- tcltk::tkframe(tt)
  tcltk::tkpack(msg_frame, fill = "both", expand = TRUE, padx = 20, pady = 20)

  # Success message
  success_label <- tcltk::tklabel(msg_frame, text = "[SUCCESS] Project logs created successfully!",
                                  font = tcltk::tkfont.create(weight = "bold"))
  tcltk::tkpack(success_label, pady = c(0, 10))

  # Project list
  for(project in projects) {
    proj_label <- tcltk::tklabel(msg_frame, text = paste("  *", project, "logged"))
    tcltk::tkpack(proj_label, anchor = "w")
  }

  # Info message
  info_label <- tcltk::tklabel(msg_frame,
                               text = "\nLogs saved in each project's personal_notes/logs folder")
  tcltk::tkpack(info_label, pady = c(10, 0))

  # OK button
  ok_func <- function() {
    tcltk::tkdestroy(tt)
  }

  ok_btn <- tcltk::tkbutton(tt, text = "OK", command = ok_func, width = 10)
  tcltk::tkpack(ok_btn, pady = 10)

  tcltk::tkbind(tt, "<Return>", ok_func)
  tcltk::tkbind(tt, "<Escape>", ok_func)

  tcltk::tkfocus(ok_btn)
  tcltk::tkwait.window(tt)
}

#' Generate project log with user comment
#'
#' Enhanced version that includes user's next steps/ideas
#'
#' @param days_back How many days back to look for changes
#' @param user_comment User's comment about next steps
#' @param save_to_file Save log to file
#' @param log_folder Folder to save logs
#' @param project_paths Optional: pre-computed project paths (for exit logging)
#' @export
generate_project_log_with_comment <- function(days_back = 7, user_comment = "", save_to_file = TRUE, log_folder = NULL, project_paths = NULL) {

  # Get paths - either passed in or computed
  if(is.null(project_paths)) {
    proman_data <- read_proman()
    paths <- list(
      root = proman_data$.paths$root,
      data_source = proman_data$.paths$data_source,
      data_cleaned = proman_data$.paths$data_cleaned,
      r_programs = proman_data$.paths$r_programs,
      sources = proman_data$.paths$sources,
      personal_notes = proman_data$.paths$personal_notes,
      old = proman_data$.paths$old
    )
  } else {
    paths <- project_paths
  }

  if(is.null(log_folder)) {
    log_folder <- file.path(paths$personal_notes, "logs")
    if(!dir.exists(log_folder)) {
      dir.create(log_folder, recursive = TRUE)
    }
  }

  # Create log content (using existing functions)
  log_content <- c()
  log_content <- c(log_content, paste("PROJECT STATUS LOG -", basename(paths$root)))
  log_content <- c(log_content, paste("Generated:", Sys.time()))
  log_content <- c(log_content, paste(rep("=", 50), collapse = ""))
  log_content <- c(log_content, "")

  # Add user comment section if provided
  if(nchar(trimws(user_comment)) > 0) {
    log_content <- c(log_content, "USER NOTES & NEXT STEPS:")
    log_content <- c(log_content, paste(rep("-", 25), collapse = ""))
    log_content <- c(log_content, user_comment)
    log_content <- c(log_content, "")
  }

  # Regular log content - pass paths to all functions
  recent_activity <- analyze_recent_activity(days_back, paths)
  log_content <- c(log_content, recent_activity)

  # Last fallback info from .proman
  if(is.null(project_paths)) {
    # We already have proman_data from earlier
    if(!is.null(proman_data$last_fallback_time)) {
      log_content <- c(log_content, "", paste("Last fallback:", proman_data$last_fallback_time))
    }
  } else {
    # Need to read it since paths were passed in
    proman_data_temp <- read_proman(paths$root)
    if(!is.null(proman_data_temp$last_fallback_time)) {
      log_content <- c(log_content, "", paste("Last fallback:", proman_data_temp$last_fallback_time))
    }
  }

  project_tree <- generate_project_tree(paths)
  log_content <- c(log_content, "", "CURRENT PROJECT STRUCTURE:")
  log_content <- c(log_content, paste(rep("-", 30), collapse = ""))
  log_content <- c(log_content, project_tree)

  file_sets <- analyze_file_sets(project_paths = paths)
  log_content <- c(log_content, "", "FILE SETS DETECTED:")
  log_content <- c(log_content, paste(rep("-", 20), collapse = ""))
  log_content <- c(log_content, file_sets)

  activity_summary <- infer_recent_activity(days_back, paths)
  log_content <- c(log_content, "", "RECENT ACTIVITY SUMMARY:")
  log_content <- c(log_content, paste(rep("-", 25), collapse = ""))
  log_content <- c(log_content, activity_summary)

  # Print to console
  cat(paste(log_content, collapse = "\n"))

  # Save to file if requested
  if(save_to_file) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_filename <- paste0("project_log_", timestamp, ".txt")
    log_path <- file.path(log_folder, log_filename)

    writeLines(log_content, log_path)

    message(paste("\nLog saved to:", log_filename))

    return(invisible(log_path))
  }

  return(invisible(NULL))
}


