
# R/workflows.R
#
# Enables marking analysis code sections as reusable workflows
# that can be applied to file sets with intelligent I/O handling

### ------------------------------------------------------------ ###
### WORKFLOW EXTRACTION AND REGISTRATION

#' Extract workflows from script with intelligent I/O detection
#'
#' Finds workflow sections marked with #WORKFLOW_START and #WORKFLOW_END
#' and registers them for reuse on file sets
#'
#' @param script_path Path to script (NULL for active document in RStudio)
#' @param auto_register Automatically register found workflows
#' @param verbose Show detailed extraction information
#' @return List of extracted workflows (invisibly)
#' @export
extract_workflows <- function(script_path = NULL, auto_register = TRUE, verbose = TRUE) {

  paths <- get_project_paths()

  # Get script path
  if(is.null(script_path)) {
    if(rstudioapi::isAvailable()) {
      script_path <- rstudioapi::getActiveDocumentContext()$path
      if(script_path == "") {
        stop("Please save your script first")
      }
    } else {
      stop("Please provide script_path")
    }
  }

  lines <- readLines(script_path)

  # Find workflow markers
  start_pattern <- "^\\s*#WORKFLOW_START:\\s*(.+)$"
  end_pattern <- "^\\s*#WORKFLOW_END"

  starts <- grep(start_pattern, lines)
  ends <- grep(end_pattern, lines)

  if(length(starts) == 0) {
    if(verbose) {
      cat("No workflows found. To create a workflow:\n\n")
      cat("#WORKFLOW_START: workflow_name, Description of what it does\n")
      cat("data <- read.csv(FILE_PATH)\n")
      cat("# Your analysis code here\n")
      cat("result <- your_output  # Optional return value\n")
      cat("#WORKFLOW_END\n")
    }
    return(invisible(NULL))
  }

  if(length(starts) != length(ends)) {
    stop("Mismatched WORKFLOW_START and WORKFLOW_END markers")
  }

  workflows <- list()

  if(verbose) {
    cat("📋 Extracting workflows from:", basename(script_path), "\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }

  for(i in seq_along(starts)) {
    # Parse marker line
    marker_line <- lines[starts[i]]
    marker_content <- gsub(start_pattern, "\\1", marker_line)

    # Parse name and description
    parts <- strsplit(marker_content, ",", fixed = TRUE)[[1]]
    workflow_name <- trimws(parts[1])
    description <- if(length(parts) > 1) trimws(paste(parts[-1], collapse = ",")) else ""

    # Extract code
    code_lines <- lines[(starts[i] + 1):(ends[i] - 1)]

    # Detect output mode
    output_mode <- detect_output_mode(code_lines)

    # Detect I/O with enhanced parsing
    io_info <- detect_workflow_io(code_lines)

    # Detect required packages
    required_packages <- detect_required_packages(code_lines)

    #find results
    has_result <- any(grepl("^\\s*result\\s*<-", code_lines))

    if(!has_result && verbose) {
      cat("   ⚠️  No 'result' assignment found\n")
      cat("      Add 'result <- your_output' to save workflow output\n")
    }

    # Store this in the workflow object
    workflow <- list(
      name = workflow_name,
      description = description,
      code = code_lines,
      output_mode = output_mode,
      io = io_info,
      required_packages = required_packages,
      source_file = script_path,
      line_range = c(starts[i], ends[i]),
      created = Sys.time(),
      has_result = has_result
    )

    workflows[[workflow_name]] <- workflow

    if(verbose) {
      cat("📌 Workflow:", workflow_name, "\n")
      if(nchar(description) > 0) cat("   Description:", description, "\n")
      cat("   Lines:", starts[i], "-", ends[i],
          "(", length(code_lines), "lines)\n")
      cat("   Output mode:", output_mode, "\n")
      if(length(required_packages) > 0) {
        cat("   Required packages:", paste(required_packages, collapse = ", "), "\n")
      }
      if(length(io_info$saves) > 0) {
        cat("   Saves:", length(io_info$saves), "outputs\n")
      }

      if(auto_register) {
        register_workflow_internal(workflow)
        cat("   ✅ Registered\n")
      }
      cat("\n")
    }
  }

  if(verbose) {
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat("Extracted", length(workflows), "workflow(s)\n")
  }

  # Store for later use
  assign(".last_extracted_workflows", workflows, envir = .GlobalEnv)

  return(invisible(workflows))
}


#' Detect output mode from workflow code
#'
#' @param code_lines Lines of workflow code
#' @return "single" or "file_set"
detect_output_mode <- function(code_lines) {
  # Look for explicit mode marker
  mode_line <- grep("#OUTPUT_MODE:", code_lines, value = TRUE)
  if(length(mode_line) > 0) {
    if(grepl("single", mode_line[1], ignore.case = TRUE)) return("single")
    if(grepl("file_set", mode_line[1], ignore.case = TRUE)) return("file_set")
  }

  # Default
  return("file_set")
}


#' Detect required packages from workflow code
#'
#' @param code_lines Lines of workflow code
#' @return Character vector of required packages
detect_required_packages <- function(code_lines) {
  packages <- character()

  # Join lines for better detection
  full_code <- paste(code_lines, collapse = "\n")

  # Direct library/require calls
  lib_pattern <- "(?:library|require)\\s*\\(\\s*['\"]?([^)'\"\\s]+)['\"]?\\s*\\)"
  lib_matches <- gregexpr(lib_pattern, full_code, perl = TRUE)
  lib_results <- regmatches(full_code, lib_matches)[[1]]

  for(match in lib_results) {
    pkg_match <- regmatches(match, regexec(lib_pattern, match, perl = TRUE))[[1]]
    if(length(pkg_match) > 1) {
      packages <- c(packages, pkg_match[2])
    }
  }

  # Package::function calls
  pkg_pattern <- "([a-zA-Z][a-zA-Z0-9.]*)::"
  pkg_matches <- gregexpr(pkg_pattern, full_code, perl = TRUE)
  pkg_results <- regmatches(full_code, pkg_matches)[[1]]

  for(match in pkg_results) {
    pkg_name <- sub("::", "", match)
    packages <- c(packages, pkg_name)
  }

  # Check for common functions that imply packages
  if(any(grepl("read_excel", full_code))) packages <- c(packages, "readxl")
  if(any(grepl("read_csv|write_csv", full_code))) packages <- c(packages, "readr")
  if(any(grepl("%>%", full_code))) packages <- c(packages, "magrittr")
  if(any(grepl("ggplot", full_code))) packages <- c(packages, "ggplot2")
  if(any(grepl("case_when|mutate|filter|select|arrange", full_code))) packages <- c(packages, "dplyr")

  return(unique(packages))
}


#' Detect inputs and outputs in workflow code with enhanced parsing
#'
#' @param code_lines Lines of workflow code
#' @return List with I/O information
detect_workflow_io <- function(code_lines) {
  io <- list(
    inputs = list(),
    outputs = list(),
    saves = list(),
    creates_plots = FALSE,
    saves_files = FALSE
  )

  # Join code lines for better pattern matching across line breaks
  full_code <- paste(code_lines, collapse = "\n")

  # Try to parse the code for better detection
  parsed_ok <- FALSE
  tryCatch({
    parsed <- parse(text = full_code)
    parsed_ok <- TRUE
  }, error = function(e) {
    # Fall back to regex-based detection
    warning("Could not parse workflow code for enhanced I/O detection: ", e$message)
  })

  # Detect inputs
  if(any(grepl("FILE_PATH", code_lines))) {
    read_patterns <- c(
      "read\\.csv.*FILE_PATH" = list(type = "csv", read_function = "read.csv"),
      "read_csv.*FILE_PATH" = list(type = "csv", read_function = "read_csv"),
      "readRDS.*FILE_PATH" = list(type = "rds", read_function = "readRDS"),
      "read_excel.*FILE_PATH" = list(type = "excel", read_function = "read_excel"),
      "readxl::read_excel.*FILE_PATH" = list(type = "excel", read_function = "readxl::read_excel")
    )

    for(pattern in names(read_patterns)) {
      if(grepl(pattern, full_code)) {
        io$inputs <- read_patterns[[pattern]]
        break
      }
    }
  }

  # Enhanced save detection - handle multi-line function calls
  # Pattern for save_figure() that might span multiple lines
  save_fig_pattern <- "save_figure\\s*\\([^)]*\\)"
  save_fig_matches <- gregexpr(save_fig_pattern, gsub("\n", " ", full_code), perl = TRUE)

  if(save_fig_matches[[1]][1] != -1) {
    save_fig_calls <- regmatches(gsub("\n", " ", full_code), save_fig_matches)[[1]]
    for(i in seq_along(save_fig_calls)) {
      # Try to extract parameters
      params_match <- regmatches(save_fig_calls[i],
                                 regexec("save_figure\\s*\\(\\s*([^,]+)\\s*,\\s*[\"']([^\"']+)[\"']",
                                         save_fig_calls[i]))[[1]]
      if(length(params_match) >= 3) {
        io$saves[[length(io$saves) + 1]] <- list(
          type = "figure",
          object = trimws(params_match[2]),
          name = params_match[3],
          save_function = "save_figure"
        )
      }
    }
    io$saves_files <- TRUE
    io$creates_plots <- TRUE
  }

  # Pattern for save_with_timestamp()
  save_ts_pattern <- "save_with_timestamp\\s*\\([^)]*\\)"
  save_ts_matches <- gregexpr(save_ts_pattern, gsub("\n", " ", full_code), perl = TRUE)

  if(save_ts_matches[[1]][1] != -1) {
    save_ts_calls <- regmatches(gsub("\n", " ", full_code), save_ts_matches)[[1]]
    for(i in seq_along(save_ts_calls)) {
      params_match <- regmatches(save_ts_calls[i],
                                 regexec("save_with_timestamp\\s*\\(\\s*([^,]+)\\s*,\\s*[\"']([^\"']+)[\"']",
                                         save_ts_calls[i]))[[1]]
      if(length(params_match) >= 3) {
        io$saves[[length(io$saves) + 1]] <- list(
          type = "timestamped_file",
          object = trimws(params_match[2]),
          base_name = params_match[3],
          save_function = "save_with_timestamp"
        )
      }
    }
    io$saves_files <- TRUE
  }

  # Detect other save operations
  if(grepl("write\\.csv\\s*\\(", full_code)) {
    io$saves_files <- TRUE
  }

  if(grepl("ggsave\\s*\\(", full_code)) {
    io$saves_files <- TRUE
    io$creates_plots <- TRUE
  }

  # Detect result object
  if(any(grepl("^\\s*result\\s*<-", code_lines))) {
    io$outputs$result <- list(type = "return_value")
  }

  return(io)
}


#' Register workflow for reuse
#'
#' @param workflow Workflow object from extraction
register_workflow_internal <- function(workflow) {
  paths <- get_project_paths()

  # Create workflows directory
  workflows_dir <- file.path(paths$r_programs, "workflows")
  if(!dir.exists(workflows_dir)) {
    dir.create(workflows_dir)
  }

  # Save workflow
  workflow_file <- file.path(workflows_dir, paste0(workflow$name, ".rds"))
  saveRDS(workflow, workflow_file)

  # Also save as readable R file
  r_file <- file.path(workflows_dir, paste0(workflow$name, "_workflow.R"))
  writeLines(c(
    paste("# Workflow:", workflow$name),
    paste("# Description:", workflow$description),
    paste("# Output mode:", workflow$output_mode),
    paste("# Required packages:", paste(workflow$required_packages, collapse = ", ")),
    paste("# Created:", workflow$created),
    paste("# Source:", basename(workflow$source_file)),
    "",
    workflow$code
  ), r_file)
}


### ------------------------------------------------------------ ###
### WORKFLOW APPLICATION

#' Apply workflow to file set with intelligent output handling
#'
#' @param workflow_name Name of registered workflow
#' @param file_set Either a set name or vector of file paths
#' @param output_mode Override output mode: "auto", "single", "file_set"
#' @param naming_pattern How to name outputs: "prefix", "suffix", "folder"
#' @param save_outputs Whether to save outputs (vs just return results)
#' @param progress Show progress bar
#' @param verbose Show detailed progress
#' @param continue_on_error Continue processing if some files fail
#' @return Results from workflow application
#' @export
apply_workflow <- function(workflow_name,
                           file_set,
                           output_mode = "auto",
                           naming_pattern = "prefix",
                           save_outputs = TRUE,
                           progress = TRUE,
                           verbose = TRUE,
                           continue_on_error = FALSE,
                           parallel = FALSE,
                           n_cores = NULL) {

  paths <- get_project_paths()

  # Load workflow
  workflow_file <- file.path(paths$r_programs, "workflows", paste0(workflow_name, ".rds"))
  if(!file.exists(workflow_file)) {
    stop("Workflow '", workflow_name, "' not found. Run list_workflows() to see available.")
  }

  workflow <- readRDS(workflow_file)

  # Determine output mode
  if(output_mode == "auto") {
    output_mode <- workflow$output_mode
  }

  # Get files
  if(length(file_set) == 1 && !file.exists(file_set)) {
    # It's a set name
    files <- get_files_in_set(file_set)
    set_name <- file_set
  } else {
    # Direct file list
    files <- file_set
    set_name <- "custom"
  }

  if(length(files) == 0) {
    stop("No files found")
  }

  # Validate files exist
  files <- validate_file_set(files)

  # Group files by type for better error reporting
  file_types <- tools::file_ext(files)
  if(length(unique(file_types)) > 1 && verbose) {
    message("Note: Processing multiple file types: ",
            paste(unique(file_types), collapse = ", "))
  }

  # Header
  if(verbose) {
    cat("\n🔄 APPLYING WORKFLOW:", workflow_name, "\n")
    if(nchar(workflow$description) > 0) {
      cat("📋", workflow$description, "\n")
    }
    cat("Files:", length(files), "\n")
    cat("Output mode:", output_mode, "\n")
    cat("Naming pattern:", naming_pattern, "\n")
    if(continue_on_error) cat("Continue on error: TRUE\n")
    cat(paste(rep("-", 60), collapse = ""), "\n\n")
  }

  # Process files
  results <- list()
  errors <- list()
  skipped <- character()

  # Choose processing method
  if(parallel && length(files) > 2) {  # Only parallelize if worthwhile

    # Process in parallel
    all_results <- process_files_parallel(workflow, files, save_outputs,
                                          workflow_name, n_cores, verbose)

    # Separate successes and errors
    for(i in seq_along(all_results)) {
      if(!is.null(all_results[[i]]$error)) {
        errors[[names(all_results)[i]]] <- all_results[[i]]$error
      } else {
        results[[names(all_results)[i]]] <- all_results[[i]]
      }
    }

  } else {
    # Original sequential processing
    if(progress && length(files) > 1) {
      pb <- txtProgressBar(min = 0, max = length(files), style = 3)
    }

    for(i in seq_along(files)) {
    file <- files[i]

    if(progress && length(files) > 1) {
      setTxtProgressBar(pb, i)
    }

    # Skip if file doesn't exist
    if(!file.exists(file)) {
      skipped <- c(skipped, file)
      next
    }

    # Process this file
    if(continue_on_error) {
      result <- tryCatch({
        process_workflow_file(workflow, file, output_mode, naming_pattern,
                              save_outputs, workflow_name)
      }, error = function(e) {
        list(error = e$message)
      })
    } else {
      result <- tryCatch({
        process_workflow_file(workflow, file, output_mode, naming_pattern,
                              save_outputs, workflow_name)
      }, error = function(e) {
        if(progress && length(files) > 1) close(pb)
        stop("Error processing '", basename(file), "': ", e$message)
      })
    }

    if(!is.null(result$error)) {
      errors[[file]] <- result$error
    } else {
      results[[file]] <- result
    }
  }

  if(progress && length(files) > 1) {
    close(pb)
  }
}
  # Summary
  if(verbose) {
    cat("\n✅ Complete:", length(results), "succeeded,",
        length(errors), "failed")
    if(length(skipped) > 0) {
      cat(",", length(skipped), "skipped")
    }
    cat("\n")

    if(length(errors) > 0) {
      cat("\n❌ Errors:\n")
      for(name in names(errors)) {
        cat("  -", basename(name), ":", errors[[name]], "\n")
      }
    }

    if(length(skipped) > 0) {
      cat("\n⚠️  Skipped (file not found):\n")
      for(file in skipped) {
        cat("  -", basename(file), "\n")
      }
    }
  }

  if(verbose && (length(results) > 0 || length(errors) > 0)) {
    create_workflow_execution_summary(workflow_name, results, errors,
                                      save_summary = save_outputs)
  }

  # Handle output mode
  if(output_mode == "single" && length(results) > 0) {
    # Combine results
    combined <- combine_workflow_results(results)

    if(save_outputs) {
      # Save combined output
      output_name <- paste0(workflow_name, "_", set_name, "_combined")
      save_combined_outputs(combined, output_name, workflow_name)
    }

    return(combined)
  }

  return(results)
}


#' Process single file through workflow with enhanced error handling
#'
#' @param workflow Workflow object
#' @param file File to process
#' @param output_mode Output mode
#' @param naming_pattern Naming pattern
#' @param save_outputs Whether to save
#' @param workflow_name Workflow name
process_workflow_file <- function(workflow, file, output_mode, naming_pattern,
                                  save_outputs, workflow_name) {

  paths <- get_project_paths()

  # Determine file path with enhanced file finding
  if(file.exists(file)) {
    file_path <- file
  } else {
    # Try common locations
    file_path <- file.path(paths$data_cleaned, file)
    if(!file.exists(file_path)) {
      file_path <- file.path(paths$data_source, file)
    }

    # Try with common extensions if not found
    if(!file.exists(file_path)) {
      for(ext in c(".xlsx", ".xls", ".csv", ".rds")) {
        test_path <- paste0(file_path, ext)
        if(file.exists(test_path)) {
          file_path <- test_path
          break
        }
      }
    }
  }

  if(!file.exists(file_path)) {
    stop("File not found: ", file)
  }

  # Create execution environment
  env <- new.env()
  env$FILE_PATH <- file_path
  env$FILE_NAME <- tools::file_path_sans_ext(basename(file))
  env$paths <- paths

  # Set up naming helpers if saving outputs
  if(save_outputs && output_mode == "file_set") {
    setup_naming_helpers(env, naming_pattern, workflow_name)
  }

  # Load required packages
  load_workflow_packages(workflow, env)

  # Create safe execution wrapper
  safe_execute <- function(code, env, context = "") {
    tryCatch({
      eval(parse(text = code), envir = env)
    }, error = function(e) {
      # Enhanced error with context
      error_msg <- paste0(
        "Workflow '", workflow_name, "' failed",
        if(nchar(context) > 0) paste0(" ", context) else "",
        ":\n", e$message
      )

      # Add debugging info if available
      if(exists("FILE_NAME", envir = env)) {
        error_msg <- paste0(error_msg, "\nFile: ", env$FILE_NAME)
      }

      stop(error_msg, call. = FALSE)
    })
  }

  # Execute workflow code as a single block
  # Filter out empty lines and OUTPUT_MODE lines
  code_to_run <- workflow$code[nchar(trimws(workflow$code)) > 0 &
                                 !grepl("^#OUTPUT_MODE:", workflow$code)]

  # Join all lines into a single string
  full_code <- paste(code_to_run, collapse = "\n")

  # Execute with better error context
  safe_execute(full_code, env, paste0("processing '", basename(file), "'"))

  # Get result - PRIORITIZE 'result' variable
  if(exists("result", envir = env)) {
    output <- get("result", envir = env)

    # Save if requested
    if(save_outputs) {
      output_name <- paste0(env$FILE_NAME, "_", workflow_name, "_result")

      if(is.data.frame(output)) {
        output_file <- paste0(output_name, ".csv")
        write.csv(output, file.path(dirname(file_path), output_file), row.names = FALSE)
      }
    }
    return(output)
  } else {
    # Fallback to current behavior
    obj_names <- setdiff(ls(env), c("FILE_PATH", "FILE_NAME", "paths", "library",
                                    "OUTPUT_PREFIX", "get_output_name"))
    if(length(obj_names) == 1) {
      return(get(obj_names[1], envir = env))
    } else if(length(obj_names) > 0) {
      warning("No 'result' found. Returning all objects. Consider adding: result <- your_output")
      return(mget(obj_names, envir = env))
    } else {
      return(list())
    }
  }
}

#' Load required packages for workflow
#'
#' @param workflow Workflow object
#' @param env Environment to load packages into
load_workflow_packages <- function(workflow, env) {
  # Always load base functionality
  env$library <- base::library

  # Load tidyverse if likely needed
  if(any(grepl("%>%|mutate|filter|select|ggplot", paste(workflow$code, collapse = " ")))) {
    suppressPackageStartupMessages({
      eval(expression(library(tidyverse)), envir = env)
    })
  }

  # Load other required packages
  if(!is.null(workflow$required_packages)) {
    for(pkg in workflow$required_packages) {
      if(requireNamespace(pkg, quietly = TRUE)) {
        suppressPackageStartupMessages({
          eval(parse(text = sprintf("library(%s)", pkg)), envir = env)
        })
      } else {
        warning("Package '", pkg, "' is required but not installed")
      }
    }
  }
}


#' Validate file set
#'
#' @param files Vector of file paths
#' @return Validated file paths
validate_file_set <- function(files) {
  # Remove duplicates
  files <- unique(files)

  # Check which files exist
  exists_mask <- file.exists(files)

  if(sum(!exists_mask) > 0) {
    warning("Some files not found: ", sum(!exists_mask), " files")
  }

  return(files)
}


#' Set up naming helpers in environment
#'
#' @param env Environment to modify
#' @param pattern Naming pattern
#' @param workflow_name Workflow name
setup_naming_helpers <- function(env, pattern, workflow_name) {

  # Provide naming helper function
  env$get_output_name <- function(base_name) {
    if(pattern == "prefix") {
      return(paste0(env$FILE_NAME, "_", base_name))
    } else if(pattern == "suffix") {
      return(paste0(base_name, "_", env$FILE_NAME))
    } else if(pattern == "workflow_prefix") {
      return(paste0(workflow_name, "_", env$FILE_NAME, "_", base_name))
    } else {
      return(base_name)
    }
  }

  # Provide common naming prefix
  env$OUTPUT_PREFIX <- paste0(env$FILE_NAME, "_", format(Sys.Date(), "%Y%m%d"))
}


#' Combine workflow results based on type
#'
#' @param results List of results from each file
#' @return Combined results
combine_workflow_results <- function(results) {

  if(length(results) == 0) return(list())

  # Get first result to determine type
  first_result <- results[[1]]

  if(is.data.frame(first_result)) {
    # All data frames - bind rows
    return(bind_rows(results, .id = "source_file"))

  } else if(is.list(first_result) && length(first_result) > 0) {
    # Lists - try to combine each component
    combined <- list()
    component_names <- names(first_result)

    for(comp in component_names) {
      comp_data <- lapply(results, function(x) x[[comp]])

      # Check if all are data frames
      if(all(sapply(comp_data, is.data.frame))) {
        combined[[comp]] <- bind_rows(comp_data, .id = "source_file")
      } else if(all(sapply(comp_data, is.numeric)) &&
                all(sapply(comp_data, length) == 1)) {
        # Single numeric values - create summary
        combined[[comp]] <- data.frame(
          source_file = names(results),
          value = unlist(comp_data)
        )
      } else {
        # Keep as list
        combined[[comp]] <- comp_data
      }
    }

    return(combined)

  } else {
    # Can't combine - return as list
    return(results)
  }
}


#' Save combined outputs
#'
#' @param combined Combined results
#' @param output_name Base name for outputs
#' @param workflow_name Workflow name
save_combined_outputs <- function(combined, output_name, workflow_name) {

  paths <- get_project_paths()

  if(is.data.frame(combined)) {
    # Save as CSV
    filename <- paste0(output_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    write.csv(combined, file.path(paths$data_cleaned, filename), row.names = FALSE)
    message("Saved combined data: ", filename)

  } else if(is.list(combined)) {
    # Save each component
    for(comp_name in names(combined)) {
      if(is.data.frame(combined[[comp_name]])) {
        filename <- paste0(output_name, "_", comp_name, "_",
                           format(Sys.Date(), "%Y%m%d"), ".csv")
        write.csv(combined[[comp_name]],
                  file.path(paths$data_cleaned, filename),
                  row.names = FALSE)
      } else if(inherits(combined[[comp_name]], "ggplot")) {
        save_figure(combined[[comp_name]],
                    paste0(output_name, "_", comp_name),
                    use_gui = FALSE)
      }
    }
    message("Saved ", length(combined), " combined outputs")
  }
}


#' Apply workflow to large file sets with memory management
#'
#' @param workflow_name Name of workflow to apply
#' @param file_set File set to process
#' @param chunk_size Number of files to process at once
#' @param save_intermediate Save results after each chunk
#' @param ... Additional arguments passed to apply_workflow
#' @return Combined results from all chunks
#' @export
apply_workflow_chunked <- function(workflow_name,
                                   file_set,
                                   chunk_size = 100,
                                   save_intermediate = FALSE,
                                   ...) {

  # Get files
  if(length(file_set) == 1 && !file.exists(file_set)) {
    files <- get_files_in_set(file_set)
  } else {
    files <- file_set
  }

  n_chunks <- ceiling(length(files) / chunk_size)

  if(n_chunks == 1) {
    # No need for chunking
    return(apply_workflow(workflow_name, files, ...))
  }

  cat("📦 Processing", length(files), "files in", n_chunks, "chunks\n")
  cat("   Chunk size:", chunk_size, "\n\n")

  all_results <- list()
  chunk_errors <- list()

  for(i in seq_len(n_chunks)) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, length(files))

    chunk_files <- files[start_idx:end_idx]

    cat(sprintf("🔄 Chunk %d/%d (%d files)\n",
                i, n_chunks, length(chunk_files)))

    # Process chunk
    chunk_results <- tryCatch({
      apply_workflow(workflow_name, chunk_files, verbose = FALSE, ...)
    }, error = function(e) {
      chunk_errors[[paste0("chunk_", i)]] <- e$message
      list()
    })

    # Save intermediate results if requested
    if(save_intermediate && length(chunk_results) > 0) {
      chunk_file <- sprintf("workflow_%s_chunk_%d_%s.rds",
                            workflow_name, i, format(Sys.Date(), "%Y%m%d"))
      saveRDS(chunk_results, chunk_file)
      cat("   💾 Saved intermediate:", chunk_file, "\n")
    }

    # Combine results
    if(length(chunk_results) > 0) {
      all_results <- c(all_results, chunk_results)
    }

    # Garbage collection between chunks
    gc(verbose = FALSE)

    cat("   ✅ Complete\n\n")
  }

  # Report any chunk errors
  if(length(chunk_errors) > 0) {
    cat("⚠️  Some chunks had errors:\n")
    for(chunk in names(chunk_errors)) {
      cat("  -", chunk, ":", chunk_errors[[chunk]], "\n")
    }
  }

  cat("   All chunks processed\n")
  cat("   Total results:", length(all_results), "\n")

  return(all_results)
}


### ------------------------------------------------------------ ###
### WORKFLOW MANAGEMENT FUNCTIONS

#' List available workflows
#'
#' Shows all registered workflows with their descriptions
#'
#' @param show_details Show detailed information
#' @return Data frame of workflows (invisibly)
#' @export
list_workflows <- function(show_details = TRUE) {

  paths <- get_project_paths()
  workflows_dir <- file.path(paths$r_programs, "workflows")

  if(!dir.exists(workflows_dir)) {
    cat("No workflows registered yet.\n")
    cat("Use extract_workflows() to register workflows from your scripts.\n")
    return(invisible(NULL))
  }

  workflow_files <- list.files(workflows_dir, pattern = "\\.rds$", full.names = TRUE)

  if(length(workflow_files) == 0) {
    cat("No workflows found.\n")
    return(invisible(NULL))
  }

  # Load workflow info
  workflows_info <- data.frame(
    name = character(),
    description = character(),
    output_mode = character(),
    n_saves = integer(),
    n_packages = integer(),
    source_file = character(),
    created = character(),
    stringsAsFactors = FALSE
  )

  for(file in workflow_files) {
    wf <- readRDS(file)
    workflows_info <- rbind(workflows_info, data.frame(
      name = wf$name,
      description = wf$description,
      output_mode = wf$output_mode,
      n_saves = length(wf$io$saves),
      n_packages = length(wf$required_packages),
      source_file = basename(wf$source_file),
      created = format(wf$created, "%Y-%m-%d"),
      stringsAsFactors = FALSE
    ))
  }

  if(show_details) {
    cat("📚 REGISTERED WORKFLOWS\n")
    cat(paste(rep("=", 70), collapse = ""), "\n\n")

    for(i in 1:nrow(workflows_info)) {
      wf <- workflows_info[i,]
      cat("📌", wf$name, "\n")
      if(nchar(wf$description) > 0) {
        cat("  ", wf$description, "\n")
      }
      cat("   Output mode:", wf$output_mode, "| Saves:", wf$n_saves, "outputs")
      if(wf$n_packages > 0) {
        cat(" | Packages:", wf$n_packages)
      }
      cat("\n")
      cat("   Source:", wf$source_file, "| Created:", wf$created, "\n\n")
    }
  } else {
    print(workflows_info)
  }

  return(invisible(workflows_info))
}


#' Show workflow details including I/O analysis
#'
#' @param workflow_name Name of workflow to examine
#' @export
show_workflow <- function(workflow_name) {

  paths <- get_project_paths()
  workflow_file <- file.path(paths$r_programs, "workflows", paste0(workflow_name, ".rds"))

  if(!file.exists(workflow_file)) {
    stop("Workflow not found: ", workflow_name)
  }

  workflow <- readRDS(workflow_file)

  cat("\n📋 WORKFLOW:", workflow$name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  if(nchar(workflow$description) > 0) {
    cat("Description:", workflow$description, "\n")
  }
  cat("Output mode:", workflow$output_mode, "\n")
  cat("Source file:", basename(workflow$source_file), "\n")
  cat("Lines:", workflow$line_range[1], "-", workflow$line_range[2], "\n")
  cat("Created:", format(workflow$created, "%Y-%m-%d %H:%M"), "\n")

  # Required packages
  if(length(workflow$required_packages) > 0) {
    cat("\n📦 REQUIRED PACKAGES:\n")
    cat("  ", paste(workflow$required_packages, collapse = ", "), "\n")
  }

  # I/O Analysis
  cat("\n📥 INPUTS:\n")
  if(length(workflow$io$inputs) > 0) {
    cat("  Type:", workflow$io$inputs$type, "\n")
    cat("  Function:", workflow$io$inputs$read_function, "\n")
  } else {
    cat("  No file inputs detected\n")
  }

  cat("\n💾 SAVES:\n")
  if(length(workflow$io$saves) > 0) {
    for(save in workflow$io$saves) {
      cat("  ", save$save_function, "(", save$object, ")")
      if(!is.null(save$name)) cat(" → '", save$name, "'")
      cat("\n")
    }
  } else {
    cat("  No explicit saves detected\n")
  }

  cat("\n📤 RETURNS:\n")
  if(!is.null(workflow$io$outputs$result)) {
    cat("  result object\n")
  } else {
    cat("  No result object\n")
  }

  cat("\n📝 CODE PREVIEW (first 10 lines):\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  preview_lines <- head(workflow$code[nchar(trimws(workflow$code)) > 0], 10)
  cat(paste(preview_lines, collapse = "\n"), "\n")
  if(length(workflow$code) > 10) {
    cat("... (", length(workflow$code) - 10, " more lines)\n", sep = "")
  }
}


#' Validate workflow code with enhanced checks
#'
#' Check workflow for common issues
#'
#' @param workflow_name Workflow to validate
#' @export
validate_workflow <- function(workflow_name) {

  paths <- get_project_paths()
  workflow_file <- file.path(paths$r_programs, "workflows", paste0(workflow_name, ".rds"))

  if(!file.exists(workflow_file)) {
    stop("Workflow not found: ", workflow_name)
  }

  workflow <- readRDS(workflow_file)
  issues <- character()
  warnings <- character()

  # Check for FILE_PATH usage
  if(!any(grepl("FILE_PATH", workflow$code))) {
    issues <- c(issues, "No FILE_PATH found - workflow won't read input files")
  }

  # Check if FILE_PATH is modified
  if(any(grepl("FILE_PATH\\s*<-", workflow$code))) {
    issues <- c(issues, "FILE_PATH should not be modified within workflow")
  }

  # Check for hardcoded paths
  if(any(grepl("C:|/Users/|~/", workflow$code))) {
    issues <- c(issues, "Hardcoded paths detected - use paths$ variables instead")
  }

  # Check for saves
  if(length(workflow$io$saves) == 0 && !any(grepl("result\\s*<-", workflow$code))) {
    warnings <- c(warnings, "No saves or result detected - workflow produces no output")
  }

  # Check for required packages
  if(length(workflow$required_packages) > 0) {
    missing_pkgs <- character()
    for(pkg in workflow$required_packages) {
      if(!requireNamespace(pkg, quietly = TRUE)) {
        missing_pkgs <- c(missing_pkgs, pkg)
      }
    }
    if(length(missing_pkgs) > 0) {
      issues <- c(issues, paste("Missing required packages:",
                                paste(missing_pkgs, collapse = ", ")))
    }
  }

  # Check for infinite loops
  if(any(grepl("while\\s*\\(\\s*TRUE\\s*\\)", workflow$code))) {
    issues <- c(issues, "Potential infinite loop detected")
  }

  # Check for dangerous operations
  dangerous_patterns <- list(
    "unlink\\s*\\(" = "File deletion",
    "system\\s*\\(" = "System call",
    "shell\\s*\\(" = "Shell command",
    "file\\.remove\\s*\\(" = "File removal"
  )

  for(pattern in names(dangerous_patterns)) {
    if(any(grepl(pattern, workflow$code))) {
      warnings <- c(warnings,
                    paste("Potentially dangerous operation:",
                          dangerous_patterns[[pattern]]))
    }
  }

  # Test parse the workflow code
  tryCatch({
    parse(text = paste(workflow$code, collapse = "\n"))
  }, error = function(e) {
    issues <- c(issues, paste("Syntax error:", e$message))
  })

  # Report
  cat("🔍 WORKFLOW VALIDATION:", workflow_name, "\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")

  if(length(issues) > 0) {
    cat("\n❌ ISSUES:\n")
    for(issue in issues) cat("  •", issue, "\n")
  }

  if(length(warnings) > 0) {
    cat("\n⚠️  WARNINGS:\n")
    for(warning in warnings) cat("  •", warning, "\n")
  }

  if(length(issues) == 0 && length(warnings) == 0) {
    cat("\n✅ Workflow appears valid!\n")
  }

  return(invisible(list(issues = issues, warnings = warnings)))
}


#' Remove a workflow
#'
#' @param workflow_name Name of workflow to remove
#' @param confirm Ask for confirmation
#' @export
remove_workflow <- function(workflow_name, confirm = TRUE) {

  paths <- get_project_paths()
  workflows_dir <- file.path(paths$r_programs, "workflows")

  rds_file <- file.path(workflows_dir, paste0(workflow_name, ".rds"))
  r_file <- file.path(workflows_dir, paste0(workflow_name, "_workflow.R"))

  if(!file.exists(rds_file)) {
    stop("Workflow not found: ", workflow_name)
  }

  if(confirm && interactive()) {
    response <- readline(paste("Remove workflow", workflow_name, "? (y/n): "))
    if(tolower(substr(response, 1, 1)) != "y") {
      message("Cancelled")
      return(invisible(FALSE))
    }
  }

  file.remove(rds_file)
  if(file.exists(r_file)) file.remove(r_file)

  message("Removed workflow: ", workflow_name)
  return(invisible(TRUE))
}


### ------------------------------------------------------------ ###
### HELPER FUNCTIONS

#' Check if a workflow exists
#'
#' @param workflow_name Name of workflow to check
#' @return TRUE if workflow exists
#' @export
workflow_exists <- function(workflow_name) {
  paths <- get_project_paths()
  workflow_file <- file.path(paths$r_programs, "workflows",
                             paste0(workflow_name, ".rds"))
  return(file.exists(workflow_file))
}

#' Get files in a file set
#'
#' @param set_name Name of file set
#' @return Vector of file names
get_files_in_set <- function(set_name) {
  # First try as a file set
  files <- select_files(set_name = set_name, folder = "source", full_path = TRUE)

  if(length(files) == 0) {
    # Try cleaned folder
    files <- select_files(set_name = set_name, folder = "cleaned", full_path = TRUE)
  }

  if(length(files) == 0) {
    # Fall back to pattern matching in both folders
    files <- select_files(pattern = set_name, folder = "source", full_path = TRUE)
    if(length(files) == 0) {
      files <- select_files(pattern = set_name, folder = "cleaned", full_path = TRUE)
    }
  }

  if(length(files) == 0) {
    warning("No files found for: ", set_name)
  }

  return(files)
}


#' Create workflow template
#'
#' Generates a workflow template with proper markers
#'
#' @param name Workflow name
#' @param description Description
#' @param output_mode "file_set" or "single"
#' @export
workflow_template <- function(name = "my_workflow",
                              description = "What this workflow does",
                              output_mode = "file_set") {

  template <- c(
    paste0("#WORKFLOW_START: ", name, ", ", description),
    paste0("#OUTPUT_MODE: ", output_mode),
    "",
    "# Load data",
    "data <- read.csv(FILE_PATH)",
    "",
    "# Your analysis code here",
    "# Example: create summary",
    "summary_stats <- data %>%",
    "  summarise(",
    "    n = n(),",
    "    mean_value = mean(value, na.rm = TRUE)",
    "  )",
    "",
    "# IMPORTANT: Assign your main output to 'result'",
    "# This is what gets saved as the workflow output",
    "result <- summary_stats  # ← This is what gets saved!",
    "",
    "# Optional: You can still save other files directly",
    "# save_figure(p1, paste0('plot_', FILE_NAME), use_gui = FALSE)",
    "",
    "#WORKFLOW_END"
  )

  cat(paste(template, collapse = "\n"))
  cat("\n\nCopy and paste this template into your analysis script\n")
  cat("Then use extract_workflows() to register it\n")

  if(rstudioapi::isAvailable() && interactive()) {
    if(rstudioapi::showQuestion("Insert", "Insert template at cursor?")) {
      rstudioapi::insertText(paste(template, collapse = "\n"))
    }
  }
}


#' Quick workflow test on single file
#'
#' Test a workflow on one file before applying to entire set
#'
#' @param workflow_name Workflow to test
#' @param test_file Single file to test on
#' @param verbose Show detailed output
#' @return Result from workflow
#' @export
test_workflow <- function(workflow_name, test_file, verbose = TRUE) {

  if(verbose) {
    cat("🧪 Testing workflow:", workflow_name, "\n")
    cat("   On file:", test_file, "\n\n")
  }

  # Apply to single file
  result <- apply_workflow(workflow_name,
                           test_file,
                           output_mode = "file_set",
                           save_outputs = FALSE,
                           verbose = verbose)

  # Show result structure
  if(verbose && length(result) > 0) {
    cat("\n📊 Result structure:\n")
    str(result[[1]], max.level = 2)
  }

  return(result[[1]])
}


#' Run multiple workflows on same file set
#'
#' @param workflow_names Vector of workflow names
#' @param file_set File set to process
#' @param ... Additional arguments passed to apply_workflow
#' @return Named list of results
#' @export
apply_workflows <- function(workflow_names, file_set, ...) {

  results <- list()

  cat("🔄 Running", length(workflow_names), "workflows on", file_set, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  for(wf in workflow_names) {
    cat("▶️  Running:", wf, "\n")
    results[[wf]] <- apply_workflow(wf, file_set, verbose = FALSE, ...)
    cat("✅ Complete\n\n")
  }

  return(results)
}


#' Create workflow chain for sequential processing
#'
#' @param name Name for the chain
#' @param workflow_names Ordered vector of workflows
#' @param description Description of what chain does
#' @export
create_workflow_chain <- function(name, workflow_names, description = "") {

  paths <- get_project_paths()
  workflows_dir <- file.path(paths$r_programs, "workflows")

  chain <- list(
    name = name,
    workflows = workflow_names,
    description = description,
    type = "chain",
    created = Sys.Date()
  )

  # Save chain definition
  chain_file <- file.path(workflows_dir, paste0("chain_", name, ".rds"))
  saveRDS(chain, chain_file)

  cat("🔗 Created workflow chain:", name, "\n")
  cat("   Workflows:", paste(workflow_names, collapse = " → "), "\n")

  return(invisible(chain))
}


#' Apply workflow chain
#'
#' @param chain_name Name of workflow chain
#' @param file_set File set to process
#' @param stop_on_error Stop if any workflow fails
#' @return Results from all workflows in chain
#' @export
apply_workflow_chain <- function(chain_name, file_set, stop_on_error = TRUE) {

  paths <- get_project_paths()
  chain_file <- file.path(paths$r_programs, "workflows", paste0("chain_", chain_name, ".rds"))

  if(!file.exists(chain_file)) {
    stop("Workflow chain not found: ", chain_name)
  }

  chain <- readRDS(chain_file)

  cat("🔗 Running workflow chain:", chain_name, "\n")
  if(nchar(chain$description) > 0) {
    cat("   ", chain$description, "\n")
  }
  cat("   Steps:", paste(chain$workflows, collapse = " → "), "\n\n")

  results <- list()

  for(i in seq_along(chain$workflows)) {
    wf <- chain$workflows[i]
    cat(paste0("[", i, "/", length(chain$workflows), "] ", wf, "...\n"))

    tryCatch({
      results[[wf]] <- apply_workflow(wf, file_set, verbose = FALSE)
      cat("   ✅ Success\n\n")
    }, error = function(e) {
      cat("   ❌ Error:", e$message, "\n\n")
      if(stop_on_error) stop("Chain stopped due to error")
    })
  }

  cat("🎉 Chain complete\n")
  return(results)
}


### ------------------------------------------------------------ ###
### WORKFLOW EXAMPLES AND UTILITIES

#' Generate workflow from existing analysis code
#'
#' Helper to convert existing analysis into workflow format
#'
#' @param input_line Line that reads the data (to identify what to replace)
#' @param output_lines Lines that save outputs (to identify what to parameterize)
#' @export
convert_to_workflow <- function(input_line = NULL, output_lines = NULL) {

  cat("🔄 WORKFLOW CONVERSION HELPER\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  cat("To convert existing code to a workflow:\n\n")

  cat("1. Replace your data loading line:\n")
  cat("   FROM: data <- read.csv('specific_file.csv')\n")
  cat("   TO:   data <- read.csv(FILE_PATH)\n\n")

  cat("2. Use FILE_NAME for file-specific naming:\n")
  cat("   FROM: ggsave('my_plot.png')\n")
  cat("   TO:   save_figure(p1, paste0('plot_', FILE_NAME), use_gui = FALSE)\n\n")

  cat("3. Add workflow markers:\n")
  cat("   At start: #WORKFLOW_START: name, description\n")
  cat("   At end:   #WORKFLOW_END\n\n")

  cat("4. Optionally specify output mode:\n")
  cat("   #OUTPUT_MODE: file_set  (separate outputs per file)\n")
  cat("   #OUTPUT_MODE: single    (combine all results)\n\n")

  cat("5. Store results in 'result' variable:\n")
  cat("   result <- list(data = my_data, plot = my_plot)\n")
}


#' Export workflow for sharing
#'
#' Create standalone version of workflow
#'
#' @param workflow_name Workflow to export
#' @param output_file Where to save (defaults to workflow_name.R)
#' @export
export_workflow <- function(workflow_name, output_file = NULL) {

  paths <- get_project_paths()
  workflow_file <- file.path(paths$r_programs, "workflows", paste0(workflow_name, ".rds"))

  if(!file.exists(workflow_file)) {
    stop("Workflow not found: ", workflow_name)
  }

  workflow <- readRDS(workflow_file)

  if(is.null(output_file)) {
    output_file <- paste0(workflow_name, "_standalone.R")
  }

  # Create standalone script
  script <- c(
    paste0("# Standalone workflow: ", workflow$name),
    paste0("# Description: ", workflow$description),
    paste0("# Created from PRoMan workflow on ", Sys.Date()),
    "",
    "# Required packages:",
    if(length(workflow$required_packages) > 0) {
      paste0("# ", paste(workflow$required_packages, collapse = ", "))
    } else {
      "# None detected"
    },
    "# To use this workflow:",
    "# 1. Set FILE_PATH to your input file",
    "# 2. Set FILE_NAME to identify outputs",
    "# 3. Run the code",
    "",
    "# Example:",
    "# FILE_PATH <- 'my_data.csv'",
    "# FILE_NAME <- 'my_data'",
    "",
    paste0("# Original workflow code (", length(workflow$code), " lines):"),
    paste0("# ", paste(rep("-", 50), collapse = "")),
    "",
    workflow$code
  )

  writeLines(script, output_file)
  cat("📤 Exported workflow to:", output_file, "\n")

  return(invisible(output_file))
}


### ------------------------------------------------------------ ###
### INTEGRATION WITH BLITZSCHREIBEN

#' Convert Blitzschreiben component to reusable workflow
#'
#' @param component_file Path to component script
#' @param workflow_name Name for the workflow
#' @param description Description of what the workflow does
#' @export
componentize_to_workflow <- function(component_file, workflow_name = NULL,
                                     description = NULL) {

  if(!file.exists(component_file)) {
    stop("Component file not found: ", component_file)
  }

  if(is.null(workflow_name)) {
    # Extract from filename: "data_prep_P3.1.R" -> "data_prep"
    workflow_name <- gsub("_[A-Z0-9.]+\\.R$", "", basename(component_file))
  }

  if(is.null(description)) {
    description <- paste("Converted from", basename(component_file))
  }

  lines <- readLines(component_file)

  # Find the main processing code (skip header comments)
  start_line <- which(!grepl("^#", trimws(lines)))[1]
  if(is.na(start_line)) start_line <- 1

  # Look for file loading patterns to convert
  converted_lines <- lines

  # Convert specific file references to FILE_PATH
  converted_lines <- gsub(
    'read\\.csv\\(file\\.path\\(paths\\$[^,]+,\\s*"[^"]+\\.csv"\\)\\)',
    'read.csv(FILE_PATH)',
    converted_lines
  )

  # Convert OUTPUT_PREFIX references if not already dynamic
  converted_lines <- gsub(
    'OUTPUT_PREFIX <- "[^"]+"',
    'OUTPUT_PREFIX <- paste0(FILE_NAME, "_", format(Sys.Date(), "%Y%m%d"))',
    converted_lines
  )

  # Wrap in workflow markers
  workflow_lines <- c(
    paste0("#WORKFLOW_START: ", workflow_name, ", ", description),
    "#OUTPUT_MODE: file_set",
    "",
    "# Converted component - uses FILE_PATH and FILE_NAME",
    converted_lines[start_line:length(converted_lines)],
    "",
    "#WORKFLOW_END"
  )

  # Save as new workflow script
  workflow_script <- file.path(dirname(component_file),
                               paste0("workflow_", workflow_name, ".R"))
  writeLines(workflow_lines, workflow_script)

  # Extract and register
  extract_workflows(workflow_script, auto_register = TRUE)

  message("✅ Converted component to workflow: ", workflow_name)
  message("   Saved as: ", basename(workflow_script))
  message("   Review and test before using in production")

  return(invisible(workflow_script))
}


#' Create workflow chain from master script analysis
#'
#' @param project_code Project code to analyze
#' @param chain_name Name for the workflow chain
#' @export
create_chain_from_master <- function(project_code, chain_name = NULL) {

  paths <- get_project_paths()

  if(is.null(chain_name)) {
    chain_name <- paste0("analysis_", project_code)
  }

  # Parse master script to find components
  master_file <- file.path(paths$r_programs, paste0("MASTER_", project_code, ".R"))

  if(!file.exists(master_file)) {
    stop("Master script not found: ", basename(master_file))
  }

  master_lines <- readLines(master_file)

  # Extract component names from source() calls
  source_pattern <- 'source\\(file\\.path\\(paths\\$r_programs,\\s*"([^"]+)"\\)\\)'
  source_matches <- regmatches(master_lines,
                               regexec(source_pattern, master_lines))

  components <- character()
  for(match in source_matches) {
    if(length(match) > 1) {
      # Extract component name from filename
      filename <- match[2]
      component <- gsub(paste0("_", project_code, "\\.R$"), "", filename)
      components <- c(components, component)
    }
  }

  if(length(components) == 0) {
    stop("No components found in master script")
  }

  # Check which exist as workflows
  available_workflows <- list_workflows(show_details = FALSE)$name
  workflow_components <- components[components %in% available_workflows]

  if(length(workflow_components) == 0) {
    cat("No components exist as workflows yet.\n")
    cat("Found components:", paste(components, collapse = ", "), "\n")
    cat("Use componentize_to_workflow() to convert them first.\n")
    return(invisible(NULL))
  }

  # Create the chain
  create_workflow_chain(chain_name, workflow_components,
                        paste("Analysis pipeline from", project_code))

  cat("✅ Created workflow chain with", length(workflow_components), "components\n")

  missing <- setdiff(components, workflow_components)
  if(length(missing) > 0) {
    cat("⚠️  Not yet workflows:", paste(missing, collapse = ", "), "\n")
  }

  return(invisible(chain_name))
}


### ------------------------------------------------------------ ###
### WORKFLOW SUMMARY FUNCTION

#' Summary of workflow system usage
#'
#' Shows statistics about registered workflows
#'
#' @export
workflow_summary <- function() {

  paths <- get_project_paths()
  workflows_dir <- file.path(paths$r_programs, "workflows")

  if(!dir.exists(workflows_dir)) {
    cat("No workflows registered yet.\n")
    return(invisible(NULL))
  }

  # Count workflows
  workflow_files <- list.files(workflows_dir, pattern = "^[^chain].*\\.rds$")
  chain_files <- list.files(workflows_dir, pattern = "^chain.*\\.rds$")

  cat("📊 WORKFLOW SUMMARY\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Registered workflows:", length(workflow_files), "\n")
  cat("Workflow chains:", length(chain_files), "\n")

  if(length(workflow_files) > 0) {
    # Analyze workflows
    total_saves <- 0
    output_modes <- character()
    all_packages <- character()

    for(file in file.path(workflows_dir, workflow_files)) {
      wf <- readRDS(file)
      total_saves <- total_saves + length(wf$io$saves)
      output_modes <- c(output_modes, wf$output_mode)
      if(!is.null(wf$required_packages)) {
        all_packages <- c(all_packages, wf$required_packages)
      }
    }

    cat("\nOutput modes:\n")
    cat("  File set:", sum(output_modes == "file_set"), "\n")
    cat("  Single:", sum(output_modes == "single"), "\n")
    cat("\nTotal save operations:", total_saves, "\n")
  }
  if(length(all_packages) > 0) {
    unique_packages <- unique(all_packages)
    cat("\nRequired packages across all workflows:\n")
    cat("  ", paste(unique_packages, collapse = ", "), "\n")
  }
  cat("\nUse list_workflows() to see all workflows\n")
}


#' Create summary of workflow execution
#'
#' @param workflow_name Name of workflow
#' @param results Results list from apply_workflow
#' @param errors Errors list from apply_workflow
#' @param save_summary Whether to save summary file
#' @return Summary file path if saved
create_workflow_execution_summary <- function(workflow_name, results, errors,
                                              save_summary = TRUE) {

  paths <- get_project_paths()

  # Build summary
  summary_lines <- c(
    paste("WORKFLOW EXECUTION SUMMARY:", workflow_name),
    paste(rep("=", 60), collapse = ""),
    paste("Executed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("Files processed:", length(results) + length(errors)),
    paste("Successful:", length(results)),
    paste("Failed:", length(errors)),
    "",
    "OUTPUTS CREATED:",
    paste(rep("-", 40), collapse = "")
  )

  # List outputs with clear pattern
  output_pattern <- paste0("*_", workflow_name, "_result.*")
  summary_lines <- c(summary_lines,
                     paste("Pattern:", output_pattern),
                     "",
                     "Files:"
  )

  for(name in names(results)) {
    summary_lines <- c(summary_lines,
                       paste("  •", basename(name), "→",
                             paste0(tools::file_path_sans_ext(basename(name)), "_",
                                    workflow_name, "_result.*"))
    )
  }

  if(length(errors) > 0) {
    summary_lines <- c(summary_lines,
                       "",
                       "ERRORS:",
                       paste(rep("-", 40), collapse = "")
    )
    for(name in names(errors)) {
      summary_lines <- c(summary_lines,
                         paste("  •", basename(name), ":", errors[[name]])
      )
    }
  }

  # Usage instructions
  summary_lines <- c(summary_lines,
                     "",
                     "TO LOAD ALL OUTPUTS:",
                     paste(rep("-", 40), collapse = ""),
                     paste0('output_files <- list.files(pattern = "', output_pattern, '")'),
                     'data_list <- lapply(output_files[grepl("\\\\.csv$", output_files)], read.csv)',
                     'rds_list <- lapply(output_files[grepl("\\\\.rds$", output_files)], readRDS)'
  )

  if(save_summary) {
    summary_file <- paste0("workflow_summary_", workflow_name, "_",
                           format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    summary_path <- file.path(paths$data_cleaned, summary_file)
    writeLines(summary_lines, summary_path)

    cat("\n📄 Summary saved to:", summary_file, "\n")
    return(summary_path)
  } else {
    cat(paste(summary_lines, collapse = "\n"))
    return(NULL)
  }
}



#' Process files in parallel
#'
#' @param workflow Workflow object
#' @param files Vector of file paths
#' @param save_outputs Whether to save outputs
#' @param workflow_name Name of workflow
#' @param n_cores Number of cores (NULL for auto-detect)
#' @param verbose Show progress
#' @return List of results
process_files_parallel <- function(workflow, files, save_outputs, workflow_name,
                                   n_cores = NULL, verbose = TRUE) {

  # Determine number of cores
  if(is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  if(verbose) {
    cat("🚀 Parallel processing with", n_cores, "cores\n")
  }

  # Create cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl))

  # Export required functions and packages to cluster
  parallel::clusterEvalQ(cl, {
    # Load PRoMan if available
    if("PRoMan" %in% .packages(all.available = TRUE)) {
      library(PRoMan)
    }
  })

  # Export the workflow processing function
  parallel::clusterExport(cl, c("process_workflow_file", "get_project_paths",
                                "load_workflow_packages", "setup_naming_helpers",
                                "safe_execute", "validate_file_set"),
                          envir = environment())

  # Export workflow and parameters
  parallel::clusterExport(cl, c("workflow", "save_outputs", "workflow_name"),
                          envir = environment())

  # Process files in parallel
  results <- parallel::parLapply(cl, files, function(file) {
    tryCatch({
      process_workflow_file(workflow, file, "file_set", "prefix",
                            save_outputs, workflow_name)
    }, error = function(e) {
      list(error = e$message, file = basename(file))
    })
  })

  # Name results
  names(results) <- basename(files)

  return(results)
}

