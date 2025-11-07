
## Updated workflows.R

# R/workflows.R
#
# Workflow management system for PRoMan
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

    # Detect I/O
    io_info <- detect_workflow_io(code_lines)

    # Create workflow object
    workflow <- list(
      name = workflow_name,
      description = description,
      code = code_lines,
      output_mode = output_mode,
      io = io_info,
      source_file = script_path,
      line_range = c(starts[i], ends[i]),
      created = Sys.time()
    )

    workflows[[workflow_name]] <- workflow

    if(verbose) {
      cat("📌 Workflow:", workflow_name, "\n")
      if(nchar(description) > 0) cat("   Description:", description, "\n")
      cat("   Lines:", starts[i], "-", ends[i],
          "(", length(code_lines), "lines)\n")
      cat("   Output mode:", output_mode, "\n")
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


#' Detect inputs and outputs in workflow code
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

  # Detect inputs
  if(any(grepl("FILE_PATH", code_lines))) {
    read_lines <- grep("read.*FILE_PATH", code_lines, value = TRUE)
    if(length(read_lines) > 0) {
      if(any(grepl("read\\.csv", read_lines))) {
        io$inputs$type <- "csv"
        io$inputs$read_function <- "read.csv"  # Changed from 'function'
      } else if(any(grepl("readRDS", read_lines))) {
        io$inputs$type <- "rds"
        io$inputs$read_function <- "readRDS"   # Changed from 'function'
      }
    }
  }

  # Detect saves using PRoMan functions
  for(i in seq_along(code_lines)) {
    line <- code_lines[i]

    # save_figure() calls
    if(grepl("save_figure\\s*\\(", line)) {
      # Try to extract details
      match <- regmatches(line,
                          regexec("save_figure\\s*\\(\\s*([^,]+)\\s*,\\s*[\"']([^\"']+)[\"']",
                                  line))[[1]]
      if(length(match) >= 3) {
        io$saves[[length(io$saves) + 1]] <- list(
          type = "figure",
          line = i,
          object = trimws(match[2]),
          name = match[3],
          save_function = "save_figure"  # Changed from 'function'
        )
      }
      io$saves_files <- TRUE
      io$creates_plots <- TRUE
    }

    # save_with_timestamp() calls
    if(grepl("save_with_timestamp\\s*\\(", line)) {
      match <- regmatches(line,
                          regexec("save_with_timestamp\\s*\\(\\s*([^,]+)\\s*,\\s*[\"']([^\"']+)[\"']",
                                  line))[[1]]
      if(length(match) >= 3) {
        io$saves[[length(io$saves) + 1]] <- list(
          type = "timestamped_file",
          line = i,
          object = trimws(match[2]),
          base_name = match[3],
          save_function = "save_with_timestamp"  # Changed from 'function'
        )
      }
      io$saves_files <- TRUE
    }

    # write.csv() calls
    if(grepl("write\\.csv\\s*\\(", line)) {
      io$saves_files <- TRUE
    }
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
#' @return Results from workflow application
#' @export
apply_workflow <- function(workflow_name,
                           file_set,
                           output_mode = "auto",
                           naming_pattern = "prefix",
                           save_outputs = TRUE,
                           progress = TRUE,
                           verbose = TRUE) {

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

  # Header
  if(verbose) {
    cat("\n🔄 APPLYING WORKFLOW:", workflow_name, "\n")
    if(nchar(workflow$description) > 0) {
      cat("📋", workflow$description, "\n")
    }
    cat("Files:", length(files), "\n")
    cat("Output mode:", output_mode, "\n")
    cat("Naming pattern:", naming_pattern, "\n")
    cat(paste(rep("-", 60), collapse = ""), "\n\n")
  }

  # Progress bar
  if(progress && length(files) > 1) {
    pb <- txtProgressBar(min = 0, max = length(files), style = 3)
  }

  # Process files
  results <- list()
  errors <- list()

  for(i in seq_along(files)) {
    file <- files[i]

    if(progress && length(files) > 1) {
      setTxtProgressBar(pb, i)
    }

    # Process this file
    result <- tryCatch({
      process_workflow_file(workflow, file, output_mode, naming_pattern,
                            save_outputs, workflow_name)
    }, error = function(e) {
      list(error = e$message)
    })

    if(!is.null(result$error)) {
      errors[[file]] <- result$error
    } else {
      results[[file]] <- result
    }
  }

  if(progress && length(files) > 1) {
    close(pb)
  }

  # Summary
  if(verbose) {
    cat("\n✅ Complete:", length(results), "succeeded,",
        length(errors), "failed\n")

    if(length(errors) > 0) {
      cat("\n❌ Errors:\n")
      for(name in names(errors)) {
        cat("  -", basename(name), ":", errors[[name]], "\n")
      }
    }
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


#' Process single file through workflow
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

  # Determine file path
  if(file.exists(file)) {
    file_path <- file
  } else {
    # Try common locations
    file_path <- file.path(paths$data_cleaned, file)
    if(!file.exists(file_path)) {
      file_path <- file.path(paths$data_source, file)
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

  # Load common packages
  suppressPackageStartupMessages({
    env$library <- base::library
    eval(expression(library(tidyverse)), envir = env)
  })

  # Execute workflow code
  for(line in workflow$code) {
    if(nchar(trimws(line)) > 0 && !grepl("^#OUTPUT_MODE:", line)) {
      tryCatch({
        eval(parse(text = line), envir = env)
      }, error = function(e) {
        stop("Error in workflow line: ", line, "\n",
             "Original error: ", e$message)
      })
    }
  }

  # Get result
  if(exists("result", envir = env)) {
    return(env$result)
  } else {
    # Return all created objects
    obj_names <- setdiff(ls(env), c("FILE_PATH", "FILE_NAME", "paths", "library",
                                    "OUTPUT_PREFIX", "get_output_name"))
    if(length(obj_names) == 1) {
      return(get(obj_names[1], envir = env))
    } else if(length(obj_names) > 0) {
      return(mget(obj_names, envir = env))
    } else {
      return(list())
    }
  }
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
      cat("   Output mode:", wf$output_mode, "| Saves:", wf$n_saves, "outputs\n")
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
      cat("  Line", save$line, ":", save$save_function, "(", save$object, ")")
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

#' Get files in a file set
#'
#' @param set_name Name of file set
#' @return Vector of file names
get_files_in_set <- function(set_name) {

  paths <- get_project_paths()

  # First check if it's an actual file set
  all_sets <- analyze_file_sets(return_data = TRUE)

  for(folder_sets in all_sets) {
    if(set_name %in% names(folder_sets)) {
      return(folder_sets[[set_name]])
    }
  }

  # If not found as a set, try as keyword
  files <- select_files_by_keyword(set_name)

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
    "# Example: create plot",
    "p1 <- ggplot(data, aes(x = date, y = value)) +",
    "  geom_line() +",
    "  labs(title = paste('Analysis for', FILE_NAME))",
    "",
    "# Save outputs using PRoMan functions with dynamic naming",
    "# The FILE_NAME variable is automatically available",
    "save_figure(p1, ",
    "           figure_name = paste0('timeline_', FILE_NAME),",
    "           caption = paste('Timeline analysis for', FILE_NAME),",
    "           use_gui = FALSE)",
    "",
    "save_with_timestamp(summary_stats, ",
    "                   base_filename = paste0('summary_', FILE_NAME))",
    "",
    "# Optional: return results for further processing",
    "result <- list(",
    "  stats = summary_stats,",
    "  plot = p1",
    ")",
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


#' Validate workflow code
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

  # Check for hardcoded paths
  if(any(grepl("C:|/Users/|~/", workflow$code))) {
    issues <- c(issues, "Hardcoded paths detected - use paths$ variables instead")
  }

  # Check for saves
  if(length(workflow$io$saves) == 0 && !any(grepl("result\\s*<-", workflow$code))) {
    warnings <- c(warnings, "No saves or result detected - workflow produces no output")
  }

  # Check for required packages
  pkg_lines <- grep("library\\(|require\\(", workflow$code, value = TRUE)
  if(length(pkg_lines) > 0) {
    warnings <- c(warnings, "Package loading detected - ensure packages are installed")
  }

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

    for(file in file.path(workflows_dir, workflow_files)) {
      wf <- readRDS(file)
      total_saves <- total_saves + length(wf$io$saves)
      output_modes <- c(output_modes, wf$output_mode)
    }

    cat("\nOutput modes:\n")
    cat("  File set:", sum(output_modes == "file_set"), "\n")
    cat("  Single:", sum(output_modes == "single"), "\n")
    cat("\nTotal save operations:", total_saves, "\n")
  }

  cat("\nUse list_workflows() to see all workflows\n")
}





