# R/blitzschreiben.R - REFACTORED VERSION

### ----------------------------------------------------- ###
###                       STAGE I                         ###

#' Initialize a project for Blitzschreiben workflow
#'
#' Adds Blitzschreiben tracking to an existing PRoMan project
#'
#' @param project_code Project code (e.g., "P3.1")
#' @param idea_title Title of the research idea
#' @export
init_blitz_project <- function(project_code, idea_title) {

  paths <- get_project_paths()

  # Create the central tracker (in parent directory of all projects)
  create_blitz_tracker(project_code, paths$root)

  # Create initial Stage I template in personal_notes
  card_path <- create_blitz_template(1, project_code, title = idea_title)

  message(paste("Project", project_code, "initialized for Blitzschreiben"))
  message(paste(" Stage I card created in personal_notes"))
  message(" Use start_triage_timer() when ready to begin triage")

  return(invisible(card_path))
}

#' Create or update Blitzschreiben project tracker
#'
#' Maintains central registry of all projects across stages
#'
#' @param project_code Project code
#' @param project_path Path to project
#' @param stage Current stage (1, 2, or 3)
#' @param status Project status ("active", "completed", "paused", "discarded")
create_blitz_tracker <- function(project_code, project_path, stage = 1, status = "active") {

  paths <- get_project_paths()

  # Put tracker in personal_notes instead
  tracker_file <- file.path(paths$personal_notes, "blitzschreiben_tracker.csv")

  # Read existing tracker or create new
  if(file.exists(tracker_file)) {
    tracker <- read.csv(tracker_file, stringsAsFactors = FALSE)
  } else {
    tracker <- data.frame(
      project_code = character(0),
      title = character(0),
      stage = numeric(0),
      status = character(0),
      created_date = character(0),
      last_activity = character(0),
      next_step = character(0),
      priority = character(0),
      estimated_completion = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Add or update project entry
  existing_idx <- which(tracker$project_code == project_code)

  new_entry <- data.frame(
    project_code = project_code,
    title = "[Title from Stage I card]",
    stage = stage,
    status = status,
    created_date = as.character(Sys.Date()),
    last_activity = as.character(Sys.Date()),
    next_step = "Complete Stage I triage",
    priority = "medium",
    estimated_completion = "",
    stringsAsFactors = FALSE
  )

  if(length(existing_idx) > 0) {
    # Update existing entry
    tracker[existing_idx, ] <- new_entry
  } else {
    # Add new entry
    tracker <- rbind(tracker, new_entry)
  }

  # Save tracker
  write.csv(tracker, tracker_file, row.names = FALSE)

  return(tracker_file)
}

#' Create a Blitzschreiben template
#'
#' Creates templates for any Blitzschreiben stage in personal_notes
#'
#' @param stage Stage number (1, 2, or 3) or name ("triage", "analyse", "draft")
#' @param project_code Project code (e.g., "P3.1")
#' @param title Project title (for stage 1)
#' @param analysis_type For stage 2: "regression", "descriptive", "survival", etc.
#' @param paper_type For stage 3: "journal", "conference", "brief"
#' @param target_journal For stage 3: target journal name (optional)
#' @return Path to created template
#' @export
create_blitz_template <- function(stage, project_code, title = NULL,
                                  analysis_type = "general", paper_type = "journal",
                                  target_journal = NULL) {

  paths <- get_project_paths()

  # Normalize stage input
  if(stage %in% c("1", "stage1", "triage")) {
    stage_num <- 1
  } else if(stage %in% c("2", "stage2", "analyse", "analyseblitz")) {
    stage_num <- 2
  } else if(stage %in% c("3", "stage3", "draft", "draftschreiben")) {
    stage_num <- 3
  } else {
    stop("Stage must be 1, 2, 3 or 'triage', 'analyse', 'draft'")
  }

  # Create appropriate template content
  if(stage_num == 1) {
    if(is.null(title)) title <- paste("Project", project_code)
    template_content <- create_stage1_content(title, project_code)
    filename <- paste0("stage1_triage_", project_code, ".md")

  } else if(stage_num == 2) {
    # Try to read Stage I info if it exists
    stage1_info <- .parse_stage_i_variables(project_code)
    template_content <- create_stage2_content(project_code, analysis_type, stage1_info)
    filename <- paste0("analyseblitz_", project_code, ".qmd")

  } else if(stage_num == 3) {
    # Try to read previous stage info
    stage1_info <- .parse_stage_i_variables(project_code)
    stage2_info <- read_stage2_info(project_code)  # Add this line
    template_content <- create_stage3_content(project_code, paper_type, target_journal,
                                              stage1_info, stage2_info)  # Pass both
    filename <- paste0("draft_", project_code, ".qmd")
  }

  # All templates go to personal_notes
  filepath <- file.path(paths$personal_notes, filename)
  writeLines(template_content, filepath)

  stage_names <- c("Stage I triage", "Stage II analyseblitz", "Stage III draft")
  message(paste(" Created", stage_names[stage_num], "template:", filename))

  return(invisible(filepath))
}

# Keep the simplified wrapper functions for backward compatibility
#' @export
create_stage1_card <- function(idea_title, project_code) {
  create_blitz_template(1, project_code, title = idea_title)
}

#' @export
create_analyseblitz_template <- function(project_code, analysis_type = "general") {
  create_blitz_template(2, project_code, analysis_type = analysis_type)
}

#' @export
create_draftschreiben_template <- function(project_code, paper_type = "journal", target_journal = NULL) {
  create_blitz_template(3, project_code, paper_type = paper_type, target_journal = target_journal)
}

#' Read Stage I card information
#'
#' Extracts key information from Stage I triage card in personal_notes
#'
#' @param project_code Project code
#' @return List with card information
read_stage1_card <- function(project_code) {

  paths <- get_project_paths()
  card_path <- file.path(paths$personal_notes, paste0("stage1_triage_", project_code, ".md"))

  if(!file.exists(card_path)) {
    return(NULL)
  }

  card_content <- readLines(card_path)

  # Extract key information (basic parsing)
  title <- NULL
  research_question <- NULL

  # Find title (looking for the idea title, not the card header)
  title_section <- grep("## TITLE", card_content)
  if(length(title_section) > 0 && length(card_content) > title_section) {
    title <- card_content[title_section + 1]
  }

  # Extract research question
  rq_start <- grep("## RESEARCH QUESTION", card_content)
  if(length(rq_start) > 0 && length(card_content) > rq_start) {
    rq_line <- rq_start + 1
    if(rq_line <= length(card_content)) {
      research_question <- card_content[rq_line]
    }
  }

  return(list(
    title = title,
    research_question = research_question
  ))
}

#' Stage II progress tracker
#'
#' Shows current status of Stage II analyseblitz
#'
#' @param project_code Project code (optional, checks all if NULL)
#' @export
stage2_progress_check <- function(project_code = NULL) {

  paths <- get_project_paths()

  if(is.null(project_code)) {
    # Look for all analyseblitz files in personal_notes
    stage2_files <- list.files(paths$personal_notes,
                               pattern = "analyseblitz.*\\.qmd",
                               full.names = TRUE)

    if(length(stage2_files) == 0) {
      message("No Stage II files found in current project.")
      return(invisible(NULL))
    }

    cat("STAGE II FILES FOUND:\n")
    for(file in stage2_files) {
      mod_time <- file.mtime(file)
      cat(paste("-", basename(file), "(modified:", format(mod_time, "%m/%d %H:%M"), ")\n"))
    }
  } else {
    # Check specific project
    template_path <- file.path(paths$personal_notes,
                               paste0("analyseblitz_", project_code, ".qmd"))

    if(file.exists(template_path)) {
      mod_time <- file.mtime(template_path)
      cat(paste(" Stage II file for", project_code, "exists\n"))
      cat(paste("Last modified:", format(mod_time, "%Y-%m-%d %H:%M"), "\n"))

      # Check if it looks completed (basic heuristic)
      content <- readLines(template_path)
      mock_complete <- any(grepl("Mock Analysis Notes:", content))
      real_complete <- any(grepl("Real analysis results:", content))

      cat("Progress indicators:\n")
      cat(paste("- Mock analysis section:", ifelse(mock_complete, " Present", " Incomplete"), "\n"))
      cat(paste("- Real analysis section:", ifelse(real_complete, " Present", " Incomplete"), "\n"))

    } else {
      cat(paste(" No Stage II file found for", project_code, "\n"))
      cat("Use create_blitz_template(2, '", project_code, "') to start Stage II.\n", sep = "")
    }
  }
}

#' Stage III progress and word count tracker
#'
#' Shows completion status and word counts for draft
#'
#' @param project_code Project code
#' @export
stage3_progress_check <- function(project_code) {

  paths <- get_project_paths()
  draft_path <- file.path(paths$personal_notes, paste0("draft_", project_code, ".qmd"))

  if(!file.exists(draft_path)) {
    cat(paste(" No Stage III draft found for", project_code, "\n"))
    cat("Use create_blitz_template(3, '", project_code, "') to start Stage III.\n", sep = "")
    return(invisible(NULL))
  }

  # Read draft content
  draft_content <- readLines(draft_path)

  cat(" STAGE III PROGRESS REPORT \n")
  cat(paste("Project:", project_code, "\n"))
  cat(paste("Last modified:", format(file.mtime(draft_path), "%Y-%m-%d %H:%M"), "\n\n"))

  # Check section completion (basic heuristics)
  sections <- list(
    "Abstract" = "# Abstract",
    "Introduction" = "# Introduction",
    "Methods" = "# Methods",
    "Results" = "# Results",
    "Discussion" = "# Discussion"
  )

  cat("SECTION COMPLETION STATUS:\n")
  for(section in names(sections)) {
    section_start <- grep(sections[[section]], draft_content)
    if(length(section_start) > 0) {
      # Very basic completion check - look for content after section header
      section_lines <- draft_content[(section_start + 1):min(length(draft_content), section_start + 10)]
      content_lines <- section_lines[!grepl("^<!--", section_lines) &
                                       !grepl("^$", section_lines) &
                                       !grepl("^##", section_lines)]

      has_content <- any(nchar(trimws(content_lines)) > 10)
      status <- ifelse(has_content, " Has content", " Needs work")
    } else {
      status <- " Missing"
    }
    cat(paste("-", section, ":", status, "\n"))
  }

  # Check for bibliography file (now in personal_notes)
  bib_path <- file.path(paths$personal_notes, "references.bib")
  if(file.exists(bib_path)) {
    bib_content <- readLines(bib_path)
    ref_count <- length(grep("^@", bib_content))
    cat(paste("- References:", ref_count, "entries in bibliography\n"))
  }

  cat("\n Use word count tool in your editor to check section lengths!\n")
}

# Content creation functions (internal)
create_stage1_content <- function(idea_title, project_code) {
  paste0(
    "# Stage I: Project Planning\n\n",
    "## Date\n", Sys.Date(), "\n\n",
    "## Blitz: ", idea_title, "\n\n",
    "### Research Question\n",
    "What exactly are we asking or testing?\n\n",
    "### Dataset(s)\n",
    "- Source: [e.g., NHANES, All of Us]\n",
    "- Status: [Accessible / Apply Needed / Unavailable]\n",
    "- Notes: [E.g., size, restrictions]\n\n",
    "### Sample Description (Table 1 Sketch)\n",
    "- N ≈ [Estimated size]\n\n",
    "| Role | Variable | Type | Values/Range | Description |\n",
    "|------|----------|------|--------------|-------------|\n",
    "| Exposure | | | | |\n",
    "| Outcome | | | | |\n",
    "| Covariate | age | numeric | | Age in years |\n",
    "| Covariate | diagnosis_date | date | 2020-01-01 - 2023-12-31 | Date of diagnosis |\n",
    "| Covariate | gender | factor | | Gender/Sex |\n",
    "| Covariate | | | | |\n\n",
    "### Methods / Analysis Plan\n",
    "- [Statistical model or comparison]\n",
    "- [Adjustments / stratification]\n",
    "- [Visuals planned]\n\n",
    "### Background Rationale\n",
    "- What's the motivation or literature gap?\n\n",
    "### Barriers or Challenges\n",
    "- [Data access, missing vars, complexity, etc.]\n\n",
    "### Status / Next Step\n",
    "- [ ] Ready to share\n",
    "- [ ] Needs more development\n",
    "- [ ] Discard\n",
    "- Next step: [e.g., Consult with X, Apply for access]\n"
  )
}

# internal function to parse Stage I variables
.parse_stage_i_variables <- function(project_code) {
  paths <- get_project_paths()
  stage_i_path <- file.path(paths$personal_notes, paste0("stage1_triage_", project_code, ".md"))

  if (!file.exists(stage_i_path)) {
    warning("No stage_i.md found")
    return(NULL)
  }

  content <- readLines(stage_i_path)

  # Extract sample size
  n_line <- grep("^- N ≈", content)
  sample_size <- NULL
  if (length(n_line) > 0) {
    sample_size <- gsub("- N ≈ ", "", content[n_line[1]])
    sample_size <- gsub("\\[|\\]", "", sample_size)  # Remove brackets
    # Try to extract numeric value
    sample_size_num <- as.numeric(gsub("[^0-9]", "", sample_size))
    if (!is.na(sample_size_num)) {
      sample_size <- sample_size_num
    }
  }

  # Find the variables table
  table_start <- grep("^\\| Role", content)
  if (length(table_start) == 0) return(NULL)

  # Parse table rows
  table_lines <- content[(table_start + 2):length(content)]
  table_lines <- table_lines[grepl("^\\|", table_lines)]

  variables <- list()
  for (line in table_lines) {
    parts <- trimws(strsplit(line, "\\|")[[1]])
    parts <- parts[parts != ""]  # Remove empty elements

    if (length(parts) >= 5 && parts[1] != "Role") {
      var_name <- parts[2]
      if (var_name != "") {
        # Normalize type: treat "categorical" as "factor"
        var_type <- tolower(trimws(parts[3]))
        if (var_type == "categorical") {
          var_type <- "factor"
        }

        variables[[var_name]] <- list(
          role = parts[1],
          type = var_type,  # Use normalized type
          values = parts[4],
          description = parts[5]
        )
      }
    }
  }

  # Extract other useful information
  research_question <- extract_section_after_header(content, "### Research Question")
  analysis_plan <- extract_section_after_header(content, "### Methods / Analysis Plan")

  # Package everything
  stage_i_info <- list(
    variables = variables,
    sample_size = sample_size,
    research_question = research_question,
    analysis_plan = analysis_plan
  )

  # Update .proman with project info
  proman_data <- read_proman()
  if (is.null(proman_data$project_info)) {
    proman_data$project_info <- list()
  }
  proman_data$project_info <- stage_i_info
  proman_data$project_info$stage_i_parsed <- as.character(Sys.Date())
  write_proman(proman_data)

  return(stage_i_info)
}

# Add helper function for parsing
extract_section_after_header <- function(content, header) {
  header_line <- grep(paste0("^", header), content)
  if (length(header_line) == 0) return(NULL)

  # Find next header (any line starting with ###)
  next_header <- grep("^###", content[(header_line[1] + 1):length(content)])[1]
  if (is.na(next_header)) {
    end_line <- length(content)
  } else {
    end_line <- header_line[1] + next_header - 1
  }

  section_content <- content[(header_line[1] + 1):end_line]
  section_content <- section_content[section_content != ""]
  paste(trimws(section_content), collapse = " ")
}


create_stage2_content <- function(project_code, analysis_type, stage1_info = NULL) {

  # If stage1_info not provided, parse it
  if (is.null(stage1_info)) {
    stage1_info <- .parse_stage_i_variables(project_code)
  }

  # Generate simulation code based on parsed variables
  sim_code <- ""
  if (!is.null(stage1_info$variables) && length(stage1_info$variables) > 0) {
    sim_lines <- character()

    # Start with creating the data frame with just ID
    sim_lines <- c(sim_lines, "# Create data frame with ID column")
    sim_lines <- c(sim_lines, "sim_data <- data.frame(id = 1:n)")
    sim_lines <- c(sim_lines, "")
    sim_lines <- c(sim_lines, "# Add variables from Stage I")

    for (var_name in names(stage1_info$variables)) {
      var_info <- stage1_info$variables[[var_name]]

      # Convert variable name to snake_case
      clean_var_name <- to_snake_case(var_name)

      # Add comment about the variable
      sim_lines <- c(sim_lines,
                     sprintf("# %s (%s): %s", var_name, var_info$type, var_info$description))

      if (var_info$type == "factor" || var_info$type == "categorical") {
        # Parse factor levels
        levels <- strsplit(var_info$values, ",")[[1]]
        levels <- trimws(levels)
        levels <- levels[levels != ""]

        if (length(levels) > 0) {
          sim_lines <- c(sim_lines,
                         sprintf('sim_data$%s <- sample(c(%s), n, replace = TRUE)',
                                 clean_var_name,
                                 paste0('"', levels, '"', collapse = ", ")))
        } else {
          sim_lines <- c(sim_lines,
                         sprintf('sim_data$%s <- sample(c("A", "B"), n, replace = TRUE)  # Update with actual levels',
                                 clean_var_name))
        }

      } else if (var_info$type == "numeric") {
        if (grepl("-", var_info$values)) {
          # Range like "18-65"
          range_parts <- strsplit(var_info$values, "-")[[1]]
          range_vals <- as.numeric(trimws(range_parts))
          if (length(range_vals) == 2 && !any(is.na(range_vals))) {
            sim_lines <- c(sim_lines,
                           sprintf('sim_data$%s <- runif(n, min = %d, max = %d)',
                                   clean_var_name, range_vals[1], range_vals[2]))
          } else {
            sim_lines <- c(sim_lines,
                           sprintf('sim_data$%s <- rnorm(n, mean = 0, sd = 1)  # Update distribution',
                                   clean_var_name))
          }
        } else if (tolower(var_info$values) == "positive") {
          sim_lines <- c(sim_lines,
                         sprintf('sim_data$%s <- rgamma(n, shape = 2, rate = 1)  # Positive values',
                                 var_name))
        } else {
          sim_lines <- c(sim_lines,
                         sprintf('sim_data$%s <- rnorm(n, mean = 0, sd = 1)',
                                 var_name))
        }
      } else if (var_info$type == "date") {
        # Handle date variables
        if (grepl("-", var_info$values)) {
          # Date range like "2020-01-01 - 2023-12-31"
          date_parts <- strsplit(var_info$values, " - ")[[1]]  # Note: split on " - " with spaces
          date_parts <- trimws(date_parts)

          # Try multiple date formats
          date_formats <- c(
            "%Y-%m-%d",     # ISO: 2023-12-31
            "%m/%d/%Y",     # US: 12/31/2023
            "%d/%m/%Y",     # EU: 31/12/2023
            "%d.%m.%Y",     # EU dots: 31.12.2023
            "%d.%m.%y",     # EU dots short: 31.12.23
            "%B %d, %Y",    # Long: December 31, 2023
            "%b %d, %Y",    # Short: Dec 31, 2023
            "%d-%b-%Y",     # 31-Dec-2023
            "%d %B %Y",     # 31 December 2023
            "%d %b %Y"      # 31 Dec 2023
          )

          start_date <- NA
          end_date <- NA

          for(fmt in date_formats) {
            if(is.na(start_date)) {
              start_date <- try(as.Date(date_parts[1], format = fmt), silent = TRUE)
              if(inherits(start_date, "try-error")) start_date <- NA
            }
            if(is.na(end_date)) {
              end_date <- try(as.Date(date_parts[length(date_parts)], format = fmt), silent = TRUE)
              if(inherits(end_date, "try-error")) end_date <- NA
            }
            # Stop if both dates are successfully parsed
            if(!is.na(start_date) && !is.na(end_date)) break
          }

          if (!is.na(start_date) && !is.na(end_date)) {
            sim_lines <- c(sim_lines,
                           sprintf('sim_data$%s <- sample(seq(as.Date("%s"), as.Date("%s"), by = "day"), n, replace = TRUE)',
                                   clean_var_name, start_date, end_date))
          } else {
            # Default to recent dates if parsing fails
            sim_lines <- c(sim_lines,
                           sprintf('sim_data$%s <- sample(seq(Sys.Date() - 365, Sys.Date(), by = "day"), n, replace = TRUE)  # Update date range',
                                   clean_var_name))
          }
        } else {
          # No range specified, use default recent dates
          sim_lines <- c(sim_lines,
                         sprintf('sim_data$%s <- sample(seq(Sys.Date() - 365, Sys.Date(), by = "day"), n, replace = TRUE)  # Update date range',
                                 clean_var_name))
        }
      }

      sim_lines <- c(sim_lines, "")  # Add blank line between variables
    }

    sim_code <- paste(sim_lines, collapse = "\n")
  }

  # Determine sample size
  n_value <- ifelse(!is.null(stage1_info$sample_size),
                    stage1_info$sample_size,
                    500)

  # Now build the template with the improved simulation code
  paste0(
    "---\n",
    "title: \"Stage II Analyseblitz: ", project_code, "\"\n",
    "author: \"Your Name\"\n",
    "date: \"`r Sys.Date()`\"\n",
    "format: html\n",
    "editor: visual\n",
    "---\n\n",
    "# STAGE II: ANALYSEBLITZ - ", project_code, "\n\n",
    "**Analysis Type:** ", analysis_type, "\n",
    "**Created:** `r Sys.Date()`\n\n",
    ifelse(!is.null(stage1_info$research_question),
           paste0("**Research Question:** ", stage1_info$research_question, "\n\n"), ""),
    "---\n\n",
    "## PREP PHASE (15-30 minutes)\n\n",
    "### Relevant References\n",
    "```{r}\n",
    "# Key papers/tutorials found:\n",
    "# 1. [Paper title] - [Key finding/method]\n",
    "# 2. [Tutorial] - [Specific technique]\n",
    "```\n\n",
    "### Package Requirements\n",
    "```{r setup}\n",
    "library(PRoMan)\n",
    "paths <- get_project_paths()\n\n",
    "library(tidyverse)\n",
    "# Add other packages as needed\n",
    "```\n\n",
    "## MOCK ANALYSIS PHASE\n\n",
    "### Data Simulation\n",
    "```{r simulate-data}\n",
    "# Data simulation based on Stage I specifications\n",
    "set.seed(42)\n",
    "n <- ", n_value, "  # Sample size from Stage I\n\n",
    ifelse(sim_code != "",
           sim_code,
           "# No variables parsed from Stage I - add manually\n# Example:\nsim_data <- data.frame(id = 1:n)\nsim_data$outcome <- rnorm(n)\nsim_data$predictor <- rnorm(n)\n"),
    "\n",
    "# Add relationships between variables if needed\n",
    "# Example: create correlation between predictors\n",
    "# sim_data$predictor2 <- sim_data$predictor1 * 0.5 + rnorm(n, sd = 0.5)\n\n",
    "# Check the simulated data\n",
    "glimpse(sim_data)\n",
    "summary(sim_data)\n",
    "```\n\n",
    "### Mock Analysis Run\n",
    "```{r mock-analysis}\n",
    "# Quick descriptives\n",
    "summary(sim_data)\n\n",
    "# Main analysis on simulated data\n",
    "# [This will be copy-pasted for real analysis]\n\n",
    "# Example analysis - replace with your specific method:\n",
    "# model <- lm(outcome ~ predictor + group, data = sim_data)\n",
    "# summary(model)\n",
    "```\n\n",
    "### Mock Visualization\n",
    "```{r mock-plots}\n",
    "# Key visualization(s)\n",
    "# ggplot(sim_data, aes(x = predictor, y = outcome, color = group)) +\n",
    "#   geom_point() +\n",
    "#   geom_smooth(method = 'lm') +\n",
    "#   theme_minimal() +\n",
    "#   labs(title = 'Mock Analysis Results')\n",
    "```\n\n",
    "**Mock Analysis Notes:**\n",
    "- Does the code run without errors?\n",
    "- Are the results in expected format?\n",
    "- What adjustments needed for real data?\n\n",
    "---\n\n",
    "## REAL ANALYSIS PHASE\n\n",
    "### Load Real Data\n",
    "```{r load-real-data}\n",
    "# Use PRoMan functions to load data\n",
    "# data_files <- select_files_by_keyword('your_keyword')\n",
    "# real_data <- read.csv(file.path(paths$data_source, data_files[1]))\n",
    "```\n\n",
    "### Real Analysis (Copy from Mock)\n",
    "```{r real-analysis}\n",
    "# COPY-PASTE from mock analysis section above\n",
    "# Replace 'sim_data' with 'real_data'\n\n",
    "# Real analysis results:\n",
    "```\n\n",
    "### Final Visualization\n",
    "```{r final-plots}\n",
    "# COPY-PASTE from mock visualization\n",
    "# Update data source and refine aesthetics\n",
    "# Use save_figure() to register plots\n",
    "```\n\n",
    "---\n\n",
    "## SNIFF TEST\n\n",
    "### Results Assessment\n",
    "```{r}\n",
    "# Do the results make sense?\n",
    "# - Effect sizes reasonable?\n",
    "# - Direction of relationships as expected?\n",
    "# - Sample characteristics match expectations?\n",
    "# - Any red flags or surprising findings?\n",
    "```\n\n",
    "### Next Steps for Stage III\n",
    "- [ ] Results interpretation complete\n",
    "- [ ] Key figures identified\n",
    "- [ ] Additional analyses needed?\n",
    "- [ ] Ready for draftschreiben?\n\n",
    "**Stage II Completed:** [Date] | **Total Time:** [Hours]\n"
  )
}

create_stage3_content <- function(project_code, paper_type, target_journal, stage1_info = NULL, stage2_info = NULL) {

  # If not provided, try to read the info
  if(is.null(stage1_info)) {
    stage1_info <- .parse_stage_i_variables(project_code)
  }
  if(is.null(stage2_info)) {
    stage2_info <- read_stage2_info(project_code)
  }

  # Determine word targets based on paper type
  word_targets <- switch(paper_type,
                         "journal" = list(abstract = 250, intro = 800, methods = 600, results = 800, discussion = 1000),
                         "conference" = list(abstract = 200, intro = 400, methods = 300, results = 400, discussion = 500),
                         "brief" = list(abstract = 150, intro = 300, methods = 200, results = 300, discussion = 400)
  )

  # Extract information from Stage I
  research_question <- ifelse(!is.null(stage1_info$research_question),
                              stage1_info$research_question,
                              "[Research objective from Stage I]")

  # Try to extract sample info from Stage I
  sample_desc <- "[Brief methods summary from Stage II analyseblitz]"
  if(!is.null(stage1_info)) {
    # You could parse the SAMPLE DESCRIPTION section here
    # For now, using placeholder
    sample_desc <- "[Check Stage I card for sample description]"
  }

  # Extract methods from Stage II
  methods_summary <- "[Statistical approach from Stage II]"
  if(!is.null(stage2_info$analysis_type)) {
    methods_summary <- paste("Analysis approach:", stage2_info$analysis_type)
  }

  paste0(
    "---\n",
    "title: \"", ifelse(!is.null(stage1_info$title), stage1_info$title, "Draft Title"), "\"\n",
    "author: \"Your Name\"\n",
    "date: \"`r Sys.Date()`\"\n",
    "format: \n",
    "  docx:\n",
    "    reference-doc: custom-reference.docx\n",
    "bibliography: references.bib\n",
    "csl: apa.csl\n",
    "---\n\n",
    "# STAGE III: DRAFTSCHREIBEN - ", project_code, "\n\n",
    "**Paper Type:** ", paper_type, "\n",
    ifelse(!is.null(target_journal), paste0("**Target Journal:** ", target_journal, "\n"), ""),
    "**Created:** `r Sys.Date()`\n",
    "**Target:** 80% complete before sharing\n\n",
    "**Word Count Targets:**\n",
    "- Abstract: ", word_targets$abstract, " words\n",
    "- Introduction: ", word_targets$intro, " words\n",
    "- Methods: ", word_targets$methods, " words\n",
    "- Results: ", word_targets$results, " words\n",
    "- Discussion: ", word_targets$discussion, " words\n\n",
    "---\n\n",
    "# Abstract\n\n",
    "<!-- Target: ", word_targets$abstract, " words -->\n",
    "<!-- Current: [] words -->\n\n",
    "**Objective:** ", research_question, "\n\n",
    "**Methods:** ", sample_desc, " ", methods_summary, "\n\n",
    "**Results:** ",
    ifelse(!is.null(stage2_info$sniff_test),
           "[Key findings from Stage II - check sniff test results]",
           "[Key findings from Stage II analysis]"), "\n\n",
    "**Conclusions:** [Main takeaway and implications]\n\n",
    "**Keywords:** [5-7 keywords]\n\n",
    "---\n\n",
    "# Introduction\n\n",
    "<!-- Target: ", word_targets$intro, " words -->\n",
    "<!-- FROM STAGE I BACKGROUND WARRANT -->\n",
    ifelse(!is.null(stage1_info),
           "<!-- Check Stage I card for background warrant and literature gap -->\n\n",
           ""),
    "## Background and Rationale\n",
    "[Context and importance - expand from Stage I background warrant]\n\n",
    "---\n\n",
    "# References\n\n",
    "<!-- Auto-populated from references.bib -->\n"
  )
}


#' Create bibliography template
#'
#' Sets up basic bibliography file in personal_notes
#'
#' @param project_code Project code (optional, for project-specific bib)
create_bibliography_template <- function(project_code = NULL) {

  paths <- get_project_paths()

  if(!is.null(project_code)) {
    bib_path <- file.path(paths$personal_notes, paste0("references_", project_code, ".bib"))
  } else {
    bib_path <- file.path(paths$personal_notes, "references.bib")
  }

  if(file.exists(bib_path)) {
    message("Bibliography file already exists")
    return(invisible(bib_path))
  }

  bib_content <- paste0(
    "% Bibliography for Blitzschreiben project\n",
    "% Add references in BibTeX format\n",
    "% Use Zotero, Mendeley, or manual entry\n\n",
    "% Example entry:\n",
    "% @article{author2023,\n",
    "%   title={Example Article Title},\n",
    "%   author={Author, First and Author, Second},\n",
    "%   journal={Journal Name},\n",
    "%   volume={10},\n",
    "%   number={2},\n",
    "%   pages={123--145},\n",
    "%   year={2023},\n",
    "%   publisher={Publisher Name}\n",
    "% }\n\n"
  )

  writeLines(bib_content, bib_path)
  message(paste("Created bibliography template:", basename(bib_path)))

  return(invisible(bib_path))
}

#' Track source collection progress
#'
#' Monitors PDF sources in standard sources folder
#'
#' @param search_strategy Description of search approach
#' @param new_sources Number of new sources found
#' @param project_code Project code (optional)
#' @export
track_sources <- function(search_strategy = NULL, new_sources = 0, project_code = NULL) {

  paths <- get_project_paths()

  # Count PDFs in standard sources folder
  if(dir.exists(paths$sources)) {
    pdf_files <- list.files(paths$sources, pattern = "\\.pdf$", ignore.case = TRUE)
    total_pdfs <- length(pdf_files)
  } else {
    total_pdfs <- 0
  }

  cat(" SOURCE TRACKING REPORT \n")
  if(!is.null(project_code)) {
    cat(paste("Project:", project_code, "\n"))
  }
  cat(paste("Total PDFs in sources folder:", total_pdfs, "\n"))

  if(new_sources > 0) {
    cat(paste("New sources added this session:", new_sources, "\n"))
  }

  if(!is.null(search_strategy)) {
    cat(paste("Current search strategy:", search_strategy, "\n"))
  }

  # Log this source tracking
  if(!is.null(project_code)) {
    log_blitz_activity("Source Collection", project_code,
                       notes = paste("Total PDFs:", total_pdfs, "| New:", new_sources))
  }

  # Suggest next steps based on count
  if(total_pdfs < 10) {
    cat(" Suggestion: Consider expanding literature search\n")
  } else if(total_pdfs > 50) {
    cat(" Suggestion: Focus on most relevant sources\n")
  }

  return(invisible(total_pdfs))
}

### TIMER FUNCTIONS (STAGE I) ###

#' Start triage timer with notifications
#'
#' Starts a 15-minute countdown for Stage I triage
#'
#' @param minutes Time limit for triage (default: 15)
#' @param project_code Project being worked on (for logging)
#' @export
start_triage_timer <- function(minutes = 15, project_code = NULL) {

  start_time <- Sys.time()

  cat(" STAGE I TRIAGE TIMER STARTED \n")
  cat(paste("Time limit:", minutes, "minutes\n"))
  if(!is.null(project_code)) {
    cat(paste("Project:", project_code, "\n"))
  }
  cat(paste("Started at:", format(start_time, "%H:%M:%S"), "\n"))
  cat("Focus on rapid completion - don't overthink!\n")
  cat(paste(rep("=", 40), collapse = ""), "\n\n")

  # Store timer info globally for checking
  assign(".blitz_timer", list(
    start = start_time,
    minutes = minutes,
    project = project_code
  ), envir = .GlobalEnv)

  message("Timer started! Use check_triage_time() to see remaining time.")
  message("Use end_triage_timer() when finished.")
}

#' Check remaining triage time
#'
#' Shows time remaining on current triage timer
#'
#' @export
check_triage_time <- function() {

  if(!exists(".blitz_timer", envir = .GlobalEnv)) {
    message("No active timer found. Use start_triage_timer() first.")
    return(invisible(NULL))
  }

  timer_info <- get(".blitz_timer", envir = .GlobalEnv)

  elapsed <- as.numeric(difftime(Sys.time(), timer_info$start, units = "mins"))
  remaining <- timer_info$minutes - elapsed

  if(remaining > 0) {
    cat(" TRIAGE TIMER STATUS \n")
    if(!is.null(timer_info$project)) {
      cat(paste("Project:", timer_info$project, "\n"))
    }
    cat(paste("Elapsed:", round(elapsed, 1), "minutes\n"))
    cat(paste("Remaining:", round(remaining, 1), "minutes\n"))

    if(remaining < 3) {
      cat(" LESS THAN 3 MINUTES LEFT! \n")
    } else if(remaining < 5) {
      cat(" 5 minutes or less - wrap up soon! \n")
    }
  } else {
    cat(" TIME'S UP! \n")
    if(!is.null(timer_info$project)) {
      cat(paste("Project:", timer_info$project, "\n"))
    }
    cat(paste("Overtime:", round(abs(remaining), 1), "minutes\n"))
    cat("Make your triage decision now!\n")
  }

  cat(paste(rep("=", 30), collapse = ""), "\n")
}

#' End triage timer and log completion
#'
#' Stops timer and logs the triage session
#'
#' @param decision Triage decision made
#' @param next_steps Brief note about next steps
#' @export
end_triage_timer <- function(decision = NULL, next_steps = NULL) {

  if(!exists(".blitz_timer", envir = .GlobalEnv)) {
    message("No active timer found.")
    return(invisible(NULL))
  }

  timer_info <- get(".blitz_timer", envir = .GlobalEnv)
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, timer_info$start, units = "mins"))

  cat(" STAGE I TRIAGE COMPLETED \n")
  if(!is.null(timer_info$project)) {
    cat(paste("Project:", timer_info$project, "\n"))
  }
  cat(paste("Total time:", round(total_time, 1), "minutes\n"))

  if(total_time <= timer_info$minutes) {
    cat(" Completed within time limit!\n")
  } else {
    cat(" Went over time - aim to be faster next time.\n")
  }

  if(!is.null(decision)) {
    cat(paste("Decision:", decision, "\n"))
  }

  if(!is.null(next_steps)) {
    cat(paste("Next steps:", next_steps, "\n"))
  }

  # Log this triage session
  log_blitz_activity("Stage I Triage", timer_info$project, total_time, decision)

  # Clean up timer
  remove(".blitz_timer", envir = .GlobalEnv)

  cat("Great work! \n")
}

### DATA SIMULATION HELPER (STAGE II) ###

#' Generate data simulation code based on Stage I specs
#'
#' Creates R code for data simulation matching Stage I card specifications
#'
#' @param project_code Project code
#' @param sample_size Sample size from Stage I card
#' @param variables List of key variables from Stage I card
#' @param analysis_type Type of analysis planned
#' @return Character string with simulation code
#' @export
generate_simulation_code <- function(project_code, sample_size = 100, variables = NULL, analysis_type = "general") {

  # Basic template
  sim_code <- paste0(
    "# Data simulation for ", project_code, "\n",
    "set.seed(42)\n\n",
    "simulated_data <- data.frame(\n",
    "  id = 1:", sample_size, ",\n"
  )

  # Add variables based on analysis type
  if(analysis_type == "regression" || analysis_type == "general") {
    sim_code <- paste0(sim_code,
                       "  outcome = rnorm(", sample_size, "),\n",
                       "  predictor1 = rnorm(", sample_size, "),\n",
                       "  predictor2 = rnorm(", sample_size, "),\n",
                       "  group = sample(c('Treatment', 'Control'), ", sample_size, ", replace = TRUE),\n",
                       "  age = rnorm(", sample_size, ", mean = 45, sd = 15)\n"
    )
  } else if(analysis_type == "survival") {
    sim_code <- paste0(sim_code,
                       "  time = rexp(", sample_size, ", rate = 0.1),\n",
                       "  status = rbinom(", sample_size, ", 1, 0.7),\n",
                       "  treatment = sample(c(0, 1), ", sample_size, ", replace = TRUE),\n",
                       "  age = rnorm(", sample_size, ", mean = 60, sd = 12)\n"
    )
  }

  # Add custom variables if provided
  if(!is.null(variables) && length(variables) > 0) {
    for(var in variables) {
      sim_code <- paste0(sim_code, "  ", var, " = rnorm(", sample_size, "),\n")
    }
  }

  # Close the data.frame
  sim_code <- paste0(sim_code, ")\n\n")

  # Add basic checks
  sim_code <- paste0(sim_code,
                     "# Quick check\n",
                     "glimpse(simulated_data)\n",
                     "summary(simulated_data)\n"
  )

  return(sim_code)
}

### SOURCE MANAGEMENT ###

#' Pearl growing search tracker
#'
#' Helps track upstream/downstream reference searches
#'
#' @param seed_paper Main paper for pearl growing
#' @param direction "upstream" (references) or "downstream" (citing papers)
#' @param found_count Number of relevant papers found
#' @export
pearl_growing_tracker <- function(seed_paper, direction = "upstream", found_count = 0) {

  cat(" PEARL GROWING SEARCH \n")
  cat(paste("Seed paper:", seed_paper, "\n"))
  cat(paste("Direction:", direction, "\n"))
  cat(paste("Relevant papers found:", found_count, "\n"))

  if(direction == "upstream") {
    cat(" Tip: Check reference list of seed paper\n")
  } else {
    cat(" Tip: Use Google Scholar 'Cited by' or Web of Science\n")
  }

  # Track in log
  log_blitz_activity("Pearl Growing Search",
                     notes = paste(direction, "from", seed_paper, "-", found_count, "found"))
}

### PIPELINE MANAGEMENT ###

#' Show complete Blitzschreiben pipeline status
#'
#' Displays all projects across all stages with current status
#'
#' @param base_path Base path containing all projects
#' @export
blitzschreiben_status <- function(base_path = NULL) {

  if(is.null(base_path)) {
    # Try to find tracker from current location
    possible_paths <- c(getwd(), dirname(getwd()), "~/Desktop/projects")
    tracker_file <- NULL

    for(path in possible_paths) {
      test_file <- file.path(path, "blitzschreiben_tracker.csv")
      if(file.exists(test_file)) {
        tracker_file <- test_file
        base_path <- path
        break
      }
    }

    if(is.null(tracker_file)) {
      cat(" No Blitzschreiben tracker found. Create a project first!\n")
      return(invisible(NULL))
    }
  } else {
    tracker_file <- file.path(base_path, "blitzschreiben_tracker.csv")
  }

  if(!file.exists(tracker_file)) {
    cat(" No Blitzschreiben tracker found at:", tracker_file, "\n")
    return(invisible(NULL))
  }

  tracker <- read.csv(tracker_file, stringsAsFactors = FALSE)
  active_projects <- tracker[tracker$status == "active", ]

  cat(" BLITZSCHREIBEN PIPELINE STATUS \n")
  cat(paste("Updated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n"))
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # Stage-wise breakdown
  for(stage_num in 1:3) {
    stage_name <- switch(stage_num,
                         "1" = "STAGE I: IDEA TRIAGE",
                         "2" = "STAGE II: ANALYSEBLITZ",
                         "3" = "STAGE III: DRAFTSCHREIBEN"
    )

    stage_projects <- active_projects[active_projects$stage == stage_num, ]

    cat(paste("", stage_name, "\n"))
    cat(paste(rep("-", 25), collapse = ""), "\n")

    if(nrow(stage_projects) > 0) {
      for(i in 1:nrow(stage_projects)) {
        proj <- stage_projects[i, ]
        cat(paste("?", proj$project_code, "-", proj$title, "\n"))
        cat(paste("  Last activity:", proj$last_activity, "\n"))
        cat(paste("  Next step:", proj$next_step, "\n"))
        if(proj$priority == "high") cat("   HIGH PRIORITY\n")
        cat("\n")
      }
    } else {
      cat("  No active projects in this stage\n\n")
    }
  }

  # Pipeline health check
  stage1_count <- nrow(active_projects[active_projects$stage == 1, ])
  stage2_count <- nrow(active_projects[active_projects$stage == 2, ])
  stage3_count <- nrow(active_projects[active_projects$stage == 3, ])

  cat(" PIPELINE HEALTH CHECK:\n")
  cat(paste("Stage I projects:", stage1_count, "(target: 4-5)\n"))
  cat(paste("Stage II projects:", stage2_count, "(target: 3)\n"))
  cat(paste("Stage III projects:", stage3_count, "(target: 2)\n"))

  # Recommendations
  cat("\n RECOMMENDATIONS:\n")
  if(stage1_count < 4) {
    cat(" Add more ideas to Stage I triage\n")
  }
  if(stage1_count > 6) {
    cat(" Consider triaging some Stage I projects (discard or advance)\n")
  }
  if(stage2_count > 4) {
    cat(" Focus on completing Stage II analyses\n")
  }
  if(stage3_count > 3) {
    cat(" Prioritize finishing Stage III drafts\n")
  }

  return(invisible(active_projects))
}

#' Get today's Blitzschreiben tasks based on day of week
#'
#' Shows what you should be working on today based on your weekly schedule
#'
#' @param force_day Override today's day for testing
#' @export
todays_blitz_tasks <- function(force_day = NULL) {

  if(!is.null(force_day)) {
    today <- force_day
  } else {
    today <- weekdays(Sys.Date())
  }

  # Get current pipeline status
  tracker_file <- find_blitz_tracker()
  if(is.null(tracker_file)) {
    cat(" No Blitzschreiben tracker found\n")
    return(invisible(NULL))
  }

  tracker <- read.csv(tracker_file, stringsAsFactors = FALSE)
  active_projects <- tracker[tracker$status == "active", ]

  cat("", toupper(today), "BLITZSCHREIBEN AGENDA \n")
  cat(paste("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n"))
  cat(paste(rep("=", 40), collapse = ""), "\n\n")

  if(today == "Monday") {
    cat("MONDAY: STAGE I TRIAGE + STAGE II PREP\n\n")

    # Stage I projects to triage
    stage1_projects <- active_projects[active_projects$stage == 1, ]
    if(nrow(stage1_projects) > 0) {
      cat("STAGE I TRIAGE (15 min each):\n")
      for(i in 1:min(3, nrow(stage1_projects))) {
        cat(paste("?", stage1_projects[i, ]$project_code, "-", stage1_projects[i, ]$title, "\n"))
      }
    } else {
      cat("No Stage I projects - consider adding new ideas!\n")
    }

    cat("\nSTAGE II PREP (15-30 min each):\n")
    stage2_projects <- active_projects[active_projects$stage == 2, ]
    if(nrow(stage2_projects) > 0) {
      for(i in 1:min(2, nrow(stage2_projects))) {
        cat(paste("?", stage2_projects[i, ]$project_code, "- Find references & packages\n"))
      }
    } else {
      cat("No Stage II projects ready\n")
    }

  } else if(today == "Tuesday") {
    cat(" TUESDAY: STAGE II MOCK ANALYSIS\n\n")

    stage2_projects <- active_projects[active_projects$stage == 2, ]
    if(nrow(stage2_projects) > 0) {
      cat("MOCK ANALYSIS FOCUS:\n")
      for(i in 1:min(2, nrow(stage2_projects))) {
        cat(paste("?", stage2_projects[i, ]$project_code, "- Data simulation & mock run\n"))
      }
      cat("\n Goal: Get analysis code working with simulated data\n")
    } else {
      cat("No Stage II projects ready for mock analysis\n")
    }

  } else if(today == "Wednesday") {
    cat(" WEDNESDAY: REAL ANALYSIS DAY\n\n")

    stage2_projects <- active_projects[active_projects$stage == 2, ]
    if(nrow(stage2_projects) > 0) {
      cat("REAL ANALYSIS EXECUTION:\n")
      for(i in 1:min(2, nrow(stage2_projects))) {
        cat(paste("?", stage2_projects[i, ]$project_code, "- Run analysis on real data\n"))
      }
      cat("\n Goal: Copy-paste from mock analysis, interpret results\n")
    } else {
      cat("No Stage II projects ready for real analysis\n")
    }

  } else if(today == "Thursday") {
    cat(" THURSDAY: DRAFTSCHREIBENTAG\n\n")

    stage3_projects <- active_projects[active_projects$stage == 3, ]
    if(nrow(stage3_projects) > 0) {
      cat("WRITING FOCUS:\n")
      for(i in 1:min(2, nrow(stage3_projects))) {
        cat(paste("?", stage3_projects[i, ]$project_code, "- Paper skeleton & writing\n"))
      }
      cat("\n Goal: Advance toward 80% completion threshold\n")
    } else {
      cat("No Stage III projects ready for writing\n")
      cat("Consider promoting Stage II projects that passed sniff test\n")
    }

  } else if(today == "Friday") {
    cat(" FRIDAY: QA REVIEW & SOURCE READING\n\n")

    cat("QA REVIEW TASKS:\n")
    completed_this_week <- active_projects[active_projects$last_activity >= (Sys.Date() - 6), ]
    if(nrow(completed_this_week) > 0) {
      cat("Review this week's completed work:\n")
      for(i in 1:nrow(completed_this_week)) {
        cat(paste("  -", completed_this_week[i, ]$project_code, "\n"))
      }
    }

    cat("\nSOURCE READING:\n")
    cat("Read new PDFs collected this week\n")
    cat("Look for new Stage I ideas from reading\n")
    cat("Update existing projects with new insights\n")

  } else {
    cat("WEEKEND: REST OR CATCH-UP\n\n")
    cat("Optional light reading\n")
    cat("Catch up on any delayed tasks\n")
    cat("Plan for next week's priorities\n")
  }

  # Show pipeline balance
  cat("\n CURRENT PIPELINE BALANCE:\n")
  stage_counts <- table(active_projects$stage)
  cat(paste("Stage I:", ifelse("1" %in% names(stage_counts), stage_counts[["1"]], 0), "| "))
  cat(paste("Stage II:", ifelse("2" %in% names(stage_counts), stage_counts[["2"]], 0), "| "))
  cat(paste("Stage III:", ifelse("3" %in% names(stage_counts), stage_counts[["3"]], 0), "\n"))
}

#' Find Blitzschreiben tracker file
#'
#' Internal function to locate the central tracker
find_blitz_tracker <- function() {
  paths <- get_project_paths()
  tracker_file <- file.path(paths$personal_notes, "blitzschreiben_tracker.csv")

  if(file.exists(tracker_file)) {
    return(tracker_file)
  }
  return(NULL)
}

#' Advance project to next stage
#'
#' Moves a project through the Blitzschreiben pipeline
#'
#' @param project_code Project code to advance
#' @param decision Decision from current stage
#' @param next_steps Note about next steps
#' @export
advance_to_next_stage <- function(project_code, decision = NULL, next_steps = NULL) {

  tracker_file <- find_blitz_tracker()
  if(is.null(tracker_file)) {
    stop("No Blitzschreiben tracker found")
  }

  tracker <- read.csv(tracker_file, stringsAsFactors = FALSE)
  project_idx <- which(tracker$project_code == project_code)

  if(length(project_idx) == 0) {
    stop(paste("Project", project_code, "not found in tracker"))
  }

  current_stage <- tracker[project_idx, "stage"]

  cat(paste(" ADVANCING PROJECT:", project_code, "\n"))
  cat(paste("Current stage:", current_stage, "\n"))

  if(current_stage == 1) {
    if(!is.null(decision) && decision %in% c("ready", "share")) {
      tracker[project_idx, "stage"] <- 2
      tracker[project_idx, "next_step"] <- "Create analyseblitz template"
      cat("Advanced to Stage II: Analyseblitz\n")
    } else if(!is.null(decision) && decision == "discard") {
      tracker[project_idx, "status"] <- "discarded"
      cat("Project discarded from pipeline\n")
    } else {
      cat("Specify decision: 'ready', 'discard', or 'develop'\n")
      return(invisible(NULL))
    }

  } else if(current_stage == 2) {
    tracker[project_idx, "stage"] <- 3
    tracker[project_idx, "next_step"] <- "Create draft template"
    cat("Advanced to Stage III: Draftschreiben\n")

  } else if(current_stage == 3) {
    tracker[project_idx, "status"] <- "completed"
    cat(" Project completed! Ready for sharing/submission\n")
  }

  tracker[project_idx, "last_activity"] <- as.character(Sys.Date())

  if(!is.null(next_steps)) {
    tracker[project_idx, "next_step"] <- next_steps
  }

  # Save updated tracker
  write.csv(tracker, tracker_file, row.names = FALSE)

  # Log the advancement
  log_blitz_activity("Stage Advancement", project_code,
                     notes = paste("Advanced to stage", tracker[project_idx, "stage"]))
}

#' Weekly Blitzschreiben pipeline report
#'
#' Comprehensive weekly summary of all pipeline activity
#'
#' @export
weekly_blitz_report <- function() {

  cat(" WEEKLY BLITZSCHREIBEN REPORT \n")
  cat(paste("Week of:", format(Sys.Date() - 6, "%Y-%m-%d"), "to", format(Sys.Date(), "%Y-%m-%d"), "\n"))
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # Get current status
  pipeline_status <- blitzschreiben_status()

  # TODO: Add weekly metrics:
  # - Projects advanced between stages
  # - Time spent in each stage
  # - Completion rates
  # - Pipeline efficiency metrics

  cat("\n NEXT WEEK PRIORITIES:\n")
  cat("Focus on pipeline balance\n")
  cat("Complete overdue projects\n")
  cat("Add new ideas if Stage I is low\n")
}

### MASTER SCRIPT SYSTEM ###

#' Create master analysis script for Stage II
#'
#' Generates orchestrating script that calls component analysis scripts
#'
#' @param project_code Project code
#' @param analysis_components Vector of analysis component names
#' @param file_sets Named list of file sets to process
#' @param project_path Path to project (optional)
#' @return Path to master script
#' @export
create_master_script <- function(project_code, analysis_components = NULL, file_sets = NULL, project_path = NULL) {

  if(is.null(project_path)) {
    project_path <- getwd()
  }

  # Default components if none specified
  if(is.null(analysis_components)) {
    analysis_components <- c("data_prep", "comprehensive_cleaning", "descriptive_stats", "main_analysis", "visualization")
  }

  # Auto-detect file sets if none provided
  if(is.null(file_sets)) {
    file_sets <- detect_project_file_sets(project_path)
  }

  master_content <- paste0(
    "# MASTER ANALYSIS SCRIPT: ", project_code, "\n",
    "# Created: ", Sys.Date(), "\n",
    "# Purpose: Orchestrate all analysis components for Stage II analyseblitz\n\n",
    "# =============================================================================\n",
    "# SETUP AND CONFIGURATION\n",
    "# =============================================================================\n\n",
    "# Clear environment\n",
    "rm(list = ls())\n\n",
    "# Load PRoMan for project management\n",
    "library(PRoMan)\n\n",
    "# Set project paths\n",
    "paths <- get_project_paths()\n",
    "print(paths)  # Verify paths are correct\n\n",
    "# Load required packages\n",
    "library(tidyverse)\n",
    "# Add other packages as needed\n\n",
    "# Project configuration\n",
    "PROJECT_CODE <- \"", project_code, "\"\n",
    "ANALYSIS_DATE <- Sys.Date()\n",
    "OUTPUT_PREFIX <- paste0(PROJECT_CODE, \"_\", format(ANALYSIS_DATE, \"%Y%m%d\"))\n\n"
  )

  # Add file sets configuration
  if(length(file_sets) > 0) {
    master_content <- paste0(master_content,
                             "# =============================================================================\n",
                             "# FILE SETS CONFIGURATION\n",
                             "# =============================================================================\n\n"
    )

    for(set_name in names(file_sets)) {
      master_content <- paste0(master_content,
                               "# ", set_name, " files\n",
                               toupper(gsub("[^A-Z0-9]", "_", set_name)), "_FILES <- c(\n"
      )

      for(i in seq_along(file_sets[[set_name]])) {
        file <- file_sets[[set_name]][i]
        comma <- ifelse(i < length(file_sets[[set_name]]), ",", "")
        master_content <- paste0(master_content, "  \"", file, "\"", comma, "\n")
      }

      master_content <- paste0(master_content, ")\n\n")
    }
  }

  # Add component script execution
  master_content <- paste0(master_content,
                           "# =============================================================================\n",
                           "# COMPONENT SCRIPT EXECUTION\n",
                           "# =============================================================================\n\n",
                           "cat(\" Starting Master Analysis for\", PROJECT_CODE, \"\\n\")\n",
                           "cat(\"Timestamp:\", as.character(Sys.time()), \"\\n\\n\")\n\n"
  )

  # Add each component
  for(i in seq_along(analysis_components)) {
    component <- analysis_components[i]
    master_content <- paste0(master_content,
                             "# ", paste(rep("-", 50), collapse = ""), "\n",
                             "# COMPONENT ", i, ": ", toupper(gsub("_", " ", component)), "\n",
                             "# ", paste(rep("-", 50), collapse = ""), "\n\n",
                             "cat(\" Running component ", i, ":\", \"", component, "\"...\\n\")\n",
                             "component_start <- Sys.time()\n\n",
                             "# Source the component script\n",
                             "source(file.path(paths$r_programs, \"", component, "_", project_code, ".R\"))\n\n",
                             "component_time <- round(as.numeric(difftime(Sys.time(), component_start, units = \"mins\")), 2)\n",
                             "cat(\"Component ", i, " completed in\", component_time, \"minutes\\n\\n\")\n\n"
    )
  }

  # Add completion summary
  master_content <- paste0(master_content,
                           "# =============================================================================\n",
                           "# ANALYSIS COMPLETION SUMMARY\n",
                           "# =============================================================================\n\n",
                           "total_time <- round(as.numeric(difftime(Sys.time(), ANALYSIS_DATE, units = \"mins\")), 2)\n",
                           "cat(\" MASTER ANALYSIS COMPLETED \\n\")\n",
                           "cat(\"Project:\", PROJECT_CODE, \"\\n\")\n",
                           "cat(\"Total execution time:\", total_time, \"minutes\\n\")\n",
                           "cat(\"Components completed:\", length(c(", paste0("\"", analysis_components, "\"", collapse = ", "), ")), \"\\n\")\n",
                           "cat(\"Timestamp:\", as.character(Sys.time()), \"\\n\\n\")\n\n",
                           "# Log completion in PRoMan\n",
                           "if(exists(\"log_blitz_activity\")) {\n",
                           "  log_blitz_activity(\"Master Analysis Completed\", PROJECT_CODE, total_time)\n",
                           "}\n\n",
                           "cat(\" Check output files in:\", paths$data_cleaned, \"\\n\")\n",
                           "cat(\" Check figures in:\", paths$personal_notes, \"\\n\")\n"
  )

  # Save master script
  master_filename <- paste0("MASTER_", project_code, ".R")
  master_path <- file.path(project_path, "r_programs", master_filename)

  writeLines(master_content, master_path)

  # Create component script templates
  component_paths <- create_component_templates(project_code, analysis_components, file_sets, project_path)

  cat(" MASTER SCRIPT CREATED! \n")
  cat(paste("Master script:", master_filename, "\n"))
  cat("Component scripts created:\n")
  for(comp_path in component_paths) {
    cat(paste("-", basename(comp_path), "\n"))
  }

  return(master_path)
}

#' Create component analysis script templates
#'
#' Generates individual component scripts called by master script
#'
#' @param project_code Project code
#' @param analysis_components Vector of component names
#' @param file_sets File sets detected for the project
#' @param project_path Path to project
#' @return Vector of component script paths
create_component_templates <- function(project_code, analysis_components, file_sets, project_path) {

  component_paths <- character(length(analysis_components))

  for(i in seq_along(analysis_components)) {
    component <- analysis_components[i]

    # Generate component-specific template
    component_content <- generate_component_template(component, project_code, file_sets)

    # Save component script
    component_filename <- paste0(component, "_", project_code, ".R")
    component_path <- file.path(project_path, "r_programs", component_filename)

    writeLines(component_content, component_path)
    component_paths[i] <- component_path
  }

  return(component_paths)
}

#' Extract keyword from file set name for loading
#'
#' Helper function to determine what keyword to use for file loading
#'
#' @param set_name Name of the file set
#' @return Keyword string for file loading
extract_keyword_from_set_name <- function(set_name) {
  # Simple extraction - take first meaningful word
  words <- strsplit(tolower(set_name), "[^a-z0-9]+")[[1]]
  meaningful_words <- words[nchar(words) > 2]  # Skip very short words

  if(length(meaningful_words) > 0) {
    return(meaningful_words[1])
  } else {
    return("data")  # fallback
  }
}

#' Detect file sets in current project
#'
#' Automatically identifies file sets for workflow configuration
#'
#' @param project_path Path to project
#' @return Named list of file sets
detect_project_file_sets <- function(project_path) {

  source_path <- file.path(project_path, "data", "source")

  if(!dir.exists(source_path)) {
    return(list())
  }

  files <- list.files(source_path)

  if(length(files) == 0) {
    return(list())
  }

  # Use existing file set detection from data management functions
  return(detect_file_sets(files))
}

#' Run master script with monitoring
#'
#' Executes master script with progress tracking and error handling
#'
#' @param project_code Project code
#' @param project_path Path to project (optional)
#' @export
run_master_analysis <- function(project_code, project_path = NULL) {

  if(is.null(project_path)) {
    project_path <- getwd()
  }

  master_path <- file.path(project_path, "r_programs", paste0("MASTER_", project_code, ".R"))

  if(!file.exists(master_path)) {
    stop(paste("Master script not found:", master_path))
  }

  cat(" EXECUTING MASTER ANALYSIS \n")
  cat(paste("Project:", project_code, "\n"))
  cat(paste("Script:", basename(master_path), "\n"))
  cat(paste("Started:", Sys.time(), "\n"))
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # Execute master script
  tryCatch({
    source(master_path, echo = FALSE)
    cat("\n Master analysis completed successfully!\n")
  }, error = function(e) {
    cat("\nError in master analysis:\n")
    cat(paste("Error:", e$message, "\n"))
    cat("Check individual component scripts for issues.\n")
  })
}

#' Log Blitzschreiben activity (simple console version)
#'
#' Simple logging for major Blitzschreiben workflow milestones
#'
#' @param activity_type Type of activity performed
#' @param project_code Project code (optional)
#' @param time_spent Time spent in minutes (optional)
#' @param notes Additional notes (optional)
log_blitz_activity <- function(activity_type, project_code = NULL, time_spent = NULL, notes = NULL) {

  # Simple console output for pipeline awareness
  timestamp <- format(Sys.time(), "%H:%M")

  message <- paste0(" [", timestamp, "] ", activity_type)
  if(!is.null(project_code)) {
    message <- paste0(message, " (", project_code, ")")
  }
  if(!is.null(notes)) {
    message <- paste0(message, " - ", notes)
  }

  cat(message, "\n")
  return(invisible(NULL))
}

#' Remove or update project in Blitzschreiben tracker
#'
#' Removes a project from tracking or changes its status
#'
#' @param project_code Project code to remove/update
#' @param action "remove" to delete, "archive" to mark as archived, "pause" to pause
#' @export
update_tracker_entry <- function(project_code, action = "remove") {

  tracker_file <- find_blitz_tracker()
  if(is.null(tracker_file)) {
    stop("No Blitzschreiben tracker found")
  }

  tracker <- read.csv(tracker_file, stringsAsFactors = FALSE)
  project_idx <- which(tracker$project_code == project_code)

  if(length(project_idx) == 0) {
    stop(paste("Project", project_code, "not found in tracker"))
  }

  if(action == "remove") {
    # Remove the project entirely
    tracker <- tracker[-project_idx, ]
    cat(paste("Removed", project_code, "from tracker\n"))

  } else if(action == "archive") {
    # Mark as archived (keeps record but removes from active pipeline)
    tracker[project_idx, "status"] <- "archived"
    tracker[project_idx, "last_activity"] <- as.character(Sys.Date())
    cat(paste(" Archived project", project_code, "\n"))

  } else if(action == "pause") {
    # Mark as paused
    tracker[project_idx, "status"] <- "paused"
    tracker[project_idx, "last_activity"] <- as.character(Sys.Date())
    cat(paste(" Paused project", project_code, "\n"))

  } else {
    stop("Action must be 'remove', 'archive', or 'pause'")
  }

  # Save updated tracker
  write.csv(tracker, tracker_file, row.names = FALSE)

  return(invisible(TRUE))
}

#' List all projects in tracker with their status
#'
#' Shows all projects including inactive ones
#'
#' @param include_inactive Show archived/paused/discarded projects
#' @export
list_all_projects <- function(include_inactive = TRUE) {

  tracker_file <- find_blitz_tracker()
  if(is.null(tracker_file)) {
    cat("No Blitzschreiben tracker found\n")
    return(invisible(NULL))
  }

  tracker <- read.csv(tracker_file, stringsAsFactors = FALSE)

  if(!include_inactive) {
    tracker <- tracker[tracker$status == "active", ]
  }

  cat("ALL TRACKED PROJECTS\n")
  cat(paste(rep("=", 40), collapse = ""), "\n\n")

  for(i in 1:nrow(tracker)) {
    proj <- tracker[i, ]
    status_emoji <- switch(proj$status,
                           "active" = "",
                           "paused" = "",
                           "archived" = "",
                           "discarded" = "?",
                           "completed" = "?",
                           "?")

    cat(paste(status_emoji, proj$project_code, "-", proj$title, "\n"))
    cat(paste("   Stage:", proj$stage, "| Status:", proj$status,
              "| Last activity:", proj$last_activity, "\n\n"))
  }

  return(invisible(tracker))
}

#' Read Stage II analyseblitz information
#'
#' Extracts key results and methods from Stage II template
#'
#' @param project_code Project code
#' @return List with methods and results info
read_stage2_info <- function(project_code) {

  paths <- get_project_paths()
  stage2_path <- file.path(paths$personal_notes, paste0("analyseblitz_", project_code, ".qmd"))

  if(!file.exists(stage2_path)) {
    return(NULL)
  }

  content <- readLines(stage2_path)

  # Extract key information
  info <- list()

  # Look for analysis type
  analysis_line <- grep("\\*\\*Analysis Type:\\*\\*", content)
  if(length(analysis_line) > 0) {
    info$analysis_type <- gsub("\\*\\*Analysis Type:\\*\\* ", "", content[analysis_line[1]])
  }

  # Look for main results (between ```{r real-analysis} and next ```)
  results_start <- grep("```\\{r real-analysis\\}", content)
  if(length(results_start) > 0) {
    results_end <- grep("```", content[(results_start[1]+1):length(content)])[1] + results_start[1]
    if(!is.na(results_end)) {
      info$results_code <- content[(results_start[1]+1):(results_end-1)]
    }
  }

  # Look for sniff test results
  sniff_start <- grep("## SNIFF TEST", content)
  if(length(sniff_start) > 0) {
    # Get a few lines after sniff test
    sniff_content <- content[(sniff_start[1]+1):min(length(content), sniff_start[1]+10)]
    info$sniff_test <- sniff_content[nchar(trimws(sniff_content)) > 0][1:3]
  }

  return(info)
}

#' Generate component template based on component type
#'
#' Creates appropriate template for each component type
#'
#' @param component Component name
#' @param project_code Project code
#' @param file_sets Available file sets
#' @return Character vector with component template
generate_component_template <- function(component, project_code, file_sets) {

  # Basic header for all components
  header <- paste0(
    "# ", toupper(gsub("_", " ", component)), " COMPONENT\n",
    "# Project: ", project_code, "\n",
    "# Generated: ", Sys.Date(), "\n",
    "# This script is called by MASTER_", project_code, ".R\n\n",
    "# Component-specific code below:\n",
    "# =============================================================================\n\n"
  )

  # Component-specific templates
  if(component == "data_prep") {
    content <- paste0(header,
                      "# Load raw data files\n",
                      "# Available file sets:\n",
                      if(length(file_sets) > 0) {
                        paste0("# - ", names(file_sets), ": ", sapply(file_sets, length), " files\n", collapse = "")
                      } else {
                        "# No file sets detected - add file loading code here\n"
                      },
                      "\n",
                      "# Example: Load files by keyword\n",
                      "# raw_files <- select_files_by_keyword('keyword', folder_path = paths$data_source)\n",
                      "# data_list <- load_files_by_keyword('keyword')\n\n",
                      "# Combine or process as needed\n",
                      "# combined_data <- bind_rows(data_list)\n\n",
                      "# Save prepared data\n",
                      "# write.csv(combined_data, file.path(paths$data_cleaned, paste0(OUTPUT_PREFIX, '_prepared.csv')), row.names = FALSE)\n"
    )

  } else if(component == "comprehensive_cleaning") {
    content <- paste0(header,
                      "# Load prepared data\n",
                      "# data <- read.csv(file.path(paths$data_cleaned, paste0(OUTPUT_PREFIX, '_prepared.csv')))\n\n",
                      "# Run comprehensive cleaning\n",
                      "# data_snapshot(data, save_summary = TRUE)\n",
                      "# suspicious <- flag_suspicious_values(data)\n",
                      "# missing_patterns <- missing_data_patterns(data)\n\n",
                      "# Standardize variables\n",
                      "# data <- standardize_var_names(data)\n",
                      "# data <- clean_demographics(data, age_var = 'age', gender_var = 'gender')\n\n",
                      "# Generate data dictionary\n",
                      "# dict <- generate_data_dictionary(data, filename = paste0(OUTPUT_PREFIX, '_data_dictionary.csv'))\n\n",
                      "# Save cleaned data\n",
                      "# write.csv(data, file.path(paths$data_cleaned, paste0(OUTPUT_PREFIX, '_clean.csv')), row.names = FALSE)\n"
    )

  } else if(component == "descriptive_stats") {
    content <- paste0(header,
                      "# Load clean data\n",
                      "# data <- read.csv(file.path(paths$data_cleaned, paste0(OUTPUT_PREFIX, '_clean.csv')))\n\n",
                      "# Generate descriptive statistics\n",
                      "# summary_stats <- data %>%\n",
                      "#   summarise(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))\n\n",
                      "# Create descriptive tables and figures\n",
                      "# desc_table <- create_table_figure_pair(data, \n",
                      "#                                       table_vars = c('outcome', 'predictor'),\n",
                      "#                                       plot_type = 'histogram',\n",
                      "#                                       save_pair = TRUE,\n",
                      "#                                       pair_name = paste0(OUTPUT_PREFIX, '_descriptives'))\n\n",
                      "# Save results\n",
                      "# write.csv(summary_stats, file.path(paths$personal_notes, paste0(OUTPUT_PREFIX, '_descriptive_stats.csv')), row.names = FALSE)\n"
    )

  } else if(component == "main_analysis") {
    content <- paste0(header,
                      "# Load clean data\n",
                      "# data <- read.csv(file.path(paths$data_cleaned, paste0(OUTPUT_PREFIX, '_clean.csv')))\n\n",
                      "# Main analysis\n",
                      "# Copy from Stage II analyseblitz mock analysis\n",
                      "# model <- lm(outcome ~ predictor + controls, data = data)\n",
                      "# summary(model)\n\n",
                      "# Save model results\n",
                      "# saveRDS(model, file.path(paths$data_cleaned, paste0(OUTPUT_PREFIX, '_main_model.rds')))\n\n",
                      "# Extract key results for reporting\n",
                      "# results_summary <- broom::tidy(model)\n",
                      "# write.csv(results_summary, file.path(paths$personal_notes, paste0(OUTPUT_PREFIX, '_results.csv')), row.names = FALSE)\n"
    )

  } else if(component == "visualization") {
    content <- paste0(header,
                      "# Load model results\n",
                      "# model <- readRDS(file.path(paths$data_cleaned, paste0(OUTPUT_PREFIX, '_main_model.rds')))\n",
                      "# data <- read.csv(file.path(paths$data_cleaned, paste0(OUTPUT_PREFIX, '_clean.csv')))\n\n",
                      "# Create main results figure\n",
                      "# p1 <- ggplot(data, aes(x = predictor, y = outcome)) +\n",
                      "#   geom_point() +\n",
                      "#   geom_smooth(method = 'lm') +\n",
                      "#   theme_academic() +\n",
                      "#   labs(title = 'Main Results')\n\n",
                      "# Save figures with metadata\n",
                      "# save_figure(p1, \n",
                      "#            figure_name = paste0(OUTPUT_PREFIX, '_main_results'),\n",
                      "#            caption = 'Main results showing relationship between X and Y',\n",
                      "#            width = 8, height = 6)\n\n",
                      "# Create figure package for manuscript\n",
                      "# create_figure_summary()\n"
    )

  } else {
    # Generic template for custom components
    content <- paste0(header,
                      "# Add ", component, " code here\n\n",
                      "# Load necessary data\n",
                      "# ...\n\n",
                      "# Process/analyze\n",
                      "# ...\n\n",
                      "# Save outputs\n",
                      "# ...\n"
    )
  }

  return(content)
}


#' Test data simulation from Stage I
#'
#' Generates and displays sample simulated data based on Stage I specs
#'
#' @param project_code Project code
#' @param n Sample size (overrides Stage I if provided)
#' @export
test_stage1_simulation <- function(project_code, n = NULL) {
  stage_i_info <- .parse_stage_i_variables(project_code)

  if (is.null(stage_i_info)) {
    stop("No Stage I information found for project ", project_code)
  }

  # Use Stage I sample size if not overridden
  if (is.null(n)) {
    n <- ifelse(!is.null(stage_i_info$sample_size),
                stage_i_info$sample_size,
                100)
  }

  cat("🎲 GENERATING TEST DATA FROM STAGE I SPECS\n")
  cat(paste("Project:", project_code, "\n"))
  cat(paste("Sample size:", n, "\n\n"))

  # Generate the data
  sim_data <- data.frame(id = 1:n)

  if (length(stage_i_info$variables) > 0) {
    for (var_name in names(stage_i_info$variables)) {
      var_info <- stage_i_info$variables[[var_name]]

      if (var_info$type == "factor") {
        levels <- trimws(strsplit(var_info$values, ",")[[1]])
        levels <- levels[levels != ""]

        if (length(levels) > 0) {
          sim_data[[var_name]] <- sample(levels, n, replace = TRUE)
        } else {
          sim_data[[var_name]] <- sample(c("A", "B"), n, replace = TRUE)
        }

      } else if (var_info$type == "numeric") {
        if (grepl("-", var_info$values)) {
          range_parts <- strsplit(var_info$values, "-")[[1]]
          range_vals <- as.numeric(trimws(range_parts))
          if (length(range_vals) == 2 && !any(is.na(range_vals))) {
            sim_data[[var_name]] <- runif(n, min = range_vals[1], max = range_vals[2])
          } else {
            sim_data[[var_name]] <- rnorm(n)
          }
        } else if (tolower(var_info$values) == "positive") {
          sim_data[[var_name]] <- rgamma(n, shape = 2, rate = 1)
        } else {
          sim_data[[var_name]] <- rnorm(n)
        }
      }
    }
  }

  cat("Generated data structure:\n")
  str(sim_data)

  cat("\nFirst few rows:\n")
  print(head(sim_data))

  return(invisible(sim_data))
}



# Helper function to convert strings to snake_case... may remove later?
to_snake_case <- function(x) {
  gsub("^_+|_+$", "", gsub("[ \\-/(),]+", "_", tolower(trimws(x))))
}
