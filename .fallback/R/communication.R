# R/communication.R
#
# Communication and collaboration tools for PRoMan
#
# Includes:
# - Collaborator onboarding packages
# - Project communication templates
# - Knowledge transfer documentation
# - Meeting and update templates

### ------------------------------------------------------------ ###

#' Create collaborator onboarding package
#'
#' Generates information package to bring new collaborators up to speed
#'
#' @param project_code Project code
#' @param collaborator_name Name of collaborator being onboarded
#' @param package_type Type of package: "executive", "tutorial", "operational", "mixed"
#' @param focus_areas Areas of focus: "analysis", "writing", "methods", "domain", "all"
#' @return Path to created onboarding package
#' @export
create_collaborator_onboarding <- function(project_code = NULL, collaborator_name = "New Collaborator",
                                           package_type = "mixed", focus_areas = "all") {

  paths <- get_project_paths()
  if(is.null(project_code)) {
    project_code <- basename(paths$root)
  }

  # Create onboarding directory
  timestamp <- format(Sys.Date(), "%Y%m%d")
  onboarding_dir <- file.path(paths$root, paste0("collaborator_onboarding_", timestamp))
  dir.create(onboarding_dir, showWarnings = FALSE)

  # Generate package contents based on type
  if(package_type %in% c("executive", "mixed")) {
    create_executive_summary_doc(project_code, onboarding_dir)
  }

  if(package_type %in% c("tutorial", "mixed")) {
    create_tutorial_walkthrough_doc(project_code, onboarding_dir)
  }

  if(package_type %in% c("operational", "mixed")) {
    create_operational_details_doc(project_code, onboarding_dir)
  }

  # Create navigation document
  create_onboarding_navigation(project_code, collaborator_name, package_type, onboarding_dir)

  message(paste("Collaborator onboarding package created:", basename(onboarding_dir)))
  message("Review and customize the generated documents before sharing.")

  return(invisible(onboarding_dir))
}

#' Create executive summary document
#'
#' High-level strategic overview for the project
#'
#' @param project_code Project code
#' @param output_dir Directory to save document
create_executive_summary_doc <- function(project_code, output_dir) {

  paths <- get_project_paths()

  # Try to extract information from existing Blitzschreiben components
  stage1_info <- extract_stage1_info(project_code, paths)
  stage2_info <- extract_stage2_info(project_code, paths)
  stage3_info <- extract_stage3_info(project_code, paths)

  content <- c(
    paste("# Executive Summary:", project_code),
    paste("**Generated:** ", Sys.Date()),
    paste("**For:** Strategic overview and high-level context"),
    "",
    "---",
    "",
    "## Research Significance",
    "",
    if(!is.null(stage1_info$research_question)) {
      paste("**Research Question:** ", stage1_info$research_question)
    } else {
      "**Research Question:** [Describe the main research question and its importance]"
    },
    "",
    if(!is.null(stage1_info$background_warrant)) {
      paste("**Why This Matters:** ", stage1_info$background_warrant)
    } else {
      "**Why This Matters:** [Explain the significance and contribution to the field]"
    },
    "",
    "## Current Status",
    "",
    paste("**Project Stage:** ", determine_current_stage(project_code, paths)),
    "",
    "**Key Progress:**",
    "- [List major milestones completed]",
    "- [Highlight key findings or breakthroughs]",
    "- [Note any significant decisions made]",
    "",
    "## Key Findings (So Far)",
    "",
    if(!is.null(stage2_info$main_findings)) {
      stage2_info$main_findings
    } else {
      c("**Main Results:**",
        "- [Primary finding 1 with brief interpretation]",
        "- [Primary finding 2 with brief interpretation]",
        "- [Any unexpected or notable findings]")
    },
    "",
    "## Strategic Considerations",
    "",
    "**Publication Potential:**",
    "- Target venue: [Journal or conference]",
    "- Estimated timeline: [Timeline for submission]",
    "- Contribution type: [Methodological, empirical, theoretical]",
    "",
    "**Resource Needs:**",
    "- [What resources, expertise, or support is needed]",
    "- [Any potential obstacles or challenges]",
    "",
    "## Next Steps",
    "",
    if(!is.null(stage3_info$next_steps)) {
      stage3_info$next_steps
    } else {
      c("**Immediate Priorities:**",
        "- [Next 2-4 weeks]",
        "",
        "**Medium-term Goals:**",
        "- [Next 1-3 months]",
        "",
        "**Long-term Vision:**",
        "- [Ultimate project goals]")
    },
    "",
    "---",
    "",
    "*This document provides strategic context. See tutorial walkthrough for detailed methods explanation or operational details for hands-on implementation information.*"
  )

  # Save document
  doc_path <- file.path(output_dir, "01_executive_summary.md")
  writeLines(content, doc_path)

  return(doc_path)
}

#' Create tutorial walkthrough document
#'
#' Educational explanation of research process and methods
#'
#' @param project_code Project code
#' @param output_dir Directory to save document
create_tutorial_walkthrough_doc <- function(project_code, output_dir) {

  paths <- get_project_paths()
  stage1_info <- extract_stage1_info(project_code, paths)
  stage2_info <- extract_stage2_info(project_code, paths)

  content <- c(
    paste("# Tutorial Walkthrough:", project_code),
    paste("**Generated:** ", Sys.Date()),
    paste("**For:** Understanding methods, rationale, and learning context"),
    "",
    "---",
    "",
    "## Research Question Development",
    "",
    "**How We Got Here:**",
    if(!is.null(stage1_info$background_warrant)) {
      paste("Starting point: ", stage1_info$background_warrant)
    } else {
      "- [Describe the problem or gap that led to this research]"
    },
    "- [Explain how the specific research question was refined]",
    "- [Note any alternative questions considered and why this one was chosen]",
    "",
    "## Data and Sample",
    "",
    if(!is.null(stage1_info$sample_description)) {
      stage1_info$sample_description
    } else {
      c("**Data Source:** [Where the data comes from and why it's appropriate]",
        "**Sample Characteristics:** [Key demographics and sample size]",
        "**Variables of Interest:** [Main variables and how they're measured]")
    },
    "",
    "## Analytical Approach",
    "",
    "**Why We Chose This Method:**",
    if(!is.null(stage1_info$analysis_plan)) {
      paste("Planned approach: ", stage1_info$analysis_plan)
    } else {
      "- [Explain the analytical strategy and why it fits the research question]"
    },
    "- [Describe any alternative approaches considered]",
    "- [Note assumptions and limitations of the chosen approach]",
    "",
    "**Step-by-Step Process:**",
    if(!is.null(stage2_info$analysis_steps)) {
      stage2_info$analysis_steps
    } else {
      c("1. [Data preparation and cleaning steps]",
        "2. [Descriptive analysis and exploration]",
        "3. [Main statistical analysis]",
        "4. [Sensitivity analyses or robustness checks]",
        "5. [Results interpretation and validation]")
    },
    "",
    "## Understanding the Results",
    "",
    "**How to Interpret the Findings:**",
    "- [Explain what the statistical results mean in practical terms]",
    "- [Describe how to read key tables and figures]",
    "- [Note what constitutes meaningful vs. trivial differences]",
    "",
    "**Connecting to the Literature:**",
    "- [How findings relate to previous research]",
    "- [What's new or different about these results]",
    "- [Implications for theory or practice]",
    "",
    "## Learning Resources",
    "",
    "**Key References for Methods:**",
    "- [Essential papers for understanding the analytical approach]",
    "- [Tutorials or textbooks for deeper learning]",
    "",
    "**Domain Background:**",
    "- [Key papers for understanding the research area]",
    "- [Important concepts or terminology to know]",
    "",
    "---",
    "",
    "*This document focuses on understanding and learning. See executive summary for strategic context or operational details for implementation specifics.*"
  )

  doc_path <- file.path(output_dir, "02_tutorial_walkthrough.md")
  writeLines(content, doc_path)

  return(doc_path)
}

#' Create operational details document
#'
#' Technical implementation and hands-on information
#'
#' @param project_code Project code
#' @param output_dir Directory to save document
create_operational_details_doc <- function(project_code, output_dir) {

  paths <- get_project_paths()

  # Check for existing analysis files
  has_master_script <- file.exists(file.path(paths$r_programs, paste0("MASTER_", project_code, ".R")))
  has_analyseblitz <- length(list.files(file.path(paths$personal_notes, "analyseblitz"),
                                        pattern = paste0("analyseblitz_", project_code),
                                        full.names = TRUE)) > 0

  content <- c(
    paste("# Operational Details:", project_code),
    paste("**Generated:** ", Sys.Date()),
    paste("**For:** Hands-on implementation and technical work"),
    "",
    "---",
    "",
    "## File Organization",
    "",
    "**Project Structure:**",
    "```",
    paste("Project root:", paths$root),
    paste("????????? data/"),
    paste("???   ????????? source/     # Raw data files"),
    paste("???   ????????? cleaned/    # Processed data"),
    paste("????????? r_programs/     # Analysis scripts"),
    paste("????????? personal_notes/ # Documentation and figures"),
    paste("???   ????????? figures/    # Generated plots and tables"),
    paste("???   ????????? analyseblitz/ # Analysis notebooks"),
    paste("????????? sources/        # Reference materials"),
    "```",
    "",
    "**Key Files:**",
    if(has_master_script) {
      paste("- Master analysis script: `r_programs/MASTER_", project_code, ".R`")
    } else {
      "- [List main analysis files]"
    },
    if(has_analyseblitz) {
      paste("- Analysis notebook: `personal_notes/analyseblitz/analyseblitz_", project_code, ".qmd`")
    } else {
      "- [List key documentation files]"
    },
    "- [Other important files and their purposes]",
    "",
    "## Data Dictionary",
    "",
    # Try to include actual data dictionary if it exists
    generate_data_dictionary_section(project_code, paths),
    "",
    "## Analysis Code Structure",
    "",
    if(has_master_script) {
      c("**Master Script Workflow:**",
        paste("The main analysis is orchestrated by `MASTER_", project_code, ".R` which calls:"),
        "- [List component scripts and their functions]")
    } else {
      "**Analysis Workflow:**"
    },
    "- [Describe the sequence of analysis steps]",
    "- [Note key functions and their purposes]",
    "- [Explain any custom functions or utilities]",
    "",
    "## Reproducing the Analysis",
    "",
    "**Prerequisites:**",
    "- R version: [Specify version if important]",
    "- Required packages: [List key packages]",
    "- Data access: [How to access source data]",
    "",
    "**Step-by-Step Reproduction:**",
    if(has_master_script) {
      c("1. Set working directory to project root",
        paste("2. Run `source('r_programs/MASTER_", project_code, ".R')`"),
        "3. Check outputs in `data/cleaned/` and `personal_notes/figures/`")
    } else {
      c("1. [Step-by-step instructions for running the analysis]",
        "2. [Note any manual steps or interventions needed]",
        "3. [Describe expected outputs and where to find them]")
    },
    "",
    "## Figure and Table Generation",
    "",
    generate_figure_info_section(project_code, paths),
    "",
    "## Common Issues and Troubleshooting",
    "",
    "**Potential Problems:**",
    "- [List common errors and their solutions]",
    "- [Note any data-specific issues to watch for]",
    "- [Describe any manual quality checks needed]",
    "",
    "**Getting Help:**",
    "- [Contact information for technical questions]",
    "- [Resources for understanding specific methods]",
    "",
    "---",
    "",
    "*This document focuses on implementation. See executive summary for strategic context or tutorial walkthrough for conceptual understanding.*"
  )

  doc_path <- file.path(output_dir, "03_operational_details.md")
  writeLines(content, doc_path)

  return(doc_path)
}

#' Create navigation document for onboarding package
#'
#' Main entry point that explains what's in the package
#'
#' @param project_code Project code
#' @param collaborator_name Name of collaborator
#' @param package_type Type of package created
#' @param output_dir Directory to save document
create_onboarding_navigation <- function(project_code, collaborator_name, package_type, output_dir) {

  content <- c(
    paste("# Welcome to", project_code, "!"),
    paste("**Onboarding package for:** ", collaborator_name),
    paste("**Created:** ", Sys.Date()),
    "",
    "---",
    "",
    "## How to Use This Package",
    "",
    "This onboarding package contains different types of information to help you get up to speed quickly. **Choose what you need:**",
    "",
    if(package_type %in% c("executive", "mixed")) {
      c("### ???? Executive Summary (`01_executive_summary.md`)",
        "**Use this if you want:** Strategic overview, key findings, publication context",
        "**Good for:** Understanding significance, providing guidance, co-authorship decisions",
        "")
    },
    if(package_type %in% c("tutorial", "mixed")) {
      c("### ???? Tutorial Walkthrough (`02_tutorial_walkthrough.md`)",
        "**Use this if you want:** Methods explanation, learning context, conceptual understanding",
        "**Good for:** Understanding the approach, learning new methods, cross-domain collaboration",
        "")
    },
    if(package_type %in% c("operational", "mixed")) {
      c("### ?????? Operational Details (`03_operational_details.md`)",
        "**Use this if you want:** Technical implementation, code structure, hands-on work",
        "**Good for:** Reproducing analysis, making modifications, troubleshooting",
        "")
    },
    "## Quick Start Recommendations",
    "",
    "**If you're here to:**",
    "- **Provide strategic input** ??? Start with Executive Summary",
    "- **Learn the methods** ??? Start with Tutorial Walkthrough",
    "- **Do hands-on work** ??? Start with Operational Details",
    "- **Understand everything** ??? Read in order: Executive ??? Tutorial ??? Operational",
    "",
    "## Next Steps",
    "",
    "After reviewing the relevant documents:",
    "1. **Ask questions** - Note anything unclear or missing",
    "2. **Identify your role** - What will you contribute to this project?",
    "3. **Set up communication** - How will we coordinate going forward?",
    "",
    "## Important Notes",
    "",
    "?????? **These documents are starting points** - they pull information from the project automatically but may need updates or corrections.",
    "",
    "?????? **Please customize** - Add your questions, notes, or corrections directly to these documents.",
    "",
    "???? **This is iterative** - We can regenerate and refine these materials as the project evolves.",
    "",
    "---",
    "",
    paste("**Project contact:** [Your contact information]"),
    paste("**Best way to reach me:** [Preferred communication method]"),
    paste("**Project timeline:** [Key deadlines or milestones]")
  )

  doc_path <- file.path(output_dir, "00_START_HERE.md")
  writeLines(content, doc_path)

  return(doc_path)
}

### ------------------------------------------------------------ ###
### INFORMATION EXTRACTION HELPERS

#' Extract Stage I information from project
#'
#' Pulls information from Stage I triage card if it exists
#'
#' @param project_code Project code
#' @param paths Project paths
#' @return List of extracted information
extract_stage1_info <- function(project_code, paths) {

  # Look for Stage I card
  stage1_dir <- file.path(paths$root, "stage1_triage")
  if(!dir.exists(stage1_dir)) {
    return(NULL)
  }

  card_files <- list.files(stage1_dir, pattern = paste0("stage1_card_", project_code), full.names = TRUE)
  if(length(card_files) == 0) {
    return(NULL)
  }

  # Read and parse Stage I card (basic extraction)
  card_content <- readLines(card_files[1])

  # Extract key sections (simple pattern matching)
  research_question <- extract_section_content(card_content, "## RESEARCH QUESTION")
  background_warrant <- extract_section_content(card_content, "## BACKGROUND WARRANT")
  sample_description <- extract_section_content(card_content, "## SAMPLE DESCRIPTION")
  analysis_plan <- extract_section_content(card_content, "## ANALYSIS PLAN")

  return(list(
    research_question = research_question,
    background_warrant = background_warrant,
    sample_description = sample_description,
    analysis_plan = analysis_plan
  ))
}

#' Extract Stage II information from project
#'
#' Pulls information from analyseblitz files if they exist
#'
#' @param project_code Project code
#' @param paths Project paths
#' @return List of extracted information
extract_stage2_info <- function(project_code, paths) {

  # Look for analyseblitz files
  analyseblitz_dir <- file.path(paths$personal_notes, "analyseblitz")
  if(!dir.exists(analyseblitz_dir)) {
    return(NULL)
  }

  blitz_files <- list.files(analyseblitz_dir, pattern = paste0("analyseblitz_", project_code), full.names = TRUE)
  if(length(blitz_files) == 0) {
    return(NULL)
  }

  # Basic extraction - could be enhanced to parse Quarto documents more sophisticatedly
  return(list(
    main_findings = "**[Extract from Stage II analysis - main results section]**",
    analysis_steps = "**[Extract from Stage II analysis - methodology section]**"
  ))
}

#' Extract Stage III information from project
#'
#' Pulls information from draft documents if they exist
#'
#' @param project_code Project code
#' @param paths Project paths
#' @return List of extracted information
extract_stage3_info <- function(project_code, paths) {

  # Look for Stage III drafts
  stage3_dir <- file.path(paths$root, "stage3_draftschreiben")
  if(!dir.exists(stage3_dir)) {
    return(NULL)
  }

  draft_files <- list.files(stage3_dir, pattern = paste0("draft_", project_code), full.names = TRUE)
  if(length(draft_files) == 0) {
    return(NULL)
  }

  return(list(
    next_steps = "**[Extract from Stage III draft - next steps section]**"
  ))
}

#' Extract section content from markdown-style document
#'
#' Helper function to extract content under specific headings
#'
#' @param content Vector of document lines
#' @param section_header Header to look for
#' @return Extracted content or NULL
extract_section_content <- function(content, section_header) {

  header_line <- grep(paste0("^", section_header), content)
  if(length(header_line) == 0) {
    return(NULL)
  }

  # Find next header or end of document
  next_header <- grep("^##", content[(header_line + 1):length(content)])
  if(length(next_header) > 0) {
    end_line <- header_line + next_header[1] - 1
  } else {
    end_line <- length(content)
  }

  # Extract content between headers
  section_content <- content[(header_line + 1):end_line]
  section_content <- section_content[nchar(trimws(section_content)) > 0]  # Remove empty lines

  if(length(section_content) == 0) {
    return(NULL)
  }

  return(paste(section_content, collapse = "\n"))
}

#' Determine current project stage
#'
#' Figures out what stage the project is currently in
#'
#' @param project_code Project code
#' @param paths Project paths
#' @return Stage description
determine_current_stage <- function(project_code, paths) {

  # Check for Stage III files
  stage3_dir <- file.path(paths$root, "stage3_draftschreiben")
  if(dir.exists(stage3_dir) && length(list.files(stage3_dir, pattern = project_code)) > 0) {
    return("Stage III: Draftschreiben (Writing)")
  }

  # Check for Stage II files
  analyseblitz_dir <- file.path(paths$personal_notes, "analyseblitz")
  if(dir.exists(analyseblitz_dir) && length(list.files(analyseblitz_dir, pattern = project_code)) > 0) {
    return("Stage II: Analyseblitz (Analysis)")
  }

  # Check for Stage I files
  stage1_dir <- file.path(paths$root, "stage1_triage")
  if(dir.exists(stage1_dir) && length(list.files(stage1_dir, pattern = project_code)) > 0) {
    return("Stage I: Triage (Planning)")
  }

  return("Pre-Blitzschreiben (Traditional project)")
}

#' Generate data dictionary section for operational details
#'
#' Creates data dictionary content for the operational document
#'
#' @param project_code Project code
#' @param paths Project paths
#' @return Character vector with data dictionary content
generate_data_dictionary_section <- function(project_code, paths) {

  # Look for existing data dictionary
  dict_files <- list.files(paths$personal_notes, pattern = "data_dictionary.*\\.csv", full.names = TRUE)

  if(length(dict_files) > 0) {
    # Read and format existing dictionary
    dict <- read.csv(dict_files[1], stringsAsFactors = FALSE)

    content <- c("**Variables in Dataset:**")
    for(i in 1:min(10, nrow(dict))) {  # Show first 10 variables
      var_info <- dict[i, ]
      content <- c(content, paste("- `", var_info$variable, "`: ", var_info$description, sep = ""))
    }

    if(nrow(dict) > 10) {
      content <- c(content, paste("- [... and", nrow(dict) - 10, "more variables - see full data dictionary file]"))
    }

    return(content)
  } else {
    return(c("**Variables in Dataset:**",
             "- [Data dictionary not yet generated - run data cleaning workflow to create]",
             "- [List key variables and their meanings here]"))
  }
}

#' Generate figure information section for operational details
#'
#' Creates figure information content for the operational document
#'
#' @param project_code Project code
#' @param paths Project paths
#' @return Character vector with figure information
generate_figure_info_section <- function(project_code, paths) {

  # Check for figure registry
  registry_path <- file.path(paths$personal_notes, "figures", "figure_registry.csv")

  if(file.exists(registry_path)) {
    registry <- read.csv(registry_path, stringsAsFactors = FALSE)

    content <- c("**Generated Figures:**")
    for(i in 1:nrow(registry)) {
      fig_info <- registry[i, ]
      content <- c(content, paste("- `", fig_info$figure_name, "`: ", fig_info$caption, sep = ""))
    }

    content <- c(content, "", "All figures are saved in `personal_notes/figures/` with metadata in `figure_registry.csv`")

    return(content)
  } else {
    return(c("**Generated Figures:**",
             "- [No figures registered yet - use save_figure() to create and track figures]",
             "- [List key figures and their purposes here]"))
  }
}

### ------------------------------------------------------------ ###
### COMMUNICATION TEMPLATES

#' Create project update email template
#'
#' Generates template for regular project updates to collaborators
#'
#' @param project_code Project code
#' @param update_type Type of update: "progress", "milestone", "request", "results"
#' @return Path to email template file
#' @export
create_update_template <- function(project_code = NULL, update_type = "progress") {

  paths <- get_project_paths()
  if(is.null(project_code)) {
    project_code <- basename(paths$root)
  }

  # Create different templates based on update type
  if(update_type == "progress") {
    content <- c(
      paste("Subject: Progress Update -", project_code),
      "",
      paste("Hi [Collaborator names],"),
      "",
      paste("Quick update on", project_code, ":"),
      "",
      "**This week's progress:**",
      "- [Key accomplishment 1]",
      "- [Key accomplishment 2]",
      "- [Any challenges encountered]",
      "",
      "**Next steps:**",
      "- [What you're working on next]",
      "- [Any help or input needed]",
      "",
      "**Timeline:**",
      "- [Upcoming milestones or deadlines]",
      "",
      "Let me know if you have questions or want to discuss anything!",
      "",
      "Best,",
      "[Your name]"
    )
  } else if(update_type == "milestone") {
    content <- c(
      paste("Subject: Milestone Reached -", project_code),
      "",
      paste("Hi [Collaborator names],"),
      "",
      paste("Great news! We've reached a key milestone on", project_code, ":"),
      "",
      "**What we accomplished:**",
      "- [Major milestone description]",
      "- [Key findings or outcomes]",
      "",
      "**What this means:**",
      "- [Implications for the project]",
      "- [How this advances our goals]",
      "",
      "**Next phase:**",
      "- [What comes next]",
      "- [Timeline for next steps]",
      "",
      "Thanks for your contributions to getting us here!",
      "",
      "Best,",
      "[Your name]"
    )
  } else if(update_type == "request") {
    content <- c(
      paste("Subject: Input Needed -", project_code),
      "",
      paste("Hi [Collaborator names],"),
      "",
      paste("I'm reaching out about", project_code, "because I could use your expertise on:"),
      "",
      "**Specific request:**",
      "- [What you need help with]",
      "- [Why their input is valuable]",
      "",
      "**Context:**",
      "- [Background information they need]",
      "- [Current status of the project]",
      "",
      "**Timeline:**",
      "- [When you need their input]",
      "- [How much time it might take]",
      "",
      "Would you be available for [meeting/email discussion/review]? Happy to provide more details as needed.",
      "",
      "Thanks!",
      "[Your name]"
    )
  } else if(update_type == "results") {
    content <- c(
      paste("Subject: Key Results -", project_code),
      "",
      paste("Hi [Collaborator names],"),
      "",
      paste("Exciting update on", project_code, "- we have some key results to share:"),
      "",
      "**Main findings:**",
      "- [Key result 1 with brief interpretation]",
      "- [Key result 2 with brief interpretation]",
      "- [Any surprising or notable findings]",
      "",
      "**What this suggests:**",
      "- [Implications for your research question]",
      "- [How this fits with expectations]",
      "",
      "**Next steps:**",
      "- [Additional analyses planned]",
      "- [Writing/publication timeline]",
      "",
      "I'd love to get your thoughts on these results and discuss implications. Available for a call this week?",
      "",
      "Best,",
      "[Your name]"
    )
  }

  # Save template
  template_filename <- paste0("email_template_", update_type, "_", format(Sys.Date(), "%Y%m%d"), ".txt")
  template_path <- file.path(paths$personal_notes, template_filename)

  writeLines(content, template_path)

  message(paste("Email template created:", template_filename))
  message("Customize the template before sending.")

  return(invisible(template_path))
}

#' Create meeting agenda template
#'
#' Generates structured agenda for project meetings
#'
#' @param project_code Project code
#' @param meeting_type Type of meeting: "kickoff", "progress", "results", "planning"
#' @param attendees Vector of attendee names
#' @return Path to agenda template file
#' @export
create_meeting_agenda <- function(project_code = NULL, meeting_type = "progress", attendees = NULL) {

  paths <- get_project_paths()
  if(is.null(project_code)) {
    project_code <- basename(paths$root)
  }

  content <- c(
    paste("# Meeting Agenda:", project_code),
    paste("**Date:** [Meeting date]"),
    paste("**Time:** [Meeting time and duration]"),
    if(!is.null(attendees)) paste("**Attendees:** ", paste(attendees, collapse = ", ")),
    "",
    "---",
    ""
  )

  # Add meeting-specific content
  if(meeting_type == "kickoff") {
    content <- c(content,
                 "## 1. Project Overview (10 min)",
                 "- Research question and objectives",
                 "- Expected outcomes and timeline",
                 "",
                 "## 2. Roles and Responsibilities (10 min)",
                 "- Team member assignments",
                 "- Communication protocols",
                 "",
                 "## 3. Next Steps (10 min)",
                 "- Immediate action items",
                 "- Schedule next meeting"
    )
  } else if(meeting_type == "progress") {
    content <- c(content,
                 "## 1. Progress Update (15 min)",
                 "- Completed tasks since last meeting",
                 "- Current status overview",
                 "",
                 "## 2. Challenges and Solutions (10 min)",
                 "- Issues encountered",
                 "- Proposed solutions",
                 "",
                 "## 3. Next Steps (5 min)",
                 "- Action items and deadlines",
                 "- Next meeting date"
    )
  } else if(meeting_type == "results") {
    content <- c(content,
                 "## 1. Results Presentation (20 min)",
                 "- Key findings",
                 "- Statistical outcomes",
                 "- Visualizations",
                 "",
                 "## 2. Interpretation and Discussion (15 min)",
                 "- What results mean",
                 "- Limitations and caveats",
                 "",
                 "## 3. Publication Planning (10 min)",
                 "- Target journals",
                 "- Writing assignments",
                 "- Timeline"
    )
  } else if(meeting_type == "planning") {
    content <- c(content,
                 "## 1. Project Goals Review (10 min)",
                 "- Objectives and milestones",
                 "- Success criteria",
                 "",
                 "## 2. Resource Planning (10 min)",
                 "- Data needs",
                 "- Personnel allocation",
                 "- Budget considerations",
                 "",
                 "## 3. Timeline Development (10 min)",
                 "- Key milestones",
                 "- Dependencies",
                 "- Risk factors"
    )
  }

  content <- c(content,
               "",
               "---",
               "",
               "## Action Items",
               "- [ ] [Task 1] - [Assignee] - [Due date]",
               "- [ ] [Task 2] - [Assignee] - [Due date]",
               "",
               "## Notes",
               "[Meeting notes here]"
  )

  # Save agenda
  agenda_filename <- paste0("meeting_agenda_", meeting_type, "_", format(Sys.Date(), "%Y%m%d"), ".md")
  agenda_path <- file.path(paths$personal_notes, agenda_filename)

  writeLines(content, agenda_path)

  message(paste("Meeting agenda created:", agenda_filename))
  return(invisible(agenda_path))
}
