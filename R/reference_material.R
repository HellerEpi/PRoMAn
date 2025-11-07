#R/reference_material.R
## Reference guide and materials for PRoMan

#' Open PRoMan quick reference guide
#'
#' Opens an interactive HTML cheat sheet in your default browser
#'
#' @export
proman_help <- function() {

  # Create HTML content
  html_content <- create_reference_html()

  # Save to temp file
  temp_file <- tempfile(fileext = ".html")
  writeLines(html_content, temp_file)

  # Open in browser
  browseURL(temp_file)

  message("PRoMan reference opened in browser")
}

# Helper to create the HTML
create_reference_html <- function() {

  # Define all functions by category
  functions <- list(
    "Project Setup & Navigation" = list(
      list(name = "create_project()",
           desc = "Create new project with GUI or programmatically",
           usage = 'create_project("P3.1", "My Project")'),
      list(name = "set_project()",
           desc = "Set working directory and load file shortcuts",
           usage = "set_project()  # finds .proman file"),
      list(name = "get_project_paths()",
           desc = "Get all project folder paths",
           usage = "paths <- get_project_paths()"),
      list(name = "set_fallback()",
           desc = "Create snapshot of current project state",
           usage = "set_fallback()"),
      list(name = "fallback()",
           desc = "Restore project to previous snapshot",
           usage = "fallback()  # prompts for confirmation"),
      list(name = "fallback_info()",
           desc = "Show information about existing fallback",
           usage = "fallback_info()"),
      list(name = "file_path()",
           desc = "Get full path with minimal syntax",
           usage = 'file_path("myfile.csv", "cleaned")'),
      list(name = "read_file()",
           desc = "Read file from any project folder",
           usage = 'data <- read_file("mydata", "source")  # auto-detects extension'),
      list(name = "save_file()",
           desc = "Save file to any project folder",
           usage = 'save_file(data, "results.csv", "cleaned")'),
      list(name = "read_source()",
           desc = "Quick read from data/source",
           usage = 'data <- read_source("raw_data.csv")'),
      list(name = "read_cleaned()",
           desc = "Quick read from data/cleaned",
           usage = 'data <- read_cleaned("processed_data")'),
      list(name = "save_cleaned()",
           desc = "Quick save to data/cleaned",
           usage = 'save_cleaned(results, "output.csv")'),
      list(name = "list_files()",
           desc = "List files in project folder",
           usage = 'csv_files <- list_files("source", pattern = "*.csv")'),
      list(name = "file_exists()",
           desc = "Check if file exists in project",
           usage = 'if(file_exists("data.csv")) { ... }')
    ),

    "Data Management" = list(
      list(name = "select_files_by_keyword()",
           desc = "Find files matching keywords (supports exclusions)",
           usage = 'files <- select_files_by_keyword("baseline", exclude = "old")'),
      list(name = "load_files_by_keyword()",
           desc = "Load multiple files matching keywords",
           usage = 'data_list <- load_files_by_keyword("2024", exclude = "test")'),
      list(name = "create_file_set()",
           desc = "Create file sets with identifier",
           usage = 'create_file_set("recent:2h", "batch_1")'),
      list(name = "list_file_sets()",
           desc = "Show available file sets",
           usage = "list_file_sets()"),
      list(name = "add_to_sets()",
           desc = "Add identifier to existing file sets",
           usage = 'add_to_sets(c("baseline", "followup"), "clean_v2")'),
      list(name = "select_recent_files()",
           desc = "Get recently modified files",
           usage = "select_recent_files(hours = 2)"),
      list(name = "clean_strings()",
           desc = "Standardize strings for matching",
           usage = 'df <- clean_strings(df, "name_column")'),
      list(name = "save_with_timestamp()",
           desc = "Save with automatic timestamp",
           usage = 'save_with_timestamp(data, "analysis_results")'),
      list(name = "archive_files()",
           desc = "Move files to old/ folder",
           usage = 'archive_files(c("old_data.csv", "temp.csv"))')
    ),

    "Workflow System" = list(
      list(name = "workflow_template()",
           desc = "Generate workflow template",
           usage = "workflow_template()  # shows template"),
      list(name = "extract_workflows()",
           desc = "Extract workflows from script",
           usage = "extract_workflows()  # from active script"),
      list(name = "apply_workflow()",
           desc = "Apply workflow to file sets",
           usage = 'apply_workflow("clean_data", "baseline", parallel = TRUE)'),
      list(name = "list_workflows()",
           desc = "Show registered workflows",
           usage = "list_workflows()"),
      list(name = "test_workflow()",
           desc = "Test workflow on single file",
           usage = 'test_workflow("clean_data", "test_file.csv")'),
      list(name = "create_workflow_chain()",
           desc = "Chain workflows together",
           usage = 'create_workflow_chain("pipeline", c("clean", "analyze"))'),
      list(name = "apply_workflow_chain()",
           desc = "Run workflow chain",
           usage = 'apply_workflow_chain("pipeline", "dataset")')
    ),

    "Data Analysis Helpers" = list(
      list(name = "data_snapshot()",
           desc = "Quick overview of dataset",
           usage = "data_snapshot(df)"),
      list(name = "compare_datasets()",
           desc = "Compare two datasets for differences",
           usage = "compare_datasets(old_df, new_df)"),
      list(name = "missing_data_patterns()",
           desc = "Analyze missing data",
           usage = "missing_data_patterns(df, threshold = 5)"),
      list(name = "flag_suspicious_values()",
           desc = "Find potential missing data codes",
           usage = "flag_suspicious_values(df, c(-999, -99, 999))"),
      list(name = "detect_id_columns()",
           desc = "Identify likely ID variables",
           usage = "id_cols <- detect_id_columns(df)"),
      list(name = "standardize_var_names()",
           desc = "Convert variable names to snake_case",
           usage = 'df <- standardize_var_names(df, "snake_case")'),
      list(name = "generate_data_dictionary()",
           desc = "Create data documentation",
           usage = 'dict <- generate_data_dictionary(df, "data_dict.csv")')
    ),

    "Project Logging & Status" = list(
      list(name = "project_status()",
           desc = "Show comprehensive project status",
           usage = "project_status()"),
      list(name = "log_project_status()",
           desc = "Save project status to log file",
           usage = 'log_project_status("Completed data cleaning")'),
      list(name = "analyze_file_sets()",
           desc = "Analyze file organization patterns",
           usage = "analyze_file_sets()"),
      list(name = "find_stale_files()",
           desc = "Find files not recently modified",
           usage = "find_stale_files(days = 30)"),
      list(name = "find_large_files()",
           desc = "Find files above size threshold",
           usage = "find_large_files(min_size_mb = 10)"),
      list(name = "get_folder_summary()",
           desc = "Summary of folder contents",
           usage = 'get_folder_summary(sort_by = "date")')
    ),

    "Figure Management" = list(
      list(name = "save_figure()",
           desc = "Save figures with metadata tracking",
           usage = 'save_figure(p1, "analysis_plot", caption = "Main results")'),
      list(name = "list_figures()",
           desc = "Show all registered figures",
           usage = "list_figures()"),
      list(name = "get_figure()",
           desc = "Retrieve figure by name",
           usage = 'p <- get_figure("analysis_plot")'),
      list(name = "update_figure_caption()",
           desc = "Update figure metadata",
           usage = 'update_figure_caption("plot1", "New caption")'),
      list(name = "create_figure_summary()",
           desc = "Generate figure documentation",
           usage = "create_figure_summary()"),
      list(name = "deregister_figures()",
           desc = "Remove figures from registry",
           usage = 'deregister_figures(c("old_plot1", "temp_fig"))')
    ),

    "Blitzschreiben Integration" = list(
      list(name = "blitz_setup()",
           desc = "Set up Blitzschreiben structure",
           usage = 'blitz_setup("P3.1", "RCT Analysis")'),
      list(name = "create_master_script()",
           desc = "Generate master analysis script",
           usage = "create_master_script()"),
      list(name = "create_component_script()",
           desc = "Create analysis component",
           usage = 'create_component_script("data_prep", "Prepare analysis dataset")'),
      list(name = "parse_stage_i()",
           desc = "Extract info from Stage I document",
           usage = 'info <- parse_stage_i("stage_i.md")'),
      list(name = "check_draft_status()",
           desc = "Check manuscript draft progress",
           usage = "check_draft_status()"),
      list(name = "log_blitz_activity()",
           desc = "Log Blitzschreiben activity",
           usage = 'log_blitz_activity("Completed primary analysis")')
    ),

    "Collaboration & Communication" = list(
      list(name = "create_collaborator_onboarding()",
           desc = "Generate onboarding package for collaborator",
           usage = 'create_collaborator_onboarding("Dr. Smith", package_type = "mixed")'),
      list(name = "create_update_template()",
           desc = "Email templates for project updates",
           usage = 'create_update_template(update_type = "progress")'),
      list(name = "create_meeting_agenda()",
           desc = "Generate meeting agenda template",
           usage = 'create_meeting_agenda(meeting_type = "results")')
    )
  )


  # Build HTML with compact styling
  html <- c(
    '<!DOCTYPE html>',
    '<html><head>',
    '<title>PRoMan Quick Reference</title>',
    '<style>',
    'body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Arial, sans-serif; ',
    '  margin: 15px; background-color: #f8f9fa; max-width: 600px; }',
    'h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; ',
    '  font-size: 22px; margin-bottom: 10px; }',
    '.category { margin-bottom: 12px; background: white; border-radius: 6px; ',
    '  box-shadow: 0 1px 3px rgba(0,0,0,0.1); }',
    '.category-header { ',
    '  background: #34495e; color: white; padding: 10px 15px; ',
    '  cursor: pointer; user-select: none; border-radius: 6px 6px 0 0;',
    '  display: flex; justify-content: space-between; align-items: center;',
    '  font-weight: bold; font-size: 13px;',
    '}',
    '.category-header:hover { background: #2c3e50; }',
    '.category-header::after { content: "-"; font-size: 20px; }',
    '.category-header.collapsed::after { content: "+"; font-size: 20px; }',
    '.category-content { padding: 8px; display: block; }',
    '.category-content.collapsed { display: none; }',
    '.function { padding: 6px 8px; border-bottom: 1px solid #ecf0f1; }',
    '.function:last-child { border-bottom: none; }',
    '.fname { color: #e74c3c; font-family: "Consolas", "Monaco", monospace; ',
    '  font-weight: bold; font-size: 12px; }',
    '.desc { color: #555; font-size: 11px; margin-top: 2px; }',
    '.usage { color: #27ae60; font-family: "Consolas", "Monaco", monospace; ',
    '  font-size: 10px; margin-top: 2px; background: #f8f9fa; padding: 2px 4px; ',
    '  border-radius: 3px; display: inline-block; }',
    '.tip { background: #e8f4fd; padding: 10px; border-left: 3px solid #3498db; ',
    '  margin: 10px 0; font-size: 12px; }',
    '.controls { margin: 10px 0; text-align: center; }',
    '.btn { background: #3498db; color: white; border: none; padding: 6px 12px; ',
    '  margin: 0 4px; border-radius: 4px; cursor: pointer; font-size: 11px; }',
    '.btn:hover { background: #2980b9; }',
    '</style>',
    '</head><body>',
    '<h1>PRoMan Quick Reference</h1>'
  )

  # Add controls
  html <- c(html,
            '<div class="controls">',
            '<button class="btn" onclick="expandAll()">Expand All</button>',
            '<button class="btn" onclick="collapseAll()">Collapse All</button>',
            '</div>'
  )

  # Add quick tip
  html <- c(html,
            '<div class="tip">',
            '<strong>Tip:</strong> After running <code>set_project()</code>, use shortcuts like ',
            '<code>read_cleaned("myfile")</code> instead of long path expressions. ',
            'Most functions work from any project subdirectory.',
            '</div>'
  )

  # Add functions by category
  for(i in seq_along(names(functions))) {
    cat_name <- names(functions)[i]
    # Start collapsed except first two categories
    collapsed_class <- if(i > 2) " collapsed" else ""

    html <- c(html,
              paste0('<div class="category">'),
              paste0('<div class="category-header', collapsed_class, '" onclick="toggleCategory(this)">'),
              paste0('<span>', cat_name, ' (', length(functions[[cat_name]]), ')</span>'),
              '</div>',
              paste0('<div class="category-content', collapsed_class, '">')
    )

    for(func in functions[[cat_name]]) {
      html <- c(html,
                '<div class="function">',
                paste0('<div class="fname">', func$name, '</div>'),
                paste0('<div class="desc">', func$desc, '</div>'))

      if(!is.null(func$usage)) {
        html <- c(html, paste0('<div class="usage">', func$usage, '</div>'))
      }

      html <- c(html, '</div>')
    }

    html <- c(html, '</div></div>')
  }

  # Add JavaScript for interactivity
  html <- c(html,
            '<script>',
            'function toggleCategory(header) {',
            '  header.classList.toggle("collapsed");',
            '  header.nextElementSibling.classList.toggle("collapsed");',
            '}',
            'function expandAll() {',
            '  document.querySelectorAll(".category-header").forEach(h => {',
            '    h.classList.remove("collapsed");',
            '    h.nextElementSibling.classList.remove("collapsed");',
            '  });',
            '}',
            'function collapseAll() {',
            '  document.querySelectorAll(".category-header").forEach(h => {',
            '    h.classList.add("collapsed");',
            '    h.nextElementSibling.classList.add("collapsed");',
            '  });',
            '}',
            '</script>',
            '</body></html>'
  )

  return(html)
}




