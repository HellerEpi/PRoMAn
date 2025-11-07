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

  # Define all functions by category with better descriptions
  functions <- list(
    "Project Setup & Navigation" = list(
      list(name = "create_project()",
           desc = "Create new project with GUI or programmatically",
           usage = 'create_project("P3.1", "My Project")'),
      list(name = "set_project()",
           desc = "Set working directory to project root",
           usage = "set_project()  # finds .proman file"),
      list(name = "get_project_paths()",
           desc = "Get all project folder paths",
           usage = "paths <- get_project_paths()"),
      list(name = "set_fallback()",
           desc = "Create snapshot of current project state",
           usage = "set_fallback()"),
      list(name = "fallback()",
           desc = "Restore project to previous snapshot",
           usage = "fallback()  # prompts for confirmation")
    ),

    "Data Management" = list(
      list(name = "select_files_by_keyword()",
           desc = "Find files matching keywords",
           usage = 'select_files_by_keyword("baseline")'),
      list(name = "load_files_by_keyword()",
           desc = "Load multiple CSV/RDS files as list",
           usage = 'data_list <- load_files_by_keyword("2024")'),
      list(name = "clean_strings()",
           desc = "Standardize strings for joining (uppercase, no spaces)",
           usage = 'df <- clean_strings(df, "id_column")'),
      list(name = "data_snapshot()",
           desc = "Quick overview of dataset quality",
           usage = "data_snapshot(mydata)"),
      list(name = "flag_suspicious_values()",
           desc = "Find potential missing data codes (-999, 999, etc)",
           usage = "flag_suspicious_values(mydata)"),
      list(name = "missing_data_patterns()",
           desc = "Analyze missing data by variable",
           usage = "missing_data_patterns(mydata, threshold = 5)"),
      list(name = "standardize_var_names()",
           desc = "Convert variable names to snake_case",
           usage = 'standardize_var_names(mydata, "snake_case")'),
      list(name = "generate_data_dictionary()",
           desc = "Create documentation for all variables",
           usage = 'generate_data_dictionary(mydata, "data_dict.csv")')
    ),

    "File Sets & Batch Operations" = list(
      list(name = "create_file_set()",
           desc = "Add identifier to group of files",
           usage = 'create_file_set("baseline", "BL", position = "prefix")'),
      list(name = "list_file_sets()",
           desc = "Show detected file groups in project",
           usage = "list_file_sets()"),
      list(name = "add_to_sets()",
           desc = "Add new identifier to existing file sets",
           usage = 'add_to_sets(c("baseline", "followup"), "merged")'),
      list(name = "select_recent_files()",
           desc = "Get files modified in last N minutes",
           usage = "select_recent_files(30)  # last 30 minutes")
    ),

    "Blitzschreiben Workflow" = list(
      list(name = "init_blitz_project()",
           desc = "Initialize project for Blitzschreiben workflow",
           usage = 'init_blitz_project("P3.1", "COVID Analysis")'),
      list(name = "create_blitz_template()",
           desc = "Create stage template (1=triage, 2=analyse, 3=draft)",
           usage = 'create_blitz_template(1, "P3.1", title = "My Idea")'),
      list(name = "start_triage_timer()",
           desc = "Start 15-minute timer for Stage I triage",
           usage = "start_triage_timer(15, project_code = 'P3.1')"),
      list(name = "create_master_script()",
           desc = "Generate orchestrating script for analysis components",
           usage = 'create_master_script("P3.1")'),
      list(name = "blitzschreiben_status()",
           desc = "View all projects across pipeline stages",
           usage = "blitzschreiben_status()"),
      list(name = "todays_blitz_tasks()",
           desc = "Get tasks for today based on day of week",
           usage = "todays_blitz_tasks()"),
      list(name = "advance_to_next_stage()",
           desc = "Move project to next stage in pipeline",
           usage = 'advance_to_next_stage("P3.1", decision = "ready")')
    ),

    "Figures & Academic Output" = list(
      list(name = "save_figure()",
           desc = "Save plot/table with metadata (GUI prompts for details)",
           usage = "save_figure(myplot)  # GUI will ask for details"),
      list(name = "quick_save()",
           desc = "Save figure with minimal prompts",
           usage = 'quick_save(myplot, "descriptive_stats")'),
      list(name = "list_figures()",
           desc = "Show all registered figures with metadata",
           usage = 'list_figures(type = "main")'),
      list(name = "create_figure_summary()",
           desc = "Generate Word doc with all figures and captions",
           usage = "create_figure_summary()"),
      list(name = "export_figure_package()",
           desc = "Bundle figures for journal submission",
           usage = 'export_figure_package(c("fig1", "fig2"))'),
      list(name = "theme_academic()",
           desc = "Clean ggplot2 theme for publications",
           usage = "ggplot() + theme_academic()"),
      list(name = "academic_palette()",
           desc = "Colorblind-friendly color palette",
           usage = 'scale_color_manual(values = academic_palette())'),
      list(name = "format_academic()",
           desc = "Format p-values and statistics properly",
           usage = 'format_academic(0.0001, type = "p")')
    ),

    "File Management & Archiving" = list(
      list(name = "archive_files()",
           desc = "Move files to old/ folder with date stamp",
           usage = 'archive_files(c("old_data.csv", "temp.R"))'),
      list(name = "save_with_timestamp()",
           desc = "Save data with automatic timestamp in filename",
           usage = 'save_with_timestamp(mydata, "analysis_results")'),
      list(name = "find_stale_files()",
           desc = "Find files not modified in N days",
           usage = "find_stale_files(days = 30)"),
      list(name = "find_large_files()",
           desc = "Find files larger than N MB",
           usage = "find_large_files(min_size_mb = 10)"),
      list(name = "check_required_files()",
           desc = "Verify that required files exist before analysis",
           usage = 'check_required_files(c("data.csv", "codebook.xlsx"))'),
      list(name = "compare_datasets()",
           desc = "Show differences between two datasets",
           usage = "compare_datasets(old_data, new_data)")
    ),

    "Workflows & Automation" = list(
      list(name = "extract_workflows()",
           desc = "Extract reusable workflows from scripts",
           usage = "extract_workflows()  # from active script"),
      list(name = "apply_workflow()",
           desc = "Apply workflow to file set",
           usage = 'apply_workflow("clean_data", "baseline_files")'),
      list(name = "list_workflows()",
           desc = "Show all registered workflows",
           usage = "list_workflows()"),
      list(name = "workflow_template()",
           desc = "Generate template for new workflow",
           usage = 'workflow_template("my_analysis")')
    ),

    "Project Logging & Status" = list(
      list(name = "generate_project_log()",
           desc = "Create comprehensive project status report",
           usage = "generate_project_log(days_back = 7)"),
      list(name = "quick_project_status()",
           desc = "Brief status check of recent activity",
           usage = "quick_project_status()"),
      list(name = "track_sources()",
           desc = "Monitor PDF collection in sources folder",
           usage = 'track_sources("systematic review", new_sources = 5)')
    ),

    "Collaboration & Communication" = list(
      list(name = "create_collaborator_onboarding()",
           desc = "Generate onboarding package for new collaborator",
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
            '<strong>Tip:</strong> Use <code>?function_name</code> for detailed help. ',
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




