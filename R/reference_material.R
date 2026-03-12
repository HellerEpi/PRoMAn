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

  # Define all functions by category - UPDATED TO MATCH ACTUAL IMPLEMENTATION
  functions <- list(
    "Project Setup & Navigation" = list(
      list(name = "create_project()",
           desc = "Create new project with GUI or programmatically",
           usage = 'create_project("P3.1", project_name = "My Project")'),
      list(name = "set_project()",
           desc = "Set working directory and load file shortcuts to global environment",
           usage = "set_project()
# Also creates: paths, read_source(), save_cleaned(), etc."),
      list(name = "get_project_paths()",
           desc = "Get all project folder paths as named list",
           usage = "paths <- get_project_paths()
paths$data_source
paths$data_cleaned"),
      list(name = "find_project_root()",
           desc = "Search upward for .proman file",
           usage = "root <- find_project_root()"),
      list(name = "file_path()",
           desc = "Get full path with minimal syntax",
           usage = 'file_path("myfile.csv", "cleaned")
file_path("script.R", "r_programs")'),
      list(name = "read_file()",
           desc = "Read CSV/RDS/Excel from any project folder (auto-detects extension)",
           usage = 'data <- read_file("mydata", "source")
data <- read_file("results.rds", "cleaned")'),
      list(name = "save_file()",
           desc = "Save data frame to any project folder",
           usage = 'save_file(data, "results.csv", "cleaned")
save_file(model, "model.rds", "cleaned")'),
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
           usage = 'list_files("source")
list_files("cleaned", pattern = "\\\\.csv$")'),
      list(name = "file_exists()",
           desc = "Check if file exists in project (checks all folders if none specified)",
           usage = 'file_exists("data.csv")
file_exists("data.csv", "source")')
    ),

    "Fallback & Backup" = list(
      list(name = "set_fallback()",
           desc = "Create snapshot of current project state in hidden .fallback/ folder",
           usage = "set_fallback()  # Before risky operations"),
      list(name = "fallback()",
           desc = "Restore project to previous snapshot (prompts for confirmation)",
           usage = "fallback()"),
      list(name = "fallback_info()",
           desc = "Show information about existing fallback",
           usage = "fallback_info()
fallback_info(show_files = TRUE)")
    ),

    "Data Management" = list(
      list(name = "select_files()",
           desc = "Unified file selection: pattern, recent, sets, or interactive",
           usage = 'select_files(pattern = "baseline")
select_files(recent = "2h", folder = "source")
select_files(pattern = "data", exclude = c("old", "temp"))
select_files(interactive = TRUE)'),
      list(name = "load_files_by_keyword()",
           desc = "Load multiple files matching keywords into named list",
           usage = 'data_list <- load_files_by_keyword("2024")
data_list <- load_files_by_keyword("baseline", exclude = "old")'),
      list(name = "create_file_set()",
           desc = "Add identifier to group of files",
           usage = 'create_file_set(c("file1.csv", "file2.csv"), "batch_1")
create_file_set("recent:2h", "morning_data")'),
      list(name = "list_file_sets()",
           desc = "Show detected file sets in project",
           usage = "list_file_sets()"),
      list(name = "get_file_sets()",
           desc = "Get file sets as data structure for programmatic use",
           usage = "sets <- get_file_sets()"),
      list(name = "add_to_sets()",
           desc = "Add new identifier to existing file sets",
           usage = 'add_to_sets(c("baseline_series", "followup_series"), "clean_v2")'),
      list(name = "clean_strings()",
           desc = "Standardize strings: trim, uppercase, underscores",
           usage = 'clean_strings(c("New York", "new york"))
df <- clean_strings(df, "name_column")'),
      list(name = "save_with_timestamp()",
           desc = "Save with automatic timestamp in filename",
           usage = 'save_with_timestamp(data, "analysis_results")'),
      list(name = "archive_files()",
           desc = "Move files to old/ folder with optional date subfolder",
           usage = 'archive_files(c("old_data.csv", "temp.csv"))'),
      list(name = "check_required_files()",
           desc = "Verify required files exist before analysis",
           usage = 'check_required_files(c("data.csv", "config.R"))')
    ),

    "Data Analysis Helpers" = list(
      list(name = "data_snapshot()",
           desc = "Quick overview: dimensions, types, missingness, unique values",
           usage = "data_snapshot(df)
data_snapshot(df, save_summary = TRUE)"),
      list(name = "compare_datasets()",
           desc = "Compare two datasets: added/removed columns, type changes",
           usage = "compare_datasets(old_df, new_df)"),
      list(name = "missing_data_patterns()",
           desc = "Analyze missing data with threshold flagging",
           usage = "missing_data_patterns(df)
missing_data_patterns(df, threshold = 10)"),
      list(name = "flag_suspicious_values()",
           desc = "Find potential missing data codes (-999, -99, etc.)",
           usage = "flag_suspicious_values(df)
flag_suspicious_values(df, c(-999, -99, 999))"),
      list(name = "detect_id_columns()",
           desc = "Identify likely ID variables by name pattern or uniqueness",
           usage = "id_cols <- detect_id_columns(df)"),
      list(name = "standardize_var_names()",
           desc = "Convert variable names to snake_case",
           usage = 'df <- standardize_var_names(df)'),
      list(name = "clean_demographics()",
           desc = "Clean common demographic variables",
           usage = 'df <- clean_demographics(df, age_var = "age", gender_var = "sex")'),
      list(name = "generate_data_dictionary()",
           desc = "Create data documentation with optional stats",
           usage = 'generate_data_dictionary(df)
generate_data_dictionary(df, filename = "data_dict.csv", guess_descriptions = TRUE)'),
      list(name = "get_folder_summary()",
           desc = "Summary of folder contents with sizes and dates",
           usage = 'get_folder_summary()
get_folder_summary(sort_by = "size")'),
      list(name = "find_stale_files()",
           desc = "Find files not modified in N days",
           usage = "find_stale_files(days = 30)"),
      list(name = "find_large_files()",
           desc = "Find files above size threshold",
           usage = "find_large_files(min_size_mb = 10)")
    ),

    "Project Logging & Status" = list(
      list(name = "generate_project_log()",
           desc = "Create comprehensive project status log",
           usage = "generate_project_log()
generate_project_log(days_back = 14, save_to_file = TRUE)"),
      list(name = "generate_project_log_with_comment()",
           desc = "Generate log with user notes about progress/next steps",
           usage = 'generate_project_log_with_comment(user_comment = "Completed data cleaning")'),
      list(name = "quick_project_status()",
           desc = "Short status summary for quick check",
           usage = "quick_project_status()"),
      list(name = "analyze_file_sets()",
           desc = "Detect and display file organization patterns",
           usage = "analyze_file_sets()
analyze_file_sets(return_data = TRUE)"),
      list(name = "infer_recent_activity()",
           desc = "Smart inference about recent work patterns",
           usage = "infer_recent_activity(days_back = 7)")
    ),

    "Figure Management" = list(
      list(name = "save_figure()",
           desc = "Save figures with metadata tracking (ggplot, gt, base R)",
           usage = 'save_figure(p1, "analysis_plot")
save_figure(p1, "fig1", caption = "Main results", figure_type = "main")
save_figure(base_plot_function = function() plot(x, y))'),
      list(name = "quick_save()",
           desc = "Save figure with auto-generated timestamped name",
           usage = "quick_save(p1)
quick_save(p1, prefix = 'exploratory')"),
      list(name = "list_figures()",
           desc = "Show all registered figures with metadata",
           usage = 'list_figures()
list_figures(type = "main")'),
      list(name = "update_figure_info()",
           desc = "Update caption, notes, or type for existing figure",
           usage = 'update_figure_info("fig1", caption = "Updated caption")'),
      list(name = "deregister_figure()",
           desc = "Remove figure(s) from registry, optionally delete files",
           usage = 'deregister_figure("old_plot")
deregister_figure(c("temp1", "temp2"), delete_files = TRUE)'),
      list(name = "create_figure_summary()",
           desc = "Generate markdown document of all figures",
           usage = "create_figure_summary()"),
      list(name = "export_figure_package()",
           desc = "Bundle figures for manuscript submission",
           usage = 'export_figure_package()
export_figure_package(format = "tiff", dpi = 600)'),
      list(name = "theme_academic()",
           desc = "Clean ggplot theme for publication",
           usage = 'p + theme_academic()
p + theme_academic(base_size = 12, grid = "y")'),
      list(name = "academic_palette()",
           desc = "Colorblind-friendly color palettes",
           usage = 'academic_palette("categorical", n = 4)
academic_palette("sequential")
academic_palette("heller")'),
      list(name = "format_academic()",
           desc = "Format numbers for academic presentation",
           usage = 'format_academic(0.0034, "p_value")  # "p < .01"
format_academic(0.45, "correlation")  # ".45"'),
      list(name = "create_table_figure_pair()",
           desc = "Generate matching table and visualization",
           usage = 'create_table_figure_pair(df, c("var1", "var2"), save_pair = TRUE)')
    ),

    "Workflow System" = list(
      list(name = "workflow_template()",
           desc = "Generate workflow template with markers",
           usage = "workflow_template()  # Shows/inserts template"),
      list(name = "extract_workflows()",
           desc = "Extract and register workflows from script",
           usage = "extract_workflows()  # From active document
extract_workflows('analysis.R')"),
      list(name = "apply_workflow()",
           desc = "Apply workflow to file set",
           usage = 'apply_workflow("clean_data", "baseline_files")
apply_workflow("analysis", files, parallel = TRUE, continue_on_error = TRUE)'),
      list(name = "apply_workflow_chunked()",
           desc = "Apply workflow with memory management for large sets",
           usage = 'apply_workflow_chunked("process", files, chunk_size = 100)'),
      list(name = "list_workflows()",
           desc = "Show registered workflows",
           usage = "list_workflows()"),
      list(name = "show_workflow()",
           desc = "Show detailed workflow info including I/O analysis",
           usage = 'show_workflow("clean_data")'),
      list(name = "validate_workflow()",
           desc = "Check workflow for common issues",
           usage = 'validate_workflow("my_workflow")'),
      list(name = "test_workflow()",
           desc = "Test workflow on single file before batch",
           usage = 'test_workflow("clean_data", "test_file.csv")'),
      list(name = "remove_workflow()",
           desc = "Delete registered workflow",
           usage = 'remove_workflow("old_workflow")'),
      list(name = "apply_workflows()",
           desc = "Run multiple workflows on same file set",
           usage = 'apply_workflows(c("clean", "analyze"), "dataset")'),
      list(name = "create_workflow_chain()",
           desc = "Define sequential workflow pipeline",
           usage = 'create_workflow_chain("pipeline", c("prep", "clean", "analyze"))'),
      list(name = "apply_workflow_chain()",
           desc = "Run workflow chain",
           usage = 'apply_workflow_chain("pipeline", "dataset")'),
      list(name = "export_workflow()",
           desc = "Export workflow as standalone R script",
           usage = 'export_workflow("clean_data")'),
      list(name = "workflow_summary()",
           desc = "Statistics about registered workflows",
           usage = "workflow_summary()")
    ),

    "Blitzschreiben Workflow" = list(
      list(name = "init_blitz_project()",
           desc = "Initialize project for Blitzschreiben methodology",
           usage = 'init_blitz_project("P3.1", "RCT Analysis")'),
      list(name = "create_blitz_template()",
           desc = "Create template for any stage",
           usage = 'create_blitz_template(1, "P3.1", title = "My Study")
create_blitz_template(2, "P3.1", analysis_type = "regression")
create_blitz_template(3, "P3.1", paper_type = "journal")'),
      list(name = "create_stage1_card()",
           desc = "Create Stage I triage card (wrapper)",
           usage = 'create_stage1_card("My Research Idea", "P3.1")'),
      list(name = "create_analyseblitz_template()",
           desc = "Create Stage II analysis template (wrapper)",
           usage = 'create_analyseblitz_template("P3.1")'),
      list(name = "create_draftschreiben_template()",
           desc = "Create Stage III writing template (wrapper)",
           usage = 'create_draftschreiben_template("P3.1", target_journal = "JAMA")'),
      list(name = "start_triage_timer()",
           desc = "Start 15-minute triage countdown",
           usage = 'start_triage_timer()
start_triage_timer(minutes = 20, project_code = "P3.1")'),
      list(name = "check_triage_time()",
           desc = "Check remaining time on triage timer",
           usage = "check_triage_time()"),
      list(name = "end_triage_timer()",
           desc = "End timer and log completion",
           usage = 'end_triage_timer(decision = "ready", next_steps = "Start Stage II")'),
      list(name = "stage2_progress_check()",
           desc = "Check Stage II analyseblitz status",
           usage = 'stage2_progress_check("P3.1")'),
      list(name = "stage3_progress_check()",
           desc = "Check Stage III draft progress and word counts",
           usage = 'stage3_progress_check("P3.1")'),
      list(name = "blitzschreiben_status()",
           desc = "Dashboard of all projects across stages",
           usage = "blitzschreiben_status()"),
      list(name = "todays_blitz_tasks()",
           desc = "Day-specific task recommendations",
           usage = "todays_blitz_tasks()"),
      list(name = "advance_to_next_stage()",
           desc = "Move project through pipeline",
           usage = 'advance_to_next_stage("P3.1", decision = "ready")'),
      list(name = "update_tracker_entry()",
           desc = "Remove, archive, or pause project in tracker",
           usage = 'update_tracker_entry("P3.1", action = "archive")'),
      list(name = "list_all_projects()",
           desc = "Show all tracked projects including inactive",
           usage = "list_all_projects()"),
      list(name = "create_master_script()",
           desc = "Generate orchestrating analysis script",
           usage = 'create_master_script("P3.1")'),
      list(name = "run_master_analysis()",
           desc = "Execute master script with monitoring",
           usage = 'run_master_analysis("P3.1")'),
      list(name = "test_stage1_simulation()",
           desc = "Test data simulation from Stage I specs",
           usage = 'test_stage1_simulation("P3.1")'),
      list(name = "track_sources()",
           desc = "Monitor PDF collection progress",
           usage = 'track_sources(search_strategy = "PubMed: sleep AND cognition")'),
      list(name = "log_blitz_activity()",
           desc = "Log Blitzschreiben activity",
           usage = 'log_blitz_activity("Completed analysis", "P3.1")')
    ),

    "Bibliography & Source Management" = list(
      list(name = "generate_bibliography()",
           desc = "Create .bib file from PDFs using DOI lookup and text extraction",
           usage = "generate_bibliography()
generate_bibliography(tag = 'methods')"),
      list(name = "update_bibliography()",
           desc = "Merge .bib files and incorporate annotations",
           usage = "update_bibliography()"),
      list(name = "check_bibliography()",
           desc = "Show bibliography status and statistics",
           usage = "check_bibliography()"),
      list(name = "annotated_bibliography()",
           desc = "Create Quarto template for reviewing/annotating entries",
           usage = "annotated_bibliography()
annotated_bibliography(only_incomplete = TRUE)"),
      list(name = "find_missing_dois()",
           desc = "Search CrossRef for entries without DOIs",
           usage = "find_missing_dois()"),
      list(name = "tag_pdfs()",
           desc = "Tag PDFs with keywords for organization",
           usage = 'tag_pdfs("all", c("methods", "background"))
tag_pdfs(c("paper1.pdf", "paper2.pdf"), "key_reference")'),
      list(name = "rename_pdfs()",
           desc = "Suggest Author_Year_Title.pdf naming",
           usage = "rename_pdfs()
rename_pdfs(apply = TRUE)"),
      list(name = "translate_bibliography()",
           desc = "Export to RIS or CSV format",
           usage = 'translate_bibliography("ris")
translate_bibliography("csv")'),
      list(name = "create_pdf_reading_note()",
           desc = "Create structured reading note from PDF",
           usage = 'create_pdf_reading_note("paper.pdf")
create_pdf_reading_note("select")  # Interactive'),
      list(name = "review_reading_notes()",
           desc = "Dashboard of all reading notes",
           usage = "review_reading_notes()
review_reading_notes(days_back = 7)"),
      list(name = "export_project_reading_notes()",
           desc = "Compile reading notes for a project",
           usage = 'export_project_reading_notes("P3.1")')
    ),

    "Collaboration & Communication" = list(
      list(name = "create_collaborator_onboarding()",
           desc = "Generate onboarding package with executive/tutorial/operational docs",
           usage = 'create_collaborator_onboarding()
create_collaborator_onboarding(collaborator_name = "Dr. Smith", package_type = "mixed")'),
      list(name = "create_update_template()",
           desc = "Email template for project updates",
           usage = 'create_update_template(update_type = "progress")
create_update_template(update_type = "results")'),
      list(name = "create_meeting_agenda()",
           desc = "Generate meeting agenda template",
           usage = 'create_meeting_agenda(meeting_type = "kickoff")
create_meeting_agenda(meeting_type = "results", attendees = c("Dr. A", "Dr. B"))')
    )
  )


  # Build HTML with compact styling
  html <- c(
    '<!DOCTYPE html>',
    '<html><head>',
    '<meta charset="UTF-8">',
    '<title>PRoMan Quick Reference</title>',
    '<style>',
    'body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Arial, sans-serif; ',
    '  margin: 15px; background-color: #f8f9fa; max-width: 700px; }',
    'h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; ',
    '  font-size: 22px; margin-bottom: 10px; }',
    '.subtitle { color: #7f8c8d; font-size: 12px; margin-bottom: 15px; }',
    '.category { margin-bottom: 12px; background: white; border-radius: 6px; ',
    '  box-shadow: 0 1px 3px rgba(0,0,0,0.1); }',
    '.category-header { ',
    '  background: #34495e; color: white; padding: 10px 15px; ',
    '  cursor: pointer; user-select: none; border-radius: 6px 6px 0 0;',
    '  display: flex; justify-content: space-between; align-items: center;',
    '  font-weight: bold; font-size: 13px;',
    '}',
    '.category-header:hover { background: #2c3e50; }',
    '.category-header::after { content: "\\2212"; font-size: 18px; font-weight: bold; }',
    '.category-header.collapsed::after { content: "+"; font-size: 18px; }',
    '.category-content { padding: 8px; display: block; }',
    '.category-content.collapsed { display: none; }',
    '.function { padding: 8px; border-bottom: 1px solid #ecf0f1; }',
    '.function:last-child { border-bottom: none; }',
    '.function:hover { background: #fafbfc; }',
    '.fname { color: #c0392b; font-family: "Consolas", "Monaco", monospace; ',
    '  font-weight: bold; font-size: 12px; }',
    '.desc { color: #555; font-size: 11px; margin-top: 3px; }',
    '.usage { color: #27ae60; font-family: "Consolas", "Monaco", monospace; ',
    '  font-size: 10px; margin-top: 4px; background: #f8f9fa; padding: 4px 6px; ',
    '  border-radius: 3px; display: block; white-space: pre-wrap; line-height: 1.4; }',
    '.tip { background: #e8f4fd; padding: 12px; border-left: 4px solid #3498db; ',
    '  margin: 15px 0; font-size: 12px; border-radius: 0 4px 4px 0; }',
    '.tip code { background: #d4e9f7; padding: 1px 4px; border-radius: 2px; }',
    '.controls { margin: 15px 0; text-align: center; }',
    '.btn { background: #3498db; color: white; border: none; padding: 8px 16px; ',
    '  margin: 0 4px; border-radius: 4px; cursor: pointer; font-size: 12px; }',
    '.btn:hover { background: #2980b9; }',
    '.search-box { width: 100%; padding: 10px; font-size: 14px; border: 1px solid #ddd; ',
    '  border-radius: 4px; margin-bottom: 15px; box-sizing: border-box; }',
    '.hidden { display: none !important; }',
    '.version { text-align: center; color: #95a5a6; font-size: 11px; margin-top: 20px; }',
    '</style>',
    '</head><body>',
    '<h1>PRoMan Quick Reference</h1>',
    '<div class="subtitle">Project Management in R (Academic) - Function Reference</div>'
  )

  # Add search box
  html <- c(html,
            '<input type="text" class="search-box" id="searchBox" ',
            'placeholder="Search functions..." onkeyup="searchFunctions()">',
            ''
  )

  # Add controls
  html <- c(html,
            '<div class="controls">',
            '<button class="btn" onclick="expandAll()">Expand All</button>',
            '<button class="btn" onclick="collapseAll()">Collapse All</button>',
            '</div>'
  )

  # Add quick tips
  html <- c(html,
            '<div class="tip">',
            '<strong>Quick Start:</strong> Run <code>set_project()</code> first! ',
            'This loads shortcuts like <code>read_source()</code>, <code>save_cleaned()</code>, ',
            'and <code>paths</code> into your environment. Most functions then work from any subdirectory.',
            '</div>'
  )

  # Count total functions
  total_funcs <- sum(sapply(functions, length))

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
      # Escape HTML in usage examples
      usage_escaped <- gsub("<", "&lt;", func$usage)
      usage_escaped <- gsub(">", "&gt;", usage_escaped)

      html <- c(html,
                '<div class="function">',
                paste0('<div class="fname">', func$name, '</div>'),
                paste0('<div class="desc">', func$desc, '</div>'))

      if(!is.null(func$usage)) {
        html <- c(html, paste0('<div class="usage">', usage_escaped, '</div>'))
      }

      html <- c(html, '</div>')
    }

    html <- c(html, '</div></div>')
  }

  # Add version info
  html <- c(html,
            paste0('<div class="version">PRoMan v', as.character(packageVersion("PRoMan")),
                   ' | ', total_funcs, ' functions | Generated ', Sys.Date(), '</div>')
  )

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
            'function searchFunctions() {',
            '  const query = document.getElementById("searchBox").value.toLowerCase();',
            '  const functions = document.querySelectorAll(".function");',
            '  const categories = document.querySelectorAll(".category");',
            '',
            '  if (query === "") {',
            '    // Reset to default state',
            '    functions.forEach(f => f.classList.remove("hidden"));',
            '    categories.forEach(c => c.classList.remove("hidden"));',
            '    return;',
            '  }',
            '',
            '  // Hide non-matching functions',
            '  functions.forEach(f => {',
            '    const text = f.textContent.toLowerCase();',
            '    if (text.includes(query)) {',
            '      f.classList.remove("hidden");',
            '    } else {',
            '      f.classList.add("hidden");',
            '    }',
            '  });',
            '',
            '  // Hide categories with no visible functions',
            '  categories.forEach(cat => {',
            '    const content = cat.querySelector(".category-content");',
            '    const visibleFuncs = content.querySelectorAll(".function:not(.hidden)");',
            '    if (visibleFuncs.length === 0) {',
            '      cat.classList.add("hidden");',
            '    } else {',
            '      cat.classList.remove("hidden");',
            '      // Expand categories with matches',
            '      cat.querySelector(".category-header").classList.remove("collapsed");',
            '      content.classList.remove("collapsed");',
            '    }',
            '  });',
            '}',
            '</script>',
            '</body></html>'
  )

  return(paste(html, collapse = "\n"))
}



