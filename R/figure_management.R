# R/figure_management.R
#
# Figure and publication management for PRoMan
#
# Includes:
# - Figure registry and metadata management
# - Academic styling and standards
# - Table-figure combinations
# - Word document integration
# - Figure organization and workflow

### ------------------------------------------------------------ ###


#' Get figure metadata via GUI form
#'
#' Opens a form to collect figure information
#'
#' @param defaults List with default values
#' @return List with figure metadata or NULL if cancelled
get_figure_metadata_gui <- function(defaults = list()) {

  if(!requireNamespace("tcltk", quietly = TRUE)) {
    return(NULL)
  }

  # Create dialog window
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "Save Figure")

  # Variables with defaults
  name_var <- tcltk::tclVar(null_coalesce(defaults$name, "figure_1"))
  caption_var <- tcltk::tclVar(null_coalesce(defaults$caption, ""))
  type_var <- tcltk::tclVar(null_coalesce(defaults$type, "main"))
  width_var <- tcltk::tclVar(null_coalesce(defaults$width, "6.5"))
  height_var <- tcltk::tclVar(null_coalesce(defaults$height, "4"))
  dpi_var <- tcltk::tclVar(null_coalesce(defaults$dpi, "300"))
  notes_var <- tcltk::tclVar(null_coalesce(defaults$notes, ""))

  # Build form
  row <- 0

  # Name field
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Figure name:"),
                row = row, column = 0, sticky = "w", padx = 5, pady = 5)
  name_entry <- tcltk::tkentry(tt, textvariable = name_var, width = 30)
  tcltk::tkgrid(name_entry, row = row, column = 1, padx = 5, pady = 5)

  # Caption field
  row <- row + 1
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Caption:"),
                row = row, column = 0, sticky = "nw", padx = 5, pady = 5)
  caption_text <- tcltk::tktext(tt, height = 3, width = 40, wrap = "word")
  tcltk::tkinsert(caption_text, "1.0", tcltk::tclvalue(caption_var))
  tcltk::tkgrid(caption_text, row = row, column = 1, padx = 5, pady = 5)

  # Type selection
  row <- row + 1
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Type:"),
                row = row, column = 0, sticky = "w", padx = 5, pady = 5)
  type_frame <- tcltk::tkframe(tt)
  tcltk::tkgrid(type_frame, row = row, column = 1, sticky = "w")

  for(type in c("main", "supplementary", "exploratory")) {
    tcltk::tkpack(tcltk::tkradiobutton(type_frame, text = type,
                                       variable = type_var, value = type),
                  side = "left", padx = 5)
  }

  # Size fields in a frame
  row <- row + 1
  size_frame <- tcltk::tkframe(tt)
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Size:"),
                row = row, column = 0, sticky = "w", padx = 5, pady = 5)
  tcltk::tkgrid(size_frame, row = row, column = 1, sticky = "w", padx = 5, pady = 5)

  # Width
  tcltk::tkpack(tcltk::tklabel(size_frame, text = "Width:"), side = "left")
  width_entry <- tcltk::tkentry(size_frame, textvariable = width_var, width = 8)
  tcltk::tkpack(width_entry, side = "left", padx = 5)

  # Height
  tcltk::tkpack(tcltk::tklabel(size_frame, text = "Height:"), side = "left")
  height_entry <- tcltk::tkentry(size_frame, textvariable = height_var, width = 8)
  tcltk::tkpack(height_entry, side = "left", padx = 5)

  # DPI
  tcltk::tkpack(tcltk::tklabel(size_frame, text = "DPI:"), side = "left")
  dpi_entry <- tcltk::tkentry(size_frame, textvariable = dpi_var, width = 8)
  tcltk::tkpack(dpi_entry, side = "left", padx = 5)

  # Notes field
  row <- row + 1
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Notes (optional):"),
                row = row, column = 0, sticky = "nw", padx = 5, pady = 5)
  notes_text <- tcltk::tktext(tt, height = 2, width = 40, wrap = "word")
  tcltk::tkinsert(notes_text, "1.0", tcltk::tclvalue(notes_var))
  tcltk::tkgrid(notes_text, row = row, column = 1, padx = 5, pady = 5)

  # Result storage
  result <- NULL

  # Button functions
  save_function <- function() {
    # Get the values
    notes_value <- as.character(tcltk::tkget(notes_text, "1.0", "end-1c"))
    caption_value <- as.character(tcltk::tkget(caption_text, "1.0", "end-1c"))

    result <<- list(
      name = as.character(tcltk::tclvalue(name_var)),
      caption = if(length(caption_value) == 0) "" else caption_value,
      type = as.character(tcltk::tclvalue(type_var)),
      width = as.numeric(as.character(tcltk::tclvalue(width_var))),
      height = as.numeric(as.character(tcltk::tclvalue(height_var))),
      dpi = as.numeric(as.character(tcltk::tclvalue(dpi_var))),
      notes = if(length(notes_value) == 0) "" else notes_value
    )
    tcltk::tkdestroy(tt)
  }
  cancel_function <- function() {
    result <<- NULL
    tcltk::tkdestroy(tt)
  }

  # Button frame
  row <- row + 1
  button_frame <- tcltk::tkframe(tt)
  tcltk::tkgrid(button_frame, row = row, column = 0, columnspan = 2, pady = 10)

  save_btn <- tcltk::tkbutton(button_frame, text = "Save Figure",
                              command = save_function, width = 12)
  cancel_btn <- tcltk::tkbutton(button_frame, text = "Cancel",
                                command = cancel_function, width = 12)

  tcltk::tkpack(save_btn, side = "left", padx = 5)
  tcltk::tkpack(cancel_btn, side = "left", padx = 5)

  # Bind Enter key to save
  tcltk::tkbind(tt, "<Return>", save_function)
  tcltk::tkbind(tt, "<Escape>", cancel_function)

  # Focus and wait
  tcltk::tkfocus(name_entry)
  tcltk::tkgrab.set(tt)
  tcltk::tkwait.window(tt)

  return(result)
}

#' Save figure with GUI prompts for metadata
#'
#' Enhanced version that prompts for missing information via GUI
#'
#' @param plot_object Plot/table object (ggplot, gt_tbl, etc.)
#' @param figure_name Descriptive name (prompts if NULL)
#' @param figure_type Type: "main", "supplementary", "exploratory"
#' @param caption Figure caption (prompts if NULL)
#' @param notes Internal notes
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution
#' @param use_gui Use GUI for missing info (default: TRUE in interactive sessions)
#' @param formats Formats to save (auto-detected if NULL)
#' @param base_plot_function Function for base R plots
#' @return Path to saved figure
#' @export
save_figure <- function(plot_object = NULL,
                        figure_name = NULL,
                        figure_type = "main",
                        caption = NULL,
                        notes = "",
                        width = 6.5,
                        height = 4,
                        dpi = 300,
                        use_gui = interactive(),
                        formats = NULL,
                        base_plot_function = NULL) {

  # Get project data directly from .proman
  proman_data <- read_proman()
  figures_dir <- proman_data$.paths$figures

  if(!dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE)
  }

  # Check if we need to collect metadata via GUI
  needs_gui <- use_gui && (is.null(figure_name) || is.null(caption))

  if(needs_gui && requireNamespace("tcltk", quietly = TRUE)) {
    # Prepare defaults
    defaults <- list(
      name = null_coalesce(figure_name, paste0("figure_", format(Sys.time(), "%Y%m%d_%H%M"))),
      caption = null_coalesce(caption, ""),
      type = figure_type,
      width = width,
      height = height,
      dpi = dpi,
      notes = notes
    )

    # Get metadata via GUI
    metadata <- get_figure_metadata_gui(defaults)

    if(is.null(metadata)) {
      message("Figure save cancelled")
      return(invisible(NULL))
    }

    # Update parameters from GUI
    figure_name <- metadata$name
    caption <- metadata$caption
    figure_type <- metadata$type
    width <- metadata$width
    height <- metadata$height
    dpi <- metadata$dpi
    notes <- metadata$notes

  } else if(is.null(figure_name)) {
    # Only error if GUI is not available
    stop("figure_name is required when use_gui = FALSE")
  }

  # Set caption to empty string if still NULL
  if(is.null(caption)) caption <- ""

  # Clean figure name
  clean_name <- gsub("[^a-zA-Z0-9_]", "_", figure_name)
  saved_files <- character()

  # Auto-detect formats based on object type
  if(!is.null(plot_object)) {
    if(inherits(plot_object, "gt_tbl")) {
      if(is.null(formats)) formats <- c("png", "html")

      for(format in formats) {
        filename <- paste0(clean_name, ".", format)
        filepath <- file.path(figures_dir, filename)

        if(format == "png") {
          # Convert width to pixels (approximately 100 dpi for gt)
          width_px <- width * 100
          gt::gtsave(plot_object, filepath, vwidth = width_px, expand = 10)
        } else if(format == "rds") {
          saveRDS(plot_object, filepath)
        } else {
          gt::gtsave(plot_object, filepath)
        }

        saved_files <- c(saved_files, filepath)
        message(paste("Created:", filename))
      }

      # Tables don't have traditional width/height
      width <- NA
      height <- NA

    } else if(inherits(plot_object, c("ggplot", "cowplot", "gtable", "grob"))) {
      if(is.null(formats)) formats <- c("png", "rds")

      for(format in formats) {
        filename <- paste0(clean_name, ".", format)
        filepath <- file.path(figures_dir, filename)

        if(format == "rds") {
          saveRDS(plot_object, filepath)
        } else {
          # Check if we have a ggplot object and if ggplot2 is available
          if(inherits(plot_object, c("ggplot", "ggplot2"))) {
            if(!requireNamespace("ggplot2", quietly = TRUE)) {
              stop("Package 'ggplot2' is required to save ggplot objects. Please install it or save as RDS format instead.")
            }
            # Use ggplot2::ggsave with explicit namespace
            ggplot2::ggsave(filepath, plot_object, width = width, height = height,
                            dpi = dpi, device = format, bg = "white")
          } else if(inherits(plot_object, c("grob", "gtable"))) {
            # For grid objects, use base R graphics
            if(format == "png") {
              png(filepath, width = width, height = height, units = "in", res = dpi)
              grid::grid.draw(plot_object)
              dev.off()
            } else if(format == "pdf") {
              pdf(filepath, width = width, height = height)
              grid::grid.draw(plot_object)
              dev.off()
            }
          } else {
            stop("Unknown plot object type. Use base_plot_function for base R plots.")
          }
        }
        saved_files <- c(saved_files, filepath)
        message(paste("Created:", filename))
      }
    }

  } else if(!is.null(base_plot_function)) {
    # Base R plot
    if(is.null(formats)) formats <- c("png", "pdf")

    for(format in formats) {
      filename <- paste0(clean_name, ".", format)
      filepath <- file.path(figures_dir, filename)

      if(format == "png") {
        png(filepath, width = width, height = height, units = "in", res = dpi)
        base_plot_function()
        dev.off()
      } else if(format == "pdf") {
        pdf(filepath, width = width, height = height)
        base_plot_function()
        dev.off()
      }

      saved_files <- c(saved_files, filepath)
      message(paste("Created:", filename))
    }

  } else {
    stop("Must provide either plot_object or base_plot_function")
  }

  # Register figure in metadata
  register_figure(figure_name, clean_name, figure_type, caption, notes,
                  formats, width, height)

  # Update statistics in .proman file
  registry_path <- file.path(figures_dir, "figure_registry.csv")
  if (file.exists(registry_path)) {
    proman_data$statistics$total_figures <- nrow(read.csv(registry_path))
  }
  proman_data$statistics$last_updated <- as.character(Sys.Date())

  # Write back the updated data
  write_proman(proman_data)

  # Show summary
  cat("\nFigure saved:", figure_name, "\n")
  if(nchar(caption) > 0) {
    caption_preview <- if(nchar(caption) > 60) {
      paste0(substr(caption, 1, 60), "...")
    } else {
      caption
    }
    cat("   Caption:", caption_preview, "\n")
  }
  cat("   Type:", figure_type, "\n")
  cat("   Project:", proman_data$project_name, "\n")
  cat("   Location:", file.path(proman_data$folders$registry$figures, "/\n"))

  return(invisible(saved_files))
}


#' Quick save figure with minimal input
#'
#' Wrapper for common use case - just save with auto-generated name
#'
#' @param plot_object Plot to save
#' @param prefix Prefix for auto-generated name (default: "fig")
#' @export
quick_save <- function(plot_object, prefix = "fig") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
  auto_name <- paste0(prefix, "_", timestamp)

  save_figure(plot_object,
              figure_name = auto_name,
              use_gui = FALSE)
}



#' Register figure in metadata registry
#'
#' Internal function to track figure metadata
#'
#' @param figure_name Original figure name
#' @param filename_base Base filename used
#' @param figure_type Type of figure
#' @param caption Caption text
#' @param notes Notes about figure
#' @param formats Formats saved
#' @param width Figure width
#' @param height Figure height
register_figure <- function(figure_name, filename_base, figure_type, caption, notes, formats, width, height) {

  proman_data <- read_proman()
  registry_path <- file.path(proman_data$.paths$figures, "figure_registry.csv")

  # Create or read existing registry
  if(file.exists(registry_path)) {
    registry <- read.csv(registry_path, stringsAsFactors = FALSE)
  } else {
    registry <- data.frame(
      figure_name = character(0),
      filename_base = character(0),
      figure_type = character(0),
      caption = character(0),
      notes = character(0),
      formats = character(0),
      width = numeric(0),
      height = numeric(0),
      created_date = character(0),
      modified_date = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Check if figure already exists
  existing_idx <- which(registry$figure_name == figure_name)



  new_entry <- data.frame(
    figure_name = figure_name,
    filename_base = filename_base,
    figure_type = figure_type,
    caption = caption,
    notes = notes,
    formats = paste(formats, collapse = ";"),
    width = width,
    height = height,
    created_date = ifelse(length(existing_idx) > 0, registry$created_date[existing_idx], as.character(Sys.Date())),
    modified_date = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )

  if(length(existing_idx) > 0) {
    # Update existing entry
    registry[existing_idx, ] <- new_entry
    message("Updated existing figure registration")
  } else {
    # Add new entry
    registry <- rbind(registry, new_entry)
    message("Registered new figure")
  }

  # Save registry
  write.csv(registry, registry_path, row.names = FALSE)
}

#' List all figures with metadata
#'
#' Shows registered figures and their information
#'
#' @param type Filter by figure type: "all", "main", "supplementary", "exploratory"
#' @param project_code Project code (currently unused, for future multi-project support)
#' @return Data frame with figure information
#' @export
list_figures <- function(type = "all", project_code = NULL) {

  # Get project data directly from .proman
  proman_data <- read_proman()

  # Use project_code from .proman if not provided
  if (is.null(project_code)) {
    project_code <- proman_data$project_code
  }

  registry_path <- file.path(proman_data$.paths$figures, "figure_registry.csv")

  if(!file.exists(registry_path)) {
    message("No figures registered yet in project: ", proman_data$project_name)
    return(data.frame())
  }

  registry <- read.csv(registry_path, stringsAsFactors = FALSE)

  if(type != "all") {
    registry <- registry[registry$figure_type == type, ]
  }

  if(nrow(registry) == 0) {
    message(paste("No figures found for type:", type))
    return(data.frame())
  }

  # Display summary with project info
  cat("FIGURE REGISTRY - ", proman_data$project_name, " (", proman_data$project_code, ")\n", sep = "")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  for(i in 1:nrow(registry)) {
    fig <- registry[i, ]
    cat(paste("Figure:", fig$figure_name, "\n"))
    cat(paste("  Type:", fig$figure_type, "\n"))
    cat(paste("  Files:", fig$filename_base, ".", gsub(";", ", ", fig$formats), "\n", sep = ""))
    cat(paste("  Size:", fig$width, "x", fig$height, "inches\n"))
    if(nchar(fig$caption) > 0) {
      cat(paste("  Caption:", fig$caption, "\n"))
    }
    if(nchar(fig$notes) > 0) {
      cat(paste("  Notes:", fig$notes, "\n"))
    }
    cat(paste("  Modified:", fig$modified_date, "\n"))
    cat("\n")
  }

  return(invisible(registry))
}

#' Update figure metadata without regenerating figure
#'
#' Modify caption, notes, or type for existing figure
#'
#' @param figure_name Name of existing figure
#' @param caption New caption text
#' @param notes New notes
#' @param figure_type New figure type
#' @export
update_figure_info <- function(figure_name, caption = NULL, notes = NULL, figure_type = NULL) {

  proman_data <- read_proman()
  registry_path <- file.path(proman_data$.paths$figures, "figure_registry.csv")

  if(!file.exists(registry_path)) {
    stop("No figure registry found. Create figures first with save_figure().")
  }

  registry <- read.csv(registry_path, stringsAsFactors = FALSE)
  existing_idx <- which(registry$figure_name == figure_name)

  if(length(existing_idx) == 0) {
    stop(paste("Figure not found:", figure_name))
  }

  # Update specified fields
  if(!is.null(caption)) {
    registry$caption[existing_idx] <- caption
  }
  if(!is.null(notes)) {
    registry$notes[existing_idx] <- notes
  }
  if(!is.null(figure_type)) {
    registry$figure_type[existing_idx] <- figure_type
  }

  registry$modified_date[existing_idx] <- as.character(Sys.Date())

  # Save updated registry
  write.csv(registry, registry_path, row.names = FALSE)

  message(paste("Updated figure info for:", figure_name))
}

### ------------------------------------------------------------ ###
### ACADEMIC STYLING AND STANDARDS

#' Academic ggplot theme for publication-ready figures
#'
#' Clean, minimal theme suitable for academic journals
#'
#' @param base_size Base font size
#' @param grid Show grid lines
#' @param use_website_colors Use custom website color scheme
#' @return ggplot theme object
#' @export
theme_academic <- function(base_size = 11, grid = FALSE) {

  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for theme_academic()")
  }

  # Use standard academic colors
  text_color <- "black"
  grid_color <- "grey90"
  strip_bg <- "grey90"

  theme <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Text elements
      text = ggplot2::element_text(color = text_color),
      axis.text = ggplot2::element_text(color = text_color),
      axis.title = ggplot2::element_text(color = text_color, size = base_size),
      plot.title = ggplot2::element_text(size = base_size + 2, hjust = 0, margin = ggplot2::margin(b = 20)),
      plot.subtitle = ggplot2::element_text(size = base_size, hjust = 0, margin = ggplot2::margin(b = 15)),

      # Panel and background
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),

      # Axis lines
      axis.line = ggplot2::element_line(color = text_color, linewidth = 0.5),
      axis.ticks = ggplot2::element_line(color = text_color, linewidth = 0.5),

      # Legend
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "bottom",

      # Strip text for facets
      strip.background = ggplot2::element_rect(fill = strip_bg, color = text_color),
      strip.text = ggplot2::element_text(color = text_color)
    )

  #work with plot grid
  if(isFALSE(grid)) {
    theme <- theme + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  } else if(isTRUE(grid)) {
    theme <- theme + ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = grid_color, size = 0.3),
      panel.grid.minor = ggplot2::element_blank()
    )
  } else if(grid == "x") {
    theme <- theme + ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = grid_color, size = 0.3),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  } else if(grid == "y") {
    theme <- theme + ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = grid_color, size = 0.3),
      panel.grid.minor = ggplot2::element_blank()
    )
  }


  return(theme)
}


#' Colorblind-friendly academic color palettes
#'
#' Returns colorblind-safe and print-friendly color palettes
#'
#' @param type Type of palette: "categorical", "sequential", "diverging"
#' @param n Number of colors needed (for categorical)
#' @return Vector of color codes
#' @export
academic_palette <- function(type = "categorical", n = NULL) {

  if(type == "categorical") {
    # Colorblind-friendly categorical palette (Wong, 2011)
    colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


    if(is.null(n)) {
      return(colors)
    } else if(n <= length(colors)) {
      return(colors[1:n])
    } else {
      warning(paste("Only", length(colors), "categorical colors available. Recycling colors."))
      return(rep(colors, length.out = n))
    }

  } else if(type == "heller") {
    # Your custom website palette
    colors <- c(
      green = "#1e5c4b",
      orange = "#C03221",
      blue = "#2F6690",
      indigo = "#440381",
      purple = "#730071",
      pink = "#D6A2AD",
      red = "#A0030B",
      yellow = "#F0A202",
      teal = "#008767",
      cyan = "#C0D6DF"
    )

    if(is.null(n)) {
      return(colors)
    } else if(n <= length(colors)) {
      return(unname(colors[1:n]))
    } else {
      warning(paste("Only", length(colors), "colors in custom palette. Recycling colors."))
      return(rep(unname(colors), length.out = n))
    }

  } else if(type == "sequential") {
    # Blue sequential palette
    return(c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"))

  } else if(type == "green_sequential") {
    # Hunter green to light mint sequential palette
    return(c("#F0F9F4", "#D0E8DC", "#7FC29B", "#4A9D6F", "#2F7A4E", "#1B5E3F", "#0B4228", "#052818"))
  } else if(type == "diverging") {
    # Blue-red diverging palette
    return(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))

  } else {
    stop("Palette type must be 'categorical', 'sequential', 'green_sequential', or 'diverging', or 'heller'")
  }
}

#' Format numbers for academic presentation
#'
#' Formats numbers according to academic conventions
#'
#' @param x Numeric vector to format
#' @param type Type of formatting: "p_value", "effect_size", "sample_size", "correlation"
#' @return Character vector of formatted numbers
#' @export
format_academic <- function(x, type = "p_value") {

  if(type == "p_value") {
    # Standard p-value formatting
    formatted <- ifelse(x < 0.001, "p < .001",
                        ifelse(x < 0.01, paste0("p = ", sprintf("%.3f", x)),
                               paste0("p = ", sprintf("%.2f", x))))
    # Remove leading zero
    formatted <- gsub("p = 0\\.", "p = .", formatted)
    return(formatted)

  } else if(type == "effect_size") {
    # Effect sizes to 2-3 decimal places
    return(sprintf("%.2f", x))

  } else if(type == "sample_size") {
    # Add commas for large sample sizes
    return(format(x, big.mark = ",", scientific = FALSE))

  } else if(type == "correlation") {
    # Correlations to 2 decimal places, no leading zero
    formatted <- sprintf("%.2f", x)
    formatted <- gsub("^0\\.", ".", formatted)
    formatted <- gsub("^-0\\.", "-.", formatted)
    return(formatted)

  } else {
    stop("Format type must be 'p_value', 'effect_size', 'sample_size', or 'correlation'")
  }
}

### ------------------------------------------------------------ ###
### TABLE-FIGURE COMBINATIONS

#' Create paired table and visualization
#'
#' Generates matching table and plot with consistent formatting
#'
#' @param data Data frame containing the data
#' @param table_vars Variables to include in summary table
#' @param plot_type Type of plot: "bar", "point", "line"
#' @param group_var Optional grouping variable
#' @param save_pair Whether to save both table and figure
#' @param pair_name Base name for saved files
#' @return List containing table and plot objects
#' @export
create_table_figure_pair <- function(data, table_vars, plot_type = "bar",
                                     group_var = NULL, save_pair = FALSE,
                                     pair_name = "table_figure_pair") {

  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for create_table_figure_pair()")
  }

  # Create summary table
  if(is.null(group_var)) {
    # Overall summary
    summary_table <- data.frame(
      Variable = table_vars,
      Mean = sapply(table_vars, function(var) {
        if(var %in% names(data) && is.numeric(data[[var]])) {
          round(mean(data[[var]], na.rm = TRUE), 2)
        } else {
          NA
        }
      }),
      SD = sapply(table_vars, function(var) {
        if(var %in% names(data) && is.numeric(data[[var]])) {
          round(sd(data[[var]], na.rm = TRUE), 2)
        } else {
          NA
        }
      }),
      N = sapply(table_vars, function(var) {
        if(var %in% names(data)) {
          sum(!is.na(data[[var]]))
        } else {
          NA
        }
      }),
      stringsAsFactors = FALSE
    )
  } else {
    # Grouped summary
    summary_table <- data.frame()
    groups <- unique(data[[group_var]][!is.na(data[[group_var]])])

    for(group in groups) {
      group_data <- data[data[[group_var]] == group & !is.na(data[[group_var]]), ]

      group_summary <- data.frame(
        Group = group,
        Variable = table_vars,
        Mean = sapply(table_vars, function(var) {
          if(var %in% names(group_data) && is.numeric(group_data[[var]])) {
            round(mean(group_data[[var]], na.rm = TRUE), 2)
          } else {
            NA
          }
        }),
        SD = sapply(table_vars, function(var) {
          if(var %in% names(group_data) && is.numeric(group_data[[var]])) {
            round(sd(group_data[[var]], na.rm = TRUE), 2)
          } else {
            NA
          }
        }),
        N = sapply(table_vars, function(var) {
          if(var %in% names(group_data)) {
            sum(!is.na(group_data[[var]]))
          } else {
            NA
          }
        }),
        stringsAsFactors = FALSE
      )

      summary_table <- rbind(summary_table, group_summary)
    }
  }

  # Create corresponding plot
  if(is.null(group_var)) {
    plot_data <- data.frame(
      Variable = table_vars,
      Value = sapply(table_vars, function(var) {
        if(var %in% names(data) && is.numeric(data[[var]])) {
          mean(data[[var]], na.rm = TRUE)
        } else {
          NA
        }
      }),
      stringsAsFactors = FALSE
    )

    if(plot_type == "bar") {
      plot_obj <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Variable, y = Value)) +
        ggplot2::geom_col(fill = academic_palette("categorical", 1)) +
        theme_academic() +
        ggplot2::labs(title = "Summary Statistics", x = "Variable", y = "Mean Value")
    }

  } else {
    # Grouped plot
    plot_data <- summary_table[!is.na(summary_table$Mean), ]

    if(plot_type == "bar") {
      plot_obj <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Variable, y = Mean, fill = Group)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::scale_fill_manual(values = academic_palette("categorical", length(unique(plot_data$Group)))) +
        theme_academic() +
        ggplot2::labs(title = "Summary Statistics by Group", x = "Variable", y = "Mean Value")
    }
  }

  # Save if requested
  if(save_pair) {
    # Save table
    proman_data <- read_proman()
    table_path <- file.path(proman_data$.paths$figures, paste0(pair_name, "_table.csv"))
    write.csv(summary_table, table_path, row.names = FALSE)

    # Save figure
    save_figure(plot_obj, paste0(pair_name, "_plot"), figure_type = "main",
                caption = "Visualization corresponding to summary table")

    message(paste("Table-figure pair saved:", pair_name))
  }

  return(list(table = summary_table, plot = plot_obj))
}

#' Format data frame for academic table presentation
#'
#' Applies academic formatting standards to tables
#'
#' @param data Data frame to format
#' @param caption Table caption
#' @param note Table note or footer
#' @param round_digits Number of decimal places for rounding
#' @return Formatted data frame
#' @export
format_academic_table <- function(data, caption = "", note = "", round_digits = 2) {

  # Round numeric columns
  numeric_cols <- sapply(data, is.numeric)
  data[numeric_cols] <- lapply(data[numeric_cols], round, round_digits)

  # Store caption and note as attributes
  attr(data, "caption") <- caption
  attr(data, "note") <- note

  message("Table formatted for academic presentation")
  if(nchar(caption) > 0) {
    cat("Caption:", caption, "\n")
  }
  if(nchar(note) > 0) {
    cat("Note:", note, "\n")
  }

  return(data)
}

### ------------------------------------------------------------ ###
### DOCUMENT INTEGRATION

#' Create figure summary document
#'
#' Generates HTML/Markdown document showing all figures with metadata
#'
#' @param project_code Project code for title
#' @param output_format Format: "html" or "markdown"
#' @return Path to created summary document
#' @export
create_figure_summary <- function(project_code = NULL, output_format = "html") {
  proman_data <- read_proman()

  # Use project_code from .proman if not provided
  if(is.null(project_code)) {
    project_code <- proman_data$project_code
  }

  registry_path <- file.path(proman_data$.paths$figures, "figure_registry.csv")


  if(!file.exists(registry_path)) {
    stop("No figures registered. Use save_figure() to create figures first.")
  }

  registry <- read.csv(registry_path, stringsAsFactors = FALSE)

  # Create document content
  if(is.null(project_code)) {
    project_code <- proman_data$project_code
  }

  content <- c()
  content <- c(content, paste("# Figure Summary:", project_code))
  content <- c(content, paste("Generated:", Sys.time()))
  content <- c(content, "")

  for(i in 1:nrow(registry)) {
    fig <- registry[i, ]

    content <- c(content, paste("##", fig$figure_name))
    content <- c(content, paste("**Type:**", fig$figure_type))
    content <- c(content, paste("**Files:**", fig$filename_base, ".", gsub(";", ", ", fig$formats)))
    content <- c(content, paste("**Size:**", fig$width, "x", fig$height, "inches"))
    content <- c(content, paste("**Modified:**", fig$modified_date))

    if(nchar(fig$caption) > 0) {
      content <- c(content, paste("**Caption:**", fig$caption))
    }

    if(nchar(fig$notes) > 0) {
      content <- c(content, paste("**Notes:**", fig$notes))
    }

    # Add image if HTML format
    if(output_format == "html") {
      png_file <- paste0(fig$filename_base, ".png")
      if(file.exists(file.path(proman_data$.paths$figures, png_file))) {
        content <- c(content, paste0("![", fig$figure_name, "](figures/", png_file, ")"))
      }
    }

    content <- c(content, "")
  }

  # Save document
  if(output_format == "html") {
    filename <- paste0("figure_summary_", format(Sys.Date(), "%Y%m%d"), ".md")
    doc_path <- file.path(proman_data$.paths$root, filename)
  } else {
    filename <- paste0("figure_summary_", format(Sys.Date(), "%Y%m%d"), ".md")
    doc_path <- file.path(proman_data$.paths$root, filename)
  }

  writeLines(content, doc_path)

  message(paste("Figure summary created:", filename))
  return(invisible(doc_path))
}

#' Export figure package for manuscript submission
#'
#' Bundles figures with submission-ready formatting and documentation
#'
#' @param figures Vector of figure names to include, or "all"
#' @param format Output format: "png", "tiff", "pdf"
#' @param dpi Resolution for raster formats
#' @param create_list Create figure list document
#' @return Path to export directory
#' @export
export_figure_package <- function(figures = "all", format = "png", dpi = 300, create_list = TRUE) {

  proman_data <- read_proman()
  registry_path <- file.path(proman_data$.paths$figures, "figure_registry.csv")

  if(!file.exists(registry_path)) {
    stop("No figures registered. Use save_figure() to create figures first.")
  }

  registry <- read.csv(registry_path, stringsAsFactors = FALSE)

  # Filter figures if specific ones requested
  if(!identical(figures, "all")) {
    registry <- registry[registry$figure_name %in% figures, ]
    if(nrow(registry) == 0) {
      stop("No matching figures found")
    }
  }

  # Create export directory
  export_dir <- file.path(proman_data$.paths$root, paste0("figures_for_submission_", format(Sys.Date(), "%Y%m%d")))
  if(!dir.exists(export_dir)) {
    dir.create(export_dir)
  }

  # Copy/convert figures
  figures_dir <- proman_data$.paths$figures
  copied_files <- character(nrow(registry))

  for(i in 1:nrow(registry)) {
    fig <- registry[i, ]

    # Find source file in requested format
    source_file <- file.path(figures_dir, paste0(fig$filename_base, ".", format))

    if(file.exists(source_file)) {
      # Copy file with submission-ready name
      dest_name <- paste0("Figure_", i, ".", format)
      dest_path <- file.path(export_dir, dest_name)
      file.copy(source_file, dest_path)
      copied_files[i] <- dest_name
    } else {
      warning(paste("Source file not found:", source_file))
      copied_files[i] <- NA
    }
  }

  # Create figure list document
  if(create_list) {
    figure_list <- data.frame(
      File_Name = copied_files,
      Figure_Name = registry$figure_name,
      Figure_Type = registry$figure_type,
      Caption = registry$caption,
      stringsAsFactors = FALSE
    )

    figure_list <- figure_list[!is.na(figure_list$File_Name), ]

    list_path <- file.path(export_dir, "figure_list.csv")
    write.csv(figure_list, list_path, row.names = FALSE)

    # Also create readable version
    readable_list <- c("FIGURE LIST FOR SUBMISSION", paste("Created:", Sys.time()), "")

    for(i in 1:nrow(figure_list)) {
      fig <- figure_list[i, ]
      readable_list <- c(readable_list,
                         paste("File:", fig$File_Name),
                         paste("Original Name:", fig$Figure_Name),
                         paste("Type:", fig$Figure_Type),
                         paste("Caption:", fig$Caption),
                         "")
    }

    writeLines(readable_list, file.path(export_dir, "figure_list.txt"))
  }

  message(paste("Figure package exported to:", basename(export_dir)))
  message(paste("Files exported:", sum(!is.na(copied_files))))

  return(invisible(export_dir))
}


#' Deregister figure(s) from registry
#'
#' Removes one or more figure entries from the registry and optionally deletes the files
#'
#' @param figure_names Character vector of figure name(s) to deregister
#' @param delete_files Whether to also delete the actual figure files (default: FALSE)
#' @param confirm Whether to ask for confirmation before deleting (default: TRUE in interactive sessions)
#' @return Invisible NULL
#' @export
deregister_figure <- function(figure_names, delete_files = FALSE, confirm = interactive()) {

  proman_data <- read_proman()
  registry_path <- file.path(proman_data$.paths$figures, "figure_registry.csv")

  if(!file.exists(registry_path)) {
    stop("No figure registry found.")
  }

  # Read registry
  registry <- read.csv(registry_path, stringsAsFactors = FALSE)

  # Ensure figure_names is a vector
  figure_names <- as.character(figure_names)

  # Find all figures
  indices <- which(registry$figure_name %in% figure_names)

  if(length(indices) == 0) {
    stop("No matching figures found in registry")
  }

  # Check which were found and which weren't
  found_names <- registry$figure_name[indices]
  not_found <- setdiff(figure_names, found_names)

  if(length(not_found) > 0) {
    warning("Not found in registry: ", paste(not_found, collapse = ", "))
  }

  # Collect all files that would be deleted
  all_files_to_delete <- character()

  for(idx in indices) {
    fig_info <- registry[idx, ]
    formats <- strsplit(fig_info$formats, ";")[[1]]
    files <- file.path(proman_data$.paths$figures, paste0(fig_info$filename_base, ".", formats))
    all_files_to_delete <- c(all_files_to_delete, files)
  }

  # Confirm deletion
  if(confirm && length(found_names) > 0) {
    cat("Will deregister", length(found_names), "figure(s):\n")
    for(name in found_names) {
      cat("  -", name, "\n")
    }

    if(delete_files) {
      existing_files <- all_files_to_delete[file.exists(all_files_to_delete)]
      if(length(existing_files) > 0) {
        cat("\nWill delete", length(existing_files), "file(s):\n")
        for(f in existing_files) {
          cat("  -", basename(f), "\n")
        }
      }
    }

    response <- readline("\nProceed? (y/n): ")
    if(tolower(substr(response, 1, 1)) != "y") {
      message("Cancelled")
      return(invisible(NULL))
    }
  }

  # Remove from registry
  registry <- registry[-indices, ]
  write.csv(registry, registry_path, row.names = FALSE)

  # Delete files if requested
  deleted_count <- 0
  if(delete_files) {
    for(f in all_files_to_delete) {
      if(file.exists(f)) {
        unlink(f)
        deleted_count <- deleted_count + 1
      }
    }
    if(deleted_count > 0) {
      message(paste("Deleted", deleted_count, "files"))
    }
  }

  message(paste("Deregistered", length(found_names), "figure(s)"))

  # Update .proman statistics
  proman_data$statistics$total_figures <- nrow(registry)
  proman_data$statistics$last_updated <- as.character(Sys.Date())
  write_proman(proman_data)


  invisible(NULL)
}



