# Global objects

# export_latex_variable function ------------------------------------------
# Create function to add or update a variable and value to latex_vars.txt file
export_latex_variable <- function(variable, value, path = latex_vars_path) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  
  # If file missing, create headers
  if (!file.exists(path)) {
    write.table(
      data.frame(variable = character(), value = character(),
                 stringsAsFactors = FALSE),
      file = path, sep = ",", row.names = FALSE, quote = TRUE
    )
  }
  
  # Read current data
  df <- tryCatch(
    read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE, 
               quote = "\""),
    error = function(e) data.frame(variable = character(), value = character(), 
                                   stringsAsFactors = FALSE)
  )
  
  variable <- as.character(variable)
  value    <- as.character(value)
  
  status <- NULL
  
  # Update or append
  if (variable %in% df$variable) {
    df$value[df$variable == variable] <- value
    status <- "updated"
  } else {
    df <- rbind(df, data.frame(variable = variable, value = value, 
                               stringsAsFactors = FALSE))
    status <- "added"
  }
  
  # Remove accidental duplicates (keep last)
  if (any(duplicated(df$variable))) {
    df <- df[!duplicated(df$variable, fromLast = TRUE), , drop = FALSE]
  }
  
  # Write back
  write.table(df, file = path, sep = ",", row.names = FALSE, quote = TRUE)
  
  # Inform when new variable was added/ updated
  message(sprintf('✅ Variable `%s` = `%s` %s in %s', variable, value, status, 
                  latex_vars_filename))
  
  invisible(df)
}

# latex_vars.txt ----------------------------------------------------------
# Create a latex_vars.txt file that contains variables and their values which
# can be referenced in latex. 
{ # Create latex_vars.txt
  dir.create("02_Data/data_processed", recursive = TRUE, showWarnings = FALSE)
  latex_vars_filename <- "latex_vars.txt"
  
  latex_vars_path <- file.path("data", "data_processed", 
                               "LaTeX data values", latex_vars_filename)
  
  if (!file.exists(latex_vars_path)) {
    init_df <- data.frame(
      variable = character(),
      value    = character(),
      stringsAsFactors = FALSE
    )
    write.table(
      init_df,
      file = latex_vars_path,
      sep = ",", # use comma separator
      row.names = FALSE,
      quote = TRUE # keep quotes in case values contain commas/spaces
    )
  }
}

# Plotting ----------------------------------------------------------------
FONTSIZE_SMALL <- 10
FONTSIZE_MED <- 12
FONTSIZE_LARGE <- 14
FONTSIZE_CAPTION <- 16
FONTSIZE_LATEX_TABLE <- "footnotesize"
FONT_FAMILY <- "Times"

plt_theme_2 <- theme(
  text = element_text(family = FONT_FAMILY),
  plot.title = element_text(face = "plain", size = FONTSIZE_LARGE),
  axis.title = element_text(face = "plain", size = FONTSIZE_LARGE),
  axis.text = element_text(size = FONTSIZE_SMALL),
  legend.direction = "vertical", 
  legend.position = "right", 
  legend.title = element_text(face = "plain", size = FONTSIZE_LARGE),
  legend.text = element_text(size = FONTSIZE_MED), 
  strip.text = element_text(size = FONTSIZE_LARGE)
)
