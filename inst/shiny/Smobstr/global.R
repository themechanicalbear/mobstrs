# Global setup ----
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("shiny", "shinythemes", "shinydashboard", "shinyjs",
                    "ggvis", "rbokeh", "ggplot2", "scales", "shinyBS",
                    "shinycssloaders", "rintrojs", "dplyr", "here", "DT",
                    "Smobstr")
  invisible(lapply(library_list, require, character.only = TRUE))
})))

# Variables that can be put on the axis
axis_vars <- c(
  "Open Date" = "entry_date",
  "Days Held" = "days_held",
  "IV Rank" = "open_ivrank",
  "Open ROC" = "open_roc",
  "Put Profit" = "put_profit",
  "RSI" = "open_rsi",
  "Year" = "year")

symbol_list <- ("")

open_trades <- new.env(hash = TRUE, size = NA)
options_data <- new.env(hash = TRUE, size = NA)
close_trades <- new.env(hash = TRUE, size = NA)
trade_results <- new.env(hash = TRUE, size = NA)
