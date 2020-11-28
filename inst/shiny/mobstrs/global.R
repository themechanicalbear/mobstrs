# Global setup ----
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("shiny", "shinythemes", "shinydashboard", "shinyjs",
                    "ggvis", "rbokeh", "ggplot2", "scales", "shinyBS",
                    "shinycssloaders", "rintrojs", "dplyr", "here", "DT",
                    "mobstrs")
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

global_athena_db <- Sys.getenv("AWS_ATHENA_DATABASE_SHINY_APP")
global_athena_tbl <- Sys.getenv("AWS_ATHENA_TABLE_SHINY_APP")
global_main_shiny_color <- Sys.getenv("SHINY_APP_MAIN_COLOR")
global_shiny_app_debug_mode <- as.logical(Sys.getenv("SHINY_APP_DEBUG_MODE"))
global_shiny_title <- "The Mechanical Bear"
global_shiny_skin_color <- "green" # Must be one of (blue, black, purple, green, red, yellow)
global_website_url <- "http://www.themechanicalbear.com"
global_logo <- 'logo2.png'

global_host_types <- c("athena", "local", "git")
global_sidebar_study_types <- c("Short Put", "Short Call", "Short Put Spread")
global_sidebar_openoptions <- c("First of Month", "High IV", "First of Week", "Daily")