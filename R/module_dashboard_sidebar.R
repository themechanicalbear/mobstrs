#' @export

moduleDashboardSidebarServer <- function(input, output, session) {
  return(reactive(input$host))
  return(reactive(input$stock))
  return(reactive(input$study))
  return(reactive(input$openOption))
  return(reactive(input$open_dte))
  return(reactive(input$call_delta))
  return(reactive(input$put_delta))
  return(reactive(input$second_dte))
  return(reactive(input$open_ivrank))
  return(reactive(input$min_roc))
  return(reactive(input$proftarg))
  return(reactive(input$loss_lim))
  return(reactive(input$l_loss_lim))
  return(reactive(input$gamma_days))
  return(reactive(input$goPlot))
}

#' @export

# Module UI function
moduleDashboardSidebarUI <- function(id, host, stock, study, openOption) {
  ns <- NS(id)

  tagList(
    # Shift the sidebar down to ensure the logo fits above it
    # Hide the default logout panel w/ dynamically-generated user panel
    tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    uiOutput("userpanel"),

    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Study_Module", tabName = "Study", icon = icon("cogs"), startExpanded = TRUE,
                               shiny::selectInput("host", "Host", host),
                               shiny::selectInput("stock", "Stock", "A"),
                               shiny::selectInput("study", "Study", c("Short Put", "Short Call", "Short Put Spread")),
                               shiny::selectInput("openOption", "Open on", c("First of Month", "High IV", "First of Week", "Daily"))),
      shinydashboard::menuItem("Entry Criteria", tabName = "Entry Criteria", icon = icon("cogs"),
                               shiny::sliderInput("open_dte", "DTE", 0, 90, 30, step = 5),
                               shiny::conditionalPanel(condition = ("input_study == 'Short Call'"),
                                                       shiny::sliderInput("call_delta", "Call delta", 0, 1, .16, step = .01)),
                               shiny::conditionalPanel(condition = ("input_study == 'Short Put' || input_study == 'Strangle'"),
                                                       shiny::sliderInput("put_delta", "Put delta", -1, 0, -.16, step = .01)),
                               shiny::conditionalPanel(condition = ("input_study == 'Call Calendar' || input_study == 'Poor Mans Cov Call'"),
                                                       shiny::sliderInput("second_dte", "Min short DTE", 0, 90, 30, step = 5)),
                               shiny::sliderInput("open_ivrank", "IV Rank", 0, 100, c(0, 100), step = 1),
                               shiny::sliderInput("min_roc", "Min ROC", 0, 50, 0, step = 1)),
      shinydashboard::menuItem("Exit Criteria", tabName = "Exit Criteria", icon = icon("cogs"),
                               shiny::sliderInput("proftarg", "Profit target %", 0, 100, 50, step = 5),
                               shiny::sliderInput("loss_lim", "Max loss x times credit received", 0, 10, 2, step = .25),
                               shiny::conditionalPanel(condition = ("input_study == 'Poor Mans Cov Call'"),
                                                       shiny::sliderInput("l_loss_lim", "Long max loss % debit paid", 10, 100, 50, step = 5)),
                               shiny::sliderInput("gamma_days", "Days prior to expiration", 0, 30, 0, step = 1)),
      shiny::actionButton('goPlot', 'Run Study', icon = icon("play-circle"))
    )
  )
}
