# Shiny UI ----
# JavaScript ----
actionLink <- function(inputId, ...) {
  tags$a(href = 'javascript:void',
         id = inputId,
         class = 'action-button',
         ...)
}
useShinyjs()  # Setup shinyjs

# Dashboard ----
shinydashboard::dashboardPage(
  skin = global_shiny_skin_color,
  title = global_shiny_title,
  shinydashboard::dashboardHeader(
    title = a(href = global_website_url,
              img(src = global_logo, title = global_shiny_title, height = "100px"),
              style = "padding-top:10px; padding-bottom:10px;"),
    tags$li(class = "dropdown",
            actionButton("help", "Press for instructions")),
    tags$li(a(href = global_website_url,
              icon("cogs"),
              title = "Back to Apps Home"),
            class = "dropdown"),
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 100px}"),
            tags$style(".main-header .logo {height: 100px;}"),
            tags$style(".sidebar-toggle {height: 100px; padding-top: 1px !important;}"),
            tags$style(".navbar {min-height:100px !important}")),
    shinydashboard::dropdownMenuOutput("messageMenu"),
    shinydashboard::dropdownMenuOutput("notificationMenu"),
    shinydashboard::dropdownMenuOutput("taskMenu")
  ),
  # Sidebar ----
  sidebar <- shinydashboard::dashboardSidebar(
    # Shift the sidebar down to ensure the logo fits above it
    # Hide the default logout panel w/ dynamically-generated user panel
    tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    uiOutput("userpanel"),

    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Study", tabName = "Study", icon = icon("cogs"), startExpanded = TRUE,
                               shiny::selectInput("host", "Host", global_host_types),
                               shiny::selectInput("stock", "Stock", symbol_list),
                               shiny::selectInput("study", "Study", global_sidebar_study_types),
                               shiny::selectInput("openOption", "Open on", global_sidebar_openoptions)),
      shinydashboard::menuItem("Entry Criteria", tabName = "Entry Criteria", icon = icon("cogs"),
                               shiny::sliderInput("open_dte", "DTE", 0, 90, 30, step = 5),
                               shiny::conditionalPanel(
                                 condition = ("input_study == 'Short Call'"),
                                 shiny::sliderInput("call_delta", "Call delta", 0, 1, .16, step = .01)),
                               shiny::conditionalPanel(
                                 condition = ("input_study == 'Short Put' || input_study == 'Strangle'"),
                                 shiny::sliderInput("put_delta", "Put delta", -1, 0, -.16, step = .01)),
                               shiny::conditionalPanel(
                                 condition = ("input_study == 'Call Calendar' || input_study == 'Poor Mans Cov Call'"),
                                 shiny::sliderInput("second_dte", "Min short DTE", 0, 90, 30, step = 5)),
                               shiny::sliderInput("open_ivrank", "IV Rank", 0, 100, c(0, 100), step = 1),
                               shiny::sliderInput("min_roc", "Min ROC", 0, 50, 0, step = 1)),
      shinydashboard::menuItem("Exit Criteria", tabName = "Exit Criteria", icon = icon("cogs"),
                               shiny::sliderInput("proftarg", "Profit target %", 0, 100, 50, step = 5),
                               shiny::sliderInput("loss_lim", "Max loss x times credit received", 0, 10, 2, step = .25),
                               shiny::conditionalPanel(
                                 condition = ("input_study == 'Poor Mans Cov Call'"),
                                 shiny::sliderInput("l_loss_lim", "Long max loss % debit paid", 10, 100, 50, step = 5)),
                               shiny::sliderInput("gamma_days", "Days prior to expiration", 0, 30, 0, step = 1)),
      shiny::actionButton('goPlot', 'Run Study', icon = icon("play-circle"))
    )
  ),
  # Body ----
  body <- shinydashboard::dashboardBody(
    introjsUI(), # Setup help walkthrough
    shiny::fluidRow(
      shiny::column(width = 4,
                    tags$head(HTML("<script type='text/javascript' src='google-analytics.js'></script>")),
                    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                    shiny::htmlOutput("total_profit"),
                    shiny::htmlOutput("avg_prof_trade"),
                    h4("")),
      shiny::column(width = 4,
                    shiny::htmlOutput("n_trades"),
                    shiny::htmlOutput("percent_winners"),
                    shiny::htmlOutput("max_loss"),
                    shiny::htmlOutput("max_win")),
      shiny::column(width = 4,
                    shiny::htmlOutput("exit_profit_target"),
                    shiny::htmlOutput("exit_loss_limit"),
                    shiny::htmlOutput("exit_expiration"),
                    shiny::htmlOutput("exit_gamma_risk"))),
    shiny::fluidRow(
      tabBox(id = "tabset1", height = "800px", width = "1000px",
             shiny::tabPanel("Module_Table", moduleTableUI("table1")),
             shiny::tabPanel("Module_GGPlot", moduleGGPlotUI("ggplot_chart")),
             shiny::tabPanel("Module_Portfolio_Plot", modulePortfolioPlotUI("portfolio_plot")),
             shiny::tabPanel("Module_Quantmod_Plot", moduleQuantmodPlotUI("quantmod_plot"))
      )
    )
  )
)
