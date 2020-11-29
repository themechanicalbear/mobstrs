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
    moduleDashboardSidebarUI(id = "shiny_sidebar", host = global_host_types, stock = symbol_list,
                                      study = global_sidebar_study_types,
                                      openOption = global_sidebar_openoptions)
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
