#' @export

modulePortfolioPlotUI <- function(id) {
  tagList(
    introBox(plotOutput(NS(id, "Module_Portfolio_Plot")), data.step = 2,
             data.intro = "Here we will plot the running proift of the chosen study against the buy and hold stock position.")
  )
}

#' @export

modulePortfolioPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$Module_Portfolio_Plot <- shiny::renderPlot(
      ggplot() +
        geom_line(data = results, aes(x = entry_date, y = portfolio_profit)) +
        xlab("Entry Date") +
        ylab("Trade Profit") +
        theme_bw() +
        theme(axis.title = element_text(size = 14, face = "bold")),
      height = 600, width = "auto")
  })
}



# moduleGGPlotUI <- function(id) {
#   tagList(
#     shiny::htmlOutput(NS(id, "welcome_message")),
#     introBox(plotOutput(NS(id, "Module_GGPlot"), height = 650) %>%
#                withSpinner(color = "#0dc5c1"), data.step = 1, data.intro = "Here we will plot the
#                                  running profit of the chosen study against the buy and hold stock position."),
#     shiny::fluidRow(
#       shiny::column(width = 1, ""),
#       shiny::column(width = 5,
#                     shiny::selectInput(NS(id, "xvar"), "X-axis variable", axis_vars, selected = "entry_date")),
#       shiny::column(width = 6,
#                     shiny::selectInput(NS(id, "yvar"), "Y-axis variable", axis_vars, selected = "put_profit"))
#     )
#   )
# }

#' #' @export
#'
#' moduleGGPlotServer <- function(id) {
#'   moduleServer(id, function(input, output, session) {
#'     shiny::observe({
#'       xvar_name <- names(axis_vars)[axis_vars == input$xvar]
#'       yvar_name <- names(axis_vars)[axis_vars == input$yvar]
#'
#'       output$Module_GGPlot <- shiny::renderPlot(
#'         ggplot(results, aes_string(input$xvar, input$yvar)) +
#'           geom_point(aes(colour = put_profit > 0), alpha = 0.5, size = 1) +
#'           scale_fill_continuous(type = "viridis") +
#'           # scale_colour_manual(name = 'profit > 0', values = setNames(c('#00a65a', 'red'), c(TRUE, FALSE))) +
#'           xlab(xvar_name) +
#'           ylab(yvar_name) +
#'           theme_bw() +
#'           theme(axis.title = element_text(size = 14, face = "bold")),
#'         height = 600, width = "auto")
#'     })
#'   })
#' }