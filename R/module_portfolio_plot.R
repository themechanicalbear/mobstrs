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