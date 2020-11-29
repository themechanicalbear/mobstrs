#' @export

moduleQuantmodPlotUI <- function(id) {
  tagList(
    plotOutput(NS(id, "Module_Quantmod_Plot"))
  )
}

#' @export

moduleQuantmodPlotServer <- function(id, stock) {
  moduleServer(id, function(input, output, session) {
    # Download data for a stock if needed, and return the data
    # require_symbol <- function(symbol, envir = parent.frame()) {
    #   if (is.null(envir[[symbol]])) {
    #     envir[[symbol]] <- quantmod::getSymbols(symbol, auto.assign = FALSE)
    #   }
    #   envir[[symbol]]
    # }

    # Create an environment for storing data
    # symbol_env <- new.env()

    make_chart <- function(symbol) {
      # symbol_data <- require_symbol(symbol, symbol_env)
      symbol_data <- quantmod::getSymbols(symbol, auto.assign = FALSE)

      quantmod::chartSeries(symbol_data,
                            name = symbol,
                            type = "auto",
                            TA = 'addBBands(); addVo(); addMACD()',
                            subset = '2018',
                            log.scale = FALSE,
                            theme = "white")
    }
    # output$Module_Quantmod_Plot <- renderPlot({make_chart(input$stock)})
    output$Module_Quantmod_Plot <- renderPlot({make_chart(stock)})
  })
}