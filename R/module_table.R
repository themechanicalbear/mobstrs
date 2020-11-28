#' @export

moduleTableUI <- function(id) {
  tagList(
    introBox(shiny::downloadButton(NS(id, 'downloadData'), 'Download'), h4(" "), data.step = 4, data.intro = "Click this
                                 button to download the results of the study in .csv format."),
    introBox(shiny::dataTableOutput(NS(id, 'table')), data.step = 3, data.intro = "Here we will provide a table of the
                                 trade results for download."),
    column(12, DT::DTOutput(NS(id, 'Module_Table')))
  )
}

#' @export

moduleTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$Module_Table <- DT::renderDT(results_table,
                                    options = list(pageLength = 15,
                                                   lengthMenu = c(15, 25, nrow(results_table)),
                                                   scrollX = TRUE),
                                    server = FALSE)
  })
}

