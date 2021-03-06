# Shiny Server ----
shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp) # Stops the shiny server when the browser window is closed
  shiny::shinyOptions(progress.style = "old")
  main_color <- global_main_shiny_color
  print(global_shiny_app_debug_mode)
  options(shiny.trace = global_shiny_app_debug_mode) # Debug to the console when TRUE set via environment variable
  # The following two lines can be inserted to box in code section for profiling
  # Rprof("boot.out")
  # Rprof(NULL)
  # options(shiny.error = recover)

  # initiate hints on startup with custom button and event
  rintrojs::hintjs(session, options = list("hintButtonLabel" = "Hope this hint was helpful"),
                   events = list("onhintclose" = I('alert("Wasn\'t that hint helpful")')))

  shiny::observeEvent(input$help, {
    environment(show_help) <- environment()
    show_help()
  })

  output$userpanel <- shiny::renderUI({
    # session$user is non-NULL only in authenticated sessions
    #if (!is.null(session$user)) {
    shinydashboard::sidebarUserPanel(
      span("Logged in as ", session$user),
      subtitle = a(icon("sign-out"), "Logout", href = "__logout__"))
    #}
  })

  # Help ----
  output$notificationMenu <- shinydashboard::renderMenu({
    shinydashboard::notificationItem(text = "Need help?", icon = shiny::icon("user"),
                                     status = "info", href = "mailto:jason@themechanicalbear.com")
  })

  data.frame(x_rng = c(as.Date("2012-01-03"), as.Date("2021-01-03")),
             y_rng = c(-100, 100)) %>%
    ggvis(~x_rng, ~y_rng) %>%
    set_options(width = "auto",
                height = 600) %>%
    layer_points(fillOpacity := 0) %>%
    add_axis("x", title = "Trade Open Date") %>%
    add_axis("y", title = "Profit") %>%
    bind_shiny("ggvis_trades")

  # Welcome message ----
  output$welcome_message <- shiny::renderUI({
    HTML("<p style=\"text-align: center; font-size: 60px; color: #FFFFFF;\">.</p>
         <p style=\"text-align: center; font-size: 20px;\">
         <strong>Welcome:</strong> Please fill out the study inputs in the left sidebar</p>
         <p style=\"text-align: center; font-size: 20px;\">When ready click&nbsp;
         <strong>Run Study</strong>&nbsp;to see the results</p>")
  })

  moduleDashboardSidebarServer("shiny_sidebar")

  observe({
    if (input$host == "local") {
      data_root <- here::here("data/options/")
      symbol_list <- gsub(".RDS", "", list.files(here("data/options/")))
      assign("symbol_list", symbol_list, envir = .GlobalEnv)
      assign("data_root", data_root, envir = .GlobalEnv)
      updateSelectizeInput(session, 'stock', choices = symbol_list)
    }
    if (input$host == "git") {
      data_root <- here::here("data/git_options/")
      symbol_list <- gsub(".RDS", "", list.files(here("data/git_options/")))
      assign("symbol_list", symbol_list, envir = .GlobalEnv)
      assign("data_root", data_root, envir = .GlobalEnv)
      updateSelectizeInput(session, 'stock', choices = symbol_list)
    }
    if (input$host == "athena") {
      athena <- mobstr::athena_connect(global_athena_db)
      symbol_list <- athena %>%
        dplyr::tbl(global_athena_tbl) %>%
        dplyr::distinct(symbol) %>%
        dplyr::arrange(symbol) %>%
        dplyr::collect()
      print(symbol_list)
      assign("symbol_list", symbol_list, envir = .GlobalEnv)
      updateSelectizeInput(session, 'stock', choices = symbol_list)
    }
  })

  # Loading image ----
  shiny::observeEvent(input$goPlot, {
    # Reset the results data.frame when inputs are changed
    if (exists("results", envir = .GlobalEnv) &&
        is.data.frame(get("results"))) {
      rm(results, envir = .GlobalEnv)
    }

    # Determine customer's choice for frequency and set
    assign("openOption", input$openOption, envir = .GlobalEnv)
    if (openOption == "First of Month") {
      # monthly <- readRDS(here("data/monthly.RDS"))
      assign("first_day", mobstrs:::monthly, envir = .GlobalEnv)
      assign("inc.amount", .004)
    }
    else if (openOption == "High IV") {
      monthly <- readRDS(here("data/monthly_high_iv.RDS"))
      assign("first_day", monthly, envir = .GlobalEnv)
      assign("inc.amount", .004)
    }
    else if (openOption == "First of Week") {
      data(list = "mondays")
      assign("first_day", mondays, envir = .GlobalEnv)
      assign("inc.amount", .001)
    }
    else if (openOption == "Daily") {
      data(list = "open_daily")
      assign("first_day", open_daily, envir = .GlobalEnv)
      assign("inc.amount", .0002)
    }
    shiny::withProgress(message = "Progress", detail = "Setting up study", value = .05, {
      t <- 0 # Set inital trade number to zero
      progress.int <- inc.amount # Set progress bar increment amount

      # Values defined by the customer in shiny ui
      assign("study", input$study, envir = .GlobalEnv)
      assign("stock", input$stock, envir = .GlobalEnv)
      assign("low_iv", input$open_ivrank[1], envir = .GlobalEnv)
      assign("high_iv", input$open_ivrank[2], envir = .GlobalEnv)
      assign("o_dte", input$open_dte, envir = .GlobalEnv)
      assign("s_dte", input$second_dte, envir = .GlobalEnv)
      assign("c_delta", input$call_delta, envir = .GlobalEnv)
      assign("p_delta", input$put_delta, envir = .GlobalEnv)
      assign("prof_targ", input$proftarg / 100, envir = .GlobalEnv)
      assign("loss_lim", input$loss_lim + 1, envir = .GlobalEnv)
      assign("l_loss_lim", input$l_loss_lim / 100, envir = .GlobalEnv)
      assign("g", input$gamma_days, envir = .GlobalEnv)
      assign("earn_close", input$earn_close, envir = .GlobalEnv)
      assign("min_roc", input$min_roc, envir = .GlobalEnv)
      assign("p_delta_lim", p_delta + .1, envir = .GlobalEnv)
      assign("c_delta_lim", c_delta - .1, envir = .GlobalEnv)
    })

    # Run function for study selected ----
    if (study == "Call Calendar") {call_calendar(progress.int, t)}
    if (study == "Poor Mans Cov Call") {pmcc(progress.int, t)}
    if (study == "Short Call") {short_call(progress.int, t)}
    if (study == "Short Put") {moduleStudyShortPutServer("ShortPutStudy", input$study, input$stock, input$open_dte)}
    if (study == "Short Put Spread") {short_put_spread(progress.int, t)}
    if (study == "Long Stock") {LongStock(progress.int, t)}
    if (study == "Strangle") {strangle(progress.int, data_set, t)}
    if (study == "Straddle") {straddle(progress.int, t)}
    if (study == "Strangle Daily Close") {strangle_daily_close(progress.int, t)}

    # HTML output ----
    output$welcome_message <- shiny::renderUI({
      HTML("")
    })
    output$n_trades <- shiny::renderUI({
      str_num_trades <- paste0("Number of trades in ", toupper(input$stock), ": ", nrow(results))
      HTML(str_num_trades)
    })

    environment(output_HTML) <- environment()
    output_HTML()
    moduleTableServer("table1")
    moduleGGPlotServer("ggplot_chart")
    modulePortfolioPlotServer("portfolio_plot")
    moduleQuantmodPlotServer("quantmod_plot", input$stock)

    # Download table
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0(stock, '_', study, '_results.csv')},
      content = function(file) {write.csv(results_table, file)}
    )

    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]

    strategy_min_max <- reactive({
      strategy_min_max <- results %>%
        dplyr::filter(strategy_portfolio == max(strategy_portfolio) |
                        strategy_portfolio == min(strategy_portfolio)) %>%
        dplyr::group_by(strategy_portfolio) %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::ungroup()
    })
  })
})
