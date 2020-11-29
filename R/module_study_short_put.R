#' @export

moduleStudyShortPutUI <- function(id) {
  tagList(
  )
}

#' @export

moduleStudyShortPutServer <- function(id, study, stock, open_dte) {
  moduleServer(id, function(input, output, session) {
    study_params <- tolower(paste0(input$study, "_", input$stock, "_",
                                   input$open_dte))
    study_params <- gsub("-", "", study_params)
    study_params <- gsub(" ", "", study_params)

    assign("study_params", study_params, envir = .GlobalEnv)

    athena <- mobstr::athena_connect(global_athena_db)
    tbl_list <- DBI::dbListTables(athena)

    # if (study_params %in% tbl_list) {
    #   results <- athena %>%
    #     dplyr::tbl(study_params) %>%
    #     dplyr::collect()
    #
    #   assign("results", results, envir = .GlobalEnv)
    #   assign("results_table", results, envir = .GlobalEnv)
    # }

    # else {
    opened_puts <- mobstr::open_leg(conn = athena,
                                    table = global_athena_tbl,
                                    stock = stock,
                                    put_call = "put",
                                    direction = "short",
                                    tar_delta = p_delta,
                                    tar_dte = o_dte) %>%
      dplyr::mutate(quotedate = as.Date(quotedate, "%Y-%m-%d"),
                    expiration = as.Date(expiration, "%Y-%m-%d")) %>%
      dplyr::filter(quotedate %in% first_day$date)

    assign("opened_puts", opened_puts, envir = open_trades)

    sub_options <- athena %>%
      dplyr::tbl(global_athena_tbl) %>%
      dplyr::filter(symbol == stock) %>%
      dplyr::filter(quotedate == expiration,
                    strike %in% !!opened_puts$put_strike) %>%
      dplyr::collect()

    results <- purrr::pmap_dfr(list(df = list(sub_options),
                                    entry_date = opened_puts$quotedate,
                                    exp = opened_puts$expiration,
                                    typ = list("put"),
                                    stk = opened_puts$put_strike,
                                    entry_mid = opened_puts$put_open_short,
                                    entry_delta = opened_puts$delta,
                                    stk_price = opened_puts$close,
                                    direction = list("short")),
                               mobstr::close_leg) %>%
      arrange(entry_date) %>%
      mutate(profit = put_profit * 100) %>%
      mutate(portfolio_profit = cumsum(profit))

    # assign_global(results, results)
    assign("results", results, envir = .GlobalEnv)
    assign("results_table", results, envir = .GlobalEnv)

    # aws.s3::s3write_using(results, FUN = readr::write_csv,
    #               bucket = paste0("mechanicalbear-athena", "/", study_params),
    #               object = paste0(study_params, ".csv"))

    # Write to Athena
    # athena <- mobstr::athena_connect(global_athena_db)

    # mobstr::athena_load(conn = athena,
    #             database = global_athena_db,
    #             s3_bucket = "mechanicalbear-athena",
    #             name = study_params,
    #             df = results)
    #}
  })
}