#' \code{mobstrs} package
#'
#' Shiny mobstrs R app
#'
#' See the README on
#'
#' @docType package
#' @name mobstrs
#' @import dplyr
#' @import rlang
#' @import TTR
#' @import rintrojs
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect
#' @importFrom stats complete.cases
#' @importFrom shiny HTML renderUI
#' @importFrom readr write_csv

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("stock", "results", "options_data", "tot_profit",
                           "num_t", "tot_days", "avg_profit", "avg_days",
                           "percent", "close_trades", "quotedate", "type",
                           "strike", "expiration", "bid", "ask", "spread",
                           "trade_open_price", "symbol", "dte", "trade_open",
                           "trade_close", "trade_close_price", "spread_perc",
                           "input", "par", "plot", "results_table2",
                           "trade_results", ".", "stock_move", "profit",
                           "days_held", "mid", "desc", "lead", "lead_1",
                           "lead_2", "wk_4_coupon", "wk_12_coupon",
                           "put_mid_price", "call_mid_price", "near_dte",
                           "near_dte_diff", "dense_rank", "dte_rank",
                           "open_trades", "points", "wk_13_coupon",
                           "next_dte", "k1", "r1", "t1", "m1", "k2", "r2",
                           "t2", "m2", "f1", "f2", "strike_gap", "f1", "r1",
                           "t1", "f2", "r2", "t2", "k1", "k2",
                           "near_strike_contrib", "next_strike_contrib",
                           "near_tot", "q1", "next_tot", "q2", "sigma1",
                           "next_dte", "sigma2", "IV", "iv_rank_252",
                           "exp_type", "put_bid", "call_bid", "dte_near",
                           "p_c_abs", "dte_nex", "dte_next", "first_day",
                           "o_dte", "delta", "c_delta", "m_dte", "m_delta",
                           "avg_prof_day", "percent_winners",
                           "exit_profit_target", "exit_loss_limit",
                           "exit_expiration", "exit_gamma_risk", "maximum_loss",
                           "max_win", "avg_entry_margin", "p_delta",
                           "data_root", "session"))
}
