# symbols <- data.frame(symbols = c("AMD", "SPY", "T", "IWM", "GLD", "QQQ", "DIA", "AAPL", "NFLX", "TLT", "XLE",
#                                   "EEM", "MA", "FB", "FXI", "SLV", "EFA", "EWZ", "USO", "FXE", "TBT", "GDX",
#                                   "IBM", "C", "BA", "GILD", "XOP", "GS", "JPM", "CRM", "V", "VRX", "FSLR",
#                                   "IYR", "CAT", "CF", "MSFT", "HYG", "CELG", "XRT", "XLY", "APC", "UNP", "SLB",
#                                   "CVX", "EOG"), stringsAsFactors = FALSE)
#
# results <- data.frame()
#
# ma_cross <- function(data) {
#   data <- data %>%
#     dplyr::distinct(symbol, quotedate, close) %>%
#     dplyr::arrange(quotedate) %>%
#     dplyr::mutate(ma_20 = TTR::SMA(close, n = 20),
#                   ma_50 = TTR::SMA(close, n = 50)) %>%
#     dplyr::mutate(ma_20_lag = lag(ma_20, 1),
#                   ma_50_lag = lag(ma_50, 1)) %>%
#     dplyr::filter(complete.cases(.))
#
#   assign("options_data_ma", data, envir = .GlobalEnv)
# }
#
# ma_open <- function(data) {
#   data <- data %>%
#     dplyr::filter(ma_20 > ma_50,
#                   ma_20_lag < ma_50_lag)
#
#   assign("ma_open_data", data, envir = .GlobalEnv)
# }
#
# ma_close <- function(date, open) {
#   data <- options_data_ma %>%
#     dplyr::filter(quotedate > date) %>%
#     dplyr::filter(ma_20 < ma_50) %>%
#     dplyr::arrange(quotedate) %>%
#     dplyr::filter(dplyr::row_number() == 1) %>%
#     dplyr::mutate(open_date = as.Date(date, origin = "1970-01-01"),
#                   open_price = open,
#                   days_open = quotedate - open_date,
#                   profit = (close - open_price) * (10000 / open_price))
#   results <- rbind(results, data)
#   assign("results", results, envir = .GlobalEnv)
# }
#
# ma_analysis <- function(symbol) {
#   gc()
#   options_data <- readRDS(paste0(here::here(),"/data/options/", symbol, ".RDS"))
#   ma_cross(options_data)
#   ma_open(options_data_ma)
#   purrr::pmap(list(ma_open_data$quotedate, ma_open_data$close), ma_close)
# }
#
# profvis::profvis(purrr::map(symbols$symbols, ma_analysis))
#
# results <- results %>%
#   dplyr::group_by(symbol) %>%
#   dplyr::mutate(total_profit = sum(profit),
#                 avg_profit = mean(profit),
#                 avg_days = mean(days_open),
#                 max_loss = ifelse(min(profit) < 0, min(profit), 0),
#                 max_profit = ifelse(max(profit) > 0, max(profit), 0)) %>%
#   dplyr::ungroup()
