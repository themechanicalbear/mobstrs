# https://www.cboe.com/micro/vix/vixwhite.pdf

# The VIX Index measures 30-day expected volatility of the S&P 500 Index.
# The components of the VIX Index are near- and next-term put and call options with more than 23 days and
# less than 37 days to expiration.
# These include SPX options with “standard” 3rd Friday expiration dates and “weekly” SPX options that expire every Friday,
# except the 3rd Friday of each month.
# Once each week, the SPX options used to calculate the VIX Index “roll” to new contract maturities.
# For example, on the second Tuesday in October, the VIX Index would be calculated using SPX options expiring 24 days later
# (i.e., “near- term”) and 31 days later (i.e., “next-term”).
# On the following day, the SPX options that expire in 30 calendar days would become the “near-term” options and
# SPX options that expire in 37 calendar days would be the “next-term” options.
# In this hypothetical example, the near-term options are “standard” SPX options with 25 days to expiration,
# the next-term options are P.M.-settled SPX Weeklys with 32 days to expiration;
# and the calculation reflects prices observed at 9:46 a.m. Chicago time.
# For the purpose of calculating time to expiration, “standard” SPX options are deemed to expire at the open of
# trading on SPX settlement day - the third Friday of the month , and “weekly” SPX options are deemed to expire
# at the close of trading (i.e., 3:00 p.m. CT).
# The VIX Index calculation measures time to expiration, T, in calendar days and divides each day into minutes
# in order to replicate the precision that is commonly used by professional option and volatility traders.
# The time to expiration is given by the following expression:
#
# T = { MCurrent day + MSettlement day + MOther days } / Minutes in a year
#


# The risk-free interest rates, R1 and R2, are yields based on U.S. Treasury yield curve rates
# (commonly referred to as “Constant Maturity Treasury” rates or CMTs), to which a cubic spline is applied to
# derive yields on the expiration dates of relevant SPX options. As such, the VIX Index calculation may use
# different risk-free interest rates for near- and next-term options. In this example, assume that R1 = 0.0305%
# for the near-term options and that R2 = 0.0286% for the next-term options.
# Note in this example, T2 uses a value of 900 for MSettlement day, which reflects the 3:00 p.m. expiration time
# of the next-term SPX Weeklys options. Since many of the interim calculations are repetitive,
# only representative samples appear below.
# The complete set of SPX option data and calculations may be found in Appendix 1.

# Step 1: Select the options to be used in the VIX Index calculation
# The selected options are out-of-the-money SPX calls and out-of-the-money SPX puts centered around an at-the-money
# strike price, K0. Only SPX options quoted with non-zero bid prices are used in the VIX Index calculation.
# One important note: as volatility rises and falls, the strike price range of options with non-zero bids tends to
# expand and contract. As a result, the number of options used in the VIX Index calculation may vary from
# month-to-month, day-to-day and possibly, even minute-to-minute.

# For each contract month:

# Determine the forward SPX level, F, by identifying the strike price at which the absolute difference between the
# call and put prices is smallest. The call and put prices in the following table reflect the average of each
# option’s bid / ask quotation. As shown below, the difference between the call and put prices is smallest at
# the 1965 strike for the near- and the 1960 strike for the next-term options.

# Using the 1965 call and put in the near-term, and the 1960 call and put in the next-term contract applied
# to the formula:

# F = Strike Price + eRT x (Call Price - Put Price)
# the forward index prices, F1 and F2, for the near- and next-term options, respectively, are:

# F1 = 1965 + e(0.000305 x 0.0683486) x (21.05 - 23.15) = 1962.89996 F2 = 1960 + e(0.000286 x 0.0882686) x (27.30 - 24.90) = 1962.40006

# Next, determine K0 - the strike price immediately below the forward index level,
# F - for the near- and next-term options. In this example, K0,1 = 1960 and K0,2 = 1960.
# Select out-of-the-money put options with strike prices < K0. Start with the put strike immediately lower than K0
# and move to successively lower strike prices. Exclude any put option that has a bid price equal to zero (i.e., no bid).
# As shown below, once two puts with consecutive strike prices are found to have zero bid prices, no puts with lower
# strikes are considered for inclusion.
# (Note that the 1350 and 1355 put options are not included despite having non-zero bid prices.)

# Next, select out-of-the-money call options with strike prices > K0.
# Start with the call strike immediately higher than K0 and move to successively higher strike prices,
# excluding call options that have a bid price of zero. As with the puts, once two consecutive call options are found
# to have zero bid prices, no calls with higher strikes are considered.
# (Note that the 2225 call option is not included despite having a non-zero bid price.)

# Finally, select both the put and call with strike price K0. Notice that two options are selected at K0,
# while a single option, either a put or a call, is used for every other strike price.
# The following table contains the options used to calculate the VIX Index in this example.
# The VIX Index uses the average of quoted bid and ask, or mid-quote, prices for each option selected.
# The K0 put and call prices are averaged to produce a single value.
# The price used for the 1960 strike in the near-term is, therefore, (24.25 + 21.30)/2 = 22.775;
# and the price used in the next- term is (27.30 + 24.90)/2 = 26.10.


# Step 2: Calculate volatility for both near-term and next-term options
# Applying the VIX formula (1) to the near-term and next-term options with time to expiration of T1 and T2, respectively, yields:

# The VIX Index is an amalgam of the information reflected in the prices of all of the selected options.
# The contribution of a single option to the VIX value is proportional to ΔK and the price of that option,
# and inversely proportional to the square of the option’s strike price.
# Generally, ΔKi is half the difference between the strike prices on either side of Ki.
# For example, the ΔK for the next-term 1325 Put is 37.5: ΔK1325 Put = (1350 – 1275)/2.
# At the upper and lower edges of any given strip of options, ΔKi is simply
# the difference between Ki and the adjacent strike price. In this example,
# the 1370 Put is the lowest strike in the strip of near- term options and 1375 is the adjacent strike.
# Therefore, ΔK1370 Put = 5 (i.e., 1375 – 1370).

# The contribution of the near-term 1370 Put is given by:

# A similar calculation is performed for each option.
# The resulting values for the near-term options are then summed and multiplied by 2/T1.
# Likewise, the resulting values for the next-term options are summed and multiplied by 2/T2.
# The table below summarizes the results for each strip of options.

# Step 3
# Calculate the 30-day weighted average of σ21 and σ22.
# Then take the square root of that value and multiply by 100 to get the VIX value.

# The inclusion of SPX Weeklys in the VIX Index calculation means that the near-term options will always
# have more than 23 days to expiration and the next-term options always have less than 37 days to expiration,
# so the resulting VIX value will always reflect an interpolation of σ21 and σ22 ;
# i.e., each individual weight is less than or equal to 1 and the sum of the weights equals 1.

# Returning to the example...
# NT1 = number of minutes to settlement of the near-term options (35,924)
# NT2 = number of minutes to settlement of the next-term options (46,394)
# N30 = number of minutes in 30 days (30 x 1,440 = 43,200)
# N365 = number of minutes in a 365-day year (365 x 1,440 = 525,600)







library(dplyr)

implied_vol <- function(symbol) {
  #symbol <- "AMD"
  min_day <- 1440
  min_month <- min_day * 30
  min_year <- 525600

  option_data <- readRDS(paste0(here::here(), "/data/options/", symbol, ".RDS"))
  rates <- risk_free_rate()

  option_data <- option_data %>%
    dplyr::mutate(mid = (ask + bid) / 2)

  puts <- option_data %>%
    dplyr::filter(type == "put") %>%
    dplyr::mutate(put_mid_price = mid,
                  put_bid = bid) %>%
    dplyr::select(c(symbol, quotedate, close, expiration, strike, dte, exp_type, put_mid_price, put_bid))

  calls <- option_data %>%
    dplyr::filter(type == "call") %>%
    dplyr::mutate(call_mid_price = mid,
                  call_bid = bid) %>%
    dplyr::select(c(symbol, quotedate, close, expiration, strike, dte, exp_type, call_mid_price, call_bid))

  option_data <- puts %>%
    dplyr::left_join(calls, by = Reduce(intersect, list(names(puts), names(calls))))

  option_data <- option_data %>%
    dplyr::filter(dte > 23,
                  ((strike <= close & put_bid > 0) | (strike >= close & call_bid > 0))) %>%
    dplyr::group_by(quotedate) %>%
    dplyr::mutate(dte_rank = dense_rank(dte)) %>%
    dplyr::filter(dte_rank == 1 | dte_rank == 2) %>%
    dplyr::mutate(dte_near = min(dte),
                  dte_next = max(dte)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(rates, by = c("quotedate" = "date"))

  summary_output <- data.frame()

  vol_calc <- function(dt) {
    sub_data <- filter(option_data, quotedate == dt)
    if (nrow(sub_data) > 0) {
      r0 <- dplyr::filter(rates, date == dt)

      F_level_near <- sub_data %>%
        dplyr::filter(dte == dte_near) %>%
        dplyr::mutate(p_c_abs = abs(call_mid_price - put_mid_price)) %>%
        dplyr::filter(p_c_abs == min(p_c_abs, na.rm = TRUE))

      F_level_next <- sub_data %>%
        dplyr::filter(dte == dte_next) %>%
        dplyr::mutate(p_c_abs = abs(call_mid_price - put_mid_price)) %>%
        dplyr::filter(p_c_abs == min(p_c_abs, na.rm = TRUE))

      if (nrow(F_level_near) > 0 & nrow(F_level_next) > 0 & nrow(r0) > 0) {
        sub_data <- sub_data %>%
          dplyr::filter(dplyr::row_number() == 1) %>%
          dplyr::mutate(r1 = r0[[1, 3]] / 100,
                        r2 = r0[[1, 5]] / 100,
                        t1 = (dte_near * min_day) / min_year,
                        t2 = (dte_next * min_day) / min_year,
                        k1 = F_level_near[[1, "strike"]],
                        m1 = F_level_near[[1, "call_mid_price"]] - F_level_near[[1, "put_mid_price"]],
                        f1 = k1 + exp(r1 * t1) * m1,
                        k2 = F_level_next[[1, "strike"]],
                        m2 = F_level_next[[1, "call_mid_price"]] - F_level_next[[1, "put_mid_price"]],
                        f2 = k2 + exp(r2 * t2) * m2) %>%
          dplyr::select(symbol, quotedate, r1, r2, t1, t2, k1, k2, m1, m2, f1, f2)

        summary_output <- rbind(summary_output, sub_data)
        assign("summary_output", summary_output, envir = .GlobalEnv)
      }
    }
  }

  system.time(purrr::map(unique(option_data$quotedate), vol_calc))

  # option_data <- option_data %>%
  #   dplyr::mutate(strike_diff = close - strike) %>%
  #   dplyr::group_by(quotedate, expiration) %>%
  #   dplyr::top_n(-4, abs(strike_diff)) %>%
  #   dplyr::ungroup()

  option_data <- dplyr::left_join(option_data, summary_output, by = c("symbol", "quotedate"))

  option_data <- option_data %>%
    dplyr::filter(complete.cases(.))

  option_data <- option_data %>%
    dplyr::group_by(quotedate, dte) %>%
    dplyr::arrange(strike) %>%
    dplyr::mutate(strike_gap = dplyr::lead(strike, 1) - strike) %>%
    dplyr::mutate(strike_gap = ifelse(is.na(strike_gap), strike - dplyr::lag(strike, 1), strike_gap)) %>%
    dplyr::ungroup()

  option_data <- option_data %>%
    dplyr::mutate(near_strike_contrib =
                    ifelse(dte == dte_near & strike < f1,
                           (strike_gap/(strike ^ 2)) * exp(r1 * t1) * put_mid_price,
                           ifelse(dte == dte_near & strike > f1,
                                  (strike_gap/(strike ^ 2)) * exp(r1 * t1) * call_mid_price,
                                  ifelse(dte == dte_near & strike == f1,
                                         (strike_gap/(strike ^ 2)) * exp(r1 * t1) * ((call_mid_price + put_mid_price) / 2),
                                         0)))) %>%
    dplyr::mutate(next_strike_contrib =
                    ifelse(dte == dte_next & strike < f2,
                           (strike_gap/(strike ^ 2)) * exp(r2 * t2) * put_mid_price,
                           ifelse(dte == dte_next & strike > f2,
                                  (strike_gap/(strike ^ 2)) * exp(r2 * t2) * call_mid_price,
                                  ifelse(dte == dte_next & strike == f2,
                                         (strike_gap/(strike ^ 2)) * exp(r2 * t2) * ((call_mid_price + put_mid_price) / 2),
                                         0))))

  option_data <- option_data %>%
    dplyr::mutate(q1 = (1/t1) * ((f1/k1) - 1) ^ 2,
                  q2 = (1/t2) * ((f2/k2) - 1) ^ 2) %>%
    dplyr::group_by(quotedate) %>%
    dplyr::mutate(near_tot = (2 / t1) * sum(near_strike_contrib),
                  next_tot = (2 / t2) * sum(next_strike_contrib)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sigma1 = near_tot - q1,
                  sigma2 = next_tot - q2) %>%
    dplyr::mutate(IV =
                    100 * sqrt(
                      (t1 * sigma1 * (((min_day * dte_next) - min_month) / ((min_day * dte_next) - (min_day * dte_near))) +
                         t2 * sigma2 * ((min_month - (min_day * dte_near)) / ((min_day * dte_next) - (min_day * dte_near)))) *
                        (min_year / min_month))
    )

  IV_data <- option_data %>%
    dplyr::distinct(quotedate, close, IV)
}

