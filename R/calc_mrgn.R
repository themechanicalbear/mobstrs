#' Calculate initial margin on opening trades
#'
#' @description{
#' calc_mrgn calculates the opening margin required depending on strategy type
#' }
#'
#'
#'
#' @param strat String representing option strategy used in the study ie. Short Put
#' @param prc numeric stock price
#' @param spstrk numeric short put strike
#' @param lpstrk numeric long put strike
#' @param scstrk numeric short call strike
#' @param lcstrk numeric long call strike
#' @param spcred numeric short put credit
#' @param lpcred numeric long put credit
#' @param sccred numeric short call credit
#' @param lccred numeric long call credit
#' @param qty numeric quantity of option positions
#' @param shares numeric quantitiy of stock shares default 100
#'
#'
# Function arguments:-----------------------------------------------------------
#   strat = option strategy
#   prc = stock price
#   (sp, lp, sc, lc) + strk = short/long put/call strikes
#   (sp, lp, sc, lc) + cred = short/long put/call mid price
#   qty = lot size
#   shares = option share multiplier


# Function----------------------------------------------------------------------
# Opening margin
calc_mrgn <- function(strat = NULL, prc = NULL, spstrk = NULL, lpstrk = NULL,
                      scstrk = NULL, lcstrk = NULL, spcred = NULL, lpcred = NULL,
                      sccred = NULL, lccred = NULL, qty = 1, shares = 100) {
  if (strat == "short_put") {
    max(
      (.2 * prc * qty * shares) - ((prc - spstrk) * qty * shares) + (spcred * qty * shares),
      (.1 * spstrk * qty * shares ) + (qty * shares * spcred),
      (50 * qty) + (spcred * qty * shares)
    )
  }
}
