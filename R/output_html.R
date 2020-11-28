#' Simple function to format HTML output to pass back to shiny
#'
#' @description{
#' output_HTML Called by the server.R script to send HTML output to screen
#' }
#'

output_HTML <- function(){
  num_t <- nrow(results)
  total_profit <- 100 * sum(results$put_profit)
  avg_prof_trade <- round(total_profit / num_t, digits = 2)
  percent_winners <- percent(length(which(results$put_profit > 0)) / num_t)
  maximum_loss <- 100 * ifelse(min(results$put_profit) >= 0, 0, min(results$put_profit))
  max_win <- 100 * ifelse(max(results$put_profit) < 0, 0, max(results$put_profit))

  output$total_profit <- renderUI({HTML(paste0("Total return: $", print_money(total_profit)))})
  output$avg_prof_trade <- renderUI({HTML(paste0("Average return/trade: $", print_money(avg_prof_trade)))})
  output$avg_days <- renderUI({HTML(paste0("Average # days held: ", round(avg_days, digits = 0)))})
  output$avg_prof_day <- renderUI({HTML(paste0("Average return/day: $", print_money(round(avg_prof_day, digits = 2))))})
  output$percent_winners <- renderUI({HTML(paste0("Percent winners: ", percent_winners))})
  output$exit.profit.target <- renderUI({HTML(paste0("Profit target: ", exit_profit_target))})
  output$exit.loss.limit <- renderUI({HTML(paste0("Loss limit: ", exit_loss_limit))})
  output$exit.expiration <- renderUI({HTML(paste0("Expiration: ", exit_expiration))})
  output$exit.gamma.risk <- renderUI({HTML(paste0("Gamma risk: ", exit_gamma_risk))})
  #output$exit.earnings <- renderUI({HTML(paste0("Earnings: ", exit.earnings))})
  output$max_loss <- renderUI({HTML(paste0("Max loss: $", print_money(maximum_loss)))})
  output$max_win <- renderUI({HTML(paste0("Max win: $", print_money(max_win)))})
  #output$avg.exit.roc <- renderUI({HTML(paste0("Avg Exit ROC: ", round(avg.exit.roc, digits = 2), "%"))})
  output$avg_entry_margin <- renderUI({HTML(paste0("Avg Entry Margin: $", print_money(round(avg_entry_margin, digits = 2))))})
}
