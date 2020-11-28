

load_options_data <- function(stock) {
  data <- paste0(data_root, stock, ".RDS")
  assign(paste0(stock, "_options"),
         readRDS(data),
         envir = options_data)
}