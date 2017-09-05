#' Calculate rolling statistics
#' @param x data frame to use
#' @param roll_n number of periods used for the rolling statistics
calc_roll_stats <- function(x, roll_n = 3) {
  require(zoo)
  x %>%
    mutate(
      rollMean = zoo::rollapply(
        Value,
        width = roll_n,
        FUN = function(x) {
          mean(x)
        },
        fill = NA,
        align = "right"
      )) %>% 
    mutate(rollMean = lag(rollMean)) %>%
    mutate(
      rollSE = zoo::rollapplyr(
        Value,
        width = roll_n,
        FUN = function(x) {
          sqrt(sum((x - mean(x)) ^ 2) / roll_n)
        },
        fill = NA,
        align = "right"
      )
    ) %>% 
    mutate(rollSE = lag(rollSE)) 
}

