#' Function for applying rules
#' @param x data frame to use
#' @param rule rune to apply
apply_rule <-
  function(x, rule) {
    # browser()
    name <- lazyeval::interp(~str_c(text), text = rule$name)
    rule_threshold <- rule$threshold
    x %>%
      mutate_(.dots = setNames(list(rule_threshold), "Threshold")) %>%
      mutate(Surge = ifelse(Value > Threshold, 1, 0)) %>%
      mutate_(.dots = setNames(list(name), "Rule")) %>% 
      ungroup
  }