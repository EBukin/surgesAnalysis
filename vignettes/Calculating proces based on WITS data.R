# Prices analysis


library(plyr)
library(tidyverse)
library(stringr)

read_csv("data/Import_export_world/imp_exp_world_updt_21.csv") %>%
  select(Nomenclature, ProductCode, ProductDescription, ReporterISO3, Year,
         PartnerName, TradeFlowName, `TradeValue in 1000 USD`, Quantity, `NetWeight in KGM` ) %>%
  filter(!is.na(Quantity), TradeFlowName == "Import") %>% 
  mutate(priceUSD_MT = `TradeValue in 1000 USD` / Quantity * 10^6) %>% 
  gather(Var, Val, `TradeValue in 1000 USD` ,  Quantity, priceUSD_MT, `NetWeight in KGM` ) %>% 
  slice(c(987, 988))
  spread( Year, Val) %>% arrange(TradeFlowName, Var, ProductDescription) %>% 
  write_csv("vignettes/imp_exp_prices_upd_2.csv")

