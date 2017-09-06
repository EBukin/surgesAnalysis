

# Import surges analysis for a policy note.
# This scrip contains only quantity surges analysis.

# Purpose:
#   To create data for the report on impotr surges.
#   Follow discussions and developments of the policy brief on import surges from before.

# Setup -------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(zoo)
# devtools::install_github("EBukin/tradeAnalysis-pack")
library(tradeAnalysis)

# Sourcing functions from R folder
sapply(file.path("R", list.files("R", '.R')), source, .GlobalEnv)

# Ad-hock function --------------------------------------------------------

# Function for reading data
read_data_file <- function(path) {
  df <-
    read_csv(path, col_types = cols(.default = col_character()))
  if (length(problems(df)) > 0) {
    rows <- problems(df)$row
    df <-
      df %>%
      filter(!row_number() %in% rows)
  }
  # Standardizing  df
  df <-
    df %>%
    # select(areacode, comcode, elecode, dplyr::matches("\\d{4}$")) %>%
    gather(Year, Value, dplyr::matches("\\d{4}$")) %>%
    mutate(
      Value = as.numeric(Value),
      Year = as.integer(str_sub(Year, 2, 5)),
      areacode = as.integer(areacode),
      comcode = as.integer(comcode),
      elecode = as.integer(elecode)
    ) %>%
    distinct() %>%
    arrange(areacode, comcode, elecode, Year) %>%
    filter(!is.na(Value))
}

# Function for adding lables in this assignment
add_labels <- function(x, areas = areasMT, coms = comsMT) {
  # browser()
  x <- 
    join_labels_generic(x, 
                      mappingTable = rename(areas, Code = areacode, Name = areaname), 
                      oldNames = c("areacode"), 
                      newNames = c("areaname"), 
                      keepCodes = TRUE)
  x <- 
    join_labels_generic(x, 
                        mappingTable = select(rename(coms, Code = comcode, Name = comname), Code, Name), 
                        oldNames = c("comcode"), 
                        newNames = c("comname"), 
                        keepCodes = TRUE)
  x
}

# Making years groups for aggregation
add_year_groups <-
  function(x,
           n_group = 10,
           startYear = NULL) {
    # browser()
    allYear <- unique(x$Year)
    if (!is_null(startYear)) {
      allYear <- allYear[allYear >= startYear]
    }
    YearGrops <-
      split(allYear, ceiling(seq_along(allYear) / n_group)) %>%
      map_df(.,
             function(y) {
               tibble(YearGrop = str_c(min(y), "/", str_sub(as.character(max(
                 y
               )), 3, 4)),
               Year = y)
             })
    x %>%
      left_join(YearGrops, "Year") %>%
      mutate(Year = YearGrop) %>%
      select(-YearGrop)
  }

# Add region function(
add_region <-
  function(data,
           RegionsType = "Income",
           mt = system.file("extdata", str_c(mtType, "_regions.csv"), package = "tradeAnalysis"),
           mtType = "ct") {
    
    # Maping table
    regionsMT <-
      read.csv(mt, stringsAsFactors = FALSE) %>% 
      tbl_df() %>% 
      select(Code, contains(RegionsType, ignore.case = TRUE), -contains("source"))
    names(regionsMT)[2] <- "Region"
    
    data %>% 
      left_join(regionsMT, by = c("Reporter.Code" = "Code"))
    
  }

# Loading data ------------------------------------------------------------

# Lisintg files
files <-
  file.path("data-raw/xCBS", dir("data-raw/xCBS", pattern = "\\.csv$"))

# Loading all data
loaded_df <-
  map_df(files, read_data_file) %>%
  distinct() # Cleaning duplicating rows if any

# Loading commodities elements mapping tabels which were previously created
com_ele_mt <-
  read_csv(
    "data-raw/mappings/filter_com_ele.csv",
    col_types = cols(
      comcode = col_integer(),
      comname = col_character(),
      elecode = col_integer(),
      elename = col_character()
    )
  ) %>%
  select(comcode, elecode)

# Loading areas/timespand mapping tabels downloaded from FAOSTAT
# To access it, go to the faostat website, any database: http://www.fao.org/faostat/en/#data/QA
# Click on Defiunitions and standards in the right middle page in the section "Liva anymal" (Where metadata is)
# The window "Definitions and standards - Live Animals" wil pop up.
# Clic on "Country/Region" and then on the download table (green round button).
areas_time_mt <-
  read_csv("data-raw/mappings/faostat_countries_time.csv") %>%
  select(`Country Code`, `M49 Code`, Country, `Start Year`, `End Year`) %>%
  arrange(`Country Code`) %>%
  rename(areacode = `Country Code`)

# Loading list of countries we are interested in:
# This countries are coming from a mapping table which is in the package tradeAnalysis
# We read this data directly from the package
mtPath <-
  system.file("extdata", str_c("fs", "_regions.csv"), package = "tradeAnalysis")
country_of_interest <-
  read_csv(mtPath) %>%
  filter(OldGroupOF103 == "oldGroup" |
           NewGroupOF92 == "newGroup") %>%
  .$Code %>%
  unique()

# Selected commodity list
com_list <- c(15, 56, 31, 236, 201, 204, 203, 207, 257, 1213)
#     comcode      comname elecode          elename
#       <int>        <chr>   <int>            <chr>
#   1      15        wheat      61             <NA>
#   2      56        maize      61             <NA>
#   3      31  milled rice      67             <NA>
#   4     236      soybean    6101 MY imports total
#   5     201  bovine meat      61          Imports
#   6     204   ovine meat      61          Imports
#   7     203      pigmeat      61          Imports
#   8     207 poultry meat      61          Imports
#   9     257     palm oil    6101 MY imports total
#   10    1213        sugar    1610    Imports total

# Main regions used in the analysis
allRegions <- c("LDC", "G33", "SVE", "RAM", "WTO", "newGroup", "oldGroup")

# Cleaning data for analysis ---------------------------------------------

filtered_df <-
  
  # Filtering only selected elements/commodities
  right_join(loaded_df, com_ele_mt, c("comcode", "elecode")) %>%
  distinct() %>%
  
  # Filtering out problematic countries
  # We filter them because those countries have problems in matching FAOSTAT
  #   names to the xCBS names.
  filter(!areacode %in% c(229, 248)) %>%
  
  # Cleaning countries names
  left_join(select(areas_time_mt, areacode, Country), "areacode") %>%
  mutate(areaname = Country) %>%
  select_( ~ -Country) %>%
  distinct() %>%
  
  # Cleaning countries by their time of existance
  left_join(select(areas_time_mt, areacode, `Start Year`, `End Year`),
            "areacode") %>%
  mutate(
    Value =
      ifelse(!is.na(`Start Year`) & Year < `Start Year`,
             NA,
             Value),
    Value =
      ifelse(!is.na(`End Year`) & Year > `End Year`,
             NA,
             Value)
  ) %>%
  
  # Filtering Years and values
  filter(!is.na(Value), Year <= 2016) %>%
  select_(~ -`Start Year`, ~ -`End Year`) %>%
  distinct() %>%
  
  # Spreading years with NA Values
  # slice(c(1173, 1174))
  spread(Year, Value) %>%
  gather(Year, Value, matches("\\d{4}")) %>%
  mutate(Year = as.integer(Year)) %>%
  select_( ~ areacode,
           ~ comcode,
           ~ elecode,
           ~ areaname,
           ~ comname,
           ~ elename,
           ~ eleunit,
           ~ Year,
           ~ Value) %>%
  
  # filtering only celected commodities
  filter(comcode %in% com_list)

# Mapping tables --------------------------------------------------------

# Separating country names mapping table
areasMT <-
  filtered_df %>%
  select(areacode, areaname) %>%
  distinct() %>%
  arrange(areaname)

# Separating commodity names mapping table
comsMT <-
  filtered_df %>%
  select(comname, comcode, elecode, elename, eleunit) %>%
  distinct() %>%
  arrange(comname)

# Final cleaning of a data frame ------------------------------------------

# Removing all irrelevand names from the data frame
filtered_df <-
  filtered_df %>%
  select(-areaname, -comname, -elename, -eleunit) %>%
  arrange(areacode, comcode, elecode, Year)

# Analyzing data availability ---------------------------------------------

filtered_df %>% 
  filter(!is.na(Value),
         areacode %in% country_of_interest,
         Year >= 1992) %>% 
  group_by(areacode, Year) %>% 
  count() %>% 
  ungroup() %>% 
  spread(Year, n) %>% 
  add_labels() %>% 
  select(areacode, areaname, everything()) %>% 
  write_excel_csv("output/quantityDataAvailability.csv")

# Functions for calculations --------------------------------------------

# sources from the  R folder at the beggining of the script

# Initializing rules ------------------------------------------------------

# rollMean + % threshold
rule_mean_ad <- function(adRange = c(1.3)) {
  # Rules for deviation from mean
  plyr::llply(adRange,
              function(ma_adj) {
                # Rule name
                rollRuleName <- stringr::str_c("mean*", ma_adj)
                
                # Rule threshold value
                rollThreshold <-
                  lazyeval::interp(
                    ~  y * z,
                    y = quote(rollMean),
                    z = ma_adj
                  )
                
                list(name = rollRuleName,
                     threshold = rollThreshold)
              })
}

# Rule for the mean + standard deviation
rule_mean_sd <- function(adRange = c(1)) {
  # Rules for deviation from mean
  plyr::llply(adRange,
              function(sd_adj) {
                # Rule name
                rollRuleName <- stringr::str_c("mean+SD*", sd_adj)
                
                # Rule threshold value
                rollThreshold <-
                  lazyeval::interp(
                    ~ y + z * t,
                    y = quote(rollMean),
                    z = quote(rollSE),
                    t = sd_adj)
                
                list(name = rollRuleName,
                     threshold = rollThreshold)
              })
}

# rule_mean_sd(seq(1,3,0.01))

# Establishing rules to apply
rules <-
  list(rule_mean_ad(seq(1, 3, 0.01))) %>%
  unlist(., FALSE)

# Applying rules to data and summarising it  -------------------------------

# Calculating all rolling statistics
calcs <-
  filtered_df %>%
  group_by_(.dots = names(filtered_df)[!names(filtered_df) %in% c("Year", "Value")]) %>%
  arrange_( ~ areacode, ~ comcode, ~ elecode, ~ Year) %>% 
  calc_roll_stats(.) %>% 
  ungroup()

# write_rds(calcs, "output/calcs.rds", compress = 'gz')

# Applying rules to an example
aplliedRules <- map_df(rules, ~apply_rule(calcs, .x)) 

# Calculating number of transactions, where there could have happend a surge
totalTransaction <- 
  aplliedRules %>% 
  filter(!is.na(Surge)) %>% 
  select(areacode, comcode, elecode, Rule, Year) %>% 
  mutate(transaction = 1,
         Partner.Code = NA) %>% 
  rename(Reporter.Code = areacode) 
  
# Aggregating total number of surges
calcsSummary <- 
  aplliedRules %>% 
  group_by_(.dots = names(.)[!names(.) %in% c("Value", "rollMean", "rollSE", "Threshold", "Surge")]) %>% 
  summarise(Surge = sum(Surge, na.rm = TRUE)) %>% 
  mutate(Partner.Code = NA) %>% 
  rename(Reporter.Code = areacode) %>% 
  ungroup()

# Aggregating total number of import surges for a commodity/region for regions
# Aggregating total number of import surges per country. 
# Joining this parameters together
allRegionsData <-
  map_df(allRegions,
         function(x) {
           theRegion = x
           agg_reporters(
             calcsSummary,
             RegionsType = theRegion,
             valCol = "Surge",
             mtType = "fs"
           ) %>%
             left_join(
               agg_reporters(
                 totalTransaction,
                 RegionsType = theRegion,
                 valCol = "transaction",
                 mtType = "fs"
               ),
               by = c(
                 "Reporter.Code",
                 "comcode",
                 "elecode",
                 "Year",
                 "Rule",
                 "Partner.Code"
               )
             ) %>%
             filter(!is.na(transaction))
         }) %>%
  rename(areacode = Reporter.Code) %>%
  select(-Partner.Code) %>%
  filter(!is.na(areacode),
         areacode %in% allRegions)

# Based on the number of existing transactions, where a surge could potentiall occur.

############################
############################
############################
allRegionsData %>% 
  group_by(areacode, Rule) %>% 
  summarise(Surge = sum(Surge, na.rm = TRUE),
            transaction = sum(transaction, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(subrule = str_sub(Rule, end = 5),
         Rule = as.numeric(str_sub(Rule, 6))) %>% 
  ggplot(aes(Rule, Surge/transaction, group = areacode, fill = areacode, colour = areacode)) +
  geom_line() +
  ggtitle(label = "All commodities, different groups") +
  xlab("Moving averag * ")
############################

allRegionsData %>% 
  add_labels() %>% 
  group_by(areacode, comname, Rule) %>% 
  summarise(Surge = sum(Surge, na.rm = TRUE),
            transaction = sum(transaction, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Rule = as.numeric(str_sub(Rule, 6))) %>% 
  ggplot(aes(Rule, Surge/transaction, group = areacode, fill = areacode, colour = areacode)) +
  geom_line() +
  facet_wrap(~comname) +
  ggtitle(label = "By commodities, different groups") +
  xlab("Moving averag * ")


############################
############################
############################



te <-
  allRegionsData %>% 
  filter(!is.na(Year)) %>% 
  # add_year_groups(., 5, 1992) %>% 
  # group_by_(.dots = names(.)[!names(.) %in% c("Value", "rollMean", "rollSE", "Threshold", "Surge", "transaction")]) %>%
  # summarise(Surge = sum(Surge, na.rm = TRUE),
  #           transaction = sum(transaction, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(Year)



############################
############################
############################

library(ggplot2)
te %>% 
add_labels() %>% 
ggplot( aes(Year, Surge/transaction, group = areacode, fill = areacode, colour = areacode)) +
  geom_line() +
  facet_wrap(~comname)
  

# write_rds(appliedRules, "output/appliedRules.rds", compress = 'gz')

# Data export and visualisation -------------------------------------------



# Group Years -------------------------------------------------------------

# Sammarising year grous
calcsSummary <-
  appliedRules %>% 
  add_year_groups(., 5, 1992) %>% 
  group_by_(.dots = names(.)[!names(.) %in% c("Value", "rollMean", "rollSE", "Threshold", "Surge")]) %>% 
  summarise(Surge = sum(Surge, na.rm = TRUE)) %>% 
  mutate(Partner.Code = NA) %>% 
  rename(Reporter.Code = areacode) %>% 
  ungroup()

# Data by regions
regional_surges <- 
  bind_rows(
    agg_regions(calcsSummary, RegionsType = "LDC", valCol = "Surge", mtType = "fs"),
    agg_regions(calcsSummary, RegionsType = "G33", valCol = "Surge", mtType = "fs"),
    agg_regions(calcsSummary, RegionsType = "SVE", valCol = "Surge", mtType = "fs"),
    agg_regions(calcsSummary, RegionsType = "RAM", valCol = "Surge", mtType = "fs"),
    # agg_regions(calcsSummary, RegionsType = "Income", valCol = "Surge", mtType = "fs"),  
    # agg_regions(calcsSummary, RegionsType = "Development", valCol = "Surge", mtType = "fs"),  
    agg_regions(calcsSummary, RegionsType = "WTO", valCol = "Surge", mtType = "fs"),  
    agg_regions(calcsSummary, RegionsType = "NewGroup", valCol = "Surge", mtType = "fs"),  
    agg_regions(calcsSummary, RegionsType = "OldGroup", valCol = "Surge", mtType = "fs")
  ) %>% 
  rename(areacode = Reporter.Code) %>% 
  select(-Partner.Code) %>% 
  distinct() %>% 
  filter(!is.na(areacode))


# Do not group years ------------------------------------------------------

# Sammarising year grous
calcsUngrouped <-
  calcs %>%
  # add_year_groups(., 5, 1992) %>%
  group_by_(.dots = names(.)[!names(.) %in% c("Value", "rollMean", "rollSE", "Threshold", "Surge")]) %>%
  summarise(Surge = sum(Surge, na.rm = TRUE)) %>%
  mutate(Partner.Code = NA) %>%
  rename(Reporter.Code = areacode) %>%
  ungroup()

# Data by regions
regional_surges_by_year <- 
  bind_rows(
    agg_regions(calcsUngrouped, RegionsType = "LDC", valCol = "Surge", mtType = "fs"),
    agg_regions(calcsUngrouped, RegionsType = "G33", valCol = "Surge", mtType = "fs"),
    agg_regions(calcsUngrouped, RegionsType = "SVE", valCol = "Surge", mtType = "fs"),
    agg_regions(calcsUngrouped, RegionsType = "RAM", valCol = "Surge", mtType = "fs"),
    agg_regions(calcsUngrouped, RegionsType = "Income", valCol = "Surge", mtType = "fs"),
    # agg_regions(calcsUngrouped, RegionsType = "Development", valCol = "Surge", mtType = "fs"),  
    # agg_regions(calcsUngrouped, RegionsType = "WTO", valCol = "Surge", mtType = "fs"),  
    agg_regions(calcsUngrouped, RegionsType = "NewGroup", valCol = "Surge", mtType = "fs"),  
    agg_regions(calcsUngrouped, RegionsType = "OldGroup", valCol = "Surge", mtType = "fs")
  ) %>% 
  rename(areacode = Reporter.Code) %>% 
  # select(-Partner.Code) %>% 
  distinct() %>% 
  filter(!is.na(areacode))




# Visualisation -----------------------------------------------------------
# 
# calcs %>% 
#   gather(SurgeVar, SurgeVal, Value, rollMean, rollSE, Threshold, Surge) %>% 
#   filter(!is.na(SurgeVal),
#          Rule %in% c("mean+SD*1" )) %>%
#   rpivotTable(.)

# Writing the result ------------------------------------------------------
calcs %>% 
  gather(SurgeVar, SurgeVal, Value, rollMean, rollSE, Threshold, Surge) %>% 
  filter(areacode %in% c(country_of_interest)) %>% 
  # filter(#!is.na(SurgeVal),
  #        Rule %in% c("mean*1.3", "mean+SD*1" )) %>%
  write_csv(file.path("output", str_c("surges_by_country_year_", Sys.Date(), ".csv")), na = "")



write_csv(calcs, file.path("output", str_c("surges_calculations_by_year_", Sys.Date(), ".csv")), na = "")
write_csv(calcsSummary, file.path("output", str_c("surges_countries_by_periods_", Sys.Date(), ".csv")))
write_csv(regional_surges, file.path("output", str_c("surges_regions_by_periods_", Sys.Date(), ".csv")))
write_csv(regional_surges_by_year, file.path("output", str_c("surges_regions_by_years_", Sys.Date(), ".csv")))



