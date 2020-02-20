setwd("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/inputs")

rm(list = ls())
gc()

source("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/code/county_clean.R")
source("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/code/acs_clean.R")
source("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/code/hc_clean.R")

clean_acs <- inner_join(clean_acs, counties, by = c("statefip", "countyfips"), copy = FALSE)

#anti-black crimes per year
analysis_black_year <- left_join(clean_acs, black_crime_year, by = c("statefip", "countyfips", "year"), copy = FALSE)

analysis_black_year <- analysis_black_year %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born,
         english, above_ba, employed, black, black_anc_only, black_anc_other, black_anc_none,
         weight, anti_black_any, anti_black_sum, black_crime_1000,
         black_crime_blackcap, black_crime_black1000, pop_black, pop_pct_black) %>%
  replace_na(list(anti_black_any = 0, anti_black_sum = 0, black_crime_1000 = 0,
                  black_crime_blackcap = 0, black_crime_black1000 = 0))

##anti-black crimes total
analysis_black_total <- left_join(clean_acs, black_crime_total, by = c("statefip", "countyfips"), copy = FALSE)

analysis_black_total <- analysis_black_total %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born,
         english, above_ba, employed, black, black_anc_only, black_anc_other, black_anc_none,
         weight, pop_black, pop_pct_black, anti_black_total, black_crime_cap, black_crime_1000,
         black_crime_blackcap, black_crime_black1000) %>%
  replace_na(list(anti_black_total = 0, black_crime_cap = 0, black_crime_1000 = 0,
                  black_crime_blackcap = 0, black_crime_black1000 = 0))

##violent anti-black crimes per year
analysis_black_violent_year <- left_join(clean_acs, black_violent_crime_year, by = c("statefip", "countyfips", "year"), copy = FALSE)

analysis_black_violent_year <- analysis_black_violent_year %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born,
         english, above_ba, employed, black, black_anc_only, black_anc_other, black_anc_none,
         weight, pop_black, pop_pct_black, anti_black_violent_mean, black_violent_crime_1000,
         black_violent_crime_blackcap, black_violent_crime_black1000, anti_black_violent_sum,
         anti_black_violent_any) %>%
  replace_na(list(anti_black_violent_mean = 0, black_violent_crime_1000 = 0, black_violent_crime_blackcap = 0,
                  black_violent_crime_black1000 = 0, anti_black_violent_sum = 0))

##violent anti-black crimes total
analysis_black_violent_crime_total <- left_join(clean_acs, black_violent_crime_total, by = c("statefip", "countyfips"), copy = FALSE)

analysis_black_violent_crime_total <- analysis_black_violent_crime_total %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born,
         english, above_ba, employed, black, black_anc_only, black_anc_other, black_anc_none,
         weight, pop_black, pop_pct_black, anti_black_violent_total, black_violent_crime_cap,
         black_violent_crime_1000, black_violent_crime_blackcap, black_violent_crime_black1000) %>%
  replace_na(list(anti_black_violent_total = 0, black_violent_crime_cap = 0, black_violent_crime_1000 = 0,
                  black_violent_crime_blackcap = 0, black_violent_crime_black1000 = 0))

# ##anti-hisp crimes per year
analysis_hisp_year <- left_join(clean_acs, hisp_crime_year, by = c("statefip", "countyfips", "year"), copy = FALSE)

analysis_hisp_year <- analysis_hisp_year %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born,
         english, above_ba, employed, hisp, hisp_anc_only, hisp_anc_other, hisp_anc_none,
         weight, anti_hisp_any, anti_hisp_sum, hisp_crime_1000,
         hisp_crime_hispcap, hisp_crime_hisp1000, pop_hisp, pop_pct_hisp) %>%
  replace_na(list(anti_hisp_any = 0, anti_hisp_sum = 0, hisp_crime_1000 = 0,
                  hisp_crime_hispcap = 0, hisp_crime_hisp1000 = 0))

##anti-hisp crimes total
analysis_hisp_total <- left_join(clean_acs, hisp_crime_total, by = c("statefip", "countyfips"), copy = FALSE)

analysis_hisp_total <- analysis_hisp_total %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born,
         english, above_ba, employed, hisp, hisp_anc_only, hisp_anc_other, hisp_anc_none,
         weight, pop_hisp, pop_pct_hisp, anti_hisp_total, anti_hisp_mean, hisp_crime_cap,
         hisp_crime_1000, hisp_crime_hispcap, hisp_crime_hisp1000) %>%
  replace_na(list(anti_hisp_any = 0, anti_hisp_sum = 0, anti_hisp_mean = 0, hisp_crime_1000 = 0,
                  hisp_crime_hispcap = 0, hisp_crime_hisp1000 = 0))

##violent anti-hisp crimes per year
analysis_hisp_violent_year <- left_join(clean_acs, hisp_violent_crime_year, by = c("statefip", "countyfips", "year"), copy = FALSE)

analysis_hisp_violent_year <- analysis_hisp_violent_year %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born,
         english, above_ba, employed, hisp, hisp_anc_only, hisp_anc_other, hisp_anc_none,
         weight, pop_hisp, pop_pct_hisp, anti_hisp_violent_mean, hisp_violent_crime_1000,
         hisp_violent_crime_hispcap, hisp_violent_crime_hisp1000, anti_hisp_violent_sum,
         anti_hisp_violent_any) %>%
  replace_na(list(anti_hisp_violent_mean = 0, hisp_violent_crime_1000 = 0, hisp_violent_crime_hispcap = 0,
                  hisp_violent_crime_hisp1000 = 0, anti_hisp_violent_sum = 0))

##violent anti-hisp crimes total
analysis_hisp_violent_crime_total <- left_join(clean_acs, hisp_violent_crime_total, by = c("statefip", "countyfips"), copy = FALSE)

analysis_hisp_violent_crime_total <- analysis_hisp_violent_crime_total %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born,
         english, above_ba, employed, hisp, hisp_anc_only, hisp_anc_other, hisp_anc_none,
         weight, pop_hisp, pop_pct_hisp, anti_hisp_violent_total, hisp_violent_crime_cap,
         hisp_violent_crime_1000, hisp_violent_crime_hispcap, hisp_violent_crime_hisp1000) %>%
  replace_na(list(anti_hisp_violent_total = 0, hisp_violent_crime_cap = 0, hisp_violent_crime_1000 = 0,
                  hisp_violent_crime_hispcap = 0, hisp_violent_crime_hisp1000 = 0))