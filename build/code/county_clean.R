##bringing in the county data, cleaning it

library(tidyverse)
library(haven)

setwd("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/inputs")

counties <- read_csv("county_demos.csv") %>%
  data.frame() %>%
  select(COUNTY, STATE, AGEGRP, YEAR, TOT_POP, BAC_MALE, BAC_FEMALE,
         H_MALE, H_FEMALE, IAC_MALE, IAC_FEMALE, AAC_MALE, AAC_FEMALE, 
         NAC_MALE, NAC_FEMALE) %>%
  rename("countyfips" = COUNTY, 
         "statefip" = STATE, 
         "pop" = TOT_POP) %>%
  filter(YEAR == 3, 
         AGEGRP == 0) %>%
  ##calculating racial populations "in combination"
  ##codebook here: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2017/cc-est2017-alldata.pdf
  mutate(pop_black = BAC_MALE + BAC_FEMALE, 
         pop_hisp = H_MALE + H_FEMALE, 
         pop_pct_black = (BAC_MALE + BAC_FEMALE) / pop, 
         pop_pct_hisp = (H_MALE + H_FEMALE) / pop) %>%
  select(countyfips, statefip, pop, pop_black, pop_hisp, pop_pct_black, pop_pct_hisp)
