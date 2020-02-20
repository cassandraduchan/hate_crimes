library(haven)
library(readxl)
library(tidyverse)

county_dems <- read_delim("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/inputs/stco-mr2010_al_mo.csv", delim = ",")

demo <- county_dems %>%
  select(STATE, COUNTY, SEX, ORIGIN, AGEGRP, IMPRACE, RESPOP) %>%
  rename("statefip" = STATE, 
         "countyfips" = COUNTY) %>%
  mutate(race = case_when(IMPRACE == 1 ~ "White",
                          IMPRACE == 2 ~ "Black",
                          IMPRACE == 3 ~ "Native", 
                          IMPRACE == 4 | IMPRACE == 5 ~ "Asian/Pacific Islander", 
                          IMPRACE >= 6 ~ "Mixed Race"), 
         gender = case_when(SEX == 1 ~ "Man", 
                            SEX == 2 ~ "Woman"), 
         hisp = case_when(ORIGIN == 1)) %>%
  
  group_by(statefip, countyfips, race)
  