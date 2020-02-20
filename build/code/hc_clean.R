##bringing in the hate crime data, cleaning it

##work
setwd("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/code")
# setwd("Y:/_Research/_Cassandra Duchan/research/hate_crimes/build/code")
library(tidyverse)
library(haven)

setwd("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/inputs")
# setwd("Y:/_Research/_Cassandra Duchan/research/hate_crimes/build/inputs")

##loading in the micro hate crime data 
	hc_2005 <- read_csv("hatecrimes_2005_micro.csv")
	hc_2006 <- read_csv("hatecrimes_2006_micro.csv")
	hc_2007 <- read_csv("hatecrimes_2007_micro.csv")
	hc_2008 <- read_csv("hatecrimes_2008_micro.csv")
	hc_2009 <- read_csv("hatecrimes_2009_micro.csv")
	hc_2010 <- read_csv("hatecrimes_2010_micro.csv")
	hc_2011 <- read_csv("hatecrimes_2011_micro.csv")
	hc_2012 <- read_csv("hatecrimes_2012_micro.csv")
	hc_2013 <- read_csv("hatecrimes_2013_micro.csv")
	hc_2014 <- read_csv("hatecrimes_2014_micro.csv")
	hc_2015 <- read_csv("hatecrimes_2015_micro.csv")
	hc_2016 <- read_csv("hatecrimes_2016_micro.csv")

	allhc <- rbind(hc_2005, hc_2006, hc_2007, hc_2008, hc_2009, hc_2010, hc_2012, hc_2011, hc_2013, hc_2014, hc_2015, hc_2016)
	rm(hc_2005, hc_2006, hc_2007, hc_2008, hc_2009, hc_2010, hc_2011, hc_2012, hc_2013, hc_2014, hc_2015, hc_2016)

##cleaning

hc <- allhc %>% 
  select(STATECOD, CFIPS1, OFFCOD1, BIASMO1, NUMVTM1, LOCCOD1, MASTERYR) %>%
  rename("state_abbrev" = STATECOD,
         "countyfips" = CFIPS1, 
         "offns_type" = OFFCOD1, 
         "type" = BIASMO1, 
         "vctms_num1" = NUMVTM1, 
         "offns_lctn1" = LOCCOD1, 
         "year" = MASTERYR) 

# allhc$countyfips <- as.integer(allhc$countyfips)

##creating new variables for analysis
hc <- hc %>%
  ##bias type
  mutate(anti_racial = if_else(type >= 11 & type <= 16 | type >= 31 & type <= 33, 1, 0, missing = NULL), 
         anti_minority = if_else(type >= 12 & type <= 16 | type == 31 | type == 32, 1, 0, missing = NULL), 
         anti_black = if_else(type == 12, 1, 0, missing = NULL), 
         anti_hisp = if_else(type == 32, 1, 0, missing = NULL), 
         anti_native = if_else(type == 13, 1, 0, missing = NULL), 
         anti_apa = if_else(type == 14 | type == 16, 1, 0, missing = NULL)) %>%
  ##offense type 
  mutate(hc_violent = if_else(offns_type == "09A" | offns_type == "09B" | offns_type == "09C" |
                              offns_type == "100" | offns_type == "11A" | offns_type == "11B" |
                              offns_type == "11C" | offns_type == "11D" | offns_type == "120" |
                              offns_type == "13A" | offns_type == "13B" | offns_type == "13C" |
                              offns_type == "200" | offns_type == "36A" | offns_type == "36B" |
                              offns_type == "64A" | offns_type == "64B", 1, 0, missing = NULL)) %>%
  mutate(hc_type = case_when(type == 12 ~ "Anti-Black", 
                             type == 32 ~ "Anti-Hispanic",
                             type == 14 ~ "Anti-Asian",
                             type == 16 ~ "Anti-Native Hawaiian or Other Pacific Islander",
                             type == 13 ~ "Anti-American Indian or Alaska Native",
                             type == 15 ~ "Anti-Multi-Racial",
                             type == 11 ~ "Anti-White", 
                             TRUE ~ "Other*")) 

##attach to county names for map and to get state FIPS code
county_name <- read_csv("countyfips.csv")
hc$countyfips <- as.integer(hc$countyfips)
hc <- left_join(hc, county_name, by = c("state_abbrev", "countyfips"), copy = FALSE)	
rm(county_name)

##merging in county demographic information
hc <- left_join(hc, counties, by = c("countyfips", "statefip"))

black_crime_year <- hc %>%
  data.frame() %>%
  distinct() %>%
  group_by(countyfips, statefip, year) %>%
  summarise(anti_black_sum = sum(anti_black),
            anti_black_any = max(anti_black)) %>%
  left_join(counties, by = c("countyfips", "statefip")) %>%
  mutate(year_lag = year + 1,
         black_crime_1000 = anti_black_sum * 1000 / pop,
         black_crime_blackcap = anti_black_sum / pop_black,
         black_crime_black1000 = anti_black_sum * 1000 / pop_black) %>%
  select(countyfips, statefip, year, anti_black_sum, anti_black_any, year_lag,
         black_crime_1000, black_crime_blackcap, black_crime_black1000)

black_crime_total <- black_crime_year %>%
  data.frame() %>%
  distinct() %>%
  group_by(countyfips, statefip) %>%
  summarise(anti_black_total = sum(anti_black_sum)) %>%
  left_join(counties, by = c("countyfips", "statefip")) %>%
  mutate(anti_black_mean = anti_black_total / 12,
         black_crime_cap = anti_black_total / pop,
         black_crime_1000 = anti_black_total * 1000 / pop,
         black_crime_blackcap = anti_black_total / pop_black,
         black_crime_black1000 = anti_black_total * 1000 / pop_black) %>%
  select(countyfips, statefip, anti_black_total, black_crime_cap, black_crime_1000,
         black_crime_blackcap, black_crime_black1000)

##ANTI BLACK VIOLENT CRIME
black_violent_crime_year <- hc %>%
  data.frame() %>%
  distinct() %>%
  mutate(anti_black_violent = anti_black * hc_violent) %>%
  group_by(countyfips, statefip, year) %>%
  summarise(anti_black_violent_sum = sum(anti_black_violent),
            anti_black_violent_any = max(anti_black_violent)) %>%
  left_join(counties, by = c("countyfips", "statefip")) %>%
  mutate(year_lag = year + 1,
         anti_black_violent_mean = anti_black_violent_sum / 12,
         black_violent_crime_1000 = anti_black_violent_sum * 1000 / pop,
         black_violent_crime_blackcap = anti_black_violent_sum / pop_black,
         black_violent_crime_black1000 = anti_black_violent_sum * 1000 / pop_black) %>%
  select(countyfips, statefip, year, anti_black_violent_sum, anti_black_violent_any, year_lag,
         anti_black_violent_mean, black_violent_crime_1000, black_violent_crime_blackcap, black_violent_crime_black1000)

black_violent_crime_total <- black_violent_crime_year %>%
  data.frame() %>%
  distinct() %>%
  group_by(countyfips, statefip) %>%
  summarise(anti_black_violent_total = sum(anti_black_violent_sum)) %>%
  left_join(counties, by = c("countyfips", "statefip")) %>%
  mutate(black_violent_crime_cap = anti_black_violent_total / pop,
         black_violent_crime_1000 = anti_black_violent_total * 1000 / pop,
         black_violent_crime_blackcap = anti_black_violent_total / pop_black,
         black_violent_crime_black1000 = anti_black_violent_total * 1000 / pop_black) %>%
  select(countyfips, statefip, anti_black_violent_total, black_violent_crime_cap,
         black_violent_crime_1000, black_violent_crime_blackcap, black_violent_crime_black1000)

##HISPANIC
#ANTI-HISPANIC CRIME BY YEAR
hisp_crime_year <- hc %>%
  data.frame() %>%
  distinct() %>%
  group_by(countyfips, statefip, year) %>%
  summarise(anti_hisp_sum = sum(anti_hisp),
            anti_hisp_any = max(anti_hisp)) %>%
  left_join(counties, by = c("countyfips", "statefip")) %>%
  mutate(year_lag = year + 1,
         hisp_crime_1000 = anti_hisp_sum * 1000 / pop,
         hisp_crime_hispcap = anti_hisp_sum / pop_hisp,
         hisp_crime_hisp1000 = anti_hisp_sum * 1000 / pop_hisp) %>%
  select(countyfips, statefip, year, anti_hisp_sum, anti_hisp_any, year_lag,
         hisp_crime_1000, hisp_crime_hispcap, hisp_crime_hisp1000)

#ANTI-HISPANIC CRIME TOTAL
hisp_crime_total <- hisp_crime_year %>%
  data.frame() %>%
  distinct() %>%
  group_by(countyfips, statefip) %>%
  summarise(anti_hisp_total = sum(anti_hisp_sum)) %>%
  left_join(counties, by = c("countyfips", "statefip")) %>%
  mutate(anti_hisp_mean = anti_hisp_total / 12,
         hisp_crime_cap = anti_hisp_total / pop,
         hisp_crime_1000 = anti_hisp_total * 1000 / pop,
         hisp_crime_hispcap = anti_hisp_total / pop_hisp,
         hisp_crime_hisp1000 = anti_hisp_total * 1000 / pop_hisp) %>%
  select(countyfips, statefip, anti_hisp_total, anti_hisp_mean, hisp_crime_cap, hisp_crime_1000,
         hisp_crime_hispcap, hisp_crime_hisp1000)

##ANTI HISPANIC VIOLENT CRIME
#VIOLENT ANTI-HISPANIC CRIME
hisp_violent_crime_year <- hc %>%
  data.frame() %>%
  distinct() %>%
  mutate(anti_hisp_violent = anti_hisp * hc_violent) %>%
  group_by(countyfips, statefip, year) %>%
  summarise(anti_hisp_violent_sum = sum(anti_hisp_violent),
            anti_hisp_violent_any = max(anti_hisp_violent)) %>%
  left_join(counties, by = c("countyfips", "statefip")) %>%
  mutate(year_lag = year + 1,
         anti_hisp_violent_mean = anti_hisp_violent_sum / 12,
         hisp_violent_crime_1000 = anti_hisp_violent_sum * 1000 / pop,
         hisp_violent_crime_hispcap = anti_hisp_violent_sum / pop_hisp,
         hisp_violent_crime_hisp1000 = anti_hisp_violent_sum * 1000 / pop_hisp) %>%
  select(countyfips, statefip, year, anti_hisp_violent_sum, anti_hisp_violent_any, year_lag,
         anti_hisp_violent_mean, hisp_violent_crime_1000, hisp_violent_crime_hispcap, hisp_violent_crime_hisp1000)

#VIOLENT ANTI-HISPANIC CRIME TOTAL
hisp_violent_crime_total <- hisp_violent_crime_year %>%
  data.frame() %>%
  distinct() %>%
  group_by(countyfips, statefip) %>%
  summarise(anti_hisp_violent_total = sum(anti_hisp_violent_sum)) %>%
  left_join(counties, by = c("countyfips", "statefip")) %>%
  mutate(hisp_violent_crime_cap = anti_hisp_violent_total / pop,
         hisp_violent_crime_1000 = anti_hisp_violent_total * 1000 / pop,
         hisp_violent_crime_hispcap = anti_hisp_violent_total / pop_hisp,
         hisp_violent_crime_hisp1000 = anti_hisp_violent_total * 1000 / pop_hisp) %>%
  select(countyfips, statefip, anti_hisp_violent_total, hisp_violent_crime_cap, hisp_violent_crime_1000,
         hisp_violent_crime_hispcap, hisp_violent_crime_hisp1000)

setwd("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/output")

write.csv(hc, file = "for_map.csv")