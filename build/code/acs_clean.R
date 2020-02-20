##bringing in the acs crime data, cleaning it
library(tidyverse)
library(haven)
library(ipumsr)
setwd("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/build/inputs")

##Loading in micro data from IPUMS
ddi <- read_ipums_ddi("usa_00049.xml")
acs <- read_ipums_micro(ddi)

rm(ddi)
gc()

clean_acs <- data.frame(acs) %>%
  ##keeping relevant data
  select(YEAR, STATEFIP, COUNTYFIPS, METRO, PERWT, SEX, AGE, MARST, RACE, HISPAN, BPL, MIGRATE1D, 
         ANCESTR1, ANCESTR2, CITIZEN, YRSUSA1, SPEAKENG, RACNUM, EDUC, EMPSTAT, INCTOT, OWNERSHP)
clean_acs <- clean_acs %>%
  filter(##use only people who have been in the same county for a year or more
         MIGRATE1D == 10 | MIGRATE1D == 21, 
         ##people with codeable ancestry responses and who didn't say "american"
         ANCESTR1 < 939,
         
         COUNTYFIPS != 0) 

clean_acs <- clean_acs %>%
  ##renaming to match other data
  rename("statefip" = STATEFIP, 
         "countyfips" = COUNTYFIPS, 
         # "year" = YEAR,
         "age" = AGE,  
         "us_years" = YRSUSA1, 
         "weight" = PERWT, 
         "income" = INCTOT) %>%
  mutate(year = YEAR - 1)
clean_acs$year <- as.integer(clean_acs$year)

clean_acs <- clean_acs %>%
	##creating race & ancestry dummies
	mutate(black = if_else(RACE == 2, 1, 0),
	       hisp = if_else(HISPAN > 0 & HISPAN < 900, 1, 0), 
	       anc1_hisp = if_else(ANCESTR1 >= 210 & ANCESTR1 < 300, 1, 0), 
	       anc1_black = if_else(ANCESTR1 >= 500 & ANCESTR1 < 600 | ANCESTR1 >= 900 &
	                            ANCESTR1 < 913 | ANCESTR1 > 296 & ANCESTR1 < 360, 1, 0), 
	       anc2_hisp = if_else(ANCESTR2 >= 210 & ANCESTR2 < 300, 1, 0), 
	       anc2_black = if_else(ANCESTR2 >= 500 & ANCESTR2 < 600 | ANCESTR2 >= 900 &
	                            ANCESTR2 < 913 | ANCESTR2 > 296 & ANCESTR2 < 360, 1, 0)) 
clean_acs <- clean_acs %>%
   mutate(black_anc = anc1_black + anc2_black, 
          hisp_anc = anc1_hisp	+ anc2_hisp)

clean_acs <- clean_acs %>%
   ##combined ancestry dummies
   mutate(black_anc_only = if_else(black_anc == 2, 1, 0), 
          black_anc_other = if_else(black_anc == 1, 1, 0),
          black_anc_none = if_else(black_anc == 0, 1, 0), 
          hisp_anc_only = if_else(hisp_anc == 2, 1, 0), 
          hisp_anc_other = if_else(hisp_anc == 1, 1, 0),
          hisp_anc_none = if_else(hisp_anc == 0, 1, 0)) 
clean_acs <- clean_acs %>%
  ##other relevant variables
  mutate(metro = if_else(METRO == 1, 0, 1), 
         own_home = if_else(OWNERSHP == 1, 1, 0), 
         woman = if_else(SEX == 2, 1, 0), 
         married = if_else(MARST < 3, 1, 0), 
         us_born = if_else(BPL <= 120, 1, 0),
         english = if_else(SPEAKENG >= 2 & SPEAKENG <= 5, 1, 0), 
         above_ba = if_else(EDUC >= 10, 1, 0),
         employed = if_else(EMPSTAT == 1, 1, 0), 
         citizen = if_else(CITIZEN <= 2, 1, 0)) 
clean_acs <- clean_acs %>%
  select(statefip, countyfips, year, age, income, metro, own_home, woman, married, us_born, english,
         above_ba, employed, black, hisp, black_anc_only, black_anc_other, black_anc_none, 
         hisp_anc_only, hisp_anc_other, hisp_anc_none, weight) 

clean_acs$countyfips <- as.integer(clean_acs$countyfips)
clean_acs$statefip <- as.integer(clean_acs$statefip)

##creating ancestry1 dummies
	# acs <- mutate(acs, anc1_white = ANCESTR1)
	# acs$anc1_white <- if_else(acs$anc1_white < 195 | acs$anc1_white <= 400 & acs$anc1_white < 500 
	# 	| acs$anc1_white == 924 | acs$anc1_white > 930 & acs$anc1_white < 936, 1, 0, missing = NULL)

	# acs <- mutate(acs, anc1_apa = ANCESTR1)
	# acs$anc1_apa <- if_else(acs$anc1_apa >= 600 & acs$anc1_apa < 900, 1, 0, missing = NULL)			

	# acs <- mutate(acs, anc1_native = ANCESTR1)
	# acs$anc1_native <- if_else(acs$anc1_native >= 913 & acs$anc1_native < 924 | acs$anc1_native == 930, 
	# 	1, 0, missing = NULL)

	# acs <- mutate(acs, anc1_poc = ANCESTR1)
	# acs$anc1_poc <- if_else(acs$anc1_poc >= 200 & acs$anc1_poc < 924 
	# 	| acs$anc1_poc == 930 , 1, 0, missing = NULL)

##creating ancestry2 dummies
	# acs <- mutate(acs, anc2_white = ANCESTR2)
	# acs$anc2_white <- if_else(acs$anc2_white < 195 | acs$anc2_white <= 400 & acs$anc2_white < 500 
	# 	| acs$anc2_white == 924 | acs$anc2_white > 930 & acs$anc2_white < 936, 1, 0, missing = NULL)

	# acs <- mutate(acs, anc2_apa = ANCESTR2)
	# acs$anc2_apa <- if_else(acs$anc2_apa >= 600 & acs$anc2_apa < 900, 1, 0, missing = NULL)			

	# acs <- mutate(acs, anc2_native = ANCESTR2)
	# acs$anc2_native <- if_else(acs$anc2_native >= 913 & acs$anc2_native < 924 | acs$anc2_native == 930, 
	# 	1, 0, missing = NULL)

	# acs <- mutate(acs, anc2_poc = ANCESTR2)
	# acs$anc2_poc <- if_else(acs$anc2_poc >= 200 & acs$anc2_poc < 924 
	# 	| acs$anc2_poc == 930 , 1, 0, missing = NULL)

	# acs <- mutate(acs, apa_anc = anc1_apa	+ anc2_apa)
	# acs <- mutate(acs, apa_only = apa_anc)
	# acs$apa_only <- if_else(acs$apa_only == 2, 1, 0, missing = NULL)
	# acs <- mutate(acs, apa_other = apa_anc)
	# acs$apa_other <- if_else(acs$apa_other == 1, 1, 0, missing = NULL)
	# acs <- mutate(acs, apa_none = apa_anc)	
	# acs$apa_none <- if_else(acs$apa_none == 0, 1, 0, missing = NULL)

	# acs <- mutate(acs, native_anc = anc1_native	+ anc2_native)
	# acs <- mutate(acs, native_only = native_anc)
	# acs$native_only <- if_else(acs$native_only == 2, 1, 0, missing = NULL)
	# acs <- mutate(acs, native_other = native_anc)
	# acs$native_other <- if_else(acs$native_other == 1, 1, 0, missing = NULL)
	# acs <- mutate(acs, native_none = native_anc)	
	# acs$native_none <- if_else(acs$native_none == 0, 1, 0, missing = NULL)

	# acs <- mutate(acs, poc_anc = anc1_poc + anc2_poc)
	# acs <- mutate(acs, poc_only = poc_anc)
	# acs$poc_only <- if_else(acs$poc_only == 2, 1, 0, missing = NULL)
	# acs <- mutate(acs, poc_other = poc_anc)
	# acs$poc_other <- if_else(acs$poc_other == 1, 1, 0, missing = NULL)
	# acs <- mutate(acs, poc_none = poc_anc)	
	# acs$poc_none <- if_else(acs$poc_none == 0, 1, 0, missing = NULL)