##bringing in the hate crime data, cleaning it, matching it to census data

##home
# setwd("C:/Users/cassa/Google Drive/Research Projects/hate_crimes")

##work
setwd("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes")
library("dplyr", warn.conflicts = FALSE)


##HATE CRIME DATA
##loading in the micro hate crime data 

	hc_2005 <- read.csv("hatecrimes_2005_micro.csv")
	hc_2006 <- read.csv("hatecrimes_2006_micro.csv")
	hc_2007 <- read.csv("hatecrimes_2007_micro.csv")
	hc_2008 <- read.csv("hatecrimes_2008_micro.csv")
	hc_2009 <- read.csv("hatecrimes_2009_micro.csv")
	hc_2010 <- read.csv("hatecrimes_2010_micro.csv")
	hc_2011 <- read.csv("hatecrimes_2011_micro.csv")
	hc_2012 <- read.csv("hatecrimes_2012_micro.csv")
	hc_2013 <- read.csv("hatecrimes_2013_micro.csv")
	hc_2014 <- read.csv("hatecrimes_2014_micro.csv")
	hc_2015 <- read.csv("hatecrimes_2015_micro.csv")
	hc_2016 <- read.csv("hatecrimes_2016_micro.csv")

	allhc <- rbind(hc_2005, hc_2006, hc_2007, hc_2008, hc_2009, hc_2010, hc_2012, hc_2011, hc_2013, hc_2014, hc_2015, hc_2016)
	rm(hc_2005, hc_2006, hc_2007, hc_2008, hc_2009, hc_2010, hc_2011, hc_2012, hc_2013, hc_2014, hc_2015, hc_2016)

##cleaning

	# colnames(allhc)[colnames(allhc)=="REC_BH"] <- "hate_type"	
	colnames(allhc)[colnames(allhc)=="STATNUM"] <- "state_full"
	colnames(allhc)[colnames(allhc)=="STATECOD"] <- "state_abbrev"
	colnames(allhc)[colnames(allhc)=="CFIPS1"] <- "county_fips"
	colnames(allhc)[colnames(allhc)=="TNUMVTMS"] <- "vctms_total"
	colnames(allhc)[colnames(allhc)=="TNUMOFF"] <- "offndrs_total"
	colnames(allhc)[colnames(allhc)=="GOFFRAC"] <- "offndrs_race"
	colnames(allhc)[colnames(allhc)=="AGINDIC"] <- "agncy_type"
	colnames(allhc)[colnames(allhc)=="OFFCOD1"] <- "offns_type1"
	colnames(allhc)[colnames(allhc)=="BIASMO1"] <- "type1"
	colnames(allhc)[colnames(allhc)=="NUMVTM1"] <- "vctms_num1"
	colnames(allhc)[colnames(allhc)=="LOCCOD1"] <- "offns_lctn1"
	colnames(allhc)[colnames(allhc)=="OFFCOD2"] <- "offns_type2"
	colnames(allhc)[colnames(allhc)=="BIASMO2"] <- "type2"
	colnames(allhc)[colnames(allhc)=="NUMVTM2"] <- "vctms_num2"
	colnames(allhc)[colnames(allhc)=="LOCCOD2"] <- "offns_lctn2"
	colnames(allhc)[colnames(allhc)=="OFFCOD3"] <- "offns_type3"
	colnames(allhc)[colnames(allhc)=="BIASMO3"] <- "type3"
	colnames(allhc)[colnames(allhc)=="NUMVTM3"] <- "vctms_num3"
	colnames(allhc)[colnames(allhc)=="LOCCOD3"] <- "offns_lctn3"
	colnames(allhc)[colnames(allhc)=="MASTERYR"] <- "year"

	drop = list("ORI", "JUDDIST","NBRSFLG", "POP3", "COUNTY3", "MSA3", "LSTPOP2",
			"LSTPOP3", "POP4", "COUNTY4", "MSA4", "LSTPOP4", "F4QACT",  "DATASRC", 
			"POP5", "COUNTY5", "MSA5", "LSTPOP5", "CFIPS2", "ORIADDDT", 
			"CFIPS3", "CFIPS4", "CFIPS5", "AGNAME", "F1QACT", "F2QACT", "F3QACT", "REC_IR", 
			"ORINIBRS", "DIVISN", "REGION", "CORECTY", "COVBYORI", "FIELDNO", "INACTDTE", 			 
			"OFFCOD4", "OFFCOD5", "OFFCOD6", "OFFCOD7", "OFFCOD8", "OFFCOD9", "OFFCOD10", 
			"BIASMO4", "BIASMO5", "BIASMO6", "BIASMO7", "BIASMO8", "BIASMO9", "BIASMO10",
			"NUMVTM4", "NUMVTM5", "NUMVTM6", "NUMVTM7", "NUMVTM8", "NUMVTM9", "NUMVTM10", 
			"LOCCOD4", "LOCCOD5", "LOCCOD6", "LOCCOD7", "LOCCOD8", "LOCCOD9", "LOCCOD10", 
			"VTYP_I4", "VTYP_I5", "VTYP_I6", "VTYP_I7", "VTYP_I8", "VTYP_I9", "VTYP_I10",
			"VTYP_B4", "VTYP_B5", "VTYP_B6", "VTYP_B7", "VTYP_B8", "VTYP_B9", "VTYP_B10",
			"VTYP_F4", "VTYP_F5", "VTYP_F6", "VTYP_F7", "VTYP_F8", "VTYP_F9", "VTYP_F10", 
			"VTYP_G4", "VTYP_G5", "VTYP_G6", "VTYP_G7", "VTYP_G8", "VTYP_G9", "VTYP_G10",
			"VTYP_R4", "VTYP_R5", "VTYP_R6", "VTYP_R7", "VTYP_R8", "VTYP_R9", "VTYP_R10", 
			"QTR1ACT", "QTR2ACT", "QTR3ACT", "QTR4ACT")


	hc <- allhc[,!(names(allhc) %in% drop)]
	rm(drop, allhc)

##creating new variables for analysis

	##crime type

	hc <- mutate(hc, anti_racial = type1)
	if_else(hc$anti_racial < 20 & hc$anti_racial >= 11 | hc$anti_racial == 32, 1, 0, missing = NULL)

	hc <- mutate(hc, anti_underrep = type1)
	if_else(hc$anti_underrep < 20 & hc$anti_underrep >= 11 | hc$anti_underrep == 32, 1, 0, missing = NULL)

	hc <- mutate(hc, anti_black = type1)
	if_else(hc$anti_black == 12, 1, 0, missing = NULL)

	hc <- mutate(hc, anti_hisp = type1)
	if_else(hc$anti_hisp == 32, 1, 0, missing = NULL)	

	hc <- mutate(hc, anti_apa = type1)
	if_else(hc$anti_apa == 14 | hc$anti_apa == 16, 1, 0, missing = NULL)		

	hc <- mutate(hc, anti_native = type1)
	if_else(hc$anti_native == 13, 1, 0, missing = NULL)			

##ACS DATA
##Loading in micro data from IPUMS
	acs_2005 <- read.csv("acs_data_2005.csv")
	acs_2006 <- read.csv("acs_data_2006.csv")
	acs_2007 <- read.csv("acs_data_2007.csv")
	acs_2008 <- read.csv("acs_data_2008.csv")
	acs_2009 <- read.csv("acs_data_2009.csv")
	acs_2010 <- read.csv("acs_data_2010.csv")
	acs_2011 <- read.csv("acs_data_2011.csv")
	acs_2012 <- read.csv("acs_data_2012.csv")
	acs_2013 <- read.csv("acs_data_2013.csv")
	acs_2014 <- read.csv("acs_data_2014.csv")
	acs_2015 <- read.csv("acs_data_2015.csv")
	acs_2016 <- read.csv("acs_data_2016.csv")
	
	acs <- rbind(acs_2005, acs_2006, acs_2007, acs_2008, acs_2009, acs_2010, acs_2011, acs_2012, acs_2013, acs_2014, acs_2015, acs_2016)

	rm(acs_2005, acs_2006, acs_2007, acs_2008, acs_2009, acs_2010, acs_2011, acs_2012, acs_2013, acs_2014, acs_2015, acs_2016)	


##creating race dummies
	
	acs <- mutate(acs, black = race)
	acs$black <- if_else(acs$black == 2, 1, 0, missing = NULL)

	acs <- mutate(acs, native = race)
	acs$native <- if_else(acs$native == 3, 1, 0, missing = NULL)

	acs <- mutate(acs, apa = race)
	acs$apa <- if_else(acs$apa >= 4 & acs$apa <= 6, 1, 0, missing = NULL)

	acs <- mutate(acs, hisp = hispan)
	acs$hisp <- if_else(acs$hisp > 0, 1, 0, missing = NULL)	

##creating ancestry dummies
	acs <- mutate(acs, anc_white = ancestr1)
	acs$anc_white <- if_else(acs$anc_white < 200 | acs$anc_white >= 400 & acs$anc_white < 500 | acs$anc_white == 924 & acs$anc_white < 995, 1, 0, missing = NULL)

	acs <- mutate(acs, anc_hisp = ancestr1)
	acs$anc_hisp <- if_else(acs$anc_hisp >= 200 & acs$anc_hisp < 300, 1, 0, missing = NULL)	

	acs <- mutate(acs, anc_black = ancestr1)
	acs$anc_black <- if_else(acs$anc_black >= 500 & acs$anc_black < 600 | acs$anc_black == 900 | acs$anc_black == 902, 1, 0, missing = NULL)	

	acs <- mutate(acs, anc_apa = ancestr1)
	acs$anc_apa <- if_else(acs$anc_apa >= 600 & acs$anc_apa < 900, 1, 0, missing = NULL)

	acs <- mutate(acs, anc_native = ancestr1)
	acs$anc_native <- if_else(acs$anc_native >= 913 & acs$anc_native < 924 | acs$anc_native == 930, 1, 0, missing = NULL)

##creating ancestry1 dummies
	acs <- mutate(acs, anc1_white = ancestr1)
	acs$anc1_white <- if_else(acs$anc1_white < 1950 | acs$anc1_white <= 4000 & acs$anc1_white < 5000 | acs$anc1_white >= 9240 & acs$anc1_white < 9950, 1, 0, missing = NULL)

	acs <- mutate(acs, anc1_hisp = ancestr1)
	acs$anc1_hisp <- if_else(acs$anc1_hisp >= 2000 & acs$anc1_hisp < 3000, 1, 0, missing = NULL)	

	acs <- mutate(acs, anc1_black = ancestr1)
	acs$anc1_black <- if_else(acs$anc1_black >= 5000 & acs$anc1_black < 6000 | acs$anc1_black >= 9000 & acs$anc1_black < 9130, 1, 0, missing = NULL)

	acs <- mutate(acs, anc1_apa = ancestr1)
	acs$anc1_apa <- if_else(acs$anc1_apa >= 6000 & acs$anc1_apa < 9000, 1, 0, missing = NULL)			

	acs <- mutate(acs, anc1_native = ancestr1)
	acs$anc1_native <- if_else(acs$anc1_native >= 9130 & acs$anc1_native < 9240, 1, 0, missing = NULL)

##creating ancestry2 dummies
	acs <- mutate(acs, anc2_white = ancestr1)
	acs$anc2_white <- if_else(acs$anc2_white < 1950 | acs$anc2_white <= 4000 & acs$anc2_white < 5000 | acs$anc2_white >= 9240 & acs$anc2_white < 9950, 1, 0, missing = NULL)

	acs <- mutate(acs, anc2_hisp = ancestr1)
	acs$anc2_hisp <- if_else(acs$anc2_hisp >= 2000 & acs$anc2_hisp < 3000, 1, 0, missing = NULL)	

	acs <- mutate(acs, anc2_black = ancestr1)
	acs$anc2_black <- if_else(acs$anc2_black >= 5000 & acs$anc2_black < 6000 | acs$anc2_black >= 9000 & acs$anc2_black < 9130, 1, 0, missing = NULL)

	acs <- mutate(acs, anc2_apa = ancestr1)
	acs$anc2_apa <- if_else(acs$anc2_apa >= 6000 & acs$anc2_apa < 9000, 1, 0, missing = NULL)			

	acs <- mutate(acs, anc2_native = ancestr1)
	acs$anc2_native <- if_else(acs$anc2_native >= 9130 & acs$anc2_native < 9240, 1, 0, missing = NULL)

##measuring hate crimes per person

	##allhc$hcpercap <- tally(allhc$)/pop1

##measuring hate crimes by county, year
	data.frame(table(cleanmicrodata$county_fips, cleanmicrodata$year, cleanmicrodata$state_abbrev))
	count(x, ..., wt = NULL, sort = FALSE)


