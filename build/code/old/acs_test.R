	
	acs_2008 <- mutate(acs_2008, black = race)
	if_else(acs_2008$black == 2, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, native = race)
	if_else(acs_2008$native == 3, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, apa = race)
	if_else(acs_2008$apa >= 4 & acs_2008$apa <= 6, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, apa = race)
	if_else(acs_2008$apa >= 4 & acs_2008$apa <= 6, 1, 0, missing = NULL)	

##creating ancestry dummies
	acs_2008 <- mutate(acs_2008, anc_white = ancestr1)
	if_else(acs_2008$anc_white < 200 | acs_2008$anc_white >= 400 & acs_2008$anc_white < 500 | acs_2008$anc_white == 924 & acs_2008$anc_white < 995, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, anc_hisp = ancestr1)
	if_else(acs_2008$anc_hisp >= 200 & acs_2008$anc_hisp < 300, 1, 0, missing = NULL)	

	acs_2008 <- mutate(acs_2008, anc_black = ancestr1)
	if_else(acs_2008$anc_black >= 500 & acs_2008$anc_black < 600 | acs_2008$anc_black == 900 | acs_2008$anc_black == 902, 1, 0, missing = NULL)	

	acs_2008 <- mutate(acs_2008, anc_apa = ancestr1)
	if_else(acs_2008$anc_apa >= 600 & acs_2008$anc_apa < 900, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, anc_native = ancestr1)
	if_else(acs_2008$anc_native >= 913 & acs_2008$anc_native < 924 | acs_2008$anc_native == 930, 1, 0, missing = NULL)

##creating ancestry1 dummies
	acs_2008 <- mutate(acs_2008, anc1_white = ancestr1)
	if_else(acs_2008$anc1_white < 1950 | acs_2008$anc1_white <= 4000 & acs_2008$anc1_white < 5000 | acs_2008$anc1_white >= 9240 & acs_2008$anc1_white < 9950, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, anc1_hisp = ancestr1)
	if_else(acs_2008$anc1_hisp >= 2000 & acs_2008$anc1_hisp < 3000, 1, 0, missing = NULL)	

	acs_2008 <- mutate(acs_2008, anc1_black = ancestr1)
	if_else(acs_2008$anc1_black >= 5000 & acs_2008$anc1_black < 6000 | acs_2008$anc1_black >= 9000 & acs_2008$anc1_black < 9130, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, anc1_apa = ancestr1)
	if_else(acs_2008$anc1_apa >= 6000 & acs_2008$anc1_apa < 9000, 1, 0, missing = NULL)			

	acs_2008 <- mutate(acs_2008, anc1_native = ancestr1)
	if_else(acs_2008$anc1_native >= 9130 & acs_2008$anc1_native < 9240, 1, 0, missing = NULL)

##creating ancestry2 dummies
	acs_2008 <- mutate(acs_2008, anc2_white = ancestr1)
	if_else(acs_2008$anc2_white < 1950 | acs_2008$anc2_white <= 4000 & acs_2008$anc2_white < 5000 | acs_2008$anc2_white >= 9240 & acs_2008$anc2_white < 9950, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, anc2_hisp = ancestr1)
	if_else(acs_2008$anc2_hisp >= 2000 & acs_2008$anc2_hisp < 3000, 1, 0, missing = NULL)	

	acs_2008 <- mutate(acs_2008, anc2_black = ancestr1)
	if_else(acs_2008$anc2_black >= 5000 & acs_2008$anc2_black < 6000 | acs_2008$anc2_black >= 9000 & acs_2008$anc2_black < 9130, 1, 0, missing = NULL)

	acs_2008 <- mutate(acs_2008, anc2_apa = ancestr1)
	if_else(acs_2008$anc2_apa >= 6000 & acs_2008$anc2_apa < 9000, 1, 0, missing = NULL)			

	acs_2008 <- mutate(acs_2008, anc2_native = ancestr1)
	if_else(acs_2008$anc2_native >= 9130 & acs_2008$anc2_native < 9240, 1, 0, missing = NULL)	
