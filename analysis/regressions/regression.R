library(stargazer)

setwd("/mnt/DARCE/_Research/_Cassandra Duchan/research/hate_crimes/analysis/regressions")
sink("lag_regressions.txt")

##GENERAL BLACK IDENTIFICATION
# black_identity <- lm(black ~ black_anc_only + black_anc_other + black_anc_none, data = analysis_black_year)
# ##for latex
# stargazer(black_identity,
# 	column.labels=c("Black Identification"))
# rm(black_identity)
# gc()

# ##ANTI BLACK YEARLY
black_reg_any <- lm(black ~ black_anc_only*anti_black_any +
					 black_anc_other*anti_black_any + black_anc_none*anti_black_any +
					 ##acs data
					 black_anc_only +black_anc_other +
					 year + age + income + metro + own_home + woman +
					 married + us_born + english + above_ba + employed +
					 ##county demographics
					 pop_black + pop_pct_black,
					 weights = weight,
					 data = analysis_black_year)
##for latex
stargazer(black_reg_any,
          column.labels=c("Black Identification"))
rm(black_reg_any)
gc()

black_reg_sum <- lm(black ~ black_anc_only*anti_black_sum +
                    black_anc_other*anti_black_sum + black_anc_none*anti_black_sum +
                    ##acs data
                    black_anc_only + black_anc_other +
                    year + age + income + metro + own_home + woman +
                    married + us_born + english + above_ba + employed +
                    ##county demographics
                    pop_black + pop_pct_black,
                    weights = weight,
                    data = analysis_black_year)
##for latex
stargazer(black_reg_sum,
          column.labels=c("Black Identification"))
rm(black_reg_sum)
gc()
# 
# black_reg_mean <- lm(black ~ black_anc_only*anti_black_mean +
#                       black_anc_other*anti_black_mean + black_anc_none*anti_black_mean +
#                       ##acs data
#                       black_anc_only + black_anc_other +
#                       year + age + income + metro + own_home + woman +
#                       married + us_born + english + above_ba + employed +
#                       ##county demographics
#                       pop_black + pop_pct_black,
#                       weights = weight,
#                       data = analysis_black_year)
# ##for latex
# stargazer(black_reg_mean,
#           column.labels=c("Black Identification"))
# rm(black_reg_mean)
# gc()

black_reg_1000 <- lm(black ~ black_anc_only*black_crime_1000 +
                       black_anc_other*black_crime_1000 + black_anc_none*black_crime_1000 +
                       ##acs data
                       black_anc_only + black_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_black + pop_pct_black,
                     weights = weight,
                     data = analysis_black_year)
##for latex
stargazer(black_reg_1000,
          column.labels=c("Black Identification"))
rm(black_reg_1000)
gc()

black_reg_blackcap <- lm(black ~ black_anc_only*black_crime_blackcap +
                       black_anc_other*black_crime_blackcap + black_anc_none*black_crime_blackcap +
                       ##acs data
                       black_anc_only + black_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_black + pop_pct_black,
                     weights = weight,
                     data = analysis_black_year)
##for latex
stargazer(black_reg_blackcap,
          column.labels=c("Black Identification"))
rm(black_reg_blackcap)
gc()

black_reg_black1000 <- lm(black ~ black_anc_only*black_crime_black1000 +
                       black_anc_other*black_crime_black1000 + black_anc_none*black_crime_black1000 +
                       ##acs data
                       black_anc_only + black_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_black + pop_pct_black,
                     weights = weight,
                     data = analysis_black_year)
##for latex
stargazer(black_reg_black1000,
          column.labels=c("Black Identification"))
rm(black_reg_black1000)
gc()

# 
# ##ANTI BLACK TOTAL
# black_reg_any <- lm(black ~ black_anc_only*anti_black_any +
# 					 black_anc_other*anti_black_any + black_anc_none*anti_black_any +
# 					 ##acs data
# 					 black_anc_only +black_anc_other +
# 					 year + age + income + metro + own_home + woman +
# 					 married + us_born + english + above_ba + employed +
# 					 ##county demographics
# 					 pop_black + pop_pct_black,
# 					 weights = weight,
# 					 data = analysis_black_total)
# ##for latex
# stargazer(black_reg_any,
#           column.labels=c("Black Identification"))
# rm(black_reg_any)
# gc()

black_reg_total <- lm(black ~ black_anc_only*anti_black_total +
                    black_anc_other*anti_black_total + black_anc_none*anti_black_total +
                    ##acs data
                    black_anc_only + black_anc_other +
                    year + age + income + metro + own_home + woman +
                    married + us_born + english + above_ba + employed +
                    ##county demographics
                    pop_black + pop_pct_black,
                    weights = weight,
                    data = analysis_black_total)
##for latex
stargazer(black_reg_total,
          column.labels=c("Black Identification"))
rm(black_reg_total)
gc()
# 
# black_reg_mean <- lm(black ~ black_anc_only*anti_black_mean +
#                       black_anc_other*anti_black_mean + black_anc_none*anti_black_mean +
#                       ##acs data
#                       black_anc_only + black_anc_other +
#                       year + age + income + metro + own_home + woman +
#                       married + us_born + english + above_ba + employed +
#                       ##county demographics
#                       pop_black + pop_pct_black,
#                       weights = weight,
#                       data = analysis_black_total)
# ##for latex
# stargazer(black_reg_mean,
#           column.labels=c("Black Identification"))
# rm(black_reg_mean)
# gc()

black_reg_1000 <- lm(black ~ black_anc_only*black_crime_1000 +
                       black_anc_other*black_crime_1000 + black_anc_none*black_crime_1000 +
                       ##acs data
                       black_anc_only + black_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_black + pop_pct_black,
                     weights = weight,
                     data = analysis_black_total)
##for latex
stargazer(black_reg_1000,
          column.labels=c("Black Identification"))
rm(black_reg_1000)
gc()

black_reg_blackcap <- lm(black ~ black_anc_only*black_crime_blackcap +
                       black_anc_other*black_crime_blackcap + black_anc_none*black_crime_blackcap +
                       ##acs data
                       black_anc_only + black_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_black + pop_pct_black,
                     weights = weight,
                     data = analysis_black_total)
##for latex
stargazer(black_reg_blackcap,
          column.labels=c("Black Identification"))
rm(black_reg_blackcap)
gc()

black_reg_black1000 <- lm(black ~ black_anc_only*black_crime_black1000 +
                       black_anc_other*black_crime_black1000 + black_anc_none*black_crime_black1000 +
                       ##acs data
                       black_anc_only + black_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_black + pop_pct_black,
                     weights = weight,
                     data = analysis_black_total)
##for latex
stargazer(black_reg_black1000,
          column.labels=c("Black Identification"))
rm(black_reg_black1000)
gc()
# 
# black_reg_violent_any <- lm(black ~ black_anc_only*anti_black_violent_any +
#                             black_anc_other*anti_black_violent_any + black_anc_none*anti_black_violent_any +
#                             ##acs data
#                             black_anc_only + black_anc_other +
#                             year + age + income + metro + own_home + woman +
#                             married + us_born + english + above_ba + employed +
#                             ##county demographics
#                             pop_black + pop_pct_black,
#                             weights = weight,
#                             data = analysis_black_violent_year)
# ##for latex
# stargazer(black_reg_violent_any,
#           column.labels=c("Black Identification"))
# rm(black_reg_violent_any)
# gc()
# 
# black_reg_sum <- lm(black ~ black_anc_only*anti_black_violent_sum +
#                       black_anc_other*anti_black_violent_sum + black_anc_none*anti_black_violent_sum + 
#                       ##acs data 
#                       black_anc_only + black_anc_other + 
#                       year + age + income + metro + own_home + woman +
#                       married + us_born + english + above_ba + employed +
#                       ##county demographics
#                       pop_black + pop_pct_black,
#                     weights = weight,
#                     data = analysis_black_violent_year)
# ##for latex
# stargazer(black_reg_sum, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_sum)
# gc()
# 
# black_reg_mean <- lm(black ~ black_anc_only*anti_black_violent_mean +
#                        black_anc_other*anti_black_violent_mean + black_anc_none*anti_black_violent_mean + 
#                        ##acs data 
#                        black_anc_only + black_anc_other + 
#                        year + age + income + metro + own_home + woman +
#                        married + us_born + english + above_ba + employed +
#                        ##county demographics
#                        pop_black + pop_pct_black,
#                      weights = weight,
#                      data = analysis_black_violent_year)
# ##for latex
# stargazer(black_reg_mean, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_mean)
# gc()
# 
# black_reg_1000 <- lm(black ~ black_anc_only*black_violent_crime_1000 +
#                        black_anc_other*black_violent_crime_1000 + black_anc_none*black_violent_crime_1000 + 
#                        ##acs data 
#                        black_anc_only + black_anc_other + 
#                        year + age + income + metro + own_home + woman +
#                        married + us_born + english + above_ba + employed +
#                        ##county demographics
#                        pop_black + pop_pct_black,
#                      weights = weight,
#                      data = analysis_black_violent_year)
# ##for latex
# stargazer(black_reg_1000, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_1000)
# gc()
# 
# black_reg_blackcap <- lm(black ~ black_anc_only*black_violent_crime_blackcap +
#                            black_anc_other*black_violent_crime_blackcap + black_anc_none*black_violent_crime_blackcap + 
#                            ##acs data 
#                            black_anc_only + black_anc_other + 
#                            year + age + income + metro + own_home + woman +
#                            married + us_born + english + above_ba + employed +
#                            ##county demographics
#                            pop_black + pop_pct_black,
#                          weights = weight,
#                          data = analysis_black_violent_year)
# ##for latex
# stargazer(black_reg_blackcap, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_blackcap)
# gc()
# 
# black_reg_black1000 <- lm(black ~ black_anc_only*black_violent_crime_black1000 +
#                             black_anc_other*black_violent_crime_black1000 + black_anc_none*black_violent_crime_black1000 + 
#                             ##acs data 
#                             black_anc_only + black_anc_other + 
#                             year + age + income + metro + own_home + woman +
#                             married + us_born + english + above_ba + employed +
#                             ##county demographics
#                             pop_black + pop_pct_black,
#                           weights = weight,
#                           data = analysis_black_violent_year)
# ##for latex
# stargazer(black_reg_black1000, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_black1000)
# gc()
# 
# 
# black_reg_total <- lm(black ~ black_anc_only*anti_black_violent_total +
#                         black_anc_other*anti_black_violent_total + black_anc_none*anti_black_violent_total+ 
#                         ##acs data 
#                         black_anc_only + black_anc_other + 
#                         year + age + income + metro + own_home + woman +
#                         married + us_born + english + above_ba + employed +
#                         ##county demographics
#                         pop_black + pop_pct_black,
#                       weights = weight,
#                       data = analysis_black_violent_crime_total)
# ##for latex
# stargazer(black_reg_total, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_total)
# gc()
# 
# black_reg_1000 <- lm(black ~ black_anc_only*black_violent_crime_1000 +
#                        black_anc_other*black_violent_crime_1000 + black_anc_none*black_violent_crime_1000 + 
#                        ##acs data 
#                        black_anc_only + black_anc_other + 
#                        year + age + income + metro + own_home + woman +
#                        married + us_born + english + above_ba + employed +
#                        ##county demographics
#                        pop_black + pop_pct_black,
#                      weights = weight,
#                      data = analysis_black_violent_crime_total)
# ##for latex
# stargazer(black_reg_1000, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_1000)
# gc()
# 
# black_reg_blackcap <- lm(black ~ black_anc_only*black_violent_crime_blackcap +
#                            black_anc_other*black_violent_crime_blackcap + black_anc_none*black_violent_crime_blackcap + 
#                            ##acs data 
#                            black_anc_only + black_anc_other + 
#                            year + age + income + metro + own_home + woman +
#                            married + us_born + english + above_ba + employed +
#                            ##county demographics
#                            pop_black + pop_pct_black,
#                          weights = weight,
#                          data = analysis_black_violent_crime_total)
# ##for latex
# stargazer(black_reg_blackcap, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_blackcap)
# gc()
# 
# black_reg_black1000 <- lm(black ~ black_anc_only*black_violent_crime_black1000 +
#                             black_anc_other*black_violent_crime_black1000 + black_anc_none*black_violent_crime_black1000 + 
#                             ##acs data 
#                             black_anc_only + black_anc_other + 
#                             year + age + income + metro + own_home + woman +
#                             married + us_born + english + above_ba + employed +
#                             ##county demographics
#                             pop_black + pop_pct_black,
#                           weights = weight,
#                           data = analysis_black_violent_crime_total)
# ##for latex
# stargazer(black_reg_black1000, 
#           column.labels=c("Black Identification")) 
# rm(black_reg_black1000)
# gc()


##GENERAL HISPANIC IDENTIFICATION
# hisp_identity <- lm(hisp ~ hisp_anc_only + hisp_anc_other + hisp_anc_none, data = analysis_hisp_year)
# ##for latex
# stargazer(hisp_identity, 
#   column.labels=c("hisp Identification"))
# rm(hisp_identity)
# gc()

#ANTI HISP YEARLY
hisp_reg_any <- lm(hisp ~ hisp_anc_only*anti_hisp_any +
           hisp_anc_other*anti_hisp_any + hisp_anc_none*anti_hisp_any +
           ##acs data
           hisp_anc_only +hisp_anc_other +
           year + age + income + metro + own_home + woman +
           married + us_born + english + above_ba + employed +
           ##county demographics
           pop_hisp + pop_pct_hisp,
           weights = weight,
           data = analysis_hisp_year)
##for latex
stargazer(hisp_reg_any,
          column.labels=c("hisp Identification"))
rm(hisp_reg_any)
gc()

hisp_reg_sum <- lm(hisp ~ hisp_anc_only*anti_hisp_sum +
                    hisp_anc_other*anti_hisp_sum + hisp_anc_none*anti_hisp_sum +
                    ##acs data
                    hisp_anc_only + hisp_anc_other +
                    year + age + income + metro + own_home + woman +
                    married + us_born + english + above_ba + employed +
                    ##county demographics
                    pop_hisp + pop_pct_hisp,
                    weights = weight,
                    data = analysis_hisp_year)
##for latex
stargazer(hisp_reg_sum,
          column.labels=c("hisp Identification"))
rm(hisp_reg_sum)
gc()

# hisp_reg_mean <- lm(hisp ~ hisp_anc_only*anti_hisp_mean +
#                       hisp_anc_other*anti_hisp_mean + hisp_anc_none*anti_hisp_mean +
#                       ##acs data
#                       hisp_anc_only + hisp_anc_other +
#                       year + age + income + metro + own_home + woman +
#                       married + us_born + english + above_ba + employed +
#                       ##county demographics
#                       pop_hisp + pop_pct_hisp,
#                       weights = weight,
#                       data = analysis_hisp_year)
# ##for latex
# stargazer(hisp_reg_mean,
#           column.labels=c("hisp Identification"))
# rm(hisp_reg_mean)
# gc()

hisp_reg_1000 <- lm(hisp ~ hisp_anc_only*hisp_crime_1000 +
                       hisp_anc_other*hisp_crime_1000 + hisp_anc_none*hisp_crime_1000 +
                       ##acs data
                       hisp_anc_only + hisp_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_hisp + pop_pct_hisp,
                     weights = weight,
                     data = analysis_hisp_year)
##for latex
stargazer(hisp_reg_1000,
          column.labels=c("hisp Identification"))
rm(hisp_reg_1000)
gc()

hisp_reg_hispcap <- lm(hisp ~ hisp_anc_only*hisp_crime_hispcap +
                       hisp_anc_other*hisp_crime_hispcap + hisp_anc_none*hisp_crime_hispcap +
                       ##acs data
                       hisp_anc_only + hisp_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_hisp + pop_pct_hisp,
                     weights = weight,
                     data = analysis_hisp_year)
##for latex
stargazer(hisp_reg_hispcap,
          column.labels=c("hisp Identification"))
rm(hisp_reg_hispcap)
gc()

hisp_reg_hisp1000 <- lm(hisp ~ hisp_anc_only*hisp_crime_hisp1000 +
                       hisp_anc_other*hisp_crime_hisp1000 + hisp_anc_none*hisp_crime_hisp1000 +
                       ##acs data
                       hisp_anc_only + hisp_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_hisp + pop_pct_hisp,
                     weights = weight,
                     data = analysis_hisp_year)
##for latex
stargazer(hisp_reg_hisp1000,
          column.labels=c("hisp Identification"))
rm(hisp_reg_hisp1000)
gc()


# # ##ANTI HISP TOTAL
# hisp_reg_any <- lm(hisp ~ hisp_anc_only*anti_hisp_any +
#            hisp_anc_other*anti_hisp_any + hisp_anc_none*anti_hisp_any +
#            ##acs data
#            hisp_anc_only +hisp_anc_other +
#            year + age + income + metro + own_home + woman +
#            married + us_born + english + above_ba + employed +
#            ##county demographics
#            pop_hisp + pop_pct_hisp,
#            weights = weight,
#            data = analysis_hisp_total)
# ##for latex
# stargazer(hisp_reg_any,
#           column.labels=c("hisp Identification"))
# rm(hisp_reg_any)
# gc()

hisp_reg_total <- lm(hisp ~ hisp_anc_only*anti_hisp_total +
                    hisp_anc_other*anti_hisp_total + hisp_anc_none*anti_hisp_total +
                    ##acs data
                    hisp_anc_only + hisp_anc_other +
                    year + age + income + metro + own_home + woman +
                    married + us_born + english + above_ba + employed +
                    ##county demographics
                    pop_hisp + pop_pct_hisp,
                    weights = weight,
                    data = analysis_hisp_total)
##for latex
stargazer(hisp_reg_sum,
          column.labels=c("hisp Identification"))
rm(hisp_reg_total)
gc()

hisp_reg_mean <- lm(hisp ~ hisp_anc_only*anti_hisp_mean +
                      hisp_anc_other*anti_hisp_mean + hisp_anc_none*anti_hisp_mean +
                      ##acs data
                      hisp_anc_only + hisp_anc_other +
                      year + age + income + metro + own_home + woman +
                      married + us_born + english + above_ba + employed +
                      ##county demographics
                      pop_hisp + pop_pct_hisp,
                      weights = weight,
                      data = analysis_hisp_total)
##for latex
stargazer(hisp_reg_mean,
          column.labels=c("hisp Identification"))
rm(hisp_reg_mean)
gc()

hisp_reg_1000 <- lm(hisp ~ hisp_anc_only*hisp_crime_1000 +
                       hisp_anc_other*hisp_crime_1000 + hisp_anc_none*hisp_crime_1000 +
                       ##acs data
                       hisp_anc_only + hisp_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_hisp + pop_pct_hisp,
                     weights = weight,
                     data = analysis_hisp_total)
##for latex
stargazer(hisp_reg_1000,
          column.labels=c("hisp Identification"))
rm(hisp_reg_1000)
gc()

hisp_reg_hispcap <- lm(hisp ~ hisp_anc_only*hisp_crime_hispcap +
                       hisp_anc_other*hisp_crime_hispcap + hisp_anc_none*hisp_crime_hispcap +
                       ##acs data
                       hisp_anc_only + hisp_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_hisp + pop_pct_hisp,
                     weights = weight,
                     data = analysis_hisp_total)
##for latex
stargazer(hisp_reg_hispcap,
          column.labels=c("hisp Identification"))
rm(hisp_reg_hispcap)
gc()

hisp_reg_hisp1000 <- lm(hisp ~ hisp_anc_only*hisp_crime_hisp1000 +
                       hisp_anc_other*hisp_crime_hisp1000 + hisp_anc_none*hisp_crime_hisp1000 +
                       ##acs data
                       hisp_anc_only + hisp_anc_other +
                       year + age + income + metro + own_home + woman +
                       married + us_born + english + above_ba + employed +
                       ##county demographics
                       pop_hisp + pop_pct_hisp,
                     weights = weight,
                     data = analysis_hisp_total)
##for latex
stargazer(hisp_reg_hisp1000,
          column.labels=c("hisp Identification"))
rm(hisp_reg_hisp1000)
gc()
# 
# hisp_reg_violent_any <- lm(hisp ~ hisp_anc_only*anti_hisp_violent_any +
#                             hisp_anc_other*anti_hisp_violent_any + hisp_anc_none*anti_hisp_violent_any +
#                             ##acs data
#                             hisp_anc_only + hisp_anc_other +
#                             year + age + income + metro + own_home + woman +
#                             married + us_born + english + above_ba + employed +
#                             ##county demographics
#                             pop_hisp + pop_pct_hisp,
#                             weights = weight,
#                             data = analysis_hisp_violent_year)
# ##for latex
# stargazer(hisp_reg_violent_any,
#           column.labels=c("hisp Identification"))
# rm(hisp_reg_violent_any)
# gc()
# 
# hisp_reg_sum <- lm(hisp ~ hisp_anc_only*anti_hisp_violent_sum +
#                      hisp_anc_other*anti_hisp_violent_sum + hisp_anc_none*anti_hisp_violent_sum + 
#                      ##acs data 
#                      hisp_anc_only + hisp_anc_other + 
#                      year + age + income + metro + own_home + woman +
#                      married + us_born + english + above_ba + employed +
#                      ##county demographics
#                      pop_hisp + pop_pct_hisp,
#                    weights = weight,
#                    data = analysis_hisp_violent_year)
# ##for latex
# stargazer(hisp_reg_sum, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_sum)
# gc()
# 
# hisp_reg_mean <- lm(hisp ~ hisp_anc_only*anti_hisp_violent_mean +
#                       hisp_anc_other*anti_hisp_violent_mean + hisp_anc_none*anti_hisp_violent_mean + 
#                       ##acs data 
#                       hisp_anc_only + hisp_anc_other + 
#                       year + age + income + metro + own_home + woman +
#                       married + us_born + english + above_ba + employed +
#                       ##county demographics
#                       pop_hisp + pop_pct_hisp,
#                     weights = weight,
#                     data = analysis_hisp_violent_year)
# ##for latex
# stargazer(hisp_reg_mean, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_mean)
# gc()
# 
# hisp_reg_1000 <- lm(hisp ~ hisp_anc_only*hisp_violent_crime_1000 +
#                       hisp_anc_other*hisp_violent_crime_1000 + hisp_anc_none*hisp_violent_crime_1000 + 
#                       ##acs data 
#                       hisp_anc_only + hisp_anc_other + 
#                       year + age + income + metro + own_home + woman +
#                       married + us_born + english + above_ba + employed +
#                       ##county demographics
#                       pop_hisp + pop_pct_hisp,
#                     weights = weight,
#                     data = analysis_hisp_violent_year)
# ##for latex
# stargazer(hisp_reg_1000, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_1000)
# gc()
# 
# hisp_reg_hispcap <- lm(hisp ~ hisp_anc_only*hisp_violent_crime_hispcap +
#                          hisp_anc_other*hisp_violent_crime_hispcap + hisp_anc_none*hisp_violent_crime_hispcap + 
#                          ##acs data 
#                          hisp_anc_only + hisp_anc_other + 
#                          year + age + income + metro + own_home + woman +
#                          married + us_born + english + above_ba + employed +
#                          ##county demographics
#                          pop_hisp + pop_pct_hisp,
#                        weights = weight,
#                        data = analysis_hisp_violent_year)
# ##for latex
# stargazer(hisp_reg_hispcap, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_hispcap)
# gc()
# 
# hisp_reg_hisp1000 <- lm(hisp ~ hisp_anc_only*hisp_violent_crime_hisp1000 +
#                           hisp_anc_other*hisp_violent_crime_hisp1000 + hisp_anc_none*hisp_violent_crime_hisp1000 + 
#                           ##acs data 
#                           hisp_anc_only + hisp_anc_other + 
#                           year + age + income + metro + own_home + woman +
#                           married + us_born + english + above_ba + employed +
#                           ##county demographics
#                           pop_hisp + pop_pct_hisp,
#                         weights = weight,
#                         data = analysis_hisp_violent_year)
# ##for latex
# stargazer(hisp_reg_hisp1000, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_hisp1000)
# gc()
# 
# 
# hisp_reg_total <- lm(hisp ~ hisp_anc_only*anti_hisp_violent_total +
#                        hisp_anc_other*anti_hisp_violent_total + hisp_anc_none*anti_hisp_violent_total + 
#                        ##acs data 
#                        hisp_anc_only + hisp_anc_other + 
#                        year + age + income + metro + own_home + woman +
#                        married + us_born + english + above_ba + employed +
#                        ##county demographics
#                        pop_hisp + pop_pct_hisp,
#                      weights = weight,
#                      data = analysis_hisp_violent_crime_total)
# ##for latex
# stargazer(hisp_reg_total, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_total)
# gc()
# 
# hisp_reg_1000 <- lm(hisp ~ hisp_anc_only*hisp_violent_crime_1000 +
#                       hisp_anc_other*hisp_violent_crime_1000 + hisp_anc_none*hisp_violent_crime_1000 + 
#                       ##acs data 
#                       hisp_anc_only + hisp_anc_other + 
#                       year + age + income + metro + own_home + woman +
#                       married + us_born + english + above_ba + employed +
#                       ##county demographics
#                       pop_hisp + pop_pct_hisp,
#                     weights = weight,
#                     data = analysis_hisp_violent_crime_total)
# ##for latex
# stargazer(hisp_reg_1000, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_1000)
# gc()
# 
# hisp_reg_hispcap <- lm(hisp ~ hisp_anc_only*hisp_violent_crime_hispcap +
#                          hisp_anc_other*hisp_violent_crime_hispcap + hisp_anc_none*hisp_violent_crime_hispcap + 
#                          ##acs data 
#                          hisp_anc_only + hisp_anc_other + 
#                          year + age + income + metro + own_home + woman +
#                          married + us_born + english + above_ba + employed +
#                          ##county demographics
#                          pop_hisp + pop_pct_hisp,
#                        weights = weight,
#                        data = analysis_hisp_violent_crime_total)
# ##for latex
# stargazer(hisp_reg_hispcap, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_hispcap)
# gc()
# 
# hisp_reg_hisp1000 <- lm(hisp ~ hisp_anc_only*hisp_violent_crime_hisp1000 +
#                           hisp_anc_other*hisp_violent_crime_hisp1000 + hisp_anc_none*hisp_violent_crime_hisp1000 + 
#                           ##acs data 
#                           hisp_anc_only + hisp_anc_other + 
#                           year + age + income + metro + own_home + woman +
#                           married + us_born + english + above_ba + employed +
#                           ##county demographics
#                           pop_hisp + pop_pct_hisp,
#                         weights = weight,
#                         data = analysis_hisp_violent_crime_total)
# ##for latex
# stargazer(hisp_reg_hisp1000, 
#           column.labels=c("hisp Identification")) 
# rm(hisp_reg_hisp1000)
# gc()

sink()