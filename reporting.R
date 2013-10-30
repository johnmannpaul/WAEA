#presumes you've run process.R
source("reporting-defs.R")


##achievement
paws.aggregates <- produce.aggregates.scoped(paws.df)

paws.N <- produce.aggregates.scoped(paws.df,
                             obs="WISER_ID",
                             value.label="N_TESTERS",
                             aggregator=function (x) c(N_TESTERS=length(unique(x)))
)



paws.aggregates.tab <- paws.aggregates$tab

paws.N.norm <- paws.N$norm

paws.tab.N <- merge(paws.aggregates.tab, paws.N.norm[as.character(paws.N.norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])

names(paws.tab.N)[ncol(paws.tab.N)] <- 'N'


#must aggree
head(paws.tab.N[paws.tab.N$SCHOOL_YEAR =='2012-13' & 
                  paws.tab.N$GRADE_ENROLLED == 'ALL' & paws.tab.N$STATISTIC=='PERCENT_PROFICIENT',],12)
head(achievement[achievement$SCHOOL_YEAR=='2012-13',],12)

paws.tab.N[paws.tab.N$SCHOOL_YEAR=='2012-13' & paws.tab.N$SCHOOL_ID=='0101001' & paws.tab.N$STATISTIC=='PERCENT_PROFICIENT',]
##end validation


     
#propagate results to paired schools
paws.tab.N <- rbind(paws.tab.N, propagate.to.paired.schools(paws.tab.N))
write.csv(paws.tab.N,file="reporting/grade-level-statistics.csv", na="", row.names=FALSE, quote=FALSE)



##MGP
growth.aggregates <- produce.aggregates.scoped(growth.df,
                                        obs="SGP",
                                        aggregator=function (x) 
                                          c(MGP=median(x),
                                            N_SGP=length(x)))

growth.N <- produce.aggregates.scoped(growth.df,
                               obs="WISER_ID",
                               value.label="N_TESTERS",
                               aggregator=function (x) c(N_TESTERS=length(unique(x)))
)


growth.tab.N <- merge(growth.aggregates$tab, growth.N$norm[as.character(growth.N$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(growth.tab.N)[length(growth.tab.N)] <- "N"


head(growth.tab.N[growth.tab.N$SCHOOL_YEAR =='2012-13' & 
                    growth.tab.N$GRADE_ENROLLED == 'ALL' &
                    growth.tab.N$STATISTIC == 'MGP',],12)
head(growth[growth$SCHOOL_YEAR=='2012-13',],12)

growth.tab.N[growth.tab.N$SCHOOL_YEAR==current.school.year & growth.tab.N$SCHOOL_ID=='0101001' & growth.tab.N$STATISTIC == 'MGP',]

##end validation



growth.tab.N <- rbind(growth.tab.N, propagate.to.paired.schools(growth.tab.N))
write.csv(growth.tab.N,file="reporting/grade-level-statistics-growth.csv", na="", row.names=FALSE, quote=FALSE)     


##equity     
growth.aggregates <- produce.aggregates.scoped(consolidated.subgroup.df,
                                        obs="MET_AGP",
                                        aggregator=function (x) {
                                          c(PERCENT_MET_AGP =round((sum(ifelse(x == 'T', 1, 0))/length(x)) * 100, 1),
                                            N_AGP=length(x),
                                            N_MET_AGP=sum(ifelse(x %in% c('3','4'), 1, 0)))
                                          },
                                               filter=bquote(!EXCLUDE_FROM_STATE_AVERAGE)
                                               
)

growth.N <- produce.aggregates.scoped(consolidated.subgroup.df,
                               obs="WISER_ID",
                               value.label="N_TESTERS",
                               aggregator=function (x) {
                                 c(N_TESTERS=length(unique(x)))
                               },
                                      filter=bquote(!EXCLUDE_FROM_STATE_AVERAGE)
)


growth.tab.N <- merge(growth.aggregates$tab, growth.N$norm[as.character(growth.N$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(growth.tab.N)[length(growth.tab.N)] <- "N"


head(growth.tab.N[growth.tab.N$SCHOOL_YEAR =='2012-13' & 
                    growth.tab.N$GRADE_ENROLLED == 'ALL' &
                    growth.tab.N$STATISTIC == 'PERCENT_MET_AGP',],12)
head(equity[equity$SCHOOL_YEAR=='2012-13',],12)


growth.tab.N[growth.tab.N$SCHOOL_YEAR==current.school.year & growth.tab.N$SCHOOL_ID=='0101001' & growth.tab.N$STATISTIC == 'PERCENT_MET_AGP',]

##end validation


growth.tab.N <- rbind(growth.tab.N, propagate.to.paired.schools(growth.tab.N))
write.csv(growth.tab.N,file="reporting/grade-level-statistics-equity.csv", na="", row.names=FALSE, quote=FALSE)

     
#write schools file
write.csv(cbind(schools[,c("SCHOOL_YEAR", "DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                     "SHORT_NAME", "LOW_GRADE",
                     "HIGH_GRADE", "GRADES_SERVED", "WAEA_SCHOOL_TYPE", "PAIRED_SCHOOL_ID", "PAIRED_SCHOOL_NAME")],
                ACCOUNTABILITY_SPL = ifelse(is.na(schools$ACCOUNTABILITY_SPL),
                                            NA,
                                            SPL.labels[schools[,"ACCOUNTABILITY_SPL"]])),file="reporting/schools.csv", 
                na="", row.names=FALSE, quote=FALSE)               

#write school indicators file
with(schools, write.csv(cbind(schools[,c("SCHOOL_YEAR", "SCHOOL_ID", "SMALL_SCHOOL")], 
                              ifelse(is.na(schools[,"YEARS_BACK"]),
                                     NA,
                                     ifelse(schools[,"YEARS_BACK"] < Inf,
                                            schools[,"YEARS_BACK"],
                                            NA)),
                              schools[,"YEARS_BACK_SUBGROUP"],
                              schools[,c("ACHIEVEMENT_CUT_1", "ACHIEVEMENT_CUT_2", "PERCENT_PROFICIENT", "N_ACHIEVEMENT")],
                              ACHIEVEMENT_TARGET_LEVEL = ifelse(is.na(schools[,c("ACHIEVEMENT_TARGET_LEVEL")]), 
                                                                NA, 
                                                                indicator.labels[schools[,c("ACHIEVEMENT_TARGET_LEVEL")]]),
                              schools[,c("GROWTH_CUT_1", "GROWTH_CUT_2",  "MGP", "N_GROWTH")],
                              GROWTH_TARGET_LEVEL = ifelse(is.na(schools[,c("GROWTH_TARGET_LEVEL")]), 
                                                           NA, 
                                                           indicator.labels[schools[,c("GROWTH_TARGET_LEVEL")]]),
                              schools[,c("EQUITY_CUT_1", "EQUITY_CUT_2",  "PERCENT_MEETING_AGP", "N_SUBGROUP")],
                              EQUITY_TARGET_LEVEL = ifelse(is.na(schools[,c("EQUITY_TARGET_LEVEL")]), 
                                                           NA, 
                                                           indicator.labels[schools[,c("EQUITY_TARGET_LEVEL")]]),
                              schools[,c("PARTICIPATION_RATE")], 
                              ifelse(is.na(schools[, "PARTICIPATION_RATE_LEVEL"]), 
                                     NA,
                                     participation.labels[schools[, "PARTICIPATION_RATE_LEVEL"]]),
                              schools[,"N_INDICATORS"],
                              SPL = ifelse(is.na(schools[,c("SPL")]), 
                                           NA, 
                                           SPL.labels[schools[,c("SPL")]]),
                              SPL_ADJUSTED = ifelse(is.na(schools[,c("SPL_ADJUSTED")]), 
                                                    NA, 
                                                    SPL.labels[schools[,c("SPL_ADJUSTED")]])),
                        file="reporting/school-indicators-nonHS.csv", na="", row.names=FALSE, quote=FALSE))

with(schools, write.csv(cbind(schools[,c("SCHOOL_YEAR", "SCHOOL_ID", "SMALL_SCHOOL_HS")], 
                              ifelse(is.na(schools[,"YEARS_BACK_HS"]),
                                     NA,
                                     ifelse(schools[,"YEARS_BACK_HS"] < Inf,
                                            schools[,"YEARS_BACK_HS"],
                                            NA)),
                              schools[,c("ACHIEVEMENT_CUT_1_HS", "ACHIEVEMENT_CUT_2_HS",  "PERCENT_PROFICIENT_HS", "N_ACHIEVEMENT_HS")],
                              ACHIEVEMENT_TARGET_LEVEL = ifelse(is.na(schools[,c("ACHIEVEMENT_TARGET_LEVEL_HS")]), 
                                                                NA, 
                                                                indicator.labels[schools[,c("ACHIEVEMENT_TARGET_LEVEL_HS")]]),
                              schools[,c("READINESS_CUT_1", "READINESS_CUT_2",  "TOTAL_READINESS_HS", 
                                         "N_TESTED_READINESS", "N_GRADUATION", "N_TOTAL_READINESS_HS")],
                              GROWTH_TARGET_LEVEL = ifelse(is.na(schools[,c("READINESS_TARGET_LEVEL")]), 
                                                           NA, 
                                                           indicator.labels[schools[,c("READINESS_TARGET_LEVEL")]]),
                              schools[,c("IMPROVEMENT_CUT_LOW_REVERSED", "IMPROVEMENT_CUT_HIGH_REVERSED",  "IMPROVEMENT_SCORE", "N_ACHIEVEMENT_HS")],
                              EQUITY_TARGET_LEVEL = ifelse(is.na(schools[,c("EQUITY_TARGET_LEVEL_HS")]), 
                                                           NA, 
                                                           indicator.labels[schools[,c("EQUITY_TARGET_LEVEL_HS")]]),
                              schools[,c("PARTICIPATION_RATE_ACHIEVEMENT_HS", "PARTICIPATION_RATE_TESTED_READINESS", "PARTICIPATION_RATE_HS")], 
                              ifelse(is.na(schools[, "PARTICIPATION_RATE_LEVEL_HS"]), 
                                     NA,
                                     participation.labels[schools[, "PARTICIPATION_RATE_LEVEL_HS"]]),
                              schools[,"N_INDICATORS_HS"],
                              SPL = ifelse(is.na(schools[,c("SPL_HS")]), 
                                           NA, 
                                           SPL.labels[schools[,c("SPL_HS")]]),
                              SPL_ADJUSTED = ifelse(is.na(schools[,c("SPL_ADJUSTED_HS")]), 
                                                    NA, 
                                                    SPL.labels[schools[,c("SPL_ADJUSTED_HS")]])),
                        file="reporting/school-indicators-HS.csv", na="", row.names=FALSE, quote=FALSE))


##act

act.report <- rbind(data.frame(act.df[,c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")], 
                               GRADE_ENROLLED = rep("11", nrow(act.df)), 
                               SUBJECT_CODE = rep('MA', nrow(act.df)),
                               TESTING_STATUS_CODE = act.df[,"TESTING_STATUS_CODE_MATH"],
                               ACCOUNTABILITY_PERF_LEVEL = act.df[,"WDE_PERFORMANCE_LEVEL_MATH"]),
                    data.frame(act.df[,c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")], 
                               GRADE_ENROLLED = rep("11", nrow(act.df)), 
                               SUBJECT_CODE = rep('RE', nrow(act.df)),
                               TESTING_STATUS_CODE = act.df[,"TESTING_STATUS_CODE_READING"],
                               ACCOUNTABILITY_PERF_LEVEL = act.df[,"WDE_PERFORMANCE_LEVEL_READING"]),
                    data.frame(act.df[,c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")], 
                               GRADE_ENROLLED = rep("11", nrow(act.df)), 
                               SUBJECT_CODE = rep('SC', nrow(act.df)),
                               TESTING_STATUS_CODE = act.df[,"TESTING_STATUS_CODE_SCIENCE"],
                               ACCOUNTABILITY_PERF_LEVEL = act.df[,"WDE_PERFORMANCE_LEVEL_SCIENCE"]))

nrow(act.report)     
act.report <- with(act.report, act.report[TESTING_STATUS_CODE=='T',])

nrow(act.report)          


act.aggregates <- produce.aggregates.scoped(act.report, 
                                     orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                      GRADE_ENROLLED = c("ALL", "11")))

act.N <- produce.aggregates.scoped(act.report,
                                   orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                    GRADE_ENROLLED = c("ALL", "11")),
                                   obs="WISER_ID",
                                   value.label="N_TESTERS",
                                   aggregator=function (x) c(N_TESTERS=length(unique(x)))
)


act.tab.N <- merge(act.aggregates$tab, act.N$norm[as.character(act.N$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(act.tab.N)[length(act.tab.N)] <- "N"

#kludge to get the participation rate in there
act.participation.agg <- act.participation[act.participation$SCHOOL_ID != state.school.id,c("SCHOOL_YEAR", "SCHOOL_ID", "PARTICIPATION_RATE_ACHIEVEMENT")]

names(act.participation.agg) <- c("SCHOOL_YEAR", "SCHOOL_ID", "PARTICIPATION_RATE")
act.participation.agg$SCOPE <- rep("SCHOOL", nrow(act.participation.agg))

act.participation.agg.state <- merge(act.participation.agg[,c("SCHOOL_YEAR", "SCHOOL_ID")],
                                     act.participation[act.participation$SCHOOL_ID == state.school.id,c("SCHOOL_YEAR", "PARTICIPATION_RATE_ACHIEVEMENT")])

names(act.participation.agg.state) <- c("SCHOOL_YEAR", "SCHOOL_ID", "PARTICIPATION_RATE")
act.participation.agg.state$SCOPE <- rep("STATE", nrow(act.participation.agg.state))

act.participation.agg <- rbind(act.participation.agg,act.participation.agg.state)

act.tab.N <- merge(act.tab.N, act.participation.agg)


head(act.tab.N[act.tab.N$SCHOOL_YEAR =='2012-13' & 
                 act.tab.N$GRADE_ENROLLED == 'ALL' & act.tab.N$STATISTIC=="PERCENT_PROFICIENT",],12)
head(act.tab.N[act.tab.N$SCHOOL_YEAR =='2012-13' & 
                 act.tab.N$GRADE_ENROLLED == '11' & act.tab.N$STATISTIC=="PERCENT_PROFICIENT",],12)
head(act.achievement[act.achievement$SCHOOL_YEAR=='2012-13',],12)



with(act.tab.N, act.tab.N[SCHOOL_ID %in% c('1101055') & STATISTIC=='PERCENT_PROFICIENT' & GRADE_ENROLLED=='11',])


write.csv(act.tab.N,file="reporting/grade-level-high-school-achievement.csv", na="", row.names=FALSE, quote=FALSE)


##hs equity

# equity.hs.report <- rbind(with(schools, schools[!is.na(EQUITY_TARGET_LEVEL_HS), 
#                                                 c("SCHOOL_YEAR", "SCHOOL_ID", "PERCENT_NONPROFICIENT_PRIOR", 
#                                                   "PERCENT_NONPROFICIENT", "IMPROVEMENT_SCORE", "N_EQUITY_HS", "N_ACHIEVEMENT_HS_PRIOR", "IMPROVEMENT_CATEGORY",
#                                                   "PERCENT_NONPROFICIENT_CATEGORY")]),
#                           with(state.school, state.school[!is.na(EQUITY_TARGET_LEVEL_HS), 
#                                                           c("SCHOOL_YEAR", "SCHOOL_ID", "PERCENT_NONPROFICIENT_PRIOR", 
#                                                             "PERCENT_NONPROFICIENT", "IMPROVEMENT_SCORE", "N_EQUITY_HS", "N_ACHIEVEMENT_HS_PRIOR", "IMPROVEMENT_CATEGORY",
#                                                             "PERCENT_NONPROFICIENT_CATEGORY")]))
# 
# equity.hs.report$IMPROVEMENT_CATEGORY <- ifelse(!is.na(equity.hs.report$IMPROVEMENT_CATEGORY), hs.equity.improve.labels[equity.hs.report$IMPROVEMENT_CATEGORY],
#                                                 na)
# 
# equity.hs.report$PERCENT_NONPROFICIENT_CATEGORY <- ifelse(!is.na(equity.hs.report$PERCENT_NONPROFICIENT_CATEGORY), 
#                                                           hs.equity.np.labels[equity.hs.report$PERCENT_NONPROFICIENT_CATEGORY],
#                                                 na)
# 
# equity.hs.report$GRADE_ENROLLED <- rep("11", nrow(equity.hs.report))
# 
# equity.hs.report$ORDER <- seq(1:nrow(equity.hs.report))
#                                
# with(equity.hs.report, equity.hs.report[SCHOOL_ID %in% c('1101055','7700000'), ])
# 
# write.csv(equity.hs.report[,c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "PERCENT_NONPROFICIENT_PRIOR", 
#                             "PERCENT_NONPROFICIENT", "IMPROVEMENT_SCORE",  "IMPROVEMENT_CATEGORY",
#                               "PERCENT_NONPROFICIENT_CATEGORY", "N_EQUITY_HS", "N_ACHIEVEMENT_HS_PRIOR","ORDER")], 
#           file="reporting/high-school-equity.csv", na="", row.names=FALSE, quote=FALSE)



equity.act.report <- act.report[act.report$SUBJECT_CODE %in% c('RE', 'MA'),]
equity.aggregates.np <- produce.aggregates.scoped(equity.act.report, 
                                        orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                      GRADE_ENROLLED = c("ALL", "11")),
                                        aggregator=function (x) 
                                          c(PERCENT_NONPROFICIENT =round((sum(ifelse(x %in% c('1','2'), 1, 0))/length(x)) * 100, 1),
                                            N_TESTS=length(x),
                                            N_NONPROFICIENT=sum(ifelse(x %in% c('1','2'), 1, 0))))

equity.act.N <- produce.aggregates.scoped(equity.act.report,
                                          orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                           GRADE_ENROLLED = c("ALL", "11")),
                                          obs="WISER_ID",
                                          value.label="N_TESTERS",
                                          aggregator=function (x) c(N_TESTERS=length(unique(x)))
)


equity.act.tab.N <- merge(equity.aggregates.np$tab, equity.act.N$norm[as.character(equity.act.N$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(equity.act.tab.N)[length(equity.act.tab.N)] <- "N"

equity.aggregates.np.prior <- produce.aggregates.scoped(paws_11.df, 
                                                 orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                                  GRADE_ENROLLED = c("ALL", "11")),
                                                 aggregator=function (x) 
                                                   c(PERCENT_NONPROFICIENT_PRIOR =round((sum(ifelse(x %in% c('1','2'), 1, 0))/length(x)) * 100, 1),
                                                     N_TESTS_PRIOR=length(x),
                                                     N_NONPROFICIENT_PRIOR=sum(ifelse(x %in% c('1','2'), 1, 0))))

equity.act.N.prior <- produce.aggregates.scoped(paws_11.df,
                                         orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                          GRADE_ENROLLED = c("ALL", "11")),
                                         obs="WISER_ID",
                                         value.label="N_TESTERS",
                                         aggregator=function (x) c(N_TESTERS=length(unique(x)))
)

equity.act.tab.N.prior <- merge(equity.aggregates.np.prior$tab, equity.act.N.prior$norm[as.character(equity.act.N.prior$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(equity.act.tab.N.prior)[length(equity.act.tab.N.prior)] <- "N"

equity.act.tab.N.prior$SCHOOL_YEAR <- sapply(equity.act.tab.N.prior$SCHOOL_YEAR, increment.school.year)

head(equity.act.tab.N[equity.act.tab.N$SCHOOL_YEAR =='2012-13' & 
                        equity.act.tab.N$GRADE_ENROLLED == 'ALL' & 
                        equity.act.tab.N$STATISTIC == 'PERCENT_NONPROFICIENT',],12)
head(equity.act.tab.N[equity.act.tab.N$SCHOOL_YEAR =='2012-13' & 
                        equity.act.tab.N$GRADE_ENROLLED == '11' & 
                        equity.act.tab.N$STATISTIC == 'PERCENT_NONPROFICIENT',],12)
head(hs.equity.df[hs.equity.df$SCHOOL_YEAR=='2012-13',c("SCHOOL_ID", 
                                                        "PERCENT_NONPROFICIENT", 
                                                        "N_ACHIEVEMENT", "PERCENT_PROFICIENT")],12)

head(equity.act.tab.N.prior[equity.act.tab.N.prior$SCHOOL_YEAR =='2012-13' & 
                              equity.act.tab.N.prior$GRADE_ENROLLED == '11' & 
                              equity.act.tab.N.prior$STATISTIC == 'PERCENT_NONPROFICIENT_PRIOR',],12)
head(hs.equity.df[hs.equity.df$SCHOOL_YEAR=='2012-13',c("SCHOOL_ID", 
                                                        "PERCENT_NONPROFICIENT_PRIOR", 
                                                        "N_ACHIEVEMENT_PRIOR")],12)

equity.act.tab.delta <- merge(with(equity.act.tab.N, 
                                   equity.act.tab.N[STATISTIC=='PERCENT_NONPROFICIENT',c("SCOPE","SCHOOL_YEAR",
                                                                                          "GRADE_ENROLLED",
                                                                                         "SCHOOL_ID",
                                                                                         "ALL",
                                                                                         "RE",
                                                                                         "MA",
                                                                                         "SC",
                                                                                         "ORDER")]),
                              with(equity.act.tab.N.prior, 
                                   equity.act.tab.N.prior[STATISTIC=='PERCENT_NONPROFICIENT_PRIOR',c("SCOPE", "SCHOOL_YEAR",
                                                                                                     "GRADE_ENROLLED",
                                                                                                     "SCHOOL_ID",
                                                                                                     "ALL",
                                                                                                     "RE",
                                                                                                     "MA",
                                                                                                     "SC",
                                                                                                     "ORDER")]),
                              by=c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID"),
                              all.x=TRUE)

equity.act.tab.delta <- cbind(equity.act.tab.delta, 
                              data.frame(t(apply(equity.act.tab.delta[, c("ALL.x", "RE.x", "MA.x", "SC.x",
                                                                          "ALL.y", "RE.y", "MA.y", "SC.y")],
                                                 c(1),
                                                 function (school) {
                                                   all <- ifelse(!is.na(school[["ALL.y"]]),
                                                                 school[["ALL.x"]] - school[["ALL.y"]],
                                                                 NA)
                                                   re <- ifelse(!is.na(school[["RE.y"]]),
                                                                school[["RE.x"]] - school[["RE.y"]],
                                                                NA)
                                                   ma <- ifelse(!is.na(school[["MA.y"]]),
                                                          school[["MA.x"]] - school[["MA.y"]],
                                                          NA)
                                                   sc <- ifelse(!is.na(school[["SC.y"]]),
                                                                school[["SC.x"]] - school[["SC.y"]],
                                                                NA)
                                                   c(ALL=all, RE=re, MA=ma, SC=sc)
                                                 }))))

equity.act.tab.delta$STATISTIC <- rep("DELTA_NONPROFICIENT", nrow(equity.act.tab.delta))
equity.act.tab.delta$N <-  rep(NA, nrow(equity.act.tab.delta))
equity.act.tab.delta$ORDER <- equity.act.tab.delta$ORDER.x  
equity.act.tab.delta <- equity.act.tab.delta[,c("SCOPE", "SCHOOL_YEAR", 
                                                "GRADE_ENROLLED",
                                                "SCHOOL_ID",
                                                "STATISTIC",
                                                "ALL",
                                                "RE",
                                                "MA",
                                                "SC",
                                                "ORDER",   
                                                "N")]

equity.stats <- rbind(equity.act.tab.N,
                      equity.act.tab.N.prior,
                      equity.act.tab.delta)

write.csv(equity.stats, 
          file="reporting/high-school-equity.csv", na="", row.names=FALSE, quote=FALSE)
#hs readiness


readiness.hs.report <- rbind(with(schools, schools[!is.na(TOTAL_READINESS_HS), 
                                                   c("SCHOOL_YEAR", "SCHOOL_ID", "TESTED_READINESS", 
                                                     "N_TESTED_READINESS", "SCHOOL_GRADUATION_INDEX", "N_GRADUATION")]),
                             with(state.school, state.school[!is.na(TOTAL_READINESS_HS), 
                                                             c("SCHOOL_YEAR", "SCHOOL_ID", "TESTED_READINESS", 
                                                               "N_TESTED_READINESS", "SCHOOL_GRADUATION_INDEX", "N_GRADUATION")]))


readiness.hs.report <- cbind(readiness.hs.report, data.frame(TESTED_READINESS_WEIGHT=rep(hs.readiness.weights["TESTED_READINESS"], nrow(readiness.hs.report)),
                                                             GRADUATION_INDEX_WEIGHT=rep(hs.readiness.weights["SCHOOL_GRADUATION_INDEX"], nrow(readiness.hs.report))))


readiness.hs.report$TESTED_READINESS_WEIGHTED <- readiness.hs.report$TESTED_READINESS * readiness.hs.report$TESTED_READINESS_WEIGHT
readiness.hs.report$SCHOOL_GRADUATION_INDEX_WEIGHTED <- readiness.hs.report$SCHOOL_GRADUATION_INDEX * readiness.hs.report$GRADUATION_INDEX_WEIGHT

readiness.hs.report$ORDER <- seq(1:nrow(readiness.hs.report))

write.csv(readiness.hs.report,
          file="reporting/high-school-readiness.csv", na="", row.names=FALSE, quote=FALSE)


##tested readiness reports

readiness.tested.students.df.agg <- readiness.tested.students.df
readiness.tested.students.df.agg$TEST_DATA <- paste(readiness.tested.students.df.agg$TEST_TYPE, 
                                                    readiness.tested.students.df.agg$RAW_SCORE,
                                                sep="-")

readiness.tested.students.df.agg$GRADE_ENROLLED <- ifelse(readiness.tested.students.df.agg$TEST_TYPE == 'EXPLORE',
                                                          '09',
                                                          ifelse(readiness.tested.students.df.agg$TEST_TYPE == 'PLAN',
                                                                 '10',
                                                                 '11'))

tested.readiness.agg <- produce.aggregates.scoped(readiness.tested.students.df.agg[,c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "TEST_TYPE", "TEST_DATA")], 
                                                  ids.fixed= list(SCHOOL = c("SCHOOL_YEAR", "SCHOOL_ID"),
                                                                 STATE = c("SCHOOL_YEAR")),
                                                  id.choices = c(GRADE_ENROLLED="ALL", TEST_TYPE="ALL"),
                                                  orderings = list(TEST_TYPE = c("ALL", "EXPLORE","PLAN","ACT","PAWS Alternate"),
                                                                   GRADE_ENROLLED = c("ALL", "09", "10", "11")),
                                                  col.vars = list(TEST_TYPE=c("EXPLORE","PLAN","ACT","PAWS Alternate")),
                                                  obs="TEST_DATA",
                                                  value.label = NA,
                                                  aggregator=function (x) {
                                                    index.by.test.type = list(ACT=act_index,
                                                                              EXPLORE=explore_index,
                                                                              `PAWS Alternate`=alt_index,
                                                                              PLAN=plan_index)
                                                    
                                                    eval.test <- function (test) {
                                                      test.type <- strsplit(test,'-')[[1]][1]
                                                      test.score <- strsplit(test,'-')[[1]][2]                                                      
                                                      index <- index.by.test.type[[test.type]]
                                                      if (is.null(index))
                                                        NA
                                                      else {
                                                        
                                                        index[test.score]
                                                      }
                                                      
                                                    }
                                                    
                                                    
                                                    indexed.scores = sapply(x, eval.test)
                                                    c(AVERAGE_INDEX=round(mean(indexed.scores, na.rm=TRUE),1),
                                                      N_TESTS=length(which(!is.na(indexed.scores))))
                                                    
                                                  })


tested.readiness.agg.N <- produce.aggregates.scoped(readiness.tested.students.df.agg[,c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "TEST_TYPE", "TEST_DATA")], 
                                                    ids.fixed= list(SCHOOL = c("SCHOOL_YEAR", "SCHOOL_ID"),
                                                                    STATE = c("SCHOOL_YEAR")),
                                                    id.choices = c(GRADE_ENROLLED="ALL", TEST_TYPE="ALL"),
                                                    orderings = list(TEST_TYPE = c("ALL", "EXPLORE","PLAN","ACT","PAWS Alternate"),
                                                                     GRADE_ENROLLED = c("ALL", "09", "10", "11")),
                                                    col.vars = list(TEST_TYPE=c("EXPLORE","PLAN","ACT","PAWS Alternate")),
                                                    obs="TEST_DATA",
                                                    value.label="N_TESTERS",
                                                    aggregator=function (x) c(N_TESTERS=length(x))
)


tested.readiness.tab.N <- merge(tested.readiness.agg$tab, tested.readiness.agg.N$norm[as.character(tested.readiness.agg.N$norm$TEST_TYPE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "VALUE")])


names(tested.readiness.tab.N)[length(tested.readiness.tab.N)] <- "N"

#validation
head(with(tested.readiness.tab.N, tested.readiness.tab.N[GRADE_ENROLLED=='ALL' & STATISTIC=='AVERAGE_INDEX',]))

with(tested.readiness.tab.N, tested.readiness.tab.N[GRADE_ENROLLED=='ALL' & STATISTIC=='AVERAGE_INDEX' & SCHOOL_ID=='1301057',])
#end validation

readiness.participation.df.agg <- readiness.participation.df
readiness.participation.df.agg$GRADE_ENROLLED <- ifelse(readiness.participation.df.agg$TEST_TYPE == 'EXPLORE',
                                                       '09',
                                                       ifelse(readiness.participation.df.agg$TEST_TYPE == 'PLAN',
                                                              '10',
                                                              '11'))

tested.readiness.part.agg <- produce.aggregates.scoped(readiness.participation.df.agg[,c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "TEST_TYPE", "TESTING_STATUS_CODE")], 
                                                  ids.fixed= list(SCHOOL = c("SCHOOL_YEAR", "SCHOOL_ID"),
                                                                  STATE = c("SCHOOL_YEAR")),
                                                  id.choices = c(GRADE_ENROLLED="ALL", TEST_TYPE="ALL"),
                                                  orderings = list(TEST_TYPE = c("ALL", "EXPLORE","PLAN","ACT","PAWS Alternate"),
                                                                   GRADE_ENROLLED = c("ALL", "09", "10", "11")),
                                                  col.vars = list(TEST_TYPE=c("EXPLORE","PLAN","ACT","PAWS Alternate")),
                                                  obs="TESTING_STATUS_CODE",
                                                  value.label = "PARTICIPATION_RATE",
                                                  aggregator=function (x) {
                                                    
                                                    round((length(x[which(x=='T')])/length(x))*100, 1)
                                                    
                                                  })


tested.readiness.part.agg.N <- produce.aggregates.scoped(readiness.participation.df.agg[,c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "TEST_TYPE", "TESTING_STATUS_CODE")], 
                                                    ids.fixed= list(SCHOOL = c("SCHOOL_YEAR", "SCHOOL_ID"),
                                                                    STATE = c("SCHOOL_YEAR")),
                                                    id.choices = c(GRADE_ENROLLED="ALL", TEST_TYPE="ALL"),
                                                    orderings = list(TEST_TYPE = c("ALL", "EXPLORE","PLAN","ACT","PAWS Alternate"),
                                                                     GRADE_ENROLLED = c("ALL", "09", "10", "11")),
                                                    col.vars = list(TEST_TYPE=c("EXPLORE","PLAN","ACT","PAWS Alternate")),
                                                    obs="TESTING_STATUS_CODE",
                                                    value.label="N_TESTERS",
                                                    aggregator=function (x) c(N_TESTERS=length(x))
)


tested.readiness.part.tab.N <- merge(tested.readiness.part.agg$tab, tested.readiness.part.agg.N$norm[as.character(tested.readiness.part.agg.N$norm$TEST_TYPE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "VALUE")])


names(tested.readiness.part.tab.N)[length(tested.readiness.part.tab.N)] <- "N"

#validation
head(with(tested.readiness.part.tab.N, tested.readiness.part.tab.N[GRADE_ENROLLED=='ALL' & STATISTIC=='PARTICIPATION_RATE',]))

with(tested.readiness.part.tab.N, tested.readiness.part.tab.N[GRADE_ENROLLED=='ALL' & STATISTIC=='PARTICIPATION_RATE' & SCHOOL_ID=='1301057',])


tested.readiness.tab.all <- rbind(with(tested.readiness.tab.N, tested.readiness.tab.N[GRADE_ENROLLED=='ALL' & STATISTIC=='AVERAGE_INDEX',]),
                                  with(tested.readiness.part.tab.N, tested.readiness.part.tab.N[GRADE_ENROLLED=='ALL' & STATISTIC=='PARTICIPATION_RATE',]))

with(tested.readiness.tab.all, tested.readiness.tab.all[GRADE_ENROLLED=='ALL' & SCHOOL_ID=='1301057',])

write.csv(with(tested.readiness.tab.all, tested.readiness.tab.all[GRADE_ENROLLED=='ALL',c(setdiff(names(tested.readiness.tab.all), "GRADE_ENROLLED"))]),
          file="reporting/high-school-act.csv", na="", row.names=FALSE, quote=FALSE)









