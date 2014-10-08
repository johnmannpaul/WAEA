#presumes you've run process.R
source("reporting-defs.R")
report.precision <- 0

##achievement

paws.aggregates <- produce.aggregates.scoped(achievement.g38.indicator$students.fay,
                                             orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "WR"),                                                 
                                                              GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),
                                             col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "WR")),
                                             value.label = "PERCENT_PROFICIENT",
                                             aggregator=function (x) {
                                               PERCENT_PROFICIENT =round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, report.precision)
                                             }
                                             obs="PERFORMANCE_LEVEL",
                                             filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL))


paws.N <- produce.aggregates.scoped(achievement.g38.indicator$students.fay,
                                    orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "WR"),                                                 
                                                     GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),
                                    col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "WR")),
                                    obs="WISER_ID",
                                    value.label="N_TESTERS",
                                    aggregator=function (x) c(N_TESTERS=length(unique(x))),
                                    filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
)



paws.aggregates.tab <- paws.aggregates$tab

paws.N.norm <- paws.N$norm

paws.tab.N <- merge(paws.aggregates.tab, paws.N.norm[as.character(paws.N.norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])

names(paws.tab.N)[ncol(paws.tab.N)] <- 'N'


#must aggree
head(paws.tab.N[paws.tab.N$SCHOOL_YEAR ==current.school.year & 
                  paws.tab.N$GRADE_ENROLLED == 'ALL' & paws.tab.N$STATISTIC=='PERCENT_PROFICIENT',],12)

head(with(achievement.g38.indicator$schools, 
          achievement.g38.indicator$schools[WAEA_SCHOOL_TYPE %in% nonHS.types & SCHOOL_YEAR==current.school.year,c("SCHOOL_ID", achievement.g38.labels)]),12)


##end validation


     
#propagate results to paired schools
paws.tab.N <- rbind(paws.tab.N, propagate.to.paired.schools(paws.tab.N))
write.csv(paws.tab.N[paws.tab.N$SCHOOL_YEAR==current.school.year & paws.tab.N$STATISTIC=='PERCENT_PROFICIENT', ],file="reporting/grade-level-statistics.csv", na="", row.names=FALSE, quote=FALSE)



##MGP
growth.aggregates <- produce.aggregates.scoped(growth.g38.indicator$students.fay,
                                               orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "WR"),                                                 
                                                                GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),
                                               col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "WR")),
                                               obs="SGP",
                                               aggregator=function (x) 
                                                 c(MGP=median(x),
                                                   N_SGP=length(x)),
                                               filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
                                               )

growth.N <- produce.aggregates.scoped(growth.g38.indicator$students.fay,
                                      orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "WR"),                                                 
                                                       GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),
                                      col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "WR")),       
                                      obs="WISER_ID",
                                      value.label="N_TESTERS",
                                      aggregator=function (x) c(N_TESTERS=length(unique(x))),
                                      filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
)


growth.tab.N <- merge(growth.aggregates$tab, growth.N$norm[as.character(growth.N$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(growth.tab.N)[length(growth.tab.N)] <- "N"


head(growth.tab.N[growth.tab.N$SCHOOL_YEAR ==current.school.year & 
                    growth.tab.N$GRADE_ENROLLED == 'ALL' &
                    growth.tab.N$STATISTIC == 'MGP',],12)
head(with(growth.g38.indicator$schools, 
          growth.g38.indicator$schools[WAEA_SCHOOL_TYPE %in% nonHS.types & SCHOOL_YEAR==current.school.year,c("SCHOOL_ID", growth.labels)]),12)


##end validation



growth.tab.N <- rbind(growth.tab.N, propagate.to.paired.schools(growth.tab.N))
write.csv(growth.tab.N[growth.tab.N$SCHOOL_YEAR==current.school.year & growth.tab.N$STATISTIC=='MGP',],file="reporting/grade-level-statistics-growth.csv", na="", row.names=FALSE, quote=FALSE)     


##equity     
equity.aggregates <- produce.aggregates.scoped(equity.g38.indicator$students.fay,
                                               orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "WR"),                                                 
                                                                GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),
                                               col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "WR")),
                                               obs="STD_SCORE",
                                               value.label="MEAN_STD_SCORE",
                                               aggregator=function (x) c(MEAN_STD_SCORE=round(mean(x), 1)),
                                               filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
                                               
)

equity.N <- produce.aggregates.scoped(equity.g38.indicator$students.fay,
                                      orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "WR"),                                                 
                                                       GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),
                                      col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "WR")),
                                      obs="WISER_ID",
                                      value.label="N_TESTERS",
                                      aggregator=function (x) {
                                        c(N_TESTERS=length(unique(x)))
                                      },
                                      filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
)


equity.tab.N <- merge(equity.aggregates$tab, equity.N$norm[as.character(equity.N$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(equity.tab.N)[length(equity.tab.N)] <- "N"


head(equity.tab.N[equity.tab.N$SCHOOL_YEAR ==current.school.year & 
                    equity.tab.N$GRADE_ENROLLED == 'ALL' &
                    equity.tab.N$STATISTIC == 'MEAN_STD_SCORE',],12)
head(with(equity.g38.indicator$schools, 
          equity.g38.indicator$schools[WAEA_SCHOOL_TYPE %in% nonHS.types & SCHOOL_YEAR==current.school.year,c("SCHOOL_ID", equity.g38.labels)]),12)


equity.tab.N[equity.tab.N$SCHOOL_YEAR==current.school.year & equity.tab.N$SCHOOL_ID=='0101001' & equity.tab.N$STATISTIC == 'MEAN_STD_SCORE',]

##end validation


equity.tab.N <- rbind(equity.tab.N, propagate.to.paired.schools(equity.tab.N))
write.csv(equity.tab.N[equity.tab.N$SCHOOL_YEAR==current.school.year & equity.tab.N$STATISTIC=='MEAN_STD_SCORE',],file="reporting/grade-level-statistics-equity.csv", na="", row.names=FALSE, quote=FALSE)

     
#write schools file
write.csv(cbind(schools[schools$SCHOOL_YEAR==current.school.year,c("SCHOOL_YEAR", "DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                     "SHORT_NAME", "LOW_GRADE",
                     "HIGH_GRADE", "GRADES_SERVED", "WAEA_SCHOOL_TYPE", "PAIRED_SCHOOL_ID", "PAIRED_SCHOOL_NAME")],
                ACCOUNTABILITY_SPL = ifelse(is.na(schools[, "ALL_SPL_ACCOUNTABILITY"]),
                                            NA,
                                            SPL.labels[schools[, "ALL_SPL_ACCOUNTABILITY"]])),file="reporting/schools.csv", 
                na="", row.names=FALSE, quote=FALSE)               

#write school indicators file
g38.schools <- schools[schools$SCHOOL_YEAR==current.school.year & schools$WAEA_SCHOOL_TYPE %in% c(nonHS.types, paired.types),]

g38.school.indicators <- with(g38.schools, cbind(g38.schools[,c("SCHOOL_YEAR", "SCHOOL_ID", "G38_ACHIEVEMENT_ALL_SMALL_SCHOOL")], 
                                                           G38_ACHIEVEMENT_ALL_YEARS_BACK = ifelse(is.na(g38.schools[,"G38_ACHIEVEMENT_ALL_YEARS_BACK"]),
                                                                                                   NA,
                                                                                                   ifelse(g38.schools[,"G38_ACHIEVEMENT_ALL_YEARS_BACK"] < Inf,
                                                                                                          g38.schools[,"G38_ACHIEVEMENT_ALL_YEARS_BACK"],
                                                                                                          NA)),
                                                           ACHIEVEMENT_CUT_1 = g38.achievement.cuts[1],
                                                           ACHIEVEMENT_CUT_2 = g38.achievement.cuts[2],
                                                           g38.schools[,c("G38_ACHIEVEMENT_ALL_N_TESTS", "G38_ACHIEVEMENT_ALL_N_PROFICIENT_TESTS", "G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT", "G38_ACHIEVEMENT_ALL_N",
                                                                          "G38_ACHIEVEMENT_ALL_TESTS_ACTUAL_COUNT", "G38_ACHIEVEMENT_ALL_TESTS_EXPECTED_COUNT", "G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE")],
                                                           
                                                           
                                                           ACHIEVEMENT_TARGET_LEVEL = ifelse(is.na(g38.schools[,c("G38_ACHIEVEMENT_ALL_TARGET_LEVEL")]), 
                                                                                             NA, 
                                                                                             indicator.labels[g38.schools[,c("G38_ACHIEVEMENT_ALL_TARGET_LEVEL")]]),
                                                           
                                                           g38.schools[,"G38_GROWTH_SMALL_SCHOOL"],
                                                           G38_GROWTH_YEARS_BACK = ifelse(is.na(g38.schools[,"G38_GROWTH_YEARS_BACK"]),
                                                                                          NA,
                                                                                          ifelse(g38.schools[,"G38_GROWTH_YEARS_BACK"] < Inf,
                                                                                                 g38.schools[,"G38_GROWTH_YEARS_BACK"],
                                                                                                 NA)),
                                                           GROWTH_CUT_1 = g38.growth.cuts[1],
                                                           GROWTH_CUT_2 = g38.growth.cuts[2],
                                                           
                                                           g38.schools[,c("G38_GROWTH_MGP", "G38_GROWTH_N")],
                                                           
                                                           GROWTH_TARGET_LEVEL = ifelse(is.na(g38.schools[,c("G38_GROWTH_TARGET_LEVEL")]), 
                                                                                        NA, 
                                                                                        indicator.labels[g38.schools[,c("G38_GROWTH_TARGET_LEVEL")]]),
                                                           
                                                           g38.schools[,"G38_EQUITY_SMALL_SCHOOL"],
                                                           G38_EQUITY_YEARS_BACK = ifelse(is.na(g38.schools[,"G38_EQUITY_YEARS_BACK"]),
                                                                                          NA,
                                                                                          ifelse(g38.schools[,"G38_EQUITY_YEARS_BACK"] < Inf,
                                                                                                 g38.schools[,"G38_EQUITY_YEARS_BACK"],
                                                                                                 NA)),
                                                           EQUITY_CUT_1 = g38.equity.cuts[1],
                                                           EQUITY_CUT_2 = g38.equity.cuts[2],
                                                           
                                                           g38.schools[,c("G38_EQUITY_MEAN", "G38_EQUITY_N", 
                                                                          "G38_EQUITY_TESTS_ACTUAL_COUNT", "G38_EQUITY_TESTS_EXPECTED_COUNT", "G38_EQUITY_PARTICIPATION_RATE")],
                                                           
                                                           EQUITY_TARGET_LEVEL = ifelse(is.na(g38.schools[,c("G38_EQUITY_TARGET_LEVEL")]), 
                                                                                        NA, 
                                                                                        indicator.labels[g38.schools[,c("G38_EQUITY_TARGET_LEVEL")]]),
                                                           
                                                           g38.schools[,c("G38_INDICATORS_N", "G38_PARTICIPATION_RATE")],
                                                           G38_PARTICIPATION_RATE_CAT = ifelse(is.na(g38.schools[, "G38_PARTICIPATION_RATE_CAT"]), 
                                                                                               NA,
                                                                                               participation.labels[g38.schools[, "G38_PARTICIPATION_RATE_CAT"]]),
                                                           SPL = ifelse(is.na(g38.schools[,c("G38_SPL")]), 
                                                                        NA, 
                                                                        SPL.labels[g38.schools[,c("G38_SPL")]]),
                                                           SPL_ACCOUNTABILITY = ifelse(is.na(g38.schools[,c("G38_SPL_ACCOUNTABILITY")]), 
                                                                                       NA, 
                                                                                       SPL.labels[g38.schools[,c("G38_SPL_ACCOUNTABILITY")]])))

write.csv(g38.school.indicators, file="reporting/school-indicators-nonHS.csv", na="", row.names=FALSE, quote=FALSE)

high.schools <- schools[schools$SCHOOL_YEAR==current.school.year & schools$WAEA_SCHOOL_TYPE %in% HS.types,]
high.school.indicators <- with(high.schools, cbind(high.schools[,c("SCHOOL_YEAR", "SCHOOL_ID", "HS_ACHIEVEMENT_SMALL_SCHOOL")], 
                                                   HS_ACHIEVEMENT_YEARS_BACK = ifelse(is.na(high.schools[,"HS_ACHIEVEMENT_YEARS_BACK"]),
                                                                                      NA,
                                                                                      ifelse(high.schools[,"HS_ACHIEVEMENT_YEARS_BACK"] < Inf,
                                                                                             high.schools[,"HS_ACHIEVEMENT_YEARS_BACK"],
                                                                                             NA)),
                                                   HS_ACHIEVEMENT_CUT_1_HS = hs.achievement.cuts[1],
                                                   HS_ACHIEVEMENT_CUT_2_HS = hs.achievement.cuts[2],
                                                   high.schools[,c("HS_ACHIEVEMENT_N_TESTS", "HS_ACHIEVEMENT_N_PROFICIENT_TESTS", "HS_ACHIEVEMENT_PERCENT_PROFICIENT", "HS_ACHIEVEMENT_N",
                                                                   "HS_ACHIEVEMENT_TESTS_ACTUAL_COUNT", "HS_ACHIEVEMENT_TESTS_EXPECTED_COUNT", "HS_ACHIEVEMENT_PARTICIPATION_RATE")],
                                                   ACHIEVEMENT_TARGET_LEVEL = ifelse(is.na(high.schools[,c("HS_ACHIEVEMENT_TARGET_LEVEL")]), 
                                                                                     NA, 
                                                                                     indicator.labels[high.schools[,c("HS_ACHIEVEMENT_TARGET_LEVEL")]]),
                                                   high.schools["HS_EQUITY_SMALL_SCHOOL"],
                                                   HS_EQUITY_YEARS_BACK = ifelse(is.na(high.schools[,"HS_EQUITY_YEARS_BACK"]),
                                                                                      NA,
                                                                                      ifelse(high.schools[,"HS_EQUITY_YEARS_BACK"] < Inf,
                                                                                             high.schools[,"HS_EQUITY_YEARS_BACK"],
                                                                                             NA)),
                                                   HS_EQUITY_CUT_1_HS = hs.equity.cuts[1],
                                                   HS_EQUITY_CUT_2_HS = hs.equity.cuts[2],
                                                   high.schools[,c("HS_EQUITY_MEAN", "HS_EQUITY_N", 
                                                                   "HS_EQUITY_TESTS_ACTUAL_COUNT", "HS_EQUITY_TESTS_EXPECTED_COUNT", "HS_EQUITY_PARTICIPATION_RATE")],
                                                   EQUITY_TARGET_LEVEL = ifelse(is.na(high.schools[,c("HS_EQUITY_TARGET_LEVEL")]), 
                                                                                     NA, 
                                                                                     indicator.labels[high.schools[,c("HS_EQUITY_TARGET_LEVEL")]]),
                                                   
                                                   high.schools[c("GRAD_RATE_4_YR.2012.13", "COHORT_4_YR_N.2012.13", "GRAD_RATE_EXTENDED", "COHORT_EXTENDED_N.2012.13", "IMPROVEMENT_TARGET")],
                                                   GRAD_RATE_TARGET_LEVEL = ifelse(is.na(high.schools[,c("IMPROVE_CAT_2013")]), 
                                                                                   NA, 
                                                                                   indicator.labels[high.schools[,c("IMPROVE_CAT_2013")]]),
                                                   high.schools[c("SMALL_SCHOOL_GRADE_NINE_CREDIT", "GRADE_NINE_CREDITS_N", "GRADE_NINE_CREDITS_MET_N", "PERCENT_GD_9_CREDIT_MET", "REQUIRED_GRAD_CREDITS")],
                                                   PERCENT_GD_9_CREDIT_MET_WEIGHTED = round(additional.readiness.weights["grade.nine"] * high.schools[["PERCENT_GD_9_CREDIT_MET"]],1),
                                                   high.schools[c("SMALL_SCHOOL_HATH_ELIGIBILITY", "HATH_INDEX_SCORE_N", "HATH_INDEX_SCORE_MEAN")],
                                                   HATH_INDEX_SCORE_MEAN_WEIGHTED = round(additional.readiness.weights["hathaway"] * high.schools[["HATH_INDEX_SCORE_MEAN"]],1),
                                                   high.schools[c("HS_TESTED_READINESS_SMALL_SCHOOL" )],
                                                   HS_TESTED_READINESS_YEARS_BACK = ifelse(is.na(high.schools[,"HS_TESTED_READINESS_YEARS_BACK"]),
                                                                                           NA,
                                                                                           ifelse(high.schools[,"HS_TESTED_READINESS_YEARS_BACK"] < Inf,
                                                                                                  high.schools[,"HS_TESTED_READINESS_YEARS_BACK"],
                                                                                                  NA)),
                                                   high.schools[c("HS_TESTED_READINESS_MEAN", "HS_TESTED_READINESS_N",
                                                                  "HS_TESTED_READINESS_TESTS_ACTUAL_COUNT", "HS_TESTED_READINESS_TESTS_EXPECTED_COUNT",
                                                                  "HS_TESTED_READINESS_PARTICIPATION_RATE")],
                                                   HS_TESTED_READINESS_WEIGHTED = round(additional.readiness.weights["tested"] * high.schools[["HS_TESTED_READINESS_MEAN"]],1),
                                                   high.schools[c("HS_ADD_READINESS_TYPE_LABEL", "HS_ADD_READINESS_CUT1", "HS_ADD_READINESS_CUT2",
                                                                  "HS_ADD_READINESS_SCORE")],
                                                   HS_ADD_READINESS_CAT = ifelse(is.na(high.schools[,c("HS_ADD_READINESS_CAT")]), 
                                                                                 NA, 
                                                                                 indicator.labels[high.schools[,c("HS_ADD_READINESS_CAT")]]),
                                                   high.schools[c("HS_ACHIEVEMENT_INDICATORS_N", "HS_READINESS_INDICATORS_N", "HS_INDICATORS_N")],
                                                   HS_OVERALL_READINESS = ifelse(is.na(high.schools[,c("HS_OVERALL_READINESS")]), 
                                                                                 NA, 
                                                                                 indicator.labels[high.schools[,c("HS_OVERALL_READINESS")]]),
                                                   HS_OVERALL_ACHIEVEMENT = ifelse(is.na(high.schools[,c("HS_OVERALL_ACHIEVEMENT")]), 
                                                                                   NA, 
                                                                                   indicator.labels[high.schools[,c("HS_OVERALL_ACHIEVEMENT")]]),
                                                   high.schools["HS_PARTICIPATION_RATE"],
                                                   HS_PARTICIPATION_RATE_CAT = ifelse(is.na(high.schools[, "HS_PARTICIPATION_RATE_CAT"]), 
                                                                                      NA,
                                                                                      participation.labels[high.schools[,"HS_PARTICIPATION_RATE_CAT"]]),
                                                   HS_SPL = ifelse(is.na(high.schools[,c("HS_SPL")]), 
                                                                   NA, 
                                                                   SPL.labels[high.schools[,c("HS_SPL")]]),
                                                   HS_SPL_ACCOUNTABILITY = ifelse(is.na(high.schools[,c("HS_SPL_ACCOUNTABILITY")]), 
                                                                                  NA, 
                                                                                  SPL.labels[high.schools[,c("HS_SPL_ACCOUNTABILITY")]])))

write.csv(high.school.indicators, file="reporting/school-indicators-HS.csv", na="", row.names=FALSE, quote=FALSE)


##act
achievement.act.students <- achievement.hs.indicator$students.fay

table(achievement.act.students$SUBJECT_CODE, useNA="ifany")

#encode wr as engwri for purposes of totals
achievement.act.students$SUBJECT_CODE <- ifelse(achievement.act.students$SUBJECT_CODE == 'WR', 'ENGWRI', achievement.act.students$SUBJECT_CODE)

table(achievement.act.students$SUBJECT_CODE, useNA="ifany")
act.aggregates <- produce.aggregates.scoped(achievement.act.students,
                                             orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "ENGWRI"),                                                 
                                                              GRADE_ENROLLED = c("ALL", "11")),
                                             col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "ENGWRI")),
                                             obs="PERFORMANCE_LEVEL",
                                            filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL))



act.N <- produce.aggregates.scoped(achievement.act.students,
                                   orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "ENGWRI"),                                                 
                                                    GRADE_ENROLLED = c("ALL", "11")),
                                   col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "ENGWRI")),
                                   obs="WISER_ID",
                                   aggregator=function (x) c(N_TESTERS=length(unique(x))),
                                   filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
)


act.tab.N <- merge(act.aggregates$tab, act.N$norm[as.character(act.N$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(act.tab.N)[length(act.tab.N)] <- "N"


head(act.tab.N[act.tab.N$SCHOOL_YEAR ==current.school.year & 
                 act.tab.N$GRADE_ENROLLED == 'ALL' & act.tab.N$STATISTIC=="PERCENT_PROFICIENT",],12)
head(act.tab.N[act.tab.N$SCHOOL_YEAR ==current.school.year & 
                 act.tab.N$GRADE_ENROLLED == '11' & act.tab.N$STATISTIC=="PERCENT_PROFICIENT",],12)
head(with(achievement.hs.indicator$schools,
          achievement.hs.indicator$schools[WAEA_SCHOOL_TYPE %in% HS.types & SCHOOL_YEAR==current.school.year,
                                           c("SCHOOL_ID", achievement.hs.labels)]),12)




write.csv(act.tab.N[act.tab.N$SCHOOL_YEAR==current.school.year & act.tab.N$GRADE_ENROLLED=='ALL' &
                      act.tab.N$STATISTIC=='PERCENT_PROFICIENT', !(names(act.tab.N) %in% "GRADE_ENROLLED")],file="reporting/grade-level-high-school-achievement.csv", na="", row.names=FALSE, quote=FALSE)



hs.equity.students <- equity.hs.indicator$students.fay
hs.equity.students$SUBJECT_CODE <- ifelse(hs.equity.students$SUBJECT == 'Math', 'MA', 'RE')
hs.equity.students$GRADE_ENROLLED <- '11'
hs.equity.students$WY_ACT_SCALE_SCORE <- as.numeric(hs.equity.students$WY_ACT_SCALE_SCORE)

hs.equity.aggregates <- produce.aggregates.scoped(hs.equity.students,
                                                  orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "ENGWRI"),                                                 
                                                                   GRADE_ENROLLED = c("ALL", "11")),
                                                  col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "ENGWRI")),
                                                  obs="WY_ACT_SCALE_SCORE",
                                                  value.label="MEAN_WY_ACT_SCORE",
                                                  aggregator=function (x) c(MEAN_WY_ACT_SCORE=round(mean(x), 1)),
                                                  filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
                                               
)

hs.equity.N <- produce.aggregates.scoped(hs.equity.students,
                                         orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "ENGWRI"),                                                 
                                                          GRADE_ENROLLED = c("ALL", "11")),
                                         col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "ENGWRI")),
                                      obs="WISER_ID",
                                      value.label="N_TESTERS",
                                      aggregator=function (x) {
                                        c(N_TESTERS=length(unique(x)))
                                      },
                                      filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
)


hs.equity.tab.N <- merge(hs.equity.aggregates$tab, 
                         hs.equity.N$norm[as.character(hs.equity.N$norm$SUBJECT_CODE)=='ALL',
                                          c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])



names(hs.equity.tab.N)[length(hs.equity.tab.N)] <- "N"



head(hs.equity.tab.N[hs.equity.tab.N$SCHOOL_YEAR ==current.school.year & 
                       hs.equity.tab.N$GRADE_ENROLLED == 'ALL' & hs.equity.tab.N$STATISTIC=="MEAN_WY_ACT_SCORE",],12)
head(hs.equity.tab.N[hs.equity.tab.N$SCHOOL_YEAR ==current.school.year & 
                       hs.equity.tab.N$GRADE_ENROLLED == '11' & hs.equity.tab.N$STATISTIC=="MEAN_WY_ACT_SCORE",],12)

head(with(equity.hs.indicator$schools,
          equity.hs.indicator$schools[WAEA_SCHOOL_TYPE %in% HS.types & SCHOOL_YEAR==current.school.year,
                                           c("SCHOOL_ID", equity.hs.labels)]),12)




write.csv(hs.equity.tab.N[hs.equity.tab.N$SCHOOL_YEAR==current.school.year & hs.equity.tab.N$GRADE_ENROLLED=='ALL' &
                            STATISTIC=='MEAN_WY_ACT_SCORE', !(names(hs.equity.tab.N) %in% "GRADE_ENROLLED")],file="reporting/grade-level-high-school-equity.csv", na="", row.names=FALSE, quote=FALSE)

#hs readiness



hs.tested.aggregates <- produce.aggregates.scoped(tested.readiness.indicator$students.fay,
                                                  id.choices = c(GRADE_ENROLLED="ALL", TEST_TYPE="ALL"),
                                                  orderings = list(TEST_TYPE = c("ALL", "EXPLORE","PLAN","ACT", "ALT"),                                                 
                                                                   GRADE_ENROLLED = c("ALL", "09", "10", "11")),
                                                  col.vars = list(TEST_TYPE=c("EXPLORE","PLAN","ACT", "ALT")),
                                                  obs="TESTED_READINESS_INDEX_SCORE",
                                                  value.label="TESTED_READINESS_MEAN_INDEX_SCORE",
                                                  aggregator=function (x) c(MEAN_INDEX_SCORE=round(mean(x), 1)),
                                                  filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
                                                  
)


hs.tested.N <- produce.aggregates.scoped(tested.readiness.indicator$students.fay,
                                         id.choices = c(GRADE_ENROLLED="ALL", TEST_TYPE="ALL"),
                                         orderings = list(TEST_TYPE = c("ALL", "EXPLORE","PLAN","ACT", "ALT"),                                                 
                                                          GRADE_ENROLLED = c("ALL", "09", "10", "11")),
                                         col.vars = list(TEST_TYPE=c("EXPLORE","PLAN","ACT", "ALT")),
                                         obs="WISER_ID",
                                         value.label="N_TESTERS",
                                         aggregator=function (x) {
                                           c(N_TESTERS=length(unique(x)))
                                         },
                                         filter=quote(SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL)
)



hs.tested.tab.N <- merge(hs.tested.aggregates$tab, 
                         hs.tested.N$norm[as.character(hs.tested.N$norm$TEST_TYPE)=='ALL',
                                          c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])



names(hs.tested.tab.N)[length(hs.tested.tab.N)] <- "N"



head(hs.tested.tab.N[hs.tested.tab.N$SCHOOL_YEAR ==current.school.year & 
                       hs.tested.tab.N$GRADE_ENROLLED == 'ALL' & hs.tested.tab.N$STATISTIC=="TESTED_READINESS_MEAN_INDEX_SCORE",],12)
head(hs.tested.tab.N[hs.tested.tab.N$SCHOOL_YEAR ==current.school.year & hs.tested.tab.N$STATISTIC=="TESTED_READINESS_MEAN_INDEX_SCORE",],12)

head(with(tested.readiness.indicator$schools,
          tested.readiness.indicator$schools[WAEA_SCHOOL_TYPE %in% HS.types & SCHOOL_YEAR==current.school.year,
                                      c("SCHOOL_ID", tested.readiness.labels)]),12)


write.csv(hs.tested.tab.N[hs.tested.tab.N$SCHOOL_YEAR==current.school.year & hs.tested.tab.N$GRADE_ENROLLED=='ALL', !(names(hs.tested.tab.N) %in% "GRADE_ENROLLED")],
          file="reporting/grade-level-high-school-tested-readiness.csv", na="", row.names=FALSE, quote=FALSE)



#high school tested readiness participation

tested.readiness.students <- readiness.all.df
tested.readiness.students$TEST_TYPE <- ifelse(grepl("Alternate$", tested.readiness.students$TEST_TYPE),
                                              "ALT",
                                              tested.readiness.students$TEST_TYPE)

hs.tested.part.aggregates <- produce.aggregates.scoped(tested.readiness.students,
                                                  id.choices = c(GRADE_ENROLLED="ALL", TEST_TYPE="ALL"),
                                                  orderings = list(TEST_TYPE = c("ALL", "EXPLORE","PLAN","ACT", "ALT"),                                                 
                                                                   GRADE_ENROLLED = c("ALL", "09", "10", "11")),
                                                  col.vars = list(TEST_TYPE=c("EXPLORE","PLAN","ACT", "ALT")),
                                                  obs="TESTING_STATUS_CODE",
                                                  value.label="TESTED_READINESS_PARTICIPATION",
                                                  aggregator=function (x) c(TESTED_READINESS_PARTICIPATION=round(100*(length(which(x=='T'))/
                                                                                                     length(which(x %in% c('T','N')))), 1))
                                                  
)

hs.tested.part.N <- produce.aggregates.scoped(tested.readiness.students[tested.readiness.students$TESTING_STATUS_CODE != 'X',],
                                              id.choices = c(GRADE_ENROLLED="ALL", TEST_TYPE="ALL"),
                                              orderings = list(TEST_TYPE = c("ALL", "EXPLORE","PLAN","ACT", "ALT"),                                                 
                                                               GRADE_ENROLLED = c("ALL", "09", "10", "11")),
                                              col.vars = list(TEST_TYPE=c("EXPLORE","PLAN","ACT", "ALT")),
                                              obs="WISER_ID",
                                              value.label="N_TESTERS",
                                              aggregator=function (x) {
                                                c(N_TESTERS=length(unique(x)))
                                              }
)

hs.tested.part.tab.N <- merge(hs.tested.part.aggregates$tab, 
                              hs.tested.part.N$norm[as.character(hs.tested.part.N$norm$TEST_TYPE)=='ALL',
                                          c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])



names(hs.tested.part.tab.N)[length(hs.tested.part.tab.N)] <- "N"




head(hs.tested.part.tab.N[hs.tested.part.tab.N$SCHOOL_YEAR ==current.school.year & 
                            hs.tested.part.tab.N$GRADE_ENROLLED == 'ALL' & hs.tested.part.tab.N$STATISTIC=="TESTED_READINESS_PARTICIPATION",],12)

head(with(tested.readiness.indicator$schools,
          tested.readiness.indicator$schools[WAEA_SCHOOL_TYPE %in% HS.types & SCHOOL_YEAR==current.school.year,
                                             c("SCHOOL_ID", tested.readiness.labels)]),12)

with(hs.tested.part.tab.N, hs.tested.part.tab.N[SCHOOL_ID %in% c('1101055') & STATISTIC=='TESTED_READINESS_PARTICIPATION' & GRADE_ENROLLED=='ALL',])

with(hs.tested.part.tab.N, hs.tested.part.tab.N[SCHOOL_ID %in% c('0101055') & STATISTIC=='TESTED_READINESS_PARTICIPATION' & GRADE_ENROLLED=='ALL',])

write.csv(hs.tested.part.tab.N[hs.tested.part.tab.N$SCHOOL_YEAR==current.school.year & hs.tested.part.tab.N$GRADE_ENROLLED=='ALL', !(names(hs.tested.part.tab.N) %in% "GRADE_ENROLLED")],
          file="reporting/high-school-tested-readiness-participation.csv", na="", row.names=FALSE, quote=FALSE)






#participation rates for the consolidated subgroup


act.current.year.subgroup.part <- act.current.year.subgroup
act.current.year.subgroup.part$GRADE_ENROLLED <- "11"
act.current.year.subgroup.part$SUBJECT_CODE <- ifelse(act.current.year.subgroup.part$SUBJECT == 'Reading', 'RE', 'MA')
hs.subgroup.part.aggregates <- produce.aggregates.scoped(act.current.year.subgroup.part,
                                                         orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "ENGWRI"),                                                 
                                                                          GRADE_ENROLLED = c("ALL", "11")),
                                                         col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "ENGWRI")),
                                                         obs="TESTING_STATUS_CODE",
                                                         value.label="HS_SUBGROUP_PARTICIPATION",
                                                         aggregator=function (x) c(HS_SUBGROUP_PARTICIPATION=round(100*(length(which(x=='T'))/
                                                                                                                             length(which(x %in% c('T','N')))), 1))
                                                       
)

hs.subgroup.part.N <- produce.aggregates.scoped(act.current.year.subgroup.part[act.current.year.subgroup.part$TESTING_STATUS_CODE != 'X',],
                                              orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "ENGWRI"),                                                 
                                                               GRADE_ENROLLED = c("ALL", "11")),
                                              col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "ENGWRI")),
                                              obs="WISER_ID",
                                              value.label="N_TESTERS",
                                              aggregator=function (x) {
                                                c(N_TESTERS=length(unique(x)))
                                              }
)

hs.subgroup.part.tab.N <- merge(hs.subgroup.part.aggregates$tab, 
                                hs.subgroup.part.N$norm[as.character(hs.subgroup.part.N$norm$SUBJECT_CODE)=='ALL',
                                                    c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])



names(hs.subgroup.part.tab.N)[length(hs.subgroup.part.tab.N)] <- "N"


head(hs.subgroup.part.tab.N[hs.subgroup.part.tab.N$SCHOOL_YEAR ==current.school.year & 
                              hs.subgroup.part.tab.N$GRADE_ENROLLED == 'ALL' & hs.subgroup.part.tab.N$STATISTIC=="HS_SUBGROUP_PARTICIPATION",],12)

head(with(equity.hs.indicator$schools,
          equity.hs.indicator$schools[WAEA_SCHOOL_TYPE %in% HS.types & SCHOOL_YEAR==current.school.year,
                                             c("SCHOOL_ID", equity.hs.labels)]),12)


write.csv(hs.subgroup.part.tab.N[hs.subgroup.part.tab.N$SCHOOL_YEAR==current.school.year & 
                                   hs.subgroup.part.tab.N$GRADE_ENROLLED=='ALL', !(names(hs.subgroup.part.tab.N) %in% "GRADE_ENROLLED")],
          file="reporting/high-school-subgroup-participation.csv", na="", row.names=FALSE, quote=FALSE)


#high school act participation

achievement.act.students.part <- act.achieve

table(achievement.act.students.part$SUBJECT_CODE, useNA="ifany")

#encode wr as engwri for purposes of totals
achievement.act.students.part$SUBJECT_CODE <- ifelse(achievement.act.students.part$SUBJECT_CODE == 'WR', 
                                                     'ENGWRI', achievement.act.students.part$SUBJECT_CODE)

table(achievement.act.students.part$SUBJECT_CODE, useNA="ifany")

hs.act.part.aggregates <- produce.aggregates.scoped(achievement.act.students.part,
                                                         orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "ENGWRI"),                                                 
                                                                          GRADE_ENROLLED = c("ALL", "11")),
                                                         col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "ENGWRI")),
                                                         obs="TESTING_STATUS_CODE",
                                                         value.label="HS_ACT_PARTICIPATION",
                                                         aggregator=function (x) c(HS_ACT_PARTICIPATION=round(100*(length(which(x=='T'))/
                                                                                                                          length(which(x %in% c('T','N')))), 1))
                                                         
)

hs.act.part.N <- produce.aggregates.scoped(achievement.act.students[achievement.act.students.part$TESTING_STATUS_CODE != 'X',],
                                                orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC", "ENGWRI"),                                                 
                                                                 GRADE_ENROLLED = c("ALL", "11")),
                                                col.vars = list(SUBJECT_CODE=c("RE","MA","SC", "ENGWRI")),
                                                obs="WISER_ID",
                                                value.label="N_TESTERS",
                                                aggregator=function (x) {
                                                  c(N_TESTERS=length(unique(x)))
                                                }
)

hs.act.part.tab.N <- merge(hs.act.part.aggregates$tab, 
                                hs.act.part.N$norm[as.character(hs.act.part.N$norm$SUBJECT_CODE)=='ALL',
                                                        c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])



names(hs.act.part.tab.N)[length(hs.act.part.tab.N)] <- "N"


head(hs.act.part.tab.N[hs.act.part.tab.N$SCHOOL_YEAR ==current.school.year & 
                         hs.act.part.tab.N$GRADE_ENROLLED == 'ALL' & hs.act.part.tab.N$STATISTIC=="HS_ACT_PARTICIPATION",],12)

head(with(achievement.hs.indicator$schools,
          achievement.hs.indicator$schools[WAEA_SCHOOL_TYPE %in% HS.types & SCHOOL_YEAR==current.school.year,
                                      c("SCHOOL_ID", achievement.hs.labels)]),12)

write.csv(hs.act.part.tab.N[hs.act.part.tab.N$SCHOOL_YEAR==current.school.year & 
                              hs.act.part.tab.N$GRADE_ENROLLED=='ALL', !(names(hs.act.part.tab.N) %in% "GRADE_ENROLLED")],
          file="reporting/high-school-act-participation.csv", na="", row.names=FALSE, quote=FALSE)
