source("mean-z-score-fun.R")
load(file="data/ACT/plan.Rdata")
plan.prior.year <- plan[plan$SCHOOL_YEAR==prior.school.year,] 

#In 2012-13 there was only one  point difference between 35%  (16) and 50% (17)
quantile(as.numeric(plan.prior.year$PLAN_SCALE_MATHEMATICS), probs=c(0.35, 0.5), na.rm=TRUE, type=6)
table(as.numeric(plan.prior.year$PLAN_SCALE_MATHEMATICS))
table(ecdf(as.numeric(plan.prior.year$PLAN_SCALE_MATHEMATICS))(as.numeric(plan.prior.year$PLAN_SCALE_MATHEMATICS)))

quantile(as.numeric(plan.prior.year$PLAN_SCALE_READING), probs=c(0.35, 0.5), na.rm=TRUE, type=6)
table(as.numeric(plan.prior.year$PLAN_SCALE_READING))
table(ecdf(as.numeric(plan.prior.year$PLAN_SCALE_READING))(as.numeric(plan.prior.year$PLAN_SCALE_READING)))


plan.prior.year<- cbind(plan.prior.year[,!(names(plan.prior.year) %in% subgroup.labels.hs)], 
                        data.frame(t(apply(plan.prior.year[,c("PLAN_SCALE_MATHEMATICS", 
                                                              "PLAN_SCALE_READING")],
                                           c(1),
                                           function (row) {
                                             scores <- as.numeric(row)
                                             math.score <- scores[1]
                                             reading.score <- scores[2]
                                             
                                             below.scores <- as.numeric(scores < c(subgroup.hs.math.cut,
                                                                                   subgroup.hs.reading.cut))
                                             
                                             below.scores.NA <- sum(is.na(below.scores))                                                              
                                             below.scores.nonNA <-  sum(below.scores[which(!is.na(below.scores))])
                                             
                                             below.scores <- c(below.scores, ifelse(below.scores.NA == 2, NA, as.numeric(below.scores.nonNA > 0)))
                                             names(below.scores) <- subgroup.labels.hs
                                             
                                             below.scores
                                           } ))))

#cross tab
table(plan.prior.year$SUBGROUP_MATH_HS, plan.prior.year$SUBGROUP_READING_HS, useNA="ifany")
table(plan.prior.year$SUBGROUP_CONSOLIDATED_HS)

#Use last year's Plan scores for prototyping in lieu of having available the standard progression 
#of Prior Plan -> Currrent ACT
act.current.year <- merge(plan.prior.year, 
                          schools[schools$SCHOOL_YEAR==current.school.year,]["SCHOOL_ID"])
act.current.year$SCHOOL_YEAR <- current.school.year
act.current.year$TESTING_STATUS_CODE_MATHEMATICS <- act.current.year$TESTING_STATUS_CODE_COMPOSITE #there were no status codes for READING and no MATH
act.current.year$TESTING_STATUS_CODE_READING <- act.current.year$TESTING_STATUS_CODE_COMPOSITE
act.current.year.z <- calc.z.scores(act.current.year, subject.labels=c(MATH="MATHEMATICS", READING="READING"),                                      
                                    scale.score.prefix='PLAN_SCALE', 
                                    z.score.prefix='ACT_Z_SCORE', baseline.stats=high.school.baseline.achievement.stats)

act.current.year <- cbind(act.current.year, calc.z.scores(act.current.year, subject.labels=c(MATH="MATHEMATICS", READING="READING"),                                      
                                                          scale.score.prefix='PLAN_SCALE', 
                                                          z.score.prefix='ACT_Z_SCORE', baseline.stats=high.school.baseline.achievement.stats))


mean(act.current.year$ACT_Z_SCORE_MATHEMATICS, na.rm=TRUE)
mean(act.current.year$ACT_Z_SCORE_READING, na.rm=TRUE)
sd(act.current.year$ACT_Z_SCORE_MATHEMATICS, na.rm=TRUE)
sd(act.current.year$ACT_Z_SCORE_READING, na.rm=TRUE)



nrow(act.current.year)
act.current.year.subgroup <- merge(act.current.year, plan.prior.year[!is.na(plan.prior.year$SUBGROUP_CONSOLIDATED_HS) & 
                                                                                    plan.prior.year$SUBGROUP_CONSOLIDATED_HS==1,]["WISER_ID"])
nrow(act.current.year.subgroup)

#participation rates of the consolidated subroup on this year's test
act.participation.consolidated.subgroup <- calc.participation.rate(act.current.year.subgroup, subject.labels=c("MATHEMATICS", "READING"),                                      
                                                                   total.participation.labels = c("EQUITY_HS_TESTS_ACTUAL_COUNT", 
                                                                                                  "EQUITY_HS_TESTS_EXPECTED_COUNT", 
                                                                                                  "EQUITY_HS_PARTICIPATION_RATE")) 

act.participation.consolidated.subgroup <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types, 
                                                         c("SCHOOL_YEAR", "SCHOOL_ID")],
                                                 
                                                 act.participation.consolidated.subgroup,
                                                 all=TRUE)

table(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types,]$SCHOOL_YEAR)
table(with(act.participation.consolidated.subgroup,
           act.participation.consolidated.subgroup[!(SCHOOL_ID %in% state.school.id),]$SCHOOL_YEAR))

with(act.participation.consolidated.subgroup,
     act.participation.consolidated.subgroup[SCHOOL_YEAR==current.school.year & 
                                               is.na(PARTICIPATION_RATE_EQUITY_HS),])
act.participation.consolidated.subgroup[,c("MATHEMATICS_TESTED", 
                                           "MATHEMATICS_PARTICIPANTS", 
                                           "READING_TESTED", 
                                           "READING_PARTICIPANTS", 
                                           "EQUITY_HS_TESTS_ACTUAL_COUNT", 
                                           "EQUITY_HS_TESTS_EXPECTED_COUNT")] <- 
  data.frame(t(apply(act.participation.consolidated.subgroup[,c("MATHEMATICS_TESTED", 
                                                                "MATHEMATICS_PARTICIPANTS", 
                                                                "READING_TESTED", 
                                                                "READING_PARTICIPANTS", 
                                                                "EQUITY_HS_TESTS_ACTUAL_COUNT", 
                                                                "EQUITY_HS_TESTS_EXPECTED_COUNT")],
                     c(1),
                     function (row) {
                       if (sum(is.na(row)) == length(row))
                         rep(0, length(row))
                       else
                         row                       
                     })))



#reduce to FAY only
table(act.current.year.subgroup$SCHOOL_FULL_ACADEMIC_YEAR, useNA="ifany")
act.current.year.subgroup.fay <- act.current.year.subgroup[act.current.year.subgroup$SCHOOL_FULL_ACADEMIC_YEAR=='T',]
nrow(act.current.year.subgroup.fay)


#calculate the Accountability N of the consolidated subgroup

act.current.year.subgroup.fay.equity <- calc.mean.score(act.current.year.subgroup.fay,
                                                        subject.labels=c(MATH="MATHEMATICS", READING="READING"),
                                                        testing.status.prefix="TESTING_STATUS_CODE",
                                                        z.score.prefix="ACT_Z_SCORE")

names(act.current.year.subgroup.fay.equity) <- c("SCHOOL_YEAR", "SCHOOL_ID", "EQUITY_HS",  "N_EQUITY")

#Small schools are determined based on N_EQUITY being less than the minimum N for equity.
act.current.year.subgroup.fay.equity$SMALL_SCHOOL <- ifelse(act.current.year.subgroup.fay.equity$N_EQUITY < min.N.equity.hs, 'T', 'F') 
#Normally there will be a lookback for small schools, but in 2013-14 there is no data available for this.
act.current.year.subgroup.fay.equity$YEARS_BACK <- ifelse(act.current.year.subgroup.fay.equity$N_EQUITY < min.N.equity.hs, Inf, NA) 


plot.hist(act.current.year.subgroup.fay.equity[act.current.year.subgroup.fay.equity$SCHOOL_ID != state.school.id, "EQUITY_HS"], 20)

#state score should be consistent with the following 
mean(act.current.year.subgroup.fay$ACT_Z_SCORE_MATHEMATICS, na.rm=TRUE)
mean(act.current.year.subgroup.fay$ACT_Z_SCORE_READING, na.rm=TRUE)
sd(act.current.year.subgroup.fay$ACT_Z_SCORE_MATHEMATICS, na.rm=TRUE)
sd(act.current.year.subgroup.fay$ACT_Z_SCORE_READING, na.rm=TRUE)


act.current.year.subgroup.fay.equity <- merge(act.current.year.subgroup.fay.equity,
                                              act.participation.consolidated.subgroup[,c("SCHOOL_YEAR", "SCHOOL_ID",                                                                                         
                                                                                         "EQUITY_HS_TESTS_ACTUAL_COUNT", 
                                                                                         "EQUITY_HS_TESTS_EXPECTED_COUNT",
                                                                                         "EQUITY_HS_PARTICIPATION_RATE")],
                                              by=c("SCHOOL_YEAR", "SCHOOL_ID"), all.y=TRUE)


#N_EQUITY and ACHIEVEMENT_TESTED_EQUITY_HS are not necessarily identical: N_EQUITY is filtered to FAY 
#status, but ACHIEVEMENT_TESTED_EQUITY_HS is not.
head(act.current.year.subgroup.fay.equity[act.current.year.subgroup.fay.equity$SCHOOL_YEAR==current.school.year,])
table(act.current.year.subgroup.fay.equity$EQUITY_HS_PARTICIPATION_RATE, useNA="ifany")
quantile(with(act.current.year.subgroup.fay.equity,
              act.current.year.subgroup.fay.equity[SCHOOL_YEAR == current.school.year & 
                          !is.na(N_EQUITY) &                           
                          N_EQUITY >= min.N.equity.hs &
                          !is.na(EQUITY_HS_PARTICIPATION_RATE) &  
                          EQUITY_HS_PARTICIPATION_RATE >= .90 & 
                          SCHOOL_ID != state.school.id,]$EQUITY_HS), 
         probs=c(.35,.65),
         type=6)

#apply cuts based on the above

act.current.year.subgroup.fay.equity$EQUITY_TARGET_LEVEL <- apply(hs.equity.df[,c("IMPROVEMENT_CATEGORY", "PERCENT_NONPROFICIENT_CATEGORY")],
                                          c(1),
                                          function (values) {
                                            score <- values[["IMPROVEMENT_CATEGORY"]] 
                                            as.numeric(score)
                                          })

 
##assign the relavent values to the schools fram
schools <- schools[, !(names(schools) %in% hs.equity.labels)]

schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                        "SCHOOL_YEAR", "SMALL_SCHOOL_HS")], c(1), 
                                             FUN=calc_hs_equity))))  

#names(schools)[(length(schools) - length(hs.equity.labels) + 1):length(schools)] <- hs.equity.labels
head(schools)
table(schools$EQUITY_TARGET_LEVEL_HS)

