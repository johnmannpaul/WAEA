source("mean-z-score-fun.R")
load(file="data/ACT/plan.Rdata")
load(file="data/ACT/act.achieve.Rdata")
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
act.current.year <- act.achieve[,c("SCHOOL_YEAR", "SCHOOL_ID",
                                   "WISER_ID", "SCHOOL_FULL_ACADEMIC_YEAR",
                                   "TESTING_STATUS_CODE_MATH", "TESTING_STATUS_CODE_READING",
                                   "WY_SCORE_MATH", "WY_SCORE_READING")]




nrow(act.current.year)

act.current.year.subgroup <- merge(act.current.year, plan.prior.year[!is.na(plan.prior.year$SUBGROUP_CONSOLIDATED_HS) & 
                                                                                    plan.prior.year$SUBGROUP_CONSOLIDATED_HS==1,]["WISER_ID"])
nrow(act.current.year.subgroup)

#participation rates of the consolidated subroup on this year's test
act.participation.consolidated.subgroup <- calc.participation.rate(act.current.year.subgroup, subject.labels=c("MATH", "READING"),                                      
                                                                   total.participation.labels = c("EQUITY_HS_TESTS_ACTUAL_COUNT", 
                                                                                                  "EQUITY_HS_TESTS_EXPECTED_COUNT", 
                                                                                                  "EQUITY_HS_PARTICIPATION_RATE")) 
table(findInterval(act.participation.consolidated.subgroup$EQUITY_HS_PARTICIPATION_RATE,
             c(90, 95)) + 1)

nrow(act.participation.consolidated.subgroup)
act.participation.consolidated.subgroup <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types, 
                                                         c("SCHOOL_YEAR", "SCHOOL_ID")],
                                                 
                                                 act.participation.consolidated.subgroup,
                                                 all=TRUE)
nrow(act.participation.consolidated.subgroup)
table(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types,]$SCHOOL_YEAR)
table(with(act.participation.consolidated.subgroup,
           act.participation.consolidated.subgroup[!(SCHOOL_ID %in% state.school.id),]$SCHOOL_YEAR))

#Which schools don't have a consolidated subgroup
with(act.participation.consolidated.subgroup,
     act.participation.consolidated.subgroup[SCHOOL_YEAR==current.school.year & 
                                               is.na(EQUITY_HS_PARTICIPATION_RATE),])

#assign zero N counts for schools that had and empty consolidated subgroup
act.participation.consolidated.subgroup[,c("MATH_TESTED", 
                                           "MATH_PARTICIPANTS", 
                                           "READING_TESTED", 
                                           "READING_PARTICIPANTS", 
                                           "EQUITY_HS_TESTS_ACTUAL_COUNT", 
                                           "EQUITY_HS_TESTS_EXPECTED_COUNT")] <- 
  zero.na.rows(act.participation.consolidated.subgroup,
               c("MATH_TESTED", 
                 "MATH_PARTICIPANTS", 
                 "READING_TESTED", 
                 "READING_PARTICIPANTS", 
                 "EQUITY_HS_TESTS_ACTUAL_COUNT", 
                 "EQUITY_HS_TESTS_EXPECTED_COUNT"))



#reduce to FAY only
table(act.current.year.subgroup$SCHOOL_FULL_ACADEMIC_YEAR, useNA="ifany")
act.current.year.subgroup.fay <- act.current.year.subgroup[act.current.year.subgroup$SCHOOL_FULL_ACADEMIC_YEAR=='T',]
nrow(act.current.year.subgroup.fay)

#do small schools
small.equity.hs <- calc.small.schools(act.current.year.subgroup.fay, schools, HS.types, "WISER_ID", attribute="EQUITY_HS")
schools <- schools[,!(names(schools) %in% setdiff(names(small.equity.hs$result.schools), names(schools)))]
schools <- cbind(schools, small.equity.hs$result.schools[,setdiff(names(small.equity.hs$result.schools), names(schools))])
act.current.year.subgroup.fay$SCHOOL_YEAR_ORIGINAL <- act.current.year.subgroup.fay$SCHOOL_YEAR
act.current.year.subgroup.fay <- rbind(act.current.year.subgroup.fay, small.equity.hs$result.students)


#calculate the Accountability N of the consolidated subgroup

act.current.year.subgroup.fay.equity <- calc.mean.score(act.current.year.subgroup.fay,
                                                        subject.labels=c(MATH="MATH", READING="READING"),
                                                        testing.status.prefix="TESTING_STATUS_CODE",
                                                        score.prefix="WY_SCORE",
                                                        agg.function=function (g) round(mean(g),0))

names(act.current.year.subgroup.fay.equity) <- c("SCHOOL_YEAR", "SCHOOL_ID", "EQUITY_HS",  "N_EQUITY_HS")


#attach participation rates.  The left outerness of this join will bring in schools that had no full year participants
act.current.year.subgroup.fay.equity <- merge(act.current.year.subgroup.fay.equity,
                                              act.participation.consolidated.subgroup[,c("SCHOOL_YEAR", "SCHOOL_ID",                                                                                         
                                                                                         "EQUITY_HS_TESTS_ACTUAL_COUNT", 
                                                                                         "EQUITY_HS_TESTS_EXPECTED_COUNT",
                                                                                         "EQUITY_HS_PARTICIPATION_RATE")],
                                              by=c("SCHOOL_YEAR", "SCHOOL_ID"), all.y=TRUE)
#set anyone's N_EQUITY_HS who didn't have any full year participants to 0
act.current.year.subgroup.fay.equity$N_EQUITY_HS <- zero.na.rows(act.current.year.subgroup.fay.equity, "N_EQUITY_HS")


#who is too small?
table(act.current.year.subgroup.fay.equity[act.current.year.subgroup.fay.equity$SCHOOL_YEAR==current.school.year,]$N_EQUITY_HS < min.N.equity.hs)
table(act.current.year.subgroup.fay.equity[act.current.year.subgroup.fay.equity$SCHOOL_YEAR==current.school.year,]$N_EQUITY_HS < 8)


source("plot-funs.R")
equity.plot.data <- with(act.current.year.subgroup.fay.equity,
                         act.current.year.subgroup.fay.equity[SCHOOL_YEAR==current.school.year & 
                                                                SCHOOL_ID != state.school.id &
                                                                !is.na(EQUITY_HS), "EQUITY_HS"])
plot.hist(equity.plot.data, 20)




#N_EQUITY and ACHIEVEMENT_TESTED_EQUITY_HS are not necessarily identical: N_EQUITY is filtered to FAY 
#status, but ACHIEVEMENT_TESTED_EQUITY_HS is not.
head(act.current.year.subgroup.fay.equity[act.current.year.subgroup.fay.equity$SCHOOL_YEAR==current.school.year,])
table(act.current.year.subgroup.fay.equity$EQUITY_HS_PARTICIPATION_RATE, useNA="ifany")
quantile(with(act.current.year.subgroup.fay.equity,
              act.current.year.subgroup.fay.equity[SCHOOL_YEAR == current.school.year &                     
                          N_EQUITY_HS >= min.N.equity.hs &
                          !is.na(EQUITY_HS_PARTICIPATION_RATE) &  
                          EQUITY_HS_PARTICIPATION_RATE >= .90 & 
                          SCHOOL_ID != state.school.id,]$EQUITY_HS), 
         probs=c(.35,.65),
         type=6)

act.current.year.subgroup.fay.equity$EQUITY_HS_TARGET_LEVEL <- findInterval(act.current.year.subgroup.fay.equity$EQUITY_HS,
                                                                            round(quantile(with(act.current.year.subgroup.fay.equity,
                                                                                          act.current.year.subgroup.fay.equity[SCHOOL_YEAR == current.school.year &                     
                                                                                                                                 N_EQUITY_HS >= min.N.equity.hs &
                                                                                                                                 !is.na(EQUITY_HS_PARTICIPATION_RATE) &  
                                                                                                                                 EQUITY_HS_PARTICIPATION_RATE >= .90 & 
                                                                                                                                 SCHOOL_ID != state.school.id,]$EQUITY_HS), 
                                                                                     probs=c(.35,.65),
                                                                                     type=6),0)
                                                                            ) + 1

schools <- bind.indicator(schools, 
                          act.current.year.subgroup.fay.equity,
                          indicator.labels.min.N = c("EQUITY_HS", N="N_EQUITY_HS", "EQUITY_HS_TARGET_LEVEL"),
                          min.N.equity.hs)

head(schools[schools$SCHOOL_YEAR==current.school.year & schools$WAEA_SCHOOL_TYPE %in% HS.types,])
