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


hs.subgroup.students <- plan.prior.year[plan.prior.year$SUBGROUP_CONSOLIDATED_HS==1, 
                                        c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", 
                                          "PLAN_SCALE_READING", "PLAN_SCALE_MATHEMATICS", 
                                          "SUBGROUP_READING_HS","SUBGROUP_MATH_HS")]

#Will limit to Math and Reading.  Also, since the indicator is based on WY_ACT_SCALE_SCORE, this
#indicator is defined only  for ACT test takers.
act.current.year <- act.achieve[act.achieve$SUBJECT %in% c("Math", "Reading"),
                                c("SCHOOL_YEAR", "SCHOOL_ID",
                                  "WISER_ID", "TEST_TYPE", "SUBJECT", 
                                  "SCHOOL_FULL_ACADEMIC_YEAR",
                                  "TESTING_STATUS_CODE", 
                                  "WY_ACT_SCALE_SCORE")]




nrow(act.current.year)

act.current.year.subgroup <- merge(act.current.year, 
                                   plan.prior.year[!is.na(plan.prior.year$SUBGROUP_CONSOLIDATED_HS) & 
                                                     plan.prior.year$SUBGROUP_CONSOLIDATED_HS==1,]["WISER_ID"])
nrow(act.current.year.subgroup)


hs.subgroup.students <- plan.prior.year[plan.prior.year$SUBGROUP_CONSOLIDATED_HS==1, 
                                        c("SCHOOL_ID", "WISER_ID", 
                                          "PLAN_SCALE_READING", "PLAN_SCALE_MATHEMATICS", 
                                          "SUBGROUP_READING_HS","SUBGROUP_MATH_HS")]

names(hs.subgroup.students) <- c("PLAN_SCHOOL_ID", "WISER_ID", "PLAN_SCALE.Reading", 
                                 "PLAN_SCALE.Math",  "BELOW.Reading", "BELOW.Math")

hs.subgroup.students$BELOW.Reading <- ifelse(hs.subgroup.students$BELOW.Reading, 'T', 'F')
hs.subgroup.students$BELOW.Math <- ifelse(hs.subgroup.students$BELOW.Math, 'T', 'F')

act.current.year.subgroup.wide <- reshape(act.current.year.subgroup[c("WISER_ID","SCHOOL_ID", "TEST_TYPE","SUBJECT", "SCHOOL_FULL_ACADEMIC_YEAR", 
                                                                      "TESTING_STATUS_CODE", "WY_ACT_SCALE_SCORE")],
                                          v.names=c("SCHOOL_FULL_ACADEMIC_YEAR", "TESTING_STATUS_CODE", "WY_ACT_SCALE_SCORE"),
                                          timevar="SUBJECT",
                                          idvar="WISER_ID",
                                          direction="wide")

hs.consolidated.subgroup.wide <- merge(act.current.year.subgroup.wide, hs.subgroup.students,
                                       by="WISER_ID")

hs.consolidated.subgroup.wide <- hs.consolidated.subgroup.wide[c("WISER_ID","SCHOOL_ID", "PLAN_SCHOOL_ID", "TEST_TYPE",                                                                 "PLAN_SCALE.Reading", "PLAN_SCALE.Math", "BELOW.Reading", "BELOW.Math",
                                                                 "TESTING_STATUS_CODE.Reading", "TESTING_STATUS_CODE.Math",
                                                                 "SCHOOL_FULL_ACADEMIC_YEAR.Reading", "SCHOOL_FULL_ACADEMIC_YEAR.Math",
                                                                 "WY_ACT_SCALE_SCORE.Reading", "WY_ACT_SCALE_SCORE.Math")]

names(hs.consolidated.subgroup.wide) <- c("WISER_ID", "ACT_SCHOOL_ID",
                                          "PLAN_SCHOOL_ID", "ACT_TEST_TYPE", "PLAN_SCALE.Reading",
                                          "PLAN_SCALE.Math", "PLAN_BELOW.Reading","PLAN_BELOW.Math",
                                          "ACT_TESTING_STATUS_CODE.Reading",
                                          "ACT_TESTING_STATUS_CODE.Math", "ACT_SCHOOL_FULL_ACADEMIC_YEAR.Reading",
                                          "ACT_SCHOOL_FULL_ACADEMIC_YEAR.Math",
                                          "WY_ACT_SCALE_SCORE.Reading",
                                          "WY_ACT_SCALE_SCORE.Math")
head(hs.consolidated.subgroup.wide)

#write.csv(hs.consolidated.subgroup.wide, file="results/statewide-high-school-consolidated-subgroup-composition.csv", row.names=FALSE, quote=FALSE, na="")

equity.hs.indicator <- compute.indicator.long(act.current.year.subgroup, #for participation, include those who may have taken an Alt assessment this year
                                              act.current.year.subgroup[act.current.year.subgroup$TEST_TYPE == 'ACT',], #for indicator evaluation we can only apply to those who took the standard ACT
                                              schools,
                                              indicator.label="HS_EQUITY",                                         
                                              score.prefix="WY_ACT_SCALE_SCORE",
                                              agg.fun=function (g) c(N_SCORES=length(!is.na(g)),
                                                                     MEAN=round(mean(g, na.rm=TRUE),0)))

equity.hs.labels <- setdiff(names(equity.hs.indicator$schools), names(schools))
schools[,equity.hs.labels] <- equity.hs.indicator$schools[,equity.hs.labels]


#who is too small?
table(schools[schools$SCHOOL_YEAR==current.school.year,]$HS_EQUITY_N < min.N.equity.hs)
table(schools[schools$SCHOOL_YEAR==current.school.year,]$HS_EQUITY_N < 8)


source("plot-funs.R")
equity.plot.data <- with(schools,
                         schools[SCHOOL_YEAR==current.school.year & 
                                   SCHOOL_ID != state.school.id &
                                   !is.na(HS_EQUITY_MEAN), "HS_EQUITY_MEAN"])
plot.hist(equity.plot.data, 20)




#N_EQUITY and ACHIEVEMENT_TESTED_HS_EQUITY are not necessarily identical: N_EQUITY is filtered to FAY 
#status, but ACHIEVEMENT_TESTED_HS_EQUITY is not.
head(schools[schools$SCHOOL_YEAR==current.school.year,])
table(schools$HS_EQUITY_PARTICIPATION_RATE, useNA="ifany")
quantile(with(schools,
              schools[WAEA_SCHOOL_TYPE %in% HS.types &
                SCHOOL_YEAR == current.school.year &                     
                          HS_EQUITY_N >= min.N.equity.hs &
                          HS_EQUITY_PARTICIPATION_RATE >= .90 & 
                          SCHOOL_ID != state.school.id,]$HS_EQUITY_MEAN), 
         probs=c(.35,.65),
         type=6)

# schools$HS_EQUITY_TARGET_LEVEL <- findInterval(schools$HS_EQUITY_MEAN,
#                                                round(quantile(with(schools,
#                                                                    schools[WAEA_SCHOOL_TYPE %in% HS.types &
#                                                                              SCHOOL_YEAR == current.school.year &                     
#                                                                              HS_EQUITY_N >= min.N.equity.hs &
#                                                                              HS_EQUITY_PARTICIPATION_RATE >= .90 & 
#                                                                              SCHOOL_ID != state.school.id,]$HS_EQUITY_MEAN), 
#                                                               probs=c(.35,.65),
#                                                               type=6),0)
# ) + 1


schools$HS_EQUITY_TARGET_LEVEL <- findInterval(schools$HS_EQUITY_MEAN,
                                               hs.equity.cuts) + 1


head(schools[schools$SCHOOL_YEAR==current.school.year & schools$WAEA_SCHOOL_TYPE %in% HS.types,],50)


write.csv(file=get.filename("high-school-equity-cfds", "results/cfds"), schools[schools$WAEA_SCHOOL_TYPE %in% HS.types &
                                                              schools$SCHOOL_YEAR==current.school.year & 
                                                              schools$HS_EQUITY_N >= min.N.equity.hs &
                                                                schools$SCHOOL_ID != state.school.id,
                                                            c("SCHOOL_YEAR", "SCHOOL_ID", "ALTERNATIVE_SCHOOL", "HS_EQUITY_PARTICIPATION_RATE", "HS_EQUITY_MEAN")],
          na="",
          row.names=FALSE)