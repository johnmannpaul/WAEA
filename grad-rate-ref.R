##Needs work.
##Will need to be able to recompute categories based on PJP redefining targets
##

grad.rate <- read.csv(file="data/grad-rate-2013.with-small-school-lookback.csv", header=TRUE )

grad.rate$GRAD_RATE_4_YR.2012.13 <- round(grad.rate$GRAD_RATE_4_YR.2012.13 * 100, grad.rate.precision)
grad.rate$GRAD_RATE_EXTENDED <- round(grad.rate$GRAD_RATE_EXTENDED * 100, grad.rate.precision)
grad.rate$GRAD_RATE_4_YR_2012 <- round(grad.rate$GRAD_RATE_4_YR_2012 * 100, grad.rate.precision)

#We need to compute state totals, and to do this we 
#have to go back to the source data because the other dataset already has lookback numbers
load(file="data/grads-and-cohorts.Rdata")
grads.state <- aggregate(grads.and.cohorts[c("COHORT_4_YR_N.2012.13", "GRADS_4_YR.2012.13", "COHORT_EXTENDED_N.2012.13", "GRADS_EXTENDED.2012.13",
                                         "COHORT_4_YR_N.2011.12", "GRADS_4_YR.2011.12")],
                             by=list(SCHOOL_ID=rep(state.school.id, nrow(grads.and.cohorts))),
                             sum, na.rm=TRUE)



grad.rate.state <- data.frame(grads.state["SCHOOL_ID"],
                              DISTRICT_ID=NA,
                              SCHOOL_NAME="State of Wyoming",
                              ALTERNATIVE_SCHOOL="FALSE",
                              grads.state[c("COHORT_4_YR_N.2012.13", "GRADS_4_YR.2012.13")],
                              LOOK_BACK_YRS_4_YR=0,
                              grads.state[c("COHORT_EXTENDED_N.2012.13", "GRADS_EXTENDED.2012.13")],
                              EXTENDED_LOOK_BACK_YRS=0,
                              GRAD_RATE_4_YR.2012.13=round((grads.state$GRADS_4_YR.2012.13/grads.state$COHORT_4_YR_N.2012.13)*100, grad.rate.precision),
                              GRAD_RATE_EXTENDED=round((grads.state$GRADS_EXTENDED.2012.13/grads.state$COHORT_EXTENDED_N.2012.13)*100, grad.rate.precision),
                              grads.state[c("COHORT_4_YR_N.2011.12", "GRADS_4_YR.2011.12")],
                              LOOK_BACK_YRS_2012_4_YR=0,
                              GRAD_RATE_4_YR_2012=round((grads.state$GRADS_4_YR.2011.12/grads.state$COHORT_4_YR_N.2011.12)*100, grad.rate.precision),
                              CAT_4_YR_2013=NA,
                              CAT_EXTENDED_2013=NA,
                              IMPROVE_TARGET_FOR_MEETS=NA, IMPROVE_TARGET_FOR_EXCEED=NA, IMPROVE_CAT_2013=NA)

grad.rate <- rbind(grad.rate, grad.rate.state)

#assign categories based on current cuts
grad.rate$CAT_4_YR_2013 <- findInterval(grad.rate$GRAD_RATE_4_YR.2012.13, hs.grad.rate.cuts) + 1
grad.rate$CAT_EXTENDED_2013 <- findInterval(grad.rate$GRAD_RATE_EXTENDED , hs.grad.rate.cuts) + 1




grad.rate[c("GRAD_RATE_TYPE", 
            "GRAD_RATE_N",
            "IMPROVE_TARGET_FOR_MEETS", 
            "IMPROVE_TARGET_FOR_EXCEED", 
            "IMPROVEMENT_TARGET",
            "IMPROVE_CAT_2013")] <- t(apply(grad.rate[grad.rate.labels],
                                          c(1),
                                          compute.grad.rate.cat, 
                                          hs.grad.rate.cuts, 
                                          grad.rate.precision, 
                                          grad.rate.labels))

# #recompute improvement categories based on last categorization
# improve.to.meet<-grad.rate[which(grad.rate$CAT_EXTENDED_2013 == 1),]
# head(improve.to.meet)
# nrow(improve.to.meet)
# improve.to.meet$IMPROVE_TARGET_FOR_MEETS<-(((hs.grad.rate.cuts[1] - improve.to.meet$`GRAD_RATE_4_YR_2012`)/3)+
#                                              improve.to.meet$`GRAD_RATE_4_YR_2012`)
# head(improve.to.meet)
# 
# 
# improve.to.meet$DIFF_FROM_MEET_TARGET<-improve.to.meet$GRAD_RATE_4_YR.2012.13 - improve.to.meet$IMPROVE_TARGET_FOR_MEETS
# head(improve.to.meet)
# improve.to.meet$IMPROVE_CAT_2013[improve.to.meet$DIFF_FROM_MEET_TARGET < 0]<- 1
# improve.to.meet$IMPROVE_CAT_2013[improve.to.meet$DIFF_FROM_MEET_TARGET >= 0]<- 2
# head(improve.to.meet)
# table(improve.to.meet$IMPROVE_CAT_2013)
# 
# table(improve.to.meet$ALTERNATIVE_SCHOOL,improve.to.meet$IMPROVE_CAT_2013)
# 
# #MEETS TARGET FOR EXCEEDS
# 
# improve.to.exceed<-grad.rate[which(grad.rate$CAT_EXTENDED_2013 == 2),]
# head(improve.to.exceed)
# nrow(improve.to.exceed)
# 
# improve.to.exceed$IMPROVE_TARGET_FOR_EXCEED<-(((hs.grad.rate.cuts[2] - improve.to.exceed$`GRAD_RATE_4_YR_2012`)/3)+
#                                                 improve.to.exceed$`GRAD_RATE_4_YR_2012`)
# head(improve.to.exceed)
# 
# improve.to.exceed$DIFF_FROM_EXCEED_TARGET<-improve.to.exceed$GRAD_RATE_4_YR.2012.13 - improve.to.exceed$IMPROVE_TARGET_FOR_EXCEED
# 
# improve.to.exceed$IMPROVE_CAT_2013[improve.to.exceed$DIFF_FROM_EXCEED_TARGET < 0]<- 2
# improve.to.exceed$IMPROVE_CAT_2013[improve.to.exceed$DIFF_FROM_EXCEED_TARGET >= 0]<- 3
# head(improve.to.exceed)
# table(improve.to.exceed$IMPROVE_CAT_2013)
# 
# table(improve.to.exceed$ALTERNATIVE_SCHOOL,improve.to.exceed$IMPROVE_CAT_2013)
# 
# #merge improvement targets into grad.rate
# 
# improve.to.meet.b<-improve.to.meet[c('SCHOOL_ID',
#                                      'IMPROVE_TARGET_FOR_MEETS')]
# improve.to.exceed.b<-improve.to.exceed[c('SCHOOL_ID',
#                                          'IMPROVE_TARGET_FOR_EXCEED')]
# grad.rate.b<-merge(grad.rate,improve.to.meet.b,all.x=TRUE)
# grad.rate.c<-merge(grad.rate.b,improve.to.exceed.b,all.x=TRUE)
# head(grad.rate.c)
# 
# #bind rows to create IMPROVE_CAT_2013 variable
# head(improve.to.meet)
# improve.to.meet.c<-improve.to.meet[c('SCHOOL_ID',
#                                      'IMPROVE_CAT_2013')]
# improve.to.exceed.c<-improve.to.exceed[c('SCHOOL_ID',
#                                          'IMPROVE_CAT_2013')]
# begin.as.exceed<-grad.rate[which(grad.rate$CAT_EXTENDED_2013 == 3),]
# nrow(begin.as.exceed)
# begin.as.exceed$IMPROVE_CAT_2013<- 3
# head(begin.as.exceed)
# begin.as.exceed.b<-begin.as.exceed[c('SCHOOL_ID',
#                                      'IMPROVE_CAT_2013')]
# 
# improvement1<-rbind(improve.to.meet.c,improve.to.exceed.c)
# improvement2<-rbind(improvement1,begin.as.exceed.b)
# nrow(improvement2)
# head(improvement2)
# 
# grad.rate.2013<-merge(grad.rate.c,improvement2,all.x=TRUE)
# head(grad.rate.2013)
# names(grad.rate.2013)
# nrow(grad.rate.2013)
# grad.rate.2013$SCHOOL_ID
# 
# grade.rate <- grad.rate.2013
# ##improvement category end

grad.rate$SCHOOL_ID <- sapply(grad.rate$SCHOOL_ID,
                              function (id) {
                                if (nchar(id) == 6)
                                  paste('0', id, sep="")
                                else
                                  id
                                })

grad.rate$SCHOOL_YEAR <- rep("2013-14", nrow(grad.rate))
schools <- bind.indicator(schools, 
                          grad.rate[, c("SCHOOL_YEAR", "SCHOOL_ID", "LOOK_BACK_YRS_4_YR", 
                                        "EXTENDED_LOOK_BACK_YRS",
                                        "GRAD_RATE_4_YR_2012","GRAD_RATE_4_YR.2012.13", "GRAD_RATE_EXTENDED",
                                        "COHORT_4_YR_N.2011.12", "COHORT_4_YR_N.2012.13", "COHORT_EXTENDED_N.2012.13", "GRAD_RATE_N",
                                        "CAT_4_YR_2013", "CAT_EXTENDED_2013",
                                        "IMPROVE_TARGET_FOR_MEETS", "IMPROVE_TARGET_FOR_EXCEED", "IMPROVEMENT_TARGET", "IMPROVE_CAT_2013")],
                          indicator.labels.min.N = c(N= "GRAD_RATE_N", score="IMPROVE_CAT_2013"),
                          min.N.grad) 

#some schools may not have an improvement category for 2013, because 
#it has only been operational for 4 years (i.e., there is no 2011-12 4 year rate)
schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% HS.types &
          schools$SCHOOL_ID != state.school.id ,]


write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
                    !(is.na(schools$COHORT_EXTENDED_N.2012.13)) &
                    schools$COHORT_EXTENDED_N.2012.13 >= min.N.grad &
                    schools$SCHOOL_YEAR==current.school.year &
                    schools$SCHOOL_ID != state.school.id,c("SCHOOL_ID", 
                                                           "SCHOOL_YEAR", 
                                                           "ALTERNATIVE_SCHOOL", 
                                                           "GRAD_RATE_4_YR.2012.13", 
                                                           "GRAD_RATE_EXTENDED",
                                                           "CAT_4_YR_2013", 
                                                           "CAT_EXTENDED_2013",
                                                           "IMPROVE_CAT_2013")], 
          file=get.filename("grad-rate-cfds", "results/cfds"), na="", row.names=FALSE)
