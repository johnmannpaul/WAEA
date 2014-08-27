##Needs work.
##Will need to be able to recompute categories based on PJP redefining targets
##

grad.rate <- read.csv(file="data/grad-rate-2013.with-small-school-lookback.csv", header=TRUE )
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
                                        "GRAD_RATE_4_YR.2012.13", "GRAD_RATE_EXTENDED",
                                        "COHORT_4_YR_N.2012.13", "COHORT_EXTENDED_N.2012.13",
                                        "CAT_4_YR_2013", "CAT_EXTENDED_2013",
                                        "IMPROVE_TARGET_FOR_MEETS", "IMPROVE_TARGET_FOR_EXCEED", "IMPROVE_CAT_2013")],
                          indicator.labels.min.N = c(N= "COHORT_EXTENDED_N.2012.13", score="IMPROVE_CAT_2013"),
                          min.N.grad) 

#some schools may not have an improvement category for 2013, because 
#it has only been operational for 4 years (i.e., there is no 2011-12 4 year rate)
schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% HS.types &
          schools$SCHOOL_ID != state.school.id ,]


