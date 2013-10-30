schools[!is.na(schools$SCHOOL_GRADUATION_INDEX),c("SCHOOL_YEAR", "SCHOOL_ID", "SCHOOL_GRADUATION_INDEX", "TESTED_READINESS")]
schools[schools$SCHOOL_ID=='1101050',]


schools[!is.na(schools$SCHOOL_GRADUATION_INDEX),c("SCHOOL_YEAR", "SCHOOL_ID", "SCHOOL_GRADUATION_INDEX", "TESTED_READINESS")]


#N for total readiness is defined to be the minimum of the Ns for each individual readiness component (i.e., graduation and tested readiness)
schools$N_TOTAL_READINESS_HS <-apply(schools[,c("N_GRADUATION", "N_TESTED_READINESS")], c(1),
                                     FUN=function (Ns) {
                                       if (sum(!is.na(Ns)) < length(Ns))
                                         NA
                                       else
                                         do.call(min, as.list(Ns))
                                     }) 

schools$TOTAL_READINESS_HS<-apply(schools[,names(hs.readiness.weights)], c(1),
                                  FUN=function (readiness.scores) {
                                    if (sum(is.na(readiness.scores)) == 0)
                                      round(hs.readiness.weights %*% as.numeric(readiness.scores), precision)
                                    else
                                      NA
                                  })


with(schools, schools[!is.na(TOTAL_READINESS_HS),c("SCHOOL_YEAR", "SCHOOL_ID", "SCHOOL_GRADUATION_INDEX", "TESTED_READINESS", "TOTAL_READINESS_HS")])                                  
                                                                    
schools <- calc.school.readiness.hs(schools)

table(schools$READINESS_TARGET_LEVEL)