schools$N_INDICATORS_HS <- apply(schools[,c("ACHIEVEMENT_TARGET_LEVEL_HS", 
                                         "READINESS_TARGET_LEVEL", 
                                         "EQUITY_TARGET_LEVEL_HS",
                                         "N_ACHIEVEMENT_HS",
                                         "N_TOTAL_READINESS_HS",
                                         "N_EQUITY_HS")],  
                              c(1),                                                                                      
                              FUN=count_indicators.hs)




schools$PARTICIPATION_RATE_HS <- apply(schools[,c("PARTICIPATION_RATE_ACHIEVEMENT_HS",
                                                  "PARTICIPATION_RATE_TESTED_READINESS")], c(1),
                                       function (school) {
                                         
                                         if (sum(is.na(school)) < 2)
                                           min(school[which(!is.na(school))])
                                         else
                                           NA                                         
                                       })




schools$PARTICIPATION_RATE_LEVEL_HS <- sapply(schools[,c("PARTICIPATION_RATE_HS")],
                                              rate.participation)

schools <- calc.SPLs(schools, "HS")

# schools$SPL_HS <- calc.SPLs(schools, "HS")
# schools$SPL_ADJUSTED_HS <- apply(schools[,c("SPL_HS",  "PARTICIPATION_RATE_LEVEL_HS")], c(1),
#                               FUN= function (school) {
#                                 SPL <- school[["SPL_HS"]]
#                                 PRL <- school[["PARTICIPATION_RATE_LEVEL_HS"]]
#                                 
#                                 if (is.na(SPL))
#                                   NA
#                                 else {
#                                   
#                                   if (PRL == 3)
#                                     SPL
#                                   else {
#                                     
#                                     if (PRL == 2)
#                                       max(1, SPL-1)
#                                     else  #not met
#                                       1
#                                   }
#                                   
#                                 }                               
#                                 
#                               }) 
