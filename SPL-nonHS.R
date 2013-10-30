schools$N_INDICATORS <- apply(schools[,c("ACHIEVEMENT_TARGET_LEVEL", 
                                         "GROWTH_TARGET_LEVEL", 
                                         "EQUITY_TARGET_LEVEL",
                                         "N_ACHIEVEMENT",
                                         "N_GROWTH",
                                         "N_SUBGROUP")],
                              c(1),                                                                                      
                              FUN=count_indicators)


#look at the distribution of indicator combinations for 3-8
matrix.slots <- cast(schools[,c("SCHOOL_YEAR", "ACHIEVEMENT_TARGET_LEVEL", "GROWTH_TARGET_LEVEL", "EQUITY_TARGET_LEVEL","SCHOOL_ID")],
                     SCHOOL_YEAR + ACHIEVEMENT_TARGET_LEVEL + GROWTH_TARGET_LEVEL + EQUITY_TARGET_LEVEL ~ .,
                     function (x) sum(!is.na(x)))
names(matrix.slots)[length(matrix.slots)] <- "N"
##schools with all three indciators defined in 2012-13
with(matrix.slots, matrix.slots[SCHOOL_YEAR=='2012-13' & !is.na(EQUITY_TARGET_LEVEL),])
with(matrix.slots, matrix.slots[SCHOOL_YEAR=='2012-13',])


schools <- calc.SPLs(schools, "nonHS")

# schools$SPL <- calc.SPLs(schools, "nonHS")
# schools$SPL_ADJUSTED <- apply(schools[,c("SPL",  "PARTICIPATION_RATE_LEVEL")], c(1),
#                              FUN= function (school) {
#                                SPL <- school[["SPL"]]
#                                PRL <- school[["PARTICIPATION_RATE_LEVEL"]]
#                                
#                                if (is.na(SPL))
#                                  NA
#                                else {
#                                  
#                                  if (PRL == 3)
#                                    SPL
#                                  else {
#                                    
#                                    if (PRL == 2)
#                                      max(1, SPL-1)
#                                    else  #not met
#                                      1
#                                  }
#                                  
#                                }                               
#                                
#                              }) 

#propagate results to paired schools
schools <- propagate.results.to.paired.schools(schools)

#check two of the paired schools
schools[schools$SCHOOL_ID %in% c('0501002','0501010'),]
with(schools, head(schools[SCHOOL_YEAR=='2012-13',]))
