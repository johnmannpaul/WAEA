
# calc.SPL <- function (school) {
# 
#   
#   SPLs <- as.numeric(school[c("SPL", "SPL_HS")])
#   SPLs <- SPLs[which(!is.na(SPLs))]
#   
#   if (length(SPLs) == 0)
#     return(NA)
#   
#   
#   part.rates <- as.numeric(school[c("PARTICIPATION_RATE_LEVEL", "PARTICIPATION_RATE_LEVEL_HS")])
#   part.rates <- part.rates[which(!is.na(part.rates))]
#   
#   if (length(SPLs) != length(part.rates))
#     print(paste(school[["SCHOOL_ID"]], school[["SCHOOL_YEAR"]], ":"))
#   
#   SPLs.adjusted <- sapply(seq(1:length(SPLs)),  #if there's an SPL defined at i then there's a participation rate defined at index i
#                           function (i) {
#                             if (part.rates[i] == 3)
#                               SPLs[i]
#                             else {
#                               
#                               if (part.rates[i] == 2)
#                                 min(1, SPLs[i] - 1)
#                               else #participation rate is not met
#                                 1                              
#                             }
#                           })
#   
# 
#   
#   result <- do.call(min, as.list(SPLs.adjusted))  
# }

#calculate an accountability SPL that is the minimum of the non-high school SPL and the high school SPL
schools$ACCOUNTABILITY_SPL <- apply(schools[,c("SPL_ADJUSTED", "SPL_ADJUSTED_HS")], c(1),
                                    FUN= function (school) {
                                      if (sum(!is.na(school)) == 0)
                                        NA
                                      else
                                        min(school[which(!is.na(school))])
                                    })


calc.indicators <- function (school) {
  SPLs <- school[c("SPL_ADJUSTED", "SPL_ADJUSTED_HS")]
  indicators <- school[c("N_INDICATORS", "N_INDICATORS_HS")]
  
  if (sum(!is.na(SPLs)) == 0)
    max(indicators)
  else {
    SPLs <- sapply(SPLs, function (x) ifelse(is.na(x), Inf, x))
    indicators[which(SPLs == min(SPLs))][1] #necessarily unique
  }
    
    
}

schools$ACCOUNTABILITY_N_INDICATORS <- apply(schools[,c("SPL_ADJUSTED", "SPL_ADJUSTED_HS", "N_INDICATORS", "N_INDICATORS_HS")], c(1),
                                    FUN= calc.indicators)
  
#separate the state into its own data frame
state.school <- with(schools, schools[SCHOOL_ID==state.school.id,])
schools <- with(schools, schools[SCHOOL_ID != state.school.id,])


##look at schools with nonHS and HS SPLs
with(schools, schools[WAEA_SCHOOL_TYPE %in% c(4,5) & SCHOOL_YEAR==current.school.year,c("SCHOOL_ID", "NAME", "LOW_GRADE", "HIGH_GRADE", "SPL", "SPL_HS", "ACCOUNTABILITY_SPL")])

cast(with(schools, schools[WAEA_SCHOOL_TYPE %in% c(4,5) & SCHOOL_YEAR==current.school.year,c("SCHOOL_ID", "SPL", "SPL_HS")]), value="SCHOOL_ID",
     SPL~SPL_HS)

#look at Wright High School with SPL=1 and SPL_HS=3
with(schools, schools[SCHOOL_ID=='0301056' & SCHOOL_YEAR==current.school.year,])

#This has moved to earlier in the process after the nonHS SPL has been calculated.  None of these schools is paired directly with a high school or K12.

#Assign the cut points and target levels of the paired schools to those of parent school
#Should just fold the achievement columns into this
# lapply(c(achievement.labels,
#          achievement.grade.band.labels,
#          growth.labels,
#          equity.labels, 
#          act.achievement.labels,
#          tested.readiness.labels,
#          grad.index.labels,
#          total.readiness.labels,
#          hs.equity.labels,
#          "PARTICIPATION_RATE",
#          "N_INDICATORS",
#          "SPL"),
#        function (column) {
#          
#          schools[[column]] <<- as.numeric(apply(schools[,c("SCHOOL_YEAR", "SCHOOL_ID", column)], 
#                                                 c(1), 
#                                                 function (school) {
#                                                   school.year <- school[["SCHOOL_YEAR"]]
#                                                   pairing <- school.pairing.lookup[[school[["SCHOOL_YEAR"]]]]
#                                                   paired.school <- pairing[[school[["SCHOOL_ID"]]]]
#                                                   ifelse(is.null(paired.school), 
#                                                          school[[column]], 
#                                                          schools[schools$SCHOOL_YEAR == school.year & 
#                                                                    schools$SCHOOL_ID == paired.school, c(column)])
#                                                   
#                                                 }))
#          
#          
#        })





#how many schools have 0,1,2,or 3 indicators defined
# lapply(sort(unique(schools$SCHOOL_YEAR)),
#        function(year) {
#          table(schools[schools$SCHOOL_YEAR == year,]$N_INDICATORS)})




##equity analysis
# table(schools[schools$SCHOOL_YEAR == current.school.year,]$EQUITY_TARGET_LEVEL)
# equity.percentiles <- lapply(c('2010-11', '2011-12', '2012-13'), function (year) {
#   quantile(schools[schools$SCHOOL_YEAR == year & schools$N_SUBGROUP > 14,]$PERCENT_MEETING_AGP, probs=seq(0, 1, 0.1), na.rm=TRUE)
# })
# 
# names(equity.percentiles) <- c('2010-11', '2011-12', '2012-13')
# aggregate(equity$N_SUBGROUP, by=list(equity$SCHOOL_YEAR), mean)
# 
# equity_percentiles <- equity.percentiles

#which schools have undefined equity, but defined achievement
schools[is.na(schools$EQUITY_TARGET_LEVEL) & 
          !is.na(schools$ACHIEVEMENT_TARGET_LEVEL) & 
          schools$SCHOOL_YEAR==current.school.year,]


##end equity analysis



#save(schools, file="results/2012-13/schools-through-hs-achievment.Rdata")
#save(schools, file="results/2012-13/schools-through-tested-readiness.Rdata")
#write.csv(schools, file="results/2012-13/schools-with-indicators-E.csv", na="")
#write.csv(schools, file="results/2012-13/schools-with-indicators-F.csv", na="")
#
#write.csv(schools, file="results/2012-13/schools-with-indicators-H.csv", na="", row.names=FALSE)
#write.csv(schools, file="results/2012-13/schools-with-indicators-I.csv", na="", row.names=FALSE)
#write.csv(schools, file="results/2012-13/schools-with-indicators-J.csv", na="", row.names=FALSE)
#write.csv(schools, file="results/2012-13/schools-with-indicators-K.csv", na="", row.names=FALSE)
#write.csv(schools, file="results/2012-13/schools-with-indicators-L.csv", na="", row.names=FALSE)
#write.csv(schools, file="results/2012-13/schools-with-indicators-M.csv", na="", row.names=FALSE)
#write.csv(schools, file="results/2012-13/schools-with-indicators-N.csv", na="", row.names=FALSE, quote=FALSE)
#write.csv(schools, file="results/2012-13/schools-with-indicators-O.csv", na="", row.names=FALSE)
#write.csv(schools, file="results/2012-13/schools-with-indicators-Q.csv", na="", row.names=FALSE)
#save(schools, file="results/2012-13/schools-through-hs-readiness.Rdata")

#write.csv(with(schools, schools[SCHOOL_YEAR=='2012-13' & SMALL_SCHOOL=='T',]), file="results/2012-13/small-schools-2012-13.csv", na="", row.names=FALSE)
