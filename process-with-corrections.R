#compute the result based on going back multiyears to build a subgroup of size 15
source("process-multiyear-subgroup.R")

schools.multiyear.subgroup <- schools 
consolidated.subgroup.df.multiyear <- consolidated.subgroup.df
equity.multiyear <- equity

source("process.R") #this is the original result


choose.outcome <- function (idx) {
  school <- schools[idx,]
  school.multiyear.subgroup <- with(schools.multiyear.subgroup,
                                schools.multiyear.subgroup[SCHOOL_YEAR == school$SCHOOL_YEAR &
                                                       SCHOOL_ID == school$SCHOOL_ID,])
  
  
    
  if (!is.na(school$SPL) & school$SPL < school.multiyear.subgroup$SPL)
    cbind(school.multiyear.subgroup[,names(school)], YEARS_BACK_EQUITY=school.multiyear.subgroup$YEARS_BACK_EQUITY, SMALL_SUBGROUP="T")
  else {
    
    cbind(school, YEARS_BACK_EQUITY=NA, SMALL_SUBGROUP="F")
    
  }
  
}

#See which two-indicator schools in the original result benefit from the multiyear equity indicator
schools <- do.call(rbind, lapply(1:nrow(schools),
                      choose.outcome))

#19 schools affected
table(with(schools, schools[SCHOOL_YEAR==current.school.year,]$SMALL_SUBGROUP), useNA="ifany")

with(schools, schools[SMALL_SCHOOL=='T' & SMALL_SUBGROUP=='T',])
with(schools, schools[SMALL_SCHOOL=='T' & N_INDICATORS==3,])
#how many small schools ended up with an equity indicator under the original implementation (0)
with(schools, schools[SMALL_SCHOOL=='T' & N_INDICATORS==3 & SCHOOL_YEAR==current.school.year,])
#how many small schools ended up with an equity indicator in the multiyear correction scheme (0)
with(schools.multiyear.subgroup, schools.multiyear.subgroup[!is.na(YEARS_BACK_EQUITY) & 
                                                              !is.na(YEARS_BACK), c("YEARS_BACK_EQUITY", "YEARS_BACK")])
#compare YEARS_BACK and YEARS_BACK_EQUITY
#these are the small schools that had some consolidated subgroup members during the last three years
smalls <- merge(small.schools, small.schools.subgroup, by=c("SCHOOL_YEAR", "SCHOOL_ID"))
schools[schools$SCHOOL_YEAR==current.school.year & schools$SCHOOL_ID %in% smalls[smalls$SCHOOL_YEAR == current.school.year & smalls$YEARS_BACK.x < Inf,"SCHOOL_ID"], c("DISTRICT_NAME", "NAME")]

#these are the small schools that didn't have any consolidated subgroup members in the last three years
other.smalls <- merge(small.schools, small.schools.subgroup, by=c("SCHOOL_YEAR", "SCHOOL_ID"), all.x=TRUE)

#Then we have to build a consolidated.subgroup.df dataframe for reporting that has the correct records
#for each school's result (whether original or multiyear corrected)
consolidated.subgroup.df.orig.keep <-  merge(with(schools, schools[is.na(YEARS_BACK_EQUITY), c("SCHOOL_YEAR", "SCHOOL_ID")]),
                                             consolidated.subgroup.df)

equity.orig.keep <- merge(with(schools, schools[is.na(YEARS_BACK_EQUITY), c("SCHOOL_YEAR", "SCHOOL_ID")]),
                          equity)

consolitdated.subgroup.df.corrected.keep <- merge(with(schools, schools[!is.na(YEARS_BACK_EQUITY), c("SCHOOL_YEAR", "SCHOOL_ID")]),
                                                  consolidated.subgroup.df.multiyear)

equity.corrected.keep <- merge(with(schools, schools[!is.na(YEARS_BACK_EQUITY), c("SCHOOL_YEAR", "SCHOOL_ID")]),
                               equity.multiyear)

#this is the number of records we added 
nrow(with(consolitdated.subgroup.df.corrected.keep, consolitdated.subgroup.df.corrected.keep[SCHOOL_YEAR==current.school.year & SCHOOL_YEAR != SCHOOL_YEAR_ORIGINAL,]))


#decision was made not to add them to the state aggregates for reporting
consolidated.subgroup.df.orig.keep$EXCLUDE_FROM_STATE_AVERAGE <- rep(FALSE,nrow(consolidated.subgroup.df.orig.keep))
consolitdated.subgroup.df.corrected.keep$EXCLUDE_FROM_STATE_AVERAGE <- ifelse(consolitdated.subgroup.df.corrected.keep$SCHOOL_YEAR !=
                                                                                consolitdated.subgroup.df.corrected.keep$SCHOOL_YEAR_ORIGINAL,
                                                                              TRUE,
                                                                              FALSE)

consolidated.subgroup.df <- rbind(consolidated.subgroup.df.orig.keep,
                                  consolitdated.subgroup.df.corrected.keep)

#so that put.cuts.nonHS still works in services.R, even though changing the cut scores fairly meaningless at this
#point
equity <- rbind(equity.orig.keep,
                equity.corrected.keep)
#then run reporting