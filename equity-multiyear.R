##assumes that growth.R has been run to define growth.df
below.proficient.priors <- growth.df[growth.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2),]
subgroup.students <- unique(below.proficient.priors[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])

#scores for all the students in the subgroup
consolidated.subgroup.df <- merge(subgroup.students, growth.df)

#we're going to go back in time to see if we can meet the minimum N requirement.  But, we don't want to compound the going
#back that we've already done to meet the minimum N for achievement and growth.  So, roll that back.
nrow(consolidated.subgroup.df)
consolidated.subgroup.df <- with(consolidated.subgroup.df, consolidated.subgroup.df[SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL,])
nrow(consolidated.subgroup.df)

schools.subgroup.N <- with(consolidated.subgroup.df, cast(consolidated.subgroup.df[, c("SCHOOL_YEAR", 
                                                                                       "SCHOOL_ID",   
                                                                                       "WISER_ID")], SCHOOL_YEAR+SCHOOL_ID~., 
                                         function (x) length(unique(x))))

names(schools.subgroup.N)[length(schools.subgroup.N)] <- "N_SUBGROUP"


#merge schools with no scores
schools.subgroup.N.0 <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                     consolidated.subgroup.df[, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                     all.x=TRUE)

schools.subgroup.N.0 <- schools.subgroup.N.0[is.na(schools.subgroup.N.0$WISER_ID) & schools.subgroup.N.0$SCHOOL_ID != state.school.id,]

schools.subgroup.N <- rbind(schools.subgroup.N, data.frame(t(apply(schools.subgroup.N.0,
                                                 c(1),
                                                 function (school){
                                                   
                                                   c(SCHOOL_YEAR = school[["SCHOOL_YEAR"]], SCHOOL_ID=school[["SCHOOL_ID"]], N_SUBGROUP=0)
                                                 }))))


tail(schools.subgroup.N, 30)

schools.subgroup.N$N_SUBGROUP <- as.numeric(schools.subgroup.N$N_SUBGROUP)
small.schools.subgroup <- data.frame(with(schools.subgroup.N, schools.subgroup.N[N_SUBGROUP < min.N.subgroup,]))

small.subgroup <- merge(cast(schools.subgroup.N[,c("SCHOOL_YEAR","SCHOOL_ID","N_SUBGROUP")], SCHOOL_ID~SCHOOL_YEAR, fill=0),
                        data.frame(SCHOOL_ID = unique(small.schools.subgroup[,c("SCHOOL_ID")])))

go.back.subgroup <- cbind(SCHOOL_ID = small.subgroup[,"SCHOOL_ID"], as.data.frame(t(apply(small.subgroup, c(1),
                                                                                        FUN=function (school) {
                                                                                          compute.N.years(school, min.N.subgroup.multiyear)          
                                                                                        }
))))




small.schools.subgroup$YEARS_BACK <- apply(small.schools.subgroup, c(1),
                                           FUN=function (school) {
                                             
                                             id <- school[["SCHOOL_ID"]]
                                             year <- school[["SCHOOL_YEAR"]]
                                             
                                             go.back.subgroup[go.back.subgroup$SCHOOL_ID==id,][[year]] 
                                             
                                           })


#look at the current year's batch
small.schools.subgroup[small.schools.subgroup$SCHOOL_YEAR==current.school.year,]

#this is how many we can rate
table(small.schools.subgroup[small.schools.subgroup$SCHOOL_YEAR==current.school.year,]$YEARS_BACK)



small.schools.fix.subgroup <- small.schools.subgroup[small.schools.subgroup$YEARS_BACK < Inf,]

small.schools.fix.subgroup$YEAR <- as.numeric(sapply(small.schools.fix.subgroup$SCHOOL_YEAR, function (y) {
  strsplit(y,'-')[[1]][1]
})) + 1

small.schools.subgroup.additional.consolidated.subgroup.df <- do.call(rbind, apply(small.schools.fix.subgroup[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK")], c(1),
                                                         FUN=function (school) get.paws.years.back(school, consolidated.subgroup.df)))

consolidated.subgroup.df <- rbind(consolidated.subgroup.df, small.schools.subgroup.additional.consolidated.subgroup.df)


schools <- schools[, !(names(schools) %in% small.school.labels.equity)]


schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                        "SCHOOL_YEAR")], c(1),                                                                                      
                                             FUN=function (school) {
                                               small.school <- with(small.schools.subgroup, 
                                                                    small.schools.subgroup[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                                 SCHOOL_YEAR == school[["SCHOOL_YEAR"]],])
                                               if (nrow(small.school) == 0)
                                                 result <- c('F', NA)
                                               else 
                                                 result <- c('T', small.school[,"YEARS_BACK"]) 
                                               names(result) <- small.school.labels.equity
                                               result
                                             }))))



schools$YEARS_BACK_EQUITY <- as.numeric(schools$YEARS_BACK_EQUITY)

#redo the equity calculation with the augmented data.frame

met.agp <- ifelse(consolidated.subgroup.df$MET_AGP == 'T', 1, 0)

equity <- aggregate(data.frame(MET_AGP=met.agp, N=rep(1,length(met.agp))), 
                    by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                            SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID), 
                    sum)


percent.meeting.agp <- round((equity$MET_AGP / equity$N) * 100, precision)

equity$PERCENT_MEETING_AGP <- percent.meeting.agp


subgroup.N <- cast(consolidated.subgroup.df[,c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")], SCHOOL_YEAR + SCHOOL_ID ~ .,
                   function (x) length(unique(x)))

names(subgroup.N)[ncol(subgroup.N)] <- "N_SUBGROUP"

equity <- merge(equity, subgroup.N)

with(equity, head(equity[SCHOOL_YEAR=='2012-13',]))



##re-assign equity SPL if this correction is an improvement over the one we computed without going back.

schools <- calc.school.equity(schools)

#look at distribution of computed target levels
table(schools[schools$SCHOOL_YEAR==current.school.year,]$EQUITY_TARGET_LEVEL)

