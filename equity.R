###based on data/PAWS_ACCOUNTABILITY_LONG.csv
##assumes that growth.R has been run to define growth.df
below.proficient.priors <- growth.df[growth.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2),]
subgroup.students <- unique(below.proficient.priors[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])

#scores for all the students in the subgroup
consolidated.subgroup.df <- merge(subgroup.students, growth.df)

#Under this strategy we do not use multiyear records to compute equity.  We should have applied this rule.  But we didn't.
#consolidated.subgroup.df <- with(consolidated.subgroup.df, consolidated.subgroup.df[SCHOOL_YEAR == SCHOOL_YEAR_ORIGINAL,])

#growth.df[growth.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2),]

met.agp <- ifelse(consolidated.subgroup.df$MET_AGP == 'T', 1, 0)

equity <- aggregate(data.frame(MET_AGP=met.agp, N=rep(1,length(met.agp))), 
                    by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                            SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID), 
                    sum)

#get the state value
equity.state <- aggregate(data.frame(MET_AGP=met.agp, N=rep(1,length(met.agp))), 
                          by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR), 
                          sum)

equity.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(equity.state)), equity.state)

equity <- rbind(equity, equity.state)
head(equity)
tail(equity)

percent.meeting.agp <- round((equity$MET_AGP / equity$N) * 100, precision)

equity$PERCENT_MEETING_AGP <- percent.meeting.agp


subgroup.N <- aggregate(data.frame(N_SUBGROUP=rep(1,nrow(subgroup.students))),
                        by=list(SCHOOL_YEAR=subgroup.students$SCHOOL_YEAR, 
                                SCHOOL_ID=subgroup.students$SCHOOL_ID), 
                        sum)

#compute state value
subgroup.N.state <- aggregate(data.frame(N_SUBGROUP=rep(1,nrow(subgroup.students))),
                        by=list(SCHOOL_YEAR=subgroup.students$SCHOOL_YEAR), 
                        sum)

subgroup.N.state <- cbind(SCHOOL_ID = rep(state.school.id, nrow(subgroup.N.state)), subgroup.N.state)

subgroup.N <-rbind(subgroup.N, subgroup.N.state)

equity <- merge(equity, subgroup.N)

with(equity, head(equity[SCHOOL_YEAR=='2012-13',]))
with(equity, tail(equity[SCHOOL_YEAR=='2012-13',]))

##equity analysis - can skip
xy <- merge(equity[equity$SCHOOL_YEAR=='2011-12', c("SCHOOL_ID", "PERCENT_MEETING_AGP"),],
            equity[equity$SCHOOL_YEAR=='2012-13', c("SCHOOL_ID", "PERCENT_MEETING_AGP"),], by=c("SCHOOL_ID"))
cor(xy$PERCENT_MEETING_AGP.x, xy$PERCENT_MEETING_AGP.y)



##assign equity SPL

schools <- calc.school.equity(schools)

#look at distribution of computed target levels
table(schools[schools$SCHOOL_YEAR==current.school.year,]$EQUITY_TARGET_LEVEL)
schools[schools$SCHOOL_ID==state.school.id,]
