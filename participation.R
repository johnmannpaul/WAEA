#The full year students with testing status T or N.  Only the Ts are counted in the numerator.
#Both the Ts and the Ns are counted in the denominator.
paws.part.df <- paws[paws$SCHOOL_FULL_ACADEMIC_YEAR=="T" & paws$TESTING_STATUS_CODE %in% c('T','N') & paws$GRADE_ENROLLED != "11", ]

participants <- ifelse(paws.part.df$TESTING_STATUS_CODE == 'T', 1, 0)

participation.df <- aggregate(data.frame(N_PARTICIPATION=participants,  N=rep(1, nrow(paws.part.df))),
                              by=list(SCHOOL_YEAR = paws.part.df$SCHOOL_YEAR,
                                      SCHOOL_ID = paws.part.df$SCHOOL_ID),
                              sum)

#compute participation for the state
participation.df.state <- aggregate(data.frame(N_PARTICIPATION=participants,  N=rep(1, nrow(paws.part.df))),
                              by=list(SCHOOL_YEAR = paws.part.df$SCHOOL_YEAR),
                              sum)

participation.df.state <- cbind(SCHOOL_ID = rep(state.school.id, nrow(participation.df.state)), participation.df.state)

participation.df <- rbind(participation.df, participation.df.state)

participation.df$PARTICIPATION_RATE <- round((participation.df$N_PARTICIPATION / participation.df$N) * 100, precision)

with(participation.df, head(participation.df[SCHOOL_YEAR==current.school.year,]))
with(participation.df, tail(participation.df[SCHOOL_YEAR==current.school.year,]))
with(participation.df, head(participation.df[order(PARTICIPATION_RATE),], 30))

##assign participation rate

#drop particpation rate
schools <- schools[, !(names(schools) %in% c("PARTICIPATION_RATE"))]

##compute participation rate
schools <- cbind(schools, data.frame(PARTICIPATION_RATE = apply(schools[,c("SCHOOL_ID",                                                     
                                                                           "SCHOOL_YEAR")], c(1),                                                                                      
                                                                FUN=calc_participation_rate)))

schools$PARTICIPATION_RATE_LEVEL <- sapply(schools[,c("PARTICIPATION_RATE")],
                                           rate.participation)

schools[schools$SCHOOL_ID=='7700000',]