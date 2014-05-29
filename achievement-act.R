##from data/ACT/2013-08-21 Confidential ACT_STUDENT_RESULTS_2013_WAEA.xlsx 
#save(act, file="data/ACT/act.R")
#

#Participation includes everyone (not just full-academic-year students).  So, use full "act" data.frame.
#We only base participation rate on the participation in the current year.  No lookback classes are inlcuded.
act.participation <- aggregate(data.frame(MATH_TESTED = ifelse(act$TESTING_STATUS_CODE_MATH == "T", 1, 0),
                                          MATH_PARTICIPANTS = ifelse(act$TESTING_STATUS_CODE_MATH == "X", 0, 1),
                                          READING_TESTED = ifelse(act$TESTING_STATUS_CODE_READING == "T", 1, 0),
                                          READING_PARTICIPANTS = ifelse(act$TESTING_STATUS_CODE_READING == "X", 0, 1),
                                          SCIENCE_TESTED = ifelse(act$TESTING_STATUS_CODE_SCIENCE == "T", 1, 0),
                                          SCIENCE_PARTICIPANTS = ifelse(act$TESTING_STATUS_CODE_SCIENCE == "X", 0, 1),
                                          ENG_WRITING_TESTED  = ifelse(act$TESTING_STATUS_CODE_ENG_WRITING == "T", 1, 0),
                                          ENG_WRITING_PARTICIPANTS = ifelse(act$TESTING_STATUS_CODE_ENG_WRITING == "X", 0, 1)),
                               by = list(SCHOOL_YEAR=act$SCHOOL_YEAR,
                                         SCHOOL_ID=act$SCHOOL_ID),
                               sum)

#do state participation 
act.participation.state <- aggregate(act.participation[,c("MATH_TESTED","MATH_PARTICIPANTS","READING_TESTED",
                                                          "READING_PARTICIPANTS","SCIENCE_TESTED","SCIENCE_PARTICIPANTS", 
                                                          "ENG_WRITING_TESTED", "ENG_WRITING_PARTICIPANTS")],
                                     by = list(SCHOOL_YEAR=act.participation$SCHOOL_YEAR), sum)


act.participation.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(act.participation.state)), act.participation.state)

act.participation <- rbind(act.participation, act.participation.state[,c(names(act.participation))])

tail(act.participation)

act.participation$MATH_PARTICIPATION_RATE <- round((act.participation$MATH_TESTED / act.participation$MATH_PARTICIPANTS) * 100, precision)                               
act.participation$READING_PARTICIPATION_RATE <- round((act.participation$READING_TESTED / act.participation$READING_PARTICIPANTS) * 100, precision)                               
act.participation$SCIENCE_PARTICIPATION_RATE <- round((act.participation$SCIENCE_TESTED / act.participation$SCIENCE_PARTICIPANTS) * 100, precision)                                                                         
act.participation$ENG_WRITING_PARTICIPATION_RATE <- round((act.participation$ENG_WRITING_TESTED / act.participation$ENG_WRITING_PARTICIPANTS) * 100, precision)                                                                         




act.participation <- cbind(act.participation, t(apply(act.participation[,c("MATH_TESTED",
                                                                               "READING_TESTED",
                                                                               "SCIENCE_TESTED",
                                                                               "ENG_WRITING_TESTED",
                                                                               "MATH_PARTICIPANTS",
                                                                               "READING_PARTICIPANTS",
                                                                               "SCIENCE_PARTICIPANTS",
                                                                               "ENG_WRITING_PARTICIPANTS")],
                                         c(1),
                                         function (school) {
                                           achievement.tested <- sum(school[c("MATH_TESTED",
                                                                              "READING_TESTED",
                                                                              "SCIENCE_TESTED",
                                                                              "ENG_WRITING_TESTED")])
                                           achievement.participants <- sum(school[c("MATH_PARTICIPANTS",
                                                                                    "READING_PARTICIPANTS",
                                                                                    "SCIENCE_PARTICIPANTS",
                                                                                    "ENG_WRITING_PARTICIPANTS")])
                                           participations.rate.achievement <- round((achievement.tested / achievement.participants) * 100, precision)
                                           
                                           result <- c(achievement.tested, achievement.participants, participations.rate.achievement)
                                           names(result) <- act.achievement.participation.labels
                                           result
                                           
                                         })))

#write.csv(act.participation, file="results/2012-13/act-participation.csv")

##calculate the mean z-score for each subject
act.achievement <- aggregate(data.frame(MATH_TESTED = ifelse(act.fay$TESTING_STATUS_CODE_MATH == "T", 1, 0),                                        
                                        READING_TESTED = ifelse(act.fay$TESTING_STATUS_CODE_READING == "T", 1, 0),                                        
                                        SCIENCE_TESTED = ifelse(act.fay$TESTING_STATUS_CODE_SCIENCE == "T", 1, 0),
                                        ENG_WRITING_TESTED = ifelse(act.fay$TESTING_STATUS_CODE_ENG_WRITING == "T", 1, 0),
                                        N_ACHIEVEMENT = act.fay$ANY_TESTED),
                             by = list(SCHOOL_YEAR=act.fay$SCHOOL_YEAR,
                                       SCHOOL_ID=act.fay$SCHOOL_ID),
                             sum)

act.achievement <- merge(act.achievement, 
                         aggregate(data.frame(MATH_MEAN_Z = act.fay[[act.accountability.z.score.labels['math']]],                                        
                                              READING_MEAN_Z = act.fay[[act.accountability.z.score.labels['reading']]],                                        
                                              SCIENCE_MEAN_Z = act.fay[[act.accountability.z.score.labels['science']]],                                        
                                              ENG_WRITING_MEAN_Z = act.fay[[act.accountability.z.score.labels['writing']]]),
                                   by = list(SCHOOL_YEAR=act.fay$SCHOOL_YEAR,
                                             SCHOOL_ID=act.fay$SCHOOL_ID),
                                   mean, na.rm=TRUE),
                         by=c("SCHOOL_ID", "SCHOOL_YEAR"),
                         all.x=TRUE)
                         
#calculate the mean z-score across subjects
act.fay.long <- reshape(act.fay,
                        varying=list(c("TESTING_STATUS_CODE_MATH",
                                       "TESTING_STATUS_CODE_READING",
                                       "TESTING_STATUS_CODE_SCIENCE",
                                       "TESTING_STATUS_CODE_ENG_WRITING"),
                                     act.accountability.z.score.labels),
                        v.names=c("TESTING_STATUS_CODE", "Z_SCORE"),
                        timevar = "SUBJECT_AREA",
                        times = c("MATH", "READING", "SCIENCE", "ENG_WRITING"),
                        direction="long")


#should be empty
with(act.fay.long, act.fay.long[TESTING_STATUS_CODE == 'T' & is.na(Z_SCORE),])
with(act.fay.long, act.fay.long[TESTING_STATUS_CODE == 'N' & !is.na(Z_SCORE),])
with(act.fay.long, act.fay.long[TESTING_STATUS_CODE == 'X' & !is.na(Z_SCORE),])
table(act.fay.long$TESTING_STATUS_CODE)


act.fay.long.testers <- act.fay.long[act.fay.long$TESTING_STATUS_CODE == 'T',]

achievement.hs.aggregate <- function (g) round(100*mean(g),0)

achievement.hs <- aggregate(data.frame(ACHIEVEMENT_HS=act.fay.long.testers$Z_SCORE),
                            by=list(SCHOOL_YEAR=act.fay.long.testers$SCHOOL_YEAR,
                                    SCHOOL_ID=act.fay.long.testers$SCHOOL_ID),
                            achievement.hs.aggregate)

achievement.hs.N <- aggregate(data.frame(N_ACHIEVEMENT_HS=act.fay.long.testers$WISER_ID),
                            by=list(SCHOOL_YEAR=act.fay.long.testers$SCHOOL_YEAR,
                                    SCHOOL_ID=act.fay.long.testers$SCHOOL_ID),
                            function(rows) length(unique(rows)))

achievement.hs <- merge(achievement.hs, achievement.hs.N)
head(achievement.hs)                                


#include state tally
achievement.hs.state <- aggregate(data.frame(ACHIEVEMENT_HS=act.fay.long.testers$Z_SCORE),
                                  by=list(SCHOOL_YEAR=act.fay.long.testers$SCHOOL_YEAR),
                                  achievement.hs.aggregate)

achievement.hs.N.state <- aggregate(data.frame(N_ACHIEVEMENT_HS=act.fay.long.testers$WISER_ID),
                              by=list(SCHOOL_YEAR=act.fay.long.testers$SCHOOL_YEAR),
                              function(rows) length(unique(rows)))

achievement.hs.state <- merge(achievement.hs.state, achievement.hs.N.state)

achievement.hs.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(achievement.hs.state)), achievement.hs.state)

achievement.hs <- rbind(achievement.hs, 
                        achievement.hs.state[,c("SCHOOL_YEAR", "SCHOOL_ID", 
                                                names(achievement.hs.state[-1:-2]))])

tail(achievement.hs)



achievement.hs <- merge(achievement.hs, act.participation[,c("SCHOOL_ID", 
                                                             "SCHOOL_YEAR", 
                                                             "PARTICIPATION_RATE_ACHIEVEMENT_HS")])



head(achievement.hs)

quantile(achievement.hs[achievement.hs$SCHOOL_YEAR == current.school.year & 
                          achievement.hs$N_ACHIEVEMENT_HS >= min.N.achievement.hs &
                          achievement.hs$PARTICIPATION_RATE_ACHIEVEMENT_HS >= 90 & 
                          achievement.hs$SCHOOL_ID != state.school.id,]$ACHIEVEMENT_HS, 
         probs=c(.35,.65),
         type=6)

##resume here

#write.csv(act.achievement, file="results/2012-13/act-achievement.csv", na="")


##assign achievement SPL
schools <- calc.school.achievement.hs(schools)

#look at distribution of computed target levels
head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==current.school.year,])
table(schools[schools$SCHOOL_YEAR==current.school.year,]$ACHIEVEMENT_TARGET_LEVEL_HS)


##show cross tabs of achievement level and alternative school status and percentages of same
achievement.level.freq <- table(schools[schools$SCHOOL_YEAR==current.school.year,]$ACHIEVEMENT_TARGET_LEVEL_HS,
      schools[schools$SCHOOL_YEAR==current.school.year,]$ALTERNATIVE_SCHOOL)

achievement.level.freq <- cbind(achievement.level.freq, Both=apply(achievement.level.freq,
                                       c(1),
                                       function (r) r[1] + r[2]))

achievement.level.freq.prop <- t(round(100*prop.table(t(achievement.level.freq),1), 1))
##cumulative distributions for achievement scores by alternative school status
achievement.score.freq <- table(schools[schools$SCHOOL_YEAR==current.school.year,]$ACHIEVEMENT_HS,
      schools[schools$SCHOOL_YEAR==current.school.year,]$ALTERNATIVE_SCHOOL)
achievement.score.freq <- cbind(achievement.score.freq, 
                                Both=apply(achievement.score.freq,
                                           c(1),
                                           function (r) r[1] + r[2]))

achievement.score.freq.prop <- t(round(100*prop.table(t(achievement.score.freq),1), 1))
achievement.score.freq.prop.names <- colnames(achievement.score.freq.prop)
achievement.score.freq.prop <- do.call(cbind, lapply(1:3, function (i) cumsum(achievement.score.freq.prop[,i])))
colnames(achievement.score.freq.prop) <- achievement.score.freq.prop.names
head(achievement.score.freq.prop,20)
#####

quantile(schools[schools$SCHOOL_YEAR==current.school.year &
                   
                   schools$ALTERNATIVE_SCHOOL=='F',]$ACHIEVEMENT_HS, probs=c(0.35, 0.65), na.rm=TRUE)
#should all be small schools
schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==current.school.year & is.na(schools$ACHIEVEMENT_HS),]

#plot histogram of scores and normal distribution superimposed
achievement.hs.scores <- schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
                                   schools$SCHOOL_YEAR==current.school.year & 
                                   schools$SCHOOL_ID != state.school.id & 
                                   !is.na(schools$ACHIEVEMENT_HS),"ACHIEVEMENT_HS"]


cast(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
               schools$SCHOOL_YEAR==current.school.year & 
               schools$SCHOOL_ID != state.school.id & 
               !is.na(schools$ACHIEVEMENT_HS),c("SCHOOL_ID", "ACHIEVEMENT_TARGET_LEVEL_HS", "ALTERNATIVE_SCHOOL")],
     forumula=ACHIEVEMENT_TARGET_LEVEL_HS + ALTERNATIVE_SCHOOL ~ .)


cast(achievement.molten, )

cast(schools[schools$SCHOOL_YEAR==current.school.year & 
               schools$SCHOOL_ID != state.school.id,
             ,c("ACHIEVEMENT_TARGET_LEVEL_HS", "ALTERNATIVE_SCHOOL")],
     ACHIEVEMENT_TARGET_LEVEL_HS ~ ALTERNATIVE_SCHOOL, margins=TRUE)

source("plot-funs.R")
plot.hist(achievement.hs.scores,15)
