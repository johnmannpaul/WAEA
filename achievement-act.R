##from data/ACT/2013-08-21 Confidential ACT_STUDENT_RESULTS_2013_WAEA.xlsx 
#save(act, file="data/ACT/act.R")
#
act.participation <- aggregate(data.frame(MATH_TESTED = ifelse(act.df$TESTING_STATUS_CODE_MATH == "T", 1, 0),
                                          MATH_PARTICIPANTS = ifelse(act.df$TESTING_STATUS_CODE_MATH == "X", 0, 1),
                                          READING_TESTED = ifelse(act.df$TESTING_STATUS_CODE_READING == "T", 1, 0),
                                          READING_PARTICIPANTS = ifelse(act.df$TESTING_STATUS_CODE_READING == "X", 0, 1),
                                          SCIENCE_TESTED = ifelse(act.df$TESTING_STATUS_CODE_SCIENCE == "T", 1, 0),
                                          SCIENCE_PARTICIPANTS = ifelse(act.df$TESTING_STATUS_CODE_SCIENCE == "X", 0, 1),
                                          N_ACHIEVEMENT = act.df$ANY_TESTED),
                               by = list(SCHOOL_YEAR=act.df$SCHOOL_YEAR,
                                         SCHOOL_ID=act.df$SCHOOL_ID),
                               sum)

#do state participation 
act.participation.state <- aggregate(act.participation[,c("MATH_TESTED","MATH_PARTICIPANTS","READING_TESTED","READING_PARTICIPANTS","SCIENCE_TESTED","SCIENCE_PARTICIPANTS","N_ACHIEVEMENT")],
                                     by = list(SCHOOL_YEAR=act.participation$SCHOOL_YEAR), sum)


act.participation.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(act.participation.state)), act.participation.state)

act.participation <- rbind(act.participation, act.participation.state)

tail(act.participation)

act.participation$MATH_PARTICIPATION_RATE <- round((act.participation$MATH_TESTED / act.participation$MATH_PARTICIPANTS) * 100, precision)                               
act.participation$READING_PARTICIPATION_RATE <- round((act.participation$READING_TESTED / act.participation$READING_PARTICIPANTS) * 100, precision)                               
act.participation$SCIENCE_PARTICIPATION_RATE <- round((act.participation$SCIENCE_TESTED / act.participation$SCIENCE_PARTICIPANTS) * 100, precision)                                                                         




# act.participation$N_ACHIEVEMENT_OLD <- apply(act.participation[,c("MATH_TESTED","READING_TESTED","SCIENCE_TESTED")],
#                                             c(1),
#                                             function (school) {
#                                               max(school)
#                                             })
# 
# with(act.participation, act.participation[N_ACHIEVEMENT != N_ACHIEVEMENT_OLD,])

act.participation$PARTICIPATION_RATE_ACHIEVEMENT <- apply(act.participation[,c("MATH_TESTED",
                                                                               "READING_TESTED",
                                                                               "SCIENCE_TESTED",
                                                                               "MATH_PARTICIPANTS",
                                                                               "READING_PARTICIPANTS",
                                                                               "SCIENCE_PARTICIPANTS")],
                                         c(1),
                                         function (school) {
                                           round((sum(school[c("MATH_TESTED",
                                                               "READING_TESTED",
                                                               "SCIENCE_TESTED")]) / sum(school[c("MATH_PARTICIPANTS",
                                                                                                  "READING_PARTICIPANTS",
                                                                                                  "SCIENCE_PARTICIPANTS")])) * 100, precision)
                                           
                                         })

#write.csv(act.participation, file="results/2012-13/act-participation.csv")

##calculate the percent proficient...
act.achievement <- aggregate(data.frame(MATH_TESTED = ifelse(act.df$TESTING_STATUS_CODE_MATH == "T", 1, 0),
                                        MATH_PROFICIENT = ifelse(act.df$WDE_PERFORMANCE_LEVEL_MATH %in% c("3","4"), 1, 0),
                                        READING_TESTED = ifelse(act.df$TESTING_STATUS_CODE_READING == "T", 1, 0),
                                        READING_PROFICIENT = ifelse(act.df$WDE_PERFORMANCE_LEVEL_READING %in% c("3","4"), 1, 0),
                                        SCIENCE_TESTED = ifelse(act.df$TESTING_STATUS_CODE_SCIENCE == "T", 1, 0),
                                        SCIENCE_PROFICIENT = ifelse(act.df$WDE_PERFORMANCE_LEVEL_SCIENCE %in% c("3","4"), 1, 0)),
                             by = list(SCHOOL_YEAR=act.df$SCHOOL_YEAR,
                                       SCHOOL_ID=act.df$SCHOOL_ID),
                             sum)

act.achievement <- cbind(act.achievement, 
                         data.frame(t(apply(act.achievement[,c("MATH_TESTED", 
                                                               "MATH_PROFICIENT",
                                                               "READING_TESTED",
                                                               "READING_PROFICIENT",
                                                               "SCIENCE_TESTED",
                                                               "SCIENCE_PROFICIENT")], c(1),                                                                                      
                                            FUN=function (school) {
                                              result <- c(N_TEST_EVENTS=sum(school[c("MATH_TESTED",
                                                                                      "READING_TESTED",
                                                                                      "SCIENCE_TESTED")]),
                                                          PROFICIENT_TEST_EVENTS=sum(school[c("MATH_PROFICIENT",
                                                                                               "READING_PROFICIENT",
                                                                                               "SCIENCE_PROFICIENT")]))
                                              
                                                                }))))

#include state tally
act.achievement.state <- aggregate(act.achievement[, setdiff(names(act.achievement), c("SCHOOL_YEAR", "SCHOOL_ID"))],
                                   by = list(SCHOOL_YEAR = act.achievement$SCHOOL_YEAR),
                                   sum)

act.achievement.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(act.achievement.state)), act.achievement.state)

act.achievement <- rbind(act.achievement, act.achievement.state)

tail(act.achievement)


act.achievement$PERCENT_PROFICIENT <- round((act.achievement$PROFICIENT_TEST_EVENTS / act.achievement$N_TEST_EVENTS) * 100, precision) 

act.achievement <- merge(act.achievement, act.participation[,c("SCHOOL_ID", "SCHOOL_YEAR", "N_ACHIEVEMENT", "PARTICIPATION_RATE_ACHIEVEMENT")])

#act.achievement <- act.achievement[,c("SCHOOL_YEAR", "SCHOOL_ID", "PERCENT_PROFICIENT", "N_ACHIEVEMENT", "PARTICIPATION_RATE_ACHIEVEMENT")]

head(act.achievement)

quantile(act.achievement[act.achievement$SCHOOL_YEAR == current.school.year & 
                           act.achievement$N_ACHIEVEMENT > 5 &
                           act.achievement$PARTICIPATION_RATE_ACHIEVEMENT >= 90 & 
                           act.achievement$SCHOOL_ID != state.school.id,]$PERCENT_PROFICIENT, 
         probs=c(.35,.65))

#write.csv(act.achievement, file="results/2012-13/act-achievement.csv", na="")


##assign achievement SPL
schools <- calc.school.achievement.hs(schools)

#look at distribution of computed target levels
table(schools[schools$SCHOOL_YEAR==current.school.year,]$ACHIEVEMENT_TARGET_LEVEL_HS)

