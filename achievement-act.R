source("mean-z-score-fun.R")
##from data/ACT/2013-08-21 Confidential ACT_STUDENT_RESULTS_2013_WAEA.xlsx 
#save(act, file="data/ACT/act.R")
#

#Participation includes everyone (not just full-academic-year students).  So, use full "act" data.frame.
#We only base participation rate on the participation in the current year.  No lookback classes are inlcuded.
source("participation-fun.R")

act.participation <- calc.participation.rate(act)
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
achievement.hs <- calc.mean.score(act.fay)
names(achievement.hs) <- c("SCHOOL_YEAR", "SCHOOL_ID", "ACHIEVEMENT_HS", "N_ACHIEVEMENT_HS") 

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



source("plot-funs.R")
plot.hist(achievement.hs.scores,15)
