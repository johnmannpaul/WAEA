source("mean-z-score-fun.R")
##from data/ACT/2013-08-21 Confidential ACT_STUDENT_RESULTS_2013_WAEA.xlsx 
#save(act, file="data/ACT/act.R")
#

#Participation includes everyone (not just full-academic-year students).  So, use full "act" data.frame.
#We only base participation rate on the participation in the current year.  No lookback classes are inlcuded.
source("participation-fun.R")

#typo in the dataset
names(act.achieve)[7] <- "TESTING_STATUS_CODE_ENGLISH"

act.participation <- calc.participation.rate(act.achieve,
                                             subject.labels=c("MATH", "READING", "SCIENCE", "ENGLISH"),                                      
                                             status.prefix='TESTING_STATUS_CODE', 
                                             status.prefix.sep="_",
                                             status.codes=c(exempt='X', participated='T', did.not.participate='N'),
                                             total.participation.labels = c("ACHIEVEMENT_HS_TESTS_ACTUAL_COUNT", "ACHIEVEMENT_HS_TESTS_EXPECTED_COUNT", "PARTICIPATION_RATE_ACHIEVEMENT_HS"),
                                             precision=1)

nrow(act.participation)
act.participation <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types, 
                                   c("SCHOOL_YEAR", "SCHOOL_ID")],                           
                           act.participation,
                           all=TRUE)

table(act.participation$SCHOOL_YEAR)

table(findInterval(act.participation$PARTICIPATION_RATE_ACHIEVEMENT_HS, c(90, 95)))

nrow(act.achieve)
act.achieve.fay <- act.achieve[act.achieve$SCHOOL_FULL_ACADEMIC_YEAR == 'T',]                         
nrow(act.achieve.fay)

#small school
small.achieve.hs <- calc.small.schools(act.achieve.fay, schools, HS.types, "WISER_ID", attribute="ACHIEVEMENT_HS")
schools <- schools[,!(names(schools) %in% setdiff(names(small.achieve.hs$result.schools), names(schools)))]
schools <- cbind(schools, small.achieve.hs$result.schools[,setdiff(names(small.achieve.hs$result.schools), names(schools))])
act.achieve.fay$SCHOOL_YEAR_ORIGINAL <- act.achieve.fay$SCHOOL_YEAR
act.achieve.fay <- rbind(act.achieve.fay, small.equity.hs$result.students)



act.achieve.fay.school <- unmatrixfy.df(calc.mean.score(act.achieve.fay, 
                                          subject.labels=c(MATH="MATH", READING="READING", SCIENCE="SCIENCE", ENGLISH="ENGLISH"),
                                                       testing.status.prefix="TESTING_STATUS_CODE",
                                                       score.prefix="WDE_PERFORMANCE_LEVEL",
                                                       prefix.sep="_",
                                                       agg.function=function (g) c(N_TESTS=length(g),
                                                                                   N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)))))


names(act.achieve.fay.school)[3:5] <- c("N_TESTS", "N_PROFICIENT_TESTS", "N_ACHIEVEMENT")

head(act.achieve.fay.school)

act.achieve.fay.school$PERCENT_PROFICIENT <- round((act.achieve.fay.school$N_PROFICIENT_TESTS/act.achieve.fay.school$N_TESTS)*100, 0)

head(act.achieve.fay.school)

nrow(act.achieve.fay.school)
act.achieve.fay.school <- merge(act.achieve.fay.school, act.participation[,c("SCHOOL_YEAR", "SCHOOL_ID", "ACHIEVEMENT_HS_TESTS_ACTUAL_COUNT", "ACHIEVEMENT_HS_TESTS_EXPECTED_COUNT",
                                                                             "PARTICIPATION_RATE_ACHIEVEMENT_HS")], all.y=TRUE)
table(act.participation$SCHOOL_YEAR)

quantile(with(act.achieve.fay.school,
              act.achieve.fay.school[SCHOOL_YEAR == current.school.year &                     
                                       N_ACHIEVEMENT >= min.N.achievement.hs &
                                       !is.na(PARTICIPATION_RATE_ACHIEVEMENT_HS) &  
                                       PARTICIPATION_RATE_ACHIEVEMENT_HS >= .90 & 
                                       SCHOOL_ID != state.school.id,]$PERCENT_PROFICIENT), 
         probs=c(.35,.65),
         type=6)


act.achieve.fay.school$ACHIEVEMENT_HS_TARGET_LEVEL <- findInterval(act.achieve.fay.school$PERCENT_PROFICIENT,
                                                                   round(quantile(with(act.achieve.fay.school,
                                                                                       act.achieve.fay.school[SCHOOL_YEAR == current.school.year &                     
                                                                                                                N_ACHIEVEMENT >= min.N.achievement.hs &
                                                                                                                !is.na(PARTICIPATION_RATE_ACHIEVEMENT_HS) &  
                                                                                                                PARTICIPATION_RATE_ACHIEVEMENT_HS >= .90 & 
                                                                                                                SCHOOL_ID != state.school.id,]$PERCENT_PROFICIENT), 
                                                                                  probs=c(.35,.65),
                                                                                  type=6)
                                                                         ,0)) + 1


schools <- bind.indicator(schools, 
                          act.achieve.fay.school[act.achieve.fay.school$SCHOOL_YEAR==current.school.year,],                               
                          indicator.labels.min.N = c("PERCENT_PROFICIENT", N="N_ACHIEVEMENT", "ACHIEVEMENT_HS_TARGET_LEVEL"),
                          min.N.achievement.hs)

table(schools$ACHIEVEMENT_HS_TARGET_LEVEL)



##assign achievement SPL


#look at distribution of computed target levels
head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==current.school.year,])
table(schools[schools$SCHOOL_YEAR==current.school.year,]$ACHIEVEMENT_HS_TARGET_LEVEL)


##show cross tabs of achievement level and alternative school status and percentages of same
achievement.level.freq <- table(schools[schools$SCHOOL_YEAR==current.school.year,c("ACHIEVEMENT_HS_TARGET_LEVEL","ALTERNATIVE_SCHOOL")])

achievement.level.freq <- cbind(achievement.level.freq, Both=apply(achievement.level.freq,
                                       c(1),
                                       function (r) r[1] + r[2]))

achievement.level.freq.prop <- t(round(100*prop.table(t(achievement.level.freq),1), 1))
##cumulative distributions for achievement scores by alternative school status
achievement.score.freq <- table(schools[schools$SCHOOL_YEAR==current.school.year,]$PERCENT_PROFICIENT,
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
                   
                   schools$ALTERNATIVE_SCHOOL=='F',]$PERCENT_PROFICIENT, probs=c(0.35, 0.65), na.rm=TRUE)
#should all be small schools
schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==current.school.year & is.na(schools$PERCENT_PROFICIENT),]

#plot histogram of scores and normal distribution superimposed
achievement.hs.scores <- schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
                                   schools$SCHOOL_YEAR==current.school.year & 
                                   schools$SCHOOL_ID != state.school.id & 
                                   !is.na(schools$PERCENT_PROFICIENT),"PERCENT_PROFICIENT"]



source("plot-funs.R")
plot.hist(achievement.hs.scores,15)
