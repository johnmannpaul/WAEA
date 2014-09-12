load(file="data/ACT/act.achieve.Rdata")
load(file="data/act.lookback.Rdata")
load(file="data/paws.lookback.2yr.Rdata")

act.achieve <- rbind(paws.lookback.2yr, act.lookback, act.achieve)
table(act.achieve$SCHOOL_YEAR)
achievement.hs.indicator <- compute.indicator.long(act.achieve, 
                                                   act.achieve,
                                                   schools,
                                                   indicator.label="HS_ACHIEVEMENT",
                                                   score.prefix="PERFORMANCE_LEVEL",
                                                   agg.fun=function (g) c(N_TESTS=length(g),
                                                                          N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                          PERCENT_PROFICIENT=round((sum(ifelse(g %in% c('3','4'), 1, 0))/length(g))*100, 0)))

achievement.labels <- names(achievement.hs.indicator$schools)[grep("^HS_ACHIEVEMENT", 
                                                                   names(achievement.hs.indicator$schools))]
schools[,achievement.labels] <- achievement.hs.indicator$schools[,achievement.labels]

table(schools[c("SCHOOL_YEAR", "HS_ACHIEVEMENT_SMALL_SCHOOL")])

schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% HS.types &
          schools$HS_ACHIEVEMENT_SMALL_SCHOOL == 'T',]

##check and compare the participation rates
table(findInterval(schools[schools$SCHOOL_YEAR==current.school.year,"HS_ACHIEVEMENT_PARTICIPATION_RATE"], c(90, 95)))
table(schools[schools$SCHOOL_YEAR==current.school.year &
              schools$WAEA_SCHOOL_TYPE %in% HS.types &
                schools$SCHOOL_ID != state.school.id,
              c("ALTERNATIVE_SCHOOL","HS_ACHIEVEMENT_SMALL_SCHOOL")])
##end checks


quantile(with(schools,
              schools[SCHOOL_YEAR == current.school.year &                     
                        HS_ACHIEVEMENT_N >= min.N.achievement.hs &
                        !is.na(HS_ACHIEVEMENT_PARTICIPATION_RATE) &  
                        HS_ACHIEVEMENT_PARTICIPATION_RATE >= .90 & 
                        SCHOOL_ID != state.school.id,]$HS_ACHIEVEMENT_PERCENT_PROFICIENT), 
         probs=c(.35,.65),
         type=6)


schools$HS_ACHIEVEMENT_TARGET_LEVEL <- findInterval(schools$HS_ACHIEVEMENT_PERCENT_PROFICIENT,
                                                    round(quantile(with(schools,
                                                                        schools[SCHOOL_YEAR == current.school.year &                     
                                                                                  HS_ACHIEVEMENT_N >= min.N.achievement.hs &
                                                                                  !is.na(HS_ACHIEVEMENT_PARTICIPATION_RATE) &  
                                                                                  HS_ACHIEVEMENT_PARTICIPATION_RATE >= .90 & 
                                                                                  SCHOOL_ID != state.school.id,]$HS_ACHIEVEMENT_PERCENT_PROFICIENT), 
                                                                   probs=c(.35,.65),
                                                                   type=6)
                                                                         ,0)) + 1


# schools <- bind.indicator(schools, 
#                           act.achieve.fay.school[act.achieve.fay.school$SCHOOL_YEAR==current.school.year,],                               
#                           indicator.labels.min.N = c("PERCENT_PROFICIENT", N="N_ACHIEVEMENT", "HS_ACHIEVEMENT_TARGET_LEVEL"),
#                           min.N.achievement.hs)

table(schools[c("SCHOOL_YEAR","HS_ACHIEVEMENT_TARGET_LEVEL")])



##assign achievement SPL


#look at distribution of computed target levels
head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==current.school.year,])
table(schools[schools$SCHOOL_YEAR==current.school.year,]$HS_ACHIEVEMENT_TARGET_LEVEL)


##show cross tabs of achievement level and alternative school status and percentages of same
achievement.level.freq <- table(schools[schools$SCHOOL_YEAR==current.school.year,c("HS_ACHIEVEMENT_TARGET_LEVEL","ALTERNATIVE_SCHOOL")])

achievement.level.freq <- cbind(achievement.level.freq, Both=apply(achievement.level.freq,
                                       c(1),
                                       function (r) r[1] + r[2]))

achievement.level.freq.prop <- t(round(100*prop.table(t(achievement.level.freq),1), 1))
##cumulative distributions for achievement scores by alternative school status
achievement.score.freq <- table(schools[schools$SCHOOL_YEAR==current.school.year,]$HS_ACHIEVEMENT_PERCENT_PROFICIENT,
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
                   
                   schools$ALTERNATIVE_SCHOOL=='F',]$HS_ACHIEVEMENT_PERCENT_PROFICIENT, probs=c(0.35, 0.65), na.rm=TRUE)
#should all be small schools
schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==current.school.year & is.na(schools$HS_ACHIEVEMENT_PERCENT_PROFICIENT),]

#plot histogram of scores and normal distribution superimposed
achievement.hs.scores <- schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
                                   schools$SCHOOL_YEAR==current.school.year & 
                                   schools$SCHOOL_ID != state.school.id & 
                                   !is.na(schools$HS_ACHIEVEMENT_PERCENT_PROFICIENT),"HS_ACHIEVEMENT_PERCENT_PROFICIENT"]



source("plot-funs.R")
plot.hist(achievement.hs.scores,15)


#  write.csv(file="results/high-schools-N-achievement.csv",
#        schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
#                  schools$SCHOOL_YEAR==current.school.year &
#                  schools$SCHOOL_ID != state.school.id, c("SCHOOL_YEAR", "SCHOOL_ID", "ALTERNATIVE_SCHOOL","N_HS_ACHIEVEMENT")],
#        na="",
#        row.names=FALSE)

# write.csv(file="results/high-school-achievement-cfds.csv",
#           schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
#                     schools$SCHOOL_YEAR==current.school.year &
#                     schools$N_HS_ACHIEVEMENT >= min.N.achievement.hs &
#                     schools$SCHOOL_ID != state.school.id, c("SCHOOL_YEAR", "SCHOOL_ID", 
#                                                             "ALTERNATIVE_SCHOOL",
#                                                             "N_HS_ACHIEVEMENT", 
#                                                             "HS_ACHIEVEMENT_PARTICIPATION_RATE",
#                                                             "PERCENT_PROFICIENT_HS_ACHIEVEMENT")],
#           na="",
#           row.names=FALSE)

