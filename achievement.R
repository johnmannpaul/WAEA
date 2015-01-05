load("data/g38.achieve.Rdata")
load("data/g38.achieve.lookback.Rdata")

g38.achieve.all <- rbind(g38.achieve.lookback,g38.achieve)

table(g38.achieve.all[c("SCHOOL_YEAR", "TEST_TYPE")], useNA="ifany")



achievement.g38.indicator <- compute.indicator.long(g38.achieve.all, 
                                                    g38.achieve.all,
                                                    schools,
                                                    school.types = nonHS.types,
                                                    indicator.label="G38_ACHIEVEMENT_ALL",
                                                    score.prefix="PERFORMANCE_LEVEL",
                                                    agg.fun=function (g) c(N_TESTS=length(g),
                                                                           N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                           PERCENT_PROFICIENT=round((sum(ifelse(g %in% c('3','4'), 1, 0))/length(g))*100, 0)))

achievement.g38.labels <- names(achievement.g38.indicator$schools)[grep("^G38_ACHIEVEMENT_ALL", 
                                                                    names(achievement.g38.indicator$schools))]

schools[,achievement.g38.labels] <- achievement.g38.indicator$schools[,achievement.g38.labels]

table(schools[c("SCHOOL_YEAR", "G38_ACHIEVEMENT_ALL_SMALL_SCHOOL")])

schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
          schools$SCHOOL_YEAR == current.school.year &
          schools$G38_ACHIEVEMENT_ALL_N >= min.N.achievement,]

schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
          schools$SCHOOL_YEAR == current.school.year &
          schools$G38_ACHIEVEMENT_ALL_N >= min.N.achievement &
          is.na(schools$G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE),]

g38.participation.rate <- schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                                    schools$SCHOOL_YEAR == current.school.year &
                                    schools$G38_ACHIEVEMENT_ALL_N >= min.N.achievement,"G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE"]

g38.participation.rate[order(g38.participation.rate)]


# schools$G38_ACHIEVEMENT_ALL_TARGET_LEVEL <- findInterval(schools$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT,
#                                                          round(quantile(with(schools,
#                                                                              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
#                                                                                        schools$SCHOOL_YEAR == current.school.year &
#                                                                                        schools$G38_ACHIEVEMENT_ALL_N >= min.N.achievement &
#                                                                                        schools$G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE > 0.9
#                                                                                      ,]$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT), 
#                                                                         probs=c(.35,.65),
#                                                                         type=6)
#                                                                ,0)) + 1

schools$G38_ACHIEVEMENT_ALL_TARGET_LEVEL <- findInterval(schools$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT,
                                                         g38.achievement.cuts) + 1

write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                    schools$SCHOOL_YEAR == current.school.year &
                    schools$G38_ACHIEVEMENT_ALL_N >= min.N.achievement &
                    schools$SCHOOL_ID != state.school.id, 
                  c("SCHOOL_YEAR", "NAME", "SCHOOL_ID", 
                    "GRADE_BAND_COMPOSITION", 
                    "G38_ACHIEVEMENT_ALL_N", 
                    "G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE", 
                    "G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT"),],
          file=get.filename("g38-achievement-cfds", "results/cfds"),
          na="",
          row.names=FALSE)


table(schools[schools$SCHOOL_YEAR == current.school.year, c("GRADE_BAND_COMPOSITION", "G38_ACHIEVEMENT_ALL_TARGET_LEVEL")])
prop.table(table(schools[schools$SCHOOL_YEAR == current.school.year, c("GRADE_BAND_COMPOSITION", "G38_ACHIEVEMENT_ALL_TARGET_LEVEL")]),1)


