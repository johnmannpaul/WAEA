load("data/g38.achieve.Rdata")
load("data/g38.achieve.lookback.Rdata")

g38.achieve.all <- rbind(g38.achieve.lookback,g38.achieve)

table(g38.achieve.all$SCHOOL_YEAR)

g38.achieve.all$GRADE_BAND <- unlist(lapply(g38.achieve.all$GRADE_ENROLLED,
                                 function (x) {
                                   if (is.na(x))
                                     NA
                                   else
                                     band.lookup[[x]]                                                                                
                                 }))


compute.grade.band.achievement <- function (band) {
  indicator.label <- paste("ACHIEVEMENT_G38_B", band, sep="")
  
  #limit scores to current band
  student.df.scores <- g38.achieve.all[g38.achieve.all$GRADE_BAND==band,]
  
  #limit scores to FAY and tests taken
  student.df.fay <- student.df.scores[student.df.scores[["SCHOOL_FULL_ACADEMIC_YEAR"]] == 'T' &
                                        student.df.scores[["TESTING_STATUS_CODE"]] == "T",]   
  
  
  achievement.indicator <- unmatrixfy.df(calc.mean.score(student.df.fay, 
                                                         testing.status.prefix = "TESTING_STATUS_CODE",
                                                         score.prefix="PERFORMANCE_LEVEL",
                                                         prefix.sep="_",
                                                         agg.function=function (g) c(N_TESTS=length(g),
                                                                                     N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                                     PERCENT_PROFICIENT=round((sum(ifelse(g %in% c('3','4'), 1, 0))/length(g))*100, 0)),
                                                         already.long=TRUE), prepend=FALSE)
  
  
  names(achievement.indicator) <- sapply(names(achievement.indicator), 
                                         function (n) if (n %in% c("SCHOOL_YEAR","SCHOOL_ID")) n else paste(indicator.label, n, sep="_"),
                                         USE.NAMES=FALSE)

  indicator.labels.min.N <- names(achievement.indicator)[(ncol(achievement.indicator)-1):ncol(achievement.indicator)]
  names(indicator.labels.min.N) <- c("indicator.score","N")
  
  achievement.indicator <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types, 
                                         c("SCHOOL_YEAR","SCHOOL_ID")],                           
                                 achievement.indicator,
                                 all=TRUE)
  
  N.labels <- setdiff(names(achievement.indicator)[grep(paste(indicator.label, "N", sep="_"), 
                                                        names(achievement.indicator),
                                                        fixed=TRUE)], c("SCHOOL_YEAR","SCHOOL_ID"))
  
  achievement.indicator[,N.labels] <- zero.na.rows(achievement.indicator,
                                                   N.labels)
  
  achievement.labels <- names(achievement.indicator)[grep(paste("^",indicator.label, sep=""), 
                                                          names(achievement.indicator))]
  
  schools[,achievement.labels] <<- bind.indicator(schools, 
                                                  achievement.indicator,                               
                                                  indicator.labels.min.N = indicator.labels.min.N,
                                                  min.N.achievement)[,achievement.labels]
}


lapply(1:2,
       compute.grade.band.achievement)


# write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
#           schools$SCHOOL_YEAR == current.school.year &
#           schools$ACHIEVEMENT_G38_B1_N >= min.N.achievement,c("SCHOOL_YEAR", "SCHOOL_ID", "ACHIEVEMENT_G38_B1_N", "ACHIEVEMENT_G38_B1_PERCENT_PROFICIENT")],
#           file="results/g38-achievement-band-1-cfd.csv",
#           na="",
#           row.names=FALSE)
# 
# write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
#           schools$SCHOOL_YEAR == current.school.year &
#           schools$ACHIEVEMENT_G38_B2_N >= min.N.achievement,c("SCHOOL_YEAR", "SCHOOL_ID", "ACHIEVEMENT_G38_B2_N", "ACHIEVEMENT_G38_B2_PERCENT_PROFICIENT")],
#           file="results/g38-achievement-band-2-cfd.csv",
#           na="",
#           row.names=FALSE)

achievement.g38.indicator <- compute.indicator.long(g38.achieve.all, 
                                                    g38.achieve.all,
                                                    schools,
                                                    school.types = nonHS.types,
                                                    indicator.label="ACHIEVEMENT_G38_ALL",
                                                    score.prefix="PERFORMANCE_LEVEL",
                                                    agg.fun=function (g) c(N_TESTS=length(g),
                                                                          N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                          PERCENT_PROFICIENT=round((sum(ifelse(g %in% c('3','4'), 1, 0))/length(g))*100, 0)))

achievement.labels <- names(achievement.g38.indicator$schools)[grep("^ACHIEVEMENT_G38_ALL", 
                                                                   names(achievement.g38.indicator$schools))]

schools[,achievement.labels] <- achievement.g38.indicator$schools[,achievement.labels]

table(schools[c("SCHOOL_YEAR", "ACHIEVEMENT_G38_ALL_SMALL_SCHOOL")])

schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                    schools$SCHOOL_YEAR == current.school.year &
                    schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement,]

schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
          schools$SCHOOL_YEAR == current.school.year &
          schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement &
          is.na(schools$ACHIEVEMENT_G38_ALL_PARTICIPATION_RATE),]

g38.participation.rate <- schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
          schools$SCHOOL_YEAR == current.school.year &
          schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement,"ACHIEVEMENT_G38_ALL_PARTICIPATION_RATE"]

g38.participation.rate[order(g38.participation.rate)]


#compare quantiles for grade bands 1 and 2
quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$ACHIEVEMENT_G38_B1_N >= min.N.achievement
                        ,]$ACHIEVEMENT_G38_B1_PERCENT_PROFICIENT), 
         probs=c(.35,.65),
         type=6)

quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$ACHIEVEMENT_G38_B2_N >= min.N.achievement
                      ,]$ACHIEVEMENT_G38_B2_PERCENT_PROFICIENT), 
         probs=c(.35,.65),
         type=6)

#compare quantiles for grade bands 1 and 2 on a more nuanced scale
quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$ACHIEVEMENT_G38_B1_N >= min.N.achievement
                      ,]$ACHIEVEMENT_G38_B1_PERCENT_PROFICIENT), 
         probs=seq(from=0, to=1, by=.1),
         type=6)

quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$ACHIEVEMENT_G38_B2_N >= min.N.achievement
                      ,]$ACHIEVEMENT_G38_B2_PERCENT_PROFICIENT), 
         probs=seq(from=0, to=1, by=.1),
         type=6)

grade.bands.ttest <- t.test(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                                      schools$SCHOOL_YEAR == current.school.year &
                                      schools$ACHIEVEMENT_G38_B1_N >= min.N.achievement
                                    ,]$ACHIEVEMENT_G38_B1_PERCENT_PROFICIENT,
                            schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                                      schools$SCHOOL_YEAR == current.school.year &
                                      schools$ACHIEVEMENT_G38_B2_N >= min.N.achievement
                                    ,]$ACHIEVEMENT_G38_B2_PERCENT_PROFICIENT,
                         alternative="two.sided",
                         var.equal = TRUE)

grade.bands.ttest

#examine the assumption that the variances are equal
grade.bands.vartest <- var.test(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                                       schools$SCHOOL_YEAR == current.school.year &
                                       schools$ACHIEVEMENT_G38_B1_N >= min.N.achievement
                                     ,]$ACHIEVEMENT_G38_B1_PERCENT_PROFICIENT,
                             schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                                       schools$SCHOOL_YEAR == current.school.year &
                                       schools$ACHIEVEMENT_G38_B2_N >= min.N.achievement
                                     ,]$ACHIEVEMENT_G38_B2_PERCENT_PROFICIENT,
                             alternative="two.sided")

grade.bands.vartest



#conclusion: In 2013-14 there is so little difference in the two bands within 
#the 20% and 80% range that it there is no longer justification for using cuts 
#based on two separate grade bands.  One set of cuts will be determined based on 
#combined achievement from both bands.

schools$ACHIEVEMENT_G38_ALL_TARGET_LEVEL <- findInterval(schools$ACHIEVEMENT_G38_ALL_PERCENT_PROFICIENT,
                                                    round(quantile(with(schools,
                                                                        schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                                                                                  schools$SCHOOL_YEAR == current.school.year &
                                                                                  schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement &
                                                                                  schools$ACHIEVEMENT_G38_ALL_PARTICIPATION_RATE > 0.9
                                                                                ,]$ACHIEVEMENT_G38_ALL_PERCENT_PROFICIENT), 
                                                                   probs=c(.35,.65),
                                                                   type=6)
                                                          ,0)) + 1

# write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
#                     schools$SCHOOL_YEAR == current.school.year &
#                     schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement, 
#                   c("SCHOOL_YEAR", "SCHOOL_ID", 
#                     "GRADE_BAND_COMPOSITION", 
#                     "ACHIEVEMENT_G38_ALL_N", 
#                     "ACHIEVEMENT_G38_ALL_PARTICIPATION_RATE", 
#                     "ACHIEVEMENT_G38_ALL_PERCENT_PROFICIENT"),],
#           file="results/g38-achievement-cfds.csv",
#           na="",
#           row.names=FALSE)

table(schools[schools$SCHOOL_YEAR == current.school.year, c("GRADE_BAND_COMPOSITION", "ACHIEVEMENT_G38_ALL_TARGET_LEVEL")])
prop.table(table(schools[schools$SCHOOL_YEAR == current.school.year, c("GRADE_BAND_COMPOSITION", "ACHIEVEMENT_G38_ALL_TARGET_LEVEL")]),1)

quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement
                      ,]$ACHIEVEMENT_G38_ALL_PERCENT_PROFICIENT), 
         probs=c(.35,.65),
         type=6)


quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types & schools$GRADE_BAND_COMPOSITION=='> 6 only' &
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement 
                      ,]$ACHIEVEMENT_G38_ALL_PERCENT_PROFICIENT), 
         probs=c(.35,.65),
         type=6)


quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types & schools$GRADE_BAND_COMPOSITION=='mixed' &
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement 
                      ,]$ACHIEVEMENT_G38_ALL_PERCENT_PROFICIENT), 
         probs=c(.35,.65),
         type=6)


quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types & schools$GRADE_BAND_COMPOSITION=='< 7 only' &                        
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement 
                      ,]$ACHIEVEMENT_G38_ALL_PERCENT_PROFICIENT), 
         probs=c(.35,.65),
         type=6)


junior.highs.pp <- schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types & schools$GRADE_BAND_COMPOSITION=='> 6 only' &
          schools$SCHOOL_YEAR == current.school.year &
          schools$ACHIEVEMENT_G38_B2_N >= min.N.achievement 
        ,]$ACHIEVEMENT_G38_B2_PERCENT_PROFICIENT

junior.highs.pp

mixed.grades.pp <- schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types & schools$GRADE_BAND_COMPOSITION=='mixed' &
                             schools$SCHOOL_YEAR == current.school.year &
                             schools$ACHIEVEMENT_G38_B2_N >= min.N.achievement 
                           ,]$ACHIEVEMENT_G38_B2_PERCENT_PROFICIENT
mixed.grades.pp
#Perform a ttest to examine the hypothesis that 7th and 8th graders at " > 6" schools
#have a lower percent proficient than 7th and 8th gradders at "mixed" schools.
#Perorm this test in the face of the assumption that the variances are equal.
grade.78.ttest <- t.test(junior.highs.pp,
       mixed.grades.pp,
       alternative="less",
       var.equal = TRUE)

grade.78.ttest

#examine the assumption that the variances are equal
grade.78.vartest <- var.test(junior.highs.pp,
                             mixed.grades.pp,
                             alternative="two.sided")

grade.78.vartest

##check that we have a disjoint partition of the nonHS schools
nrow(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types & schools$GRADE_BAND_COMPOSITION=='> 6 only' &
          schools$SCHOOL_YEAR == current.school.year &
          schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement 
        ,]) +
nrow(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types & schools$GRADE_BAND_COMPOSITION=='mixed' &
               schools$SCHOOL_YEAR == current.school.year &
               schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement 
             ,]) +
nrow(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types & schools$GRADE_BAND_COMPOSITION=='< 7 only' &
               schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
               schools$SCHOOL_YEAR == current.school.year &
               schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement 
             ,])  
  
nrow(schools[
          schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
          schools$SCHOOL_YEAR == current.school.year &
          schools$ACHIEVEMENT_G38_ALL_N >= min.N.achievement 
        ,])





