              
PROFICIENT <- ifelse(paws.df$ACCOUNTABILITY_PERF_LEVEL %in% c("3","4"), 1, 0)



achievement <- aggregate(data.frame(PROFICIENT=PROFICIENT, N=rep(1,length(PROFICIENT))), 
                         by=list(SCHOOL_YEAR=paws.df$SCHOOL_YEAR, 
                                 SCHOOL_ID=paws.df$SCHOOL_ID,
                                 GRADE_BAND=paws.df$GRADE_BAND), 
                         sum)

achievement.state <- aggregate(data.frame(PROFICIENT=PROFICIENT, N=rep(1,length(PROFICIENT))), 
                               by=list(SCHOOL_YEAR=paws.df$SCHOOL_YEAR, 
                                       GRADE_BAND=paws.df$GRADE_BAND), 
                               sum)

achievement.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(achievement.state)), achievement.state)

achievement <- rbind(achievement, achievement.state)

achievement$PERCENT_PROFICIENT <- round(achievement$PROFICIENT/achievement$N * 100, 1)


paws.tested <- with(paws.df, paws.df[TESTING_STATUS_CODE == "T", c("SCHOOL_YEAR", 
                                                                   "SCHOOL_ID",   
                                                                   "GRADE_BAND",
                                                                   "SCHOOL_YEAR_ORIGINAL",
                                                                   "WISER_ID")])

paws.tested.school <- cast(paws.tested, SCHOOL_YEAR+SCHOOL_ID~., 
                           function (x) length(unique(x)))

#for the state value we are counting scores from previous years that have been added to buttress small schools
paws.tested.state <- cast(paws.tested, SCHOOL_YEAR~., value="WISER_ID",
                          function (x) length(unique(x)))

paws.tested.state <- cbind(SCHOOL_ID = rep(state.school.id, nrow(paws.tested.state)), paws.tested.state)
paws.tested.school <- rbind(paws.tested.school, paws.tested.state)

names(paws.tested.school)[3] <- "ACCOUNTABILITY_N"

head(paws.tested.school)
tail(paws.tested.school)
##now by band
paws.tested.school.band <- cast(paws.tested, SCHOOL_YEAR+SCHOOL_ID+GRADE_BAND~.,  
                                function (x) length(unique(x)))

paws.tested.school.band.state <- cast(paws.tested, SCHOOL_YEAR+GRADE_BAND~.,  value="WISER_ID",
                                      function (x) length(unique(x)))

paws.tested.school.band.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(paws.tested.school.band.state)), paws.tested.school.band.state)
paws.tested.school.band <- rbind(paws.tested.school.band, paws.tested.school.band.state)



names(paws.tested.school.band)[4] <- "ACCOUNTABILITY_N_BAND"

head(paws.tested.school.band)
tail(paws.tested.school.band)
# paws.df.tested <- with(paws.df, paws.df[TESTING_STATUS_CODE == "T",])
# achievement.subject <- aggregate(data.frame(N=rep(1,nrow(paws.df.tested))), 
#                                  by=list(SCHOOL_YEAR=paws.df.tested$SCHOOL_YEAR, 
#                                          SUBJECT_CODE=paws.df.tested$SUBJECT_CODE,
#                                          SCHOOL_ID=paws.df.tested$SCHOOL_ID), 
#                                  sum)
# 
# 
# accountability.n_old <- aggregate(data.frame(OLD_ACCOUNTABILITY_N=achievement.subject$N), 
#                                by=list(SCHOOL_YEAR=achievement.subject$SCHOOL_YEAR, 
#                                        SCHOOL_ID=achievement.subject$SCHOOL_ID), 
#                                max)


#achievement <- achievement[, !(names(achievement) %in% c("OLD_ACCOUNTABILITY_N", "ACCOUNTABILITY_N"))]

#achievement <- merge(achievement, accountability.n_old)
achievement <- merge(achievement, paws.tested.school)
achievement <- merge(achievement, paws.tested.school.band)
head(with(achievement, achievement[SCHOOL_YEAR==current.school.year,]))
tail(with(achievement, achievement[SCHOOL_YEAR==current.school.year,]))



#with(achievement, achievement[OLD_ACCOUNTABILITY_N != ACCOUNTABILITY_N,])
##look at a school where old accountability n > accountability n

##look at 35% and 65% percentiles by grade band in the year 2011-12 for schools with at least 5 participants

# lapply(seq(1,3), 
#        function (band) {
#          quantile(achievement[achievement$SCHOOL_YEAR == current.school.year & 
#                                 achievement$ACCOUNTABILITY_N > 5 &
#                                 achievement$GRADE_BAND == band,]$PERCENT_PROFICIENT, 
#                   probs=c(.35,.65))
#       
# })


##look at proficiency and achievment N by grade band
percent.proficient.band <- with(achievement, 
                                cast(achievement[,c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_BAND", "PERCENT_PROFICIENT")],
                                     SCHOOL_YEAR+SCHOOL_ID~GRADE_BAND))

names(percent.proficient.band)[3:4] <- achievement.grade.band.labels[1:2]
with(percent.proficient.band, head(percent.proficient.band[SCHOOL_YEAR=='2012-13',]))


accountability.N.band <- with(achievement, 
                              cast(achievement[,c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_BAND", "ACCOUNTABILITY_N_BAND")],
                                   SCHOOL_YEAR+SCHOOL_ID~GRADE_BAND))
with(accountability.N.band, head(accountability.N.band[SCHOOL_YEAR=='2012-13',]))

names(accountability.N.band)[3:4] <- achievement.grade.band.labels[3:4]

achievement.bands <- merge(percent.proficient.band, accountability.N.band)


lapply(seq(1,3), 
       function (band) {
         quantile(achievement[achievement$SCHOOL_YEAR == current.school.year & achievement$SCHOOL_ID != state.school.id &
                                achievement$ACCOUNTABILITY_N_BAND > min.N.achievement &
                                achievement$GRADE_BAND == band,]$PERCENT_PROFICIENT, 
                  probs=c(.35,.65))
         
       })




##assign achievement SPL
schools <- calc.school.achievement(schools)

schools <- merge(schools, achievement.bands, all.x=TRUE)


#look at distribution of computed target levels
table(schools[schools$SCHOOL_YEAR==current.school.year,]$ACHIEVEMENT_TARGET_LEVEL)
schools[schools$SCHOOL_ID==state.school.id,]





