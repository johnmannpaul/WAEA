load("data/g38.achieve.Rdata")
load("data/paws.Rdata")
load("data/g38.achieve.lookback.Rdata")

g38.achieve.all.for.equity <- rbind(g38.achieve.lookback,g38.achieve)

g38.achieve.all.for.equity <- g38.achieve.all.for.equity[g38.achieve.all.for.equity$SUBJECT %in% c("Math", "Reading"),]


sample.stats <- unmatrixfy.df(aggregate(g38.achieve.all.for.equity$SCALE_SCORE,
                                               by=list(SCHOOL_YEAR=g38.achieve.all.for.equity$SCHOOL_YEAR,
                                                       SUBJECT=g38.achieve.all.for.equity$SUBJECT,
                                                       GRADE_ENROLLED=g38.achieve.all.for.equity$GRADE_ENROLLED),
                                               function (g) c(MEAN=mean(as.numeric(g), na.rm=TRUE),
                                                              SD=sd(as.numeric(g), na.rm=TRUE))),prepend=FALSE)

g38.achieve.all.for.equity$STD_SCORE <- apply(g38.achieve.all.for.equity[,c("SCHOOL_YEAR", "SUBJECT", "GRADE_ENROLLED", "SCALE_SCORE")],
                                            c(1),
                                            function (score) {
                                              mean <- sample.stats[sample.stats$SCHOOL_YEAR == score[["SCHOOL_YEAR"]] &
                                                                     sample.stats$SUBJECT == score[["SUBJECT"]] &
                                                                     sample.stats$GRADE_ENROLLED == score[["GRADE_ENROLLED"]], "MEAN"]
                                              
                                              sd <- sample.stats[sample.stats$SCHOOL_YEAR == score[["SCHOOL_YEAR"]] &
                                                                     sample.stats$SUBJECT == score[["SUBJECT"]] &
                                                                   sample.stats$GRADE_ENROLLED == score[["GRADE_ENROLLED"]], "SD"]
                                              
                                              z_score <- round((as.numeric(score[["SCALE_SCORE"]]) - mean)/sd, 2)
                                              
                                              z_score * 20 + 100
                                              
                                            })

unmatrixfy.df(aggregate(g38.achieve.all.for.equity$STD_SCORE,
                        by=list(SCHOOL_YEAR=g38.achieve.all.for.equity$SCHOOL_YEAR,
                                SUBJECT=g38.achieve.all.for.equity$SUBJECT,
                                GRADE_ENROLLED=g38.achieve.all.for.equity$GRADE_ENROLLED),
                        function (g) c(MEAN=mean(as.numeric(g), na.rm=TRUE),
                                       SD=sd(as.numeric(g), na.rm=TRUE))),prepend=FALSE)

achievement.prior.year <- paws[paws$SCHOOL_YEAR == prior.school.year &
                                 paws$SUBJECT_CODE %in% c("MA","RE") &
                                 paws$TEST_TYPE %in% c("PAWS-STANDARD", "PAWS Standard") &
                                 paws$TESTING_STATUS_CODE == 'T',c("WISER_ID", "SUBJECT_CODE", "STANDARD_PAWS_PERF_LEVEL")]

table(achievement.prior.year$SUBJECT_CODE, useNA="ifany")
achievement.prior.year$SUBJECT_CODE <- ifelse(achievement.prior.year$SUBJECT_CODE == 'MA', 'Math', 'Reading')
names(achievement.prior.year) <- c("WISER_ID", "SUBJECT", "PRIOR_PERFORMANCE_LEVEL")
table(achievement.prior.year$SUBJECT, useNA="ifany")

#nrow(g38.achieve)
achievement.for.equity <- merge(g38.achieve.all.for.equity[g38.achieve.all.for.equity$SCHOOL_YEAR==current.school.year,],
                                achievement.prior.year, all.x=TRUE)
#nrow(g38.achieve)

consolidated.subgroup.students <- data.frame(WISER_ID=unique(achievement.prior.year[achievement.prior.year$PRIOR_PERFORMANCE_LEVEL %in% c('1','2'), "WISER_ID"]),
                                             CONSOLIDATED_SUBGROUP='T')
                                                             
achievement.for.equity <- merge(achievement.for.equity, consolidated.subgroup.students)
achievement.for.equity$CONSOLIDATED_SUBGROUP <- ifelse(is.na(achievement.for.equity$CONSOLIDATED_SUBGROUP),
                                                       'F',
                                                       'T')
consolidated.subgroup.wide <- reshape(achievement.for.equity[achievement.for.equity$SUBJECT %in% c('Math', 'Reading') &
                                 achievement.for.equity$CONSOLIDATED_SUBGROUP == 'T',
                               c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "GRADE_ENROLLED", "TEST_TYPE", "SUBJECT", "PERFORMANCE_LEVEL", "PRIOR_PERFORMANCE_LEVEL", "TESTING_STATUS_CODE",
                                 "SCHOOL_FULL_ACADEMIC_YEAR", "SCALE_SCORE", "STD_SCORE")],
        v.names=c("PERFORMANCE_LEVEL", "PRIOR_PERFORMANCE_LEVEL", "TESTING_STATUS_CODE", "SCHOOL_FULL_ACADEMIC_YEAR", "SCALE_SCORE", "STD_SCORE"),
        timevar="SUBJECT",
        idvar="WISER_ID",
        direction="wide")

consolidated.subgroup.wide <- consolidated.subgroup.wide[c("SCHOOL_YEAR", "SCHOOL_ID",
                                "WISER_ID", "GRADE_ENROLLED", "TEST_TYPE",
                                "PRIOR_PERFORMANCE_LEVEL.Reading", "PRIOR_PERFORMANCE_LEVEL.Math",
                                "PERFORMANCE_LEVEL.Reading",  "PERFORMANCE_LEVEL.Math",  
                                "SCHOOL_FULL_ACADEMIC_YEAR.Reading", "SCHOOL_FULL_ACADEMIC_YEAR.Math",
                                "TESTING_STATUS_CODE.Reading", "TESTING_STATUS_CODE.Math",
                                "SCALE_SCORE.Reading", "SCALE_SCORE.Math",
                                "STD_SCORE.Reading", "STD_SCORE.Math")]

#write.csv(consolidated.subgroup.wide, file="results/statewide-g38-school-consolidated-subgroup-composition.csv", row.names=FALSE, quote=FALSE, na="")

# table(consolidated.subgroup.wide[c("PERFORMANCE_LEVEL.Math",
#                                    "PERFORMANCE_LEVEL.Reading")],
#       useNA="ifany")

consolidated.subgroup.wide$PROFICIENT_MATH <- ifelse(is.na(consolidated.subgroup.wide$PERFORMANCE_LEVEL.Math),
                                                     NA,
                                                     consolidated.subgroup.wide$PERFORMANCE_LEVEL.Math %in% c('3','4'))

consolidated.subgroup.wide$PROFICIENT_READING <- ifelse(is.na(consolidated.subgroup.wide$PERFORMANCE_LEVEL.Reading),
                                                        NA,
                                                        consolidated.subgroup.wide$PERFORMANCE_LEVEL.Reading %in% c('3','4'))

consolidated.subgroup.wide$PROFICIENT_MATH_PRIOR <- ifelse(is.na(consolidated.subgroup.wide$PRIOR_PERFORMANCE_LEVEL.Math),
                                                     NA,
                                                     consolidated.subgroup.wide$PRIOR_PERFORMANCE_LEVEL.Math %in% c('3','4'))

consolidated.subgroup.wide$PROFICIENT_READING_PRIOR <- ifelse(is.na(consolidated.subgroup.wide$PRIOR_PERFORMANCE_LEVEL.Reading),
                                                        NA,
                                                        consolidated.subgroup.wide$PRIOR_PERFORMANCE_LEVEL.Reading %in% c('3','4'))

#table(consolidated.subgroup.wide[c("PROFICIENT_MATH", "PROFICIENT_READING")], useNA="ifany")
table(consolidated.subgroup.wide[c("PROFICIENT_MATH_PRIOR", "PROFICIENT_READING_PRIOR")], useNA="ifany")
prop.table(table(consolidated.subgroup.wide[c("PROFICIENT_MATH_PRIOR", "PROFICIENT_READING_PRIOR")], useNA="ifany"))


paws.for.equity.lookback <- paws[paws$SCHOOL_YEAR >= '2011-12' &
                                   paws$SUBJECT_CODE %in% c("MA","RE") &
                                   paws$TEST_TYPE %in% c("PAWS-STANDARD", "PAWS Standard") &
                                   paws$TESTING_STATUS_CODE == 'T',]
  
consolidated.subgroup.students.lookback <- data.frame(unique(paws.for.equity.lookback[paws.for.equity.lookback$ACHIEVEMENT_LEVEL_PRIOR %in% c('1','2'), c("SCHOOL_YEAR", "WISER_ID")]),
                                             CONSOLIDATED_SUBGROUP='T')

table(consolidated.subgroup.students.lookback$SCHOOL_YEAR)


consolidated.subgroup.scores.lookback <- merge(g38.achieve.all.for.equity[g38.achieve.all.for.equity$SCHOOL_YEAR < current.school.year,], 
                                               consolidated.subgroup.students.lookback)

table(consolidated.subgroup.scores.lookback[c("CONSOLIDATED_SUBGROUP", "SCHOOL_YEAR")])
table(achievement.for.equity[c("CONSOLIDATED_SUBGROUP", "SCHOOL_YEAR")])


achievement.for.equity.all <- rbind(achievement.for.equity[,!(names(achievement.for.equity) %in% "PRIOR_PERFORMANCE_LEVEL")],
                                    consolidated.subgroup.scores.lookback)

table(achievement.for.equity.all$SCHOOL_YEAR)

save(achievement.for.equity.all, file="data/achievement-for-equity.Rdata")

