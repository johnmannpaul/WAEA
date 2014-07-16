source("participation-fun.R")
source("mean-z-score-fun.R")

#paws.df is long format data: each row is a test event
below.proficient.priors <- paws.df[paws.df$SUBJECT_CODE %in% c("MA","RE") &
                                     paws.df$TEST_TYPE %in% c("PAWS-STANDARD", "PAWS Standard") &
                                     paws.df$ACHIEVEMENT_LEVEL_PRIOR %in% c("1","2"),]

table(below.proficient.priors$GRADE_ENROLLED, useNA="ifany")

subgroup.students <- unique(below.proficient.priors[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])

#scores for all the students in the subgroup
consolidated.subgroup.df <- merge(subgroup.students, paws.df[paws.df$SUBJECT_CODE %in% c("MA","RE") &
                                                               paws.df$TEST_TYPE %in% c("PAWS-STANDARD", "PAWS Standard"),])

##calculate participation rate for the subgroup
subgroup.testing.status.wide <- reshape(consolidated.subgroup.df[,c("SCHOOL_YEAR", 
                                         "SCHOOL_ID", 
                                         "WISER_ID",
                                         "SUBJECT_CODE",
                                         "TESTING_STATUS_CODE")],
        v.names=c("TESTING_STATUS_CODE"),
        idvar=c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID"),
        timevar = "SUBJECT_CODE",
        times = c("MA", "RE"),
        direction="wide")

subgroup.particpation.rate <- calc.participation.rate(subgroup.testing.status.wide, 
                                     subject.labels=c("MA", "RE"),                                      
                                     status.prefix='TESTING_STATUS_CODE', 
                                     status.prefix.sep=".",
                                     status.codes=c(exempt='X', participated='T', did.not.participate='F'),
                                     total.participation.labels = c("EQUITY_TESTS_EXPECTED_COUNT", "EQUITY_TESTS_ACTUAL_COUNT", "EQUITY_PARTICIPATION_RATE"),
                                     precision=1)

#small schools lookback
consolidated.subgroup.df <- with(consolidated.subgroup.df,
                                 consolidated.subgroup.df[SCHOOL_FULL_ACADEMIC_YEAR=='T' &
                                                            TESTING_STATUS_CODE=='T',])
schools.subgroup.N <- with(consolidated.subgroup.df, cast(consolidated.subgroup.df[, c("SCHOOL_YEAR", 
                                                                                       "SCHOOL_ID",   
                                                                                       "WISER_ID")], SCHOOL_YEAR+SCHOOL_ID~., 
                                                          function (x) length(unique(x))))

names(schools.subgroup.N)[length(schools.subgroup.N)] <- "N_SUBGROUP"


#merge schools with no scores
schools.subgroup.N.0 <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                              consolidated.subgroup.df[, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                              all.x=TRUE)

schools.subgroup.N.0 <- schools.subgroup.N.0[is.na(schools.subgroup.N.0$WISER_ID) & schools.subgroup.N.0$SCHOOL_ID != state.school.id,]

schools.subgroup.N <- rbind(schools.subgroup.N, data.frame(t(apply(schools.subgroup.N.0,
                                                                   c(1),
                                                                   function (school){
                                                                     
                                                                     c(SCHOOL_YEAR = school[["SCHOOL_YEAR"]], SCHOOL_ID=school[["SCHOOL_ID"]], N_SUBGROUP=0)
                                                                   }))))


tail(schools.subgroup.N, 30)

schools.subgroup.N$N_SUBGROUP <- as.numeric(schools.subgroup.N$N_SUBGROUP)
small.schools.subgroup <- data.frame(with(schools.subgroup.N, schools.subgroup.N[N_SUBGROUP < min.N.subgroup,]))

small.subgroup <- merge(cast(schools.subgroup.N[,c("SCHOOL_YEAR","SCHOOL_ID","N_SUBGROUP")], SCHOOL_ID~SCHOOL_YEAR, fill=0),
                        data.frame(SCHOOL_ID = unique(small.schools.subgroup[,c("SCHOOL_ID")])))

go.back.subgroup <- cbind(SCHOOL_ID = small.subgroup[,"SCHOOL_ID"], as.data.frame(t(apply(small.subgroup, c(1),
                                                                                          FUN=function (school) {
                                                                                            compute.N.years(school, min.N.subgroup.multiyear)          
                                                                                          }
))))




small.schools.subgroup$YEARS_BACK <- apply(small.schools.subgroup, c(1),
                                           FUN=function (school) {
                                             
                                             id <- school[["SCHOOL_ID"]]
                                             year <- school[["SCHOOL_YEAR"]]
                                             
                                             go.back.subgroup[go.back.subgroup$SCHOOL_ID==id,][[year]] 
                                             
                                           })


#look at the current year's batch
small.schools.subgroup[small.schools.subgroup$SCHOOL_YEAR==current.school.year,]

#this is how many we can rate
table(small.schools.subgroup[small.schools.subgroup$SCHOOL_YEAR==current.school.year,]$YEARS_BACK)



small.schools.fix.subgroup <- small.schools.subgroup[small.schools.subgroup$YEARS_BACK < Inf,]

small.schools.fix.subgroup$YEAR <- as.numeric(sapply(small.schools.fix.subgroup$SCHOOL_YEAR, function (y) {
  strsplit(y,'-')[[1]][1]
})) + 1

small.schools.subgroup.additional.consolidated.subgroup.df <- do.call(rbind, apply(small.schools.fix.subgroup[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK")], c(1),
                                                                                   FUN=function (school) get.paws.years.back(school, consolidated.subgroup.df)))

consolidated.subgroup.df <- rbind(consolidated.subgroup.df, small.schools.subgroup.additional.consolidated.subgroup.df)


schools <- schools[, !(names(schools) %in% small.school.labels.equity)]


schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                        "SCHOOL_YEAR")], c(1),                                                                                      
                                             FUN=function (school) {
                                               small.school <- with(small.schools.subgroup, 
                                                                    small.schools.subgroup[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                             SCHOOL_YEAR == school[["SCHOOL_YEAR"]],])
                                               if (nrow(small.school) == 0)
                                                 result <- c('F', NA)
                                               else 
                                                 result <- c('T', small.school[,"YEARS_BACK"]) 
                                               names(result) <- small.school.labels.equity
                                               result
                                             }))))



schools$YEARS_BACK_EQUITY <- as.numeric(schools$YEARS_BACK_EQUITY)


##calculate mean z-score for the consolidated subgroup
subgroup.testing.scores.wide <- reshape(consolidated.subgroup.df[,c("SCHOOL_YEAR", 
                                                                    "SCHOOL_ID", 
                                                                    "WISER_ID",
                                                                    "SUBJECT_CODE",
                                                                    "TESTING_STATUS_CODE",
                                                                    "PAWS_Z_SCORE")],
                                        v.names=c("PAWS_Z_SCORE", "TESTING_STATUS_CODE"),
                                        idvar=c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID"),
                                        timevar = "SUBJECT_CODE",
                                        times = c("MA", "RE"),
                                        direction="wide",
                                        sep="_")


subgroup.mean.z.scores <- calc.mean.score(subgroup.testing.scores.wide, subject.labels=c(MATH="MA", READING="RE"),
          testing.status.prefix="TESTING_STATUS_CODE",
          z.score.prefix="PAWS_Z_SCORE",
          prefix="_",
          agg.function=function (g) round(100*mean(g),0)) 
subgroup.mean.z.scores[is.na(subgroup.mean.z.scores$PAWS_Z_SCORE),]
tail(subgroup.mean.z.scores)

names(subgroup.mean.z.scores) <- c("SCHOOL_YEAR","SCHOOL_ID","MEAN_PAWS_Z_SCORE", "N_SUBGROUP")

write.csv(subgroup.mean.z.scores, file="results/elementary-middle-school-subgroup-mean-z-scores.csv", row.names=FALSE, na="")
