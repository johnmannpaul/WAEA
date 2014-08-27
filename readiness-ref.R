load(file="data/ACT/explore.readiness.Rdata")
load(file="data/ACT/plan.readiness.Rdata")
load(file="data/ACT/act.readiness.Rdata")

readiness.all.df <- rbind(act.readiness, plan.readiness, explore.readiness)

#remove non-high schools (e.g. schools that serve grade 9, but do not award diplomas)

nrow(readiness.all.df)
readiness.all.df <- merge(readiness.all.df,schools[schools$WAEA_SCHOOL_TYPE %in% HS.types, c("SCHOOL_YEAR", "SCHOOL_ID")])
nrow(readiness.all.df)

table(unique(readiness.all.df[,c("SCHOOL_YEAR", "SCHOOL_ID")])$SCHOOL_YEAR)

head(readiness.all.df[!(readiness.all.df$TEST_TYPE %in% readiness.standard.test.types),])
#calculating participation rate for 'all students'.  Do we need to do it for the consolildated subgroup as well?
readiness.participation <- calc.participation.rate(readiness.all.df, 
                                                   subject.labels=c("COMPOSITE", "MATH", "READING", "SCIENCE", "WRITING"), 
                                                   status.prefix='TESTING_STATUS_CODE',
                                                   status.codes=c(exempt='X', participated='T', did.not.participate='N'),
                                                   total.participation.labels = c("TESTED_READINESS_TESTS_ACTUAL_COUNT", 
                                                                                  "TESTED_READINESS_TESTS_EXPECTED_COUNT", 
                                                                                  "PARTICIPATION_RATE_TESTED_READINESS_HS"))
##check and compare the participation rates
table(findInterval(readiness.participation[readiness.participation$SCHOOL_YEAR==prior.school.year,"PARTICIPATION_RATE_TESTED_READINESS_HS"], c(90, 95)))
table(findInterval(readiness.participation[readiness.participation$SCHOOL_YEAR==current.school.year,"PARTICIPATION_RATE_TESTED_READINESS_HS"], c(90, 95)))

table(findInterval(readiness.participation[readiness.participation$SCHOOL_YEAR==prior.school.year &
                                             readiness.participation$TESTED_READINESS_PARTICIPANTS_HS>9,"PARTICIPATION_RATE_TESTED_READINESS_HS"], c(90, 95)))
table(findInterval(readiness.participation[readiness.participation$SCHOOL_YEAR==current.school.year&
                                             readiness.participation$TESTED_READINESS_PARTICIPANTS_HS>9,"PARTICIPATION_RATE_TESTED_READINESS_HS"], c(90, 95)))

table(readiness.participation[readiness.participation$SCHOOL_YEAR==current.school.year,"PARTICIPATION_RATE_TESTED_READINESS_HS"])
table(readiness.participation[readiness.participation$SCHOOL_YEAR==prior.school.year,"PARTICIPATION_RATE_TESTED_READINESS_HS"])

readiness.participation[readiness.participation$SCHOOL_YEAR==current.school.year &
                        readiness.participation$PARTICIPATION_RATE_TESTED_READINESS_HS<60,]

schools[schools$SCHOOL_YEAR==current.school.year & 
          schools$SCHOOL_ID %in% 
          readiness.participation[readiness.participation$SCHOOL_YEAR==current.school.year &
                                  readiness.participation$PARTICIPATION_RATE_TESTED_READINESS_HS<60,]$SCHOOL_ID,]
##end checks


readiness.alternate.labels <- c("SOME_SUBJECTS_TESTED", "TESTED_READINESS_ALTERNATE_SCORE")

##compute the alt-score while we're in there doing the status
compute.alt.status <- function (student, standard.test.types, result.labels, index.intervals) {
  if(student[["TEST_TYPE"]] %in% standard.test.types) {
    result <- c('N', NA)
    names(result) <- result.labels
    return(result)
  }
  
  #Is there an ALT writing test?  
  statuses <- student[c("TESTING_STATUS_CODE_MATH",
                        "TESTING_STATUS_CODE_READING",
                        "TESTING_STATUS_CODE_SCIENCE")]                                                                                                                                                               
  
  status <- if (!any(statuses =='T'))
    'N'
  else 
    'T'
  
  outcomes <- student[c("WDE_PERFORMANCE_LEVEL_MATH",
                        "WDE_PERFORMANCE_LEVEL_READING",
                        "WDE_PERFORMANCE_LEVEL_SCIENCE")]
  
  tests.taken <- outcomes[which(statuses == 'T')]
  
  
  proficiencies <- sum(unlist(lapply(tests.taken, 
                                     function (level) {
                                       if (level %in% c('3','4'))
                                         1
                                       else
                                         0                                                    
                                     })))
  
  
  proficiency <- if (status == 'T')
    findInterval(proficiencies/length(tests.taken), index.intervals)
  else
    NA
  
  result <- c(status, proficiency)
  names(result) <- result.labels
  result
  
}


readiness.all.df[,readiness.alternate.labels] <- data.frame(t(apply(readiness.all.df[,c("TESTING_STATUS_CODE_MATH",
                                                                                   "TESTING_STATUS_CODE_READING",
                                                                                   "TESTING_STATUS_CODE_SCIENCE",
                                                                                   "TEST_TYPE",
                                                                                   "WDE_PERFORMANCE_LEVEL_MATH",
                                                                                   "WDE_PERFORMANCE_LEVEL_READING",
                                                                                   "WDE_PERFORMANCE_LEVEL_SCIENCE")],
                                                               c(1),
                                                               compute.alt.status, readiness.standard.test.types, readiness.alternate.labels, alt.index.intervals)))
##checks
head(readiness.all.df[!(readiness.all.df$TEST_TYPE %in% readiness.standard.test.types),c("TESTING_STATUS_CODE_MATH",
                                                                                         "TESTING_STATUS_CODE_READING",
                                                                                         "TESTING_STATUS_CODE_SCIENCE",
                                                                                         "WDE_PERFORMANCE_LEVEL_MATH",
                                                                                         "WDE_PERFORMANCE_LEVEL_READING",
                                                                                         "WDE_PERFORMANCE_LEVEL_SCIENCE",
                                                                                         "SOME_SUBJECTS_TESTED",
                                                                                         "TESTED_READINESS_ALTERNATE_SCORE")], 100)
head(readiness.all.df[(readiness.all.df$TEST_TYPE %in% readiness.standard.test.types),])
##end checks

readiness.all.df$TESTING_STATUS_CODE_READINESS <- ifelse(readiness.all.df$TEST_TYPE %in% readiness.standard.test.types,
                                                         readiness.all.df$TESTING_STATUS_CODE_COMPOSITE,
                                                         readiness.all.df$SOME_SUBJECTS_TESTED)



#from here on we're only interested in students who took the test
table(readiness.all.df$TESTING_STATUS_CODE_READINESS, useNA="ifany")
readiness.df <- readiness.all.df[readiness.all.df$TESTING_STATUS_CODE_READINESS=='T',]
table(readiness.df$TESTING_STATUS_CODE_READINESS, useNA="ifany")



#from here on we're only insterested in full academic year
table(readiness.df$SCHOOL_FULL_ACADEMIC_YEAR, useNA="ifany")
readiness.df.fay <- readiness.df[readiness.df$SCHOOL_FULL_ACADEMIC_YEAR=='T',]
table(readiness.df.fay$SCHOOL_FULL_ACADEMIC_YEAR, useNA="ifany")


readiness.df.fay$TESTED_READINESS_STANDARD_SCORE <- ifelse(readiness.df.fay$TEST_TYPE %in% readiness.standard.test.types, 
                                                           as.numeric(readiness.df.fay$SCALE_SCORE_COMPOSITE),
                                                           NA)


head(readiness.df.fay[!(readiness.df.fay$TEST_TYPE %in% readiness.standard.test.types),], 20)


readiness.df.fay$TESTED_READINESS_ACCOUNTABILITY_SCORE <- ifelse(readiness.df.fay$TEST_TYPE %in% readiness.standard.test.types,
                                                                 readiness.df.fay$TESTED_READINESS_STANDARD_SCORE,
                                                                 readiness.df.fay$TESTED_READINESS_ALTERNATE_SCORE)

table(readiness.df.fay[,c("SCHOOL_YEAR","TEST_TYPE")], useNA="ifany")
table(readiness.df.fay$TESTED_READINESS_ACCOUNTABILITY_SCORE, useNA="ifany")

#some scores are not numeric: composite score is -- or **...there's not much we can do about this but get rid of them or give them 20
readiness.df.fay[is.na(readiness.df.fay$TESTED_READINESS_ACCOUNTABILITY_SCORE),]
# write.csv(file="results/students-without-numeric-ACT-composite-scores.csv",
#           readiness.df.fay[is.na(readiness.df.fay$TESTED_READINESS_ACCOUNTABILITY_SCORE),c("SCHOOL_YEAR",
#                                                                                            "SCHOOL_ID",
#                                                                                            "WISER_ID", 
#                                                                                            "GRADE_ENROLLED",
#                                                                                            "SCHOOL_FULL_ACADEMIC_YEAR",
#                                                                                            "TEST_TYPE",
#                                                                                            "TESTING_STATUS_CODE_COMPOSITE", "SCALE_SCORE_COMPOSITE")], row.names=FALSE, na="")
readiness.df.fay <- readiness.df.fay[!is.na(readiness.df.fay$TESTED_READINESS_ACCOUNTABILITY_SCORE),]

table(readiness.df.fay$TEST_TYPE, useNA="ifany")
table(readiness.df.fay$TESTED_READINESS_ACCOUNTABILITY_SCORE, useNA="ifany")

#do the small school adjustment
small.readiness <- calc.small.schools(readiness.df.fay, schools, HS.types, "WISER_ID", attribute="TESTED_READINESS_HS")
schools <- schools[,!(names(schools) %in% setdiff(names(small.readiness$result.schools), names(schools)))]
schools <- cbind(schools, small.readiness$result.schools[,setdiff(names(small.readiness$result.schools), names(schools))])
readiness.df.fay$SCHOOL_YEAR_ORIGINAL <- readiness.df.fay$SCHOOL_YEAR
readiness.df.fay <- rbind(readiness.df.fay, small.readiness$result.students)

schools[!is.na(schools$YEARS_BACK_TESTED_READINESS_HS) & schools$YEARS_BACK_TESTED_READINESS_HS < Inf,]
tail(readiness.df.fay)

readiness.df.fay$TESTED_READINESS_INDEX_SCORE <- apply(readiness.df.fay[,c("TEST_TYPE",
                                                                           "TESTED_READINESS_ACCOUNTABILITY_SCORE")],
                                                       c(1),
                                                       function (student, lookup) {
                                                         tt <- student[["TEST_TYPE"]]
                                                         tt <- if (tt %in% readiness.standard.test.types) tt else 'ALT'
                                                         lookup.value <- as.character(as.numeric(student[["TESTED_READINESS_ACCOUNTABILITY_SCORE"]]))
                                                         
                                                         lookup[[tt]][lookup.value]
                                                         
                                                       }, readiness.indeces)


head(readiness.df.fay[!(readiness.df.fay$TEST_TYPE %in% readiness.standard.test.types),], 20)
table(readiness.df.fay[,c("TEST_TYPE","TESTED_READINESS_INDEX_SCORE")], useNA="ifany")




readiness.df.schools <- unmatrixfy.df(aggregate(data.frame(TESTED_READINESS_INDEX = readiness.df.fay$TESTED_READINESS_INDEX_SCORE),
                                  by=list(SCHOOL_YEAR = readiness.df.fay$SCHOOL_YEAR,
                                          SCHOOL_ID = readiness.df.fay$SCHOOL_ID),
                                  function (rows) {        
                                    c(N=length(which(!is.na(as.numeric(rows)))), 
                                      MEAN=round(mean(as.numeric(rows), na.rm=TRUE), precision.readiness))
                                  }))

readiness.df.schools <- merge(readiness.df.schools, readiness.participation[,c("SCHOOL_YEAR",
                                                                               "SCHOOL_ID",
                                                                               "TESTED_READINESS_TESTS_ACTUAL_COUNT", 
                                                                               "TESTED_READINESS_TESTS_EXPECTED_COUNT", 
                                                                               "PARTICIPATION_RATE_TESTED_READINESS_HS")])



schools <- bind.indicator(schools, 
                      readiness.df.schools,
                      indicator.labels.min.N = c(N= "TESTED_READINESS_INDEX_N", score="TESTED_READINESS_INDEX_MEAN"),
                      min.N.tested.readiness) 

head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types &
               schools$SCHOOL_YEAR=='2013-14',], 5)

head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types &
               schools$SCHOOL_YEAR=='2013-14' &
               schools$SMALL_SCHOOL_TESTED_READINESS_HS == 'T',], 5)
  
