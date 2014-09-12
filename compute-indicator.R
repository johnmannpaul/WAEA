source("mean-z-score-fun.R")
source("participation-fun.R")
source("calc-small-schools-fun.R")

compute.indicator <- function (student.df,
                               schools.df,
                               school.types = HS.types,
                               indicator.label = "ACHIEVEMENT_HS",
                               subject.labels=c("MATH", "READING", "SCIENCE", "ENGLISH"),                                      
                               score.prefix='WDE_PERFORMANCE_LEVEL',
                               agg.fun=function (g) c(N_TESTS=length(g),
                                                      N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                      PERCENT_PROFICIENT=round((sum(ifelse(g %in% c('3','4'), 1, 0))/length(g))*100, 0)),
                               min.N=10,
                               already.long=FALSE,
                               status.prefix='TESTING_STATUS_CODE',                                
                               label.sep="_",
                               status.codes=c(exempt='X', participated='T', did.not.participate='N'),
                               school.fay.label="SCHOOL_FULL_ACADEMIC_YEAR",
                               precision=1,
                               school.key=c("SCHOOL_YEAR", "SCHOOL_ID"),
                               student.key="WISER_ID")   
{
  
  total.participation.labels = sapply(c("TESTS_ACTUAL_COUNT", 
                                        "TESTS_EXPECTED_COUNT", 
                                        "PARTICIPATION_RATE"),
                                      function (l)
                                        paste(indicator.label, l, sep=label.sep),
                                      USE.NAMES=FALSE)

  #count student participation by subject and total
  participation <- calc.participation.rate(student.df,
                                               subject.labels=subject.labels,                                      
                                               status.prefix=status.prefix, 
                                               status.prefix.sep=label.sep,
                                               status.codes=status.codes,
                                               total.participation.labels = total.participation.labels,
                                               precision=precision)
  

  #merge with schools
  participation <- merge(schools.df[schools.df$WAEA_SCHOOL_TYPE %in% school.types, 
                                 school.key],                           
                         participation,
                         all=TRUE)
  
  #zero out any NA counts for schools with no participants
  count.labels <- setdiff(names(participation)[grep("_RATE$", names(participation), invert=TRUE)], school.key)
  participation[,count.labels] <- zero.na.rows(participation,
                                               count.labels)
  
  
  #the indicator is only computed for FAY students
  student.df.fay <- student.df[student.df[[school.fay.label]] == 'T',]   
  
  #and ones that had some participation
  testing.status.labels <- sapply(subject.labels, function (s) paste(status.prefix, s, sep=label.sep))
  some.part <- apply(student.df.fay[testing.status.labels], c(1),
                     function (row) {
                       any(row == status.codes["participated"])
                     })
  
  student.df.fay <- student.df.fay[some.part,]
  

  #small school
  small.schools <- calc.small.schools(student.df.fay, schools.df, school.types, student.key, attribute=indicator.label)
  
  #label schools as small schools status along with their number of lookback years
  schools.df <- cbind(schools.df, small.schools$result.schools[,setdiff(names(small.schools$result.schools), names(schools.df))])  
  #add lookback records to the student data.frame
  student.df.fay$SCHOOL_YEAR_ORIGINAL <- student.df.fay$SCHOOL_YEAR
  student.df.fay <- rbind(student.df.fay, small.schools$result.students)
  

  
  indicator.fay.school <- unmatrixfy.df(calc.mean.score(student.df.fay, 
                                                        subject.labels=subject.labels,
                                                        testing.status.prefix= if (already.long) paste(status.prefix, indicator.label,sep=label.sep) else status.prefix,
                                                        score.prefix=score.prefix,
                                                        prefix.sep=label.sep,
                                                        agg.function=agg.fun,
                                                        already.long), prepend=FALSE)
  
  names(indicator.fay.school) <- sapply(names(indicator.fay.school), 
                                        function (n) if (n %in% school.key) n else paste(indicator.label, n,  sep=label.sep),
                                        USE.NAMES=FALSE)
  
  indicator.labels.min.N <- names(indicator.fay.school)[(ncol(indicator.fay.school)-1):ncol(indicator.fay.school)]
  names(indicator.labels.min.N) <- c("indicator.score","N")
  
  indicator.fay.school <- merge(indicator.fay.school, 
                                participation[,c(school.key, 
                                                     total.participation.labels)], all.y=TRUE)

  N.labels <- setdiff(names(indicator.fay.school)[grep("^N_", names(indicator.fay.school))], school.key)
  indicator.fay.school[,N.labels] <- zero.na.rows(indicator.fay.school,
                                               N.labels)

  schools.df <-bind.indicator(schools.df, 
                              indicator.fay.school,                               
                              indicator.labels.min.N = indicator.labels.min.N,
                              min.N)
  
  #return a list of the differnt data frames we computed
  list(schools=schools.df,
       students.fay=student.df.fay,
       indicator.school=indicator.fay.school,
       participation=participation)
  
}



compute.indicator.long <- function (student.df.participation,
                                    student.df.scores,
                                    schools.df,
                                    school.types = HS.types,
                                    indicator.label = "ACHIEVEMENT_HS",                                  
                                    score.prefix='WDE_PERFORMANCE_LEVEL',
                                    agg.fun=function (g) c(N_TESTS=length(g),
                                                      N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                      PERCENT_PROFICIENT=round((sum(ifelse(g %in% c('3','4'), 1, 0))/length(g))*100, 0)),
                                    min.N=10,
                                    status.prefix='TESTING_STATUS_CODE',                                
                                    label.sep="_",
                                    status.codes=c(exempt='X', participated='T', did.not.participate='N'),
                                    school.fay.label="SCHOOL_FULL_ACADEMIC_YEAR",
                                    precision=1,
                                    school.key=c("SCHOOL_YEAR", "SCHOOL_ID"),
                                    student.key="WISER_ID")   
{
  

  student.df.participation$SCHOOL_ID <- apply(student.df.participation[,c("SCHOOL_YEAR", "SCHOOL_ID")],
                                       c(1),
                                       function (school) {
                                         
                                         pairing <- school.pairing.lookup[[school[["SCHOOL_YEAR"]]]]
                                         paired.school <- pairing[[school[["SCHOOL_ID"]]]]
                                         ifelse(is.null(paired.school), 
                                                school[["SCHOOL_ID"]], 
                                                paired.school)
                                       })
  
  student.df.scores$SCHOOL_ID <- apply(student.df.scores[,c("SCHOOL_YEAR", "SCHOOL_ID")],
                                       c(1),
                                       function (school) {
                                         
                                         pairing <- school.pairing.lookup[[school[["SCHOOL_YEAR"]]]]
                                         paired.school <- pairing[[school[["SCHOOL_ID"]]]]
                                         ifelse(is.null(paired.school), 
                                                school[["SCHOOL_ID"]], 
                                                paired.school)
                                       })
  
  total.participation.labels = sapply(c("TESTS_ACTUAL_COUNT", 
                                        "TESTS_EXPECTED_COUNT", 
                                        "PARTICIPATION_RATE"),
                                      function (l)
                                        paste(indicator.label, l, sep=label.sep),
                                      USE.NAMES=FALSE)
  
  #count student participation by subject and total
  participation <- calc.participation.rate.long(student.df.participation,
                                           status.prefix=status.prefix, 
                                           status.codes=status.codes,
                                           total.participation.labels = total.participation.labels,
                                           precision=precision)
  
  
  #merge with schools
  participation <- merge(schools.df[schools.df$WAEA_SCHOOL_TYPE %in% school.types, 
                                    school.key],                           
                         participation,
                         all=TRUE)
  
  #zero out any NA counts for schools with no participants
  count.labels <- setdiff(names(participation)[grep("_RATE$", names(participation), invert=TRUE)], school.key)
  rate.labels <- setdiff(names(participation)[grep("_RATE$", names(participation))], school.key)
  participation[,c(count.labels, rate.labels)] <- zero.na.rows(participation,
                                               count.labels,
                                               rate.labels)
  
  
  #the indicator is only computed for FAY students
  student.df.fay <- student.df.scores[student.df.scores[[school.fay.label]] == 'T',]   
  
  #and ones that had some participation  
  student.df.fay <- student.df.fay[student.df.fay[[status.prefix]] == status.codes["participated"],]
  
  
  #small school
  small.schools <- calc.small.schools(student.df.fay, schools.df, school.types, student.key, attribute=indicator.label)
  
  #label schools as small schools status along with their number of lookback years
  schools.df <- cbind(schools.df, small.schools$result.schools[,setdiff(names(small.schools$result.schools), names(schools.df))])  
  #add lookback records to the student data.frame
  student.df.fay$SCHOOL_YEAR_ORIGINAL <- student.df.fay$SCHOOL_YEAR
  student.df.fay <- rbind(student.df.fay, small.schools$result.students)
  
  
  
  indicator.fay.school <- unmatrixfy.df(calc.mean.score(student.df.fay, 
                                                        testing.status.prefix = status.prefix,
                                                        score.prefix=score.prefix,
                                                        prefix.sep=label.sep,
                                                        agg.function=agg.fun,
                                                        already.long=TRUE), prepend=FALSE)
  
  names(indicator.fay.school) <- sapply(names(indicator.fay.school), 
                                        function (n) if (n %in% school.key) n else paste(indicator.label, n, sep=label.sep),
                                        USE.NAMES=FALSE)
  
  indicator.labels.min.N <- names(indicator.fay.school)[(ncol(indicator.fay.school)-1):ncol(indicator.fay.school)]
  names(indicator.labels.min.N) <- c("indicator.score","N")
  
  indicator.fay.school <- merge(indicator.fay.school, 
                                participation[,c(school.key, 
                                                 total.participation.labels)], all.y=TRUE)
  
  N.labels <- setdiff(names(indicator.fay.school)[grep(paste(indicator.label, "N", sep="_"), 
                                                       names(indicator.fay.school),
                                                       fixed=TRUE)], school.key)
  indicator.fay.school[,N.labels] <- zero.na.rows(indicator.fay.school,
                                                  N.labels)
  
  schools.df <-bind.indicator(schools.df, 
                              indicator.fay.school,                               
                              indicator.labels.min.N = indicator.labels.min.N,
                              min.N)
  
  #return a list of the differnt data frames we computed
  list(schools=schools.df,
       students.fay=student.df.fay,
       indicator.school=indicator.fay.school,
       participation=participation)
  
}
