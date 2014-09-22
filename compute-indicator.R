source("mean-z-score-fun.R")
source("participation-fun.R")
source("calc-small-schools-fun.R")



compute.indicator.school <- function (participation,
                                      student.df,
                                      status.prefix,
                                      score.prefix,                                      
                                      label.sep,
                                      agg.fun,
                                      indicator.label,
                                      school.key=c("SCHOOL_YEAR", "SCHOOL_ID")) {
  
  
  
  indicator.school <- unmatrixfy.df(calc.mean.score(student.df, 
                                                        testing.status.prefix = status.prefix,
                                                        score.prefix=score.prefix,
                                                        prefix.sep=label.sep,
                                                        agg.function=agg.fun,
                                                        already.long=TRUE), prepend=FALSE)
  
  names(indicator.school) <- sapply(names(indicator.school), 
                                        function (n) if (n %in% school.key) n else paste(indicator.label, n, sep=label.sep),
                                        USE.NAMES=FALSE)
    
  indicator.school <- merge(indicator.school, 
                                participation, all.y=TRUE)
  
  N.labels <- setdiff(names(indicator.school)[grep(paste(indicator.label, "N", sep="_"), 
                                                       names(indicator.school),
                                                       fixed=TRUE)], school.key)
  indicator.school[,N.labels] <- zero.na.rows(indicator.school,
                                                  N.labels)
  
  indicator.school
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
  
  #we can limit the participation data.frame to the schools of interest right here
  student.df.participation <- merge(student.df.participation, schools[school.key])  
  student.df.participation$SCHOOL_ID <- apply(student.df.participation[,c("SCHOOL_YEAR", "SCHOOL_ID")],
                                       c(1),
                                       function (school) {
                                         
                                         pairing <- school.pairing.lookup[[school[["SCHOOL_YEAR"]]]]
                                         paired.school <- pairing[[school[["SCHOOL_ID"]]]]
                                         ifelse(is.null(paired.school), 
                                                school[["SCHOOL_ID"]], 
                                                paired.school)
                                       })
  #we can't do the same for scores because of lookbacks limit the participation data.frame to the schools of interest right here
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
  #limit to schools and years of interest  
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
  
  #now we can filter student score records to years of interest along with those lookback records we calculated
  student.df.fay <- rbind(merge(student.df.fay, schools[school.key]), small.schools$result.students)
  
  
  indicator.fay.school <-  compute.indicator.school(participation[,c(school.key, 
                                                                     total.participation.labels)],
                                                    student.df.fay,
                                                    status.prefix,
                                                    score.prefix,                                      
                                                    label.sep,
                                                    agg.fun,
                                                    indicator.label)
  
  indicator.labels.min.N <- names(indicator.fay.school)[(ncol(indicator.fay.school)-length(total.participation.labels)-1):
                                                          (ncol(indicator.fay.school)-length(total.participation.labels))]
  names(indicator.labels.min.N) <- c("indicator.score","N")
  
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
