load("data/act-standardized.Rdata")


#rename the data frame
act <- act.standardized



#temporarily make a copy of 2012-13 and call it 2013-14
act <- rbind(data.frame(WISER_ID=act$WISER_ID,
                        SCHOOL_YEAR=rep('2013-14', nrow(act)),
                        act[,!(names(act) %in% c("WISER_ID", "SCHOOL_YEAR"))]),
             act)


act$YEAR <- as.numeric(sapply(act$SCHOOL_YEAR, function (y) {
  strsplit(y,'-')[[1]][1]
})) + 1




act$ANY_TESTED <- apply(act, 
                           c(1),
                           function (student) {
                             if (any(student[c("TESTING_STATUS_CODE_MATH", "TESTING_STATUS_CODE_READING", "TESTING_STATUS_CODE_SCIENCE", "TESTING_STATUS_CODE_ENG_WRITING")] == "T"))
                               1
                             else
                               0
                           })


act.fay <- act[act$SCHOOL_FULL_ACADEMIC_YEAR=="T",c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "WISER_ID", "TESTING_STATUS_CODE_MATH", 
                                                   "TESTING_STATUS_CODE_READING", "TESTING_STATUS_CODE_SCIENCE", "TESTING_STATUS_CODE_ENG_WRITING",
                                                   act.accountability.z.score.labels, "ANY_TESTED")]






##merge small schools

##determine small schools by looking at the number of unique students who took tested in one of the tested areas at each school
schools.achievement.hs.N <- with(act.fay, cast(merge(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                                                  act.fay[ANY_TESTED==1, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")]), SCHOOL_YEAR+SCHOOL_ID~., 
                                            function (x) length(unique(x))))
names(schools.achievement.hs.N)[length(schools.achievement.hs.N)] <- "N_ACHIEVEMENT_HS"

schools.N.0.hs <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                     act.fay[, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                     all.x=TRUE)

schools.N.0.hs <- schools.N.0.hs[is.na(schools.N.0.hs$WISER_ID) & schools.N.0.hs$SCHOOL_ID != state.school.id,]


schools.N.hs <- as.data.frame(rbind(schools.achievement.hs.N, data.frame(t(apply(schools.N.0.hs,
                                                                           c(1),
                                                                           function (school){
                                                                             
                                                                             c(SCHOOL_YEAR = school[["SCHOOL_YEAR"]], SCHOOL_ID=school[["SCHOOL_ID"]], N_ACHIEVEMENT_HS=0)
                                                                           })))))


schools.N.hs$N_ACHIEVEMENT_HS <- as.numeric(schools.N.hs$N_ACHIEVEMENT_HS)



#small hs school is one where N_ACHIEVEMENT is less than the minimum
small.schools.hs <- as.data.frame(with(schools.N.hs, schools.N.hs[N_ACHIEVEMENT_HS < min.N.achievement.hs,]))

table(small.schools.hs$SCHOOL_YEAR)


small.achievement.hs <- merge(cast(schools.N.hs[,c("SCHOOL_YEAR","SCHOOL_ID","N_ACHIEVEMENT_HS")], SCHOOL_ID~SCHOOL_YEAR, fill=0),
                           data.frame(SCHOOL_ID = unique(small.schools.hs[,c("SCHOOL_ID")])))




go.back.achievement.hs <- cbind(SCHOOL_ID = small.achievement.hs[,"SCHOOL_ID"], as.data.frame(t(apply(small.achievement.hs, c(1),
                                                                                                FUN=function (school) {
                                                                                                  compute.N.years(school, min.N.achievement.hs.multiyear)          
                                                                                                }
))))



compute.years.back.achievement.hs <- function (school) {
  
  id <- school[["SCHOOL_ID"]]
  year <- school[["SCHOOL_YEAR"]]
  
  years.back.achievement <- go.back.achievement.hs[go.back.achievement.hs$SCHOOL_ID==id,][[year]] 
  
  years.back.achievement
  
}

small.schools.hs$YEARS_BACK <- apply(small.schools.hs, c(1),
                                              FUN=compute.years.back.achievement.hs)

#look at the current year's batch
small.schools.hs[small.schools.hs$SCHOOL_YEAR==current.school.year,]


schools <- schools[, !(names(schools) %in% small.school.hs.labels)]



schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                        "SCHOOL_YEAR",                                                     
                                                        "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                             FUN=function (school) {
                                               small.school <- with(small.schools.hs, 
                                                                    small.schools.hs[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                       SCHOOL_YEAR == school[["SCHOOL_YEAR"]],])
                                               if (nrow(small.school) == 0)
                                                 result <- c('F', NA)
                                               else 
                                                 result <- c('T', small.school[,"YEARS_BACK"])
                                               names(result) <- small.school.hs.labels
                                               result
                                             }))))


schools$YEARS_BACK_HS <- as.numeric(schools$YEARS_BACK_HS)

schools[schools$SMALL_SCHOOL_HS=='T' & schools$SCHOOL_YEAR==current.school.year,]


small.schools.achievement.fix.hs <- schools[schools$SMALL_SCHOOL_HS=='T' & schools$YEARS_BACK_HS < Inf,]



small.schools.hs.additional.act.fay <- do.call(rbind, apply(small.schools.achievement.fix.hs[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK_HS")], c(1),
                                                                     FUN=function (school) get.paws.years.back(school,act.fay, "YEARS_BACK_HS")))

act.fay$SCHOOL_YEAR_ORIGINAL <- act.fay$SCHOOL_YEAR

act.fay <- rbind(act.fay, small.schools.hs.additional.act.fay)

nrow(with(act.fay, act.fay[SCHOOL_YEAR != SCHOOL_YEAR_ORIGINAL,]))
