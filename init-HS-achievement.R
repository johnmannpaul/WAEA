load("data/ACT/act.Rdata")

##correction for too liberal cuts for proficiency on ACT reading a math
nrow(act[act$ACT_SCALE_SCORE_READING == '15' & act$WDE_PERFORMANCE_LEVEL_READING == '3',])
act$WDE_PERFORMANCE_LEVEL_READING <- ifelse(act$ACT_SCALE_SCORE_READING == '15', '2', act$WDE_PERFORMANCE_LEVEL_READING)
nrow(act[act$ACT_SCALE_SCORE_READING == '15' & act$WDE_PERFORMANCE_LEVEL_READING == '3',])

nrow(act[act$ACT_SCALE_SCORE_MATH == '16' & act$WDE_PERFORMANCE_LEVEL_MATH == '3',])
act$WDE_PERFORMANCE_LEVEL_MATH <- ifelse(act$ACT_SCALE_SCORE_MATH == '16', '2', act$WDE_PERFORMANCE_LEVEL_MATH)
nrow(act[act$ACT_SCALE_SCORE_MATH == '16' & act$WDE_PERFORMANCE_LEVEL_MATH == '3',])

act.df <- act[act$SCHOOL_FULL_ACADEMIC_YEAR=="T",c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "TESTING_STATUS_CODE_MATH", 
                                                   "TESTING_STATUS_CODE_READING", "TESTING_STATUS_CODE_SCIENCE",
                                                   "WDE_PERFORMANCE_LEVEL_MATH","WDE_PERFORMANCE_LEVEL_READING", "WDE_PERFORMANCE_LEVEL_SCIENCE")]



act.df$ANY_TESTED <- apply(act.df, 
                           c(1),
                           function (student) {
                             if (any(student[c("TESTING_STATUS_CODE_MATH", "TESTING_STATUS_CODE_READING", "TESTING_STATUS_CODE_SCIENCE")] == "T"))
                               1
                             else
                               0
                           })

##merge small schools

##determine small schools by looking at the number of unique students who took tested in one of the tested areas at each school
schools.achievement.hs.N <- with(act.df, cast(merge(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                                                  act.df[ANY_TESTED==1, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")]), SCHOOL_YEAR+SCHOOL_ID~., 
                                            function (x) length(unique(x))))

names(schools.achievement.hs.N)[length(schools.achievement.hs.N)] <- "N_ACHIEVEMENT_HS"


#small hs school is one where N_ACHIEVEMENT is less than the minimum
small.schools.hs <- as.data.frame(with(schools.achievement.hs.N, schools.achievement.hs.N[N_ACHIEVEMENT_HS < min.N.achievement.hs,]))


#save(paws_11_achievement, file="data/paws_11_achievement.Rdata")
load(file="data/paws_11_achievement.Rdata")

#just keep the fields we need
paws_11.lookback <-with(paws_11_achievement, paws_11_achievement[SCHOOL_FULL_ACADEMIC_YEAR=='T',c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "MA_TESTING_STATUS_CODE", "RE_TESTING_STATUS_CODE", "SC_TESTING_STATUS_CODE",
                                           "MA_ACCOUNTABILITY_PERF_LEVEL", "RE_ACCOUNTABILITY_PERF_LEVEL", "SC_ACCOUNTABILITY_PERF_LEVEL")])

#name in accordance with act achievement frame
names(paws_11.lookback) <- c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "TESTING_STATUS_CODE_MATH", 
                      "TESTING_STATUS_CODE_READING", "TESTING_STATUS_CODE_SCIENCE",
                      "WDE_PERFORMANCE_LEVEL_MATH","WDE_PERFORMANCE_LEVEL_READING", "WDE_PERFORMANCE_LEVEL_SCIENCE")


paws_11.lookback$ANY_TESTED <- apply(paws_11.lookback, 
                           c(1),
                           function (student) {
                             if (any(student[c("TESTING_STATUS_CODE_MATH", "TESTING_STATUS_CODE_READING", "TESTING_STATUS_CODE_SCIENCE")] == "T"))
                               1
                             else
                               0                             
                           })


#combine the paws and act scores for the small schools 
small.achievement.scores <- rbind(merge(data.frame(SCHOOL_ID=small.schools.hs[,c("SCHOOL_ID")]), act.df),
                                  merge(data.frame(SCHOOL_ID=small.schools.hs[,c("SCHOOL_ID")]), paws_11.lookback))

small.achievement.scores$YEAR <- as.numeric(sapply(small.achievement.scores$SCHOOL_YEAR, function (y) {
  strsplit(y,'-')[[1]][1]
})) + 1

#re-calculate N_ACHIEVEMENT_HS by year to include the years of paws
schools.achievement.hs.N <- cast(small.achievement.scores[small.achievement.scores$ANY_TESTED==1, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")], 
                                 SCHOOL_YEAR+SCHOOL_ID~., 
                                 function (x) length(unique(x)))

names(schools.achievement.hs.N)[length(schools.achievement.hs.N)] <- "N_ACHIEVEMENT_HS"




small.achievement.hs <- as.data.frame(cast(schools.achievement.hs.N[,c("SCHOOL_YEAR","SCHOOL_ID","N_ACHIEVEMENT_HS")], SCHOOL_ID~SCHOOL_YEAR, fill=0))

go.back.achievement.hs <- cbind(SCHOOL_ID = small.achievement.hs[,"SCHOOL_ID"], as.data.frame(t(apply(small.achievement.hs, c(1),
                               FUN=function (school) {
                                 compute.N.years(school, min.N.achievement.hs.multiyear)          
                               }))))
#only good for this year
small.schools.hs$YEARS_BACK <- apply(small.schools.hs, c(1),
                                     FUN=function (school) {
                                       id <- school[["SCHOOL_ID"]]
                                       year <- school[["SCHOOL_YEAR"]]
                                       
                                       go.back.achievement.hs[go.back.achievement.hs$SCHOOL_ID==id,][[year]] 
                                       
                                     })


#look at the current year's batch
small.schools.hs[small.schools.hs$SCHOOL_YEAR==current.school.year,]


schools <- schools[, !(names(schools) %in% small.school.hs.labels)]

check.small.school <- function (school) {
  small.school <- with(small.schools.hs, small.schools.hs[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                            SCHOOL_YEAR == school[["SCHOOL_YEAR"]],])
  if (nrow(small.school) == 0)
    result <- c('F', NA)
  else 
    result <- c('T', small.school[,"YEARS_BACK"])
  names(result) <- small.school.hs.labels
  result
}

schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                        "SCHOOL_YEAR",                                                     
                                                        "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                             FUN=check.small.school))))

schools$YEARS_BACK_HS <- as.numeric(schools$YEARS_BACK_HS)

#here are your small hs schools again
schools[schools$SMALL_SCHOOL_HS=='T',]

#these are the ones we can actually do something about
small.schools.hs.fix <- schools[schools$SMALL_SCHOOL_HS=='T' & schools$YEARS_BACK_HS < Inf,]

small.schools.hs.additional.paws.df <- do.call(rbind, apply(small.schools.hs.fix[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK_HS")], c(1),
                                                         FUN=function (school) get.paws.years.back(school, small.achievement.scores, "YEARS_BACK_HS")))

#SCHOOL_YEAR_ORIGINAL is just SCHOOL_YEAR for these rows
act.df$SCHOOL_YEAR_ORIGINAL <- act.df$SCHOOL_YEAR
act.df$YEAR <- as.numeric(sapply(act.df$SCHOOL_YEAR, function (y) {
  strsplit(y,'-')[[1]][1]
})) + 1

act.df <- rbind(act.df, small.schools.hs.additional.paws.df)

