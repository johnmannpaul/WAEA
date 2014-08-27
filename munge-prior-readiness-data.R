source("constants.R")
source('db-functions.R')
source('const/private/db.R')


load(file="data/ACT/explore.Rdata")
load(file="data/ACT/plan.Rdata")
load(file="data/ACT/act.Rdata")

#massaging 2012-13 data into format expected by 2013-14 calculation

#there was no writing
act.readiness <- cbind(act[,c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "GRADE_ENROLLED", "SCHOOL_FULL_ACADEMIC_YEAR", 
                        "TEST_TYPE",
                        "TESTING_STATUS_CODE_COMPOSITE", "TESTING_STATUS_CODE_MATH", "TESTING_STATUS_CODE_READING",
                        "TESTING_STATUS_CODE_SCIENCE")], TESTING_STATUS_CODE_WRITING=rep('X', nrow(act)), act[,c("ACT_SCALE_SCORE_COMPOSITE", "WDE_PERFORMANCE_LEVEL_MATH", "WDE_PERFORMANCE_LEVEL_READING",
                        "WDE_PERFORMANCE_LEVEL_SCIENCE")], WDE_PERFORMANCE_LEVEL_WRITING=rep(NA, nrow(act)))

#Standard takers are exempt from the subject tests and alternate takers are exempt from the composite test for purposes of tested readiness
act.readiness[,c("TESTING_STATUS_CODE_COMPOSITE", 
                 "TESTING_STATUS_CODE_MATH",
                 "TESTING_STATUS_CODE_READING",
                 "TESTING_STATUS_CODE_SCIENCE", 
                 "TESTING_STATUS_CODE_WRITING")] <- data.frame(t(apply(act.readiness[,c("TEST_TYPE",
                                                                                        "TESTING_STATUS_CODE_COMPOSITE", 
                                                                                        "TESTING_STATUS_CODE_MATH",
                                                                                        "TESTING_STATUS_CODE_READING",
                                                                                        "TESTING_STATUS_CODE_SCIENCE", 
                                                                                        "TESTING_STATUS_CODE_WRITING")],
                                                                       c(1),
                                                                       function (student, standard.test.types) {
                                                                         
                                                                         if (student[["TEST_TYPE"]] %in% standard.test.types)
                                                                           c(student[["TESTING_STATUS_CODE_COMPOSITE"]], rep('X', 4))
                                                                         else
                                                                           c('X', student[c("TESTING_STATUS_CODE_MATH",
                                                                                            "TESTING_STATUS_CODE_READING",
                                                                                            "TESTING_STATUS_CODE_SCIENCE", 
                                                                                            "TESTING_STATUS_CODE_WRITING")])
                                                                       }, readiness.standard.test.types)))
                                                                         
#There was not alternate assessment on the 2012-13 plan exam
plan.readiness <- cbind(plan[,c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "GRADE_ENROLLED", "SCHOOL_FULL_ACADEMIC_YEAR", 
                                "TEST_TYPE", "TESTING_STATUS_CODE_COMPOSITE")],
                        TESTING_STATUS_CODE_MATH=rep('X', nrow(plan)),
                        TESTING_STATUS_CODE_READING=rep('X', nrow(plan)),
                        TESTING_STATUS_CODE_SCIENCE=rep('X', nrow(plan)),
                        TESTING_STATUS_CODE_WRITING=rep('X', nrow(plan)),                          
                        plan[,c("PLAN_SCALE_COMPOSITE")],
                        WDE_PERFORMANCE_LEVEL_MATH=rep(NA, nrow(plan)),
                        WDE_PERFORMANCE_LEVEL_READING=rep(NA, nrow(plan)),
                        WDE_PERFORMANCE_LEVEL_SCIENCE=rep(NA, nrow(plan)),
                        WDE_PERFORMANCE_LEVEL_WRITING=rep(NA, nrow(plan)))



explore.readiness <- cbind(explore[,c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "GRADE_ENROLLED", "SCHOOL_FULL_ACADEMIC_YEAR", 
                                "TEST_TYPE", "TESTING_STATUS_CODE_COMPOSITE")],
                        TESTING_STATUS_CODE_MATH=rep('X', nrow(explore)),
                        TESTING_STATUS_CODE_READING=rep('X', nrow(explore)),
                        TESTING_STATUS_CODE_SCIENCE=rep('X', nrow(explore)),
                        TESTING_STATUS_CODE_WRITING=rep('X', nrow(explore)),                          
                        explore[,c("EXPLORE_SCALE_COMPOSITE")],
                        WDE_PERFORMANCE_LEVEL_MATH=rep(NA, nrow(explore)),
                        WDE_PERFORMANCE_LEVEL_READING=rep(NA, nrow(explore)),
                        WDE_PERFORMANCE_LEVEL_SCIENCE=rep(NA, nrow(explore)),
                        WDE_PERFORMANCE_LEVEL_WRITING=rep(NA, nrow(explore)))



names(act.readiness) <- c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "GRADE_ENROLLED", "SCHOOL_FULL_ACADEMIC_YEAR", "TEST_TYPE", "TESTING_STATUS_CODE_COMPOSITE",                          
                          "TESTING_STATUS_CODE_MATH", "TESTING_STATUS_CODE_READING",
                          "TESTING_STATUS_CODE_SCIENCE", "TESTING_STATUS_CODE_WRITING", "SCALE_SCORE_COMPOSITE", "WDE_PERFORMANCE_LEVEL_MATH", "WDE_PERFORMANCE_LEVEL_READING",
                          "WDE_PERFORMANCE_LEVEL_SCIENCE", "WDE_PERFORMANCE_LEVEL_WRITING")

names(plan.readiness) <- names(act.readiness)
names(explore.readiness) <- names(act.readiness)

save(act.readiness, file="data/ACT/act.readiness.Rdata")
save(plan.readiness, file="data/ACT/plan.readiness.Rdata")
save(explore.readiness, file="data/ACT/explore.readiness.Rdata")





conn <- odbcConnect(dsn=db.dsn, uid=rdbms.name, pwd=rdbms.pwd)


current.act.readiness <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][["act.readiness"]], as.is=as.is.vector(names(act.readiness)))
act.readiness <- rbind(act.readiness,current.act.readiness)
save(act.readiness, file="data/ACT/act.readiness.Rdata")

