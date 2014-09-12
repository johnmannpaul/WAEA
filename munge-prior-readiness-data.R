source("constants.R")


load(file="data/ACT/explore.Rdata")
load(file="data/ACT/plan.Rdata")
load(file="data/ACT/act.Rdata")

#massaging 2012-13 data into format expected by 2013-14 calculation


act.readiness.alt <- reshape(act[act$TEST_TYPE == 'PAWS Alternate',
                                 c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "SCHOOL_FULL_ACADEMIC_YEAR", "GRADE_ENROLLED", "TEST_TYPE", 
                                   "TESTING_STATUS_CODE_MATH", "TESTING_STATUS_CODE_READING", "TESTING_STATUS_CODE_SCIENCE", 
                                   "WDE_PERFORMANCE_LEVEL_MATH", "WDE_PERFORMANCE_LEVEL_READING", "WDE_PERFORMANCE_LEVEL_SCIENCE")], 
                             varying=list(c("TESTING_STATUS_CODE_MATH", "TESTING_STATUS_CODE_READING", "TESTING_STATUS_CODE_SCIENCE"),
                                          c("WDE_PERFORMANCE_LEVEL_MATH", "WDE_PERFORMANCE_LEVEL_READING", "WDE_PERFORMANCE_LEVEL_SCIENCE")),
                             timevar="SUBJECT",
                             v.names=c("TESTING_STATUS_CODE", "PERFORMANCE_LEVEL"),
                             times=c("Math", "Reading", "Science"),
                             direction="long")

act.readiness.alt <- data.frame(act.readiness.alt[c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")],
                                SNAPSHOT=NA,
                                SUBJECT_CODE=NA,
                                act.readiness.alt[c("SUBJECT", "SCHOOL_FULL_ACADEMIC_YEAR", "GRADE_ENROLLED", "TEST_TYPE", "TESTING_STATUS_CODE",
                                                  "PERFORMANCE_LEVEL")],
                                SCALE_SCORE=NA,
                                WY_ACT_SCALE_SCORE=NA
                                )

act.readiness <- data.frame(act[act$TEST_TYPE == 'ACT', c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")],
                            SNAPSHOT=NA,
                            SUBJECT_CODE=NA,
                            SUBJECT="Composite",
                            act[act$TEST_TYPE == 'ACT', c("SCHOOL_FULL_ACADEMIC_YEAR", "GRADE_ENROLLED", "TEST_TYPE")],
                            TESTING_STATUS_CODE=act[act$TEST_TYPE == 'ACT', "TESTING_STATUS_CODE_COMPOSITE"],
                            PERFORMANCE_LEVEL=NA,
                            SCALE_SCORE=act[act$TEST_TYPE == 'ACT', "ACT_SCALE_SCORE_COMPOSITE"],
                            WY_ACT_SCALE_SCORE=NA)


act.readiness <- rbind(act.readiness.alt,act.readiness)
save(act.readiness, file="data/ACT/act.readiness.Rdata")


plan.readiness <- data.frame(plan[c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")],
                            SNAPSHOT=NA,
                            SUBJECT_CODE=NA,
                            SUBJECT="Composite",
                            plan[c("SCHOOL_FULL_ACADEMIC_YEAR", "GRADE_ENROLLED", "TEST_TYPE")],
                            TESTING_STATUS_CODE=plan[,"TESTING_STATUS_CODE_COMPOSITE"],
                            PERFORMANCE_LEVEL=NA,
                            SCALE_SCORE=plan[, "PLAN_SCALE_COMPOSITE"],
                            WY_ACT_SCALE_SCORE=NA)


save(plan.readiness, file="data/ACT/plan.readiness.Rdata")


explore.readiness <- data.frame(explore[c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")],
                             SNAPSHOT=NA,
                             SUBJECT_CODE=NA,
                             SUBJECT="Composite",
                             explore[c("SCHOOL_FULL_ACADEMIC_YEAR", "GRADE_ENROLLED", "TEST_TYPE")],
                             TESTING_STATUS_CODE=explore[,"TESTING_STATUS_CODE_COMPOSITE"],
                             PERFORMANCE_LEVEL=NA,
                             SCALE_SCORE=explore[, "EXPLORE_SCALE_COMPOSITE"],
                             WY_ACT_SCALE_SCORE=NA)


save(explore.readiness, file="data/ACT/explore.readiness.Rdata")


act.suite.readiness <- rbind(act.readiness,
                             plan.readiness,
                             explore.readiness)

save(act.suite.readiness, file="data/ACT/act.suite.readiness.Rdata")
