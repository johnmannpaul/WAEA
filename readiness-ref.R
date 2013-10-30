#save(explore, file="data/ACT/explore.Rdata")
#save(plan, file="data/ACT/plan.Rdata")

load(file="data/ACT/explore.Rdata")
load(file="data/ACT/plan.Rdata")
load(file="data/ACT/act.Rdata")

table(explore[explore$TESTING_STATUS_CODE_COMPOSITE != 'X',]$SCHOOL_FULL_ACADEMIC_YEAR)
explore.df <- explore[explore$SCHOOL_FULL_ACADEMIC_YEAR == 'T' & explore$TESTING_STATUS_CODE_COMPOSITE != 'X', c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "TESTING_STATUS_CODE_COMPOSITE",
                                                                                                                 "TEST_TYPE",
                                                                                                                 "EXPLORE_SCALE_COMPOSITE")]
table(explore.df$TESTING_STATUS_CODE_COMPOSITE)
nrow(explore.df) #must equal number of Ts above


table(plan[plan$TESTING_STATUS_CODE_COMPOSITE != 'X',]$SCHOOL_FULL_ACADEMIC_YEAR)
plan.df <- plan[plan$SCHOOL_FULL_ACADEMIC_YEAR == 'T' & plan$TESTING_STATUS_CODE_COMPOSITE != 'X', c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "TESTING_STATUS_CODE_COMPOSITE",
                                                                                                     "TEST_TYPE",
                                                                                                     "PLAN_SCALE_COMPOSITE")]
nrow(plan.df)  #must equal number of Ts above

table(act[act$TESTING_STATUS_CODE_COMPOSITE != 'X',]$SCHOOL_FULL_ACADEMIC_YEAR)
act.readiness.df <- act[act$SCHOOL_FULL_ACADEMIC_YEAR == 'T' & 
                          act$TESTING_STATUS_CODE_COMPOSITE != 'X', c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", 
                                                                      "TESTING_STATUS_CODE_COMPOSITE",
                                                                      "TESTING_STATUS_CODE_MATH", 
                                                                      "TESTING_STATUS_CODE_READING",
                                                                      "TESTING_STATUS_CODE_SCIENCE",
                                                                      "TEST_TYPE",
                                                                      "ACT_SCALE_SCORE_COMPOSITE", 
                                                                      "WDE_PERFORMANCE_LEVEL_MATH",
                                                                      "WDE_PERFORMANCE_LEVEL_READING",
                                                                      "WDE_PERFORMANCE_LEVEL_SCIENCE")]
nrow(act.readiness.df)  #must equal number of Ts above


table(unique(explore[,c("SCHOOL_YEAR","SCHOOL_ID")])$SCHOOL_YEAR)

nrow(explore.df[explore.df$SCHOOL_ID %in% schools[schools$GRADES_SERVED %in% c("6-9", "7-9", "K-9"),c("SCHOOL_ID")],])
#count of ninth graders in schools with non high school configurations that took the explore
nrow(explore.df[explore.df$SCHOOL_ID %in% schools[!(schools$WAEA_SCHOOL_TYPE %in% c(2,4,5)),c("SCHOOL_ID")],])
#strip those scores out beloning to the above schools...postpone this until the end
#explore.df <- merge(explore.df, schools[schools$WAEA_SCHOOL_TYPE %in% c(2,4,5),c("SCHOOL_YEAR","SCHOOL_ID")])
#
table(unique(explore[,c("SCHOOL_YEAR","SCHOOL_ID")])$SCHOOL_YEAR)


explore.df$RAW_SCORE <- as.numeric(explore.df$EXPLORE_SCALE_COMPOSITE)
plan.df$RAW_SCORE <- as.numeric(plan.df$PLAN_SCALE_COMPOSITE)
act.readiness.df$RAW_SCORE <- apply(act.readiness.df[,c("TESTING_STATUS_CODE_MATH",
                                                        "TESTING_STATUS_CODE_READING",
                                                        "TESTING_STATUS_CODE_SCIENCE",
                                                        "TEST_TYPE",
                                                        "WDE_PERFORMANCE_LEVEL_MATH",
                                                        "WDE_PERFORMANCE_LEVEL_READING",
                                                        "WDE_PERFORMANCE_LEVEL_SCIENCE",
                                                        "ACT_SCALE_SCORE_COMPOSITE")],
                                    c(1),
                                    function (student) {
                                      if(student[["TEST_TYPE"]] == 'ACT')
                                        return(as.numeric(student[["ACT_SCALE_SCORE_COMPOSITE"]]))
                                      
                                      statuses <- student[c("TESTING_STATUS_CODE_MATH",
                                                            "TESTING_STATUS_CODE_READING",
                                                            "TESTING_STATUS_CODE_SCIENCE")]
                                      
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
                                      
                                      if (all(statuses =='N'))
                                        NA
                                      else
                                        proficiencies
                                      
                                    })
act.readiness.df$TESTING_STATUS_CODE <- apply(act.readiness.df[,c("TESTING_STATUS_CODE_MATH",
                                                                  "TESTING_STATUS_CODE_READING",
                                                                  "TESTING_STATUS_CODE_SCIENCE",
                                                                  "TESTING_STATUS_CODE_COMPOSITE",
                                                                  "TEST_TYPE")],
                                              c(1),
                                              function (student) {
                                                if (student[["TEST_TYPE"]] == 'ACT')
                                                  return(student[["TESTING_STATUS_CODE_COMPOSITE"]])
                                                
                                                if(any(student[c("TESTING_STATUS_CODE_MATH",
                                                                 "TESTING_STATUS_CODE_READING",
                                                                 "TESTING_STATUS_CODE_SCIENCE")] == "T"))
                                                  "T"
                                                else
                                                  "N"
                                              }
)


readiness.participation.df <- rbind(plan.df[,c("SCHOOL_YEAR", "SCHOOL_ID", "TEST_TYPE", "RAW_SCORE", "TESTING_STATUS_CODE_COMPOSITE")],
                                    explore.df[,c("SCHOOL_YEAR", "SCHOOL_ID", "TEST_TYPE", "RAW_SCORE", "TESTING_STATUS_CODE_COMPOSITE")])

names(readiness.participation.df) <- c("SCHOOL_YEAR", "SCHOOL_ID", "TEST_TYPE", "RAW_SCORE", "TESTING_STATUS_CODE")

readiness.participation.df <- rbind(readiness.participation.df, act.readiness.df[c("SCHOOL_YEAR", "SCHOOL_ID", "TEST_TYPE", "RAW_SCORE", "TESTING_STATUS_CODE")])



nrow(readiness.participation.df)
table(readiness.participation.df$TEST_TYPE)
nrow(plan.df)
nrow(explore.df)
nrow(act.readiness.df)

readiness.participation.schools <- aggregate(data.frame(N_TESTED = ifelse(readiness.participation.df$TESTING_STATUS_CODE == "T", 1, 0),
                                                        N_PARTICIPANTS = rep(1, nrow(readiness.participation.df))),
                                             by=list(SCHOOL_YEAR = readiness.participation.df$SCHOOL_YEAR,
                                                     SCHOOL_ID = readiness.participation.df$SCHOOL_ID),
                                             sum)

#do state participation
readiness.participation.schools <- state.level.aggregate(readiness.participation.schools, sum, state.school.id)


tail(readiness.participation.schools)



readiness.participation.schools$PARTICIPATION_RATE <- round((readiness.participation.schools$N_TESTED / readiness.participation.schools$N_PARTICIPANTS) * 100, precision)

table(readiness.participation.df$TESTING_STATUS_CODE)
readiness.tested.students.df <- readiness.participation.df[readiness.participation.df$TESTING_STATUS_CODE == "T",] 
nrow(readiness.tested.students.df)




tested.readiness.df <- calc.indexed.readiness(schools[schools$WAEA_SCHOOL_TYPE %in% c(2,4,5),c("SCHOOL_YEAR","SCHOOL_ID")], 
                       readiness.tested.students.df, 
                       readiness.participation.schools[,c("SCHOOL_YEAR", "SCHOOL_ID", "N_TESTED", "PARTICIPATION_RATE")])



##assign tested readiness SPL
schools <- calc.school.tested.readiness.hs(schools)








