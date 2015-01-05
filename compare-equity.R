##equity reroster
equity.reroster <- merge(with(achievement.for.equity.all, 
     achievement.for.equity.all[SCHOOL_YEAR==current.school.year & 
                                  SCHOOL_FULL_ACADEMIC_YEAR == 'T' & 
                                  TESTING_STATUS_CODE=='T',c("WISER_ID", "SUBJECT_CODE", "SCHOOL_ID", "STD_SCORE")]),
     with(g38.achieve.all.for.equity, g38.achieve.all.for.equity[SCHOOL_YEAR==prior.school.year &
                                  TESTING_STATUS_CODE=='T', c("WISER_ID", "SUBJECT_CODE", "STD_SCORE")]),
     by=c("WISER_ID", "SUBJECT_CODE"),
     all.x=TRUE)


equity.reroster[is.na(equity.reroster$STD_SCORE.y),]


equity.reroster.agg <- aggregate(equity.reroster[c("STD_SCORE.x",
                                                   "STD_SCORE.y")],
                                 by=list(SCHOOL_ID=equity.reroster$SCHOOL_ID),
                                 function (g) round(mean(g, na.rm=TRUE), 0))

equity.reroster.agg.stud <- aggregate(equity.reroster[c("STD_SCORE.x",
                                                   "STD_SCORE.y")],
                                 by=list(WISER_ID=equity.reroster$SCHOOL_ID),
                                 function (g) round(mean(g, na.rm=TRUE), 0))


##end equity reroster


##begin participation rate comparison
table(data.frame(RATE=factor(findInterval(schools[[c(g38.participation.labels["achievement"])]],
                         participation.level.lookup) + 1, 
                         levels=c("1","2","3"), 
                         exclude=c()),
            MET=factor(schools[[g38.participation.level.labels["achievement"]]], 
                       levels=c("1","2","3"), 
                       exclude=c())), useNA="ifany")


table(data.frame(RATE=factor(findInterval(schools[[c(g38.participation.labels["equity"])]],
                                          participation.level.lookup) + 1, 
                             levels=c("1","2","3"), 
                             exclude=c()),
                 MET=factor(schools[[g38.participation.level.labels["equity"]]], 
                            levels=c("1","2","3"), 
                            exclude=c())), useNA="ifany")


table(data.frame(RATE=factor(findInterval(schools[[c(g38.participation.labels["equity"])]],
                                          participation.level.lookup) + 1, 
                             levels=c("1","2","3"), 
                             exclude=c()),
                 MET=factor(schools[[g38.participation.level.labels["equity"]]], 
                            levels=c("1","2","3"), 
                            exclude=c())), useNA="ifany")


table(data.frame(RATE=factor(findInterval(schools[[c(hs.participation.labels["achievement"])]],
                                          participation.level.lookup) + 1, 
                             levels=c("1","2","3"), 
                             exclude=c()),
                 MET=factor(schools[[hs.participation.level.labels["achievement"]]], 
                            levels=c("1","2","3"), 
                            exclude=c())), useNA="ifany")

table(data.frame(RATE=factor(findInterval(schools[[c(hs.participation.labels["equity"])]],
                                          participation.level.lookup) + 1, 
                             levels=c("1","2","3"), 
                             exclude=c()),
                 MET=factor(schools[[hs.participation.level.labels["equity"]]], 
                            levels=c("1","2","3"), 
                            exclude=c())), useNA="ifany")


table(data.frame(RATE=factor(findInterval(schools[[c(hs.participation.labels["tested.readiness"])]],
                                          participation.level.lookup) + 1, 
                             levels=c("1","2","3"), 
                             exclude=c()),
                 MET=factor(schools[[hs.participation.level.labels["tested.readiness"]]], 
                            levels=c("1","2","3"), 
                            exclude=c())), useNA="ifany")

table(data.frame(RATE=factor(schools[["G38_PARTICIPATION_RATE_CAT"]], 
                             levels=c("1","2","3"), 
                             exclude=c()),
                 MET=factor(schools[["G38_PARTICIPATION_CAT"]], 
                            levels=c("1","2","3"), 
                            exclude=c())), useNA="ifany")


table(data.frame(RATE=factor(schools[["HS_PARTICIPATION_RATE_CAT"]], 
                             levels=c("1","2","3"), 
                             exclude=c()),
                 MET=factor(schools[["HS_PARTICIPATION_CAT"]], 
                            levels=c("1","2","3"), 
                            exclude=c())), useNA="ifany")

table(data.frame(RATE=factor(schools[["G38_SPL_ACCOUNTABILITY_OLD"]], 
                             levels=c("1","2","3","4"), 
                             exclude=c()),
                 MET=factor(schools[["G38_SPL_ACCOUNTABILITY"]], 
                            levels=c("1","2","3","4"), 
                            exclude=c())), useNA="ifany")


table(data.frame(RATE=factor(schools[["HS_SPL_ACCOUNTABILITY_OLD"]], 
                             levels=c("1","2","3","4"), 
                             exclude=c()),
                 MET=factor(schools[["HS_SPL_ACCOUNTABILITY"]], 
                            levels=c("1","2","3","4"), 
                            exclude=c())), useNA="ifany")

affected.high.schools <- cbind(schools[!is.na(schools$HS_PARTICIPATION_RATE_CAT) & schools$HS_PARTICIPATION_RATE_CAT != schools$HS_PARTICIPATION_CAT,c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                                                                                                                                            "HS_PARTICIPATION_RATE_CAT",
                                                                                                                                            "HS_PARTICIPATION_CAT",
                                                                                                                                            "HS_SPL_ACCOUNTABILITY_OLD",
                                                                                                                                            "HS_SPL_ACCOUNTABILITY")], TYPE="High School")

affected.g38.schools <- cbind(schools[!is.na(schools$G38_PARTICIPATION_RATE_CAT) & schools$G38_PARTICIPATION_RATE_CAT != schools$G38_PARTICIPATION_CAT,c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                                                                                                                                                 "G38_PARTICIPATION_RATE_CAT",
                                                                                                                                                 "G38_PARTICIPATION_CAT",
                                                                                                                                                 "G38_SPL_ACCOUNTABILITY_OLD",
                                                                                                                                                 "G38_SPL_ACCOUNTABILITY")], TYPE='Elem/Middle School')

names(affected.high.schools)<-c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                                "PARTICIPATION_CAT_PRE_CORRECTION",
                                "PARTICIPATION_CAT",
                                "SPL_PRE_CORRECTION",
                                "SPL", "TYPE")

names(affected.g38.schools)<-c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                                "PARTICIPATION_CAT_PRE_CORRECTION",
                                "PARTICIPATION_CAT",
                                "SPL_PRE_CORRECTION",
                                "SPL", "TYPE")
affected.schools <- rbind(affected.g38.schools, affected.high.schools)
affected.schools[c("PARTICIPATION_CAT_PRE_CORRECTION",
                   "PARTICIPATION_CAT",
                   "SPL_PRE_CORRECTION",
                   "SPL")] <- t(apply(affected.schools[c("PARTICIPATION_CAT_PRE_CORRECTION",
                                                         "PARTICIPATION_CAT",
                                                         "SPL_PRE_CORRECTION",
                                                         "SPL")],
                                                       c(1),
                                                       function (row) {
                                                         c(sapply(1:2,
                                                                  function (i) participation.labels[row[[i]]]),
                                                           sapply(3:4,
                                                                  function (i) SPL.labels[row[[i]]]))
                                                         
                                                       }))

affected.schools <- cbind(affected.schools[c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME", "TYPE")],
                          affected.schools[c("PARTICIPATION_CAT_PRE_CORRECTION",
                                             "PARTICIPATION_CAT",
                                             "SPL_PRE_CORRECTION",
                                             "SPL")])


write.csv(affected.schools, "results/schools-affected-by-part-rate-correction.csv", na="", row.names=FALSE)

##end participation rate comparison


#begin achievement correction comparison

schools.pre <- read.csv(file="results/schools-2013-14-with-indicators-124.csv", colClasses="character")
schools.post <- read.csv(file="results/schools-2013-14-with-indicators-126.csv", colClasses="character")

schools.pre$WAEA_SCHOOL_TYPE <- as.numeric(schools.pre$WAEA_SCHOOL_TYPE)
schools.post$WAEA_SCHOOL_TYPE <- as.numeric(schools.post$WAEA_SCHOOL_TYPE)

schools.pre <- schools.pre[schools.pre$WAEA_SCHOOL_TYPE %in% HS.types, c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME", "HS_ACHIEVEMENT_N_PROFICIENT_TESTS", "HS_ACHIEVEMENT_PERCENT_PROFICIENT", "HS_ACHIEVEMENT_TARGET_LEVEL", "HS_SPL_ACCOUNTABILITY")] 
schools.post <- schools.post[schools.post$WAEA_SCHOOL_TYPE %in% HS.types, c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME", "HS_ACHIEVEMENT_N_PROFICIENT_TESTS", "HS_ACHIEVEMENT_PERCENT_PROFICIENT", "HS_ACHIEVEMENT_TARGET_LEVEL", "HS_SPL_ACCOUNTABILITY")] 

schools.comp <- merge(schools.pre, schools.post, by=c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME"))

names(schools.comp) <- c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME", "HS_ACHIEVEMENT_N_PROFICIENT_TESTS_PRE", "HS_ACHIEVEMENT_PERCENT_PROFICIENT_PRE", "HS_ACHIEVEMENT_TARGET_LEVEL_PRE", "HS_SPL_ACCOUNTABILITY_PRE", 
                         "HS_ACHIEVEMENT_N_PROFICIENT_TESTS_POST", "HS_ACHIEVEMENT_PERCENT_PROFICIENT_POST", "HS_ACHIEVEMENT_TARGET_LEVEL_POST", "HS_SPL_ACCOUNTABILITY_POST")

schools.comp <- schools.comp[c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME", 
                                                                                     "HS_ACHIEVEMENT_N_PROFICIENT_TESTS_PRE", "HS_ACHIEVEMENT_N_PROFICIENT_TESTS_POST",
                                                                                     "HS_ACHIEVEMENT_PERCENT_PROFICIENT_PRE", "HS_ACHIEVEMENT_PERCENT_PROFICIENT_POST",
                                                                                     "HS_ACHIEVEMENT_TARGET_LEVEL_PRE", "HS_ACHIEVEMENT_TARGET_LEVEL_POST", 
                                                                                     "HS_SPL_ACCOUNTABILITY_PRE", "HS_SPL_ACCOUNTABILITY_POST")]

schools.comp[schools.comp$HS_ACHIEVEMENT_N_PROFICIENT_TESTS_PRE != schools.comp$HS_ACHIEVEMENT_N_PROFICIENT_TESTS_POST,]

write.csv(schools.comp[schools.comp$HS_ACHIEVEMENT_N_PROFICIENT_TESTS_PRE != schools.comp$HS_ACHIEVEMENT_N_PROFICIENT_TESTS_POST,], "results/schools-affected-by-engwr-correction.csv", na="", row.names=FALSE)

#end achievement correction comparison

