load(file="data/ACT/act.suite.readiness.Rdata")



#remove non-high schools (e.g. schools that serve grade 9, but do not award diplomas)

nrow(act.suite.readiness)

readiness.all.df <- merge(act.suite.readiness,
                          data.frame(SCHOOL_ID=unique(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types, "SCHOOL_ID"])))


nrow(readiness.all.df)


#compute raw scores for each student
readiness.raw.scores <- readiness.all.df[readiness.all.df$TEST_TYPE %in% readiness.standard.test.types &
                                           readiness.all.df$SCHOOL_FULL_ACADEMIC_YEAR == 'T',
                                         c("SCHOOL_YEAR", 
                                           "SCHOOL_ID", 
                                           "WISER_ID",
                                           "GRADE_ENROLLED",
                                           "SCHOOL_FULL_ACADEMIC_YEAR",
                                           "TESTING_STATUS_CODE",
                                           "TEST_TYPE",
                                           "SCALE_SCORE")]

names(readiness.raw.scores)[ncol(readiness.raw.scores)] <- "TESTED_READINESS_RAW_SCORE"

readiness.all.df.alt <- readiness.all.df[!(readiness.all.df$TEST_TYPE %in% readiness.standard.test.types) &
                                           readiness.all.df$SCHOOL_FULL_ACADEMIC_YEAR == 'T',] 

readiness.raw.scores.alt <- aggregate(data.frame(TESTED_READINESS_ALTERNATE_PROFICIENCIES = ifelse(readiness.all.df.alt$PERFORMANCE_LEVEL %in% c('3','4'), 1, 0),
                     TESTED_READINESS_ALTERNATE_TESTS_TAKEN = ifelse(readiness.all.df.alt$TESTING_STATUS_CODE == 'T', 1, 0)),
          by=list(SCHOOL_YEAR = readiness.all.df.alt$SCHOOL_YEAR,
                  SCHOOL_ID = readiness.all.df.alt$SCHOOL_ID,
                  WISER_ID =  readiness.all.df.alt$WISER_ID,
                  GRADE_ENROLLED = readiness.all.df.alt$GRADE_ENROLLED),
          sum)

table(readiness.raw.scores.alt$TESTED_READINESS_ALTERNATE_PROFICIENCIES/
        readiness.raw.scores.alt$TESTED_READINESS_ALTERNATE_TESTS_TAKEN)

readiness.raw.scores.alt <- data.frame(readiness.raw.scores.alt[c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID", "GRADE_ENROLLED")],
                                       SCHOOL_FULL_ACADEMIC_YEAR='T',                                       
                                       TESTING_STATUS_CODE = ifelse(readiness.raw.scores.alt$TESTED_READINESS_ALTERNATE_TESTS_TAKEN > 0, 'T', 'N'),
                                       
                                       TEST_TYPE='ALT',
                                       TESTED_READINESS_RAW_SCORE = findInterval(readiness.raw.scores.alt$TESTED_READINESS_ALTERNATE_PROFICIENCIES/
                                                                                   readiness.raw.scores.alt$TESTED_READINESS_ALTERNATE_TESTS_TAKEN,
                                                                                 alt.index.intervals))
table(readiness.raw.scores.alt$TESTED_READINESS_RAW_SCORE)
#compute indexed scores for each student
readiness.indexed.scores <- rbind(readiness.raw.scores, readiness.raw.scores.alt)

readiness.indexed.scores$TESTED_READINESS_INDEX_SCORE <- apply(readiness.indexed.scores[,c("TEST_TYPE",
                                                                                           "TESTING_STATUS_CODE",
                                                                                           "TESTED_READINESS_RAW_SCORE")],
                                                       c(1),
                                                       function (student, lookup) {
                                                         status <- student[["TESTING_STATUS_CODE"]]
                                                         if (status != 'T')
                                                           return(NA)
                                                         
                                                         tt <- student[["TEST_TYPE"]]
                                                         tt <- if (tt %in% readiness.standard.test.types) tt else 'ALT'
                                                         lookup.value <- as.character(as.numeric(student[["TESTED_READINESS_RAW_SCORE"]]))
                                                         
                                                         lookup[[tt]][lookup.value]
                                                         
                                                       }, readiness.indeces)


#compute the indicator using the original student frame for participation
#and the indexed scores frame for scores
tested.readiness.indicator <- compute.indicator.long(readiness.all.df,
                                                     readiness.indexed.scores,
                                                     schools,
                                                     indicator.label = "HS_TESTED_READINESS",                                                                                    
                                                     score.prefix="TESTED_READINESS_INDEX_SCORE",
                                                     agg.fun=function (g) {
                                                       c(N_sCORES=length(which(!is.na(as.numeric(g)))), 
                                                         MEAN=round(mean(as.numeric(g), na.rm=TRUE), precision.readiness))
                                                     })


tested.readiness.labels <- setdiff(names(tested.readiness.indicator$schools), names(schools))
schools[,tested.readiness.labels] <- tested.readiness.indicator$schools[,tested.readiness.labels]





##check and compare the participation rates
table(findInterval(schools[schools$SCHOOL_YEAR==prior.school.year,"HS_TESTED_READINESS_PARTICIPATION_RATE"], c(90, 95)))
table(findInterval(schools[schools$SCHOOL_YEAR==current.school.year,"HS_TESTED_READINESS_PARTICIPATION_RATE"], c(90, 95)))
##end checks


write.csv(file=get.filename("tested-readiness-cfds", "results/cfds"), schools[
  schools$WAEA_SCHOOL_TYPE %in% HS.types &
    schools$SCHOOL_YEAR==current.school.year & 
    schools$HS_TESTED_READINESS_N >= min.N.tested.readiness &
    schools$SCHOOL_ID != state.school.id,
                  c("SCHOOL_YEAR", "SCHOOL_ID", "ALTERNATIVE_SCHOOL", "HS_TESTED_READINESS_PARTICIPATION_RATE", "HS_TESTED_READINESS_MEAN")],
          na="",
          row.names=FALSE)
##checks

head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types &
               schools$SCHOOL_YEAR==current.school.year,], 5)

table(schools[c("SCHOOL_YEAR","HS_TESTED_READINESS_SMALL_SCHOOL")])
table(schools[c("SCHOOL_YEAR","HS_TESTED_READINESS_YEARS_BACK")])

head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types &
               schools$SCHOOL_YEAR==current.school.year &
               schools$HS_TESTED_READINESS_SMALL_SCHOOL %in% 'T',], 5)
  
head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types &
               schools$SCHOOL_YEAR==current.school.year &
               !is.na(schools$HS_TESTED_READINESS_YEARS_BACK) &
               schools$HS_TESTED_READINESS_YEARS_BACK < Inf,], 5)
