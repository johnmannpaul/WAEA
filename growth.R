load("data/growth-2013-14.Rdata")
load("data/g38.achieve.Rdata")
load("data/paws.Rdata")

nrow(sgp.df@SGP$SGPercentiles$READING.2013_2014)
growth.reading <- merge(g38.achieve[g38.achieve$SUBJECT == 'Reading',],
                   sgp.df@SGP$SGPercentiles$READING.2013_2014[,c("ID","SGP")],
                   by.x="WISER_ID",
                   by.y="ID")
nrow(growth.reading)

nrow(sgp.df@SGP$SGPercentiles$MATHEMATICS.2013_2014)
growth.math <- merge(g38.achieve[g38.achieve$SUBJECT == 'Math',],
                        sgp.df@SGP$SGPercentiles$MATHEMATICS.2013_2014[,c("ID","SGP")],
                        by.x="WISER_ID",
                        by.y="ID")
nrow(growth.math)

growth.2013.14 <- rbind(growth.reading, growth.math)


paws$SGP <- as.numeric(paws$SGP)
table(paws$SGP, useNA="ifany")

paws.lookback.for.growth <- paws[!is.na(paws$SGP) &
                        paws$SCHOOL_YEAR >= '2011-12',]

paws.lookback.for.growth$SUBJECT <- sapply(paws.lookback.for.growth$SUBJECT_CODE, function (c) switch(c, MA="Math", RE="Reading", SC="Science", NA))

growth.lookback <- data.frame(paws.lookback.for.growth[c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                              SNAPSHOT=NA,
                              SUBJECT_CODE=NA,
                              paws.lookback.for.growth[c("SUBJECT","SCHOOL_FULL_ACADEMIC_YEAR",
                                                "GRADE_ENROLLED","TEST_TYPE", "TESTING_STATUS_CODE")],
                              PERFORMANCE_LEVEL=paws.lookback.for.growth$STANDARD_PAWS_PERF_LEVEL,
                              SCALE_SCORE=paws.lookback.for.growth$STANDARD_PAWS_SCALE_SCORE,
                              WY_ACT_SCALE_SCORE=NA,
                              paws.lookback.for.growth["SGP"]
)
growth.all <- rbind(growth.lookback, growth.2013.14)

table(growth.all[c("SCHOOL_YEAR", "SUBJECT")])



growth.g38.indicator <- compute.indicator.long(growth.all, 
                                               growth.all,
                                               schools,
                                               school.types = nonHS.types,
                                               indicator.label="G38_GROWTH",
                                               score.prefix="SGP",
                                                    agg.fun=function (g) c(N_TESTS=length(g),                                                                           
                                                                           MGP=median(g)))

growth.labels <- names(growth.g38.indicator$schools)[grep("^G38_GROWTH", 
                                                          names(growth.g38.indicator$schools))]

schools[,growth.labels] <- growth.g38.indicator$schools[,growth.labels]

table(schools[c("SCHOOL_YEAR", "G38_GROWTH_SMALL_SCHOOL")])

quantile(with(schools,
              schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                        schools$SCHOOL_YEAR == current.school.year &
                        schools$G38_GROWTH_N >= min.N.growth 
                      ,]$G38_GROWTH_MGP), 
         probs=c(.35,.65),
         type=6)

#start with last year's cuts
schools$G38_GROWTH_TARGET_LEVEL <- findInterval(schools$G38_GROWTH_MGP,
                                            c(45, 60)) + 1

table(schools[schools$SCHOOL_YEAR == current.school.year, c("GRADE_BAND_COMPOSITION", "G38_GROWTH_TARGET_LEVEL")])
prop.table(table(schools[schools$SCHOOL_YEAR == current.school.year, c("GRADE_BAND_COMPOSITION", "G38_GROWTH_TARGET_LEVEL")]),1)

schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
          schools$GRADE_BAND_COMPOSITION == '> 6 only' &
          schools$G38_GROWTH_TARGET_LEVEL == 3,]




write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                    schools$SCHOOL_YEAR == current.school.year &
                    schools$G38_GROWTH_N >= min.N.growth &
                    schools$SCHOOL_ID != state.school.id, 
                  c("SCHOOL_YEAR", "NAME", "SCHOOL_ID", 
                    "GRADE_BAND_COMPOSITION", 
                    "G38_GROWTH_N", 
                    "G38_GROWTH_MGP"),],
          file=get.filename("g38-growth-cfds", "results/cfds"),
          na="",
          row.names=FALSE)


