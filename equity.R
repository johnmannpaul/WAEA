load(file="data/achievement-for-equity.Rdata")

equity.g38.indicator <- compute.indicator.long(achievement.for.equity.all, 
                                               achievement.for.equity.all,
                                               schools,
                                               school.types = nonHS.types,
                                               indicator.label="G38_EQUITY",
                                               score.prefix="STD_SCORE",
                                               agg.fun=function (g) c(N_TESTS=length(g),                                                                           
                                                                      MEAN=round(mean(g), 0)))


equity.g38.labels <- names(equity.g38.indicator$schools)[grep("^G38_EQUITY", 
                                                              names(equity.g38.indicator$schools))]

schools[,equity.g38.labels] <- equity.g38.indicator$schools[,equity.g38.labels]

table(schools[c("SCHOOL_YEAR", "G38_EQUITY_SMALL_SCHOOL")])

table(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types,
              c("SCHOOL_YEAR", "G38_EQUITY_YEARS_BACK")], useNA="ifany")
equity.g38.participation.rate <- schools[schools$SCHOOL_YEAR==current.school.year &
                                           schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                                           schools$G38_EQUITY_N >= min.N.subgroup,"G38_EQUITY_PARTICIPATION_RATE"]

equity.g38.participation.rate[order(equity.g38.participation.rate)]


quantile(with(schools,
              schools[schools$SCHOOL_YEAR==current.school.year &
                        schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                        schools$G38_EQUITY_N >= min.N.subgroup
                      ,]$G38_EQUITY_MEAN), 
         probs=c(.35,.65),
         type=6)

schools$G38_EQUITY_TARGET_LEVEL <- findInterval(schools$G38_EQUITY_MEAN,
                                                round(quantile(with(schools,
                                                                    schools[schools$SCHOOL_YEAR==current.school.year &
                                                                              schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                                                                              schools$G38_EQUITY_N >= min.N.subgroup
                                                                            ,]$G38_EQUITY_MEAN), 
                                                               probs=c(.35,.65),
                                                               type=6),0)) + 1

table(schools[schools$SCHOOL_YEAR == current.school.year, c("GRADE_BAND_COMPOSITION", "G38_EQUITY_TARGET_LEVEL")])
prop.table(table(schools[schools$SCHOOL_YEAR == current.school.year, c("GRADE_BAND_COMPOSITION", "G38_EQUITY_TARGET_LEVEL")]),1)

schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
          schools$G38_EQUITY_N >= min.N.subgroup &
          schools$G38_EQUITY_PARTICIPATION_RATE < 95
        ,]

write.csv(schools[schools$SCHOOL_YEAR==current.school.year &
                    schools$WAEA_SCHOOL_TYPE %in% nonHS.types &
                    schools$G38_EQUITY_N >= min.N.subgroup &
                    schools$SCHOOL_ID != state.school.id, 
                  c("SCHOOL_YEAR", "NAME", "SCHOOL_ID", 
                    "GRADE_BAND_COMPOSITION", 
                    "G38_EQUITY_YEARS_BACK",
                    "G38_EQUITY_N", 
                    "G38_EQUITY_MEAN"),],
          file=get.filename("g38-equity-cfds", "results/cfds"),
          na="",
          row.names=FALSE)

