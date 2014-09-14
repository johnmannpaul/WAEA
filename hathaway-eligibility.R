load(file="data/hathaway.eligibility.Rdata")



head(hathaway.eligibility)



table(hathaway.eligibility$HATH_CAT, useNA="ifany")
table(hathaway.eligibility$HATH_INDEX, useNA="ifany")

#should be equal.  No duplicate wiser_ids 
length(hathaway.eligibility$WISER_ID)
length(unique(hathaway.eligibility$WISER_ID))


class(hathaway.eligibility$HATH_CAT)
hathaway.eligibility$HATH_INDEX_SCORE <- sapply(as.numeric(hathaway.eligibility$HATH_CAT),
                                                function (c) {
                                                  hathaway.eligibility.index[c]
                                                  
                                                })


# hathaway.eligibility.index.option2 <- c(20, 40, 60, 80, 100)
# 
# hathaway.eligibility$HATH_INDEX_SCORE_OPT2 <- sapply(hathaway.eligibility$HATH_CAT,
#                                                      function (cat) {
#                                                        
#                                                        hathaway.eligibility.index.option2[as.numeric(cat)]
#                                                     })

hathaway.eligibility.school <- unmatrixfy.df(aggregate(data.frame(HATH_INDEX_SCORE=hathaway.eligibility$HATH_INDEX_SCORE),
                                                     by=list(SCHOOL_YEAR=hathaway.eligibility$ACCOUNTABILITY_YEAR,
                                                             SCHOOL_ID=hathaway.eligibility$EXIT_RECORD_SCHOOL_ID),
                                                     function (v) {
                                                       c(N=length(v),
                                                         MEAN=round(mean(v), precision.readiness))
                                                     }))


# hathaway.eligibility.school$HATH_INDEX_SCORE_OPT1_MEAN <- unmatrixfy.df(aggregate(data.frame(HATH_INDEX_SCORE_OPT1=hathaway.eligibility$HATH_INDEX_SCORE),
#                                                                                   by=list(SCHOOL_YEAR=hathaway.eligibility$ACCOUNTABILITY_YEAR,
#                                                                                           SCHOOL_ID=hathaway.eligibility$EXIT_RECORD_SCHOOL_ID),
#                                                                                   function (v) {
#                                                                                     c(N=length(v),
#                                                                                       MEAN=round(mean(v), precision.readiness))
#                                                                                   }))$HATH_INDEX_SCORE_OPT1_MEAN
# 
# hathaway.eligibility.school$HATH_INDEX_SCORE_OPT2_MEAN <- unmatrixfy.df(aggregate(data.frame(HATH_INDEX_SCORE_OPT2=hathaway.eligibility$HATH_INDEX_SCORE_OPT2),
#                                                        by=list(SCHOOL_YEAR=hathaway.eligibility$ACCOUNTABILITY_YEAR,
#                                                                SCHOOL_ID=hathaway.eligibility$EXIT_RECORD_SCHOOL_ID),
#                                                        function (v) {
#                                                          c(N=length(v),
#                                                            MEAN=round(mean(v), precision.readiness))
#                                                        }))$HATH_INDEX_SCORE_OPT2_MEAN

hathaway.eligibility.school.cat <- unmatrixfy.df(aggregate(data.frame(HATH_CAT=as.numeric(hathaway.eligibility$HATH_CAT)),
                                                       by=list(SCHOOL_YEAR=hathaway.eligibility$ACCOUNTABILITY_YEAR,
                                                               SCHOOL_ID=hathaway.eligibility$EXIT_RECORD_SCHOOL_ID),
                                                       function (v) {
                                                         c(N=length(v),
                                                           MEAN=round(mean(v), 2))
                                                       }))
head(hathaway.eligibility.school)


hathaway.eligibility.school$SMALL_SCHOOL_HATH_ELIGIBILITY <- ifelse(hathaway.eligibility.school$HATH_INDEX_SCORE_N < min.N.hath.eligibility, "T", "F") 

table(hathaway.eligibility.school$SMALL_SCHOOL_HATH_ELIGIBILITY)

schools <- bind.indicator(schools, 
                          hathaway.eligibility.school,
                          indicator.labels.min.N = c(N= "HATH_INDEX_SCORE_N", score="HATH_INDEX_SCORE_MEAN"),
                          min.N.hath.eligibility)


schools <- bind.indicator(schools, 
                          hathaway.eligibility.school.cat,
                          indicator.labels.min.N = c(N= "HATH_CAT_N", score="HATH_CAT_MEAN"),
                          min.N.hath.eligibility)


schools$SMALL_SCHOOL_HATH_ELIGIBILITY <- apply(schools[,c("SCHOOL_ID", "SCHOOL_YEAR")],
                                                c(1),
                                                function(school) {
                                                  lookup <- hathaway.eligibility.school[hathaway.eligibility.school$SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                          hathaway.eligibility.school$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],]
                                                  if (nrow(lookup) == 0)
                                                    NA
                                                  else
                                                    lookup$SMALL_SCHOOL_HATH_ELIGIBILITY
                                                  
                                                })

head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==current.school.year,])


# write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
#                     schools$SCHOOL_YEAR==current.school.year &
#                     schools$HATH_CAT_N >= min.N.hath.eligibility,c("SCHOOL_ID", 
#                                                                "SCHOOL_YEAR", 
#                                                                "ALTERNATIVE_SCHOOL", 
#                                                                "HATH_CAT_N", "HATH_CAT_MEAN", "SMALL_SCHOOL_HATH_ELIGIBILITY")], 
#           file="results/hathaway-elligibility-subindicator.csv", na="", row.names=FALSE)


write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
                    !is.na(schools$HATH_INDEX_SCORE_N) &
                    schools$HATH_INDEX_SCORE_N >= min.N.hath.eligibility &
                    schools$SCHOOL_YEAR==current.school.year &
                    schools$SCHOOL_ID != state.school.id,c("SCHOOL_ID", 
                                                           "SCHOOL_YEAR", 
                                                           "ALTERNATIVE_SCHOOL", 
                                                           "HATH_INDEX_SCORE_N", "HATH_INDEX_SCORE_MEAN", "HATH_CAT_MEAN")], 
          file=get.filename("hathaway-elligibility-cfds", "results/cfds"), na="", row.names=FALSE)
