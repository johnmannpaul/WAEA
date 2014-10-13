load("data/ACT/act.Rdata")
load("data/ACT/act.achieve.Rdata")
table(act$TEST_TYPE)


act.long <- reshape(act[c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID","GRADE_ENROLLED", "SCHOOL_FULL_ACADEMIC_YEAR", "TEST_TYPE",
              "TESTING_STATUS_CODE_MATH",
              "TESTING_STATUS_CODE_READING",
              "TESTING_STATUS_CODE_ENGLISH",
              "TESTING_STATUS_CODE_SCIENCE",
              "TESTING_STATUS_CODE_COMPOSITE",
              "ACT_SCALE_SCORE_MATH",
              "ACT_SCALE_SCORE_READING",
              "ACT_SCALE_SCORE_ENGLISH",
              "ACT_SCALE_SCORE_SCIENCE",
              "ACT_SCALE_SCORE_COMPOSITE")],
        varying=list(c("TESTING_STATUS_CODE_MATH",
                       "TESTING_STATUS_CODE_READING",
                       "TESTING_STATUS_CODE_ENGLISH",
                       "TESTING_STATUS_CODE_SCIENCE",
                       "TESTING_STATUS_CODE_COMPOSITE"),
                     c("ACT_SCALE_SCORE_MATH",
                       "ACT_SCALE_SCORE_READING",
                       "ACT_SCALE_SCORE_ENGLISH",
                       "ACT_SCALE_SCORE_SCIENCE",
                       "ACT_SCALE_SCORE_COMPOSITE")),
        v.names=c("TESTING_STATUS_CODE", "SCALE_SCORE"),
        timevar = "SUBJECT",
        times = c("Math", "Reading", "English", "Science", "Composite"),
        direction="long")

#not including English in lookback because we've gone to Enlish-Writing in the current and future years
act.2012.13 <- act.long[act.long$TEST_TYPE=="ACT" & 
                          act.long$TESTING_STATUS_CODE=='T' &
                          act.long$SUBJECT %in% c("Math", "Reading", "Science"),]

table(act.2012.13[c("SCHOOL_YEAR", "GRADE_ENROLLED")])
table(act.2012.13[c("SCHOOL_YEAR", "SUBJECT")])


act.2013.14 <- act.achieve[act.achieve$TEST_TYPE=="ACT" & 
                             act.achieve$TESTING_STATUS_CODE=='T' &
                             act.achieve$SUBJECT %in% c("Math", "Reading", "Science"),]

table(act.2013.14[c("SCHOOL_YEAR", "GRADE_ENROLLED")])
table(act.2013.14[c("SCHOOL_YEAR", "SUBJECT")])


equipercentile.lookback <- function(current.df, prior.df, perf.level.prefix="PERFORMANCE_LEVEL",
                                    score.prefix="SCALE_SCORE") {
  
  
  combos <- unique(current.df[c("SUBJECT","GRADE_ENROLLED")])
  
  percent.non.proficient <- do.call(rbind,
                                    lapply(1:nrow(combos),
                                           function (i) {
                                             data.frame(SUBJECT=combos[i,"SUBJECT"],
                                                        GRADE_ENROLLED=combos[i,"GRADE_ENROLLED"],
                                                        unmatrixfy.df(calc.mean.score(current.df[current.df$SUBJECT==combos[i,"SUBJECT"] &
                                                                                                   current.df$GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],],                               
                                                                                      score.prefix=perf.level.prefix,
                                                                                      agg.function=function (g) c(N_TESTS=length(g),
                                                                                                                  N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                                                                  PERCENT_NOT_PROFICIENT=round((sum(ifelse(g %in% c('1','2'), 1, 0))/length(g))*100, 2)),
                                                                                      already.long=TRUE), prepend=FALSE))
                                             
                                           }))
  
  statewide.percents.nonproficient <- percent.non.proficient[percent.non.proficient$SCHOOL_ID==state.school.id,]
  
  
  do.cuts <- function (i) {
    
    agg.function <- function (g) {
      c(N_TESTS=length(g),                                     
        CUT=round(quantile(g,with(statewide.percents.nonproficient,
                                  statewide.percents.nonproficient[SUBJECT==combos[i,"SUBJECT"] &
                                                                  GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],
                                                                "PERCENT_NOT_PROFICIENT"]/100),0)))
    }
    
    result <- data.frame(SUBJECT=combos[i,"SUBJECT"],
                         GRADE_ENROLLED=combos[i,"GRADE_ENROLLED"],
                         unmatrixfy.df(calc.mean.score(prior.df[prior.df$SUBJECT==combos[i,"SUBJECT"] &
                                                                  prior.df$GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],],                               
                                                       score.prefix=score.prefix,
                                                       agg.function=agg.function,
                                                       already.long=TRUE), prepend=FALSE))
    names(result)[6] <-"CUT"
    result
  }
  
  cuts.prior <- do.call(rbind,
                        lapply(1:nrow(combos), do.cuts))
  
  cuts.statewide <- cuts.prior[cuts.prior$SCHOOL_ID==state.school.id,]
  
  
  lookback.perf.level <- apply(prior.df[,c("SUBJECT", "GRADE_ENROLLED", score.prefix)],
                                    c(1),
                                    function (row) {
                                      cut <- cuts.statewide[cuts.statewide$SUBJECT == row[["SUBJECT"]] &
                                                              cuts.statewide$GRADE_ENROLLED == row[["GRADE_ENROLLED"]], "CUT"]
                                      
                                      if (as.numeric(row[[score.prefix]]) > cut)
                                        "3"
                                      else
                                        "2"
                                      
                                    })
  
  
  lookback.df <- data.frame(prior.df[c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                                     SNAPSHOT=NA,
                                     SUBJECT_CODE=NA,
                                     prior.df[c("SUBJECT","SCHOOL_FULL_ACADEMIC_YEAR",
                                                     "GRADE_ENROLLED","TEST_TYPE", "TESTING_STATUS_CODE")],
                                     PERFORMANCE_LEVEL=lookback.perf.level,
                                     SCALE_SCORE=prior.df[[score.prefix]],
                                     WY_ACT_SCALE_SCORE=NA
  )
  
  lookback.df
}

act.2013.14$SCHOOL_YEAR_ORIGINAL <- act.2013.14$SCHOOL_YEAR
act.2012.13$SCHOOL_YEAR_ORIGINAL <- act.2012.13$SCHOOL_YEAR

act.lookback <- equipercentile.lookback(act.2013.14, act.2012.13)
act.lookback$SCHOOL_YEAR_ORIGINAL <- act.lookback$SCHOOL_YEAR

combos <- unique(act.lookback[c("SUBJECT","GRADE_ENROLLED")])
act.non.proficient.lookback <- do.call(rbind,
                                           lapply(1:nrow(combos),
                                                  function (i) {
                                                    data.frame(SUBJECT=combos[i,"SUBJECT"],
                                                               GRADE_ENROLLED=combos[i,"GRADE_ENROLLED"],
                                                               unmatrixfy.df(calc.mean.score(act.lookback[act.lookback$SUBJECT==combos[i,"SUBJECT"] &
                                                                                                            act.lookback$GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],],                               
                                                                                             score.prefix="PERFORMANCE_LEVEL",
                                                                                             agg.function=function (g) c(N_TESTS=length(g),
                                                                                                                         N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                                                                         PERCENT_NOT_PROFICIENT=round((sum(ifelse(g %in% c('1','2'), 1, 0))/length(g))*100, 2)),
                                                                                             already.long=TRUE), prepend=FALSE))
                                                    
                                                  }))
statewide.act.non.proficient.lookback <- act.non.proficient.lookback[act.non.proficient.lookback$SCHOOL_ID==state.school.id,]

##look at prior score distribution
prop.table(table(act.2012.13[c("SUBJECT", "SCALE_SCORE")]), 1)

###inlcude PAWS 2011-12
act.lookback$SUBJECT_CODE <- sapply(act.lookback$SUBJECT,
                                    function (s) 
                                      switch(s, Math="MA",Reading="RE",Science="SC",NA), USE.NAMES=FALSE)

act.lookback <- act.lookback[,!(names(act.lookback) %in% c("SCHOOL_YEAR_ORIGINAL"))]
save(act.lookback, file="data/ACT/act.lookback.Rdata")


load(file="data/paws_11_achievement.Rdata")

paws.11.long <- reshape(paws_11_achievement[c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID","GRADE_ENROLLED", "SCHOOL_FULL_ACADEMIC_YEAR",
                          "MA_TESTING_STATUS_CODE",
                          "RE_TESTING_STATUS_CODE",
                          "SC_TESTING_STATUS_CODE",
                          "MA_STANDARD_PAWS_SCALE_SCORE",
                          "RE_STANDARD_PAWS_SCALE_SCORE",
                          "SC_STANDARD_PAWS_SCALE_SCORE",
                          "RE_TEST_TYPE",
                          "MA_TEST_TYPE",
                          "SC_TEST_TYPE")],
                    varying=list(c("MA_TESTING_STATUS_CODE",
                                   "RE_TESTING_STATUS_CODE",
                                   "SC_TESTING_STATUS_CODE"),
                                 c("MA_STANDARD_PAWS_SCALE_SCORE",
                                   "RE_STANDARD_PAWS_SCALE_SCORE",
                                   "SC_STANDARD_PAWS_SCALE_SCORE"),
                                 c("RE_TEST_TYPE",
                                   "MA_TEST_TYPE",
                                   "SC_TEST_TYPE")),
                    v.names=c("TESTING_STATUS_CODE", "SCALE_SCORE", "TEST_TYPE"),
                    timevar = "SUBJECT",
                    times = c("Math", "Reading", "Science"),
                    direction="long")

table(paws.11.long[c("SCHOOL_YEAR", "TEST_TYPE")])

paws.2011.12 <- paws.11.long[paws.11.long$TEST_TYPE=="PAWS-STANDARD" & 
                              paws.11.long$TESTING_STATUS_CODE=='T' &
                              paws.11.long$SCHOOL_YEAR == '2011-12' &
                              paws.11.long$SUBJECT %in% c("Math", "Reading", "Science"),]


paws.2011.12$SCHOOL_YEAR_ORIGINAL <- paws.2011.12$SCHOOL_YEAR
paws.lookback.2yr <- equipercentile.lookback(act.2013.14[act.2013.14$SUBJECT %in% c("Math", 
                                                                                   "Reading", 
                                                                                   "Science"),], paws.2011.12)
paws.lookback.2yr$SCHOOL_YEAR_ORIGINAL <- paws.lookback.2yr$SCHOOL_YEAR
combos <- unique(paws.lookback.2yr[c("SUBJECT","GRADE_ENROLLED")])
paws.non.proficient.lookback.2yr <- do.call(rbind,
                                       lapply(1:nrow(combos),
                                              function (i) {
                                                data.frame(SUBJECT=combos[i,"SUBJECT"],
                                                           GRADE_ENROLLED=combos[i,"GRADE_ENROLLED"],
                                                           unmatrixfy.df(calc.mean.score(paws.lookback.2yr[paws.lookback.2yr$SUBJECT==combos[i,"SUBJECT"] &
                                                                                                             paws.lookback.2yr$GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],],                               
                                                                                         score.prefix="PERFORMANCE_LEVEL",
                                                                                         agg.function=function (g) c(N_TESTS=length(g),
                                                                                                                     N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                                                                     PERCENT_NOT_PROFICIENT=round((sum(ifelse(g %in% c('1','2'), 1, 0))/length(g))*100, 2)),
                                                                                         already.long=TRUE), prepend=FALSE))
                                                
                                              }))

statewide.paws.non.proficient.lookback.2yr <- paws.non.proficient.lookback.2yr[paws.non.proficient.lookback.2yr$SCHOOL_ID==state.school.id,]
paws.lookback.2yr$SUBJECT_CODE <- sapply(paws.lookback.2yr$SUBJECT,
                                         function (s) 
                                           switch(s, Math="MA",Reading="RE",Science="SC",NA), USE.NAMES=FALSE)
paws.lookback.2yr <- paws.lookback.2yr[,!(names(paws.lookback.2yr) %in% c("SCHOOL_YEAR_ORIGINAL"))]
save(paws.lookback.2yr, file="data/ACT/paws.lookback.2yr.Rdata")
