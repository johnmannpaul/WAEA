load("data/g38.achieve.Rdata")

table(g38.achieve$TEST_TYPE)
g38.paws <- g38.achieve[g38.achieve$TEST_TYPE=="PAWS Standard" & 
                          g38.achieve$TESTING_STATUS_CODE=='T',]

table(g38.paws$TEST_TYPE)
table(g38.paws$SCHOOL_YEAR)
head(g38.paws)

table(g38.paws[c("SUBJECT","GRADE_ENROLLED")])

combos <- unique(g38.paws[c("SUBJECT","GRADE_ENROLLED")])

g38.paws$SCHOOL_YEAR_ORIGINAL <- g38.paws$SCHOOL_YEAR

percent.non.proficient <- do.call(rbind,
        lapply(1:nrow(combos),
               function (i) {
                 data.frame(SUBJECT=combos[i,"SUBJECT"],
                            GRADE_ENROLLED=combos[i,"GRADE_ENROLLED"],
                            unmatrixfy.df(calc.mean.score(g38.paws[g38.paws$SUBJECT==combos[i,"SUBJECT"] &
                                                                    g38.paws$GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],],                               
                                                          score.prefix="PERFORMANCE_LEVEL",
                                                          agg.function=function (g) c(N_TESTS=length(g),
                                                                                      N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                                      PERCENT_NOT_PROFICIENT=round((sum(ifelse(g %in% c('1','2'), 1, 0))/length(g))*100, 2)),
                                                          already.long=TRUE), prepend=FALSE))
                            
               }))




load(file="data/paws.Rdata")

math.07 <- as.numeric(paws[paws$SCHOOL_YEAR=='2012-13' &
                             paws$GRADE_ENROLLED=='07'&
                             paws$SUBJECT_CODE=='MA' &
                             paws$TEST_TYPE=='PAWS Standard' &
                             paws$TESTING_STATUS_CODE == 'T',"STANDARD_PAWS_SCALE_SCORE"])
quantile(math.07, probs=c(.5705))
prop.table(table(ifelse(math.07 <= 729, 1, 0)))


cut <- round(.5705 * length(math.07), 0) + 1
math.07.ordered <- math.07[order(math.07)]
math.07.ordered[cut]


paws.2012.13 <- paws[paws$SCHOOL_YEAR=='2012-13' &                             
                       paws$TEST_TYPE=='PAWS Standard' &
                       paws$TESTING_STATUS_CODE == 'T',]

table(paws.2012.13$SUBJECT_CODE)
paws.2012.13$SUBJECT <- sapply(paws.2012.13$SUBJECT_CODE, function (c) switch(c, MA="Math", RE="Reading", SC="Science", NA))
table(paws.2012.13$SUBJECT)
paws.2012.13$SCHOOL_YEAR_ORIGINAL <- paws.2012.13$SCHOOL_YEAR

statewide.percents.proficient <- percent.non.proficient[percent.non.proficient$SCHOOL_ID==state.school.id,]
do.cuts <- function (i) {
  
  agg.function <- function (g) {
    c(N_TESTS=length(g),                                     
      CUT=round(quantile(g,with(statewide.percents.proficient,
                                statewide.percents.proficient[SUBJECT==combos[i,"SUBJECT"] &
                                                                GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],
                                                              "PERCENT_NOT_PROFICIENT"]/100),0)))
  }
  
  result <- data.frame(SUBJECT=combos[i,"SUBJECT"],
             GRADE_ENROLLED=combos[i,"GRADE_ENROLLED"],
             unmatrixfy.df(calc.mean.score(paws.2012.13[paws.2012.13$SUBJECT==combos[i,"SUBJECT"] &
                                                          paws.2012.13$GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],],                               
                                           score.prefix="STANDARD_PAWS_SCALE_SCORE",
                                           agg.function=agg.function,
                                           already.long=TRUE), prepend=FALSE))
  names(result)[6] <-"CUT"
  result
}

cuts.2012.13 <- do.call(rbind,
                        lapply(1:nrow(combos), do.cuts))

head(cuts.2012.13)

cuts.statewide <- cuts.2012.13[cuts.2012.13$SCHOOL_ID==state.school.id,]

table(paws$SUBJECT_CODE)
paws$SUBJECT <- sapply(paws$SUBJECT_CODE, function (c) switch(c, MA="Math", RE="Reading", SC="Science", NA))
table(paws$SUBJECT)

paws$TEST_TYPE <- ifelse(paws$TEST_TYPE == "PAWS-STANDARD",
                         "PAWS Standard",
                         paws$TEST_TYPE)

paws.lookback <- paws[paws$TEST_TYPE=='PAWS Standard' &
                        paws$TESTING_STATUS_CODE == 'T' &
                        paws$SCHOOL_YEAR >= '2011-12',]

table(paws.lookback$SCHOOL_YEAR)

paws.lookback.perf.level <- apply(paws.lookback[,c("SUBJECT", "GRADE_ENROLLED", "STANDARD_PAWS_SCALE_SCORE")],
                                  c(1),
                                  function (row) {
                                    cut <- cuts.statewide[cuts.statewide$SUBJECT == row[["SUBJECT"]] &
                                                            cuts.statewide$GRADE_ENROLLED == row[["GRADE_ENROLLED"]], "CUT"]
                                    
                                    if (as.numeric(row[["STANDARD_PAWS_SCALE_SCORE"]]) > cut)
                                      "3"
                                    else
                                      "2"
                                    
                                  })

g38.achieve.lookback <- data.frame(paws.lookback[c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                                   SNAPSHOT=NA,
                                   SUBJECT_CODE=paws.lookback$SUBJECT_CODE,
                                   paws.lookback[c("SUBJECT","SCHOOL_FULL_ACADEMIC_YEAR",
                                                 "GRADE_ENROLLED","TEST_TYPE", "TESTING_STATUS_CODE")],
                                   PERFORMANCE_LEVEL=paws.lookback.perf.level,
                                   SCALE_SCORE=paws.lookback$STANDARD_PAWS_SCALE_SCORE,
                                   WY_ACT_SCALE_SCORE=NA
                                   )

nrow(paws.lookback)
nrow(g38.achieve.lookback)
g38.achieve.lookback$SCHOOL_YEAR_ORIGINAL <- g38.achieve.lookback$SCHOOL_YEAR
head(g38.achieve.lookback)



percent.non.proficient.lookback <- do.call(rbind,
                                  lapply(1:nrow(combos),
                                         function (i) {
                                           data.frame(SUBJECT=combos[i,"SUBJECT"],
                                                      GRADE_ENROLLED=combos[i,"GRADE_ENROLLED"],
                                                      unmatrixfy.df(calc.mean.score(g38.achieve.lookback[g38.achieve.lookback$SUBJECT==combos[i,"SUBJECT"] &
                                                                                                           g38.achieve.lookback$GRADE_ENROLLED==combos[i,"GRADE_ENROLLED"],],                               
                                                                                    score.prefix="PERFORMANCE_LEVEL",
                                                                                    agg.function=function (g) c(N_TESTS=length(g),
                                                                                                                N_PROFICIENT_TESTS=sum(ifelse(g %in% c('3','4'), 1, 0)),
                                                                                                                PERCENT_NOT_PROFICIENT=round((sum(ifelse(g %in% c('1','2'), 1, 0))/length(g))*100, 2)),
                                                                                    already.long=TRUE), prepend=FALSE))
                                           
                                         }))
statewide.percents.nonproficient.lookback <- percent.non.proficient.lookback[percent.non.proficient.lookback$SCHOOL_ID==state.school.id,]

statewide.percents.nonproficient.lookback
statewide.percents.proficient

cuts.statewide

g38.achieve.lookback <- g38.achieve.lookback[,names(g38.achieve.lookback) != "SCHOOL_YEAR_ORIGINAL"]
save(g38.achieve.lookback, file="data/g38.achieve.lookback.Rdata")
