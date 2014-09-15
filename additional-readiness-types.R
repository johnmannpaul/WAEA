table(schools[schools$SCHOOL_YEAR==current.school.year &
                schools$WAEA_SCHOOL_TYPE %in% HS.types,
              c("SMALL_SCHOOL_HATH_ELIGIBILITY")])

table(schools[schools$SCHOOL_YEAR==current.school.year &
                schools$WAEA_SCHOOL_TYPE %in% HS.types,
              c("SMALL_SCHOOL_HATH_ELIGIBILITY", "SMALL_SCHOOL_GRADE_NINE_CREDIT")])

      
table(schools[schools$SCHOOL_YEAR==current.school.year &
                schools$WAEA_SCHOOL_TYPE %in% HS.types,
              c("SMALL_SCHOOL_HATH_ELIGIBILITY", 
                "SMALL_SCHOOL_GRADE_NINE_CREDIT", 
                "HS_TESTED_READINESS_SMALL_SCHOOL")], useNA="ifany")


schools[schools$SCHOOL_YEAR==current.school.year &
                schools$WAEA_SCHOOL_TYPE %in% HS.types &
                is.na(schools$SMALL_SCHOOL_GRADE_NINE_CREDIT),]

compute.add.readiness.type <- function (school) {
  waea.type <- as.numeric(school[["WAEA_SCHOOL_TYPE"]])
  school.id <- school[["SCHOOL_ID"]]
  if (!(waea.type %in% HS.types) || school.id == state.school.id)
    return(c(NA,NA))
  
  ss.hathaway <- school[["SMALL_SCHOOL_HATH_ELIGIBILITY"]]
  ss.grade.nine <- school[["SMALL_SCHOOL_GRADE_NINE_CREDIT"]]
  years.back.tested.readiness <- as.numeric(school[["HS_TESTED_READINESS_YEARS_BACK"]])
  ss.tested.readiness <- if (!is.na(years.back.tested.readiness) &
                               years.back.tested.readiness == Inf) 
    'T'
  else 
    'F'
  
  ss.hathaway <- if (is.na(ss.hathaway)) 'T' else ss.hathaway
  ss.grade.nine <- if (is.na(ss.grade.nine)) 'T' else ss.grade.nine
  
  ss <- c(ss.hathaway, 
          ss.grade.nine, 
          ss.tested.readiness)
  
  ss.sum <- length(which(ss == 'F'))
  
  #type 0: (none) missing tested readiness, grade nine credits, and hathaway eligibility
  #type 1: (all) tested readiness, grade nine credits, and hathaway eligibility all present
  #type 2: (tested only) tested readiness only
  #type 3: (tested and gd9 only) tested readiness and grade 9 credits only
  #type 4: (tested and Hath only) tested readiness and hathaway eligibility only
  switch(ss.sum + 1,
         c(0,"none"),
         c(2, "tested only"),
         if (ss.grade.nine == 'T')
           c(4,"tested and Hath only")
         else
           c(3,"tested and gd9 only"),
         c(1,"all")
         )
}


schools[,c("HS_ADD_READINESS_TYPE","HS_ADD_READINESS_TYPE_LABEL")] <- data.frame(t(apply(schools[,c("SCHOOL_ID","WAEA_SCHOOL_TYPE",
                                                                                     "SMALL_SCHOOL_HATH_ELIGIBILITY", 
                                                                                     "SMALL_SCHOOL_GRADE_NINE_CREDIT", 
                                                                                     "HS_TESTED_READINESS_YEARS_BACK")],
                                    c(1),
                                    compute.add.readiness.type)))

table(schools[,c("SCHOOL_YEAR","HS_ADD_READINESS_TYPE")])
table(schools[,c("SCHOOL_YEAR","HS_ADD_READINESS_TYPE_LABEL")])


compute.add.readiness <- function (school, 
                                   weights, 
                                   precision,
                                   result.labels=c("HS_ADD_READINESS_SCORE", "HS_ADD_READINESS_N"),
                                   score.labels=c("HS_TESTED_READINESS_MEAN",
                                                  "PERCENT_GD_9_CREDIT_MET",
                                                  "HATH_INDEX_SCORE_MEAN"),
                                   N.labels=c("HS_TESTED_READINESS_N",
                                              "GRADE_NINE_CREDITS_MET_N",
                                              "HATH_INDEX_SCORE_N"),
                                   school.labels = c("SCHOOL_YEAR","SCHOOL_ID")) {
  labels <- school[school.labels]
  scores <- as.numeric(school[score.labels])
  Ns <- as.numeric(school[N.labels])
  add.readiness.score <- round(sum(sapply(seq(1:length(scores)),
                                    function  (i)
                                      weights[i] * scores[i])),
                               precision)
  add.readiness.N <- min(Ns)
  
  result <- c(labels, add.readiness.score, add.readiness.N)
  names(result) <- c(school.labels, result.labels)
  result
}

type.1.additional.readiness <- data.frame(t(apply(schools[schools$SCHOOL_YEAR==current.school.year &
                                                            schools$WAEA_SCHOOL_TYPE %in% HS.types &
                                                            schools$SCHOOL_ID != state.school.id &
                                                            schools$HS_ADD_READINESS_TYPE_LABEL=='all',c("SCHOOL_YEAR", 
                                                                                                      "SCHOOL_ID",
                                                                                                      "HS_TESTED_READINESS_MEAN",
                                                                                                      "PERCENT_GD_9_CREDIT_MET",
                                                                                                      "HATH_INDEX_SCORE_MEAN",
                                                                                                      "HS_TESTED_READINESS_N",
                                                                                                      "GRADE_NINE_CREDITS_MET_N",
                                                                                                      "HATH_INDEX_SCORE_N")],
                                                  c(1),
                                                  compute.add.readiness, additional.readiness.weights,
                                                  precision.add.readiness, 
                                                  result.labels=c("HS_ADD_READINESS_SCORE_TYPE1", "HS_ADD_READINESS_N_TYPE1"))))
                                          
table(type.1.additional.readiness$HS_ADD_READINESS_SCORE_TYPE1)
schools <- bind.indicator(schools, 
                          type.1.additional.readiness,
                          indicator.labels.min.N = c(N= "HS_ADD_READINESS_N_TYPE1", score="HS_ADD_READINESS_SCORE_TYPE1"),
                          min.N.readiness.hs)

quantile(schools[schools$SCHOOL_YEAR==current.school.year &
                   schools$WAEA_SCHOOL_TYPE %in% HS.types &
                   schools$SCHOOL_ID != state.school.id &
                   schools$HS_ADD_READINESS_TYPE_LABEL=='all',"HS_ADD_READINESS_SCORE_TYPE1"],
         probs=c(.30, .70))




type.1.additional.readiness$HS_ADD_READINESS_CAT_TYPE1 <- findInterval(type.1.additional.readiness$HS_ADD_READINESS_SCORE_TYPE1, 
                                                                       type.1.additional.readiness.cuts) + 1
                                                        

prop.table(table(type.1.additional.readiness$HS_ADD_READINESS_CAT_TYPE1))



schools <- bind.indicator(schools, 
                          type.1.additional.readiness,
                          indicator.labels.min.N = c(N= "HS_ADD_READINESS_N_TYPE1", score="HS_ADD_READINESS_CAT_TYPE1", "HS_ADD_READINESS_SCORE_TYPE1"),
                          min.N.readiness.hs)


head(schools[schools$SCHOOL_YEAR==current.school.year &
               schools$WAEA_SCHOOL_TYPE %in% HS.types &
               schools$SCHOOL_ID != state.school.id &
               schools$HS_ADD_READINESS_TYPE_LABEL=='all',])


head(schools[schools$SCHOOL_YEAR==current.school.year &
               schools$WAEA_SCHOOL_TYPE %in% HS.types &
               schools$SCHOOL_ID != state.school.id,])


#examine the type 1 proportions resulting from type 1 cuts
type.1.percentages <- cumsum(round(prop.table(table(type.1.additional.readiness$HS_ADD_READINESS_CAT_TYPE1)),2))[1:2]

#we'll use these percentages to set the cuts for types 2,3, and 4
compute.add.readiness.cuts <- function (scores, percentages) {
  
  cuts <- round(percentages * length(scores), 0) + 1
  ordered.scores <- scores[order(scores)]
  ordered.scores[cuts]
  
}


#type 2


compute.add.readiness.for.other.types <- function (schools, type.label, type.suffix, weights, cut.percentages, score.labels, N.labels, min.N=min.N.readiness.hs) {
  
  result.labels<-sapply(c("HS_ADD_READINESS_SCORE", "HS_ADD_READINESS_N"), paste, type.suffix, sep="_")
  names(result.labels) <- c("score", "N")
  
  
  typed.additional.readiness <- data.frame(t(apply(schools[schools$SCHOOL_YEAR==current.school.year &
                                                             schools$WAEA_SCHOOL_TYPE %in% HS.types &
                                                             schools$SCHOOL_ID != state.school.id &
                                                             schools$HS_ADD_READINESS_TYPE_LABEL %in% c('all',type.label),
                                                            c("SCHOOL_YEAR", 
                                                              "SCHOOL_ID",
                                                              score.labels,
                                                              N.labels)],
                                                    c(1),
                                                    compute.add.readiness, weights,
                                                    precision.add.readiness,
                                                    result.labels,
                                                    score.labels,
                                                    N.labels)))
  
  
  schools <- bind.indicator(schools, 
                            typed.additional.readiness,
                            indicator.labels.min.N = result.labels,
                            min.N)
  
  typed.additional.readiness.cuts <- compute.add.readiness.cuts(schools[schools$SCHOOL_YEAR==current.school.year &
                                                                          schools$WAEA_SCHOOL_TYPE %in% HS.types &
                                                                          schools$SCHOOL_ID != state.school.id &
                                                                          schools$HS_ADD_READINESS_TYPE_LABEL %in% c('all',type.label),
                                                                         result.labels[["score"]]],
                                                                 cut.percentages)
  
  cat.label <- paste("HS_ADD_READINESS_CAT", type.suffix, sep="_")
  cut.labels <- sapply(c("HS_ADD_READINESS_CUT1", "HS_ADD_READINESS_CUT2"), paste, type.suffix, sep="_")
  
  typed.additional.readiness[, c(cat.label, cut.labels)] <- t(sapply(typed.additional.readiness[[result.labels[["score"]]]],
                                                                function (s) {
                                                                  
                                                                  c(findInterval(s, typed.additional.readiness.cuts) + 1,
                                                                    typed.additional.readiness.cuts[1],
                                                                    typed.additional.readiness.cuts[2])
                                                                }))
  bind.indicator(schools, 
                 typed.additional.readiness[, c("SCHOOL_YEAR", "SCHOOL_ID", cat.label, result.labels[["N"]], cut.labels)],
                 indicator.labels.min.N = c(N= result.labels[["N"]], score=cat.label),
                 min.N)
  
}

#type 2
schools <- compute.add.readiness.for.other.types(schools, 
                                         "tested only", 
                                         "TYPE2",
                                         1,
                                         type.1.percentages, 
                                         "HS_TESTED_READINESS_MEAN", 
                                         "HS_TESTED_READINESS_N")

schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% HS.types &
          schools$SCHOOL_ID != state.school.id &
          schools$HS_ADD_READINESS_TYPE_LABEL %in% c('tested only'),]

#type3
schools <- compute.add.readiness.for.other.types(schools, 
                                                 "tested and gd9 only", 
                                                 "TYPE3", 
                                                 c(.7,.3),
                                                 type.1.percentages, 
                                                 c("HS_TESTED_READINESS_MEAN", "PERCENT_GD_9_CREDIT_MET"), 
                                                 c("HS_TESTED_READINESS_N", "GRADE_NINE_CREDITS_MET_N"))


schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% HS.types &
          schools$SCHOOL_ID != state.school.id &
          schools$HS_ADD_READINESS_TYPE_LABEL %in% c("tested and gd9 only"),]


#type4
schools <- compute.add.readiness.for.other.types(schools, 
                                                 "tested and Hath only", 
                                                 "TYPE4", 
                                                 c(.41,.59),
                                                 type.1.percentages, 
                                                 c("HS_TESTED_READINESS_MEAN", "HATH_INDEX_SCORE_MEAN"), 
                                                 c("HS_TESTED_READINESS_N", "HATH_INDEX_SCORE_N"))

schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% HS.types &
          schools$SCHOOL_ID != state.school.id &
          schools$HS_ADD_READINESS_TYPE_LABEL %in% c("tested and Hath only"),]



#assign an overall category
schools$HS_ADD_READINESS_CAT <- apply(schools[,c("SCHOOL_YEAR", "SCHOOL_ID","HS_ADD_READINESS_TYPE")],
                                   c(1),
                                   function (school) {
                                     type <- school[["HS_ADD_READINESS_TYPE"]]
                                     if (is.na(type) | type==0)
                                       NA
                                     else
                                       schools[schools$SCHOOL_YEAR == school[["SCHOOL_YEAR"]] &
                                                 schools$SCHOOL_ID == school[["SCHOOL_ID"]],
                                               paste("HS_ADD_READINESS_CAT_TYPE", type, sep="")]
                                     
                                   })


#look at the frequencies again
schools.of.interest <- schools[schools$SCHOOL_YEAR==current.school.year &
                                 schools$WAEA_SCHOOL_TYPE %in% HS.types &
                                 schools$SCHOOL_ID != state.school.id ,]


head(schools.of.interest[,grep("NESS_CAT|_TYPE_LABEL",names(schools.of.interest))])

table(schools[schools$SCHOOL_YEAR==current.school.year, "HS_ADD_READINESS_CAT_TYPE1"])
table(schools[schools$SCHOOL_YEAR==current.school.year, "HS_ADD_READINESS_SCORE_TYPE1"])
type.1.additional.readiness.cuts


table(schools[schools$SCHOOL_YEAR==current.school.year, "HS_ADD_READINESS_CAT_TYPE2"])
table(schools[schools$SCHOOL_YEAR==current.school.year, "HS_ADD_READINESS_SCORE_TYPE2"])
table(schools[schools$SCHOOL_YEAR==current.school.year, "HS_ADD_READINESS_CAT_TYPE3"])
table(schools[schools$SCHOOL_YEAR==current.school.year, "HS_ADD_READINESS_SCORE_TYPE3"])

#ordering by type3 score
#schools.of.interest[, grep("TYPE3", names(schools))][order(schools.of.interest$HS_ADD_READINESS_SCORE_TYPE3),]

table(schools[schools$SCHOOL_YEAR==current.school.year, "HS_ADD_READINESS_CAT_TYPE4"])




##check deltas in categories for the 'all' type
#  table(abs(schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% c('all'),
#          "HS_ADD_READINESS_CAT_TYPE1"] -
#            schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% 'all',
#            "HS_ADD_READINESS_CAT_TYPE2"]))
#          
# table(abs(schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% c('all'),
#                               "HS_ADD_READINESS_CAT_TYPE1"] -
#             schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% 'all',
#                                 "HS_ADD_READINESS_CAT_TYPE3"]))
# 
# 
# table(abs(schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% c('all'),
#                               "HS_ADD_READINESS_CAT_TYPE1"] -
#             schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% 'all',
#                                 "HS_ADD_READINESS_CAT_TYPE4"]))


table(schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% c('all'),
                              "HS_ADD_READINESS_CAT_TYPE1"] -
            schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% 'all',
                                "HS_ADD_READINESS_CAT_TYPE2"])

table(schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% c('all'),
                              "HS_ADD_READINESS_CAT_TYPE1"] -
            schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% 'all',
                                "HS_ADD_READINESS_CAT_TYPE3"])


table(schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% c('all'),
                              "HS_ADD_READINESS_CAT_TYPE1"] -
            schools.of.interest[schools.of.interest$HS_ADD_READINESS_TYPE_LABEL %in% 'all',
                                "HS_ADD_READINESS_CAT_TYPE4"])



table(schools.of.interest[,c("SCHOOL_YEAR","HS_ADD_READINESS_CAT")])
prop.table(table(schools.of.interest[,c("SCHOOL_YEAR","HS_ADD_READINESS_CAT")]))
prop.table(table(schools.of.interest[,c("SCHOOL_YEAR","HS_ADD_READINESS_CAT_TYPE1")]))
##compare tested readiness category to 4yr graduation rate
schools[schools$SCHOOL_YEAR==current.school.year &
          schools$WAEA_SCHOOL_TYPE %in% HS.types &
          schools$SCHOOL_ID != state.school.id &
          schools$HS_ADD_READINESS_TYPE_LABEL %in% c('tested only'),c("HS_ADD_READINESS_CAT",
                                                                   "GRAD_RATE_4_YR.2012.13")]


write.csv(file=get.filename("additional-readiness-cfds", "results/cfds"), 
          schools[schools$WAEA_SCHOOL_TYPE %in% HS.types &
                    schools$SCHOOL_YEAR==current.school.year &
                    schools$HS_ADD_READINESS_TYPE == 1 &
                    schools$HS_ADD_READINESS_N_TYPE1 >= 10 &
                    schools$SCHOOL_ID != state.school.id,
                  c("SCHOOL_YEAR", "SCHOOL_ID", "ALTERNATIVE_SCHOOL", "HS_ADD_READINESS_N_TYPE1", "HS_ADD_READINESS_SCORE_TYPE1")],
          na="",
          row.names=FALSE)

# write.csv(file="results/alternative-school-add-readiness-cat-cross-tab.csv",
#           table(schools.of.interest[,c("ALTERNATIVE_SCHOOL","HS_ADD_READINESS_CAT")]),
#           na="")
#checks
