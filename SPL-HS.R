schools$HS_ACHIEVEMENT_INDICATORS_N <- apply(schools[,hs.achievement.target.level.labels],
                                  c(1),                                                                                      
                                  FUN=function (indicators) {                                
                                    sum(!is.na(indicators))
                                  })
schools$HS_READINESS_INDICATORS_N <- apply(schools[,hs.readiness.target.level.labels],
                                             c(1),                                                                                      
                                             FUN=function (indicators) {                                
                                               sum(!is.na(indicators))
                                             })
schools$HS_INDICATORS_N <- schools$HS_ACHIEVEMENT_INDICATORS_N + schools$HS_READINESS_INDICATORS_N
#look at the distribution of indicator combinations for 3-8


hs.compose.indicators <- function (school, labels, lookup) {
  
  if (!(school[["WAEA_SCHOOL_TYPE"]] %in% HS.types))
    NA
  else {
    
    scores <- school[labels]
    scores <- scores[!is.na(scores)]
    
    if (length(scores) == 0)
      NA
    else {
      if (length(scores) == 1)
        scores
      else
        lookup[scores[1],
               scores[2]]
    }
  }
  
}


schools[,hs.overall.target.level.labels["readiness"]] <- apply(schools[,c("WAEA_SCHOOL_TYPE", 
                                                                          hs.readiness.target.level.labels)],
                                                               c(1),
                                                               FUN=hs.compose.indicators, hs.readiness.target.level.labels, SPL.lookup[["HS"]][["Readiness"]] )

schools[,hs.overall.target.level.labels["achievement"]] <- apply(schools[,c("WAEA_SCHOOL_TYPE", 
                                                 hs.achievement.target.level.labels)],
                                      c(1),
                                      FUN=hs.compose.indicators, hs.achievement.target.level.labels, SPL.lookup[["HS"]][["Achievement"]] )

calc.SPL.HS <- function (school) {
  
  if (!(school[["WAEA_SCHOOL_TYPE"]] %in% HS.types))
    NA
  else {
    
    scores <- school[hs.overall.target.level.labels]
    scores <- scores[!is.na(scores)]
    
    if (length(scores) < 2)
      NA
    else 
      SPL.lookup[["HS"]][["Overall"]][scores[1],
                                      scores[2]]
  }
  
}

schools$HS_SPL <- apply(schools[,c("WAEA_SCHOOL_TYPE", 
                                   hs.overall.target.level.labels)], c(1),
                        calc.SPL.HS)


table(schools[c("SCHOOL_YEAR", "HS_SPL")])


schools$HS_PARTICIPATION_RATE_CAT <- apply(schools[, c("WAEA_SCHOOL_TYPE", hs.participation.labels)],
                                      c(1),
                                      function (school) {
                                        
                                        if (!(school[["WAEA_SCHOOL_TYPE"]] %in% HS.types))
                                          NA
                                        else
                                          findInterval(min(school[hs.participation.labels], na.rm=TRUE), c(90,95)) + 1
                                      })
  

schools[schools$SCHOOL_YEAR==current.school.year & 
          schools$WAEA_SCHOOL_TYPE %in% HS.types &
          schools$HS_PARTICIPATION_RATE_CAT==1,c("SCHOOL_ID", "NAME", hs.participation.labels)]

table(schools[c("SCHOOL_YEAR", "HS_PARTICIPATION_RATE_CAT")])
table(apply(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types &
                      schools$SCHOOL_YEAR==current.school.year, hs.participation.labels],
            c(1),
            function (school)
              min(school, na.rm=TRUE)))


schools$HS_SPL_ACCOUNTABILITY <- apply(schools[,c("HS_SPL", hs.participation.labels)],
                                        c(1),
                                        FUN=calc.SPL.accountability, "HS_SPL",  hs.participation.labels)



table(schools[c("SCHOOL_YEAR", "HS_SPL_ACCOUNTABILITY")])


schools[!is.na(schools$HS_SPL) & schools$HS_SPL !=  schools$HS_SPL_ACCOUNTABILITY,]


schools$ALL_SPL <- apply(schools[,c("G38_SPL", "HS_SPL")],
                                    c(1),
                                    function (scores) scores[order(scores)][1])

schools$ALL_SPL_ACCOUNTABILITY <- apply(schools[,c("G38_SPL_ACCOUNTABILITY", "HS_SPL_ACCOUNTABILITY")],
                                    c(1),
                                    function (scores) scores[order(scores)][1])



write.csv(schools[schools$SCHOOL_YEAR==current.school.year,], 
          file=get.filename(paste("schools", current.school.year, "with-indicators", sep="-"), 
                            "results"), na="", row.names=FALSE)

# write.csv(schools[schools$SCHOOL_YEAR==current.school.year,], 
#           file=get.filename(paste("schools", current.school.year, "with-indicators-cuts2", sep="-"), 
#                             "results"), na="", row.names=FALSE)

# write.csv(schools[schools$SCHOOL_YEAR==current.school.year,original.labels], 
#           file=get.filename(paste("schools", current.school.year, "with-indicators-orig-format", sep="-"), 
#                             "results"), na="", row.names=FALSE)