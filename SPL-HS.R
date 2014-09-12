
hs.target.level.labels <- c(achievement="HS_ACHIEVEMENT_TARGET_LEVEL",
                            equity="HS_EQUITY_TARGET_LEVEL",
                            readiness="HS_ADD_READINESS_CAT",                            
                            grad.rate="IMPROVE_CAT_2013")

hs.achievement.target.level.labels <- hs.target.level.labels[1:2]
hs.readiness.target.level.labels <- hs.target.level.labels[3:4]

hs.overall.target.level.labels <- c(readiness="HS_OVERALL_READINESS",
                                    achievement="HS_OVERALL_ACHIEVEMENT")

hs.participation.labels <- c(achievement="HS_ACHIEVEMENT_PARTICIPATION_RATE",
                              tested.readiness="HS_TESTED_READINESS_PARTICIPATION_RATE",
                             equity="HS_EQUITY_PARTICIPATION_RATE"
                             )

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

schools$HS_SPL <- apply(schools[,c("WAEA_SCHOOL_TYPE", 
                                   hs.overall.target.level.labels)],
                        c(1),
                        function (school) {
                          
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
                          
                        })


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


schools$SPL_ACCOUNTABILITY <- apply(schools[,c("G38_SPL_ACCOUNTABILITY", "HS_SPL_ACCOUNTABILITY")],
                                    c(1),
                                    function (scores) scores[order(scores)][1])

results.files <- dir("results")[grep("schools-2013-14-with-indicators-[0-9]+\\.csv", dir("results"))]
results.files <- results.files[order(results.files, decreasing=TRUE)]
result.file <- if (length(results.files) == 0) {
  "schools-2013-14-with-indicators-001.csv"  
  
}else {
  
  last.file <- results.files[1]
  prefix <- strsplit(strsplit(last.file, ".", fixed=TRUE)[[1]][1], "-", fixed=TRUE)
  raw.index <- paste("00", as.numeric(prefix[[1]][length(prefix[[1]])]) + 1, sep="")
  index <- substr(raw.index, nchar(raw.index) - 2, nchar(raw.index))
  
  paste(do.call(paste, as.list(c(prefix[[1]][1:(length(prefix[[1]])-1)], index, sep="-"))), "csv", sep=".")
}

write.csv(schools[schools$SCHOOL_YEAR==current.school.year,], file=paste("results", result.file, sep="/"), na="", row.names=FALSE)
