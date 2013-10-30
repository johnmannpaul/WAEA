library("reshape")
#how many schools with 3-8 performance do not have an indicator
#with(schools, table(schools[SCHOOL_YEAR=='2012-13' & WAEA_SCHOOL_TYPE != c(2),]$N_INDICATORS))
report.adjusted.SPLs = TRUE

update.results.nonHS <- function (school.year, adjusted=report.adjusted.SPLs) {

  #update global state
#   schools$SPL <<- calc.SPLs(schools, "nonHS")
#   state.school$SPL <<- calc.SPLs(state.school, "nonHS")

  schools <<- calc.SPLs(schools, "nonHS")
  state.school <<- calc.SPLs(state.school, "nonHS")
  
  
  #need to propagate results to paired schools
  schools <<- propagate.results.to.paired.schools(schools)
  
  #compute impact results
  #need to have missing values inserted.  That's whey we make them factors.
  sums.3.df = schools[schools$N_INDICATORS==3 & schools$SCHOOL_YEAR==school.year,c("EQUITY_TARGET_LEVEL", "GROWTH_TARGET_LEVEL", "ACHIEVEMENT_TARGET_LEVEL", "SCHOOL_ID" )]
  sums.3.df$EQUITY_TARGET_LEVEL <- factor(sums.3.df$EQUITY_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.3.df$GROWTH_TARGET_LEVEL <- factor(sums.3.df$GROWTH_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.3.df$ACHIEVEMENT_TARGET_LEVEL <- factor(sums.3.df$ACHIEVEMENT_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.3 <- cast(sums.3.df, value="SCHOOL_ID",
                 EQUITY_TARGET_LEVEL+GROWTH_TARGET_LEVEL~ACHIEVEMENT_TARGET_LEVEL, function (x) length(x), add.missing=TRUE)
  
  
  sums.2.df <- schools[schools$N_INDICATORS==2 & schools$SCHOOL_YEAR==school.year,c("GROWTH_TARGET_LEVEL", "ACHIEVEMENT_TARGET_LEVEL", "SCHOOL_ID" )]
  sums.2.df$GROWTH_TARGET_LEVEL <- factor(sums.2.df$GROWTH_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.2.df$ACHIEVEMENT_TARGET_LEVEL <- factor(sums.2.df$ACHIEVEMENT_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.2 <- cast(sums.2.df, value="SCHOOL_ID",
                 GROWTH_TARGET_LEVEL~ACHIEVEMENT_TARGET_LEVEL, function (x) length(x), add.missing=TRUE)
  
  #combine the 9x3 and 3x3 cell totals
  sums.schools <- rbind(as.matrix(sums.3),as.matrix(sums.2))
  
  
  #compute the impact totals 
  if (adjusted) {
    SPL.schools <- schools[schools$WAEA_SCHOOL_TYPE %in% c(nonHS.types, paired.types) & schools$SCHOOL_YEAR==school.year,c("N_INDICATORS", "SPL_ADJUSTED",  "SCHOOL_ID" )]
    SPL.schools$N_INDICATORS <- factor(as.character(SPL.schools$N_INDICATORS), levels=c("3","2","1","0"), ordered=TRUE)
    SPL.schools$SPL_ADJUSTED <- ifelse(!is.na(SPL.schools$SPL_ADJUSTED), SPL.schools$SPL_ADJUSTED, 0)
    SPL.schools$SPL_ADJUSTED <- factor(as.character(SPL.schools$SPL_ADJUSTED), levels=c("1","2","3","4","0"), ordered=TRUE)
    sums.SPL <- cast(SPL.schools, value = "SCHOOL_ID", SPL_ADJUSTED ~ N_INDICATORS, 
                     function(x) length(x), add.missing=TRUE, margins=TRUE)
    sums.types <- t(rbind(with(schools, table(schools[WAEA_SCHOOL_TYPE==1 & HIGH_GRADE <= 6 & SCHOOL_YEAR==current.school.year,]$SPL_ADJUSTED, useNA="always")),
                          with(schools, table(schools[((WAEA_SCHOOL_TYPE==1 & HIGH_GRADE > 6) | WAEA_SCHOOL_TYPE ==5) & 
                                                        SCHOOL_YEAR==current.school.year,]$SPL_ADJUSTED, useNA="always")),
                          with(schools, table(schools[(WAEA_SCHOOL_TYPE ==3 | WAEA_SCHOOL_TYPE ==4) & 
                                                        SCHOOL_YEAR==current.school.year,]$SPL_ADJUSTED, useNA="always"))))
    
  } else { #use non participation  adjusted SPLS
    SPL.schools <- schools[schools$WAEA_SCHOOL_TYPE %in% c(nonHS.types, paired.types) & schools$SCHOOL_YEAR==school.year,c("N_INDICATORS", "SPL",  "SCHOOL_ID" )]
    SPL.schools$N_INDICATORS <- factor(as.character(SPL.schools$N_INDICATORS), levels=c("3","2","1","0"), ordered=TRUE)
    SPL.schools$SPL <- ifelse(!is.na(SPL.schools$SPL), SPL.schools$SPL, 0)
    SPL.schools$SPL <- factor(as.character(SPL.schools$SPL), levels=c("1","2","3","4","0"), ordered=TRUE)
    sums.SPL <- cast(SPL.schools, value = "SCHOOL_ID", SPL ~ N_INDICATORS, 
                     function(x) length(x), add.missing=TRUE, margins=TRUE)
    sums.types <- t(rbind(with(schools, table(schools[WAEA_SCHOOL_TYPE==1 & HIGH_GRADE <= 6 & SCHOOL_YEAR==current.school.year,]$SPL, useNA="always")),
                          with(schools, table(schools[((WAEA_SCHOOL_TYPE==1 & HIGH_GRADE > 6) | WAEA_SCHOOL_TYPE ==5) & 
                                                        SCHOOL_YEAR==current.school.year,]$SPL, useNA="always")),
                          with(schools, table(schools[(WAEA_SCHOOL_TYPE ==3 | WAEA_SCHOOL_TYPE ==4) & 
                                                        SCHOOL_YEAR==current.school.year,]$SPL, useNA="always"))))
    
  }
  sums.SPL <- as.matrix(sums.SPL)
  
  
  #pad in order to combine the cell totals and impact totals
  sums.schools.padded <- cbind(sums.schools, do.call(cbind,  lapply(seq(ncol(sums.schools) + 1,
                                                                        ncol(sums.SPL)),
                                                                    function (i)
                                                                      rep(NA, nrow(sums.schools)))))
  
  sums.types.padded <- cbind(sums.types, do.call(cbind,  lapply(seq(ncol(sums.types) + 1,
                                                                      ncol(sums.SPL)),
                                                                  function (i)
                                                                    rep(NA, nrow(sums.types)))))
  rbind(sums.schools.padded, sums.SPL, sums.types.padded)          
  
}
  

update.results.HS <- function (school.year, adjusted=report.adjusted.SPLs) {
  
  #update global state
#   schools$SPL_HS <<- calc.SPLs(schools, "HS")
#   state.school$SPL_HS <<- calc.SPLs(state.school, "HS")

  schools <<- calc.SPLs(schools, "HS")
  state.school <<- calc.SPLs(state.school, "HS")
  
  
  #compute impact results
  sums.3.df <- schools[schools$N_INDICATORS_HS==3 & schools$SCHOOL_YEAR==school.year,c("EQUITY_TARGET_LEVEL_HS", "READINESS_TARGET_LEVEL", "ACHIEVEMENT_TARGET_LEVEL_HS", "SCHOOL_ID" )]
  sums.3.df$EQUITY_TARGET_LEVEL_HS <- factor(sums.3.df$EQUITY_TARGET_LEVEL_HS, levels=c("1","2","3"), ordered=TRUE)
  sums.3.df$READINESS_TARGET_LEVEL <- factor(sums.3.df$READINESS_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.3.df$ACHIEVEMENT_TARGET_LEVEL_HS <- factor(sums.3.df$ACHIEVEMENT_TARGET_LEVEL_HS, levels=c("1","2","3"), ordered=TRUE)
  sums.3 <- cast(sums.3.df, value="SCHOOL_ID",
                 EQUITY_TARGET_LEVEL_HS+READINESS_TARGET_LEVEL~ACHIEVEMENT_TARGET_LEVEL_HS, function (x) length(x), add.missing=TRUE)
  
  
  sums.2.df <- schools[schools$N_INDICATORS_HS==2 & schools$SCHOOL_YEAR==school.year,c("READINESS_TARGET_LEVEL", "ACHIEVEMENT_TARGET_LEVEL_HS", "SCHOOL_ID" )]
  sums.2.df$READINESS_TARGET_LEVEL <- factor(sums.2.df$READINESS_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.2.df$ACHIEVEMENT_TARGET_LEVEL_HS <- factor(sums.2.df$ACHIEVEMENT_TARGET_LEVEL_HS, levels=c("1","2","3"), ordered=TRUE)
  sums.2 <- cast(sums.2.df, value="SCHOOL_ID",
                 READINESS_TARGET_LEVEL~ACHIEVEMENT_TARGET_LEVEL_HS, function (x) length(x), add.missing=TRUE)
  
  #combine the 9x3 and 3x3 cell totals
  sums.schools <- rbind(as.matrix(sums.3),as.matrix(sums.2))
  
  
  #compute the impact totals   
  if (adjusted) {
    SPL.schools <- schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==school.year,c("N_INDICATORS_HS", "SPL_ADJUSTED_HS",  "SCHOOL_ID" )]
    SPL.schools$N_INDICATORS_HS <- factor(as.character(SPL.schools$N_INDICATORS_HS), levels=c("3","2","1","0"), ordered=TRUE)
    SPL.schools$SPL_ADJUSTED_HS <- ifelse(!is.na(SPL.schools$SPL_ADJUSTED_HS), SPL.schools$SPL_ADJUSTED_HS, 0)
    SPL.schools$SPL_ADJUSTED_HS <- factor(as.character(SPL.schools$SPL_ADJUSTED_HS), levels=c("1","2","3","4","0"), ordered=TRUE)
    sums.SPL <- cast(SPL.schools, value="SCHOOL_ID",
                     SPL_ADJUSTED_HS~N_INDICATORS_HS, function(x) length(x), add.missing=TRUE, margins=TRUE)
  } else {
    SPL.schools <- schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==school.year,c("N_INDICATORS_HS", "SPL_HS",  "SCHOOL_ID" )]
    SPL.schools$N_INDICATORS_HS <- factor(as.character(SPL.schools$N_INDICATORS_HS), levels=c("3","2","1","0"), ordered=TRUE)
    SPL.schools$SPL_HS <- ifelse(!is.na(SPL.schools$SPL_HS), SPL.schools$SPL_HS, 0)
    SPL.schools$SPL_HS <- factor(as.character(SPL.schools$SPL_HS), levels=c("1","2","3","4","0"), ordered=TRUE)
    sums.SPL <- cast(SPL.schools, value="SCHOOL_ID",
                     SPL_HS~N_INDICATORS_HS, function(x) length(x), add.missing=TRUE, margins=TRUE)
  }  
  
  sums.SPL <- as.matrix(sums.SPL)
  
  
  
  #pad in order to combine the cell totals and impact totals
  sums.schools.padded <- cbind(sums.schools, do.call(cbind,  lapply(seq(ncol(sums.schools) + 1,
                                                                        ncol(sums.SPL)),
                                                                    function (i)
                                                                      rep(NA, nrow(sums.schools)))))
  rbind(sums.schools.padded, sums.SPL)          
  
}

get.results.nonHS <- function(matrices.df, school.year=current.school.year) {
  
  matrix.3 <- data.frame(EQUITY_TARGET_LEVEL=as.vector(sapply(c(1,2,3), function (x) rep (x, 3))),
                           GROWTH_TARGET_LEVEL=as.vector(sapply(c(1,1,1), function (x) x:3)),
                           matrices.df[1:9,])
  
  names(matrix.3)[3:length(matrix.3)] <- c("1","2","3")
  matrix.3 <- melt(matrix.3, id=c("EQUITY_TARGET_LEVEL", "GROWTH_TARGET_LEVEL"))
  
  names(matrix.3)[3:length(matrix.3)] <- c("ACHIEVEMENT_TARGET_LEVEL", "SPL")
  matrix.3$ACHIEVEMENT_TARGET_LEVEL <- as.numeric(matrix.3$ACHIEVEMENT_TARGET_LEVEL)
  
  matrix.3 <- cast(matrix.3, ACHIEVEMENT_TARGET_LEVEL~GROWTH_TARGET_LEVEL~EQUITY_TARGET_LEVEL)
#   with(matrix.3, matrix.3[order(ACHIEVEMENT_TARGET_LEVEL, GROWTH_TARGET_LEVEL, EQUITY_TARGET_LEVEL), c("ACHIEVEMENT_TARGET_LEVEL", 
#                                                                                       "GROWTH_TARGET_LEVEL", 
#                                                                                       "EQUITY_TARGET_LEVEL",
#                                                                                       "SPL")])
#   
  
  
  matrix.2 <- data.frame(GROWTH_TARGET_LEVEL=1:3, matrices.df[10:12,])
  names(matrix.2)[2:length(matrix.2)] <- c("1","2","3")
  matrix.2 <- melt(matrix.2, id=c("GROWTH_TARGET_LEVEL"))
  names(matrix.2)[2:length(matrix.2)] <- c("ACHIEVEMENT_TARGET_LEVEL", "SPL")
  matrix.2$ACHIEVEMENT_TARGET_LEVEL <- as.numeric(matrix.2$ACHIEVEMENT_TARGET_LEVEL)
  
  matrix.2 <- cast(matrix.2, ACHIEVEMENT_TARGET_LEVEL~GROWTH_TARGET_LEVEL)
  
  SPL.lookup$nonHS <<- list(`3` = matrix.3,
                           `2` = as.matrix(matrix.2))
  
  update.results.nonHS(school.year)
    
}


get.results.HS <- function(matrices.df, school.year=current.school.year) {
  
  matrix.3 <- data.frame(EQUITY_TARGET_LEVEL_HS=as.vector(sapply(c(1,2,3), function (x) rep (x, 3))),
                         READINESS_TARGET_LEVEL=as.vector(sapply(c(1,1,1), function (x) x:3)),
                         matrices.df[1:9,])
  
  names(matrix.3)[3:length(matrix.3)] <- c("1","2","3")
  matrix.3 <- melt(matrix.3, id=c("EQUITY_TARGET_LEVEL_HS", "READINESS_TARGET_LEVEL"))
  
  names(matrix.3)[3:length(matrix.3)] <- c("ACHIEVEMENT_TARGET_LEVEL_HS", "SPL")
  matrix.3$ACHIEVEMENT_TARGET_LEVEL_HS <- as.numeric(matrix.3$ACHIEVEMENT_TARGET_LEVEL_HS)
  
  matrix.3 <- cast(matrix.3, ACHIEVEMENT_TARGET_LEVEL_HS~READINESS_TARGET_LEVEL~EQUITY_TARGET_LEVEL_HS)
  #   with(matrix.3, matrix.3[order(ACHIEVEMENT_TARGET_LEVEL_HS, READINESS_TARGET_LEVEL, EQUITY_TARGET_LEVEL_HS), c("ACHIEVEMENT_TARGET_LEVEL_HS", 
  #                                                                                       "READINESS_TARGET_LEVEL", 
  #                                                                                       "EQUITY_TARGET_LEVEL_HS",
  #                                                                                       "SPL")])
  #   
  
  
  matrix.2 <- data.frame(READINESS_TARGET_LEVEL=1:3, matrices.df[10:12,])
  names(matrix.2)[2:length(matrix.2)] <- c("1","2","3")
  matrix.2 <- melt(matrix.2, id=c("READINESS_TARGET_LEVEL"))
  names(matrix.2)[2:length(matrix.2)] <- c("ACHIEVEMENT_TARGET_LEVEL_HS", "SPL")
  matrix.2$ACHIEVEMENT_TARGET_LEVEL_HS <- as.numeric(matrix.2$ACHIEVEMENT_TARGET_LEVEL_HS)
  
  matrix.2 <- cast(matrix.2, ACHIEVEMENT_TARGET_LEVEL_HS~READINESS_TARGET_LEVEL)
  
  SPL.lookup$HS <<- list(`3` = matrix.3,
                            `2` = as.matrix(matrix.2))
  
  update.results.HS(school.year)
  
}


get.results.all <- function (school.year=current.school.year) {
  
  
  SPL.schools <- schools[schools$SCHOOL_YEAR==school.year,c("ACCOUNTABILITY_N_INDICATORS", "ACCOUNTABILITY_SPL",  "SCHOOL_ID" )]
  SPL.schools$ACCOUNTABILITY_N_INDICATORS <- factor(as.character(SPL.schools$ACCOUNTABILITY_N_INDICATORS), levels=c("3","2","1","0"), ordered=TRUE)
  SPL.schools$ACCOUNTABILITY_SPL <- ifelse(!is.na(SPL.schools$ACCOUNTABILITY_SPL), SPL.schools$ACCOUNTABILITY_SPL, 0)
  SPL.schools$ACCOUNTABILITY_SPL <- factor(as.character(SPL.schools$ACCOUNTABILITY_SPL), levels=c("1","2","3","4","0"), ordered=TRUE)
  sums.SPL <- cast(SPL.schools, value="SCHOOL_ID",
                   ACCOUNTABILITY_SPL~ACCOUNTABILITY_N_INDICATORS, function(x) length(x), add.missing=TRUE, margins=TRUE)
  as.matrix(sums.SPL)
  
}


put.cuts.nonHS <- function (cuts.df, school.year=current.school.year) {  
  
  cuts.df <- as.matrix(cuts.df)
  
  achievement.level.lookup <<- list(as.vector(cuts.df[1,]),
                                    as.vector(cuts.df[2,]))
  
  growth.level.lookup <<- as.vector(cuts.df[3,])
  
  equity.level.lookup <<- as.vector(cuts.df[4,])
  
  #recalculate indicators
  schools <<- calc.school.achievement(schools)
  schools <<- calc.school.growth(schools)
  schools <<- calc.school.equity(schools)
  
  #also for the state school
  state.school <<- calc.school.achievement(state.school)
  state.school <<- calc.school.growth(state.school)
  state.school <<- calc.school.equity(state.school)
  
  update.results.nonHS(school.year)
  
}


put.cuts.HS <- function (cuts.df, school.year=current.school.year) {  
  
  cuts.df <- as.matrix(cuts.df)
  
  act.achievement.level.lookup <<- as.vector(cuts.df[1,])
  
  total.readiness.level.lookup <<- as.vector(cuts.df[2,])
  
  
  #recalculate indicators
  schools <<- calc.school.achievement.hs(schools)  
  schools <<- calc.school.readiness.hs(schools)
  
  #also for the state school
  state.school <<- calc.school.achievement.hs(state.school)  
  state.school <<- calc.school.readiness.hs(state.school)
  
  update.results.HS(school.year)
  
}


get.state.nonHS <- function (df, school.year=current.school.year) {
  
  state.values <- with(state.school, state.school[SCHOOL_YEAR==school.year,c("PERCENT_PROFICIENT_BAND_1", "PERCENT_PROFICIENT_BAND_2", "PERCENT_PROFICIENT", "MGP", 
                                                               "PERCENT_MEETING_AGP")])
  
  state.levels <- with(state.school, state.school[SCHOOL_YEAR==school.year,c("ACHIEVEMENT_TARGET_LEVEL", "GROWTH_TARGET_LEVEL", 
                                                               "EQUITY_TARGET_LEVEL", "SPL")])
  state.levels.labeled = sapply(names(state.levels), function(n) {
    if (n ==  "SPL")
      SPL.labels[[state.levels[[n]]]]
    else
      indicator.labels[[state.levels[[n]]]]
    
  })
  
  as.matrix(cbind(c(state.values, NA), c(NA, NA, state.levels.labeled), c(NA, NA, state.levels)))
  
}

get.state.HS <- function (df, school.year=current.school.year) {
  
#   state.values <- with(state.school, state.school[SCHOOL_YEAR==school.year,c("PERCENT_PROFICIENT_HS", "TESTED_READINESS", "SCHOOL_GRADUATION_INDEX", 
#                                                                              "TOTAL_READINESS_HS", "PERCENT_NONPROFICIENT",
#                                                                              "IMPROVEMENT_SCORE")])

  state.values <- with(state.school, state.school[SCHOOL_YEAR==school.year,c("PERCENT_PROFICIENT_HS", 
                                                                             "TOTAL_READINESS_HS", "PERCENT_NONPROFICIENT_CATEGORY",
                                                                             "IMPROVEMENT_CATEGORY")])
  
  
  state.levels <- with(state.school, state.school[SCHOOL_YEAR==school.year,c("ACHIEVEMENT_TARGET_LEVEL_HS", "READINESS_TARGET_LEVEL", 
                                                                             "EQUITY_TARGET_LEVEL_HS", "SPL_HS")])
  state.levels.labeled = sapply(names(state.levels), function(n) {
    if (n ==  "SPL_HS")
      SPL.labels[[state.levels[[n]]]]
    else
      indicator.labels[[state.levels[[n]]]]
    
  })
  
  as.matrix(cbind(c(state.values, NA), 
                  c(state.levels.labeled[1:2], NA, state.levels.labeled[3:4]), 
                  c(state.levels[1:2], NA, state.levels[3:4])))
  
}



put.indeces.HS <- function(df, school.year=current.school.year) {
  
  #set the indexes
  explore.index.range <<- as.vector(as.matrix(df[1,1:length(explore.index.range)]))
  explore_index <<- calc.index(explore.index.runs, explore.index.range)
  
  plan.index.range <<- as.vector(as.matrix(df[2,1:length(plan.index.range)]))
  plan_index <<- calc.index(plan.index.runs, plan.index.range)
  
  act.index.range <<- as.vector(as.matrix(df[3,1:length(act.index.range)]))
  act_index  <<- calc.index(act.index.runs, act.index.range)
  
  alt.index.range <<- as.vector(as.matrix(df[4,1:length(alt.index.range)]))
  alt_index <<- calc.index(alt.index.runs, alt.index.range)
  
  grad.index.range <<- as.vector(as.matrix(df[5,1:length(grad.index.range)]))
  grad.index <<- calc.index(grad.index.runs, grad.index.range)
  
  #put the schools and state temporarily together for the duration of this calculation
  schools.and.state <- rbind(schools, state.school)
  
  #recompute tested readiness indexes
  tested.readiness.df <<- calc.indexed.readiness(schools.and.state[schools.and.state$WAEA_SCHOOL_TYPE %in% c(2,4,5),c("SCHOOL_YEAR","SCHOOL_ID")], 
                                                readiness.tested.students.df, 
                                                readiness.participation.schools[,c("SCHOOL_YEAR", "SCHOOL_ID", "N_TESTED", "PARTICIPATION_RATE")])
  
  
  #recompute tested readiness targets
  schools.and.state <- calc.school.tested.readiness.hs(schools.and.state)
  
  #recompute grad indexes
  grads.average.index <<- calc.indexed.grads(schools.and.state, grads.df)
  
  #reompute grad target
  schools.and.state <- calc.school.grad.index(schools.and.state)
  
  #finally recompute total readiness for all schools and state
  schools.and.state$TOTAL_READINESS_HS <- apply(schools.and.state[,names(hs.readiness.weights)], c(1),
                                    FUN=function (readiness.scores) {
                                      if (sum(is.na(readiness.scores)) == 0)
                                        round(hs.readiness.weights %*% as.numeric(readiness.scores), precision)
                                      else
                                        NA
                                    })
  
  
  schools.and.state <- calc.school.readiness.hs(schools.and.state)
  
  #assign schools and state to their respective global variables
  schools <<- schools.and.state[schools.and.state$SCHOOL_ID != state.school.id,]
  state.school <<- schools.and.state[schools.and.state$SCHOOL_ID == state.school.id,]
  
  
  update.results.HS(school.year)
}