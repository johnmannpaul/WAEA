library("reshape")
# compute.N.years <- function (school, min) {  
#   tested <- rev(as.numeric(school))
#   result <- sapply(1:length(tested),
#                    function (i) {
#                      
#                      meets.min.N <- which(sapply(i:length(tested),
#                                                  function (j) {
#                                                    N <-sum(tested[i:j])
#                                                    ifelse(N < min, FALSE, TRUE)                    
#                                                  }))
#                      if (length(meets.min.N) == 0)
#                        return (Inf)  #no matter how far you go back, you won't meet min N
#                      else
#                        return(meets.min.N[1] - 1) #you have to go back this many years to meet min N
#                    }) 
#   names(result) <- rev(names(school))
#   result
# }
increment.school.year <- function (school.year, increment=1) {
  
  do.call(paste, as.list(c(as.numeric(strsplit(school.year,'-')[[1]]) + increment, sep='-')))
}

compute.N.years <- function (school, min) { 
  school.years <- school[2:length(school)]
  tested <- rev(as.numeric(school.years))
  result <- sapply(1:length(tested),
                   function (i) {
                     
                     meets.min.N <- which(sapply(i:length(tested),
                                                 function (j) {
                                                   N <-sum(tested[i:j])
                                                   ifelse(N < min, FALSE, TRUE)                    
                                                 }))
                     if (length(meets.min.N) == 0)
                       return (Inf)  #no matter how far you go back, you won't meet min N
                     else
                       return(meets.min.N[1] - 1) #you have to go back this many years to meet min N
                   }) 
  names(result) <- rev(names(school.years))
  result
}


get.paws.years.back <- function (school, df=paws.df, years.back.label="YEARS_BACK") {
  
  school.year <- school[["SCHOOL_YEAR"]]
  id <- school[["SCHOOL_ID"]]
  year <- as.numeric(school[["YEAR"]])
  
  years.back <- as.numeric(school[[years.back.label]])
  
  scores <- lapply((year-years.back):(year-1),
                   
                   function (y) {
                     df.year <- with(df, df[SCHOOL_ID==id & YEAR == y,])
                     #save the original school year for each student in SCHOOL_YEAR_ORIGINAL 
                     df.year$SCHOOL_YEAR_ORIGINAL <- df.year$SCHOOL_YEAR
                     df.year$SCHOOL_YEAR <- rep(school.year,nrow(df.year))
                     df.year$WISER_ID <- sapply(df.year$WISER_ID,
                                                function (id) {
                                                  paste(id,y,sep='.')                                                   
                                                })
                     df.year
                     
                   })
  do.call(rbind, scores)
  
}



participation.level.lookup <- c(90, 95)

rate.participation <- function (rate) {
  if(is.na(rate)) 
    NA
  else {
    low.cut <- participation.level.lookup[1]
    high.cut <- participation.level.lookup[2]
    if (rate < low.cut) {
      1                                                
    } else {
      if (low.cut <= rate & rate < high.cut)
        2
      else
        3
    }                                                                                                
  }
}


# compute.years.back <- function (school) {
#   
#   id <- school[["SCHOOL_ID"]]
#   year <- school[["SCHOOL_YEAR"]]
#   
#   years.back.achievement <- go.back.achievement[id, year]
#   years.back.growth <- go.back.growth[id, year]
#   
#   max(years.back.achievement, years.back.growth)
#   
# }


compute.years.back <- function (school) {
  
  id <- school[["SCHOOL_ID"]]
  year <- school[["SCHOOL_YEAR"]]
  
  years.back.achievement <- go.back.achievement[go.back.achievement$SCHOOL_ID==id,][[year]] 
  years.back.growth <- go.back.growth[go.back.growth$SCHOOL_ID==id,][[year]] 
  
  max(years.back.achievement, years.back.growth)
  
}

compute.years.back.subgroup <- function (school) {
  
  id <- school[["SCHOOL_ID"]]
  year <- school[["SCHOOL_YEAR"]]
  
  years.back.achievement <- go.back.achievement[go.back.achievement$SCHOOL_ID==id,][[year]] 
  years.back.growth <- go.back.growth[go.back.growth$SCHOOL_ID==id,][[year]] 
  years.back.subgroup <- go.back.subgroup[go.back.subgroup$SCHOOL_ID==id,][[year]] 
  
  years.back.all.three <- max(years.back.achievement, years.back.growth, years.back.subgroup)
  
  if (years.back.all.three == Inf) {
    #if we can't get all three then we'll see if we can get two
    years.back.only.two <- max(years.back.achievement, years.back.growth)
    #if we don't have to go back any years (i.e. 0 years) to make two indicators then we'll report the number 
    #of years needed to go back to make three (i.e. Inf at this juncture), otherwise we'll report the positive 
    #number of years needed to go back to make the two indicators
    ifelse(years.back.only.two == 0, Inf, years.back.only.two) 
  }
  else
    years.back.all.three
}


##functions
calc_type_1_target_level <- function (school, labels, min.N=min.N.achievement) {
  school_achievement <- achievement[achievement$SCHOOL_ID == school[["SCHOOL_ID"]] &
                                      achievement$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],c("GRADE_BAND","PERCENT_PROFICIENT", "ACCOUNTABILITY_N")]  
  
  if (nrow(school_achievement) == 0)
    return(rep(as.numeric(NA), length(labels)))
  
  low.cut <- achievement.level.lookup[[school_achievement[["GRADE_BAND"]]]][1]
  high.cut <- achievement.level.lookup[[school_achievement[["GRADE_BAND"]]]][2]
  n.acountability <- school_achievement[["ACCOUNTABILITY_N"]]
  if (n.acountability < min.N) {
    result <- rep(as.numeric(NA), length(labels))
    result[which(names(labels) == "N")] <- n.acountability  #no need to calculate anything if N is too small
    result[which(names(labels) == "cut.1")] <- low.cut
    result[which(names(labels) == "cut.2")] <- high.cut
    return(result)
  }
  
  
  pp <- school_achievement[["PERCENT_PROFICIENT"]]
  
  if (pp < low.cut)
    c(low.cut, high.cut, pp, n.acountability, 1)
  else {
    
    if (pp >= low.cut & pp < high.cut)
      c(low.cut, high.cut, pp, n.acountability, 2)
    else
      c(low.cut, high.cut, pp, n.acountability, 3)
  }
}



calc_type_3_target_level <- function (school, labels, precision=1, min.N = min.N.achievement) {
  school_achievement <- achievement[achievement$SCHOOL_ID == school[["SCHOOL_ID"]] &
                                      achievement$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],c("GRADE_BAND","PROFICIENT", "N", "ACCOUNTABILITY_N")]  
  if (nrow(school_achievement) == 0)
    return(rep(as.numeric(NA),length(labels)))
  
  
  N.ACCOUNTABILTY.1 <- school_achievement[school_achievement$GRADE_BAND == 1,"ACCOUNTABILITY_N"] 
  N.ACCOUNTABILTY.2 <- school_achievement[school_achievement$GRADE_BAND == 2,"ACCOUNTABILITY_N"] 
  
  N.ACCOUNTABILITY <-  max(ifelse(length(N.ACCOUNTABILTY.1) == 0, 0, N.ACCOUNTABILTY.1),
                           ifelse(length(N.ACCOUNTABILTY.2) == 0, 0, N.ACCOUNTABILTY.2)) ##if both bands are present then ACCOUNTABILITY_N is the same in both, but one band may be empty. So, we'll take the max.  
  
  low.cut.1 <- achievement.level.lookup[[1]][1]
  high.cut.1 <- achievement.level.lookup[[1]][2]
  
  low.cut.2 <- achievement.level.lookup[[2]][1]
  high.cut.2 <- achievement.level.lookup[[2]][2]
  
  
  N.1 <- school_achievement[school_achievement$GRADE_BAND == 1,"N"]
  N.2 <- school_achievement[school_achievement$GRADE_BAND == 2,"N"]  
  
  N.1 <- ifelse(length(N.1) == 0, 0, N.1)
  N.2 <- ifelse(length(N.2) == 0, 0, N.2)
  #TODO: address the situation where one of these is not defined
  
  adjusted.low.cut <- round((low.cut.1 * N.1 + low.cut.2 * N.2)/(N.1 + N.2), precision)
  adjusted.high.cut <- round((high.cut.1 * N.1 + high.cut.2 * N.2)/(N.1 + N.2), precision)
  
  if (N.ACCOUNTABILITY < min.N) {
    result <- rep(as.numeric(NA), length(labels))
    result[which(names(labels) == "N")] <- N.ACCOUNTABILITY  #no need to calculate anything if N is too small
    result[which(names(labels) == "cut.1")] <- adjusted.low.cut
    result[which(names(labels) == "cut.2")] <- adjusted.high.cut    
    return(result)
  }
  
  
  
  Proficient.1 <- school_achievement[school_achievement$GRADE_BAND == 1,"PROFICIENT"]
  Proficient.2 <- school_achievement[school_achievement$GRADE_BAND == 2,"PROFICIENT"]
  
  Proficient.1 <- ifelse(length(Proficient.1) == 0, 0, Proficient.1)
  Proficient.2 <- ifelse(length(Proficient.2) == 0, 0, Proficient.2)
  
  Percent.Proficient <- round((Proficient.1 + Proficient.2)/(N.1 + N.2) * 100, precision)
  
  if (Percent.Proficient < adjusted.low.cut)
    c(adjusted.low.cut, adjusted.high.cut, Percent.Proficient, N.ACCOUNTABILITY, 1)    
  else {
    
    if (Percent.Proficient >= adjusted.low.cut & Percent.Proficient < adjusted.high.cut)
      c(adjusted.low.cut, adjusted.high.cut, Percent.Proficient, N.ACCOUNTABILITY, 2)        
    else
      c(adjusted.low.cut, adjusted.high.cut, Percent.Proficient, N.ACCOUNTABILITY, 3)    
  }  
  
}

calc_achievement_target_level <- function (school, labels=achievement.labels) {
  school_type <- as.character(as.numeric(school[["WAEA_SCHOOL_TYPE"]]))
  result <- as.numeric(switch(school_type, 
                              `1` = calc_type_1_target_level(school, labels),
                              `3` = calc_type_3_target_level(school, labels),
                              `4` = calc_type_3_target_level(school, labels),         
                              `5` = calc_type_1_target_level(school, labels),                    
                              rep(as.numeric(NA),length(labels))))         
  names(result) <- labels
  return(result)
}


##growth


calc_growth_target_level <- function (school, labels=growth.labels, min.N=min.N.growth) {
  
  
  school_growth <- growth[growth$SCHOOL_ID == school[["SCHOOL_ID"]] &
                            growth$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],c("PERCENT_PROFICIENT_PRIOR", "MGP","GROWTH_ACCOUNTABILITY_N")]  
  if (nrow(school_growth) == 0) {
    result = rep(as.numeric(NA),length(labels))
    names(result) <- labels
    return(result)
  }
  
  N <-school_growth[["GROWTH_ACCOUNTABILITY_N"]]
  low.cut <- growth.level.lookup[1]
  high.cut <- growth.level.lookup[2]
  
  if (N < min.N) {
    result <- rep(as.numeric(NA), length(labels))
    result[which(names(labels) == "N")] <- N  #no need to calculate anything if N is too small
    result[which(names(labels) == "cut.1")] <- low.cut
    result[which(names(labels) == "cut.2")] <- high.cut    
    names(result) <- labels
    return(result)
  }
  
  
  
  mgp <- school_growth[["MGP"]]
  result <- if (mgp < low.cut)
    c(low.cut, high.cut, school_growth[["PERCENT_PROFICIENT_PRIOR"]], mgp, N, 1)
  else {
    
    if (mgp >= low.cut & mgp < high.cut)
      c(low.cut, high.cut, school_growth[["PERCENT_PROFICIENT_PRIOR"]], mgp, N, 2)
    else
      c(low.cut, high.cut, school_growth[["PERCENT_PROFICIENT_PRIOR"]], mgp, N, 3)
  }         
  names(result) <- labels
  result
}


##equity


calc_equity_target_level <- function (school, labels=equity.labels, min.N=min.N.subgroup) {
  
  
  school_equity <- equity[equity$SCHOOL_ID == school[["SCHOOL_ID"]] &
                            equity$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],c("PERCENT_MEETING_AGP","N_SUBGROUP")]  
  
  
  if (nrow(school_equity) == 0) {
    result = rep(as.numeric(NA),5)
    names(result) <- labels
    return(result)
  } 
  
  low.cut <- equity.level.lookup[1]
  high.cut <- equity.level.lookup[2]
  N <- school_equity[["N_SUBGROUP"]]
  if (N < min.N) {
    result <- rep(as.numeric(NA), length(labels))
    result[which(names(labels) == "N")] <- N  #no need to calculate anything if N is too small
    result[which(names(labels) == "cut.1")] <- low.cut
    result[which(names(labels) == "cut.2")] <- high.cut
    names(result) <- labels
    return(result)
  }
  
  
  
  percent.meeting.agp <- school_equity[["PERCENT_MEETING_AGP"]]
  result <- if (percent.meeting.agp < low.cut)
    c(low.cut, high.cut, percent.meeting.agp, school_equity[["N_SUBGROUP"]], 1)
  else {
    
    if (percent.meeting.agp >= low.cut & percent.meeting.agp < high.cut)
      c(low.cut, high.cut, percent.meeting.agp, school_equity[["N_SUBGROUP"]], 2)
    else
      c(low.cut, high.cut, percent.meeting.agp, school_equity[["N_SUBGROUP"]], 3)
  }         
  names(result) <- labels
  result
}


calc_participation_rate <- function(school, labels=c("PARTICIPATION_RATE")){
  school_participation <- participation.df[participation.df$SCHOOL_ID == school[["SCHOOL_ID"]] &
                                             participation.df$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],c("SCHOOL_ID", "SCHOOL_YEAR", "PARTICIPATION_RATE")]
  if (nrow(school_participation) == 0) {
    result = rep(as.numeric(NA),length(labels))
    names(result) <- labels
    return(result)
  } 
  
  result = c(school_participation$PARTICIPATION_RATE)
  names(result) <- labels
  result
}

count_indicators <- function (school, min.n = list(N_ACHIEVEMENT=min.N.achievement,
                                                   N_GROWTH=min.N.growth,
                                                   N_SUBGROUP=min.N.subgroup)) {
  achievement <- school[["ACHIEVEMENT_TARGET_LEVEL"]]
  growth <- school[["GROWTH_TARGET_LEVEL"]]
  equity <- school[["EQUITY_TARGET_LEVEL"]]
  
  achievement.n <- school[["N_ACHIEVEMENT"]]
  growth.n <- school[["N_GROWTH"]]
  equity.n <- school[["N_SUBGROUP"]]
  
  ifelse(is.na(achievement), 
         0, 
         ifelse(achievement.n >= min.n$N_ACHIEVEMENT, 1, 0)) + 
    ifelse(is.na(growth), 
           0, 
           ifelse(growth.n >= min.n$N_GROWTH, 1, 0)) + 
    ifelse(is.na(equity), 
           0, 
           ifelse(equity.n >= min.n$N_SUBGROUP, 1, 0))
}

#hs achievement
calc_type_hs_target_level <- function (school, labels=act.achievement.labels, min.N = min.N.achievement.hs) {
  school_achievement <- achievement.hs[achievement.hs$SCHOOL_ID == school[["SCHOOL_ID"]] &
                                          achievement.hs$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],c("ACHIEVEMENT_HS", "N_ACHIEVEMENT_HS", "PARTICIPATION_RATE_ACHIEVEMENT_HS")]  
  if (nrow(school_achievement) == 0) {
    result <- rep(as.numeric(NA), length(labels))
    names(result) <- labels
    return(result)
  }
  
  low.cut <- achievement.hs.level.lookup[1]
  high.cut <- achievement.hs.level.lookup[2]
  n.acountability <- school_achievement[["N_ACHIEVEMENT_HS"]]
  if (n.acountability < min.N) {
    result <- rep(as.numeric(NA), length(labels))
    result[which(names(labels) == "N")] <- n.acountability  #no need to calculate anything if N is too small
    result[which(names(labels) == "cut.1")] <- low.cut
    result[which(names(labels) == "cut.2")] <- high.cut
    names(result) <- labels
    return(result)
  }
  
  
  
  pp <- school_achievement[["ACHIEVEMENT_HS"]]
  
  participation.rate  <- school_achievement[["PARTICIPATION_RATE_ACHIEVEMENT_HS"]]
  
  result <- if (pp < low.cut)
    c(participation.rate, n.acountability, pp, low.cut, high.cut, 1)
  else {
    
    if (pp >= low.cut & pp < high.cut)
      c(participation.rate, n.acountability, pp, low.cut, high.cut, 2)
    else
      c(participation.rate, n.acountability, pp, low.cut, high.cut, 3)    
  }
  names(result) <- labels
  result
  
}



calc_hs_total_readiness_target_level <- function (school, labels=total.readiness.labels) {
  
  #   indicator <- with(schools, schools[SCHOOL_ID == school[["SCHOOL_ID"]] &
  #                                               SCHOOL_YEAR == school[["SCHOOL_YEAR"]], "TOTAL_READINESS_HS"])
  
  indicator <- as.numeric(school[["TOTAL_READINESS_HS"]])
  
  low.cut <- total.readiness.level.lookup[1]
  high.cut <- total.readiness.level.lookup[2]
  
  
  if (is.na(indicator)) {
    #if (length(indicator) == 0) {
    result <- rep(NA, length(labels))
    result[which(names(labels) == "cut.1")] <- low.cut
    result[which(names(labels) == "cut.2")] <- high.cut
    names(result) <- labels
    return(result)
  }
  
  
  result <- if (indicator < low.cut)
    c(low.cut, high.cut, 1)
  else {
    
    if (indicator >= low.cut & indicator < high.cut)
      c(low.cut, high.cut, 2)
    else
      c(low.cut, high.cut, 3)
  }
  
  names(result) <- labels
  result
}




# calc_hs_equity_target_level <- function (school, labels=hs.equity.labels) {
#   school_hs_equity <- hs.equity.df[hs.equity.df$SCHOOL_ID == school[["SCHOOL_ID"]] &
#                                      hs.equity.df$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],c("N_ACHIEVEMENT", 
#                                                                                            "N_ACHIEVEMENT_PRIOR", 
#                                                                                            "PARTICIPATION_RATE_ACHIEVEMENT",
#                                                                                            "PERCENT_NONPROFICIENT",
#                                                                                            "PERCENT_NONPROFICIENT_PRIOR",
#                                                                                            "IMPROVEMENT_VALUE",
#                                                                                            "EQUITY_VALUE")]  
#   
#   if (nrow(school_hs_equity) == 0) {
#     result <- rep(as.numeric(NA), length(labels))
#     names(result) <- labels
#     return(result)
#   }
# 
#   low.cut <- hs.equity.level.lookup[1]
#   high.cut <- hs.equity.level.lookup[2]
#   N_HS_EQUITY <- school_hs_equity[["N_ACHIEVEMENT"]]
#   N_HS_EQUITY_PRIOR <- school_hs_equity[["N_ACHIEVEMENT_PRIOR"]]
#   PARTICIPATION_RATE_HS_EQUITY <- school_hs_equity[["PARTICIPATION_RATE_ACHIEVEMENT"]]
#   PERCENT_NONPROFICIENT <- school_hs_equity[["PERCENT_NONPROFICIENT"]]
#   PERCENT_NONPROFICIENT_PRIOR <- school_hs_equity[["PERCENT_NONPROFICIENT_PRIOR"]]
#   IMPROVEMENT_NONPROFICIENT <- school_hs_equity[["IMPROVEMENT_VALUE"]]
#   HS_EQUITY <- school_hs_equity[["EQUITY_VALUE"]]
#     
#   result <- if (HS_EQUITY <= low.cut)
#     c(low.cut, 
#       high.cut, 
#       HS_EQUITY, 
#       IMPROVEMENT_NONPROFICIENT,
#       PERCENT_NONPROFICIENT,
#       PERCENT_NONPROFICIENT_PRIOR,
#       N_HS_EQUITY,                      
#       N_HS_EQUITY_PRIOR,
#       PARTICIPATION_RATE_HS_EQUITY, 
#       3)
#   else {
#     
#     if (HS_EQUITY > low.cut & HS_EQUITY <= high.cut)
#       c(low.cut, 
#         high.cut, 
#         HS_EQUITY, 
#         IMPROVEMENT_NONPROFICIENT,
#         PERCENT_NONPROFICIENT,
#         PERCENT_NONPROFICIENT_PRIOR,
#         N_HS_EQUITY,                      
#         N_HS_EQUITY_PRIOR,
#         PARTICIPATION_RATE_HS_EQUITY, 
#         2)
#     else
#       c(low.cut, 
#         high.cut, 
#         HS_EQUITY, 
#         IMPROVEMENT_NONPROFICIENT,
#         PERCENT_NONPROFICIENT,
#         PERCENT_NONPROFICIENT_PRIOR,
#         N_HS_EQUITY,                      
#         N_HS_EQUITY_PRIOR,
#         PARTICIPATION_RATE_HS_EQUITY, 
#         1)
#   }
#   names(result) <- labels
#   result
#   
# }




calc_hs_equity <- function (school, labels=hs.equity.labels, min.N = min.N.equity.hs) {
  
  hs.equity.val <- with(hs.equity.df,
                        hs.equity.df[SCHOOL_ID==school[["SCHOOL_ID"]] &
                                       SCHOOL_YEAR==school[["SCHOOL_YEAR"]],
                                     c("N_ACHIEVEMENT",
                                       "N_ACHIEVEMENT_PRIOR", 
                                       "PERCENT_NONPROFICIENT", 
                                       "PERCENT_NONPROFICIENT_PRIOR", 
                                       "IMPROVEMENT_VALUE",
                                       "PERCENT_NONPROFICIENT_CATEGORY", 
                                       "IMPROVEMENT_CATEGORY", 
                                       "EQUITY_TARGET_LEVEL")])
  if (nrow(hs.equity.val) == 0)  
  {  
    result = rep(as.numeric(NA), length(hs.equity.labels))
  } else {
    
    
    
    N <- hs.equity.val[["N_ACHIEVEMENT"]]
    if (N < min.N | school[["SMALL_SCHOOL_HS"]] == 'T') { #small high schools don't really have a well defined equity because we end up comparing achievement from a year ago partially to itself
      result <- rep(as.numeric(NA), length(labels))
      result[which(names(labels) == "N")] <- N  #no need to calculate anything if N is too small
      result[which(names(labels) == "cut.1")] <- hs.equity.level.lookup$IMPROVEMENT_VALUE[2]
      result[which(names(labels) == "cut.2")] <- hs.equity.level.lookup$IMPROVEMENT_VALUE[1]
      
    } else {
      
      result = unlist(c(hs.equity.val[,c("N_ACHIEVEMENT", "N_ACHIEVEMENT_PRIOR", 
                                         "PERCENT_NONPROFICIENT", "PERCENT_NONPROFICIENT_PRIOR")],
                        PERCENT_NONPROFICIENT_LOW = hs.equity.level.lookup$PERCENT_NONPROFICIENT[1],
                        PERCENT_NONPROFICIENT_HIGH = hs.equity.level.lookup$PERCENT_NONPROFICIENT[2],
                        IMPROVEMENT_VALUE=hs.equity.val[,c("IMPROVEMENT_VALUE")], 
                        IMPROVEMENT_VALUE_LOW = hs.equity.level.lookup$IMPROVEMENT_VALUE[1],
                        IMPROVEMENT_VALUE_HIGH = hs.equity.level.lookup$IMPROVEMENT_VALUE[2],
                        hs.equity.val[, c("PERCENT_NONPROFICIENT_CATEGORY", 
                                          "IMPROVEMENT_CATEGORY", "EQUITY_TARGET_LEVEL")]))
    }
  }
  names(result) <- labels
  result
}


count_indicators.hs <- function (school, min.n = list(N_ACHIEVEMENT_HS=min.N.achievement.hs,
                                                      N_TOTAL_READINESS_HS=min.N.readiness.hs,
                                                      N_EQUITY_HS=min.N.equity.hs)) {
  achievement <- school[["ACHIEVEMENT_TARGET_LEVEL_HS"]]
  readiness <- school[["READINESS_TARGET_LEVEL"]]
  equity <- school[["EQUITY_TARGET_LEVEL_HS"]]
  
  achievement.n <- school[["N_ACHIEVEMENT_HS"]]
  readiness.n <- school[["N_TOTAL_READINESS_HS"]]
  equity.n <- school[["N_EQUITY_HS"]]
  
  ifelse(is.na(readiness), 
         0, 
         ifelse(readiness.n >= min.n$N_TOTAL_READINESS_HS, 1, 0)) + 
    ifelse(is.na(achievement), 
           0, 
           ifelse(achievement.n >= min.n$N_ACHIEVEMENT_HS, 1, 0)) + 
    ifelse(is.na(equity), 
           0, 
           ifelse(equity.n >= min.n$N_EQUITY_HS, 1, 0))
}



#Not a very well written function.  Should just make two functions.
calc.SPLs <- function (schools, school.type = "nonHS") {
  
  calc.SPL.nonHS <- function (indicators, lookups) {
    n.indicators <- indicators[["N_INDICATORS"]]
    lookup <- lookups[[as.character(n.indicators)]]    
    
    if (is.null(lookup))
      return(NA)
    
    if (n.indicators == 2) {      
      lookup[as.numeric(indicators[["ACHIEVEMENT_TARGET_LEVEL"]]), 
             as.numeric(indicators[["GROWTH_TARGET_LEVEL"]])]  
      
    } else  {
      if (n.indicators == 3)
        lookup[as.numeric(indicators[["ACHIEVEMENT_TARGET_LEVEL"]]), 
               as.numeric(indicators[["GROWTH_TARGET_LEVEL"]]), 
               as.numeric(indicators[["EQUITY_TARGET_LEVEL"]])]  
      else
        NA 
    }    
  }
  
  calc.SPL.HS <- function (indicators, lookups) {
    n.indicators <- indicators[["N_INDICATORS_HS"]]
    lookup <- lookups[[as.character(n.indicators)]]    
    
    if (is.null(lookup))
      return(NA)
    
    if (n.indicators == 2) {
      lookup[as.numeric(indicators[["ACHIEVEMENT_TARGET_LEVEL_HS"]]), 
             as.numeric(indicators[["READINESS_TARGET_LEVEL"]])]  
      
    } else  {
      if (n.indicators == 3)
        lookup[as.numeric(indicators[["ACHIEVEMENT_TARGET_LEVEL_HS"]]), 
               as.numeric(indicators[["READINESS_TARGET_LEVEL"]]), 
               as.numeric(indicators[["EQUITY_TARGET_LEVEL_HS"]])]  
      else
        NA 
    }    
  }
  
  calc.SPL <- function (indicators) {
    lookups <- SPL.lookup[[school.type]]
    
    if (school.type == "nonHS") {      
      calc.SPL.nonHS(indicators, lookups)
    }
    else {
      if (school.type == "HS") {
        calc.SPL.HS(indicators, lookups)
      }
      else
        NA
    }
    
  }
  
  SPLs <- apply(schools,
                c(1),
                FUN= calc.SPL)
  
  SPL.label <- ifelse(school.type == "nonHS", "SPL", "SPL_HS")
  SPL.adjusted.label <- ifelse(school.type == "nonHS", "SPL_ADJUSTED", "SPL_ADJUSTED_HS")
  part.rate.level.label <- ifelse(school.type == "nonHS", "PARTICIPATION_RATE_LEVEL", "PARTICIPATION_RATE_LEVEL_HS")
  
  SPLs.adjusted <- sapply(1:length(SPLs),
                          FUN= function (i) {
                            SPL <- SPLs[i]
                            PRL <- schools[,part.rate.level.label][i]
                            
                            if (is.na(SPL))
                              NA
                            else {
                              
                              if (PRL == 3)
                                SPL
                              else {
                                
                                if (PRL == 2)
                                  max(1, SPL-1)
                                else  #not met
                                  1
                              }
                              
                            }                               
                            
                          })
  
  #drop the SPL columns if they exist
  result <- cbind(schools[,!(names(schools) %in% c(SPL.label, SPL.adjusted.label))], SPLs, SPLs.adjusted)
  names(result)[(ncol(result)-1):ncol(result)] <- c(SPL.label, SPL.adjusted.label)
  result  
}

calc.school.achievement <- function (schools) {
  
  #drop any existing achievement columns
  schools <- schools[, !(names(schools) %in% achievement.labels)]
  #Assign achievement cut points and a target level to each shool
  cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                               "SCHOOL_YEAR",                                                     
                                               "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                    FUN=calc_achievement_target_level))))
  
}

calc.school.growth <- function (schools) {
  
  schools <- schools[, !(names(schools) %in% growth.labels)]
  #compute growth columns
  cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                               "SCHOOL_YEAR")], c(1),                                                                                      
                                    FUN=calc_growth_target_level))))
  
}


calc.school.equity <- function (schools) {
  
  #drop equity columns
  schools <- schools[, !(names(schools) %in% equity.labels)]
  
  #compute equity columns
  cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                               "SCHOOL_YEAR")], c(1),                                                                                      
                                    FUN=calc_equity_target_level))))
  
}



calc.school.achievement.hs <- function (schools) {
  schools <- schools[, !(names(schools) %in% act.achievement.labels)]
  #Assign achievement cut points and a target level to each shool
  cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                               "SCHOOL_YEAR",                                                     
                                               "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                    FUN=calc_type_hs_target_level))))
}


calc.school.tested.readiness.hs <- function (schools, labels=tested.readiness.labels, min.N = min.N.tested.readiness) {
  
  schools <- schools[, !(names(schools) %in% tested.readiness.labels)]
  
  cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                               "SCHOOL_YEAR",                                                     
                                               "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                    FUN=function (school) {
                                      readiness <- with(tested.readiness.df, tested.readiness.df[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                                   SCHOOL_YEAR == school[["SCHOOL_YEAR"]],
                                                                                                 c("TESTED_READINESS", "N_TESTED", "PARTICIPATION_RATE")])
                                      
                                      if (nrow(readiness) == 0)
                                        result <- rep(NA, length(labels))
                                      else {
                                        N <- readiness[["N_TESTED"]]
                                        if (N < min.N) {
                                          result <- rep(as.numeric(NA), length(labels))
                                          result[which(names(labels) == "N")] <- N  #no need to calculate anything if N is too small
                                        } else {                                                   
                                          result <- as.vector(as.matrix(readiness))
                                        }
                                      }
                                      names(result) <- labels
                                      result
                                    }
                                    
  ))))
  
}



calc.indexed.readiness <- function (schools, readiness.students.df, readiness.participation.schools, index.by.test.type = list(ACT=act_index,
                                                                                                                               EXPLORE=explore_index,
                                                                                                                               `PAWS Alternate`=alt_index,
                                                                                                                               PLAN=plan_index)) {
  
  eval.student <- function (student) {
    index <- index.by.test.type[[student[["TEST_TYPE"]]]]
    if (is.null(index))
      NA
    else {
      
      index[as.character(as.numeric(student[["RAW_SCORE"]]))]
    }
    
  }
  
  #calculate indexed score for each student based on the type of text they took  
  readiness.students.df$READINESS_INDEX <- apply(readiness.students.df[,c("TEST_TYPE", "RAW_SCORE")], c(1),
                                                 eval.student)
  
  #should be the empty set
  #readiness.students.df[is.na(readiness.students.df$READINESS_INDEX),]
  
  readiness.schools.df <- aggregate(data.frame(TESTED_READINESS_SUM = readiness.students.df$READINESS_INDEX, N=rep(1, nrow(readiness.students.df))),
                                    by=list(SCHOOL_YEAR = readiness.students.df$SCHOOL_YEAR,
                                            SCHOOL_ID = readiness.students.df$SCHOOL_ID),
                                    sum)
  
  #do state level aggregate
  readiness.schools.df <- state.level.aggregate(readiness.schools.df, sum, state.school.id)
  
  #tail(readiness.schools.df)
  
  readiness.schools.df$TESTED_READINESS <- round((readiness.schools.df$TESTED_READINESS_SUM/readiness.schools.df$N), precision)
  
  readiness.schools.df <- merge(readiness.schools.df, readiness.participation.schools)
  
  readiness.schools.df <- merge(readiness.schools.df, schools)
  
  readiness.schools.df
  
}


calc.school.readiness.hs <- function (schools) { 
  
  #   schools$READINESS_TARGET_LEVEL <- apply(schools, c(1),
  #                                           FUN=calc_hs_total_readiness_target_level)
  #   
  schools <- schools[, !(names(schools) %in% total.readiness.labels)]
  
  cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                               "SCHOOL_YEAR",                                                     
                                               "WAEA_SCHOOL_TYPE",
                                               "TOTAL_READINESS_HS")], c(1),                                                                                      
                                    FUN=calc_hs_total_readiness_target_level))))
  
}

calc.indexed.grads <- function (schools, grads.df, labels = grad.index.labels, min.N=min.N.grad) {
  
  
  grads.df$VALUE <- sapply(grads.df$INDEX, function(idx) grad.index[[idx]])
  
  
  grads.average.index <- cast(grads.df[,c("SCHOOL_YEAR","SCHOOL_ID", "VALUE")],
                              SCHOOL_YEAR+SCHOOL_ID~.,
                              function (x) c(AVERAGE_VALUE=round(mean(x),precision), N=length(x)), margins="SCHOOL_YEAR")
  
  #relable state average
  grads.average.index[grads.average.index$SCHOOL_ID=='(all)',]
  grads.average.index$SCHOOL_ID <- as.character(grads.average.index$SCHOOL_ID)
  grads.average.index[grads.average.index$SCHOOL_ID=='(all)',c("SCHOOL_ID")] <- rep(state.school.id, nrow(grads.average.index[grads.average.index$SCHOOL_ID=='(all)',]))
  
  
  names(grads.average.index)[3:4] <- labels
  
  #if N is too small then set grad index to NA
  grads.average.index[[labels["Indicator"]]] <- ifelse(grads.average.index[[labels["N"]]] < min.N, NA,  grads.average.index[[labels["Indicator"]]]) 
  
  #this index applies only to high schools
  grads.average.index <- merge(grads.average.index, unique(with(schools, schools[WAEA_SCHOOL_TYPE %in% HS.types, c("SCHOOL_YEAR","SCHOOL_ID")])))
  
  grads.average.index
  
}


calc.school.grad.index <- function (schools) {
  
  schools <- schools[, !(names(schools) %in% grad.index.labels)]
  
  merge(schools, grads.average.index, all.x=TRUE)
  
  
}

#could optimize this
propagate.results.to.paired.schools <- function (schools) {
  
  #just propagating the nonHS columns. Don't think any of the schools are paired with high schools or K12s.
  lapply(c(achievement.labels,
           achievement.grade.band.labels,
           growth.labels,
           equity.labels, 
           "PARTICIPATION_RATE",
           #still report them in the summary as 0 indicator schools?
           #"N_INDICATORS",
           "SPL",
           "SPL_ADJUSTED"),
         function (column) {
           
           schools[[column]] <<- as.numeric(apply(schools[,c("SCHOOL_YEAR", "SCHOOL_ID", column)], 
                                                  c(1), 
                                                  function (school) {
                                                    school.year <- school[["SCHOOL_YEAR"]]
                                                    pairing <- school.pairing.lookup[[school[["SCHOOL_YEAR"]]]]
                                                    paired.school <- pairing[[school[["SCHOOL_ID"]]]]
                                                    ifelse(is.null(paired.school), 
                                                           school[[column]], 
                                                           schools[schools$SCHOOL_YEAR == school.year & 
                                                                     schools$SCHOOL_ID == paired.school, c(column)])
                                                    
                                                  }))
           
           
         })
  schools
}


school.labels = c("SCHOOL_ID", "SCHOOL_YEAR", "DISTRICT_ID", "DISTRICT_NAME", "NAME", "SHORT_NAME", "CATEGORY", "LOW_GRADE", "HIGH_GRADE", "GRADES_SERVED", 
                  "WAEA_SCHOOL_TYPE", "YEAR", "SMALL_SCHOOL", "YEARS_BACK", achievement.labels, achievement.grade.band.labels, growth.labels, equity.labels, "PARTICIPATION_RATE",
                  "N_INDICATORS", "SPL", "SMALL_SCHOOL_HS", "YEARS_BACK_HS",
                  act.achievement.labels, tested.readiness.labels, grad.index.labels, "N_TOTAL_READINESS_HS", 
                  "TOTAL_READINESS_HS", total.readiness.labels, hs.equity.labels, "N_INDICATORS_HS", "SPL_HS", "ACCOUNTABILITY_SPL")

# school.labels = c("SCHOOL_ID", "SCHOOL_YEAR", "DISTRICT_ID", "DISTRICT_NAME", "NAME", "SHORT_NAME", "CATEGORY", "LOW_GRADE", "HIGH_GRADE", "GRADES_SERVED", 
#                   "WAEA_SCHOOL_TYPE", "YEAR", "SMALL_SCHOOL", "YEARS_BACK", "ACHIEVEMENT_CUT_1", "ACHIEVEMENT_CUT_2", "PERCENT_PROFICIENT", "N_ACHIEVEMENT", 
#                   "ACHIEVEMENT_TARGET_LEVEL", "PERCENT_PROFICIENT_BAND_1", "PERCENT_PROFICIENT_BAND_2", "N_ACHIEVEMENT_BAND_1", "N_ACHIEVEMENT_BAND_2", 
#                   "GROWTH_CUT_1", "GROWTH_CUT_2", "PERCENT_PROFICIENT_PRIOR", "MGP", "N_GROWTH", "GROWTH_TARGET_LEVEL", "EQUITY_CUT_1", "EQUITY_CUT_2", 
#                   "PERCENT_MEETING_AGP", "N_SUBGROUP", "EQUITY_TARGET_LEVEL", "PARTICIPATION_RATE", "N_INDICATORS", "SPL", "ACHIEVEMENT_CUT_1_HS", 
#                   "ACHIEVEMENT_CUT_2_HS", "PERCENT_PROFICIENT_HS", "N_ACHIEVEMENT_HS", "PARTICIPATION_RATE_ACHIEVEMENT_HS", "ACHIEVEMENT_TARGET_LEVEL_HS", 
#                   "TESTED_READINESS", "N_TESTED_READINESS", "PARTICIPATION_RATE_TESTED_READINESS", "SCHOOL_GRADUATION_INDEX", "N_GRADUATION", "N_TOTAL_READINESS_HS", 
#                   "TOTAL_READINESS_HS", "READINESS_CUT_1", "READINESS_CUT_2", "READINESS_TARGET_LEVEL", "N_EQUITY_HS", "N_ACHIEVEMENT_HS_PRIOR", 
#                   "PERCENT_NONPROFICIENT", "PERCENT_NONPROFICIENT_CUT_LOW_REVERSED", "PERCENT_NONPROFICIENT_CUT_HIGH_REVERSED", "IMPROVEMENT_SCORE", 
#                   "IMPROVEMENT_CUT_LOW_REVERSED", "IMPROVEMENT_CUT_HIGH_REVERSED", "PERCENT_NONPROFICIENT_CATEGORY", "IMPROVEMENT_CATEGORY", "EQUITY_TARGET_LEVEL_HS",
#                   "N_INDICATORS_HS", "SPL_HS", "ACCOUNTABILITY_SPL")

reorder.school.columns <- function (schools) {
  
  schools[,school.labels]
}


state.level.aggregate <- function (df, fun, state.id=state.school.id) {
  
  df.state <- aggregate(df[,setdiff(names(df), c("SCHOOL_ID", "SCHOOL_YEAR"))],
                        by=list(SCHOOL_YEAR=df$SCHOOL_YEAR),
                        fun)
  df.state <- cbind(SCHOOL_ID=rep(state.id, nrow(df.state)), df.state)
  
  rbind(df, df.state)  
}