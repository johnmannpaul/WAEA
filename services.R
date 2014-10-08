library("reshape")
library(rcom)
library(compare)



#how many schools with 3-8 performance do not have an indicator
#with(schools, table(schools[SCHOOL_YEAR=='2012-13' & WAEA_SCHOOL_TYPE != c(2),]$N_INDICATORS))
report.adjusted.SPLs = FALSE

schools.pilot <- read.csv(file="data/2012-13-Pilot.csv", header=TRUE, colClasses=c(SCHOOL_ID="character"))
schools.prior <- schools.pilot[schools.pilot$SCHOOL_YEAR==prior.school.year &
                                 schools.pilot$SCHOOL_ID!=state.school.id,
                               c("SCHOOL_YEAR", "SCHOOL_ID", "WAEA_SCHOOL_TYPE", "SPL", "SPL_ADJUSTED",
                                 "SPL_HS", "SPL_ADJUSTED_HS",
                                 "ACCOUNTABILITY_SPL")]

schools.prior$ALL_SPL <- apply(schools.prior[,c("SPL", "SPL_HS")],
                               c(1),
                               function (scores) scores[order(scores)][1])
SPL.prior <- schools.prior[,c(1:7,9,8)]
names(SPL.prior) <- c("SCHOOL_YEAR", "SCHOOL_ID", "WAEA_SCHOOL_TYPE", SPL.types)

calc.sums.2 <- function (sums.2.df, cast.FUN) {
  
  axes <- names(sums.2.df)[1:2]
  value <- names(sums.2.df)[3]
  
  sums.2.df[axes[1]] <- factor(sums.2.df[[axes[1]]], levels=c("1","2","3", NA), exclude=c(), ordered=TRUE)
  sums.2.df[axes[2]] <- factor(sums.2.df[[axes[2]]], levels=c("1","2","3", NA), exclude=c(), ordered=TRUE)
  cast(sums.2.df, value=value,
       as.formula(paste(axes[1],axes[2], sep="~")), cast.FUN, add.missing=TRUE)
  
}

update.results.nonHS <- function (school.year, adjusted=report.adjusted.SPLs, label.sum.by="SCHOOL_ID", cast.FUN.name = "length") {

  cast.FUN <- if(cast.FUN.name == 'sum') sum else length

  #recompute G38 SPLs
  schools$G38_SPL <<- apply(schools[,c("WAEA_SCHOOL_TYPE", "G38_INDICATORS_N", g38.target.level.labels)],
                           c(1),
                           FUN=calc.SPL.nonHS )
  
  schools$G38_SPL_ACCOUNTABILITY <<- apply(schools[,c("G38_SPL", g38.participation.labels)],
                                          c(1),
                                          FUN=calc.SPL.accountability, "G38_SPL",  g38.participation.labels)
  
  #propagate G38 values to paired schools (none of these schools are paired with schools serving 12th grade)
  g38.accountability.labels <- names(schools)[grep("^G38_", names(schools))]
  schools <<- propagate.results.to.paired.schools(schools, 
                                                  g38.accountability.labels)
  
  #The 'all' SPL depends on G38 SPLs so recompute it.
  schools$ALL_SPL <<- apply(schools[,c("G38_SPL", "HS_SPL")],
                            c(1),
                            function (scores) scores[order(scores)][1])
  
  schools$ALL_SPL_ACCOUNTABILITY <<- apply(schools[,c("G38_SPL_ACCOUNTABILITY", "HS_SPL_ACCOUNTABILITY")],
                                           c(1),
                                           function (scores) scores[order(scores)][1])
                                            
  
  #compute impact results
  #need to have missing values inserted.  That's whey we make them factors.
  sums.3.df = schools[schools$G38_INDICATORS_N==3 & 
                        schools$SCHOOL_YEAR==school.year &
                        schools$SCHOOL_ID != state.school.id,
                      c(g38.target.level.labels["equity"], 
                        g38.target.level.labels["growth"], 
                        g38.target.level.labels["achievement"], 
                        label.sum.by )]
  
  names(sums.3.df) <- c("EQUITY_TARGET_LEVEL", "GROWTH_TARGET_LEVEL", "ACHIEVEMENT_TARGET_LEVEL", label.sum.by)
  sums.3.df$EQUITY_TARGET_LEVEL <- factor(sums.3.df$EQUITY_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.3.df$GROWTH_TARGET_LEVEL <- factor(sums.3.df$GROWTH_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.3.df$ACHIEVEMENT_TARGET_LEVEL <- factor(sums.3.df$ACHIEVEMENT_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.3 <- cast(sums.3.df, value=label.sum.by,
                 EQUITY_TARGET_LEVEL+GROWTH_TARGET_LEVEL~ACHIEVEMENT_TARGET_LEVEL, cast.FUN, add.missing=TRUE)
  
  
  sums.2.df <- schools[schools$G38_INDICATORS_N==2 & 
                         schools$SCHOOL_YEAR==school.year &
                         schools$SCHOOL_ID != state.school.id,
                       c(g38.target.level.labels["growth"], 
                         g38.target.level.labels["achievement"], 
                         label.sum.by )]
  
  names(sums.2.df) <- c("GROWTH_TARGET_LEVEL", "ACHIEVEMENT_TARGET_LEVEL", label.sum.by)
  sums.2.df$GROWTH_TARGET_LEVEL <- factor(sums.2.df$GROWTH_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.2.df$ACHIEVEMENT_TARGET_LEVEL <- factor(sums.2.df$ACHIEVEMENT_TARGET_LEVEL, levels=c("1","2","3"), ordered=TRUE)
  sums.2 <- cast(sums.2.df, value=label.sum.by,
                 GROWTH_TARGET_LEVEL~ACHIEVEMENT_TARGET_LEVEL, cast.FUN, add.missing=TRUE)
  
  #combine the 9x3 and 3x3 cell totals
  sums.schools <- rbind(as.matrix(sums.3),as.matrix(sums.2))
  
  
  #compute the impact totals 
  if (adjusted) 
    SPL.schools <- schools[schools$WAEA_SCHOOL_TYPE %in% c(nonHS.types, paired.types) & 
                             schools$SCHOOL_YEAR==school.year &
                             schools$SCHOOL_ID != state.school.id,
                           c("G38_INDICATORS_N", "G38_SPL_ACCOUNTABILITY",  label.sum.by )]
  else
    SPL.schools <- schools[schools$WAEA_SCHOOL_TYPE %in% c(nonHS.types, paired.types) & 
                             schools$SCHOOL_YEAR==school.year &
                             schools$SCHOOL_ID != state.school.id,
                           c("G38_INDICATORS_N", "G38_SPL",  label.sum.by )]
  
  names(SPL.schools) <- c("N_INDICATORS", "SPL", label.sum.by)
  SPL.schools$N_INDICATORS <- factor(as.character(SPL.schools$N_INDICATORS), levels=c("3","2","1","0"), ordered=TRUE)
  SPL.schools$SPL <- ifelse(!is.na(SPL.schools$SPL), SPL.schools$SPL, 0)
  SPL.schools$SPL <- factor(as.character(SPL.schools$SPL), levels=c("1","2","3","4","0"), ordered=TRUE)
  sums.SPL <- cast(SPL.schools, value = label.sum.by, SPL ~ N_INDICATORS, 
                   cast.FUN, add.missing=TRUE, margins=TRUE)
  sums.types <- t(rbind(with(schools, table(schools[WAEA_SCHOOL_TYPE==1 & HIGH_GRADE <= 6 & SCHOOL_YEAR==current.school.year,]$G38_SPL, useNA="always")),
                        with(schools, table(schools[((WAEA_SCHOOL_TYPE==1 & HIGH_GRADE > 6) | WAEA_SCHOOL_TYPE ==5) & 
                                                      SCHOOL_YEAR==current.school.year,]$G38_SPL, useNA="always")),
                        with(schools, table(schools[(WAEA_SCHOOL_TYPE ==3 | WAEA_SCHOOL_TYPE ==4) & 
                                                      SCHOOL_YEAR==current.school.year,]$G38_SPL, useNA="always"))))
  
  
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
  

update.results.HS <- function (school.year, adjusted=report.adjusted.SPLs, label.sum.by="SCHOOL_ID", cast.FUN.name = "length") {
  
  
  cast.FUN <- if(cast.FUN.name == 'sum') sum else length
  
  #recompute the two indcator compositions
  schools[,hs.overall.target.level.labels["readiness"]] <<- apply(schools[,c("WAEA_SCHOOL_TYPE", 
                                                                            hs.readiness.target.level.labels)],
                                                                 c(1),
                                                                 FUN=hs.compose.indicators, hs.readiness.target.level.labels, SPL.lookup[["HS"]][["Readiness"]] )
  
  schools[,hs.overall.target.level.labels["achievement"]] <<- apply(schools[,c("WAEA_SCHOOL_TYPE", 
                                                                              hs.achievement.target.level.labels)],
                                                                   c(1),
                                                                   FUN=hs.compose.indicators, hs.achievement.target.level.labels, SPL.lookup[["HS"]][["Achievement"]] )
  
  #now redo the SPLs
  schools$HS_SPL <<- apply(schools[,c("WAEA_SCHOOL_TYPE", 
                                     hs.overall.target.level.labels)], c(1),
                          calc.SPL.HS)
  
  schools$HS_SPL_ACCOUNTABILITY <<- apply(schools[,c("HS_SPL", hs.participation.labels)],
                                         c(1),
                                         FUN=calc.SPL.accountability, "HS_SPL",  hs.participation.labels)
  
  

  #The'all' SPLs depend on HS SPLs.  So recompute those now.
  schools$ALL_SPL <<- apply(schools[,c("G38_SPL", "HS_SPL")],
                           c(1),
                           function (scores) scores[order(scores)][1])
  
  schools$ALL_SPL_ACCOUNTABILITY <<- apply(schools[,c("G38_SPL_ACCOUNTABILITY", "HS_SPL_ACCOUNTABILITY")],
                                          c(1),
                                          function (scores) scores[order(scores)][1])
  
  #compute impact results
  
  overall.readiness.sum <- calc.sums.2(schools[schools$SCHOOL_YEAR==school.year &
                                                 schools$WAEA_SCHOOL_TYPE %in% HS.types &
                                                 schools$SCHOOL_ID != state.school.id,
                                               c(hs.readiness.target.level.labels, label.sum.by)], cast.FUN)
                                       
  academics.sum <- calc.sums.2(schools[schools$SCHOOL_YEAR==school.year &
                                         schools$WAEA_SCHOOL_TYPE %in% HS.types &
                                         schools$SCHOOL_ID != state.school.id,
                                       c(hs.achievement.target.level.labels, label.sum.by)], cast.FUN)
  
  SPL.sum <- calc.sums.2(schools[schools$SCHOOL_YEAR==school.year &
                                   schools$WAEA_SCHOOL_TYPE %in% HS.types &
                                   schools$SCHOOL_ID != state.school.id,
                                 c(hs.overall.target.level.labels, label.sum.by)], cast.FUN)
  
    

  
  #compute the impact totals 
  if (adjusted) 
    SPL.schools <- schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
                             schools$SCHOOL_YEAR==school.year &
                             schools$SCHOOL_ID != state.school.id,
                           c("ALTERNATIVE_SCHOOL", "HS_SPL_ACCOUNTABILITY",  grad.rate.cats, grad.rate.labels[c("4yr.N", "extended.N")], label.sum.by )]
  else
    SPL.schools <- schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
                             schools$SCHOOL_YEAR==school.year &
                             schools$SCHOOL_ID != state.school.id,
                           c("ALTERNATIVE_SCHOOL", "HS_SPL",  grad.rate.cats, grad.rate.labels[c("4yr.N", "extended.N")], label.sum.by )]
  
  names(SPL.schools) <- c("ALTERNATIVE", "SPL_HS", grad.rate.cats, grad.rate.labels[c("4yr.N", "extended.N")], label.sum.by)
  SPL.schools$SPL_DEFINED <- factor(ifelse(is.na(SPL.schools$SPL_HS), "no", "yes"), levels=c("yes", "no"), ordered=TRUE)
  SPL.schools$SPL_HS <- factor(as.character(SPL.schools$SPL_HS), levels=c("1","2","3","4",NA), exclude=c(), ordered=TRUE)
  SPL.schools$ALTERNATIVE <- factor(as.character(SPL.schools$ALTERNATIVE), levels=c("T","F"), ordered=TRUE)
  SPL.schools[grad.rate.cats["4yr"]] <- factor(SPL.schools[[grad.rate.cats["4yr"]]], levels=c("1","2","3"), ordered=TRUE)
  SPL.schools[grad.rate.cats["extended"]] <- factor(SPL.schools[[grad.rate.cats["extended"]]], levels=c("1","2","3"), ordered=TRUE)
  SPL.schools[grad.rate.cats["improve"]] <- factor(SPL.schools[[grad.rate.cats["improve"]]], levels=c("1","2","3"), ordered=TRUE)
  
  sums.SPL <- as.matrix(cast(SPL.schools[c("SPL_DEFINED", "SPL_HS", label.sum.by)], value=label.sum.by,
                   SPL_HS~SPL_DEFINED, cast.FUN, add.missing=TRUE, margins=TRUE))
    
  sums.SPL.alternative <- as.matrix(cast(SPL.schools[c("ALTERNATIVE", "SPL_HS", label.sum.by)], value=label.sum.by,
                   SPL_HS~ALTERNATIVE, cast.FUN, add.missing=TRUE, margins=TRUE))
  

  sums.4yr.alternative <- as.matrix(cast(SPL.schools[!is.na(SPL.schools[[grad.rate.cats["4yr"]]]) &
                                                       SPL.schools[[grad.rate.labels["4yr.N"]]] >= min.N.grad,c("ALTERNATIVE", grad.rate.cats["4yr"], label.sum.by)], value=label.sum.by,
                                         as.formula(paste(grad.rate.cats["4yr"], "ALTERNATIVE", sep="~")), cast.FUN, add.missing=TRUE, margins=TRUE))
  
  sums.ext.alternative <- as.matrix(cast(SPL.schools[!is.na(SPL.schools[[grad.rate.cats["extended"]]]) &
                                                       SPL.schools[[grad.rate.labels["extended.N"]]] >= min.N.grad,c("ALTERNATIVE", grad.rate.cats["extended"], label.sum.by)], value=label.sum.by,
                                         as.formula(paste(grad.rate.cats["extended"], "ALTERNATIVE", sep="~")), cast.FUN, add.missing=TRUE, margins=TRUE))
  
  sums.imp.alternative <- as.matrix(cast(SPL.schools[!is.na(SPL.schools[[grad.rate.cats["improve"]]]) &
                                                       SPL.schools[[grad.rate.labels["extended.N"]]] >= min.N.grad,c("ALTERNATIVE", grad.rate.cats["improve"], label.sum.by)], value=label.sum.by,
                                         as.formula(paste(grad.rate.cats["improve"], "ALTERNATIVE", sep="~")), cast.FUN, add.missing=TRUE, margins=TRUE))
  
  
  rbind(as.matrix(overall.readiness.sum),
        as.matrix(academics.sum),
        as.matrix(SPL.sum),
        cbind(sums.SPL, NA),
        cbind(sums.SPL.alternative, NA),
        cbind(sums.4yr.alternative, NA),
        cbind(sums.ext.alternative, NA),
        cbind(sums.imp.alternative, NA))
  
  
}

get.results.nonHS <- function(matrices.df, label.sum.by="SCHOOL_ID", cast.FUN.name = "length", adjusted=report.adjusted.SPLs, school.year=current.school.year) {
  
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
  
  update.results.nonHS(school.year, adjusted, label.sum.by, cast.FUN.name)
    
}


get.results.HS <- function(matrices.df, label.sum.by="SCHOOL_ID", cast.FUN.name = "length",  adjusted=report.adjusted.SPLs, school.year=current.school.year) {
  
  colnames(matrices.df) <- c("1","2","3")
  
  SPL.lookup$HS$Readiness <<- matrices.df[1:3,]
  SPL.lookup$HS$Achievement <<- matrices.df[4:6,]
  SPL.lookup$HS$Overall <<- matrices.df[7:9,]
  
  
  update.results.HS(school.year, adjusted, label.sum.by, cast.FUN.name)
  
}



get.results.all <- function (label.sum.by="SCHOOL_ID", 
                             cast.FUN.name = "length",
                             adjusted=report.adjusted.SPLs,
                             school.year=current.school.year) {
  
  
  cast.FUN <- if(cast.FUN.name == 'sum') sum else length

  SPL.schools <- rbind(schools[schools$SCHOOL_YEAR==school.year &
                                 schools$SCHOOL_ID != state.school.id,c("SCHOOL_YEAR", "SCHOOL_ID", "WAEA_SCHOOL_TYPE",
                                  SPL.types)],
                       SPL.prior)
  
  G38.SPL.schools <- SPL.schools[SPL.schools$WAEA_SCHOOL_TYPE %in% c(nonHS.types, paired.types),
                             c(if (adjusted)  "G38_SPL_ACCOUNTABILITY" else "G38_SPL",  "SCHOOL_YEAR", label.sum.by )]
  
  names(G38.SPL.schools)[1] <- "SPL"
  
  HS.SPL.schools <- SPL.schools[SPL.schools$WAEA_SCHOOL_TYPE %in% HS.types,
                            c(if (adjusted) "HS_SPL_ACCOUNTABILITY" else "HS_SPL", "SCHOOL_YEAR", label.sum.by )]
  
  names(HS.SPL.schools)[1] <- "SPL"
  
  All.SPL.schools <- SPL.schools[,
                             c(if (adjusted) "ALL_SPL_ACCOUNTABILITY" else "ALL_SPL", "SCHOOL_YEAR", label.sum.by )]
  
  names(All.SPL.schools)[1] <- "SPL"
  
  cats.sums <- do.call(cbind, lapply(list(G38.SPL.schools[G38.SPL.schools$SCHOOL_YEAR==school.year,c("SPL", "SCHOOL_ID")],
                             HS.SPL.schools[HS.SPL.schools$SCHOOL_YEAR==school.year,c("SPL", "SCHOOL_ID")],
                             All.SPL.schools[All.SPL.schools$SCHOOL_YEAR==school.year,c("SPL", "SCHOOL_ID")]),
                        function (SPL.schools) {
                          SPL.schools$SPL <- ifelse(!is.na(SPL.schools$SPL), SPL.schools$SPL, 0)
                          SPL.schools$SPL <- factor(as.character(SPL.schools$SPL), levels=c("1","2","3","4","0"), ordered=TRUE)
                          sums.SPL <- cast(SPL.schools, value = label.sum.by, SPL ~ ., 
                                           cast.FUN, add.missing=TRUE, margins=TRUE)[,2]
                        }))
  
  cats.compare <- do.call(rbind, lapply(list(G38.SPL.schools,
                             HS.SPL.schools,
                             All.SPL.schools),
                        function (schools) {
                          schools.wide <- reshape(schools, timevar="SCHOOL_YEAR", idvar="SCHOOL_ID", v.names="SPL", direction="wide")
                          names(schools.wide) <- c("SCHOOL_ID", "SPL.current", "SPL.prior")
                          schools.wide$SPL.current <- factor(schools.wide$SPL.current, levels=c("1","2","3", "4", NA), exclude=c(), ordered=TRUE)
                          schools.wide$SPL.prior <- factor(schools.wide$SPL.prior, levels=c("1","2","3", "4", NA), exclude=c(), ordered=TRUE)
                          cast(schools.wide, SPL.prior ~ SPL.current, add.missing=TRUE, value="SCHOOL_ID", fun.aggregate=length)
                          #as.matrix(sums.SPL)                                                    
                        })) 
  
  matches <- do.call(rbind, 
         lapply(0:2, 
         function (i) {
           offset <- (i*5) + 1
           m <- as.matrix(cats.compare[offset:(offset+3),2:5])
           c(round(((m[1,1] + m[2,2] + m[3,3] + m[4,4])/sum(m))*100,2),
             round(((m[1,1] + m[1,2] + 
                       m[2,1] + m[2,2] + m[2,3] + 
                       m[3,2] + m[3,3] + m[3,4] + 
                       m[4,3] + m[4,4])/sum(m))*100,2))
         }))
  rbind(cbind(cats.sums, NA, NA),
        as.matrix(cats.compare[2:6]),
        cbind(matches, NA,NA,NA))
  
}


put.cuts.nonHS <- function (cuts.df, school.year=current.school.year) {  
  
  cuts.df <- as.matrix(cuts.df)
  
  
  g38.achievement.cuts <<- as.vector(cuts.df[1,])
                                    
  
  g38.growth.cuts <<- as.vector(cuts.df[2,])
  
  g38.equity.cuts <<- as.vector(cuts.df[3,])
  
  #recalculate indicators
  schools$G38_ACHIEVEMENT_ALL_TARGET_LEVEL <<- findInterval(schools$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT,
                                                           g38.achievement.cuts) + 1
  
  schools$G38_GROWTH_TARGET_LEVEL <<- findInterval(schools$G38_GROWTH_MGP,
                                                  g38.growth.cuts) + 1
  
  schools$G38_EQUITY_TARGET_LEVEL <<- findInterval(schools$G38_EQUITY_MEAN,
                                                  g38.equity.cuts) + 1
  
  
  update.results.nonHS(school.year)
  
}


put.cuts.HS <- function (cuts.df, school.year=current.school.year) {  

  #update cuts
  cuts.df <- as.matrix(cuts.df)
  
  hs.achievement.cuts <<- as.vector(cuts.df[1,])
  
  hs.equity.cuts <<- as.vector(cuts.df[2,])
  
  hs.grad.rate.cuts <<- as.vector(cuts.df[3,])
  
  type.1.additional.readiness.cuts <<- as.vector(cuts.df[4,])
  
  #update indicators
  schools$HS_ACHIEVEMENT_TARGET_LEVEL <<- findInterval(schools$HS_ACHIEVEMENT_PERCENT_PROFICIENT,
                                                      hs.achievement.cuts) + 1
  
  schools$HS_EQUITY_TARGET_LEVEL <<- findInterval(schools$HS_EQUITY_MEAN,
                                                 hs.equity.cuts) + 1
  
  
  schools$CAT_4_YR_2013 <<- findInterval(schools$GRAD_RATE_4_YR.2012.13, hs.grad.rate.cuts) +1 
  schools$CAT_EXTENDED_2013 <<- findInterval(schools$GRAD_RATE_EXTENDED , hs.grad.rate.cuts) + 1
  
  
  schools[c("IMPROVE_TARGET_FOR_MEETS", 
            "IMPROVE_TARGET_FOR_EXCEED", 
            "IMPROVEMENT_TARGET",
            "IMPROVE_CAT_2013")] <<- t(apply(schools[grad.rate.labels],
                                              c(1),
                                              compute.grad.rate.cat, 
                                              hs.grad.rate.cuts, 
                                              grad.rate.precision, 
                                              grad.rate.labels))[,2:5]
  
  
  schools$HS_ADD_READINESS_CAT_TYPE1 <<- findInterval(schools$HS_ADD_READINESS_SCORE_TYPE1, 
                                                                         type.1.additional.readiness.cuts) + 1
  
  add.readiness.other.types <- do.call(cbind, lapply(c(2:4), function (type) {
    type.suffix <- paste("TYPE", type, sep="")
    compute.add.readiness.cat.and.cuts(schools, 
                                       additional.readiness.types[type], 
                                       type.suffix, 
                                       paste("HS_ADD_READINESS_SCORE", type.suffix, sep="_"),  
                                       type.1.additional.readiness.cuts)
  }))
  
  schools[names(add.readiness.other.types)] <<- add.readiness.other.types

  schools[c("HS_ADD_READINESS_CUT1", "HS_ADD_READINESS_CUT2", 
            "HS_ADD_READINESS_SCORE", "HS_ADD_READINESS_CAT")] <<- t(compute.add.readiness.overall(schools, type.1.additional.readiness.cuts))
  
  
  update.results.HS(school.year)
  
}


get.state.nonHS <- function (df, school.year=current.school.year) {
  
  state.values <- with(schools, schools[SCHOOL_YEAR==school.year &
                                        SCHOOL_ID==state.school.id,
                                        g38.indicator.labels])
  
  state.levels <- with(schools, schools[SCHOOL_YEAR==school.year&
                                          SCHOOL_ID==state.school.id,
                                        c(g38.target.level.labels, ifelse(report.adjusted.SPLs, g38.SPL.labels["accountability"], g38.SPL.labels["unmodified"]))])
  
  state.levels.labeled = sapply(names(state.levels), function(n) {
    if (grepl("SPL", n))
      SPL.labels[[state.levels[[n]]]]
    else
      indicator.labels[[state.levels[[n]]]]
    
  })
  
  as.matrix(cbind(c(state.values, NA), c(state.levels.labeled), c(state.levels)))
  
}

get.state.HS <- function (df, school.year=current.school.year) {
  
  state.values <- with(schools, schools[SCHOOL_YEAR==school.year &
                                          SCHOOL_ID==state.school.id,
                                        hs.indicator.labels])
  
    
  state.levels <- with(schools, schools[SCHOOL_YEAR==school.year&
                                          SCHOOL_ID==state.school.id,
                                        c(hs.target.level.labels, hs.overall.target.level.labels, ifelse(report.adjusted.SPLs, hs.SPL.labels["accountability"], hs.SPL.labels["unmodified"]))])
  
  state.levels.labeled = sapply(names(state.levels), function(n) {
    if (n ==  "HS_SPL")
      SPL.labels[[state.levels[[n]]]]
    else
      indicator.labels[[state.levels[[n]]]]
    
  })
  

  
  as.matrix(cbind(c(state.values, NA), 
                  c(state.levels.labeled), 
                  c(state.levels)))
  
}



put.indeces.HS <- function(indeces, cuts, school.year=current.school.year) {
  
  
  indeces <- as.matrix(indeces)
    

  #redefine the hathaway point system and compute hathaway index
  hathaway.eligibility.index.old <- hathaway.eligibility.index
  hathaway.eligibility.index <<-indeces[2,]

  #define map from old point system to new point system
  hathaway.eligibility.index.new <- hathaway.eligibility.index
  names(hathaway.eligibility.index.new) <- hathaway.eligibility.index.old
  

  #update the student frame
  hathaway.student$HATH_INDEX_SCORE <<- sapply(hathaway.student$HATH_INDEX_SCORE,
                                                   function (s) hathaway.eligibility.index.new[as.character(s)]) 
  
  #recompute the mean
  hath.subindicator <- compute.indicator.school(schools[c("SCHOOL_YEAR", "SCHOOL_ID")],
                                                hathaway.student,
                                        "STATUS_CODE",
                                        "HATH_INDEX_SCORE",                                      
                                        "_",
                                        function (g) c(N_SCORES=length(!is.na(g)),
                                                       MEAN=round(mean(g),0)),
                                        "HATH_INDEX_SCORE",
                                        school.key=c("SCHOOL_YEAR", "SCHOOL_ID"))
  
  #assign to schools data frame
  schools$HATH_INDEX_SCORE_MEAN <<- ifelse(hath.subindicator$HATH_INDEX_SCORE_N >= min.N.hath.eligibility,
                                           hath.subindicator$HATH_INDEX_SCORE_MEAN,
                                           NA)
  
  
  
  #recode tested readiness point system and recompute tested readiness
  tested.readiness.names <- names(tested.readiness)
  tested.readiness <<-  indeces[1,1:4]
  names(tested.readiness) <<- tested.readiness.names
  
  tested.readiness.points.new <- c(tested.readiness[1:2], 
                                   (tested.readiness[2] + tested.readiness[3])/2,
                                   tested.readiness[3:4])
  
  #create a mapping from the old point system to the new point system  
  tested.readiness.points.old <- alt.index.range   #alt.index.range has the interpolated point
  alt.index.range <<- tested.readiness.points.new
  names(tested.readiness.points.new) <- tested.readiness.points.old
    
  #map the old points to the new points
  tested.readiness.indicator$students.fay$TESTED_READINESS_INDEX_SCORE <<- ifelse(is.na(tested.readiness.indicator$students.fay$TESTED_READINESS_INDEX_SCORE), 
                                                                   NA, 
                                                                   tested.readiness.points.new[as.character(tested.readiness.indicator$students.fay$TESTED_READINESS_INDEX_SCORE)])
  #recompute the readiness indicator
  tested.readiness.school <- compute.indicator.school(schools[c("SCHOOL_YEAR", "SCHOOL_ID")],
                                                      tested.readiness.indicator$students.fay,
                                                      "TESTING_STATUS_CODE",
                                                      "TESTED_READINESS_INDEX_SCORE",                                      
                                                      "_",
                                                      function (g) {
                                                        c(N_sCORES=length(which(!is.na(as.numeric(g)))), 
                                                          MEAN=round(mean(as.numeric(g), na.rm=TRUE), precision.readiness))
                                                      },
                                                      "HS_TESTED_READINESS",
                                                      school.key=c("SCHOOL_YEAR", "SCHOOL_ID"))
  
  schools$HS_TESTED_READINESS_MEAN <<- ifelse(tested.readiness.school$HS_TESTED_READINESS_N >= min.N.tested.readiness,
                                             tested.readiness.school$HS_TESTED_READINESS_MEAN,
                                             NA)
  
  
  
  
  #set readiness weights.  This must come after we have done hathaway and tested readiness index!
  weight.names <- names(additional.readiness.weights)
  additional.readiness.weights <<- indeces[3,1:3]
  names(additional.readiness.weights) <<- weight.names
  
  #recompute all the additional readiness types
  
  add.readiness.map <- c("HS_TESTED_READINESS_MEAN",
                         "PERCENT_GD_9_CREDIT_MET",
                         "HATH_INDEX_SCORE_MEAN")
  names(add.readiness.map) <- names(additional.readiness.weights)
  
  add.readiness.types <- list(names(additional.readiness.weights)[c(1,2,3)],
                              names(additional.readiness.weights)[c(1)],
                              names(additional.readiness.weights)[c(1,2)],
                              names(additional.readiness.weights)[c(1,3)])
  
  
  compute.add.readiness.helper <- function (school, i, weights, precision) {
    school <- as.numeric(school)
    type <- school[[1]]
    if (is.na(type) | (type != 1 & type != i))
      NA
    else
      round(sum(weights[add.readiness.types[[i]]] * school[2:length(school)]), 
            precision)
  }
  
  schools[c("HS_ADD_READINESS_SCORE_TYPE1",
            "HS_ADD_READINESS_SCORE_TYPE2",
            "HS_ADD_READINESS_SCORE_TYPE3",
            "HS_ADD_READINESS_SCORE_TYPE4")] <<- do.call(cbind, 
                                                         lapply(c(1:4),
                                                                function (i) {                                                                  
                                                                  apply(schools[c("HS_ADD_READINESS_TYPE",
                                                                                  add.readiness.map[add.readiness.types[[i]]])],
                                                                        c(1),
                                                                        compute.add.readiness.helper, i, round(prop.table(additional.readiness.weights[add.readiness.types[[i]]]),2), precision.add.readiness)
                                                                  
                                                                }))
  
  
  #recompute target levels
  put.cuts.HS(cuts, school.year)

}