#presumes you've run process.R
source("constants.R")
source("function-defs.R")
source("initialize-schools.R")
source("reporting-defs.R")
source('db-functions.R')
source('const/private/db.R')


calc.perf.subgroups <- function (prior.performance) {
  
  id <- 1
  subject <- 2
  perf <-3
  
  below.proficient.priors <- prior.performance[prior.performance[[perf]] %in% c(1,2),]
  subgroup.students <- unique(below.proficient.priors[1])
  subgroup.students$CONSOLIDATED_SUBGROUP <- 'T'
  prior.performance <- merge(prior.performance, subgroup.students, all.x=TRUE)
  prior.performance$CONSOLIDATED_SUBGROUP <- ifelse(is.na(prior.performance$CONSOLIDATED_SUBGROUP), 'F', 'T')
  
  
  advanced.priors <- prior.performance[prior.performance[[perf]] %in% c(4),]
  advanced.priors.agg <- aggregate(advanced.priors[2],
                                   by=list("ID"=advanced.priors[[1]]),
                                   length)
  advanced.students <- advanced.priors.agg[advanced.priors.agg[[2]] == 2,][id]  #students with two advanced scores ~12% or current year's students, but ~16% of those with a prior test
  #advanced.students <- advanced.priors.agg[id] #anyone who got at least one advance score ~28%
  advanced.students$ADVANCED_SUBGROUP <- 'T'
  
  prior.performance <- merge(prior.performance, advanced.students, by.x=names(prior.performance)[id], by.y="ID", all.x=TRUE)
  prior.performance$ADVANCED_SUBGROUP <- ifelse(is.na(prior.performance$ADVANCED_SUBGROUP), 'F', 'T')
  #prop.table(table(cbind(prior.performance[c("ADVANCED_SUBGROUP", "CONSOLIDATED_SUBGROUP")], no.prior=is.na(prior.performance$STANDARD_PAWS_PERF_LEVEL_PRIOR)), useNA='ifany'))
  #prop.table(table(cbind(prior.performance[c("ADVANCED_SUBGROUP", "CONSOLIDATED_SUBGROUP")], no.prior=is.na(prior.performance$STANDARD_PAWS_PERF_LEVEL_PRIOR)), useNA='ifany'),1)
  prior.performance[names(prior.performance)[seq(names(prior.performance)) != perf]]
}

calc.paired.schools <- function (perf.df, school.year) {
  pairings <- school.pairing.lookup[[school.year]]
  
  sapply(perf.df$SCHOOL_ID,
         function(s) {
           
           paired.s <- pairings[[s]]
           if (is.null(paired.s))
             s
           else
             paired.s                              
         })
  
}

get.achievement.frame <-function (for.year, table.name, prior.achievement.label) {
  

  conn <- odbcConnect(dsn=db.dsn, uid=rdbms.name, pwd=rdbms.pwd)
  perf.df <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][[table.name]], 
                     as.is=as.is.vector(conn, data.tables.lexicon[[current.school.year]][[table.name]]))
  odbcClose(conn)
  
  perf.df <- perf.df[perf.df$GRADE_ENROLLED %in% c("03", "04", "05", "06", "07", "08") &
                       perf.df$SCHOOL_YEAR == for.year &
                       perf.df$TESTING_STATUS_CODE == 'T' &
                       perf.df$SUBJECT_CODE %in% c('MA', 'RE') &
                       grepl("standard", perf.df$TEST_TYPE, ignore.case=TRUE),]

  pairings <- school.pairing.lookup[[for.year]]
  
  sapply(names(pairings), function (s) nrow(perf.df[perf.df$SCHOOL_ID==s,]))
  unlist(lapply(pairings, function (s) nrow(perf.df[perf.df$SCHOOL_ID==s,])))
  
  perf.df$SCHOOL_ID <- calc.paired.schools(perf.df, for.year)
  
  sapply(names(pairings), function (s) nrow(perf.df[perf.df$SCHOOL_ID==s,]))
  unlist(lapply(pairings, function (s) nrow(perf.df[perf.df$SCHOOL_ID==s,])))

  perf.df$SGP <- as.numeric(perf.df$SGP)
  perf.df$AGP <- as.numeric(perf.df$AGP)
  perf.df$AGP <- ifelse(is.na(perf.df$AGP), NaN, perf.df$AGP)
  
  merge(perf.df, calc.perf.subgroups(perf.df[c("WISER_ID", "SUBJECT_CODE", prior.achievement.label)]), all.x=TRUE)
  
}

min.N.growth.report=10
round.to <- 1
growthCuts <- c(35, 65)

#load 2013-14 paws

paws.df <- get.achievement.frame(current.school.year, "growth.by.achievement", "STANDARD_PAWS_PERF_LEVEL_PRIOR")
paws.df.prior <- get.achievement.frame(prior.school.year, "achievement.prior", "ACHIEVEMENT_LEVEL_PRIOR")

#stats on availablity of prior scores by grade
prop.table(table(cbind(no.prior=is.na(paws.df$STANDARD_PAWS_PERF_LEVEL_PRIOR), paws.df["GRADE_ENROLLED"])))
prop.table(table(cbind(no.prior=is.na(paws.df.prior$ACHIEVEMENT_LEVEL_PRIOR), paws.df.prior["GRADE_ENROLLED"])))



##
growth.subgroups <- compute.growth.subgroups(paws.df, paws.df, schools, school.pairing.lookup[[current.school.year]], min.N.growth.report, round.to, growthCuts,  year.id.offset=1)

paws.df.prior$SCHOOL_YEAR <- current.school.year
growth.subgroups.lagged <- compute.growth.subgroups(paws.df.prior, paws.df, schools, school.pairing.lookup[[current.school.year]], min.N.growth.report, round.to, growthCuts, year.id.offset=1)

result <- growth.subgroups$result
head(result)


write.csv(result[result$SchoolYear==current.school.year,c('DataScope'
                                                          ,'DistrictId'
                                                          ,'DistrictName'
                                                          ,'SchoolId'
                                                          ,'SchoolName'
                                                          ,'SchoolYear'
                                                          ,'GradeEnrolled'
                                                          ,'Subgroup'
                                                          ,'SubgroupDescription'
                                                          ,'StudentMobility'
                                                          ,'SubjectCode'
                                                          ,'Subject'
                                                          ,'NLowGrowth'
                                                          ,'NTypicalGrowth'
                                                          ,'NHighGrowth'
                                                          ,'NGrowthScores'
                                                          ,'NStudentsGrowth'
                                                          ,'PLowGrowth'
                                                          ,'PTypicalGrowth'
                                                          ,'PHighGrowth'
                                                          ,'MGP'  #?
                                                          ,'PProficient'
                                                          ,'NProficient'
                                                          ,'NStudentsAchievement'
                                                          ,'NTestsAchievement')],file=paste("reporting/growth-distribution-by-school-subgroups-", current.school.year, ".csv", sep=""), na="", row.names=FALSE, quote=FALSE)

stats.table <- growth.subgroups$factored.result$Stats
write.csv(stats.table, file=paste("results/growth-stats-suppressed","-",current.school.year,".csv",sep=""), na="", row.names=FALSE, quote=FALSE)


factored.result <- growth.subgroups$factored.result
factored.result.lagged <- growth.subgroups.lagged$factored.result

head(factored.result.lagged$Stats)
head(factored.result$Stats)


factored.result$Stats$Lagged <- 0
factored.result.lagged$Stats$Lagged <- 1

factored.result$Stats <- rbind(factored.result$Stats, factored.result.lagged$Stats)
write.tables("OTTO", factored.result, "growth", append=TRUE, other.fact.key=c("Lagged"))
