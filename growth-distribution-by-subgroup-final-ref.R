source("constants.R")
source("function-defs.R")
source("reporting-defs.R")
source("initialize-schools.R")

min.N.growth.report=10
current.school.year<- '2012-13'
prior.school.year <- '2011-12'
round.to <- 1
growthCuts <- c(35, 65)

load(file="data/paws.Rdata")

paws.df <- paws[paws$TESTING_STATUS_CODE == "T" & paws$GRADE_ENROLLED != "11" & paws$SUBJECT_CODE %in% c('RE', 'MA') & paws$STANDARD_PAWS_PERF_LEVEL %in% c('1','2','3','4') & paws$SCHOOL_YEAR==current.school.year, ]
paws.df$SGP <- as.numeric(paws.df$SGP)
paws.df$AGP <- as.numeric(paws.df$AGP)

pairings <- school.pairing.lookup[[current.school.year]]
sapply(names(pairings), function (s) nrow(paws.df[paws.df$SCHOOL_ID==s,]))
unlist(lapply(pairings, function (s) nrow(paws.df[paws.df$SCHOOL_ID==s,])))
paws.df$SCHOOL_ID <- sapply(paws.df$SCHOOL_ID,
                            function(s) {
                              
                              paired.s <- pairings[[s]]
                              if (is.null(paired.s))
                                s
                              else
                                paired.s
                              
                            })
sapply(names(pairings), function (s) nrow(paws.df[paws.df$SCHOOL_ID==s,]))
unlist(lapply(pairings, function (s) nrow(paws.df[paws.df$SCHOOL_ID==s,])))


#paws.df$SCHOOL_ID <- as.factor(paws.df$SCHOOL_ID) #so we have an entry for every school even when we pair down the dataset to include just the growth scores
#paws.df$GRADE_ENROLLED <- as.factor(paws.df$GRADE_ENROLLED) #so we have an entry for every grade even when we pair down the dataset to include just the growth scores

below.proficient.priors <- paws.df[paws.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2),]
subgroup.students <- unique(below.proficient.priors[, c("SCHOOL_YEAR", "WISER_ID")])
subgroup.students$CONSOLIDATED_SUBGROUP <- rep('T', nrow(subgroup.students))
nrow(paws.df)
paws.df <- merge(paws.df, subgroup.students, all.x=TRUE)
nrow(paws.df)
paws.df$CONSOLIDATED_SUBGROUP <- ifelse(is.na(paws.df$CONSOLIDATED_SUBGROUP), 'F', 'T')
table(paws.df[c("SCHOOL_YEAR", "CONSOLIDATED_SUBGROUP")], useNA='ifany')

advanced.priors <- paws.df[paws.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(4),]
advanced.students <- unique(advanced.priors[, c("SCHOOL_YEAR", "WISER_ID")])
advanced.students$ADVANCED_SUBGROUP <- 'T'
nonadvanced.priors <- paws.df[paws.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2,3),]
nonadvanced.students <- unique(nonadvanced.priors[, c("SCHOOL_YEAR", "WISER_ID")])
nonadvanced.students$ADVANCED_SUBGROUP <- 'F'
advanced.students <- merge(advanced.students, nonadvanced.students, all.x=TRUE, by=c("SCHOOL_YEAR", "WISER_ID"))
nrow(advanced.students)
advanced.students <- advanced.students[is.na(advanced.students$ADVANCED_SUBGROUP.y),]
names(advanced.students)[1:3] <- c("SCHOOL_YEAR", "WISER_ID", "ADVANCED_SUBGROUP")
nrow(advanced.students)
paws.df <- merge(paws.df, advanced.students[c("SCHOOL_YEAR", "WISER_ID", "ADVANCED_SUBGROUP")], all.x=TRUE)
paws.df$ADVANCED_SUBGROUP <- ifelse(is.na(paws.df$ADVANCED_SUBGROUP), 'F', 'T')
table(paws.df[c("SCHOOL_YEAR", "ADVANCED_SUBGROUP")], useNA='ifany')

growth.subgroups <- compute.growth.subgroups(paws.df, paws.df, schools, pairings, min.N.growth.report, round.to, growthCuts)


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

factored.result <- growth.subgroups$factored.result
stats.table <- factored.result$Stats
write.csv(stats.table, file=paste("results/growth-stats-suppressed","-",current.school.year,".csv",sep=""), na="", row.names=FALSE, quote=FALSE)

#now do it with lagged achievement
paws.df.lagged <- paws[paws$TESTING_STATUS_CODE == "T" & paws$GRADE_ENROLLED != "11" & paws$SUBJECT_CODE %in% c('RE', 'MA') & paws$STANDARD_PAWS_PERF_LEVEL %in% c('1','2','3','4') & paws$SCHOOL_YEAR==prior.school.year, ]
head(paws.df.lagged)
paws.df.lagged$SCHOOL_YEAR <- current.school.year

below.proficient.priors <- paws.df.lagged[paws.df.lagged$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2),]
subgroup.students <- unique(below.proficient.priors[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])
subgroup.students$CONSOLIDATED_SUBGROUP <- rep('T', nrow(subgroup.students))
nrow(paws.df.lagged)
paws.df.lagged <- merge(paws.df.lagged, subgroup.students, all.x=TRUE)
nrow(paws.df.lagged)
paws.df.lagged$CONSOLIDATED_SUBGROUP <- ifelse(is.na(paws.df.lagged$CONSOLIDATED_SUBGROUP), 'F', 'T')


advanced.priors <- paws.df.lagged[paws.df.lagged$ACHIEVEMENT_LEVEL_PRIOR %in% c(4),]
advanced.students <- unique(advanced.priors[, c("SCHOOL_YEAR", "WISER_ID")])
advanced.students$ADVANCED_SUBGROUP <- 'T'
nonadvanced.priors <- paws.df.lagged[paws.df.lagged$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2,3),]
nonadvanced.students <- unique(nonadvanced.priors[, c("SCHOOL_YEAR", "WISER_ID")])
nonadvanced.students$ADVANCED_SUBGROUP <- 'F'
advanced.students <- merge(advanced.students, nonadvanced.students, all.x=TRUE, by=c("SCHOOL_YEAR", "WISER_ID"))
nrow(advanced.students)
advanced.students <- advanced.students[is.na(advanced.students$ADVANCED_SUBGROUP.y),]
names(advanced.students)[1:3] <- c("SCHOOL_YEAR", "WISER_ID", "ADVANCED_SUBGROUP")
nrow(advanced.students)
paws.df.lagged <- merge(paws.df.lagged, advanced.students[c("SCHOOL_YEAR", "WISER_ID", "ADVANCED_SUBGROUP")], all.x=TRUE)
paws.df.lagged$ADVANCED_SUBGROUP <- ifelse(is.na(paws.df.lagged$ADVANCED_SUBGROUP), 'F', 'T')
table(paws.df.lagged[c("SCHOOL_YEAR", "ADVANCED_SUBGROUP")], useNA='ifany')

growth.subgroups.lagged <- compute.growth.subgroups(paws.df.lagged, paws.df, schools, pairings, min.N.growth.report, round.to, growthCuts)
factored.result.lagged <- growth.subgroups.lagged$factored.result

head(factored.result.lagged$Stats)
head(factored.result$Stats)

factored.result$Stats$Lagged <- 0
factored.result.lagged$Stats$Lagged <- 1

factored.result$Stats <- rbind(factored.result$Stats, factored.result.lagged$Stats)
write.tables("OTTO", factored.result, "growth", other.fact.key=c("Lagged"))
