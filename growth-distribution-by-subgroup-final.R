require(reshape)
source("constants.R")
source("function-defs.R")
source("initialize-schools.R")
source("initialize-paws.R")
source("reporting-defs.R")

current.school.year<- '2012-13'
round.to <- 1
growthCuts <- c(35, 65)

paws.df <- paws[paws$TESTING_STATUS_CODE == "T" & paws$GRADE_ENROLLED != "11" & paws$SUBJECT_CODE %in% c('RE', 'MA') & paws$STANDARD_PAWS_PERF_LEVEL %in% c('1','2','3','4'), ]
paws.df$SGP <- as.numeric(paws.df$SGP)

#paws.df$SCHOOL_ID <- as.factor(paws.df$SCHOOL_ID) #so we have an entry for every school even when we pair down the dataset to include just the growth scores
#paws.df$GRADE_ENROLLED <- as.factor(paws.df$GRADE_ENROLLED) #so we have an entry for every grade even when we pair down the dataset to include just the growth scores

below.proficient.priors <- paws.df[paws.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2),]
subgroup.students <- unique(below.proficient.priors[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])
subgroup.students$CONSOLIDATED_SUBGROUP <- rep('T', nrow(subgroup.students))
nrow(paws.df)
paws.df <- merge(paws.df, subgroup.students, all.x=TRUE)
nrow(paws.df)
paws.df$CONSOLIDATED_SUBGROUP <- ifelse(is.na(paws.df$CONSOLIDATED_SUBGROUP), 'F', 'T')
table(paws.df$CONSOLIDATED_SUBGROUP, useNA='ifany')


paws.agg.sg <- aggregate.subgroup.combos.product(paws.df,
                                                 subgroups.intersecting = list(SCHOOL_FULL_ACADEMIC_YEAR=c(T='Yes', F="No")
                                                 ),
                                                 subgroups.nonintersecting = list(STUDENT_LUNCH=c(T='FRL', F='NFRL'),
                                                                                  IDEA_CURRENT_YEAR=c(T='IDEA', F='NIDEA'),                                                        
                                                                                  CONSOLIDATED_SUBGROUP=c(T='CSG', F='NCSG'),
                                                                                  ELL_CURRENT_YEAR=c(T='ELL', F='NELL'),
                                                                                  GENDER=NA,
                                                                                  TESTING_STATUS_CODE=c(T='All')
                                                 ),
                                                 obs=c(STANDARD_PAWS_PERF_LEVEL="character",WISER_ID="character"))


with(paws.agg.sg, paws.agg.sg[SCOPE %in% c("STATE_SCHOOL", "STATE_DISTRICT") & SCHOOL_YEAR==current.school.year & GRADE_ENROLLED=='All',])


growth.agg.sg <- aggregate.subgroup.combos.product( paws.df[!is.na(paws.df$SGP),],
                                                    subgroups.intersecting = list(SCHOOL_FULL_ACADEMIC_YEAR=c(T='Yes', F="No")
                                                    ),
                                                    subgroups.nonintersecting = list(STUDENT_LUNCH=c(T='FRL', F='NFRL'),
                                                                                     IDEA_CURRENT_YEAR=c(T='IDEA', F='NIDEA'),                                                        
                                                                                     CONSOLIDATED_SUBGROUP=c(T='CSG', F='NCSG'),
                                                                                     ELL_CURRENT_YEAR=c(T='ELL', F='NELL'),
                                                                                     GENDER=NA,
                                                                                     TESTING_STATUS_CODE=c(T='All')
                                                    ),
                                                    obs=c(SGP="numeric",WISER_ID="character"),
                                                    FUN=c(
                                                      function (x) 
                                                        c(NLowGrowth = sum(ifelse(x <= growthCuts[1], 1, 0)),
                                                          NTypicalGrowth = sum(ifelse(growthCuts[1] < x & x <= growthCuts[2], 1, 0)),
                                                          NHighGrowth = sum(ifelse(growthCuts[2] < x, 1, 0)),
                                                          NGrowth=length(x),
                                                          PLowGrowth = if (length(x) == 0) NA else round((sum(ifelse(x <= growthCuts[1], 1, 0))/length(x))*100,round.to),
                                                          PTypicalGrowth = if (length(x) == 0) NA else round((sum(ifelse(growthCuts[1] < x & x <= growthCuts[2], 1, 0))/length(x))*100,round.to),
                                                          PHighGrowth = if (length(x) == 0) NA else round((sum(ifelse(growthCuts[2] < x, 1, 0))/length(x))*100,round.to),
                                                          MGP = if (length(x) == 0) NA else median(x)),
                                                      N_STUDENTS=function (x) {
                                                        length(unique(x))
                                                      }))

with(growth.agg.sg, growth.agg.sg[SCOPE %in% c("STATE_SCHOOL", "STATE_DISTRICT") & SCHOOL_YEAR==current.school.year & GRADE_ENROLLED=='All',])


nrow(paws.agg.sg)
nrow(growth.agg.sg)
combo.agg <- merge (paws.agg.sg, growth.agg.sg, by=c("SCOPE","SCHOOL_YEAR", "GRADE_ENROLLED", "SUBGROUP", "SUBJECT_CODE", "DISTRICT_ID", "SCHOOL_ID", "SCHOOL_FULL_ACADEMIC_YEAR"), all.x=TRUE)
#should  be the same as growth.agg
nrow(combo.agg)

combo.agg[,c("NLowGrowth", "NTypicalGrowth", "NHighGrowth", "NGrowth", 
             "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP", "N_STUDENTS.y")] <- data.frame(t(apply(combo.agg[,c("NLowGrowth", "NTypicalGrowth", "NHighGrowth", "NGrowth", 
                                                                                                                    "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP", "N_STUDENTS.y")], c(1),                                                                                      
                                                                                                   FUN=function (row) {
                                                                                                     if (all(is.na(row)))
                                                                                                       c(0,0,0,0,NA,NA,NA,NA,0)
                                                                                                     else
                                                                                                       row                                                          
                                                                                                   })))
#rename the two n_students columns
names(combo.agg)[names(combo.agg) %in% c("N_STUDENTS.x", "N_STUDENTS.y")] <- c("N_STUDENTS_ACHIEVEMENT", "N_STUDENTS_GROWTH")
head(combo.agg)


table(combo.agg$SCOPE)
combo.agg <- with(combo.agg, combo.agg[!(SCOPE %in% c("STATE_DISTRICT")),])
combo.agg$SCOPE <- ifelse(combo.agg$SCOPE == "STATE_SCHOOL", "STATE", combo.agg$SCOPE)
table(combo.agg$SCOPE)

#need to get in district name, school name, subject description
combo.agg$SUBJECT_DESCRIPTION <- unlist(lapply(combo.agg$SUBJECT_CODE, function (s) { switch(as.character(s), RE="Reading", MA="Mathematics", "Reading & Math") }))
table(combo.agg$SUBJECT_DESCRIPTION, useNA="ifany")

with(combo.agg, combo.agg[SCOPE %in% c("STATE") & SCHOOL_YEAR==current.school.year & GRADE_ENROLLED=='All',])

table(combo.agg$SUBGROUP, useNA="ifany")
combo.agg$SUBGROUP_DESCRIPTION <- unlist(lapply(combo.agg$SUBGROUP, function (x) switch(x,
                                                                                       All='All Students', 
                                                                                       CSG="Consolidated Subgroup", 
                                                                                       FRL="Free And Reduced Lunch", 
                                                                                       IDEA="Students with Disabilities",
                                                                                       NCSG="Not Consolidated Subgroup",
                                                                                       NFRL="Not Free and Reduced Lunch",
                                                                                       NIDEA="Students without Disabilities",
                                                                                       ELL="English Language Learner",
                                                                                       NELL="Not English Language Learner",
                                                                                       M="Male",
                                                                                       F="Female",
                                                                                       NA)))
table(combo.agg$SUBGROUP_DESCRIPTION, useNA="ifany")

nrow(combo.agg)
combo.agg <- merge(combo.agg, schools[,c("SCHOOL_YEAR", "SCHOOL_ID", "NAME")], all.x=TRUE)
combo.agg <- merge(combo.agg, unique(schools[,c("SCHOOL_YEAR", "DISTRICT_ID", "DISTRICT_NAME")]), all.x=TRUE)
nrow(combo.agg)
combo.agg <- with(combo.agg, combo.agg[(SCOPE == 'SCHOOL' & !is.na(NAME)) | (SCOPE == 'DISTRICT' & !is.na(DISTRICT_NAME)) | SCOPE=='STATE',])  #
nrow(combo.agg)  #should be same as above
head(combo.agg)
head(combo.agg[combo.agg$SCHOOL_ID=='All',])  #will have NA school name
head(combo.agg[combo.agg$DISTRICT_ID=='All',])  #will have NA district name

ncol(combo.agg)
result <- combo.agg[,c("SCOPE", "DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                       "SCHOOL_YEAR", "GRADE_ENROLLED", "SUBGROUP", "SUBGROUP_DESCRIPTION", "SCHOOL_FULL_ACADEMIC_YEAR", "SUBJECT_CODE", "SUBJECT_DESCRIPTION", 
                       "NLowGrowth", "NTypicalGrowth", "NHighGrowth", "NGrowth", "N_STUDENTS_GROWTH",
                       "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP",
                       "PERCENT_PROFICIENT", "N_PROFICIENT", "N_STUDENTS_ACHIEVEMENT", "N_TESTS")]
ncol(result) #should be same as above

names(result) <- c("DataScope"
                   ,"DistrictId"
                   ,"DistrictName"
                   ,"SchoolId"
                   ,"SchoolName"
                   ,"SchoolYear"
                   ,"GradeEnrolled"
                   ,"Subgroup"
                   ,"SubgroupDescription"
                   ,"StudentMobility"
                   ,"SubjectCode"
                   ,"Subject"
                   ,"NLowGrowth"
                   ,"NTypicalGrowth"
                   ,"NHighGrowth"
                   ,"NGrowthScores"
                   ,"NStudentsGrowth"
                   ,"PLowGrowth"
                   ,"PTypicalGrowth"
                   ,"PHighGrowth"
                   ,"MGP"
                   ,"PProficient"
                   ,"NProficient"
                   ,"NStudentsAchievement"
                   ,"NTestsAchievement")

result$StudentMobility <- unlist(lapply(result$StudentMobility, 
                                         function (x) switch(x, Yes='Yes', No='No', All='All Students', NA)))
head(result)
table(result$SchoolYear)
table(result$GradeEnrolled)
head(result[result$GradeEnrolled=='04',])
write.csv(result[result$SchoolYear==current.school.year,],file="reporting/growth-distribution-by-school-subgroups.csv", na="", row.names=FALSE, quote=FALSE)
