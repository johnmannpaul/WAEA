#presumes you've run process.R
require(reshape)
require(plyr)
source("reporting-defs.R")
source('db-functions.R')
source('const/private/db.R')

min.N.growth.report=10
round.to <- 1
growthCuts <- c(35, 65)

#load 2013-14 paws
conn <- odbcConnect(dsn=db.dsn, uid=rdbms.name, pwd=rdbms.pwd)
paws.df <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][["growth.by.accountability"]], as.is=as.is.vector(conn, data.tables.lexicon[[current.school.year]][["growth.by.accountability"]]))
odbcClose(conn)

paws.df$SGP <- as.numeric(paws.df$SGP)
paws.df$AGP <- as.numeric(paws.df$AGP)
paws.df$AGP <- ifelse(is.na(paws.df$AGP), NaN, paws.df$AGP)
#paws.df$AGP <- ifelse(is.nan(paws.df$AGP), NA, paws.df$AGP)
nrow(paws.df)
paws.df <- paws.df[paws.df$GRADE_ENROLLED %in% c("03", "04", "05", "06", "07", "08"),]
nrow(paws.df)
paws.df <- merge(paws.df, with(equity.g38.indicator$students.fay,
                               equity.g38.indicator$students.fay[SCHOOL_YEAR==SCHOOL_YEAR_ORIGINAL, c("SCHOOL_YEAR","WISER_ID","SUBJECT_CODE", "CONSOLIDATED_SUBGROUP")]),
                 all.x=TRUE)
nrow(paws.df)
table(paws.df$CONSOLIDATED_SUBGROUP, useNA="ifany")
paws.df$CONSOLIDATED_SUBGROUP <- ifelse(is.na(paws.df$CONSOLIDATED_SUBGROUP), "F", "T")
table(paws.df$CONSOLIDATED_SUBGROUP, useNA="ifany")


paws.agg.sg <- aggregate.subgroup.combos.product(paws.df,
                                                 subgroups.intersecting = list(SCHOOL_FULL_ACADEMIC_YEAR=c(T='Yes', F="No")
                                                 ),
                                                 subgroups.nonintersecting = list(STUDENT_LUNCH=c(T='FRL', F='NFRL'),
                                                                                  IDEA_CURRENT_YEAR=c(T='IDEA', F='NIDEA'),                                                        
                                                                                  CONSOLIDATED_SUBGROUP=c(T='CSG', F='NCSG'),
                                                                                  ELL_CURRENT_YEAR=c(T='ELL', F='NELL'),
                                                                                  ETHNICITY=NA,
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
                                                                                     ETHNICITY=NA,
                                                                                     GENDER=NA,
                                                                                     TESTING_STATUS_CODE=c(T='All')
                                                    ),
                                                    obs=c(SGP="numeric",AGP="numeric",WISER_ID="character"),
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
                                                      MAGP=function (x) {
                                                        if (length(x) == 0) NA else median(x)
                                                      },
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
             
             "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP", "MAGP", "N_STUDENTS.y")] <- data.frame(t(apply(combo.agg[,c("NLowGrowth", "NTypicalGrowth", "NHighGrowth", "NGrowth", 
                                                                                                                               "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP", "MAGP", "N_STUDENTS.y")], c(1),                                                                                      
                                                                                                                  FUN=function (row) {
                                                                                                                    if (all(is.na(row)))
                                                                                                                      c(0,0,0,0,NA,NA,NA,NA, NA,0)
                                                                                                                    
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
                                                                                        
                                                                                        ALL='All Students', 
                                                                                        
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
                                                                                        A="Asian",
                                                                                        B="Black (not Hispanic)",
                                                                                        H="Hispanic",
                                                                                        I="American Indian/Alaska Native",
                                                                                        P="Native Hawaiian/Pacific Islander",
                                                                                        W="White (not Hispanic)",
                                                                                        Z="Two or More Races",
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

#assign 'All Schools' and 'All Districts' descriptors
combo.agg[combo.agg$SCHOOL_ID=='All',]$NAME <- 'All Schools'
combo.agg[combo.agg$DISTRICT_ID=='All',]$DISTRICT_NAME <- 'All Districts'
table(combo.agg$NAME, useNA="ifany")


#GRADE_ENROLLED is an ordered factor (should look into this)
combo.agg$GRADE_DESCRIPTION <- unlist(lapply(combo.agg$GRADE_ENROLLED, function (x) switch(as.character(x),
                                                                                           All="All Grades",
                                                                                           `03`="Third Grade", 
                                                                                           `04`="Fourth Grade", 
                                                                                           `05`="Fifth Grade", 
                                                                                           `06`="Sixth Grade", 
                                                                                           `07`="Seventh Grade",
                                                                                           `08`="Eigth Grade",
                                                                                           NA)))


combo.agg$SCHOOL_FAY_DESCRIPTION <- unlist(lapply(combo.agg$SCHOOL_FULL_ACADEMIC_YEAR, function (x) switch(x,
                                                                                                           All="All Students",
                                                                                                           Yes="Full Academic Year Only", 
                                                                                                           No="Not Full Academic Year Only",
                                                                                                           NA)))
ncol(combo.agg)
result <- combo.agg[,c("SCOPE", "DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                       "SCHOOL_YEAR", "GRADE_ENROLLED", "GRADE_DESCRIPTION", "SUBGROUP", "SUBGROUP_DESCRIPTION", "SCHOOL_FULL_ACADEMIC_YEAR", "SCHOOL_FAY_DESCRIPTION",
                       "SUBJECT_CODE", "SUBJECT_DESCRIPTION", 
                       "NLowGrowth", "NTypicalGrowth", "NHighGrowth", "NGrowth", "N_STUDENTS_GROWTH",
                       
                       "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP", "MAGP",
                       
                       "PERCENT_PROFICIENT", "N_PROFICIENT", "N_STUDENTS_ACHIEVEMENT", "N_TESTS")]
ncol(result) #should be same as above

names(result) <- c("DataScope"
                   ,"DistrictId"
                   ,"DistrictName"
                   ,"SchoolId"
                   ,"SchoolName"
                   ,"SchoolYear"
                   ,"GradeEnrolled"
                   ,"GradeDescription"
                   ,"Subgroup"
                   ,"SubgroupDescription"
                   ,"StudentMobility"
                   ,"StudentMobilityDescription"
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
                   ,"MAGP"
                   ,"PProficient"
                   ,"NProficient"
                   ,"NStudentsAchievement"
                   ,"NTestsAchievement")
#need to this represent FAY (not mobility)
#result$StudentMobility <- unlist(lapply(result$StudentMobility, 
#                                        function (x) switch(x, Yes='No', No='Yes', All='All', NA)))


head(result)
table(result$SchoolYear)
table(result$GradeEnrolled)
head(result[result$GradeEnrolled=='04',])
head(result[result$GradeEnrolled=='04' & result$SchoolId=='0101001' & result$Subgroup=='B' & result$SubjectCode=='RE',],50)
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

dim.spec <- list(Scope="DataScope", 
                 District=c(ID="DistrictId", "DistrictName"), 
                 School=c(ID="SchoolId", "SchoolName"), 
                 Grade=c(ID="GradeEnrolled", "GradeDescription"), 
                 Subgroup=c(ID="Subgroup", "SubgroupDescription"), 
                 Mobility=c(ID="StudentMobility", "StudentMobilityDescription"),
                 Subject=c(ID="SubjectCode", "Subject"))

factored.result <- factor.data.frame(result,                                      
                                     dim.spec,
                                     c(3),
                                     "SchoolYear")

#
disjoint.subgroups <- list(c('A','B','H','I','P','W','Z'),
                           c('CSG', 'NCSG'),
                           c('ELL', 'NELL'),
                           c('M','F'),
                           c('IDEA', 'NIDEA'),
                           c('FRL', 'NFRL'))

factored.result$Stats$Suppressed <- ifelse(factored.result$Stats$NStudentsGrowth < 
                                             min.N.growth.report, TRUE, FALSE)



s <- factored.result[[4]]
s[s$SchoolId==200,]
head(result[result$SchoolId=='1601005' & result$SchoolYear=='2012-13' ,])

g <- factored.result[[9]]
head(g[g$SchoolId=='1601005' & g$SchoolYearId==3,])

#suppress based on complementary subgroups
require(data.table)
stats.key <- c("SchoolYearId", "ScopeId", "DistrictId", "SchoolId", 
               "GradeEnrolled", "StudentMobility", "SubjectCode") 

stats.table <- data.table(factored.result$Stats,
                          key=stats.key)


stats.table[SchoolYearId==1 & ScopeId==1 & 
              DistrictId=='0101000' & SchoolId=='All' & 
              GradeEnrolled=='All' & SubjectCode=='All' & 
              StudentMobility =='All' & Suppressed==1,]

suppress.group <- function (a,b) {
  any(b[which(a>0)]==0)
}

table(stats.table$Suppressed)

lapply(disjoint.subgroups,
       function (g) {
         stats.table[Subgroup %in% g,
                     Suppressed:=Suppressed | suppress.group(NStudentsAchievement,PProficient), 
                     by=key(stats.table)]
       }
)

stats.table[SchoolYearId==1 & ScopeId==1 & 
              DistrictId=='0101000' & SchoolId=='All' & 
              GradeEnrolled=='All' & SubjectCode=='All' & 
              StudentMobility =='All',]


stats.table[SchoolYearId==1 & 
              ScopeId==1 & 
              DistrictId=='0101000' &
              GradeEnrolled=='05' & 
              StudentMobility=='All' & 
              SubjectCode=='MA' & SchoolId=='All' & 
              Subgroup %in% disjoint.subgroups[[1]]]

head(stats.table[Suppressed==TRUE,],50)
table(stats.table$Suppressed)

stats.table$Suppressed <- as.numeric(stats.table$Suppressed)
factored.result$Stats <- stats.table[Suppressed==0]

table(factored.result$Stats$Suppressed)

write.csv(factored.result$Stats, file="results/growth-stats-suppressed.csv", na="", row.names=FALSE, quote=FALSE)
##better have a good network...
#library("RODBC")
#write.tables("LocalReportDM", factored.result, "growth", "localreaderwriter", "")
#write.tables("WIGGUM", factored.result, "growth", "jpaul", "")
