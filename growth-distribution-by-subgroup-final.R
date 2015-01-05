require(reshape)
require(plyr)
source("constants.R")
source("function-defs.R")
source("initialize-schools.R")
source("initialize-paws.R")
source("reporting-defs.R")

min.N.growth.report=10
current.school.year<- '2012-13'
round.to <- 1
growthCuts <- c(35, 65)

paws.df <- paws[paws$TESTING_STATUS_CODE == "T" & paws$GRADE_ENROLLED != "11" & paws$SUBJECT_CODE %in% c('RE', 'MA') & paws$STANDARD_PAWS_PERF_LEVEL %in% c('1','2','3','4'), ]
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
result <- combo.agg[combo.agg$SCHOOL_YEAR==current.school.year,c("SCOPE", "DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
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
head(result[result$GradeEnrolled=='04' & result$SchoolCode=='0101001' & result$Subgroup=='B' & result$SubjectCode=='RE',],50)


#paired schools obtain the same values as their parent schools
sapply(names(pairings), function (s) nrow(result[result$SchoolId==s,]))
unlist(lapply(pairings, function (s) nrow(result[result$SchoolId==s,])))
pairing.helper <- function (s) {
  
  school.name <- schools[schools$SCHOOL_YEAR == current.school.year & schools$SCHOOL_ID==s,"NAME"]
  if (length(school.name) == 0)
    return(NULL)
  
  parent.school <- pairings[[s]]
  parent.recs <-result[result$SchoolId == parent.school,]
  parent.recs$SchoolId <- s
  parent.recs$SchoolName <- school.name
  parent.recs
}
result <- rbind(result[!(result$SchoolId %in% names(pairings)),], do.call(rbind, lapply(names(pairings),
                                                                                        pairing.helper)))
sapply(names(pairings), function (s) nrow(result[result$SchoolId==s,]))
unlist(lapply(pairings, function (s) nrow(result[result$SchoolId==s,])))


write.csv(result,file="reporting/growth-distribution-by-school-subgroups.csv", na="", row.names=FALSE, quote=FALSE)

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
                                     "SchoolYear",
                                     year.id.offset=2)

#
disjoint.subgroups <- list(c('A','B','H','I','P','W','Z'),
                           c('CSG', 'NCSG'),
                           c('ELL', 'NELL'),
                           c('M','F'),
                           c('IDEA', 'NIDEA'),
                           c('FRL', 'NFRL'))

factored.result$Stats$Suppressed <- ifelse(factored.result$Stats$NStudentsGrowth >0 & factored.result$Stats$NStudentsGrowth < 
                                             min.N.growth.report, TRUE, FALSE)




#suppress based on complementary subgroups for percent proficient
require(data.table)
stats.key <- c("SchoolYearId", "ScopeId", "DistrictId", "SchoolId", 
               "Subgroup", "GradeEnrolled", "StudentMobility", "SubjectCode") 

stats.table <- data.table(factored.result$Stats,
                          key=stats.key)


stats.table[SchoolYearId==1 & ScopeId==1 & 
              DistrictId=='0101000' & SchoolId=='All' & 
              GradeEnrolled=='All' & SubjectCode=='All' & 
              StudentMobility =='All' & Suppressed==1,]

# suppress.group <- function (a,b) {
#   any(b[which(a>0)]==0)
# }

suppress.group <- function (s, a) {
  
  if (any(s)) 
    sum(a[which(s)]) > 0 & sum(a[which(s)]) < min.N.growth.report
  else
    FALSE
}
table(stats.table$Suppressed)

lapply(disjoint.subgroups,
       function (g) {
         stats.table[Subgroup %in% g,
                     Suppressed:=Suppressed | suppress.group(Suppressed,NStudentsGrowth), 
                     by=c("SchoolYearId", "ScopeId", "DistrictId", "SchoolId", 
                          "GradeEnrolled", "StudentMobility", "SubjectCode") ]
       }
)

table(stats.table$Suppressed)

stats.table[StudentMobility %in% c('No','Yes'),
            Suppressed:=Suppressed | suppress.group(Suppressed,NStudentsGrowth), 
            by=c("SchoolYearId", "ScopeId", "DistrictId", "SchoolId", 
                 "Subgroup", "GradeEnrolled", "SubjectCode")]

table(stats.table$Suppressed)
table(stats.table$GradeEnrolled)
stats.table[GradeEnrolled %in% c("03", "04", "05", "06", "07", "08"),
            Suppressed:=Suppressed | suppress.group(Suppressed,NStudentsGrowth), 
            by=c("SchoolYearId", "ScopeId", "DistrictId", "SchoolId", 
                 "Subgroup", "StudentMobility", "SubjectCode")]

table(stats.table$Suppressed)

stats.table[SchoolYearId==1 & ScopeId==1 & 
              DistrictId=='0101000' & SchoolId=='All' & 
              GradeEnrolled=='All' & SubjectCode=='All' & 
              StudentMobility =='All',]


stats.table[SchoolYearId==1 &
              DistrictId=='0101000' & SchoolId=='0101005',]


stats.table[SchoolYearId==1 & 
              ScopeId==1 & 
              DistrictId=='0101000' &
              GradeEnrolled=='05' & 
              StudentMobility=='All' & 
              SubjectCode=='MA' & SchoolId=='All' & 
              Subgroup %in% disjoint.subgroups[[1]]]

head(stats.table[Suppressed==TRUE,],50)
table(stats.table$Suppressed)


#convert back to data.frame to apply capping
stats.table <- data.frame(stats.table)

tail(stats.table[stats.table$SchoolYearId==4 & stats.table$ScopeId==1 & 
                   stats.table$DistrictId=='0101000',],50)

stats.table$NStudentsGrowthCap <- sapply(stats.table$NStudentsGrowth,
                                         function (a) {
                                           
                                           if (a == 0)
                                             return("0")
                                           
                                           low <- if (a < 5) 1 else (a%/%5)*5
                                           high <- ((a+5)%/%5)*5  - 1
                                           
                                           paste(low,high, sep="-")                                                                                                                              
                                         })

stats.table$NStudentsAchievementCap <- sapply(stats.table$NStudentsAchievement,
                                              function (a) {
                                                
                                                if (a == 0)
                                                  return("0")
                                                
                                                low <- if (a < 5) 1 else (a%/%5)*5
                                                high <- ((a+5)%/%5)*5  - 1
                                                
                                                paste(low,high, sep="-")                                                                                                                              
                                              })


stats.table[c("PProficientCap","PProficientCapOperator")] <- t(apply(stats.table[c("NStudentsAchievementCap",
                                                                                   "PProficient",
                                                                                   "Suppressed")],
                                                                     c(1),
                                                                     function (row) {
                                                                       par <- as.numeric(row[["PProficient"]])
                                                                       aac <- row[["NStudentsAchievementCap"]]
                                                                       suppressd <- as.numeric(row[["Suppressed"]])
                                                                       
                                                                       if (is.na(par))
                                                                         return(c(NA, NA))
                                                                       
                                                                       cap <- switch(aac, `1-4`=100, `5-9`=20, `10-14`=10, `15-19`=6.7, 5)
                                                                       
                                                                       if (par <= cap)
                                                                         c(cap, "<=")
                                                                       else {
                                                                         
                                                                         if (par >= 100-cap)
                                                                           c(100-cap, ">=")
                                                                         else
                                                                           c(par, "==")
                                                                         
                                                                       }                                                                               
                                                                     }))

stats.table$PProficientCap <- as.numeric(stats.table$PProficientCap)

table(stats.table$PProficientCapOperator, useNA="ifany")
table(is.na(stats.table$PProficient))


stats.table$Suppressed <- as.numeric(stats.table$Suppressed)

head(stats.table)

write.csv(stats.table, file=paste("results/growth-stats-suppressed","-",current.school.year,".csv",sep=""), na="", row.names=FALSE, quote=FALSE)

factored.result$Stats <- stats.table
write.tables("OTTO", factored.result, "growth")
