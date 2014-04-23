#based on data/PAWS_ACCOUNTABILITY_LONG.csv
#and Science-2013 sheet in Achievement.xlsx
#save(paws, file="data/paws.Rdata")
#save(Science_Scores_2013, file="data/Science_Scores_2013.Rdata")


#c("SCHOOL_YEAR", "WISER_ID", "SUBJECT_CODE") is the key
load(file="data/paws.Rdata")

#c("SCHOOL_YEAR", "WISER_ID", "SUBJECT_CODE") is the key
load(file="data/Science_Scores_2013.Rdata")

#add GRADE_BAND column
paws$GRADE_BAND <- unlist(lapply(paws$GRADE_ENROLLED,
                                 function (x) {
                                   if (is.na(x))
                                     NA
                                   else
                                     band.lookup[[x]]                                                                                
                                 }))


#Define a numeric school year.  2010-11 -> 2011, 2011-12 -> 2012, 2012-13 -> 2013 
paws$YEAR <- as.numeric(sapply(paws$SCHOOL_YEAR, function (y) {
  strsplit(y,'-')[[1]][1]
})) + 1

#should be undefined


#more blanks than should be becuase of missing science
with(paws, table(paws[SCHOOL_YEAR=='2012-13' & SCHOOL_ID=='0101001',]$ACCOUNTABILITY_PERF_LEVEL))

#merge existing accountability levels with corrections for science
paws.merged <- merge(paws[,c("SCHOOL_YEAR", "WISER_ID", "SUBJECT_CODE", "ACCOUNTABILITY_PERF_LEVEL")],
                     Science_Scores_2013, by=c("SCHOOL_YEAR", "WISER_ID", "SUBJECT_CODE"), all=TRUE)

#give paws df the same order as the merged frame
paws <- with(paws, paws[order(SCHOOL_YEAR, WISER_ID, SUBJECT_CODE),])

#assign corrections
paws$ACCOUNTABILITY_PERF_LEVEL <- ifelse(!is.na(paws.merged$ACCOUNTABILITY_PERF_LEVEL.y), 
                                         paws.merged$ACCOUNTABILITY_PERF_LEVEL.y, 
                                         paws.merged$ACCOUNTABILITY_PERF_LEVEL.x)

##reorder paws by school
paws <- with(paws, paws[order(SCHOOL_YEAR, SCHOOL_ID, WISER_ID, SUBJECT_CODE),])

#check corrections
with(paws, table(paws[SCHOOL_YEAR=='2012-13' & SCHOOL_ID=='0101001',]$ACCOUNTABILITY_PERF_LEVEL))




#Students with testing status code 'N' do not count for achievement.  We'll be working with this frame for the the nonHS calculations
paws.df <- paws[paws$SCHOOL_FULL_ACADEMIC_YEAR=="T" & paws$TESTING_STATUS_CODE == "T" & paws$GRADE_ENROLLED != "11", ]

paws.df$SGP <- as.numeric(paws.df$SGP)


table(paws.df$SCHOOL_FULL_ACADEMIC_YEAR)
table(paws.df$TESTING_STATUS_CODE)
table(paws.df$GRADE_ENROLLED)
#paws.df$AYP_FINAL_PERF_LEVEL <- as.numeric(paws.df$AYP_FINAL_PERF_LEVEL)


#Remap school ids, taking pairing into consideration
school.ids.paired <- apply(paws.df[,c("SCHOOL_YEAR", "SCHOOL_ID")], 
                           c(1), 
                           function (school) {
                             pairing <- school.pairing.lookup[[school[["SCHOOL_YEAR"]]]]
                             paired.school <- pairing[[school[["SCHOOL_ID"]]]]
                             ifelse(is.null(paired.school), school[["SCHOOL_ID"]], paired.school)
                             
                           })

##some validations
school.ids.compare <- data.frame(school.id.paired=school.ids.paired, school.id=paws.df[,c("SCHOOL_ID")])
names(school.ids.compare) <- c("school.id.paired", "school.id")
changes <- school.ids.compare[school.ids.compare$school.id %in% names(school.pairing.lookup$`2010-11`),]
no.changes <- school.ids.compare[!(school.ids.compare$school.id %in% names(school.pairing.lookup$`2010-11`)),]
nrow(no.changes[no.changes$school.id != no.changes$school.id.paired,])
head(changes)
##end validations

nrow(paws.df[paws.df$SCHOOL_ID=='1001006' & paws.df$GRADE_ENROLLED=='03',])
#reassign the scores of the students in paired schools (ie. third graders) to the appropriate parent school
paws.df$SCHOOL_ID <- school.ids.paired
#should be same number as just above
nrow(paws.df[paws.df$SCHOOL_ID=='1001002' & paws.df$GRADE_ENROLLED=='03',])


