#save(schools, file="data/schools.orig.Rdata")


load(file="data/schools.orig.Rdata")


table(schools$GRADES_SERVED)
schools$GRADES_SERVED <- ifelse(schools$TYPE_CODE != '02', NA, schools$GRADES_SERVED)

schools$WAEA_SCHOOL_TYPE <- unlist(lapply(schools$GRADES_SERVED,
                                          function (x) {
                                            if (is.na(x))
                                              NA
                                            else
                                              type.lookup[[x]]                                                                                
                                          }))

schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_YEAR", "SCHOOL_ID")], c(1),
                                             function (school) {
                                               pairing.lookup <- school.pairing.lookup[[school[["SCHOOL_YEAR"]]]]
                                               paired.school.id <- pairing.lookup[[school[["SCHOOL_ID"]]]]                                               
#                                                if (is.null(paired.school.id)) {#if forward lookup is null then do a reverse lookup...assumption is that the mapping is 1-1
#                                                  idx <- which(pairing.lookup == school[["SCHOOL_ID"]])
#                                                  if (length(idx) > 0)
#                                                    paired.school.id <- names(pairing.lookup)[[idx]]
#                                                }
#                                                  
                                               if (is.null(paired.school.id))                                                 
                                                 result <- c(as.character(NA), as.character(NA))
                                               else {
                                                 paired.school.name <- with(schools, schools[SCHOOL_ID==paired.school.id & SCHOOL_YEAR == school[["SCHOOL_YEAR"]], "NAME" ])
                                                 result <- c(paired.school.id, paired.school.name)
                                               }
                                               names(result) <- c("PAIRED_SCHOOL_ID", "PAIRED_SCHOOL_NAME")
                                               result
                                             }))))

#fix some of the high schools manually
#Even though they appear as type 5, there are so few 7th and 8th graders historically, that they would not even meet minimum N requirements.
#Campbell County High School is labeled at type 4.  It is not, however, a PK-12.  It is a 9-12 with a PK attached to it.
with(schools, schools[SCHOOL_ID=='0301055',]$WAEA_SCHOOL_TYPE <<- 2) #Campbell County High School
with(schools, schools[SCHOOL_ID=='0601058',]$WAEA_SCHOOL_TYPE <<- 2) #Bear Lodge High School 
with(schools, schools[SCHOOL_ID=='1201056',]$WAEA_SCHOOL_TYPE <<- 2) #Kemmerer Alternative School
with(schools, schools[SCHOOL_ID=='1301058',]$WAEA_SCHOOL_TYPE <<- 2) #Roosevelt High School


schools$YEAR <- as.numeric(sapply(schools$SCHOOL_YEAR, function (y) {
  strsplit(y,'-')[[1]][1]
})) + 1

lapply(c("2009-10", "2010-11", "2011-12", "2012-13"),
       function(year) table(schools[schools$SCHOOL_YEAR == year,]$TYPE_CODE))
#only want schools, not districts
schools <- schools[schools$TYPE_CODE=='02',]
table(schools$SCHOOL_YEAR)

#don't care about the 2009-10 school year
table(schools$YEAR)
schools <- schools[schools$YEAR > 2010,]
table(schools$YEAR)

#don't care about expiration date and effective date
schools <- schools[,!(names(schools) %in% c("EXPIRATION_DATE", "EFFECTIVE_DATE", "TYPE_CODE"))]

schools <- rbind(schools, 
                 list(DISTRICT_ID=NA, DISTRICT_NAME=NA, SCHOOL_ID=state.school.id, NAME="State of Wyoming", SCHOOL_YEAR='2010-11', SHORT_NAME='State of Wyoming', CATEGORY='K-12 School', LOW_GRADE='KG', HIGH_GRADE='12', GRADES_SERVED='K-12', WAEA_SCHOOL_TYPE=4, PAIRED_SCHOOL_ID=NA, PAIRED_SCHOOL_NAME=NA, YEAR=2011),
                 list(DISTRICT_ID=NA, DISTRICT_NAME=NA, SCHOOL_ID=state.school.id, NAME="State of Wyoming", SCHOOL_YEAR='2011-12', SHORT_NAME='State of Wyoming', CATEGORY='K-12 School', LOW_GRADE='KG', HIGH_GRADE='12', GRADES_SERVED='K-12', WAEA_SCHOOL_TYPE=4, PAIRED_SCHOOL_ID=NA, PAIRED_SCHOOL_NAME=NA, YEAR=2012),
                 list(DISTRICT_ID=NA, DISTRICT_NAME=NA, SCHOOL_ID=state.school.id, NAME="State of Wyoming", SCHOOL_YEAR='2012-13', SHORT_NAME='State of Wyoming', CATEGORY='K-12 School', LOW_GRADE='KG', HIGH_GRADE='12', GRADES_SERVED='K-12', WAEA_SCHOOL_TYPE=4, PAIRED_SCHOOL_ID=NA, PAIRED_SCHOOL_NAME=NA, YEAR=2013))

#save(schools_other_attributes, file="data/schools-other-attributes.Rdata")
load(file="data/schools-other-attributes.Rdata")
schools_other_attributes <- schools_other_attributes[,!(names(schools_other_attributes) %in% c("TYPE_CODE"))]
names(schools_other_attributes)
#merge in other attributes for schools like Title 1 status and alternative school status
nrow(schools)
schools <- merge(schools, schools_other_attributes, all.x=TRUE)
nrow(schools)
nrow(schools[!is.na(schools$TITLE_1_SCHOOL),])

#save(school_enrollment, file="data/school-enrollment.Rdata")
load(file="data/school-enrollment.Rdata")
schools <- merge(schools, school_enrollment, all.x=TRUE)
schools$ENROLLMENT <- as.numeric(schools$ENROLLMENT)
nrow(schools[is.na(schools$ENROLLMENT),])