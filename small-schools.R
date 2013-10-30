#determine "small" schools
schools.achievement.N <- with(paws.df, cast(merge(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                                                  paws.df[, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")]), SCHOOL_YEAR+SCHOOL_ID~., 
                                            function (x) length(unique(x))))

names(schools.achievement.N)[length(schools.achievement.N)] <- "N_ACHIEVEMENT"




schools.growth.N <- with(paws.df, cast(paws.df[!is.na(SGP), c("SCHOOL_YEAR", 
                                                              "SCHOOL_ID",   
                                                              "WISER_ID")], SCHOOL_YEAR+SCHOOL_ID~., 
                                       function (x) length(unique(x))))
names(schools.growth.N)[length(schools.growth.N)] <- "N_GROWTH"

schools.N <- merge(schools.achievement.N,
                   schools.growth.N, all.x=TRUE)

#merge schools with no scores
schools.N.0 <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                     paws.df[, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                     all.x=TRUE)

schools.N.0 <- schools.N.0[is.na(schools.N.0$WISER_ID) & schools.N.0$SCHOOL_ID != state.school.id,]

schools.N <- rbind(schools.N, data.frame(t(apply(schools.N.0,
                                                                         c(1),
                                                                         function (school){
                                                                           
                                                                           c(SCHOOL_YEAR = school[["SCHOOL_YEAR"]], SCHOOL_ID=school[["SCHOOL_ID"]], N_ACHIEVEMENT=0 ,N_GROWTH=0)
                                                                         }))))


#assign 0 to replace NA growth
schools.N$N_GROWTH <- ifelse(is.na(schools.N$N_GROWTH), 0, schools.N$N_GROWTH)

##a small school is one where either N_ACHIEVEMENT < min.N.achievement OR N_GROWTH < min.N.growth
schools.N$N_ACHIEVEMENT <- as.numeric(schools.N$N_ACHIEVEMENT)
schools.N$N_GROWTH <- as.numeric(schools.N$N_GROWTH)
small.schools <- with(schools.N, schools.N[N_ACHIEVEMENT < min.N.achievement | N_GROWTH < min.N.growth,])





#now we need to determine how far back to go 
#small.achievement <- cast(small.schools[,c("SCHOOL_YEAR","SCHOOL_ID","N_ACHIEVEMENT")], SCHOOL_ID~SCHOOL_YEAR, fill=0)
small.achievement <- merge(cast(schools.N[,c("SCHOOL_YEAR","SCHOOL_ID","N_ACHIEVEMENT")], SCHOOL_ID~SCHOOL_YEAR, fill=0),
                            data.frame(SCHOOL_ID = unique(small.schools[,c("SCHOOL_ID")])))


#small.growth <- cast(small.schools[,c("SCHOOL_YEAR","SCHOOL_ID","N_GROWTH")], SCHOOL_ID~SCHOOL_YEAR, fill=0)
 small.growth <- merge(cast(schools.N[,c("SCHOOL_YEAR","SCHOOL_ID","N_GROWTH")], SCHOOL_ID~SCHOOL_YEAR, fill=0),
                       data.frame(SCHOOL_ID = unique(small.schools[,c("SCHOOL_ID")])))






#this is a dataframe
go.back.achievement <- cbind(SCHOOL_ID = small.achievement[,"SCHOOL_ID"], as.data.frame(t(apply(small.achievement, c(1),
                               FUN=function (school) {
                                 compute.N.years(school, min.N.achievement.multiyear)          
                               }
))))

#this is a dataframe
go.back.growth <- cbind(SCHOOL_ID = small.growth[,"SCHOOL_ID"], as.data.frame(t(apply(small.growth, c(1),
                                                                                  FUN=function (school) {
                                                                                    compute.N.years(school, min.N.growth.multiyear)          
                                                                                  }
))))


small.schools$YEARS_BACK <- apply(small.schools, c(1),
                                  FUN=compute.years.back)

#look at the current year's batch
small.schools[small.schools$SCHOOL_YEAR==current.school.year,]

#this is how many we can rate
table(small.schools$YEARS_BACK)

##now assign back to schools dataframe
schools <- schools[, !(names(schools) %in% small.school.labels)]

schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                        "SCHOOL_YEAR",                                                     
                                                        "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                             FUN=function (school) {
                                               small.school <- with(small.schools, small.schools[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                                   SCHOOL_YEAR == school[["SCHOOL_YEAR"]],])
                                               if (nrow(small.school) == 0)
                                                 result <- c('F', NA)
                                               else 
                                                 result <- c('T', small.school[,"YEARS_BACK"])
                                               names(result) <- small.school.labels
                                               result
                                             }))))

schools$YEARS_BACK <- as.numeric(schools$YEARS_BACK)

#here are your small schools again
schools[schools$SMALL_SCHOOL=='T',]

#these are the ones we can actually do something about
small.schools.fix <- schools[schools$SMALL_SCHOOL=='T' & schools$YEARS_BACK < Inf,]



small.schools.additional.paws.df <- do.call(rbind, apply(small.schools.fix[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK")], c(1),
                                                         FUN=function (school) get.paws.years.back(school, paws.df)))

#SCHOOL_YEAR_ORIGINAL is just SCHOOL_YEAR for these rows
paws.df$SCHOOL_YEAR_ORIGINAL <- paws.df$SCHOOL_YEAR
paws.df <- rbind(paws.df, small.schools.additional.paws.df)

#for calculating participation rate we also have to add the students from the previous years (including those with testing status 'N' from the previous years) to the paws data frame 
small.schools.additional.paws <- do.call(rbind, apply(small.schools.fix[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK")], c(1),
                                                      FUN=function (school) get.paws.years.back(school, paws)))
paws$SCHOOL_YEAR_ORIGINAL <- paws$SCHOOL_YEAR
paws <- rbind(paws, small.schools.additional.paws)

##end small schools
