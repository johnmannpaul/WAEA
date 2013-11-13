#rollback any prior year records that were added for multiyear achievement
paws.df <- with(paws.df, paws.df[SCHOOL_YEAR_ORIGINAL == SCHOOL_YEAR,])

#determine "small" schools


schools.growth.N <- with(paws.df, cast(paws.df[!is.na(SGP), c("SCHOOL_YEAR", 
                                                              "SCHOOL_ID",   
                                                              "WISER_ID")], SCHOOL_YEAR+SCHOOL_ID~., 
                                       function (x) length(unique(x))))

names(schools.growth.N)[length(schools.growth.N)] <- "N_GROWTH"


#merge schools with no scores
schools.N.0 <- merge(schools[schools$WAEA_SCHOOL_TYPE %in% nonHS.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                     paws.df[, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                     all.x=TRUE)

schools.N.0 <- schools.N.0[is.na(schools.N.0$WISER_ID) & schools.N.0$SCHOOL_ID != state.school.id,]

schools.N <- as.data.frame(rbind(schools.growth.N, data.frame(t(apply(schools.N.0,
                                                 c(1),
                                                 function (school){
                                                   
                                                   c(SCHOOL_YEAR = school[["SCHOOL_YEAR"]], SCHOOL_ID=school[["SCHOOL_ID"]], N_GROWTH=0)
                                                 })))))


#assign 0 to replace NA growth
schools.N$N_GROWTH <- ifelse(is.na(schools.N$N_GROWTH), 0, schools.N$N_GROWTH)

##a small school is one where either N_ACHIEVEMENT < min.N.achievement OR N_GROWTH < min.N.growth
schools.N$N_GROWTH <- as.numeric(schools.N$N_GROWTH)
small.schools.growth <- with(schools.N, schools.N[N_GROWTH < min.N.growth,])





#small.growth <- cast(small.schools.growth[,c("SCHOOL_YEAR","SCHOOL_ID","N_GROWTH")], SCHOOL_ID~SCHOOL_YEAR, fill=0)
small.growth <- merge(cast(schools.N[,c("SCHOOL_YEAR","SCHOOL_ID","N_GROWTH")], SCHOOL_ID~SCHOOL_YEAR, fill=0),
                      data.frame(SCHOOL_ID = unique(small.schools.growth[,c("SCHOOL_ID")])))




#this is a dataframe
go.back.growth <- cbind(SCHOOL_ID = small.growth[,"SCHOOL_ID"], as.data.frame(t(apply(small.growth, c(1),
                                                                                      FUN=function (school) {
                                                                                        compute.N.years(school, min.N.growth.multiyear)          
                                                                                      }
))))

compute.years.back.growth <- function (school) {
  
  id <- school[["SCHOOL_ID"]]
  year <- school[["SCHOOL_YEAR"]]
  
  years.back.growth <- go.back.growth[go.back.growth$SCHOOL_ID==id,][[year]] 
  
  years.back.growth
  
}


small.schools.growth$YEARS_BACK <- apply(small.schools.growth, c(1),
                                  FUN=compute.years.back.growth)

#look at the current year's batch
small.schools.growth[small.schools.growth$SCHOOL_YEAR==current.school.year,]

#this is how many we can rate
table(small.schools.growth$YEARS_BACK)

##now assign back to schools dataframe
schools <- schools[, !(names(schools) %in% small.school.labels.growth)]

schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                        "SCHOOL_YEAR",                                                     
                                                        "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                             FUN=function (school) {
                                               small.school <- with(small.schools.growth, small.schools.growth[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                                   SCHOOL_YEAR == school[["SCHOOL_YEAR"]],])
                                               if (nrow(small.school) == 0)
                                                 result <- c('F', NA)
                                               else 
                                                 result <- c('T', small.school[,"YEARS_BACK"])
                                               names(result) <- small.school.labels.growth
                                               result
                                             }))))

schools$YEARS_BACK_GROWTH <- as.numeric(schools$YEARS_BACK_GROWTH)

#here are your small schools again
schools[schools$SMALL_SCHOOL=='T',]

#these are the ones we can actually do something about
small.schools.growth.fix <- schools[schools$SMALL_SCHOOL_GROWTH=='T' & schools$YEARS_BACK_GROWTH < Inf,]



small.schools.growth.additional.paws.df <- do.call(rbind, apply(small.schools.growth.fix[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK_GROWTH")], c(1),
                                                         FUN=function (school) get.paws.years.back(school, paws.df, "YEARS_BACK_GROWTH")))

#SCHOOL_YEAR_ORIGINAL is just SCHOOL_YEAR for these rows
paws.df$SCHOOL_YEAR_ORIGINAL <- paws.df$SCHOOL_YEAR
paws.df <- rbind(paws.df, small.schools.growth.additional.paws.df)


##end small schools
