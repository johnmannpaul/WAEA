calc.small.schools <- function (df, schools.df, waea.school.types, excluded.school.ids=c('7700000'), attribute="ACHIEVEMENT") {
  
  #determine "small" schools in df 
  schools.N <- with(df, cast(merge(schools.df[schools.df$WAEA_SCHOOL_TYPE %in% waea.school.types,
                                              c("SCHOOL_YEAR", "SCHOOL_ID")],
                                   df[, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")]), SCHOOL_YEAR+SCHOOL_ID~., 
                             function (x) length(unique(x))))
  
  
  N.label <-  paste("N", attribute, sep="_")
  names(schools.N)[length(schools.N)] <- N.label
  
  
  
  #merge schools with no scores
  schools.N.0 <- merge(schools.df[schools.df$WAEA_SCHOOL_TYPE %in% waea.school.types,c("SCHOOL_YEAR", "SCHOOL_ID")],
                       df[, c("SCHOOL_YEAR","SCHOOL_ID","WISER_ID")],
                       all.x=TRUE)
  
  schools.N.0 <- schools.N.0[is.na(schools.N.0$WISER_ID) & !(schools.N.0$SCHOOL_ID %in% excluded.school.ids),]
  
  schools.N <- as.data.frame(rbind(schools.N, data.frame(t(apply(schools.N.0,
                                                                 c(1),
                                                                 function (school){
                                                                   
                                                                   c(SCHOOL_YEAR = school[["SCHOOL_YEAR"]], SCHOOL_ID=school[["SCHOOL_ID"]], N_ACHIEVEMENT=0)
                                                                 })))))
  
  
    
  ##cast as numeric
  schools.N$N.label <- as.numeric(schools.N$N.label)
  
  small.schools.achievement <- with(schools.N, schools.N[N_ACHIEVEMENT < min.N.achievement,])
  
  
  
  
  
  #now we need to determine how far back to go 
  #small.achievement <- cast(small.schools.achievement[,c("SCHOOL_YEAR","SCHOOL_ID","N_ACHIEVEMENT")], SCHOOL_ID~SCHOOL_YEAR, fill=0)
  small.achievement <- merge(cast(schools.N[,c("SCHOOL_YEAR","SCHOOL_ID","N_ACHIEVEMENT")], SCHOOL_ID~SCHOOL_YEAR, fill=0),
                             data.frame(SCHOOL_ID = unique(small.schools.achievement[,c("SCHOOL_ID")])))
  
  
  #small.growth <- cast(small.schools[,c("SCHOOL_YEAR","SCHOOL_ID","N_GROWTH")], SCHOOL_ID~SCHOOL_YEAR, fill=0)
  
  
  
  #this is a dataframe
  go.back.achievement <- cbind(SCHOOL_ID = small.achievement[,"SCHOOL_ID"], as.data.frame(t(apply(small.achievement, c(1),
                                                                                                  FUN=function (school) {
                                                                                                    compute.N.years(school, min.N.achievement.multiyear)          
                                                                                                  }
  ))))
  
  
  compute.years.back.achievement <- function (school) {
    
    id <- school[["SCHOOL_ID"]]
    year <- school[["SCHOOL_YEAR"]]
    
    years.back.achievement <- go.back.achievement[go.back.achievement$SCHOOL_ID==id,][[year]] 
    
    years.back.achievement
    
  }
  
  small.schools.achievement$YEARS_BACK <- apply(small.schools.achievement, c(1),
                                                FUN=compute.years.back.achievement)
  
  #look at the current year's batch
  small.schools.achievement[small.schools.achievement$SCHOOL_YEAR==current.school.year,]
  
  #this is how many we can rate
  table(small.schools.achievement$YEARS_BACK)
  
  ##now assign back to schools dataframe
  schools <- schools[, !(names(schools) %in% small.school.labels.achievement)]
  
  schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                          "SCHOOL_YEAR",                                                     
                                                          "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                               FUN=function (school) {
                                                 small.school <- with(small.schools.achievement, small.schools.achievement[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                                                             SCHOOL_YEAR == school[["SCHOOL_YEAR"]],])
                                                 if (nrow(small.school) == 0)
                                                   result <- c('F', NA)
                                                 else 
                                                   result <- c('T', small.school[,"YEARS_BACK"])
                                                 names(result) <- small.school.labels.achievement
                                                 result
                                               }))))
  
  schools$YEARS_BACK_ACHIEVEMENT <- as.numeric(schools$YEARS_BACK_ACHIEVEMENT)
  
  #here are your small schools again
  schools[schools$SMALL_SCHOOL_ACHIEVEMENT=='T',]
  
  #these are the ones we can actually do something about
  small.schools.achievement.fix <- schools[schools$SMALL_SCHOOL_ACHIEVEMENT=='T' & schools$YEARS_BACK_ACHIEVEMENT < Inf,]
  
  
  
  small.schools.achievement.additional.paws.df <- do.call(rbind, apply(small.schools.achievement.fix[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK_ACHIEVEMENT")], c(1),
                                                                       FUN=function (school) get.paws.years.back(school, paws.df, "YEARS_BACK_ACHIEVEMENT")))
  
  #SCHOOL_YEAR_ORIGINAL is just SCHOOL_YEAR for these rows
  paws.df$SCHOOL_YEAR_ORIGINAL <- paws.df$SCHOOL_YEAR
  paws.df <- rbind(paws.df, small.schools.achievement.additional.paws.df)
  
  #for calculating participation rate we also have to add the students from the previous years (including those with testing status 'N' from the previous years) to the paws data frame 
  small.schools.achievement.additional.paws <- do.call(rbind, apply(small.schools.achievement.fix[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", "YEARS_BACK_ACHIEVEMENT")], c(1),
                                                                    FUN=function (school) get.paws.years.back(school, paws, "YEARS_BACK_ACHIEVEMENT")))
  paws$SCHOOL_YEAR_ORIGINAL <- paws$SCHOOL_YEAR
  paws <- rbind(paws, small.schools.achievement.additional.paws)
  
  ##end small schools
}