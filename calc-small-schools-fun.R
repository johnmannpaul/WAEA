source("constants.R")
source("function-defs.R")

calc.small.schools <- function (df, 
                                schools.df, 
                                waea.school.types,
                                id.column = "WISER_ID",
                                excluded.school.ids=c('7700000'), 
                                min.N=10,
                                attribute="ACHIEVEMENT") {
  
  #determine "small" schools in df 
  schools.N <- with(df, cast(merge(data.frame(SCHOOL_ID=unique(schools.df[schools.df$WAEA_SCHOOL_TYPE %in% waea.school.types,"SCHOOL_ID"])),
                                   df[, c("SCHOOL_YEAR","SCHOOL_ID",id.column)]), SCHOOL_YEAR+SCHOOL_ID~., 
                             function (x) length(unique(x))))
  
  
  N.label <-  paste("N", attribute, sep="_")
  names(schools.N)[length(schools.N)] <- N.label
  
  
  
  #merge schools with no scores
  schools.N.0 <- merge(schools.df[schools.df$WAEA_SCHOOL_TYPE %in% waea.school.types,c("SCHOOL_ID", "SCHOOL_YEAR")],
                       df[, c("SCHOOL_YEAR","SCHOOL_ID",id.column)],
                       all.x=TRUE)
  
  schools.N.0 <- schools.N.0[is.na(schools.N.0[id.column]) & !(schools.N.0$SCHOOL_ID %in% excluded.school.ids),]
  
  
  schools.N <- if (nrow(schools.N.0) > 0)
    as.data.frame(rbind(schools.N, data.frame(t(apply(schools.N.0,
                                                                   c(1),
                                                                   function (school){
                                                                     
                                                                     result <- c(SCHOOL_YEAR = school[["SCHOOL_YEAR"]], SCHOOL_ID=school[["SCHOOL_ID"]], 0)
                                                                     names(result)[length(result)] <- N.label
                                                                     result
                                                                   })))))
  else
    as.data.frame(schools.N)
  
  
    
  ##cast as numeric
  schools.N[N.label] <- as.numeric(schools.N[[N.label]])
  
  
  small.schools <- merge(schools.N[eval(bquote(schools.N[.(N.label)] < min.N)),],
                         schools.df[,c("SCHOOL_ID", "SCHOOL_YEAR")])
  
  
  #now we need to determine how far back to go 
  #small.achievement <- cast(small.schools.achievement[,c("SCHOOL_YEAR","SCHOOL_ID","N_ACHIEVEMENT")], SCHOOL_ID~SCHOOL_YEAR, fill=0)
  small <- merge(cast(schools.N[,c("SCHOOL_YEAR","SCHOOL_ID",N.label)], SCHOOL_ID~SCHOOL_YEAR, fill=0),
                             data.frame(SCHOOL_ID = unique(small.schools[,c("SCHOOL_ID")])))
  
  
  #small.growth <- cast(small.schools[,c("SCHOOL_YEAR","SCHOOL_ID","N_GROWTH")], SCHOOL_ID~SCHOOL_YEAR, fill=0)
  
  
  
  #this is a dataframe
  go.back <- if (ncol(small) > 2) { #at least one year to look back to
    cbind(SCHOOL_ID = small[,"SCHOOL_ID"], as.data.frame(t(apply(small, c(1),
                                                                 FUN=function (school) {
                                                                   compute.N.years(school, min.N)          
                                                                 }
    ))))
  }
  else { #no years to go back
    result <- cbind(small["SCHOOL_ID"], Inf)
    names(result)[2] <- names(small[2])
    result
  }
  
  compute.years.back <- function (school) {
    
    id <- school[["SCHOOL_ID"]]
    year <- school[["SCHOOL_YEAR"]]
    
    years.back <- go.back[go.back$SCHOOL_ID==id,][[year]] 
    
    years.back
    
  }

  years.back.label <- paste(attribute, "YEARS_BACK", sep="_")
  small.schools[years.back.label] <- apply(small.schools, c(1),
                                                FUN=compute.years.back)
  
  #look at the current year's batch
  small.schools[small.schools$SCHOOL_YEAR==current.school.year,]
  
  #this is how many we can rate
  table(small.schools[years.back.label])
  
  ##now assign back to schools dataframe
  
  
  small.school.label <- paste( attribute, "SMALL_SCHOOL", sep="_")
  small.school.labels <- c(small.school.label, years.back.label)
  result.schools <- cbind(schools.df[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID")], data.frame(t(apply(schools.df[,c("SCHOOL_ID",                                                     
                                          "SCHOOL_YEAR",                                                     
                                          "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                               FUN=function (school) {
                                 small.school <- with(small.schools, small.schools[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                                             SCHOOL_YEAR == school[["SCHOOL_YEAR"]],])
                                 if (!(school[["WAEA_SCHOOL_TYPE"]] %in% waea.school.types))
                                   result <- c(NA, NA)
                                 else {
                                   if (nrow(small.school) == 0)
                                     result <- c('F', NA)  
                                   else {
                                     #result <- c('T', small.school[,years.back.label])  
                                     if (small.school[,years.back.label] < Inf)
                                       result <- c('F', small.school[,years.back.label])
                                     else
                                       result <- c('T', small.school[,years.back.label])  
                                   }
                                 }
                                 names(result) <- small.school.labels
                                 result
                               }))))
  
  result.schools[years.back.label] <- as.numeric(result.schools[[years.back.label]])
  
  #here are your small schools again    
  result.schools[result.schools[[small.school.label]] %in% 'T',]
  
  #these are the ones we can actually do something about
#   small.schools.fix <- result.schools[eval(bquote(result.schools[.(small.school.label)] == 'T' &
#                                                     result.schools[.(years.back.label)] < Inf)),]


  small.schools.fix <- result.schools[!is.na(result.schools[[years.back.label]]) &
                                                  result.schools[[years.back.label]] < Inf,]

  
  get.students.years.back <- function (school, df, id.column, years.back.label) {
    
    #when 'apply' this function on an empty data.frame
    #if (is.logical(school) & !all(school))
    if (length(names(school)) == 0)
      return(list(NULL))
    
    school.year <- school[["SCHOOL_YEAR"]]
    id <- school[["SCHOOL_ID"]]
    year <- as.numeric(school[["YEAR"]])
    
    years.back <- as.numeric(school[[years.back.label]])
    
    scores <- lapply((year-years.back):(year-1),
                     
                     function (y) {
                       df.year <- with(df, df[SCHOOL_ID==id & school.years.to.years(SCHOOL_YEAR) == y,])
                       #save the original school year for each student in SCHOOL_YEAR_ORIGINAL 
                       df.year$SCHOOL_YEAR_ORIGINAL <- df.year$SCHOOL_YEAR
                       df.year$SCHOOL_YEAR <- rep(school.year,nrow(df.year))
                       df.year[id.column] <- sapply(df.year[[id.column]],
                                                  function (id) {
                                                    paste(id,y,sep='.')                                                   
                                                  })
                       df.year
                       
                     })
    do.call(rbind, scores)
    
  }
  
  #takes a long time for big data frames
  small.schools.additional.student.records <- do.call(rbind, apply(small.schools.fix[,c("SCHOOL_YEAR", "YEAR", "SCHOOL_ID", years.back.label)], c(1),
                                                                       FUN=function (school) get.students.years.back(school,
                                                                                                                     merge(df, 
                                                                                                                           data.frame(SCHOOL_ID = unique(small.schools.fix$SCHOOL_ID))), #only care about the student records for the small schools 
                                                                                                                     id.column,
                                                                                                                     years.back.label)))
  
  list(result.schools=result.schools, result.students=small.schools.additional.student.records)
  ##end small schools
}





