library("reshape")

school.year.to.year <- function (y) {
    as.numeric(strsplit(y,'-')[[1]][1]) + 1
}

school.years.to.years <- function (school.years) {
  sapply(school.years, school.year.to.year)
}

increment.school.year <- function (school.year, increment=1) {
  
  do.call(paste, as.list(c(as.numeric(strsplit(school.year,'-')[[1]]) + increment, sep='-')))
}

compute.N.years <- function (school, min) { 
  school.years <- school[2:length(school)]
  tested <- rev(as.numeric(school.years))
  result <- sapply(1:length(tested),
                   function (i) {
                     
                     meets.min.N <- which(sapply(i:length(tested),
                                                 function (j) {
                                                   N <-sum(tested[i:j])
                                                   ifelse(N < min, FALSE, TRUE)                    
                                                 }))
                     if (length(meets.min.N) == 0)
                       return (Inf)  #no matter how far you go back, you won't meet min N
                     else
                       return(meets.min.N[1] - 1) #you have to go back this many years to meet min N
                   }) 
  names(result) <- rev(names(school.years))
  result
}



bind.indicator <- function (schools, 
                            indicator.df,
                            indicator.labels.min.N = c(N= "N", score="SCORE", part.rate = "PARTICIPATION_RATE"),
                            min.N) {
  
  indicator.labels <- names(indicator.df)[!(names(indicator.df) %in% c("SCHOOL_YEAR", "SCHOOL_ID"))]
  
  schools <- schools[, !(names(schools) %in% indicator.labels)]
  
  process.school <- function (school) {
    indicator.row <- with(indicator.df, indicator.df[SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                       SCHOOL_YEAR == school[["SCHOOL_YEAR"]],indicator.labels])
    
    if (nrow(indicator.row) == 0)
      result <- rep(NA, length(indicator.labels))
    else {
      result <- as.vector(as.matrix(indicator.row))
      
      N <- indicator.row[[indicator.labels.min.N["N"]]]                                        
      if (N < min.N) {
        result[which(indicator.labels %in% indicator.labels.min.N[which(names(indicator.labels.min.N) != 'N')])] <- NA  #no score if N is too small
      }
      
    }
    result <- as.numeric(result) #make numeric
    names(result) <-indicator.labels
    result
  }
  
  cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                               "SCHOOL_YEAR",                                                     
                                               "WAEA_SCHOOL_TYPE")], c(1),                                                                                      
                                    FUN=process.school
                                    
  ))))
  
}




#could optimize this
propagate.results.to.paired.schools <- function (schools, labels) {
  
  #just propagating the nonHS columns. Don't think any of the schools are paired with high schools or K12s.
  lapply(c(labels),
         function (column) {
           
           schools[[column]] <<- apply(schools[,c("SCHOOL_YEAR", "SCHOOL_ID")], 
                                                  c(1), 
                                                  function (school) {
                                                    school.year <- school[["SCHOOL_YEAR"]]
                                                    school.id <- school[["SCHOOL_ID"]]
                                                    pairing <- school.pairing.lookup[[school.year]]
                                                    paired.school <- pairing[[school.id]]
                                                    
                                                    ifelse(is.null(paired.school), 
                                                           schools[schools$SCHOOL_YEAR==school.year &
                                                                     schools$SCHOOL_ID==school.id, column], 
                                                           schools[schools$SCHOOL_YEAR == school.year & 
                                                                     schools$SCHOOL_ID == paired.school, column])
                                                    
                                                  })
           
           
         })
  schools
}




#When calling aggregate with vector valued aggregator the result
#is a data.frame  with single matrix valued column of aggregated values at the end.
#This function articulates matrix valued columns and returns the resulting data.frame.
unmatrixfy.df <- function (df, sep="_", prepend=TRUE) {
  
  do.call(cbind, lapply(names(df),
                        function (n) {
                          
                          obj <- df[[n]]
                          
                          if (class(obj) != "matrix")
                            df[n]
                          else{
                            col.names <- dimnames(obj)[[2]]
                            result <- do.call(data.frame, 
                                    sapply(col.names,
                                           function (n) obj[,n],
                                           simplify=FALSE))
                            names(result) <- sapply(col.names, function (x) {
                              if(prepend)
                                paste(n, x, sep=sep)
                              else
                                x})
                            
                            result
                          }
                          
                          
                        }))
  
}


#replace NA rows with zeros
zero.na.rows <- function (df, col.labels, rate.labels=NULL) {
  
  labels <- c(col.labels, rate.labels)
  
  if (length(labels) > 1)
    t(apply(df[,labels],
            c(1),
            function (row) {
              if (sum(is.na(row)) == length(row))
                c(rep(0, length(col.labels)),
                  rep(100, length(rate.labels)))
              else
                row                       
            }))
  else
    sapply(df[,labels],
          function (val) {
            if (is.na(val))
              0
            else
              val                       
          })
}



compute.grad.rate.cat <- function (school,cuts, precision, labels) {
  extended <- school[[labels["extended"]]]
  current.year.4yr.N <- school[[labels["4yr.N"]]]
  prior.year.4yr.N <- school[[labels["4yr.N.prior"]]]
  sufficient.Ns <- !is.na(current.year.4yr.N) & !is.na(prior.year.4yr.N) & current.year.4yr.N >= min.N.grad & prior.year.4yr.N >= min.N.grad
  if (extended == 3 | !sufficient.Ns)
    return(c(1, NA, NA, NA, extended))
  else {
    current.year.4yr <- school[[labels["4yr"]]]
    prior.year.4yr <- school[[labels["4yr.prior"]]]
    if (extended == 1) {
      improvement.target <-round((((cuts[1] - prior.year.4yr)/3) +  #for meets
                                    prior.year.4yr), precision)
      return(c(ifelse(improvement.target <= current.year.4yr, 2, 1),
               improvement.target, 
               NA,
               improvement.target,
               ifelse(improvement.target <= current.year.4yr, 2, 1)))
      
    } else { #extended==2
      
      improvement.target <-round((((cuts[2] - prior.year.4yr)/3)+  #for exceeds
                                    prior.year.4yr))
      return(c(ifelse(improvement.target <= current.year.4yr, 2, 1),
               NA, 
               improvement.target,
               improvement.target,
               ifelse(improvement.target <= current.year.4yr, 3, 2)))                                                
      
    }                                                                                                                                          
  }
  
}



calc.SPL.accountability <- function (school, SPL.label, participation.labels) {
  
  SPL <- school[[SPL.label]]
  
  if (is.na(SPL))
    NA
  else {
    
    part.rates <- school[participation.labels]
    lowest.part.rate <- part.rates[order(part.rates)][1]
    
    if (lowest.part.rate < 90)
      1
    else {
      
      if (lowest.part.rate < 95)
        ifelse(SPL == 1, 1, SPL-1)
      else
        SPL                                                                
    }
    
    
  }
}


get.filename <- function (file.prefix, directory) {
  results.files <- dir(directory)[grep(paste(file.prefix,"-[0-9]+\\.csv", sep=""), dir(directory))]
  results.files <- results.files[order(results.files, decreasing=TRUE)]
  result.file <- if (length(results.files) == 0) {
    paste(file.prefix,"-001.csv", sep="")      
  }else {
    
    last.file <- results.files[1]
    prefix <- strsplit(strsplit(last.file, ".", fixed=TRUE)[[1]][1], "-", fixed=TRUE)
    raw.index <- paste("00", as.numeric(prefix[[1]][length(prefix[[1]])]) + 1, sep="")
    index <- substr(raw.index, nchar(raw.index) - 2, nchar(raw.index))
    
    paste(do.call(paste, as.list(c(prefix[[1]][1:(length(prefix[[1]])-1)], index, sep="-"))), "csv", sep=".")
  }
  paste(directory, result.file, sep="/")
}