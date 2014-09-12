#names(schools)[grep("TARGET_LEVEL", names(schools))]
#names(schools)[grep("_N$", names(schools))]

g38.target.level.labels <- c(achievement="G38_ACHIEVEMENT_ALL_TARGET_LEVEL",
                             growth="G38_GROWTH_TARGET_LEVEL",
                             equity="G38_EQUITY_TARGET_LEVEL")

g38.participation.labels <- c(achievement="G38_ACHIEVEMENT_ALL_PARTICIPATION_RATE",
                             equity="G38_EQUITY_PARTICIPATION_RATE")

schools$G38_INDICATORS_N <- apply(schools[,c(g38.target.level.labels["achievement"], g38.target.level.labels["growth"], g38.target.level.labels["equity"])],
                              c(1),                                                                                      
                              FUN=function (indicators) {                                
                                sum(!is.na(indicators))
                              })


#look at the distribution of indicator combinations for 3-8
matrix.slots <- cast(schools[,c("SCHOOL_YEAR", g38.target.level.labels["achievement"], g38.target.level.labels["growth"], g38.target.level.labels["equity"],"SCHOOL_ID")],
                     as.formula(paste(paste("SCHOOL_YEAR", g38.target.level.labels["achievement"], g38.target.level.labels["growth"], g38.target.level.labels["equity"], sep='+'),
                                '.', 
                                sep='~')),
                     function (x) sum(!is.na(x)))
names(matrix.slots)[length(matrix.slots)] <- "N"
##schools with all three indciators defined in 2012-13
matrix.slots[matrix.slots$SCHOOL_YEAR==current.school.year & !is.na(matrix.slots[[g38.target.level.labels["equity"]]]),]
with(matrix.slots, matrix.slots[SCHOOL_YEAR==current.school.year,])


calc.SPL.nonHS <- function (school) {
  
  if (!(school[["WAEA_SCHOOL_TYPE"]] %in% nonHS.types))
    NA
  else {
    n.indicators <- as.character(school[["G38_INDICATORS_N"]])
    
    lookup <- SPL.lookup[["nonHS"]][[n.indicators]]
    
    #scores <- as.numeric(school[3:5])
    
    if (is.null(lookup))
      return(NA)
    
    if (n.indicators == 2) {      
      lookup[school[g38.target.level.labels["achievement"]], 
             school[g38.target.level.labels["growth"]]]  
      
    } else  {
      if (n.indicators == 3)
        lookup[school[g38.target.level.labels["achievement"]], 
               school[g38.target.level.labels["growth"]],
               school[g38.target.level.labels["equity"]]]  
      else
        NA 
    }    
    
  }
  
}

schools$G38_SPL <- apply(schools[,c("WAEA_SCHOOL_TYPE", "G38_INDICATORS_N", g38.target.level.labels)],
                                 c(1),
                                 FUN=calc.SPL.nonHS )

schools$G38_SPL_ACCOUNTABILITY <- apply(schools[,c("G38_SPL", g38.participation.labels)],
                         c(1),
                         FUN=calc.SPL.accountability, "G38_SPL",  g38.participation.labels)

schools[!is.na(schools$G38_SPL) & schools$G38_SPL !=  schools$G38_SPL_ACCOUNTABILITY,]

table(schools[c("SCHOOL_YEAR","G38_SPL")])
table(schools[c("SCHOOL_YEAR","G38_SPL_ACCOUNTABILITY")])
#propagate results to paired schools

g38.accountability.labels <- names(schools)[grep("^G38_", names(schools))]
schools <- propagate.results.to.paired.schools(schools, g38.accountability.labels)

#check two of the paired schools
schools[schools$SCHOOL_ID %in% c('0501002','0501010'),]
with(schools, head(schools[SCHOOL_YEAR==current.school.year,]))
