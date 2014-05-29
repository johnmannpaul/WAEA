source("constants.R")
source('db-functions.R')
source('const/private/db.R')




load(file="data/schools.orig.Rdata")
load(file="data/schools-other-attributes.Rdata")
load(file="data/school-enrollment.Rdata")
 

make.query <- function (table.name, cols, year) {
  
  paste("SELECT", 
        do.call(paste, c(cols, list(sep=","))), 
        "FROM", 
        table.name, 
        "WHERE TYPE_CODE = '02' AND SCHOOL_YEAR =", paste("'",year,"'", sep=""), sep=" ")
}

as.is.vector <- function (cols) {
  
  result <- rep(TRUE, length(cols))
  names(result) <- cols
  result
}

conn <- odbcConnect(dsn=db.dsn, uid=rdbms.name, pwd=rdbms.pwd)


current.schools <- sqlQuery(conn, query=make.query(object.name(school.schema,"SCHOOL_BY_YEAR_ALL"), 
                                                   names(schools),
                                                   year=current.school.year),
                            as.is=as.is.vector(names(schools)))


current.schools.other.atributes <- sqlQuery(conn, query=make.query(object.name(school.schema,"SCHOOL_BY_YEAR_ALL"), 
                                                                   names(schools_other_attributes),
                                                                   year=current.school.year),
                                            as.is=as.is.vector(names(schools_other_attributes)))


current.schools.enrollment <- sqlQuery(conn, query=make.query(object.name(model.schema,"SCHOOL_FALL_ENROLLMENT"), 
                                                                   names(school_enrollment),
                                                                   year=current.school.year),
                                            as.is=as.is.vector(names(school_enrollment)))

odbcClose(conn)
nrow(current.schools)
nrow(current.schools.other.atributes)
nrow(current.schools.enrollment)

table(schools$SCHOOL_YEAR)
schools <- rbind(current.schools, schools[schools$SCHOOL_YEAR != current.school.year,])
table(schools$SCHOOL_YEAR)

table(schools_other_attributes$SCHOOL_YEAR)
schools_other_attributes <- rbind(current.schools.other.atributes, schools_other_attributes [schools_other_attributes $SCHOOL_YEAR != current.school.year,])
table(schools_other_attributes $SCHOOL_YEAR)

table(school_enrollment$SCHOOL_YEAR)
school_enrollment <- rbind(current.schools.enrollment, school_enrollment[school_enrollment$SCHOOL_YEAR != current.school.year,])
table(school_enrollment$SCHOOL_YEAR)


save(schools, file="data/schools.orig.Rdata")
save(schools_other_attributes, file="data/schools-other-attributes.Rdata")
save(school_enrollment, file="data/school-enrollment.Rdata")