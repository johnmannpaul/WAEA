source("constants.R")
source('db-functions.R')
source('const/private/db.R')



#load old objects
load(file="data/schools.orig.Rdata")
load(file="data/schools-other-attributes.Rdata")
load(file="data/school-enrollment.Rdata")
load(file="data/ACT/act.suite.readiness.Rdata")
 

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


#query new records
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

current.act.suite.readiness <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][["act.suite.readiness"]], as.is=as.is.vector(names(act.suite.readiness)))


grade.nine.columns <- sqlColumns(conn, data.tables.lexicon[[current.school.year]][["grade.nine.credits"]])
grade.nine.credits <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][["grade.nine.credits"]], as.is=as.is.vector(grade.nine.columns$COLUMN_NAME))

grade.nine.columns <- sqlColumns(conn, data.tables.lexicon[[current.school.year]][["grade.nine.credits"]])
grade.nine.credits <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][["grade.nine.credits"]], as.is=as.is.vector(grade.nine.columns$COLUMN_NAME))

hathaway.eligibility.columns <- sqlColumns(conn, data.tables.lexicon[[current.school.year]][["hathaway.eligibility"]]) 
hathaway.eligibility <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][["hathaway.eligibility"]], as.is=as.is.vector(hathaway.eligibility.columns$COLUMN_NAME))

act.achieve.2014.columns <- sqlColumns(conn, data.tables.lexicon[[current.school.year]][["act.achieve"]]) 
act.achieve.2014 <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][["act.achieve"]], as.is=as.is.vector(act.achieve.2014.columns$COLUMN_NAME))

g38.achieve.2014.columns <- sqlColumns(conn, data.tables.lexicon[[current.school.year]][["g38.achieve"]]) 
g38.achieve.2014 <- sqlFetch(conn, data.tables.lexicon[[current.school.year]][["g38.achieve"]], as.is=as.is.vector(g38.achieve.2014.columns$COLUMN_NAME))

# act.2014.columns <- sqlColumns(conn, "ACCOUNTABILITY.ACT_2014") 
# act.2014 <- sqlFetch(conn, "ACCOUNTABILITY.ACT_2014", as.is=as.is.vector(act.2014.columns$COLUMN_NAME))
# write.csv(act.2014, file="data/act-2014.csv", row.names=FALSE, na="")
# plan.2014.columns <- sqlColumns(conn, "ACCOUNTABILITY.PLAN_2014") 
# plan.2014 <- sqlFetch(conn, "ACCOUNTABILITY.PLAN_2014", as.is=as.is.vector(plan.2014.columns$COLUMN_NAME))
# write.csv(plan.2014, file="data/plan-2014.csv", row.names=FALSE, na="")
# explore.2014.columns <- sqlColumns(conn, "ACCOUNTABILITY.EXPLORE_2014") 
# explore.2014 <- sqlFetch(conn, "ACCOUNTABILITY.EXPLORE_2014", as.is=as.is.vector(explore.2014.columns$COLUMN_NAME))
# write.csv(explore.2014, file="data/explore-2014.csv", row.names=FALSE, na="")
# cor(as.numeric(act.2014$ACT_SCALE_SCORE_ENGLISH), as.numeric(act.2014$ACT_SCALE_SCORE_ENG_WRITING), use="complete.obs")
odbcClose(conn)
nrow(current.schools)
nrow(current.schools.other.atributes)
nrow(current.schools.enrollment)
nrow(current.act.readiness)
nrow(current.plan.readiness)
nrow(current.explore.readiness)
nrow(grade.nine.credits)
nrow(act.achieve.2014)
nrow(g38.achieve.2014)
#add new records
table(schools$SCHOOL_YEAR)
schools <- rbind(current.schools, schools[schools$SCHOOL_YEAR != current.school.year,])
table(schools$SCHOOL_YEAR)

table(schools_other_attributes$SCHOOL_YEAR)
schools_other_attributes <- rbind(current.schools.other.atributes, schools_other_attributes [schools_other_attributes $SCHOOL_YEAR != current.school.year,])
table(schools_other_attributes $SCHOOL_YEAR)

table(school_enrollment$SCHOOL_YEAR)
school_enrollment <- rbind(current.schools.enrollment, school_enrollment[school_enrollment$SCHOOL_YEAR != current.school.year,])
table(school_enrollment$SCHOOL_YEAR)

table(act.suite.readiness$SCHOOL_YEAR)
act.suite.readiness <- rbind(current.act.suite.readiness,act.suite.readiness[act.suite.readiness$SCHOOL_YEAR != current.school.year,])
table(act.suite.readiness$SCHOOL_YEAR)

act.achieve <- act.achieve.2014
table(act.achieve$SCHOOL_YEAR)

g38.achieve <- g38.achieve.2014
#save objects
save(schools, file="data/schools.orig.Rdata")
save(schools_other_attributes, file="data/schools-other-attributes.Rdata")
save(school_enrollment, file="data/school-enrollment.Rdata")

save(act.suite.readiness, file="data/ACT/act.suite.readiness.Rdata")

save(act.achieve, file="data/ACT/act.achieve.Rdata")
save(g38.achieve, file="data/g38.achieve.Rdata")
save(grade.nine.credits, file="data/grade.nine.credits.Rdata")
save(hathaway.eligibility, file="data/hathaway.eligibility.Rdata")

