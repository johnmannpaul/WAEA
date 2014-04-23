require(RODBC)
source('const/private/db.R')

as.is.vector <- function (conn, table) {
  
  cols <- sqlColumns(conn, table)$COLUMN_NAME
  result <- rep(TRUE, length(cols))
  names(result) <- cols
  result
  
}


##Oracle -> Rdata
Oracle.to.Rdata <- function(conn, data.dir="data-ora", Rdata.filename="schools.orig.Rdata", oracle.object.name="ACCOUNTABILITY.R13_SCHOOLS", R.object.name="schools", use.verbose=FALSE) {
  assign(R.object.name, sqlFetch(testing, oracle.object.name, as.is=as.is.vector(testing, oracle.object.name), nullstring=""))
  if (!file.exists(data.dir))
    dir.create(file.path(getwd(), data.dir))
  save(list=R.object.name, file=paste(data.dir, Rdata.filename, sep="/"))
}


data.dir = "data"

testing <- odbcConnect(dsn=db.dsn, uid=account.user, pwd=account.pwd)

Oracle.to.Rdata(testing, Rdata.filename="schools.orig.Rdata", oracle.object.name="ACCOUNTABILITY.R13_SCHOOLS", R.object.name="schools")
Oracle.to.Rdata(testing, Rdata.filename="schools-other-attributes.Rdata", oracle.object.name="ACCOUNTABILITY.R13_SCHOOLS_OTHER_ATTRIBUTES", R.object.name="schools_other_attributes")
Oracle.to.Rdata(testing, Rdata.filename="school-enrollment.Rdata", oracle.object.name="ACCOUNTABILITY.R13_SCHOOL_ENROLLMENT", R.object.name="school_enrollment")
Oracle.to.Rdata(testing, Rdata.filename="paws.Rdata", oracle.object.name="ACCOUNTABILITY.R13_PAWS", R.object.name="paws")
Oracle.to.Rdata(testing, Rdata.filename="Science_Scores_2013.Rdata", oracle.object.name="ACCOUNTABILITY.R13_SCIENCE_SCORES_2013", R.object.name="Science_Scores_2013")
Oracle.to.Rdata(testing, data.dir=paste(data.dir, "ACT", sep="/"), Rdata.filename="act.Rdata", oracle.object.name="ACCOUNTABILITY.R13_ACT", R.object.name="act")
Oracle.to.Rdata(testing, Rdata.filename="paws_11_achievement.Rdata", oracle.object.name="ACCOUNTABILITY.R13_PAWS_11_ACHIEVEMENT", R.object.name="paws_11_achievement")
Oracle.to.Rdata(testing, data.dir=paste(data.dir, "ACT", sep="/"), Rdata.filename="explore.Rdata", oracle.object.name="ACCOUNTABILITY.R13_EXPLORE", R.object.name="explore")
Oracle.to.Rdata(testing, data.dir=paste(data.dir, "ACT", sep="/"), Rdata.filename="plan.Rdata", oracle.object.name="ACCOUNTABILITY.R13_PLAN", R.object.name="plan")
Oracle.to.Rdata(testing, Rdata.filename="grads_nongrads_corrected.Rdata", oracle.object.name="ACCOUNTABILITY.R13_GRADS_NONGRADS_CORRECTED", R.object.name="grads_nongrads_corrected")
Oracle.to.Rdata(testing, Rdata.filename="paws_11.Rdata", oracle.object.name="ACCOUNTABILITY.R13_PAWS_11", R.object.name="paws_11")

odbcClose(testing)
