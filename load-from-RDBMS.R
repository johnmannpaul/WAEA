source('db-functions.R')
source('const/private/db.R')

data.dir = "data"

testing <- odbcConnect(dsn=db.dsn, uid=rdbms.name, pwd=rdbms.pwd)

RDBMS.to.Rdata(testing, data.dir=data.dir, Rdata.filename="schools.orig.Rdata", RDBMS.object.name=object.name(model.schema,"R13_SCHOOLS"), R.object.name="schools")
RDBMS.to.Rdata(testing, data.dir=data.dir, Rdata.filename="schools-other-attributes.Rdata", RDBMS.object.name=object.name(model.schema,"R13_SCHOOLS_OTHER_ATTRIBUTES"), R.object.name="schools_other_attributes")
RDBMS.to.Rdata(testing, data.dir=data.dir, Rdata.filename="school-enrollment.Rdata", RDBMS.object.name=object.name(model.schema,"R13_SCHOOL_ENROLLMENT"), R.object.name="school_enrollment")
RDBMS.to.Rdata(testing, data.dir=data.dir, Rdata.filename="paws.Rdata", RDBMS.object.name=object.name(model.schema,"R13_PAWS"), R.object.name="paws")
RDBMS.to.Rdata(testing, data.dir=data.dir, Rdata.filename="Science_Scores_2013.Rdata", RDBMS.object.name=object.name(model.schema,"R13_SCIENCE_SCORES_2013"), R.object.name="Science_Scores_2013")
RDBMS.to.Rdata(testing, data.dir=paste(data.dir, "ACT", sep="/"), Rdata.filename="act.Rdata", RDBMS.object.name=object.name(model.schema,"R13_ACT"), R.object.name="act")
RDBMS.to.Rdata(testing, data.dir=data.dir, Rdata.filename="paws_11_achievement.Rdata", RDBMS.object.name=object.name(model.schema,"R13_PAWS_11_ACHIEVEMENT"), R.object.name="paws_11_achievement")
RDBMS.to.Rdata(testing, data.dir=paste(data.dir, "ACT", sep="/"), Rdata.filename="explore.Rdata", RDBMS.object.name=object.name(model.schema,"R13_EXPLORE"), R.object.name="explore")
RDBMS.to.Rdata(testing, data.dir=paste(data.dir, "ACT", sep="/"), Rdata.filename="plan.Rdata", RDBMS.object.name=object.name(model.schema,"R13_PLAN"), R.object.name="plan")
RDBMS.to.Rdata(testing, data.dir=data.dir, Rdata.filename="grads_nongrads_corrected.Rdata", RDBMS.object.name=object.name(model.schema,"R13_GRADS_NONGRADS_CORRECTED"), R.object.name="grads_nongrads_corrected")
RDBMS.to.Rdata(testing, data.dir=data.dir, Rdata.filename="paws_11.Rdata", RDBMS.object.name=object.name(model.schema,"R13_PAWS_11"), R.object.name="paws_11")

odbcClose(testing)
