require(RODBC)
source('const/private/db.R')



object.name <- function (schema, simple.name) {
  paste(schema, simple.name, sep=".")  
}



estimate.varchar.lengths <- function (df) {

  estimate.varchar.lengths.aux <- function (x) {
    if (class(df[[x]]) == "character") {
      max.length <-max(nchar(df[[x]][which(!is.na(df[[x]]))]))
      if (max.length == 0) #still create a column with width 1 even if every value is empty
        max.length = 1
      log.max.length <- log(max.length, 2)
      bytes <- floor(log.max.length)
      if (bytes != log.max.length) {
        bytes = bytes + 1        
      } 
      paste("varchar(", 2^bytes, ")", sep="")        
    }
    else {
      NULL
    }
  }
  
  varcharTypes = unlist(lapply(names(df), estimate.varchar.lengths.aux))
  names(varcharTypes) = unlist(lapply(names(df), function (n) if (class(df[[n]])=="character") n else NULL))
  varcharTypes
} 



testing <- odbcConnect(dsn=db.dsn, uid=rdbms.name, pwd=rdbms.pwd)


source("constants.R")
source("function-defs.R")
source("services.R")
source("initialize-schools.R")
sqlSave(channel=testing, 
        dat=schools, 
        tablename=object.name(model.schema,'DM2013_SCHOOL'),
        verbose=TRUE,
        rownames=FALSE,
        varTypes=c(estimate.varchar.lengths(schools), c(WAEA_SCHOOL_TYPE='integer', YEAR='integer', ENROLLMENT='integer'))
)
sqlQuery(channel=testing,
         query=paste("ALTER TABLE", object.name(model.schema,'DM2013_SCHOOL'), "ADD CONSTRAINT SCHOOL_PK PRIMARY KEY (SCHOOL_YEAR, SCHOOL_ID)"))
source("initialize-paws.R")
sqlSave(channel=testing, 
        dat=paws, 
        tablename=object.name(model.schema,'DM2013_PAWS_3_THRU_8'),
        #verbose=TRUE,
        rownames=FALSE,
        varTypes=c(estimate.varchar.lengths(paws), c(GRADE_BAND='integer', YEAR='integer'))
)
sqlQuery(channel=testing,
         query=paste("ALTER TABLE",  object.name(model.schema,'DM2013_PAWS_3_THRU_8'), "ADD CONSTRAINT DM2013_PAWS_3_THRU_8_PK PRIMARY KEY (SCHOOL_YEAR, SCHOOL_ID, WISER_ID, SUBJECT_CODE)"))
source("small-schools.R")
source("achievement-bands.R")
source("growth.R")
source("equity.R")
source("participation.R")
source("SPL-nonHS.R")
source("init-HS-achievement.R")
sqlSave(channel=testing, 
        dat=act, 
        tablename=object.name(model.schema,'DM2013_ACT'),
        #verbose=TRUE,
        rownames=FALSE,
        varTypes = c(estimate.varchar.lengths(act))
)
sqlQuery(channel=testing,
         query=paste("ALTER TABLE", object.name(model.schema,'DM2013_ACT'), "ADD CONSTRAINT DM2013_ACT_PK PRIMARY KEY (SCHOOL_YEAR, SCHOOL_ID, WISER_ID)"))

sqlSave(channel=testing, 
        dat=paws_11_achievement,
        tablename=object.name(model.schema,'DM2013_PAWS_11_WIDE'),
        #verbose=TRUE,
        rownames=FALSE,
        varTypes = c(estimate.varchar.lengths(paws_11_achievement)) 
)
sqlQuery(channel=testing,
         query=paste("ALTER TABLE", object.name(model.schema,'DM2013_PAWS_11_WIDE'), "ADD CONSTRAINT DM2013_PAWS_11_WIDE_PK PRIMARY KEY (SCHOOL_YEAR, SCHOOL_ID, WISER_ID)"))
source("achievement-act.R")
source("readiness-ref.R")
sqlSave(channel=testing, 
        dat=explore,
        tablename=object.name(model.schema,'DM2013_EXPLORE'),
        #verbose=TRUE,
        rownames=FALSE,
        varTypes = c(estimate.varchar.lengths(explore)) 
)
sqlQuery(channel=testing,
         query=paste("ALTER TABLE", object.name(model.schema,'DM2013_EXPLORE'), "ADD CONSTRAINT DM2013_EXPLORE_PK PRIMARY KEY (SCHOOL_YEAR, SCHOOL_ID, WISER_ID)"))

sqlSave(channel=testing, 
        dat=plan,
        tablename=object.name(model.schema,'DM2013_PLAN'),
        #verbose=TRUE,
        rownames=FALSE,
        varTypes = c(estimate.varchar.lengths(plan)) 
)
sqlQuery(channel=testing,
         query=paste("ALTER TABLE", object.name(model.schema,'DM2013_PLAN'),  "ADD CONSTRAINT DM2013_PLAN_PK PRIMARY KEY (SCHOOL_YEAR, SCHOOL_ID, WISER_ID)"))

source("grad-rate-ref.R")
sqlSave(channel=testing, 
        dat=grads_nongrads_corrected_full,
        tablename=object.name(model.schema,'DM2013_GRAD'),
        #verbose=TRUE,
        rownames=FALSE,
        varTypes = c(estimate.varchar.lengths(grads_nongrads_corrected_full)) 
)
sqlQuery(channel=testing,
         query=paste("ALTER TABLE", object.name(model.schema,'DM2013_GRAD'), "ADD CONSTRAINT DM2013_GRAD PRIMARY KEY (SCHOOL_YEAR, SCHOOL_ID, WISER_ID)"))

source("total-readiness.R")
source("high-school-equity.R")
sqlSave(channel=testing, 
        dat=paws_11,
        tablename=object.name(model.schema,'DM2013_PAWS_11_LONG'),
        #verbose=TRUE,
        rownames=FALSE,
        varTypes = c(estimate.varchar.lengths(paws_11)) 
)
sqlQuery(channel=testing,
         query=paste("ALTER TABLE",object.name(model.schema,'DM2013_PAWS_11_LONG'), "ADD CONSTRAINT DM2013_PAWS_11_LONG PRIMARY KEY (SCHOOL_YEAR, SCHOOL_ID, WISER_ID, SUBJECT_CODE)"))

source("SPL-HS.R")

##finish
source("schools-fin.R")
odbcClose(testing)


###write the results to RDBMS
source("process-with-corrections.R")
testing <- odbcConnect(dsn=db.dsn, uid=account.user, pwd=account.pwd)
driver <- odbcGetInfo(testing)[["DBMS_Name"]]
setSqlTypeInfo(driver, list(double="number(5,2)", integer='decimal', character='varchar(255)', logical="varchar(5)"))
getSqlTypeInfo(driver)
schools$YEARS_BACK <- as.character(schools$YEARS_BACK)
schools$YEARS_BACK_HS <- as.character(schools$YEARS_BACK_HS)
sqlSave(channel=testing, 
        dat=schools, 
        tablename=object.name(model.schema,'DM2013_SPL'),
        verbose=TRUE,
        varTypes=c(estimate.varchar.lengths(schools), c(WAEA_SCHOOL_TYPE='integer', YEAR='integer', ENROLLMENT='integer',                    
                   N_ACHIEVEMENT='integer', ACHIEVEMENT_TARGET_LEVEL='integer', 
                   N_ACHIEVEMENT_BAND_1='integer',  N_ACHIEVEMENT_BAND_2='integer',
                   N_GROWTH='integer', GROWTH_TARGET_LEVEL='integer', 
                   N_SUBGROUP='integer', EQUITY_TARGET_LEVEL='integer', 
                   N_INDICATORS='integer', SPL='integer', SPL_ADJUSTED='integer',
                   N_ACHIEVEMENT_HS='integer', ACHIEVEMENT_TARGET_LEVEL_HS='integer', 
                   N_TESTED_READINESS='integer', N_GRADUATION='integer', N_TOTAL_READINESS_HS='integer', 
                   READINESS_TARGET_LEVEL='integer', 
                   N_EQUITY_HS='integer', N_ACHIEVEMENT_HS_PRIOR='integer', PERCENT_NONPROFICIENT_CATEGORY='integer',
                   IMPROVEMENT_CATEGORY='integer', EQUITY_TARGET_LEVEL_HS='integer', 
                   N_INDICATORS_HS='integer', SPL_HS='integer', SPL_ADJUSTED_HS='integer',
                   ACCOUNTABILITY_SPL='integer', ACCOUNTABILITY_N_INDICATORS='integer')),
        rownames=FALSE)
odbcClose(testing)



#save raw Rdata files for doing 2013 calculation in R
Rdata.to.RDBMS <- function (conn, Rdata.filename, use.verbose=FALSE, exclude.cols=NULL,  other.var.types=NULL, table.prefix = paste(model.schema,"R13_",sep='.') {
  obj.name <- load(Rdata.filename)
  obj <- eval(as.name(obj.name))
  if (!is.null(exclude.cols))
    obj <- obj[,!(names(obj) %in% exclude.cols)]
  sqlSave(channel=conn, 
          dat=obj, 
          tablename=paste(table.prefix, toupper(do.call(paste, as.list(c(strsplit(obj.name,'[.]')[[1]], sep="_")))), sep=""),
          verbose=use.verbose,
          varTypes=c(estimate.varchar.lengths(obj), other.var.types),
          rownames=FALSE)
}




#Rdata -> RDBMS
testing <- odbcConnect(dsn=db.dsn, uid=account.user, pwd=account.pwd)
Rdata.to.RDBMS(testing, "data/schools.orig.Rdata", TRUE, c("EFFECTIVE_DATE", "EXPIRATION_DATE"))
Rdata.to.RDBMS(testing, "data/schools-other-attributes.Rdata", TRUE)
Rdata.to.RDBMS(testing, "data/school-enrollment.Rdata", TRUE, NULL, c(ENROLLMENT='integer'))
Rdata.to.RDBMS(testing, "data/paws.Rdata")
#resume here
Rdata.to.RDBMS(testing, "data/Science_Scores_2013.Rdata")
Rdata.to.RDBMS(testing, "data/ACT/act.Rdata")
Rdata.to.RDBMS(testing, "data/paws_11_achievement.Rdata")
Rdata.to.RDBMS(testing, "data/ACT/explore.Rdata")
Rdata.to.RDBMS(testing, "data/ACT/plan.Rdata")
Rdata.to.RDBMS(testing, "data/grads_nongrads_corrected.Rdata")
Rdata.to.RDBMS(testing, "data/paws_11.Rdata")


odbcClose(testing)

