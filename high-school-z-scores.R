require(RODBC)
source("const/private/db.R")
precision=1

load("data/ACT/act.Rdata")

rdbms.connect <- odbcConnect(dsn=db.dsn, uid=rdbms.name, pwd=rdbms.pwd)


#act.rdbms <- sqlFetch(rdbms.connect, "ACT_STUDENT_RESULTS_201213", as.is=as.is.vector(rdbms.connect, "ACT_STUDENT_RESULTS_201213"))
act.rdbms <- sqlFetch(rdbms.connect, paste(assess.schema, "ACT_STUDENT_RESULTS_201213", sep='.')) 

odbcClose(rdbms.connect)

act.writing <- with(act.rdbms, act.rdbms[,c("WISER_ID", "ACT_COMBINED_ENG_WRITING_SCORE")])
table(act.writing$ACT_COMBINED_ENG_WRITING_SCORE, useNA="ifany")
act.writing$ACT_COMBINED_ENG_WRITING_SCORE <- ifelse(act.writing$ACT_COMBINED_ENG_WRITING_SCORE=='--', 
                                                     NA,
                                                     act.writing$ACT_COMBINED_ENG_WRITING_SCORE)
table(act.writing$ACT_COMBINED_ENG_WRITING_SCORE, useNA="ifany")
act.writing <- with(act.writing, act.writing[!is.na(ACT_COMBINED_ENG_WRITING_SCORE),])
table(act.writing$ACT_COMBINED_ENG_WRITING_SCORE, useNA="ifany")

head(act)
nrow(act)
act <- merge(act, act.writing, by="WISER_ID", all.x=TRUE)
nrow(act)

names(act)[names(act) %in% c("ACT_COMBINED_ENG_WRITING_SCORE")] <- "ACT_SCALE_SCORE_ENG_WRITING"
names(act)


agg.fun <- function (rows) {        
  c(N=length(which(!is.na(as.numeric(rows)))), MEAN=round(mean(as.numeric(rows), na.rm=TRUE), precision), SD=round(sd(as.numeric(rows), na.rm=TRUE), precision))
}

act.stats <-  aggregate(data.frame(ACT_SCALE_SCORE_MATH = act$ACT_SCALE_SCORE_MATH,
                                   ACT_SCALE_SCORE_READING = act$ACT_SCALE_SCORE_READING,
                                   ACT_SCALE_SCORE_SCIENCE = act$ACT_SCALE_SCORE_SCIENCE,
                                   ACT_SCALE_SCORE_ENG_WRITING = act$ACT_SCALE_SCORE_ENG_WRITING), 
                        by=list(),
                        agg.fun          
)
#These are the baseline values that will be used to compute the standardized ACT scores in 2014 and beyond
act.stats

calc.z.scores <- function (row) {
  
  calc.z.score <- function (i) {
    subject = names(row)[i]
    score = as.numeric(row[i])
    
    mu = act.stats[[subject]][2] 
    sigma = act.stats[[subject]][3]
    (score-mu)/sigma                
  }
  
  z.scores <- unlist(lapply(1:length(row), calc.z.score))
  
  
  names(z.scores) <- c("ACT_Z_SCORE_MATH", "ACT_Z_SCORE_READING", "ACT_Z_SCORE_SCIENCE", "ACT_Z_SCORE_ENG_WRITING")
  z.scores
}

act.standardized <- cbind(act, data.frame(t(apply(act[,c("ACT_SCALE_SCORE_MATH","ACT_SCALE_SCORE_READING", "ACT_SCALE_SCORE_SCIENCE", "ACT_SCALE_SCORE_ENG_WRITING")],
                                                  c(1),
                                                  calc.z.scores))))

act.z.stats <-  aggregate(data.frame(ACT_Z_SCORE_MATH = act.standardized$ACT_Z_SCORE_MATH,
                                     ACT_Z_SCORE_READING = act.standardized$ACT_Z_SCORE_READING,
                                     ACT_Z_SCORE_SCIENCE = act.standardized$ACT_Z_SCORE_SCIENCE,
                                     ACT_Z_SCORE_ENG_WRITING = act.standardized$ACT_Z_SCORE_ENG_WRITING), 
                          by=list(),
                          agg.fun          
)
length(table(act.standardized$ACT_Z_SCORE_MATH))
length(table(act.standardized$ACT_SCALE_SCORE_MATH))
head(act.z.stats)

#TODO:  Add in Alt Z-scores.  compute school statistics.  Refactor common code.
