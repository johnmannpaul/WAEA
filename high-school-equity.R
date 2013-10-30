#source("achievement-act.R")

#hs.equity.df <- act.achievement[act.achievement$N_ACHIEVEMENT > 14,]
#this equity metric is really only defined for schools for which we did not combine years of data (ie. ones where small_school_hs == 'F')
hs.equity.df <- act.achievement[act.achievement$SCHOOL_ID %in% schools[schools$SCHOOL_YEAR==current.school.year & 
                                                                         schools$SMALL_SCHOOL_HS=='F', "SCHOOL_ID"],]
table(hs.equity.df$SCHOOL_YEAR)


#We only consider proficienies in math and reading.
hs.equity.df$PERCENT_NONPROFICIENT <- round(((hs.equity.df$MATH_TESTED + hs.equity.df$READING_TESTED - 
                                         hs.equity.df$MATH_PROFICIENT - hs.equity.df$READING_PROFICIENT)/(hs.equity.df$MATH_TESTED + hs.equity.df$READING_TESTED)) * 100, hs.equity.precision)

#Being the inverse of PERCENT_NON_PROFICIENT, PERCENT_PROFICIENT will only include Reading and Math.
#This is different from the  Achievement indicator, where it did include Science, too.
hs.equity.df$PERCENT_PROFICIENT <- 100 - hs.equity.df$PERCENT_NONPROFICIENT


hs.equity.df <- hs.equity.df[,c("SCHOOL_YEAR", "SCHOOL_ID", "PERCENT_PROFICIENT", "N_ACHIEVEMENT",
                          "PERCENT_NONPROFICIENT")]

head(hs.equity.df)


#save(paws_11, file="data/paws_11.Rdata")
load(file="data/paws_11.Rdata")
paws_11.df <- paws_11[paws_11$SCHOOL_FULL_ACADEMIC_YEAR=="T" & 
                        paws_11$TESTING_STATUS_CODE == "T" & 
                        paws_11$GRADE_ENROLLED == "11" &
                        paws_11$SUBJECT_CODE %in% c('MA', 'RE'), ]

table(with(paws_11.df, paws_11.df[SCHOOL_ID %in% c('0706056', '0706055'),"SCHOOL_ID"]))
paws_11.df$SCHOOL_ID <- ifelse(paws_11.df$SCHOOL_ID == '0706055', '0706056', paws_11.df$SCHOOL_ID)
table(with(paws_11.df, paws_11.df[SCHOOL_ID %in% c('0706056', '0706055'),"SCHOOL_ID"]))

NONPROFICIENT <- ifelse(paws_11.df$ACCOUNTABILITY_PERF_LEVEL %in% c("1","2"), 1, 0)

achievement_11 <- aggregate(data.frame(NONPROFICIENT=NONPROFICIENT, N=rep(1,length(NONPROFICIENT))), 
                         by=list(SCHOOL_YEAR=paws_11.df$SCHOOL_YEAR, 
                                 SCHOOL_ID=paws_11.df$SCHOOL_ID), 
                         sum)

#get the state level aggregate in there
achievement_11 <- state.level.aggregate(achievement_11, sum, state.school.id)

tail(achievement_11)
head(achievement_11)

achievement_11$PERCENT_NONPROFICIENT <- round(achievement_11$NONPROFICIENT/achievement_11$N * 100, hs.equity.precision)



paws.11.tested <- with(paws_11.df, paws_11.df[TESTING_STATUS_CODE == "T", c("SCHOOL_YEAR", 
                                                                   "SCHOOL_ID",
                                                                   "WISER_ID")])

paws.11.tested.school <- cast(paws.11.tested, SCHOOL_YEAR+SCHOOL_ID~., 
                           function (x) length(unique(x)), margins="SCHOOL_YEAR")

names(paws.11.tested.school)[3] <- "N_ACHIEVEMENT"

#relabale state aggregate to state school id
paws.11.tested.school[paws.11.tested.school$SCHOOL_ID=='(all)',]
paws.11.tested.school$SCHOOL_ID <- as.character(paws.11.tested.school$SCHOOL_ID)
paws.11.tested.school[paws.11.tested.school$SCHOOL_ID=='(all)',c("SCHOOL_ID")] <- rep(state.school.id, nrow(paws.11.tested.school[paws.11.tested.school$SCHOOL_ID=='(all)',]))
with(paws.11.tested.school, paws.11.tested.school[SCHOOL_ID==state.school.id,])


with(paws.11.tested.school, paws.11.tested.school[SCHOOL_ID==state.school.id,])

achievement_11 <- merge(achievement_11, paws.11.tested.school)

head(achievement_11)
tail(achievement_11)

hs.equity.df$PREV_YEAR <- as.numeric(substr(hs.equity.df$SCHOOL_YEAR, 1, 4))
hs.equity.df$YEAR <- as.numeric(substr(hs.equity.df$SCHOOL_YEAR, 1, 4)) + 1
achievement_11$YEAR <- as.numeric(substr(achievement_11$SCHOOL_YEAR, 1, 4)) + 1

table(hs.equity.df$SCHOOL_YEAR)
hs.equity.df <- merge(hs.equity.df, 
                   achievement_11[,c("YEAR", 
                                     "SCHOOL_ID", 
                                     "PERCENT_NONPROFICIENT", 
                                     "N_ACHIEVEMENT")],
                   by.x=c("PREV_YEAR", "SCHOOL_ID"),
                   by.y=c("YEAR", "SCHOOL_ID"))
table(hs.equity.df$SCHOOL_YEAR)

names(hs.equity.df)[grep("\\.y",names(hs.equity.df), value=FALSE)] <- 
  sub("\\.y", "_PRIOR", names(hs.equity.df)[grep("\\.y",names(hs.equity.df), value=FALSE)])
names(hs.equity.df)

names(hs.equity.df)[grep("\\.x",names(hs.equity.df), value=FALSE)] <- 
  sub("\\.x", "", names(hs.equity.df)[grep("\\.x",names(hs.equity.df), value=FALSE)])
names(hs.equity.df)


hs.equity.df$IMPROVEMENT_VALUE <- hs.equity.df$PERCENT_NONPROFICIENT - hs.equity.df$PERCENT_NONPROFICIENT_PRIOR

quantile(hs.equity.df[hs.equity.df$N_ACHIEVEMENT > 14 & hs.equity.df$SCHOOL_ID!=state.school.id,"IMPROVEMENT_VALUE"], c(.33,.66))


hs.equity.df$IMPROVEMENT_CATEGORY <- sapply(hs.equity.df$IMPROVEMENT_VALUE,
                                            function (iv) {
                                              cuts <- hs.equity.level.lookup$IMPROVEMENT_VALUE
                                              if (iv <= cuts[1]) {
                                                3
                                              } else {
                                                
                                                if (iv > cuts[1] & iv <= cuts[2])
                                                  2
                                                else
                                                  1
                                              }                         
                                              
                                            })
quantile(hs.equity.df[hs.equity.df$N_ACHIEVEMENT > 14,"PERCENT_NONPROFICIENT"], c(.33,.66))

hs.equity.df$PERCENT_NONPROFICIENT_CATEGORY <- sapply(hs.equity.df$PERCENT_NONPROFICIENT,
                                                      function (iv) {
                                                        cuts <- hs.equity.level.lookup$PERCENT_NONPROFICIENT
                                                        if (iv <= cuts[1]) {
                                                          3
                                                        } else {
                                                          
                                                          if (iv > cuts[1] & iv <= cuts[2])
                                                            2
                                                          else
                                                            1
                                                        }                                              
                                                      }) 
#IMPROVEMENT x PERCENT_NON_PROFICIENT
#1 1 2
#1 2 3
#2 3 3
# hs.equity.df$EQUITY_TARGET_LEVEL <- apply(hs.equity.df[,c("IMPROVEMENT_CATEGORY", "PERCENT_NONPROFICIENT_CATEGORY")],
#                                    c(1),
#                                    function (values) {
#                                      score <- values[["IMPROVEMENT_CATEGORY"]] + values[["PERCENT_NONPROFICIENT_CATEGORY"]]
#                                      if (score %in% c(2,3)) {
#                                        1
#                                      } else {
#                                        
#                                        if (score == 4)
#                                          2
#                                        else #5 or 6
#                                          3                                      
#                                      }
#                                    })


#IMPROVEMENT x PERCENT_NON_PROFICIENT
#1 1 1
#2 2 2
#3 3 3
#PJP 2013 changed the above definition to this one
hs.equity.df$EQUITY_TARGET_LEVEL <- apply(hs.equity.df[,c("IMPROVEMENT_CATEGORY", "PERCENT_NONPROFICIENT_CATEGORY")],
                                          c(1),
                                          function (values) {
                                            score <- values[["IMPROVEMENT_CATEGORY"]] 
                                            as.numeric(score)
                                          })

 
##assign the relavent values to the schools fram
schools <- schools[, !(names(schools) %in% hs.equity.labels)]

schools <- cbind(schools, data.frame(t(apply(schools[,c("SCHOOL_ID",                                                     
                                                        "SCHOOL_YEAR", "SMALL_SCHOOL_HS")], c(1), 
                                             FUN=calc_hs_equity))))  

#names(schools)[(length(schools) - length(hs.equity.labels) + 1):length(schools)] <- hs.equity.labels
head(schools)
table(schools$EQUITY_TARGET_LEVEL_HS)

