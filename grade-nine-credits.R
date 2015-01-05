load(file="data/grade.nine.credits.Rdata")
table(grade.nine.credits$CREDITS_EARNED, useNA="ifany")
table(grade.nine.credits$MET_CREDIT_TARGET, useNA="ifany")

grade.nine.credits <- grade.nine.credits[grade.nine.credits$WAEA_EXEMPT=='F',]


#should be equal.  No duplicate wiser_ids 
length(grade.nine.credits$WISER_ID)
length(unique(grade.nine.credits$WISER_ID))
 


grade.nine.credits.school <- rbind(unmatrixfy.df(aggregate(data.frame(GRADE_NINE_CREDITS=grade.nine.credits$MET_CREDIT_TARGET),
                                                           by=list(SCHOOL_YEAR=grade.nine.credits$ACCOUNTABILITY_YEAR,
                                                                   SCHOOL_ID=grade.nine.credits$ACCOUNTABILITY_SCHOOL_ID),
                                                           function (v) {
                                                             c(N=length(v),
                                                               MET_N=sum(v=="MET"))
                                                           })),
                                   
                                   unmatrixfy.df(aggregate(data.frame(GRADE_NINE_CREDITS=grade.nine.credits$MET_CREDIT_TARGET),
                                                           by=list(SCHOOL_YEAR=grade.nine.credits$ACCOUNTABILITY_YEAR,
                                                                   SCHOOL_ID=rep(state.school.id, nrow(grade.nine.credits))),
                                                           function (v) {
                                                             c(N=length(v),
                                                               MET_N=sum(v=="MET"))
                                                           })))
                                   
grade.nine.credits.school <- merge(grade.nine.credits.school, 
                                    unique(grade.nine.credits[c("ACCOUNTABILITY_YEAR", 
                                                                "ACCOUNTABILITY_SCHOOL_ID", 
                                                                "REQUIRED_GRAD_CREDITS")]),
                                    all.x=TRUE,
                                    by.x=c("SCHOOL_YEAR", "SCHOOL_ID"),
                                    by.y=c("ACCOUNTABILITY_YEAR", "ACCOUNTABILITY_SCHOOL_ID"))

grade.nine.credits.school$SMALL_SCHOOL_GRADE_NINE_CREDIT <- ifelse(grade.nine.credits.school$GRADE_NINE_CREDITS_N < min.N.grade.nine.credits, "T", "F") 


table(grade.nine.credits.school$SMALL_SCHOOL_GRADE_NINE_CREDIT)

grade.nine.credits.school$PERCENT_GD_9_CREDIT_MET <- round((grade.nine.credits.school$GRADE_NINE_CREDITS_MET_N/
                                                             grade.nine.credits.school$GRADE_NINE_CREDITS_N) * 100, precision.readiness)

head(grade.nine.credits.school)

schools <- bind.indicator(schools, 
               grade.nine.credits.school,
               indicator.labels.min.N = c(N= "GRADE_NINE_CREDITS_N", score="PERCENT_GD_9_CREDIT_MET"),
               min.N.grade.nine.credits)

schools$SMALL_SCHOOL_GRADE_NINE_CREDIT <- apply(schools[,c("SCHOOL_ID", "SCHOOL_YEAR")],
                                                c(1),
                                                function(school) {
                                                  lookup <- grade.nine.credits.school[grade.nine.credits.school$SCHOOL_ID == school[["SCHOOL_ID"]] &
                                                                                        grade.nine.credits.school$SCHOOL_YEAR == school[["SCHOOL_YEAR"]],]
                                                  if (nrow(lookup) == 0)
                                                    NA
                                                  else
                                                    lookup$SMALL_SCHOOL_GRADE_NINE_CREDIT
                                                  
                                                })

head(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & schools$SCHOOL_YEAR==current.school.year,])

write.csv(schools[schools$WAEA_SCHOOL_TYPE %in% HS.types & 
                    schools$SCHOOL_YEAR==current.school.year &
                    !is.na(schools$GRADE_NINE_CREDITS_N) &
                    schools$GRADE_NINE_CREDITS_N >= min.N.grade.nine.credits &
                    schools$SCHOOL_ID != state.school.id,c("SCHOOL_ID", 
                                                               "SCHOOL_YEAR", 
                                                               "ALTERNATIVE_SCHOOL", 
                                                               "GRADE_NINE_CREDITS_N", "GRADE_NINE_CREDITS_MET_N", "SMALL_SCHOOL_GRADE_NINE_CREDIT", "PERCENT_GD_9_CREDIT_MET")], 
          file=get.filename("grade-nine-credits-cfds", "results/cfds"), na="", row.names=FALSE)


