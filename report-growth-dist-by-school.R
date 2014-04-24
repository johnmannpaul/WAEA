source("constants.R")
source("function-defs.R")
source("initialize-schools.R")
source("initialize-paws.R")
source("reporting-defs.R")

round.to <- 1

paws.df <- paws[paws$TESTING_STATUS_CODE == "T" & paws$GRADE_ENROLLED != "11" & paws$SUBJECT_CODE %in% c('RE', 'MA') , ]

paws.aggregates <- produce.aggregates.scoped(paws.df,
                                             aggregator=function (x) 
                                               c(PERCENT_PROFICIENT =round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, round.to),
                                                 N_TESTS=length(x),
                                                 N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0))))

paws.N <- produce.aggregates.scoped(paws.df,
                                    obs="WISER_ID",
                                    value.label="N_TESTERS",
                                    aggregator=function (x) c(N_TESTERS=length(unique(x)))
)



paws.aggregates.tab <- paws.aggregates$tab

paws.N.norm <- paws.N$norm

paws.tab.N <- merge(paws.aggregates.tab, paws.N.norm[as.character(paws.N.norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])

names(paws.tab.N)[ncol(paws.tab.N)] <- 'N'


paws.tab.N[paws.tab.N$SCHOOL_YEAR=='2012-13' & paws.tab.N$SCHOOL_ID=='0101001' & paws.tab.N$STATISTIC=='PERCENT_PROFICIENT',]
##end validation



#propagate results to paired schools
paws.tab.N <- rbind(paws.tab.N, propagate.to.paired.schools(paws.tab.N))


#low and high growth
growthCuts <- c(35, 65)

paws.df$SGP <- as.numeric(paws.df$SGP)

growth.df <- paws.df[!is.na(paws.df$SGP),]


growth.aggregates <- produce.aggregates.scoped(growth.df,
                                               obs="SGP",
                                               aggregator=function (x) 
                                                 c(NLowGrowth = sum(ifelse(x <= growthCuts[1], 1, 0)),
                                                   NTypicalGrowth = sum(ifelse(growthCuts[1] < x & x <= growthCuts[2], 1, 0)),
                                                   NHighGrowth = sum(ifelse(growthCuts[2] < x, 1, 0)),
                                                   NGrowth=length(x),
                                                   PLowGrowth = round((sum(ifelse(x <= growthCuts[1], 1, 0))/length(x))*100,round.to),
                                                   PTypicalGrowth = round((sum(ifelse(growthCuts[1] < x & x <= growthCuts[2], 1, 0))/length(x))*100,round.to),
                                                   PHighGrowth = round((sum(ifelse(growthCuts[2] < x, 1, 0))/length(x))*100,round.to)
                                                   ))

growth.N <- produce.aggregates.scoped(growth.df,
                                      obs="WISER_ID",
                                      value.label="N_TESTERS",
                                      aggregator=function (x) c(N_TESTERS=length(unique(x)))
)


growth.tab.N <- merge(growth.aggregates$tab, growth.N$norm[as.character(growth.N$norm$SUBJECT_CODE)=='ALL',c("SCOPE", "SCHOOL_YEAR", "GRADE_ENROLLED", "SCHOOL_ID", "VALUE")])


names(growth.tab.N)[length(growth.tab.N)] <- "N"

#growth

growth.tab.N <- with(growth.tab.N, growth.tab.N[SCOPE=='SCHOOL', !(names(growth.tab.N) %in% c('SC','ORDER'))])

growth.tab.N.molten <- melt(growth.tab.N, id=c("SCOPE", "SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "STATISTIC","N"))
names(growth.tab.N.molten)[names(growth.tab.N.molten) %in% c("variable", 'N')] <- c("NStudentsGrowth", "Subject")

head(growth.tab.N.molten)

growth.tab.N.stat <- cast(growth.tab.N.molten, ... ~ STATISTIC)

head(growth.tab.N.stat[growth.tab.N.stat$SCHOOL_ID == '0101001' & growth.tab.N.stat$SCHOOL_YEAR=='2012-13',])


#achievement

paws.tab.N <- with(paws.tab.N, paws.tab.N[SCOPE=='SCHOOL', !(names(paws.tab.N) %in% c('SC','ORDER'))])

paws.tab.N.molten <- melt(paws.tab.N, id=c("SCOPE", "SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED", "STATISTIC","N"))
names(paws.tab.N.molten)[names(paws.tab.N.molten) %in% c("variable", "N")] <- c("NStudentsAchievement", "Subject")

head(paws.tab.N.molten)

paws.tab.N.stat <- cast(paws.tab.N.molten, ... ~ STATISTIC)

names(paws.tab.N.stat)[names(paws.tab.N.stat) %in% c("PERCENT_PROFICIENT", "N_TESTS", "N_PROFICIENT")] <- c("PProficient", "NTests", "NProficient")
head(paws.tab.N.stat[paws.tab.N.stat$SCHOOL_ID == '0101001' & paws.tab.N.stat$SCHOOL_YEAR=='2012-13',])



#combine



combo.N <- merge(growth.tab.N.stat, paws.tab.N.stat, by=c('SCOPE', 'SCHOOL_YEAR', 'SCHOOL_ID', 'GRADE_ENROLLED', 'Subject'), all.y=TRUE) 



head(combo.N[combo.N$SCHOOL_ID == '0101001' & combo.N$SCHOOL_YEAR=='2012-13',])

combo.N <- merge(combo.N, 
                 merge(schools[schools$WAEA_SCHOOL_TYPE %in% c(1,3,4,5),
                               c("SCHOOL_ID", "SCHOOL_YEAR", "DISTRICT_ID", 
                                 "DISTRICT_NAME", "NAME", "WAEA_SCHOOL_TYPE")], 
                       data.frame(Subject=unique(combo.N$Subject)), all=TRUE), by=c("SCHOOL_ID", "SCHOOL_YEAR", "Subject"))

combo.N$Subject <- as.character(combo.N$Subject)
combo.N$Subject_Description <- unlist(lapply(combo.N$Subject, function (s) {
  if (s == 'RE') {
    "Reading"
  } else {
    
    if (s == 'MA')
      "Mathematics"
    else
      "Reading & Math"
    
  }
}))

combo.N <- combo.N[combo.N$SCHOOL_YEAR >= '2012-13' & combo.N$GRADE_ENROLLED != '03',c('SCOPE', 'DISTRICT_ID', 'DISTRICT_NAME', 'SCHOOL_ID', 'NAME', 'SCHOOL_YEAR', 'GRADE_ENROLLED', 'Subject', 'Subject_Description',
                      'NLowGrowth', 'NTypicalGrowth', 'NHighGrowth', 'NGrowth', 'NStudentsGrowth',  'PLowGrowth', 'PTypicalGrowth', 
                      'PHighGrowth', 'PProficient', 'NProficient', 'NStudentsAchievement', 'NTests')]

combo.N[,c('NLowGrowth', 'NTypicalGrowth', 
           'NHighGrowth', 'NGrowth', 
           'NStudentsGrowth','NProficient', 
           'NStudentsAchievement', 'NTests')] <- data.frame(t(apply(combo.N[,c('NLowGrowth', 'NTypicalGrowth', 
                                                                               'NHighGrowth', 'NGrowth', 
                                                                               'NStudentsGrowth','NProficient', 
                                                                               'NStudentsAchievement', 'NTests')], 
                                                                    c(1),                                                                                      
                                                                    FUN=function (r) {
                                                                      ifelse(is.na(r), 0, r)
                                                                    }))) 

#do the district aggregations
combo.N.by.cols <- c("DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_YEAR", "GRADE_ENROLLED", "Subject","Subject_Description")
combo.N.by <- lapply(combo.N.by.cols, function (l) { combo.N[,l]})
names(combo.N.by) <- combo.N.by.cols
combo.N.counts.district <- aggregate(combo.N[,c("NLowGrowth", "NTypicalGrowth", "NHighGrowth", 
                                                "NGrowth", "NStudentsGrowth", "NProficient", "NStudentsAchievement", "NTests")],
                                     by = combo.N.by,
                                     sum)
                                     
                                  
combo.N.district <- cbind(data.frame(SCOPE=rep("DISTRICT", nrow(combo.N.counts.district))), 
                                 combo.N.counts.district[,c('DISTRICT_ID', 'DISTRICT_NAME')],
                                 data.frame(SCHOOL_ID=rep(NA, nrow(combo.N.counts.district)),
                                            NAME=rep(NA, nrow(combo.N.counts.district))),
                                 combo.N.counts.district[,c('SCHOOL_YEAR', 'GRADE_ENROLLED', 'Subject', 
                                                          'Subject_Description', 'NLowGrowth', 'NTypicalGrowth', 
                                                          'NHighGrowth', 'NGrowth', 'NStudentsGrowth')],
                                 data.frame(PLowGrowth = round((combo.N.counts.district$NLowGrowth/combo.N.counts.district$NGrowth)*100, precision),
                                            PTypicalGrowth = round((combo.N.counts.district$NTypicalGrowth/combo.N.counts.district$NGrowth)*100, precision),
                                            PHighGrowth = round((combo.N.counts.district$NHighGrowth/combo.N.counts.district$NGrowth)*100, precision),
                                            PProficient = round((combo.N.counts.district$NProficient/combo.N.counts.district$NTests)*100, precision)),

                                 combo.N.counts.district[,c('NProficient', 'NStudentsAchievement', 'NTests')])


combo.N.by.cols <- c("SCHOOL_YEAR", "GRADE_ENROLLED", "Subject","Subject_Description")
combo.N.by <- lapply(combo.N.by.cols, function (l) { combo.N[,l]})
names(combo.N.by) <- combo.N.by.cols
combo.N.counts.state <- aggregate(combo.N[,c("NLowGrowth", "NTypicalGrowth", "NHighGrowth", 
                                                "NGrowth", "NStudentsGrowth", "NProficient", "NStudentsAchievement", "NTests")],
                                     by = combo.N.by,
                                     sum)


combo.N.state <- cbind(data.frame(SCOPE=rep("STATE", nrow(combo.N.counts.state))), 
                          data.frame(DISTRICT_ID=rep(NA, nrow(combo.N.counts.state)),
                                     DISTRICT_NAME=rep(NA, nrow(combo.N.counts.state)),
                                     SCHOOL_ID=rep(NA, nrow(combo.N.counts.state)),
                                     NAME=rep(NA, nrow(combo.N.counts.state))),
                          combo.N.counts.state[,c('SCHOOL_YEAR', 'GRADE_ENROLLED', 'Subject', 
                                                     'Subject_Description', 'NLowGrowth', 'NTypicalGrowth', 
                                                     'NHighGrowth', 'NGrowth', 'NStudentsGrowth')],
                          data.frame(PLowGrowth = round((combo.N.counts.state$NLowGrowth/combo.N.counts.state$NGrowth)*100, precision),
                                     PTypicalGrowth = round((combo.N.counts.state$NTypicalGrowth/combo.N.counts.state$NGrowth)*100, precision),
                                     PHighGrowth = round((combo.N.counts.state$NHighGrowth/combo.N.counts.state$NGrowth)*100, precision),
                                     PProficient = round((combo.N.counts.state$NProficient/combo.N.counts.state$NTests)*100, precision)),
                          
                          combo.N.counts.state[,c('NProficient', 'NStudentsAchievement', 'NTests')])

combo.N.all <- rbind(combo.N, combo.N.district, combo.N.state)

combo.N.all$NStudentsGrowth <- with(combo.N.all, ifelse(Subject == 'ALL', NStudentsGrowth, NGrowth))
                                   
write.csv(combo.N.all,file="reporting/growth-distribution-by-school.csv", na="", row.names=FALSE, quote=FALSE)



with(paws, paws[SCHOOL_ID == '1902001' & SCHOOL_YEAR=='2012-13', c("WISER_ID")])
