
##
##OBSOLETE: superseded by achievement-bands.R
##
save(paws, file="paws.Rdata")
paws.df <- paws[paws$SCHOOL_FULL_ACADEMIC_YEAR=="T" & paws$TESTING_STATUS_CODE != "X" & paws$GRADE_ENROLLED != "11", ]
table(paws.df$SCHOOL_FULL_ACADEMIC_YEAR)
table(paws.df$TESTING_STATUS_CODE)
table(paws.df$GRADE_ENROLLED)
#paws.df$AYP_FINAL_PERF_LEVEL <- as.numeric(paws.df$AYP_FINAL_PERF_LEVEL) 

school.ids.paired <- apply(paws.df[,c("SCHOOL_YEAR", "SCHOOL_ID")], 
      c(1), 
      function (school) {
          pairing <- school.pairing.lookup[[school[["SCHOOL_YEAR"]]]]
          paired.school <- pairing[[school[["SCHOOL_ID"]]]]
          ifelse(is.null(paired.school), school[["SCHOOL_ID"]], paired.school)
          
})

##some validations
school.ids.compare <- data.frame(school.id.paired=school.ids.paired, school.id=paws.df[,c("SCHOOL_ID")])
names(school.ids.compare) <- c("school.id.paired", "school.id")
changes <- school.ids.compare[school.ids.compare$school.id %in% names(school.pairing.lookup$`2010-11`),]
no.changes <- school.ids.compare[!(school.ids.compare$school.id %in% names(school.pairing.lookup$`2010-11`)),]
nrow(no.changes[no.changes$school.id != no.changes$school.id.paired,])
head(changes)
##end validations

nrow(paws.df[paws.df$SCHOOL_ID=='1001006' & paws.df$GRADE_ENROLLED=='03',])
paws.df$SCHOOL_ID <- school.ids.paired
nrow(paws.df[paws.df$SCHOOL_ID=='1001002' & paws.df$GRADE_ENROLLED=='03',])


# achievement <- aggregate(data.frame(PROFICIENT=PROFICIENT, N=rep(1,length(PROFICIENT))), 
#                          by=list(SCHOOL_YEAR=paws.df$SCHOOL_YEAR, 
#                                  SCHOOL_ID=paws.df$SCHOOL_ID, 
#                                  SUBJECT_CODE=paws.df$SUBJECT_CODE, 
#                                  GRADE_ENROLLED=paws.df$GRADE_ENROLLED), 
#                          sum)

PROFICIENT <- ifelse(paws.df$ACCOUNTABILITY_PERF_LEVEL %in% c("3","4"), 1, 0)

achievement <- aggregate(data.frame(PROFICIENT=PROFICIENT, N=rep(1,length(PROFICIENT))), 
                         by=list(SCHOOL_YEAR=paws.df$SCHOOL_YEAR, 
                                 SCHOOL_ID=paws.df$SCHOOL_ID), 
                         sum)


achievement$PERCENT_PROFICIENT <- round(achievement$PROFICIENT/achievement$N * 100, 1)


achievement.subject <- aggregate(data.frame(PROFICIENT=PROFICIENT, N=rep(1,length(PROFICIENT))), 
                          by=list(SCHOOL_YEAR=paws.df$SCHOOL_YEAR, 
                                  SUBJECT_CODE=paws.df$SUBJECT_CODE,
                                  SCHOOL_ID=paws.df$SCHOOL_ID), 
                          sum)

achievement.max.N <- aggregate(data.frame(MAX_N=achievement.subject$N), 
                              by=list(SCHOOL_YEAR=achievement.subject$SCHOOL_YEAR, 
                                      SCHOOL_ID=achievement.subject$SCHOOL_ID), 
                              max)

achievement <- merge(achievement, achievement.max.N)


##growth

paws.df$SGP <- as.numeric(paws.df$SGP)

growth.df <- paws.df[!is.na(paws.df$SGP),]

growth <- aggregate(growth.df$SGP, by=list(SCHOOL_YEAR=growth.df$SCHOOL_YEAR, 
                                            SCHOOL_ID=growth.df$SCHOOL_ID), 
                       median)

names(growth)[3] <- "MGP"

growth.subject.N <- aggregate(data.frame(N=rep(1, nrow(growth.df))), 
                      by=list(SCHOOL_YEAR=growth.df$SCHOOL_YEAR, 
                              SCHOOL_ID=growth.df$SCHOOL_ID,
                              SUBJECT_CODE=growth.df$SUBJECT_CODE), 
                    sum)

growth.N <- aggregate(data.frame(N=growth.subject.N$N), 
                              by=list(SCHOOL_YEAR=growth.subject.N$SCHOOL_YEAR, 
                                      SCHOOL_ID=growth.subject.N$SCHOOL_ID),
                              max)

growth <- merge (growth, growth.N)

Sturges <- function (sample) {
  ceiling(log(length(sample),2)) + 1 
}

Scott <- function (sample) {
  h<-3.5 * sqrt(var(sample))/length(sample)^(1/3)  
  ceiling((max(sample) - min(sample))/h)
}



#creates a folder called "plots" if it doesn't already exist
if (!file.exists("plots"))
  dir.create("plots")

#create histgram pdfs for 2010-11 and 2011-12 MGPs in plots folder
sapply(c("2010-11", "2011-12"), function (year) {
  k<-Scott(growth[growth$SCHOOL_YEAR=='2011-12' & growth$N > 5,"MGP"])
  #k<-Sturges(growth[growth$SCHOOL_YEAR=='2011-12' & growth$N > 5,"MGP"])
  pdf(file=paste("plots/MGP", year, "hist.pdf", sep="-"))
  hist(growth[growth$SCHOOL_YEAR==year & growth$N > 5 ,"MGP"], 
     n=k, 
     xlab="MGP", 
     main=paste("Histogram of", year, "MGPs for schools with N > 5", sep=" "))
  dev.off()
})

#35th and 65th percentiles for 2012-12 MGPs for schools with N > 5
quantile(growth[growth$SCHOOL_YEAR=='2011-12' & growth$N > 5,]$MGP, c(.35, .65))

