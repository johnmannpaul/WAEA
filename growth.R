#based on data/PAWS_ACCOUNTABILITY_LONG.csv
##assumes achievement.R has been used to define paws.df
##growth

growth.df <- paws.df[!is.na(paws.df$SGP),]
growth.df$ACHIEVEMENT_LEVEL_PRIOR <- as.numeric(growth.df$ACHIEVEMENT_LEVEL_PRIOR)

growth <- aggregate(growth.df$SGP, by=list(SCHOOL_YEAR=growth.df$SCHOOL_YEAR, 
                                           SCHOOL_ID=growth.df$SCHOOL_ID), 
                    median)

#state SGP has to ignore the records from previous years to buttress small schools
growth.state <- aggregate(growth.df$SGP, by=list(SCHOOL_YEAR=growth.df$SCHOOL_YEAR), 
                    median)

growth.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(growth.state)), growth.state)

growth <- rbind(growth, growth.state)

names(growth)[3] <- "MGP"

head(growth)
tail(growth)

PROFICIENT_PRIOR <- ifelse(growth.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(3,4), 1, 0)

growth.N <- aggregate(data.frame(PROFICIENT_PRIOR=PROFICIENT_PRIOR, N_SGP=rep(1, nrow(growth.df))), by=list(SCHOOL_YEAR=growth.df$SCHOOL_YEAR, 
                                                       SCHOOL_ID=growth.df$SCHOOL_ID), 
                      sum)


#get the state in there
growth.N.state <- aggregate(data.frame(PROFICIENT_PRIOR=PROFICIENT_PRIOR, N_SGP=rep(1, nrow(growth.df))), by=list(SCHOOL_YEAR=growth.df$SCHOOL_YEAR), 
                      sum)
growth.N.state <- cbind(SCHOOL_ID = rep(state.school.id, nrow(growth.N.state)), growth.N.state)

growth.N <- rbind(growth.N, growth.N.state)

growth <- merge (growth, growth.N)
growth$PERCENT_PROFICIENT_PRIOR <- round(growth$PROFICIENT_PRIOR/growth$N_SGP * 100, 1)

head(growth)
tail(growth)

growth.students <- with(growth.df, growth.df[, c("SCHOOL_YEAR", "SCHOOL_ID", "WISER_ID")])

growth.students.school <- cast(growth.students, SCHOOL_YEAR+SCHOOL_ID~., function (x) length(unique(x)))

#get the state in there
growth.students.school.state <- cast(growth.students, SCHOOL_YEAR~., value="WISER_ID", function (x) length(unique(x)))
growth.students.school.state <- cbind(SCHOOL_ID=(rep(state.school.id, nrow(growth.students.school.state))), growth.students.school.state)

growth.students.school <- rbind(growth.students.school, growth.students.school.state)

names(growth.students.school)[3] <- "GROWTH_ACCOUNTABILITY_N"
head(with(growth.students.school, growth.students.school[SCHOOL_YEAR==current.school.year,]))
tail(with(growth.students.school, growth.students.school[SCHOOL_YEAR==current.school.year,]))

growth <- merge (growth, growth.students.school)

head(with(growth, growth[SCHOOL_YEAR==current.school.year,]))
tail(with(growth, growth[SCHOOL_YEAR==current.school.year,]))


##assign growth SPL
#drop growth columns
schools <- calc.school.growth(schools)

#look at distribution of computed target levels
table(schools[schools$SCHOOL_YEAR==current.school.year,]$GROWTH_TARGET_LEVEL)



# growth.subject.N <- aggregate(data.frame(N=rep(1, nrow(growth.df))), 
#                               by=list(SCHOOL_YEAR=growth.df$SCHOOL_YEAR, 
#                                       SCHOOL_ID=growth.df$SCHOOL_ID,
#                                       SUBJECT_CODE=growth.df$SUBJECT_CODE), 
#                               sum)
# 
# growth.Accountability.N <- aggregate(data.frame(OLD_GROWTH_ACCOUNTABILITY_N=growth.subject.N$N), 
#                                      by=list(SCHOOL_YEAR=growth.subject.N$SCHOOL_YEAR, 
#                                              SCHOOL_ID=growth.subject.N$SCHOOL_ID),
#                                      max)
# 
# growth <- merge (growth, growth.Accountability.N)
# 
# head(with(growth, growth[SCHOOL_YEAR==current.school.year,]))
# 
# with(growth, growth[OLD_GROWTH_ACCOUNTABILITY_N != GROWTH_ACCOUNTABILITY_N,])
###


# Sturges <- function (sample) {
#   ceiling(log(length(sample),2)) + 1 
# }
# 
# Scott <- function (sample) {
#   h<-3.5 * sqrt(var(sample))/length(sample)^(1/3)  
#   ceiling((max(sample) - min(sample))/h)
# }



#creates a folder called "plots" if it doesn't already exist
# if (!file.exists("plots"))
#   dir.create("plots")

#create histgram pdfs for 2010-11 and 2011-12 MGPs in plots folder
# sapply(c("2010-11", "2011-12"), function (year) {
#   k<-Scott(growth[growth$SCHOOL_YEAR=='2011-12' & growth$N > 5,"MGP"])
#   #k<-Sturges(growth[growth$SCHOOL_YEAR=='2011-12' & growth$N > 5,"MGP"])
#   pdf(file=paste("plots/MGP", year, "hist.pdf", sep="-"))
#   hist(growth[growth$SCHOOL_YEAR==year & growth$N > 5 ,"MGP"], 
#        n=k, 
#        xlab="MGP", 
#        main=paste("Histogram of", year, "MGPs for schools with N > 5", sep=" "))
#   dev.off()
# })

#35th and 65th percentiles for 2012-12 MGPs for schools with N > 5
# quantile(growth[growth$SCHOOL_YEAR=='2011-12' & growth$N > 5,]$MGP, c(.35, .65))
# 
# quantile(growth[growth$SCHOOL_YEAR=='2011-12' & growth$N > 5,]$MGP, probs=c(.01,.1,.25,.5,.75,.9,.99), type=6)
