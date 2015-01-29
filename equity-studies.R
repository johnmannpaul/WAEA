unmatrixfy.df <- function (df, sep="_", prepend=TRUE) {
  
  do.call(cbind, lapply(names(df),
                        function (n) {
                          
                          obj <- df[[n]]
                          
                          if (class(obj) != "matrix")
                            df[n]
                          else{
                            col.names <- dimnames(obj)[[2]]
                            result <- do.call(data.frame, 
                                              sapply(col.names,
                                                     function (n) obj[,n],
                                                     simplify=FALSE))
                            names(result) <- sapply(col.names, function (x) {
                              if(prepend)
                                paste(n, x, sep=sep)
                              else
                                x})
                            
                            result
                          }
                          
                          
                        }))
  
}


below.proficient.priors <- growth.df[growth.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2),]
names(below.proficient.priors)
table(below.proficient.priors[c("SCHOOL_YEAR","SUBJECT_CODE")])
table(growth.df[c("SCHOOL_YEAR","SUBJECT_CODE")])

subgroup.students <- unique(below.proficient.priors[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])


#scores for all the students in the subgroup
consolidated.subgroup.df <- below.proficient.priors

#Under this strategy we do not use multiyear records to compute equity.  We should have applied this rule.  But we didn't.
#consolidated.subgroup.df <- with(consolidated.subgroup.df, consolidated.subgroup.df[SCHOOL_YEAR == SCHOOL_YEAR_ORIGINAL,])

#growth.df[growth.df$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2),]

met.agp <- ifelse(consolidated.subgroup.df$MET_AGP == 'T', 1, 0)

equity <- unmatrixfy.df(aggregate(data.frame(PERCENT_MEETING_AGP=met.agp), 
                                  by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                                          SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID),
                                  
                                  function (g) round((sum(g) / length(g)) * 100, precision)), prepend=FALSE)







subgroup.N <- aggregate(data.frame(N_SUBGROUP=rep(1,nrow(subgroup.students))),
                        by=list(SCHOOL_YEAR=subgroup.students$SCHOOL_YEAR, 
                                SCHOOL_ID=subgroup.students$SCHOOL_ID), 
                        sum)




equity <- merge(equity, subgroup.N)

with(equity, head(equity[SCHOOL_YEAR=='2012-13',]))
with(equity, tail(equity[SCHOOL_YEAR=='2012-13',]))

##equity analysis - can skip
xy <- merge(equity[equity$SCHOOL_YEAR=='2011-12', c("SCHOOL_ID", "PERCENT_MEETING_AGP"),],
            equity[equity$SCHOOL_YEAR=='2012-13', c("SCHOOL_ID", "PERCENT_MEETING_AGP"),], by=c("SCHOOL_ID"))
cor(xy$PERCENT_MEETING_AGP.x, xy$PERCENT_MEETING_AGP.y)

write.csv(equity[equity$N_SUBGROUP >= 10,], file="results/met-agp-percentages-for-nonproficient-priors.csv", row.names=FALSE, na="")


##2013-14 equity method applied only to nonproficient priors


sample.stats <- unmatrixfy.df(aggregate(paws$STANDARD_PAWS_SCALE_SCORE,
                                        by=list(SCHOOL_YEAR=paws$SCHOOL_YEAR,
                                                SUBJECT=paws$SUBJECT_CODE,
                                                GRADE_ENROLLED=paws$GRADE_ENROLLED),
                                        function (g) c(MEAN=mean(as.numeric(g), na.rm=TRUE),
                                                       SD=sd(as.numeric(g), na.rm=TRUE))),prepend=FALSE)

paws$STD_SCORE <- apply(paws[,c("SCHOOL_YEAR", "SUBJECT_CODE", "GRADE_ENROLLED", "STANDARD_PAWS_SCALE_SCORE")],
                        c(1),
                        function (score) {
                          mean <- sample.stats[sample.stats$SCHOOL_YEAR == score[["SCHOOL_YEAR"]] &
                                                 sample.stats$SUBJECT == score[["SUBJECT_CODE"]] &
                                                 sample.stats$GRADE_ENROLLED == score[["GRADE_ENROLLED"]], "MEAN"]
                          
                          sd <- sample.stats[sample.stats$SCHOOL_YEAR == score[["SCHOOL_YEAR"]] &
                                               sample.stats$SUBJECT == score[["SUBJECT_CODE"]] &
                                               sample.stats$GRADE_ENROLLED == score[["GRADE_ENROLLED"]], "SD"]
                          
                          z_score <- round((as.numeric(score[["STANDARD_PAWS_SCALE_SCORE"]]) - mean)/sd, 2)
                          
                          z_score * 30 + 150
                          
                        })

unmatrixfy.df(aggregate(paws$STD_SCORE,
                        by=list(SCHOOL_YEAR=paws$SCHOOL_YEAR,
                                SUBJECT=paws$SUBJECT_CODE,
                                GRADE_ENROLLED=paws$GRADE_ENROLLED),
                        function (g) c(MEAN=mean(as.numeric(g), na.rm=TRUE),
                                       SD=sd(as.numeric(g), na.rm=TRUE))),prepend=FALSE)


below.proficient.priors <- paws[paws$SCHOOL_FULL_ACADEMIC_YEAR == 'T' &
                                  paws$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2) &
                                  paws$SUBJECT_CODE %in% c('RE','MA') &
                                  paws$TESTING_STATUS_CODE == 'T' & 
                                  paws$TEST_TYPE %in% c("PAWS-STANDARD", "PAWS Standard"),]

table(below.proficient.priors[c("SCHOOL_YEAR","SUBJECT_CODE")])

subgroup.students <- unique(below.proficient.priors[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])


#scores for all the students in the subgroup
consolidated.subgroup.df <- below.proficient.priors



equity <- unmatrixfy.df(aggregate(data.frame(STD_SCORE=consolidated.subgroup.df$STD_SCORE), 
                                  by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                                          SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID),
                                  function (g) c(MEAN_STD_SCORE=round(mean(as.numeric(g)),1),
                                                 N_TESTS=length(g))),prepend=FALSE)


equity <- merge(equity, subgroup.N)

with(equity, head(equity[SCHOOL_YEAR=='2012-13',]))
with(equity, tail(equity[SCHOOL_YEAR=='2012-13',]))

##equity analysis - can skip
xy <- merge(equity[equity$SCHOOL_YEAR=='2011-12', c("SCHOOL_ID", "MEAN_STD_SCORE"),],
            equity[equity$SCHOOL_YEAR=='2012-13', c("SCHOOL_ID", "MEAN_STD_SCORE"),], by=c("SCHOOL_ID"))
cor(xy$MEAN_STD_SCORE.x, xy$MEAN_STD_SCORE.y)

write.csv(equity[equity$N_SUBGROUP >= 10,], file="results/mean-standard-scores-for-nonproficient-priors.csv", row.names=FALSE, na="")

equity.median.agp <- aggregate(data.frame(MEDIAN_AGP=as.numeric(consolidated.subgroup.df$AGP)), 
                               by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                                       SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID),
                               median)
equity.median.agp <- merge(equity.median.agp, subgroup.N)
with(equity.median.agp, head(equity.median.agp[SCHOOL_YEAR=='2012-13',]))

write.csv(equity.median.agp[equity.median.agp$N_SUBGROUP >= 10,], file="results/median-agp-for-nonproficient-priors.csv", row.names=FALSE, na="")

##2013-14 equity method applied to both tests

consolidated.subgroup.df <- merge(subgroup.students,paws[paws$SCHOOL_FULL_ACADEMIC_YEAR == 'T' &
                                                           paws$SUBJECT_CODE %in% c('RE','MA') &
                                                           paws$TESTING_STATUS_CODE == 'T' & 
                                                           paws$TEST_TYPE %in% c("PAWS-STANDARD", "PAWS Standard"),])

table(consolidated.subgroup.df[c("SCHOOL_YEAR", "SUBJECT_CODE")])



equity <- unmatrixfy.df(aggregate(data.frame(STD_SCORE=consolidated.subgroup.df$STD_SCORE), 
                                  by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                                          SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID),
                                  function (g) c(MEAN_STD_SCORE=round(mean(as.numeric(g)),1),
                                                 N_TESTS=length(g))),prepend=FALSE)


equity <- merge(equity, subgroup.N)

with(equity, head(equity[SCHOOL_YEAR=='2012-13',]))
with(equity, tail(equity[SCHOOL_YEAR=='2012-13',]))

##equity analysis - can skip
xy <- merge(equity[equity$SCHOOL_YEAR=='2011-12', c("SCHOOL_ID", "MEAN_STD_SCORE"),],
            equity[equity$SCHOOL_YEAR=='2012-13', c("SCHOOL_ID", "MEAN_STD_SCORE"),], by=c("SCHOOL_ID"))
cor(xy$MEAN_STD_SCORE.x, xy$MEAN_STD_SCORE.y)

write.csv(equity[equity$N_SUBGROUP >= 10,], file="results/mean-standard-scores-for-historic-subgroup.csv", row.names=FALSE, na="")


equity.median.agp <- aggregate(data.frame(MEDIAN_AGP=as.numeric(consolidated.subgroup.df$AGP)), 
                               by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                                       SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID),
                               median, na.rm=TRUE)
equity.median.agp <- merge(equity.median.agp, subgroup.N)
with(equity.median.agp, head(equity.median.agp[SCHOOL_YEAR=='2012-13',]))

write.csv(equity.median.agp[equity.median.agp$N_SUBGROUP >= 10,], file="results/median-agp-for-historic-subgroup.csv", row.names=FALSE, na="")



####MGP-MAGP for below proficienct priors

consolidated.subgroup.df <- below.proficient.priors



equity <- unmatrixfy.df(aggregate(consolidated.subgroup.df[c("SGP","AGP")], 
                                  by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                                          SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID),
                                  function (g) c(MEDIAN=round(median(as.numeric(g)),1),
                                                 N_TESTS=length(g))),prepend=TRUE)

equity <- data.frame(equity[c("SCHOOL_YEAR", "SCHOOL_ID")],
                     MGP=equity$SGP_MEDIAN,
                     MAGP=equity$AGP_MEDIAN,
                     DIFF_MGP_MAGP=equity$SGP_MEDIAN - equity$AGP_MEDIAN)

equity <- merge(equity, subgroup.N)

with(equity, head(equity[SCHOOL_YEAR=='2012-13',]))
with(equity, tail(equity[SCHOOL_YEAR=='2012-13',]))

##equity analysis - can skip
xy <- merge(equity[equity$SCHOOL_YEAR=='2011-12', c("SCHOOL_ID", "MEAN_STD_SCORE"),],
            equity[equity$SCHOOL_YEAR=='2012-13', c("SCHOOL_ID", "MEAN_STD_SCORE"),], by=c("SCHOOL_ID"))
cor(xy$MEAN_STD_SCORE.x, xy$MEAN_STD_SCORE.y)

equity<- equity[equity$N_SUBGROUP >= 10,]
write.csv(equity, file="results/difference-of-MGP-and-AGP-for-nonproficient-priors.csv", row.names=FALSE, na="")

cuts.diff <- round(quantile(equity[equity$SCHOOL_YEAR =='2010-11', "DIFF_MGP_MAGP"], c(.3,.7)))

equity$CAT <- findInterval(equity$DIFF_MGP_MAGP, cuts.diff) + 1
table(equity[c("SCHOOL_YEAR","CAT")])


equity.mgp <- equity
cuts.mgp <- c(45,60)
equity.mgp$CAT <- findInterval(equity.mgp$MGP, cuts.mgp) + 1
table(equity.mgp[c("SCHOOL_YEAR","CAT")])

equity.cond.mgp <- equity[c("SCHOOL_YEAR", "SCHOOL_ID", "MGP", "MAGP")]
cuts1.cond.mgp  <- c(40,55)
cuts2.cond.mgp  <- c(45,60)
equity.cond.mgp$CAT <- ifelse(equity.cond.mgp$MGP >= equity.cond.mgp$MAGP,
                              findInterval(equity.cond.mgp$MGP, cuts1.cond.mgp) + 1,
                              findInterval(equity.cond.mgp$MGP, cuts2.cond.mgp) + 1)
table(equity.cond.mgp[c("SCHOOL_YEAR","CAT")])
prop.table(table(equity.cond.mgp[c("SCHOOL_YEAR","CAT")]),1)

head(equity.cond.mgp)
write.csv(equity.cond.mgp, file="results/conditional-MGP-cuts-for-nonproficient-priors.csv", row.names=FALSE, na="")


#### Achievement Gaps study

consolidated.subgroup.df <- paws[paws$SCHOOL_FULL_ACADEMIC_YEAR == 'T' &
                                   paws$ACHIEVEMENT_LEVEL_PRIOR %in% c(1,2) &
                                   paws$SUBJECT_CODE %in% c('RE','MA') &
                                   paws$TESTING_STATUS_CODE == 'T' & 
                                   paws$TEST_TYPE %in% c("PAWS-STANDARD", "PAWS Standard"),]


non.consolidated.subgroup.df <- paws[paws$SCHOOL_FULL_ACADEMIC_YEAR == 'T' &
                                       paws$ACHIEVEMENT_LEVEL_PRIOR %in% c(3,4) &
                                       paws$SUBJECT_CODE %in% c('RE','MA') &
                                       paws$TESTING_STATUS_CODE == 'T' & 
                                       paws$TEST_TYPE %in% c("PAWS-STANDARD", "PAWS Standard"),]

subgroup.students <- unique(consolidated.subgroup.df[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])

subgroup.N <- aggregate(data.frame(N_SUBGROUP=rep(1,nrow(subgroup.students))),
                        by=list(SCHOOL_YEAR=subgroup.students$SCHOOL_YEAR, 
                                SCHOOL_ID=subgroup.students$SCHOOL_ID), 
                        sum)

non.subgroup.students <- unique(non.consolidated.subgroup.df[, c("SCHOOL_YEAR", "WISER_ID", "SCHOOL_ID")])

non.subgroup.N <- aggregate(data.frame(N_SUBGROUP=rep(1,nrow(non.subgroup.students))),
                            by=list(SCHOOL_YEAR=non.subgroup.students$SCHOOL_YEAR, 
                                    SCHOOL_ID=non.subgroup.students$SCHOOL_ID), 
                            sum)


mean.std.score.ncsg <- unmatrixfy.df(aggregate(data.frame(STD_SCORE=non.consolidated.subgroup.df$STD_SCORE), 
                                               by=list(SCHOOL_YEAR=non.consolidated.subgroup.df$SCHOOL_YEAR, 
                                                       SCHOOL_ID=non.consolidated.subgroup.df$SCHOOL_ID),
                                               function (g) c(MEAN_STD_SCORE=round(mean(as.numeric(g)),1),
                                                              N_TESTS=length(g))),prepend=FALSE)


mean.std.score.ncsg <- merge(mean.std.score.ncsg, non.subgroup.N)

mean.std.score.ncsg <- mean.std.score.ncsg[mean.std.score.ncsg$N_SUBGROUP >= 10,] 

head(mean.std.score.ncsg[mean.std.score.ncsg$SCHOOL_YEAR=='2012-13',])



mean.std.score.csg <- unmatrixfy.df(aggregate(data.frame(STD_SCORE=consolidated.subgroup.df$STD_SCORE), 
                                              by=list(SCHOOL_YEAR=consolidated.subgroup.df$SCHOOL_YEAR, 
                                                      SCHOOL_ID=consolidated.subgroup.df$SCHOOL_ID),
                                              function (g) c(MEAN_STD_SCORE=round(mean(as.numeric(g)),1),
                                                             N_TESTS=length(g))),prepend=FALSE)


mean.std.score.csg <- merge(mean.std.score.csg, subgroup.N)

mean.std.score.csg <- mean.std.score.csg[mean.std.score.csg$N_SUBGROUP >= 10,] 

head(mean.std.score.csg[mean.std.score.csg$SCHOOL_YEAR=='2012-13',])

csg.schools <- unique(mean.std.score.csg[c("SCHOOL_YEAR", "SCHOOL_ID")])
ncsg.schools <- unique(mean.std.score.ncsg[c("SCHOOL_YEAR", "SCHOOL_ID")])

csg.gap.schools <- merge(csg.schools, ncsg.schools)

nrow(mean.std.score.csg)
mean.std.score.csg <- merge(mean.std.score.csg, csg.gap.schools)
nrow(mean.std.score.csg)

nrow(mean.std.score.ncsg)
mean.std.score.ncsg <- merge(mean.std.score.ncsg, csg.gap.schools)
nrow(mean.std.score.ncsg)

quantile(mean.std.score.ncsg[mean.std.score.ncsg$SCHOOL_YEAR =='2010-11', "MEAN_STD_SCORE"], c(.1,.2,.3,.5,.7))

mean.std.score.ncsg[mean.std.score.ncsg$MEAN_STD_SCORE<150 & mean.std.score.ncsg$SCHOOL_YEAR =='2010-11',]

equity.gaps <- merge(mean.std.score.ncsg, mean.std.score.csg, by=c("SCHOOL_YEAR", "SCHOOL_ID"))

equity.gaps$GAP <- equity.gaps$MEAN_STD_SCORE.y - equity.gaps$MEAN_STD_SCORE.x
head(equity.gaps) 

cuts.gaps <- round(quantile(equity.gaps[equity.gaps$SCHOOL_YEAR =='2010-11', "GAP"], c(.3,.7)))
equity.gaps$CAT <- findInterval(equity.gaps$GAP, cuts.gaps) + 1

table(equity.gaps[c("SCHOOL_YEAR","CAT")])

equity.gaps.wide <- reshape(equity.gaps[c("SCHOOL_YEAR", "SCHOOL_ID", "GAP")],
                            v.names=c("GAP"),
                            timevar="SCHOOL_YEAR",
                            idvar="SCHOOL_ID",
                            direction="wide")


equity.gaps.wide$DIFF.2011_12 <- equity.gaps.wide$`GAP.2011-12` - equity.gaps.wide$`GAP.2010-11`

equity.gaps.wide$DIFF.2012_13 <- equity.gaps.wide$`GAP.2012-13` - equity.gaps.wide$`GAP.2011-12`

head(equity.gaps.wide)


equity.gaps.long <- reshape(equity.gaps.wide[c("SCHOOL_ID", "DIFF.2011_12", "DIFF.2012_13")],
                            varying=c("DIFF.2011_12", "DIFF.2012_13"),
                            idvar="SCHOOL_ID",
                            direction="long")

names(equity.gaps.long) <- c("SCHOOL_ID",    "SCHOOL_YEAR",  "DIFF_GAP")

nrow(equity.gaps.long)
equity.gaps.long <- equity.gaps.long[!is.na(equity.gaps.long$DIFF_GAP),]
nrow(equity.gaps.long)

head(equity.gaps.long)
cuts.gaps.diff <- round(quantile(equity.gaps.long[equity.gaps.long$SCHOOL_YEAR =='2011_12', "DIFF_GAP"], c(.3,.7)))
equity.gaps.long$CAT <- findInterval(equity.gaps.long$DIFF_GAP, cuts.gaps.diff) + 1

table(equity.gaps.long[c("SCHOOL_YEAR","CAT")])
head(equity.gaps.wide)
head(equity.gaps.long)

write.csv(equity.gaps.wide, 
          file="results/gaps-for-non-proficient-priors-wide.csv", row.names=FALSE, na="")
write.csv(equity.gaps.long, 
          file="results/gap-differences-for-non-proficient-priors-long.csv", row.names=FALSE, na="")

### end study

cuts <- quantile(equity[equity$SCHOOL_YEAR =='2010-11', "MEAN_STD_SCORE"], c(.3,.7))
equity$CAT <- findInterval(equity$MEAN_STD_SCORE, cuts) + 1
table(equity[c("SCHOOL_YEAR","CAT")])

equity.agp <- read.csv(file="results/met-agp-percentages-for-nonproficient-priors.csv")
cuts.agp <- quantile(equity.agp[equity.agp$SCHOOL_YEAR =='2010-11', "PERCENT_MEETING_AGP"], c(.3,.7))
equity.agp$CAT <- findInterval(equity.agp$PERCENT_MEETING_AGP, cuts.agp) + 1
table(equity.agp[c("SCHOOL_YEAR","CAT")])

equity.zscore <- read.csv(file="results/mean-standard-scores-for-nonproficient-priors.csv")
cuts.zscore <- quantile(equity.zscore[equity.zscore$SCHOOL_YEAR =='2010-11', "MEAN_STD_SCORE"], c(.3,.7))
equity.zscore$CAT <- findInterval(equity.zscore$MEAN_STD_SCORE, cuts.zscore) + 1
table(equity.zscore[c("SCHOOL_YEAR","CAT")])



equity.magp <- read.csv(file="results/median-agp-for-nonproficient-priors.csv")
cuts.magp <- quantile(equity.magp[equity.magp$SCHOOL_YEAR =='2010-11', "MEDIAN_AGP"], c(.3,.7))
equity.magp$CAT <- findInterval(equity.magp$MEDIAN_AGP, cuts.magp) + 1
table(equity.magp[c("SCHOOL_YEAR","CAT")])


equity.magp.hist <- read.csv(file="results/median-agp-for-historic-subgroup.csv")
cuts.magp.hist <- quantile(equity.magp.hist[equity.magp.hist$SCHOOL_YEAR =='2010-11', "MEDIAN_AGP"], c(.3,.7))
equity.magp.hist$CAT <- findInterval(equity.magp.hist$MEDIAN_AGP, cuts.magp.hist) + 1
table(equity.magp.hist[c("SCHOOL_YEAR","CAT")])


