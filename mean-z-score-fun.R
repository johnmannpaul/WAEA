calc.mean.score <- function (df, subject.labels=c(MATH="MATH", READING="READING", SCIENCE="SCIENCE", ENG_WRITING="ENG_WRITING"),
                             testing.status.prefix="TESTING_STATUS_CODE",
                             score.prefix="ACCOUNTABILITY_Z_SCORE",
                             prefix.sep="_",
                             agg.function=function (g) round(100*mean(g),0)) 
{
  
  testing.status.labels <- unlist(lapply(subject.labels, function (l) paste(testing.status.prefix, l, sep=prefix.sep)))
  score.labels <- unlist(lapply(subject.labels, function (l) paste(score.prefix, l, sep=prefix.sep)))
  
  df.long <- reshape(df,
                     varying=list(testing.status.labels,
                                    score.labels),
                     v.names=c(testing.status.prefix, score.prefix),
                     timevar = "SUBJECT",
                     times = subject.labels,
                     direction="long")
  
  
  #should be empty
  #with(act.fay.long, act.fay.long[TESTING_STATUS_CODE == 'T' & is.na(Z_SCORE),])
  #with(act.fay.long, act.fay.long[TESTING_STATUS_CODE == 'N' & !is.na(Z_SCORE),])
  #with(act.fay.long, act.fay.long[TESTING_STATUS_CODE == 'X' & !is.na(Z_SCORE),])
  
  #table(act.fay.long$TESTING_STATUS_CODE)
  
  
  df.long.testers <- df.long[df.long$TESTING_STATUS_CODE == 'T',]
  
  #convert score column to numeric if it's not already
  df.long.testers[[score.prefix]] <- as.numeric(df.long.testers[[score.prefix]])
  
  mean.score.df <- aggregate(data.frame(MEAN_SCORE=df.long.testers[score.prefix]),
                              by=list(SCHOOL_YEAR=df.long.testers$SCHOOL_YEAR,
                                      SCHOOL_ID=df.long.testers$SCHOOL_ID),
                              agg.function)
  
  N.df <- aggregate(data.frame(N=df.long.testers$WISER_ID),
                                by=list(SCHOOL_YEAR=df.long.testers$SCHOOL_YEAR,
                                        SCHOOL_ID=df.long.testers$SCHOOL_ID),
                                function(rows) length(unique(rows)))
  
  mean.scores <- merge(mean.score.df, N.df)
  
  #compute state value
  mean.score.df.state <- aggregate(data.frame(MEAN_SCORE=df.long.testers[score.prefix]),
                                    by=list(SCHOOL_YEAR=df.long.testers$SCHOOL_YEAR),
                                   agg.function)
  
  N.df.state <- aggregate(data.frame(N=df.long.testers$WISER_ID),
                                      by=list(SCHOOL_YEAR=df.long.testers$SCHOOL_YEAR),
                                      function(rows) length(unique(rows)))
  
  mean.scores.state <- merge(mean.score.df.state, N.df.state)
  
  mean.scores.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(mean.scores.state)), mean.scores.state)
  
  mean.scores.all <- rbind(mean.scores, 
                          mean.scores.state[,c("SCHOOL_YEAR", "SCHOOL_ID", 
                                                  names(mean.scores[-1:-2]))])
  
  mean.scores.all
  
  ##compute state values.  Then replace code in achievement-act and use in high-school-equity
}