calc.participation.rate <- function (df, subject.labels=c("MATH", "READING", "SCIENCE", "ENG_WRITING"),                                      
                                     status.prefix='TESTING_STATUS_CODE', 
                                     status.prefix.sep="_",
                                     status.codes=c(exempt='X', participated='T', did.not.participate='F'),
                                     total.participation.labels = c("ACHIEVEMENT_TESTED_HS", "ACHIEVEMENT_PARTICIPANTS_HS", "PARTICIPATION_RATE_ACHIEVEMENT_HS"),
                                     precision=1)
{  
  status.labels <- unlist(lapply(subject.labels, function (l) paste(status.prefix, l, sep=status.prefix.sep)))
  tested.labels <- unlist(lapply(subject.labels, function (l) paste(l, "TESTED", sep="_")))
  participant.labels <- unlist(lapply(subject.labels, function (l) paste(l, "PARTICIPANTS", sep="_")))
  participation.rate.labels <- unlist(lapply(subject.labels, function (l) paste(l, "PARTICIPATION_RATE", sep="_")))
  
  participants.df <- do.call(cbind, lapply(seq(1,length(subject.labels)),
                                           function (i) {
                                             participants <- data.frame(ifelse(df[status.labels[i]] == status.codes["participated"], 1, 0),
                                                                        ifelse(df[status.labels[i]] == status.codes["exempt"], 0, 1))
                                             names(participants) <- c(tested.labels[i],
                                                                      participant.labels[i])
                                             participants
                                             
                                           }
  ))
  
  participation.df <- aggregate(participants.df,
                                by = list(SCHOOL_YEAR=df$SCHOOL_YEAR,
                                          SCHOOL_ID=df$SCHOOL_ID),
                                sum)
  
  #do state participation 
  participation.cols <- unlist(lapply(seq(1,length(subject.labels)), function(i) c(tested.labels[i], participant.labels[i])))
  participation.state.df <- aggregate(participation.df[,participation.cols],
                                      by=list(SCHOOL_YEAR=participation.df$SCHOOL_YEAR), 
                                      sum)
  
  
  participation.state.df <- cbind(SCHOOL_ID=rep(state.school.id, nrow(participation.state.df)), participation.state.df)
  
  participation.df <- rbind(participation.df, participation.state.df[,c(names(participation.df))])
  
  #tail(participation.df)
  
  

  #calulate and assigne particpation rate columns
  lapply(seq(1,length(subject.labels)),
         function (i) {
         participation.df[participation.rate.labels[i]] <<- round((participation.df[tested.labels[i]] / participation.df[participant.labels[i]]) * 100, precision)                                   
       })
  



  participation.df <- cbind(participation.df, t(apply(participation.df[,c(tested.labels, participant.labels)],
                                                       c(1),
                                                       function (school) {
                                                         total.tested <- sum(school[tested.labels])
                                                         total.participants <- sum(school[participant.labels])
                                                         participation.rate.total <- round((total.tested / total.participants) * 100, precision)
                                                         
                                                         result <- c(total.tested, total.participants, participation.rate.total)
                                                         names(result) <- total.participation.labels
                                                         result
                                                         
                                                       })))

}