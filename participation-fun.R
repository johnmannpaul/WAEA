calc.participation.rate <- function (df, subject.labels=c("MATH", "READING", "SCIENCE", "ENG_WRITING"),                                      
                                     status.prefix='TESTING_STATUS_CODE', 
                                     status.prefix.sep="_",
                                     status.codes=c(exempt='X', participated='T', did.not.participate='N'),
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
  
  
  set.participation <- function (i) {
    tested <- participation.df[[tested.labels[i]]]
    participants <- participation.df[[participant.labels[i]]]
    
    participation.df[participation.rate.labels[i]] <<- ifelse(participants == 0, NA, round((tested / participants) * 100, precision))                                   
  }

  #calulate and assign particpation rate columns
  lapply(seq(1,length(subject.labels)),
         set.participation)
  



  participation.df <- cbind(participation.df, t(apply(participation.df[,c(tested.labels, participant.labels)],
                                                       c(1),
                                                       function (school) {
                                                         total.tested <- sum(school[tested.labels])
                                                         total.participants <- sum(school[participant.labels])
                                                         participation.rate.total <- ifelse(total.participants == 0, 
                                                                                            NA,
                                                                                            round((total.tested / total.participants) * 100, precision))
                                                         
                                                         result <- c(total.tested, total.participants, participation.rate.total)
                                                         names(result) <- total.participation.labels
                                                         result
                                                         
                                                       })))
  participation.df

}



calc.participation.rate.long <- function (df,                                           
                                          status.prefix='TESTING_STATUS_CODE',                                           
                                          status.codes=c(exempt='X', participated='T', did.not.participate='N'),                              
                                          total.participation.labels=c("TESTED", "PARTICIPANTS", "REQUIRED_TESTED_90",
                                                                       "REQUIRED_TESTED_95", "PARTICIPATION_LEVEL_MET",
                                                                       "PARTICIPATION_RATE"),
                                          precision=1,
                                          participation.rates = c(0.9,0.95))
{  
  
  participation.cols <- c("TESTED","PARTICIPANTS")
  
  encode.status <- function (row) {
    status <- row[[status.prefix]]
    participants <- c(ifelse(status == status.codes["participated"], 1, 0),
                               ifelse(status == status.codes["exempt"], 0, 1))
    names(participants) <- participation.cols
    participants
    
  }
  
  participants.df <- data.frame(t(apply(df[status.prefix],
                                        c(1),
                                        encode.status)))
  
  participation.df.subject <- aggregate(participants.df,
                                by = list(SCHOOL_YEAR=df$SCHOOL_YEAR,
                                          SCHOOL_ID=df$SCHOOL_ID,
                                          SUBJECT_CODE=df$SUBJECT_CODE),
                                sum)
  
  #do state participation   
  participation.df.subject.state <- aggregate(participation.df.subject[,participation.cols],
                                      by=list(SCHOOL_YEAR=participation.df.subject$SCHOOL_YEAR,
                                              SUBJECT_CODE=participation.df.subject$SUBJECT_CODE), 
                                      sum)
  
  
  participation.df.subject.state <- cbind(SCHOOL_ID=rep(state.school.id, nrow(participation.df.subject.state)), participation.df.subject.state)
  
  participation.df.subject <- rbind(participation.df.subject, participation.df.subject.state[,c(names(participation.df.subject))])
  
  
  required.participants.labels <- sapply(participation.rates,
                                         function (r) paste("REQUIRED_TESTED", 100*r, sep="_"))
  
  participation.df.subject[required.participants.labels] <- do.call(cbind, lapply(participation.rates, 
                                                                                    function (rate) {
                                                                                      floor(rate * participation.df.subject$PARTICIPANTS)
                                                                                    }))  
  
  participation.df <- aggregate(participation.df.subject[c("TESTED", "PARTICIPANTS",
                                                           "REQUIRED_TESTED_90",
                                                           "REQUIRED_TESTED_95")],
                                        by = list(SCHOOL_YEAR=participation.df.subject$SCHOOL_YEAR,
                                                  SCHOOL_ID=participation.df.subject$SCHOOL_ID),
                                        sum)
  
  
  
  
  ##
  participation.df["PARTICIPATION_LEVEL_MET"] <- apply(participation.df[c("PARTICIPANTS", "TESTED",
                                                                          required.participants.labels)],
                                                       c(1),
                                                       function (tested) {
                                                         if (tested[1] == 0) 
                                                           NA
                                                         else
                                                           findInterval(tested[2],
                                                                        tested[3:length(tested)])+1
                                                         })
                                                        
  #tail(participation.df)
  
  
    
  participation.df$PARTICIPATION_RATE <- ifelse(participation.df$PARTICIPANTS == 0, 
                                                   NA, 
                                                   round((participation.df$TESTED / participation.df$PARTICIPANTS) * 100, 
                                                         precision))                                   
  
   
  
  names(participation.df)[3:ncol(participation.df)] <- total.participation.labels
  
  
  participation.df
  
}