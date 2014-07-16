calc.z.scores <- function (df, subject.labels=c(MATH="MATH", READING="READING", SCIENCE="SCIENCE", ENG_WRITING="ENG_WRITING"),                                      
                           scale.score.prefix='ACT_SCALE_SCORE', 
                           z.score.prefix='ACT_Z_SCORE', 
                           prefix.sep="_",
                           baseline.stats)
{
  scale.score.labels <- unlist(lapply(subject.labels, function (l) paste(scale.score.prefix, l, sep=prefix.sep)))
  z.score.labels <- unlist(lapply(subject.labels, function (l) paste(z.score.prefix, l, sep=prefix.sep)))
  
  calc.z.scores.student <- function (row) {
    
    calc.z.score <- function (i) {      
      subject = names(row)[i]
      score = as.numeric(row[i])
      
      mean = baseline.stats[baseline.stats$SUBJECT==subject,"MEAN"]
      sd = baseline.stats[baseline.stats$SUBJECT==subject,"SD"]
      (score-mean)/sd                
    }
    
    #relabel the row names so we can use as a lookup into appropriate baseline statistic
    names(row) <- baseline.stats[baseline.stats$SUBJECT %in% names(subject.labels) ,"SUBJECT"]
    z.scores <- unlist(lapply(1:length(row), calc.z.score))
    
    
    names(z.scores) <- z.score.labels
    z.scores
  }
  
  
  data.frame(t(apply(df[,scale.score.labels],
                     c(1),
                     calc.z.scores.student)))
    
}