plot.hist <- function (df, bins) {
  h<-hist(df,bins)
  xfit<-seq(min(df),max(df),length.out=40) 
  yfit<-dnorm(xfit,mean=mean(df),sd=sd(df)) 
  yfit <- yfit*diff(h$mids[1:2])*length(df) 
  lines(xfit, yfit, col="blue", lwd=2)
  h
}
