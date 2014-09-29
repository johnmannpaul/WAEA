# Frequency Distributions

dist=function(object,round){
  print(table(round(object,round)))
  print(table(round(object,round))/length(object))
  print(cumsum(table(round(object,round))/length(object)))
}

#_____________________________________________________________________________________________________________

#4-Year Graduation

Grad4_hist=function(curve1,curve2){

hist1=hist(round(grad_alt$GRAD_RATE_4_YR.2012.13,0),freq=T,xaxt='n',breaks=15,
  main="Distribution Of 4-Year Graduation Rates \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Graduation Rate",ylab="Number of Schools",col="red",ylim=c(0,16))
  axis(side=1,at=seq(0,1,.2),labels=seq(0,100,20))

hist2 = hist(round(grad_nonalt$GRAD_RATE_4_YR.2012.13,0),freq=T,xaxt='n',breaks=35,col=rgb(0,1,0,.75),add=TRUE)
 
  xfit<-seq(min(round(grad_nonalt$GRAD_RATE_4_YR.2012.13,0)),max(round(grad_nonalt$GRAD_RATE_4_YR.2012.13,0)),length=100) 
  yfit<-dnorm(xfit,mean=mean(round(grad_nonalt$GRAD_RATE_4_YR.2012.13,0)),sd=sd(round(grad_nonalt$GRAD_RATE_4_YR.2012.13,0))) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(grad_nonalt$GRAD_RATE_4_YR.2012.13) 

  xfit2<-seq(min(round(grad_alt$GRAD_RATE_4_YR.2012.13,1)),max(round(grad_alt$GRAD_RATE_4_YR.2012.13,0)),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(round(grad_alt$GRAD_RATE_4_YR.2012.13,1)),sd=sd(round(grad_alt$GRAD_RATE_4_YR.2012.13,0))) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(grad_alt$GRAD_RATE_4_YR.2012.13)

  curve1=curve1
  curve2=curve2
if(curve1==T){lines(xfit2, yfit2, col="red", lwd=2)}
if(curve2==T){lines(xfit, yfit, col=rgb(0,1,0,.75), lwd=2)}

  legend("topleft", c("Alternative Schools","Non-Alternative Schools","Both"),fill=c("red",rgb(0,1,0,.75),terrain.colors(2)))
  box()

}




# Extended Graduation


Gradext_hist=function(curve1,curve2){

hist1=hist(round(grad_alt$GRAD_RATE_EXTENDED,0),freq=T,xaxt='n',breaks=15,
  main="Distribution Of Extended Graduation Rates \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Graduation Rate",ylab="Number of Schools",col="red",ylim=c(0,16))
  axis(side=1,at=seq(0,1,.2),labels=seq(0,100,20))

hist2 = hist(round(grad_nonalt$GRAD_RATE_EXTENDED,0),freq=T,xaxt='n',breaks=35,col=rgb(0,1,0,.75),add=TRUE)
 
  xfit<-seq(min(round(grad_nonalt$GRAD_RATE_EXTENDED,0)),max(round(grad_nonalt$GRAD_RATE_EXTENDED,0)),length=100) 
  yfit<-dnorm(xfit,mean=mean(round(grad_nonalt$GRAD_RATE_EXTENDED,1)),sd=sd(round(grad_nonalt$GRAD_RATE_EXTENDED,0))) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(grad_nonalt$GRAD_RATE_EXTENDED) 

  xfit2<-seq(min(round(grad_alt$GRAD_RATE_EXTENDED,0)),max(round(grad_alt$GRAD_RATE_EXTENDED,0)),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(round(grad_alt$GRAD_RATE_EXTENDED,0)),sd=sd(round(grad_alt$GRAD_RATE_EXTENDED,0))) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(grad_alt$GRAD_RATE_EXTENDED)

  curve1=curve1
  curve2=curve2
if(curve1==T){lines(xfit2, yfit2, col="red", lwd=2)}
if(curve2==T){lines(xfit, yfit, col=rgb(0,1,0,.75), lwd=2)}

  legend("topleft", c("Alternative Schools","Non-Alternative Schools"),fill=c("red",rgb(0,1,0,.75)))
  box()

}




# Grade 9 Credit


Credit9_hist=function(curve1){

x=grade9$PERCENT_GD_9_CREDIT_MET
h=hist(x,col="green",xlab="Percentage of Students in School That Met Requirement",
main="Distribution Of Grade 9 Credit Completion \n Alternative Schools vs. Non-Alternative Schools",
breaks=50,ylab="Number of Schools")
rect(c(grade9_alt$PERCENT_GD_9_CREDIT_MET)-1,0,c(grade9_alt$PERCENT_GD_9_CREDIT_MET)+1,1,col="red")
xfit2<-seq(min(x),max(x),length=100) 
yfit2<-dnorm(xfit2,mean=mean(round(x,1)),sd=sd(round(x,1))) 
yfit2 <- yfit2*diff(h$mids[1:2])*length(x)
curve1=curve1
if(curve1==T){lines(xfit2, yfit2, col="black", lwd=2)}
legend('topleft',c("Alternative Schools","Non-Alternative Schools"),fill=c("red","green"))
box()

}


# HATHAWAY MEAN CATEGORY


Hath_hist=function(curve1,curve2){

hist1=hist(round(hath_nonalt$HATH_CAT_MEAN,1),freq=T,breaks=30,
  main="Distribution Of Mean Hathaway Category \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Mean Hathaway Category",ylab="Number of Schools",col=rgb(0,1,0,.75),ylim=c(0,10))

hist2 = hist(round(hath_alt$HATH_CAT_MEAN,1),freq=T,xaxt='n',breaks=10,col=rgb(1,0,0,.75),add=TRUE)
 
  xfit<-seq(min(hath_alt$HATH_CAT_MEAN),max(hath_alt$HATH_CAT_MEAN),length=100) 
  yfit<-dnorm(xfit,mean=mean(hath_alt$HATH_CAT_MEAN),sd=sd(hath_alt$HATH_CAT_MEAN)) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(hath_alt$HATH_CAT_MEAN) 

  xfit2<-seq(min(hath_nonalt$HATH_CAT_MEAN),max(hath_nonalt$HATH_CAT_MEAN),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(hath_nonalt$HATH_CAT_MEAN),sd=sd(hath_nonalt$HATH_CAT_MEAN)) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(hath_nonalt$HATH_CAT_MEAN)

  curve1=curve1
  curve2=curve2
if(curve1==T){lines(xfit2, yfit2, col=rgb(0,1,0,.75), lwd=2)}
if(curve2==T){lines(xfit, yfit, col=rgb(1,0,0,.75), lwd=2)}
 
  legend('topleft',c("Alternative Schools","Non-Alternative Schools","Both"),
  fill=c(rgb(1,0,0,.75),rgb(0,1,0,.75),"red"),cex=.7)
  box()
}



# HATHAWAY INDEX SCORE


Hath_index_hist=function(curve1,curve2){

hist1=hist(round(hath_nonalt$HATH_INDEX_SCORE_MEAN,0),freq=T,breaks=30,
  main="Distribution Of Hathaway Score \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Hathaway Score",ylab="Number of Schools",col=rgb(0,1,0,.75),xlim=c(0,85))

hist2 = hist(round(hath_alt$HATH_INDEX_SCORE_MEAN,0),freq=T,xaxt='n',breaks=20,col=rgb(1,0,0,.75),add=TRUE)
 
  xfit<-seq(min(hath_alt$HATH_INDEX_SCORE_MEAN),max(hath_alt$HATH_INDEX_SCORE_MEAN),length=100) 
  yfit<-dnorm(xfit,mean=mean(hath_alt$HATH_INDEX_SCORE_MEAN),sd=sd(hath_alt$HATH_INDEX_SCORE_MEAN)) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(hath_alt$HATH_INDEX_SCORE_MEAN) 

  xfit2<-seq(min(hath_nonalt$HATH_INDEX_SCORE_MEAN),max(hath_nonalt$HATH_INDEX_SCORE_MEAN),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(hath_nonalt$HATH_INDEX_SCORE_MEAN),sd=sd(hath_nonalt$HATH_INDEX_SCORE_MEAN)) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(hath_nonalt$HATH_INDEX_SCORE_MEAN)

  curve1=curve1
  curve2=curve2
if(curve1==T){lines(xfit2, yfit2, col=rgb(0,1,0,.75), lwd=2)}
if(curve2==T){lines(xfit, yfit, col=rgb(1,0,0,.75), lwd=2)}
 
  legend('topleft',c("Alternative Schools","Non-Alternative Schools","Both"),
  fill=c(rgb(1,0,0,.75),rgb(0,1,0,.75),"red"),cex=.7)
  box()
}


# TESTED READINESS


Test_hist=function(curve1,curve2){

hist1=hist(test_nonalt$HS_TESTED_READINESS_MEAN,freq=T,breaks=35,
  main="Distribution Of Tested Readiness Score \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Tested Readiness Score",ylab="Number of Schools",col=rgb(0,1,0,.75),ylim=c(0,14))

hist2 = hist(test_alt$HS_TESTED_READINESS_MEAN,freq=T,xaxt='n',breaks=7,col=rgb(1,0,0,.75),add=TRUE)
 
  xfit<-seq(min(test_alt$HS_TESTED_READINESS_MEAN),max(test_alt$HS_TESTED_READINESS_MEAN),length=100) 
  yfit<-dnorm(xfit,mean=mean(test_alt$HS_TESTED_READINESS_MEAN),sd=sd(test_alt$HS_TESTED_READINESS_MEAN)) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(test_alt$HS_TESTED_READINESS_MEAN) 

  xfit2<-seq(min(test_nonalt$HS_TESTED_READINESS_MEAN),max(test_nonalt$HS_TESTED_READINESS_MEAN),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(test_nonalt$HS_TESTED_READINESS_MEAN),sd=sd(test_nonalt$HS_TESTED_READINESS_MEAN)) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(test_nonalt$HS_TESTED_READINESS_MEAN)
  
  curve1=curve1
  curve2=curve2
if(curve1==T){lines(xfit, yfit, col=rgb(1,0,0,.75),lwd=2)}
if(curve2==T){lines(xfit2, yfit2, col=rgb(0,1,0,.75), lwd=2)}
 
  legend('topleft',c("Alternative Schools","Non-Alternative Schools","Both"),fill=c(rgb(1,0,0,.75),rgb(0,1,0,.75),"red"))
  box()
}



# ADDITIONAL READINESS

Add_Read_hist=function(curve1){

x=add$HS_ADD_READINESS_SCORE_TYPE1
h=hist(x,col=rgb(0,1,0,.75),xlab="Additional Readiness Score",main="Distribution Of Additional Readiness Score \n Alternative Schools vs. Non-Alternative Schools",breaks=25,ylab="Number of Schools")
rect(c(add_alt$HS_ADD_READINESS_SCORE_TYPE1-1),0,c(add_alt$HS_ADD_READINESS_SCORE_TYPE1+1),1,col=rgb(1,0,0,.75))
xfit2<-seq(min(x),max(x),length=100) 
yfit2<-dnorm(xfit2,mean=mean(round(x,1)),sd=sd(round(x,1))) 
yfit2 <- yfit2*diff(h$mids[1:2])*length(x)
curve1=curve1
if(curve1==T){lines(xfit2, yfit2, col=rgb(0,1,0,.75), lwd=2)}
legend('topleft',c("Alternative Schools","Non-Alternative Schools"),fill=c(rgb(1,0,0,.75),rgb(0,1,0,.75)),cex=.7)
box()
}



# HIGH SCHOOL EQUITY

Equity_hist=function(curve1){

x=equity$HS_EQUITY_MEAN
h=hist(x,col=rgb(0,1,0,.75),xlab="Equity Score",main="Distribution Of Equity Score \n Alternative Schools vs. Non-Alternative Schools",
breaks=30,ylab="Number of Schools")
rect(c(equity_alt$HS_EQUITY_MEAN),0,c(equity_alt$HS_EQUITY_MEAN) +1,1,col="red")
xfit2<-seq(min(x),max(x),length=100) 
yfit2<-dnorm(xfit2,mean=mean(round(x,1)),sd=sd(round(x,1))) 
yfit2 <- yfit2*diff(h$mids[1:2])*length(x)
curve1=curve1
if(curve1==T){lines(xfit2, yfit2, col=rgb(0,1,0,.75), lwd=2)}
legend('topleft',c("Alternative Schools","Non-Alternative Schools"),fill=c("red",rgb(0,1,0,.75)))
box()
}



# HIGH SCHOOL ACHIEVEMENT

Achieve_hist=function(curve1,curve2){

hist1=hist(achieve_nonalt$HS_ACHIEVEMENT_PERCENT_PROFICIENT,freq=T,breaks=35,
  main="Distribution Of Achievement Score \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Achievement Score",ylab="Number of Schools",col="green",ylim=c(0,8))

hist2 = hist(achieve_alt$HS_ACHIEVEMENT_PERCENT_PROFICIENT,freq=T,xaxt='n',breaks=15,col=rgb(1,0,0,.75),add=TRUE)
 
  xfit<-seq(min(achieve_alt$HS_ACHIEVEMENT_PERCENT_PROFICIENT),max(achieve_alt$HS_ACHIEVEMENT_PERCENT_PROFICIENT),length=100) 
  yfit<-dnorm(xfit,mean=mean(achieve_alt$HS_ACHIEVEMENT_PERCENT_PROFICIENT),sd=sd(achieve_alt$HS_ACHIEVEMENT_PERCENT_PROFICIENT)) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(achieve_alt$HS_ACHIEVEMENT_PERCENT_PROFICIENT) 

  xfit2<-seq(min(achieve_nonalt$HS_ACHIEVEMENT_PERCENT_PROFICIENT),max(achieve_nonalt$HS_ACHIEVEMENT_PERCENT_PROFICIENT),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(achieve_nonalt$HS_ACHIEVEMENT_PERCENT_PROFICIENT),sd=sd(achieve_nonalt$HS_ACHIEVEMENT_PERCENT_PROFICIENT)) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(achieve_nonalt$HS_ACHIEVEMENT_PERCENT_PROFICIENT)

if(curve1==T){lines(xfit, yfit, col=rgb(1,0,0,.75), lwd=2)}
if(curve2==T){lines(xfit2, yfit2, col="dark green", lwd=2)}
 
  legend('topleft',c("Alternative Schools","Non-Alternative Schools"),fill=c(rgb(1,0,0,.75),"green"))
  box()
}

#______________________________________________________________________________________________________________________________________

# HIGH SCHOOL ACHIEVEMENT PARTICIPATION RATES

AchieveP_hist=function(curve1,curve2){

hist1=hist(achieve_alt$HS_ACHIEVEMENT_PARTICIPATION_RATE,freq=T,breaks=30,
  main="Distribution Of Achievement Participation \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Percent Participated",ylab="Number of Schools",col="red",ylim=c(0,40))

hist2 = hist(achieve_nonalt$HS_ACHIEVEMENT_PARTICIPATION_RATE,freq=T,xaxt='n',breaks=10,col=rgb(0,1,0,.75),add=TRUE)
 
  xfit<-seq(min(achieve_alt$HS_ACHIEVEMENT_PARTICIPATION_RATE),max(achieve_alt$HS_ACHIEVEMENT_PARTICIPATION_RATE),length=100) 
  yfit<-dnorm(xfit,mean=mean(achieve_alt$HS_ACHIEVEMENT_PARTICIPATION_RATE),sd=sd(achieve_alt$HS_ACHIEVEMENT_PARTICIPATION_RATE)) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(achieve_alt$HS_ACHIEVEMENT_PARTICIPATION_RATE) 

  xfit2<-seq(min(achieve_nonalt$HS_ACHIEVEMENT_PARTICIPATION_RATE),max(achieve_nonalt$HS_ACHIEVEMENT_PARTICIPATION_RATE),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(achieve_nonalt$HS_ACHIEVEMENT_PARTICIPATION_RATE),sd=sd(achieve_nonalt$HS_ACHIEVEMENT_PARTICIPATION_RATE)) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(achieve_nonalt$HS_ACHIEVEMENT_PARTICIPATION_RATE)
  curve1=curve1
  curve2=curve2
if(curve1==T){lines(xfit, yfit, col=rgb(1,0,0,.6), lwd=2)}
if(curve2==T){lines(xfit2, yfit2, col=rgb(0,1,0,.75), lwd=2)}
 
  legend('topleft',c("Alternative Schools","Non-Alternative Schools"),fill=c("red",rgb(0,1,0,.75)))
  box()
}


# HIGH SCHOOL EQUITY PARTICIPATION RATES

EquityP_hist=function(curve){

x=equity$HS_EQUITY_PARTICIPATION_RATE
h=hist(x,col=rgb(0,1,0,.75),xlab="Percent Participated",main="Distribution Of Equity Participation Rate \n Alternative Schools vs. Non-Alternative Schools",breaks=30,ylab="Number of Schools")
rect(c(equity_alt$HS_EQUITY_PARTICIPATION_RATE)-.4,0,c(equity_alt$HS_EQUITY_PARTICIPATION_RATE)+.1,1,col="red")
xfit2<-seq(min(x),max(x),length=100) 
yfit2<-dnorm(xfit2,mean=mean(round(x,1)),sd=sd(round(x,1))) 
yfit2 <- yfit2*diff(h$mids[1:2])*length(x)
if(curve==T){
lines(xfit2, yfit2, col="green", lwd=2)
}
legend('topleft',c("Alternative Schools","Non-Alternative Schools"),fill=c("red",rgb(0,1,0,.75)))
box()
}



# HIGH SCHOOL TESTED READINESS PARTICIPATION RATES

TestP_hist=function(curve1,curve2){

hist1=hist(test_alt$HS_TESTED_READINESS_PARTICIPATION_RATE ,freq=T,breaks=30,
  main="Distribution Of Tested Readiness Participation \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Percent Participated",ylab="Number of Schools",col="red",ylim=c(0,35))

hist2 = hist(test_nonalt$HS_TESTED_READINESS_PARTICIPATION_RATE,freq=T,xaxt='n',breaks=10,col=rgb(0,1,0,.75),add=TRUE)
 
  xfit<-seq(min(test_nonalt$HS_TESTED_READINESS_PARTICIPATION_RATE),max(test_nonalt$HS_TESTED_READINESS_PARTICIPATION_RATE),length=100) 
  yfit<-dnorm(xfit,mean=mean(test_nonalt$HS_TESTED_READINESS_PARTICIPATION_RATE),sd=sd(test_nonalt$HS_TESTED_READINESS_PARTICIPATION_RATE)) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(test_nonalt$HS_TESTED_READINESS_PARTICIPATION_RATE) 

  xfit2<-seq(min(test_alt$HS_TESTED_READINESS_PARTICIPATION_RATE),max(test_alt$HS_TESTED_READINESS_PARTICIPATION_RATE),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(test_alt$HS_TESTED_READINESS_PARTICIPATION_RATE),sd=sd(test_alt$HS_TESTED_READINESS_PARTICIPATION_RATE)) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(test_alt$HS_TESTED_READINESS_PARTICIPATION_RATE)

if(curve2==T){lines(xfit, yfit, col=rgb(0,1,0,.75), lwd=2)}
if(curve1==T){lines(xfit2, yfit2, col="red", lwd=2)}
 
  legend('topleft',c("Alternative Schools","Non-Alternative Schools"),fill=c("red",rgb(0,1,0,.75)))
  box()
}


# HIGH SCHOOL OVERALL PARTICIPATION RATES

LowP_hist=function(curve1,curve2){

hist1=hist(part_alt$LOWEST.RATE,freq=T,breaks=30,
  main="Distribution Of Overall Participation \n Alternative Schools vs. Non-Alternative Schools",
  xlab="Percent Participated",ylab="Number of Schools",col="red",ylim=c(0,35))

hist2 = hist(part_nonalt$LOWEST.RATE,freq=T,xaxt='n',breaks=10,col=rgb(0,1,0,.75),add=TRUE)
 
  xfit<-seq(min(part_nonalt$LOWEST.RATE),max(part_nonalt$LOWEST.RATE),length=100) 
  yfit<-dnorm(xfit,mean=mean(part_nonalt$LOWEST.RATE),sd=sd(part_nonalt$LOWEST.RATE)) 
  yfit <- yfit*diff(hist2$mids[1:2])*length(part_nonalt$LOWEST.RATE) 

  xfit2<-seq(min(part_alt$LOWEST.RATE),max(part_alt$LOWEST.RATE),length=100) 
  yfit2<-dnorm(xfit2,mean=mean(part_alt$LOWEST.RATE),sd=sd(part_alt$LOWEST.RATE)) 
  yfit2 <- yfit2*diff(hist1$mids[1:2])*length(part_alt$LOWEST.RATE)

if(curve2==T){lines(xfit, yfit, col=rgb(0,1,0,.75), lwd=2)}
if(curve1==T){lines(xfit2, yfit2, col="red", lwd=2)}
 
  legend('topleft',c("Alternative Schools","Non-Alternative Schools"),fill=c("red",rgb(0,1,0,.75)))
  box()
}

#___________________________________________________________________________________________________________________________


# ELEMENTARY ACHIEVEMENT

AchieveE_hist=function(curve){
  
  hist1=hist(e_elem$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT,freq=T,breaks=50,main="Achievement \n Grades 3-8",
             xlab="Percent Proficient",ylab="Number of Schools",col="red",ylim=c(0,16))
  
  
  hist2 = hist(e_mixed$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT,freq=T,breaks=25,col=rgb(0,1,0,.75),add=TRUE)
  
  hist3 = hist(e_junior$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT,freq=T,breaks=30,col=rgb(0,0,1,.75),add=TRUE)
  
  xfit<-seq(min(elem_achieve$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT),
            max(elem_achieve$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT),length=100) 
  yfit<-dnorm(xfit,mean=mean(elem_achieve$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT),
              sd=sd(elem_achieve$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT)) 
  yfit <- yfit*diff(hist1$mids[1:2])*length(elem_achieve$G38_ACHIEVEMENT_ALL_PERCENT_PROFICIENT) 
  curve=curve
  if(curve==T){lines(xfit, yfit, col="black", lwd=2)}
  legend('topleft',c("Grades 3-6",
                     "Grades 7 and/or 8","A mix of A and B"),
         fill=c("red",rgb(0,1,0,.75),rgb(0,0,1,.75)),title="Schools (Specified Grades Only)",cex=.7)
  box()
  
}


# ELEMENTARY EQUITY

EquityE_hist=function(curve){
hist1=hist(elem_equity$G38_EQUITY_MEAN,breaks=30,col="orange",main="School Equity Scores \n Grades 3-8",
xlab="Mean Equity Score",ylab="Number of Schools")

 xfit<-seq(min(elem_equity$G38_EQUITY_MEAN),max(elem_equity$G38_EQUITY_MEAN),length=100) 
 yfit<-dnorm(xfit,mean=mean(elem_equity$G38_EQUITY_MEAN),sd=sd(elem_equity$G38_EQUITY_MEAN)) 
 yfit <- yfit*diff(hist1$mids[1:2])*length(elem_equity$G38_EQUITY_MEAN) 
 curve=curve
if(curve==T){lines(xfit, yfit, col="black", lwd=2)}
}



# ELEMENTARY GROWTH

Growth_hist=function(curve){

hist1=hist(elem_growth$G38_GROWTH_MGP,breaks=50,col="light blue",main="Growth Scores \n Grades 4-8",
xlab="Median Growth Percentile",ylab="Number of Schools")

 xfit<-seq(min(elem_growth$G38_GROWTH_MGP),max(elem_growth$G38_GROWTH_MGP),length=100) 
 yfit<-dnorm(xfit,mean=mean(elem_growth$G38_GROWTH_MGP),sd=sd(elem_growth$G38_GROWTH_MGP)) 
 yfit <- yfit*diff(hist1$mids[1:2])*length(elem_growth$G38_GROWTH_MGP) 
 curve=curve
if(curve==T){lines(xfit, yfit, col="black", lwd=2)}
}




