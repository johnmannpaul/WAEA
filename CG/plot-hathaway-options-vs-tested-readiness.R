#read in data

overlap=read.csv(file.choose(),sep=',',header=T)
head(overlap)
overlap=overlap[,c(3,5,6,7)]
head(overlap)

#take out alts

overlap2=overlap[overlap=="FALSE",]
head(overlap2)

#Create Function

hists=function(x,y,col1,col2,xlim1,xlim2,main,legend1,legend2){
  hist(x/100,breaks=30,col=col1,xlim=c(xlim1,xlim2),ylim=c(0,10),freq=F,main=main,
       xlab="Mean Score",ylab="Percentage of Schools")
  hist(y/100,breaks=30,col=col2,add=T)
  legend('topleft',c(legend1,legend2),fill=c(col1,col2))
  box()
  
}


#All 3 - Alt

hist(overlap[,2]/100,breaks=30,col="blue",xlim=c(0,.9),ylim=c(0,10),freq=F,main="Hathway Scales vs. Tested Readiness \n With Alt",
     xlab="Mean Score",ylab="Percentage of Schools")
hist(overlap[,3]/100,breaks=30,col=rgb(1,0,0,.75),add=T)
hist(overlap[,4]/100,breaks=30,col=rgb(0,1,0,.75),add=T)
legend('topleft',c("Original Hathaway","New Hathaway","Tested Readiness"),fill=c('blue',rgb(1,0,0,.75),rgb(0,1,0,.75)))
box()


#All 3 - NonAlt

hist(overlap2[,2]/100,breaks=30,col="blue",xlim=c(0,.9),ylim=c(0,10),freq=F,main="Hathway Scales vs. Tested Readiness \n NonAlt",
     xlab="Mean Score",ylab="Percentage of Schools")
hist(overlap2[,3]/100,breaks=30,col=rgb(1,0,0,.75),add=T)
hist(overlap2[,4]/100,breaks=30,col=rgb(0,1,0,.75),add=T)
legend('topleft',c("Original Hathaway","New Hathaway","Tested Readiness"),fill=c('blue',rgb(1,0,0,.75),rgb(0,1,0,.75)))
box()


#Original vs. Test - Alt

hists(overlap[,2],overlap[,4],"blue",rgb(0,1,0,.6),0,.9,"Oringal Hathway Scale vs. Tested Readiness \n With Alt","Original Hathaway","Tested Readiness")


#Original vs. Test - NonAlt

hists(overlap2[,2],overlap2[,4],"blue",rgb(0,1,0,.6),0,.9,"Oringal Hathway Scale vs. Tested Readiness \n Without Alt","Original Hathaway","Tested Readiness")


#New vs. Test - Alt

hists(overlap[,3],overlap[,4],rgb(1,0,0,.6),rgb(0,1,0,.6),.2,.8,"New Hathway Scale vs. Tested Readiness \n With Alt","New Hathaway","Tested Readiness")


#New vs. Test - NonAlt

hists(overlap2[,3],overlap2[,4],rgb(1,0,0,.6),rgb(0,1,0,.6),.2,.8,"New Hathway Scale vs. Tested Readiness \n Without Alt","New Hathaway","Tested Readiness")


schools <- schools[,names(schools)[grep("^ADD", names(schools), invert=TRUE)]]