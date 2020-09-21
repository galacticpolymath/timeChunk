require(pacman)
p_load(tidyverse,fs)
#source("https://raw.githubusercontent.com/galacticpolymath/ggGalactic/master/ggGalactic.R")

#notRun
#totTime=45;segTimes=3

destFolder<-"/Users/mattwilkins/GDrive/GP/Units/unit-FemalesSing/Lessons/Math/chunking"

timeChunk <- function(totTime,segTimes,destFolder,prefix,Color,lineWidth){
  if(missing(Color)){Color="#2c83c3"}
  if(missing(lineWidth)){lineWidth=3}
  if(missing(prefix)){prefix<-""}else{prefix<-paste0(prefix,"_")}
  starts<-c(0,cumsum(segTimes)[-length(segTimes)] )
  ends<-cumsum(segTimes)
  #Test if segments add to totTime
  if(max(ends>totTime)){stop(paste0("Sum of seg times (",paste(segTimes,collapse=", "),") is > total time (",totTime,")"))}
  
  Gs<-list()
  for (i in 1:length(segTimes)){
    segTimes_i<-segTimes[i]
    df<-data.frame(x=seq(starts[i]+.5,ends[i]-.5,1))
   
  
  #plot
  g<-ggplot(df)+geom_segment(aes(x=x,xend=x,y=0,yend=.5),colour=Color,lwd=lineWidth)+scale_x_continuous(breaks=seq(0,totTime,5),limits=c(-.5,totTime+.5))+xlab("")+ylab("")+geom_text(x=-1,y=.25,label=paste(segTimes_i,"min"),size=10,hjust=1,colour=Color)+ggGalactic()+coord_cartesian(clip="off",expand=F)+
  theme(
    axis.text.y=element_blank(),
    axis.title=element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x = element_line(colour="gray30",size=.5),
    panel.grid.major.y=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.minor.x=element_blank(),
    panel.border=element_blank(),#element_rect(colour="gray50",size=.1),
    panel.background = element_rect(fill="gray95"),
    panel.grid.major.x=element_line(size=.5,colour="gray30"),
    axis.text.x=element_text(colour="gray30"),
    plot.margin=margin(c(5,10,-10,90),unit="pt")
  )
   
   ggsave(path(destFolder,paste0(prefix,"seg_",i,"of",length(segTimes)),ext="png"),plot=g,width=14,height=1)
   }#end for loop
  
  #lapply(1:length(Gs),function(i){
   #  ggsave(path(destFolder,paste0(prefix,"seg_",i,"of",length(segTimes)),ext="png"),plot=Gs[[i]],width=6,height=1)
 #})
}
timeChunk(45,c(3,5,9,3,20,5),destFolder=destFolder)


