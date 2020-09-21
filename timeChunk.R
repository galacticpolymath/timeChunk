require(pacman)
p_load(tidyverse,fs,plotly,htmlwidgets)
#source("https://raw.githubusercontent.com/galacticpolymath/ggGalactic/master/ggGalactic.R")

#notRun
#totTime=45;segTimes=3

destFolder<-"/Users/mattwilkins/GDrive/GP/Units/unit-FemalesSing/Lessons/Math/chunking"

timeChunk <- function(totTime,segTimes,destFolder,prefix,Color,fadedColor,lineWidth){
  if(missing(Color)){Color="#2c83c3"}
  if(missing(fadedColor)){fadedColor="#1A4E75"}
  if(missing(lineWidth)){lineWidth=7}
  if(missing(prefix)){prefix<-""}else{prefix<-paste0(prefix,"_")}
  starts<-c(0,cumsum(segTimes)[-length(segTimes)] )
  ends<-cumsum(segTimes)
  #Test if segments add to totTime
  if(max(ends>totTime)){stop(paste0("Sum of seg times (",paste(segTimes,collapse=", "),") is > total time (",totTime,")"))}
  
  Gs<-list()
  for (i in 1:length(segTimes)){
    segTimes_i<-segTimes[i]
    df<-data.frame(x=seq(starts[i]+.5,ends[i]-.5,1))
    faded_x=data.frame(x=if(i==1){1}else{seq(0.5,ends[i-1]-.5,1)})
    faded_col=ifelse(i==1,"transparent",fadedColor)
    cust_breaks=seq(0,totTime,1)
    cust_labels=rep("",totTime+1)
    cust_labels[seq(1,totTime+1,5)]<-seq(0,totTime,5)
    
  
  #plot
  g<-ggplot(df)+geom_segment(aes(x=x,xend=x,y=0,yend=.5),colour=Color,lwd=lineWidth,show.legend=F)+
    geom_segment(inherit.aes=F,data=faded_x,aes(x=x,xend=x,y=0,yend=.15),colour=faded_col,lwd=lineWidth,show.legend=F)+scale_y_continuous(expand=c(0,0))+scale_x_continuous(breaks=cust_breaks,labels=cust_labels,limits=c(0,totTime+.5),expand=expansion(add=.6,mult=0))+xlab("")+ylab("")+ggGalactic()+
  theme(
    axis.text.y=element_blank(),
    axis.title=element_blank(),
    axis.line.x=element_line(colour="#090816",size=1),
    axis.ticks.y=element_blank(),
    axis.ticks.x = element_line(colour="#090816",size=.5),
    axis.ticks.length=unit(6,"pt"),
    panel.grid.major.y=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.border=element_blank(),#element_rect(colour="gray50",size=.1),
    panel.background = element_rect(fill="#f0f4ff"),
    #panel.grid.major.x=element_line(size=.5,colour="#090816"),
    axis.text.x=element_text(colour="#090816"),
    plot.margin=margin(c(5,1,-10,-20),unit="pt")
  )
   
   ggsave(path(destFolder,paste0(prefix,"seg_",i,"of",length(segTimes)),ext="png"),plot=g,width=14,height=1)
  # gPlotly<-ggplotly(g,width=290*2,height=21*10,staticPlot=T,tooltip = "none")
   # saveWidget(as_widget(gPlotly),"timeChunk.html")
   
   }#end for loop
  
  #lapply(1:length(Gs),function(i){
   #  ggsave(path(destFolder,paste0(prefix,"seg_",i,"of",length(segTimes)),ext="png"),plot=Gs[[i]],width=6,height=1)
 #})
}
timeChunk(45,c(3,5,9,3,20,5),destFolder=destFolder)


