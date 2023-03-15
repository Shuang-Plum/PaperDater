# mesh plot 
MeSH_plot<-function(out5) {
  
  if (length(out5)>0) {
    
    ylen<-ceiling(length(out5)/3)
    xx <- rep(seq(1,3,1),times=ylen)
    yy <- rep(seq(ylen,1,-1),each=3)
    
    xx<-xx[1:length(out5)]
    yy<-yy[1:length(out5)]
    
    labelss <-out5
    df<-data.frame(xx,yy,labelss)
    
    ggplot()+
      theme(plot.title=element_text(hjust=0.5,size=24,face='bold'),
            panel.background=element_rect(fill='white'),
            #plot.margin = margin(5, 2, 192, 2, "pt"),
            panel.grid.major=element_blank(),
            axis.ticks = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            legend.title=element_blank(),
            legend.position='none',
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      geom_tile(aes(xx, yy), fill='#333333',color='grey',df) +
      geom_text(aes(xx, yy), label = out5,
                color ='white', df, size=5,fontface='bold')+
      ggtitle("MeSH terms matched with input text")
    
  } else {
    
    empty.df<- data.frame()
    
    ggplot(data=empty.df)+
      theme(plot.title=element_text(hjust=0.5,size=24,face='bold'),
            panel.background=element_rect(fill='white'),
            #plot.margin = margin(5, 2, 192, 2, "pt"),
            panel.grid.major=element_blank(),
            axis.ticks = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            legend.title=element_blank(),
            legend.position='none',
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      geom_blank()+
      ggtitle('No matched MeSH terms')
  }
}
