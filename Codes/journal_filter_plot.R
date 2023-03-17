# filter and plot journal mesh terms
journal_filter_plot<-function(dat.df, min.count.forlabel, j.name) {
  
  min.count.forlabel<-as.numeric(min.count.forlabel)
  
  if (is.data.frame(dat.df)) {
    
    # order mesh by total count regardless of year
    m.list<-sort(table(dat.df$MeSH),decreasing = T)
    
    # filter mesh to 20 or total num whichever is smaller
    tm<-min(length(m.list),20)
    m.list.fl<-names(m.list[1:tm])
    
    # in case MeSh is a factor
    dat.df$MeSH<-as.character(dat.df$MeSH)
    
    dat.df.fl<-dat.df[which(dat.df$MeSH %in% m.list.fl),]
    
    plot.df<-as.data.frame(table(dat.df.fl$MeSH, dat.df.fl$PubYear))
    
    colnames(plot.df)<-c('MeSH','PubYear','Freq')
    
    cur.col<-color_pick(tm)
    
    ptitle<-paste0("MeSH terms by published year for journal: ",j.name)
    
    plot.df<-plot.df[order(plot.df$PubYear,-(plot.df$Freq)),]
    
    lastyear<-max(as.character(plot.df$PubYear))
    
    plevel<-as.character(plot.df$MeSH[which(plot.df$PubYear==lastyear)])
    
    plot.df$MeSH<-factor(plot.df$MeSH,levels=c(plevel))
    
    
    ggplot(data = plot.df,
           aes(x = PubYear, y = Freq, alluvium = MeSH,
               node=factor(PubYear),stratum=MeSH,
               label=paste0(MeSH,' (',Freq,')'))) +
      xlab("Year of Publication")+
      ylab("Count")+
      scale_y_continuous(breaks= integer_breaks())+
      #scale_y_discrete() +
      theme(plot.title=element_text(hjust=0.5,size=24,face='bold'),
            panel.background=element_rect(fill='white'),
            #plot.margin = margin(5, 2, 192, 2, "pt"),
            panel.grid.major=element_line(color='grey',size=0.3),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5),
            legend.title=element_blank(),
            legend.justification=c(0,1),
            legend.position='top',
            legend.key.height = unit(0.5,'line'),
            legend.key.width  = unit(1,'line'),
            #legend.key=element_rect(fill='transparent'),
            legend.text=element_text(size=14,face='bold'),
            legend.text.align=0,
            axis.text.x=element_text(size=18, angle = -30, hjust = 0, face='bold',color='black'),
            axis.text.y=element_text(size=18,face='bold',color='black'),
            axis.title.x=element_text(size=20,face='bold'),
            axis.title.y=element_text(size=20,face='bold',angle=90)) +
      geom_alluvium(aes(fill = MeSH, colour = MeSH),
                    alpha = 0.8, width=0.2, curve_type = "xspline") +
      #geom_stratum(aes(fill=MeSH),width=0.2,alpha=0.8,color='transparent')+
      geom_label(stat='stratum',size=5,min.y=min.count.forlabel,
                 color='black',fill='white',alpha=0.5,fontface='bold',
                 label.padding = unit(0.1, "lines"),label.size = 0) +
      scale_fill_manual(values=cur.col) +
      scale_color_manual(values=cur.col) +
      #facet_wrap(~ region, scales = "fixed") +
      ggtitle(ptitle)+ 
      guides(colour = guide_legend(override.aes = list(alpha = 0.8)))
    
  } else {
    
    ptitle<-dat.df
    
    empty.df<- data.frame()
    ggplot(data = empty.df) +
      xlab("Year of Publication")+
      ylab("Count")+
      #scale_y_continuous(breaks=c(0,1))+
      #scale_x_continuous(breaks=seq(starty,endy,1)) +
      theme(plot.title=element_text(hjust=0.5,size=18,face='bold'),
            panel.background=element_rect(fill='white'),
            #plot.margin = margin(5, 2, 192, 2, "pt"),
            panel.grid.major=element_line(color='grey',size=0.3),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5),
            legend.title=element_blank(),
            legend.justification=c(0,1),
            legend.position='none',
            #legend.key.height = unit(1,'line'),
            #legend.key.width  = unit(1,'line'),
            #legend.key=element_rect(fill='transparent'),
            #legend.text=element_text(size=16,face='bold'),
            #legend.text.align=0,
            axis.text.x=element_text(size=16, angle = -30, hjust = 0, face='bold',color='black'),
            axis.text.y=element_text(size=16,face='bold',color='black'),
            axis.title.x=element_text(size=18,face='bold'),
            axis.title.y=element_text(size=18,face='bold',angle=90)) +
      geom_blank()+
      #facet_wrap(~ region, scales = "fixed") +
      ggtitle(ptitle)
  }
  
}