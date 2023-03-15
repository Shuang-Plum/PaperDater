# filter and plot for keyword search
keyword_filter_plot<-function(dat.df,min.count,min.count.forlabel,keywords){
  
  min.count<-as.numeric(min.count)
  min.count.forlabel<-as.numeric(min.count.forlabel)
  
  if (nrow(dat.df)>0) {
    
    # order journals by total count regardless of year
    j.list<-sort(table(dat.df$Journal),decreasing = T)
    
    # filter journal
    j.list.fl<-names(j.list[j.list>=min.count])
    
    j.c<-length(j.list.fl)
    
    # if still have data after filtering -- plot
    # otherwise print empty plot
    
    if (j.c>0) {
      
      # if journal is still greater than 20 after filtering
      # get first 20, tie breaks by name alphabet
      if (j.c>20) {
        j.list.fl<-j.list.fl[1:20]
        j.c<-20
      }
      
      
      dat.df.fl<-dat.df[which(dat.df$Journal %in% j.list.fl),]
      
      
      # if after filtering with min.count the journal num is still greater than 20
      # order with publication number and break tie with IF
      
      plot.df<-as.data.frame(table(dat.df.fl$Journal, dat.df.fl$PubYear))
      
      colnames(plot.df)<-c('Journal','PubYear','Freq')
      
      cur.col<-color_pick(j.c)
      
      ptitle<-paste0("Publications by published year for keywords: ",keywords)
      
      ggplot(data = plot.df,
             aes(x = PubYear, y = Freq, alluvium = Journal, 
                 node=factor(PubYear),stratum=Journal,
                 label=paste0(Journal,' (',Freq,')'))) +
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
        geom_alluvium(aes(fill = Journal, colour = Journal),
                      alpha = 0.8, width=0.2, curve_type = "xspline")+
        #geom_stratum(aes(fill=Journal),width=0.2,alpha=0.8,color='transparent')+
        geom_label(stat='stratum',size=5,min.y=min.count.forlabel,
                   color='black',fill='white',alpha=0.5,fontface='bold',
                   label.padding = unit(0.1, "lines"),label.size = 0) +
        scale_fill_manual(values=cur.col) +
        scale_color_manual(values=cur.col) +
        #facet_wrap(~ region, scales = "fixed") +
        ggtitle(ptitle)+
        guides(colour = guide_legend(override.aes = list(alpha = 0.8)))
      
    } else {
      # j.c=0 no journal after filtering
      empty.df<- data.frame()
      ggplot(data = empty.df) +
        xlab("Year of Publication")+
        ylab("Count")+
        #scale_y_continuous(breaks=c(0,1))+
        #scale_x_continuous(breaks=seq(starty,endy,1)) +
        theme(plot.title=element_text(hjust=0.5,size=24,face='bold'),
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
              axis.text.x=element_text(size=18, angle = -30, hjust = 0, face='bold',color='black'),
              axis.text.y=element_text(size=18,face='bold',color='black'),
              axis.title.x=element_text(size=20,face='bold'),
              axis.title.y=element_text(size=20,face='bold',angle=90)) +
        geom_blank()+
        #facet_wrap(~ region, scales = "fixed") +
        ggtitle("No Journal after filtering with minimal count")
      
    }
    
  } else {
    # no search results
    empty.df<- data.frame()
    ggplot(data = empty.df) +
      xlab("Year of Publication")+
      ylab("Count")+
      #scale_y_continuous(breaks=c(0,1))+
      #scale_x_continuous(breaks=seq(starty,endy,1)) +
      theme(plot.title=element_text(hjust=0.5,size=24,face='bold'),
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
            axis.text.x=element_text(size=18, angle = -30, hjust = 0, face='bold',color='black'),
            axis.text.y=element_text(size=18,face='bold',color='black'),
            axis.title.x=element_text(size=20,face='bold'),
            axis.title.y=element_text(size=20,face='bold',angle=90)) +
      geom_blank()+
      #facet_wrap(~ region, scales = "fixed") +
      ggtitle("No publication returned \n Consider change keywords")
  }
  
}