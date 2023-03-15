setwd("G:/My Drive/UNC/publication_analysis")

library(ggalluvial)
library(RISmed)
library(tidyverse)

#######################################################################
# input keywords search for recent x years publication in all journals
# plot riverplot of these journals

keywords<-c('yeast machine learning')

#keywords<-c('"yeast"[Title/Abstract] AND "machine learning"[Title/Abstract]) AND (2018:2023[pdat]')

starty<-2020
endy<-2023

min.count<-2

min.count.forlabel<-2

# RISmed generate pubmed searching query -- generative

# set time to be publication date
ydat<-paste0('(',starty,':',endy,' [dp])')
myquery<-paste0('(',keywords,') AND ',ydat)

ris.res <- EUtilsSummary(myquery,type="esearch", db="pubmed", retmax=5000)

# save query summary to a txt file for reference
sink('keywords_query_summary.txt')
summary(ris.res)
sink()

# add newline by the end of text to avoid warning when open by read.delim
write("\n", file = "keywords_query_summary.txt",append = TRUE, sep = "\n")

# check summary to see if any result is returned
res.len<-length(summary(ris.res))

if (res.len>0) {
  
  q.res <- EUtilsGet(ris.res,type="efetch", db="pubmed")
  
  # query.str<-Query(q.res)
  
  # advanced specific search with easyPubmed package
  # search keywords in title and abstract
  # also search keywords in MeSH terms
  
  # kwtiab<-paste0('(',paste(keywords, collapse = '[TIAB] AND '),'[TIAB])')
  # kwmesh<-paste0('(',paste(keywords, collapse = '[mh] AND '),'[mh])')
  # ydat<-paste0('(',starty,':',endy,' [dp])')
  # myquery<-paste0('(',kwtiab,' OR ',kwmesh,')',' AND ',ydat)
  # entrez_ids <- get_pubmed_ids(myquery)
  # dat.abs <- fetch_pubmed_data(entrez_ids, retmax=10000, format = "xml")
  # dat.list <- articles_to_list(pubmed_data = dat.abs)
  # dl<-length(dat.list)
  # dat.df<-data.frame(PMID=character(length = dl),
  #                    #Title=character(length = dl),
  #                    JournalAbbrev=character(length = dl),
  #                    Journal=character(length = dl),
  #                    PubYear=numeric(length = dl))
  # for (n in 1:dl) {
  #   # multiple PMID - journal with erratum has two PMID
  #   dat.df$PMID[n]<-unlist(custom_grep(dat.list[n], tag = "PMID"))[1]
  #   #dat.df$Title[n]<-unlist(custom_grep(dat.list[n], tag = "ArticleTitle"))
  #   # multiple Title - first is journal title, later can be references ttl etc
  #   dat.df$JournalAbbrev[n]<-unlist(custom_grep(dat.list[n], tag = "ISOAbbreviation"))
  #   dat.df$Journal[n]<-unlist(custom_grep(dat.list[n], tag = "Title"))[1]
  #   t.y<-as.character(unlist(custom_grep(dat.list[n], tag = "PubDate")))
  #   t.y<-gsub('/','',t.y)
  #   dat.df$PubYear[n]<-as.numeric(strsplit(t.y,'<Year>')[[1]][2])
  # }
  
  
  q.pmid<-PMID(q.res)
  q.pubyear<-YearPubDate(q.res)
  q.journal<-ISOAbbreviation(q.res)
  
  dat.df<-as.data.frame(cbind(q.pmid,q.pubyear,q.journal))
  colnames(dat.df)<-c('PMID','PubYear','Journal')
  
  # save the full search results for download if needed
  full.df<-as.data.frame(table(dat.df$Journal, dat.df$PubYear))
  colnames(full.df)<-c('Journal','PubYear','PubCount')
  full.df<-pivot_wider(full.df, names_from = PubYear, values_from = PubCount)
  full.df<-as.data.frame(full.df)
  if (nrow(full.df)>1){
    full.df$PubTotal<-apply(full.df[,2:ncol(full.df)],1,sum)
    full.df<-arrange(full.df,desc(PubTotal))
  } else {
    full.df$PubTotal<-sum(full.df[,2:ncol(full.df)])
  }
  
  write.csv(full.df,'completekeywordsresults.csv',row.names = F)
  
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
    
    
    col9<-c('#e41a1c','#377eb8','#4daf4a','#984ea3',
                     '#ff7f00','#ffff00','#a65628','#f781bf','#999999')
                     
    col12<-c('#e31a1c','#fb9a99','#33a02c','#b2df8a','#1f78b4','#a6cee3',
                      '#ff7f00','#fdbf6f','#6a3d9a','#cab2d6','#b15928','#ffff99')
                      
    col20<-c('#e6194B','#ff7f00','#ffff00','#aaffc3','#0000ff','#800000','#4daf4a',
                      '#ffc0cb','#42d4f4', '#911eb4','#808000','#000067','#f7a068',
                      '#026440','#f032e6','#9A6324','#9a245b','#909090','#ffff99','#000000')
                      
    
    if (j.c<=9) {
      cur.col<-col9
    } else if(j.c>9 & j.c<=12) {
      cur.col<-col12
    } else {
      cur.col=col20
    }
    
    
    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
      }
      return(fxn)
    }
    
    ptitle<-paste0("Publications by published year for keywords: ",keywords)
    
    ggplot(data = plot.df,
           aes(x = PubYear, y = Freq, alluvium = Journal, 
               node=factor(PubYear),stratum=Journal,
               label=paste0(Journal,'(',Freq,')'))) +
      xlab("Year of Publication")+
      ylab("Count")+
      scale_y_continuous(breaks= integer_breaks())+
      #scale_y_discrete() +
      theme(plot.title=element_text(hjust=0.5,size=18,face='bold'),
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
            legend.text=element_text(size=10,face='bold'),
            legend.text.align=0,
            axis.text.x=element_text(size=16, angle = -30, hjust = 0, face='bold',color='black'),
            axis.text.y=element_text(size=16,face='bold',color='black'),
            axis.title.x=element_text(size=18,face='bold'),
            axis.title.y=element_text(size=18,face='bold',angle=90)) +
      geom_alluvium(aes(fill = Journal, colour = Journal),
                    alpha = 0.8, width=0.2, curve_type = "xspline")+
      #geom_stratum(aes(fill=Journal),width=0.2,alpha=0.8,color='transparent')+
      geom_label(stat='stratum',size=3,min.y=min.count.forlabel,
                 color='black',fill='white',alpha=0.5,fontface='bold',
                 label.padding = unit(0.1, "lines"),label.size = 0) +
      scale_fill_manual(values=cur.col) +
      scale_color_manual(values=cur.col) +
      #facet_wrap(~ region, scales = "fixed") +
      ggtitle(ptitle)+
      guides(colour = guide_legend(override.aes = list(alpha = 0.8)))
    
    
    
    # ggplot(data = plot.df,
    #        aes(x = PubYear, y = Freq, alluvium = Journal)) +
    #   geom_alluvium(aes(fill = Journal, colour = Journal),
    #                 alpha = 0.8, decreasing = FALSE) +
    #   xlab("Year of Publication")+
    #   ylab("Count")+
    #   scale_y_continuous(breaks= integer_breaks())+
    #   scale_fill_manual(values=cur.col) +
    #   scale_color_manual(values=cur.col) +
    #   ggtitle(ptitle)+
    #   guides(colour = guide_legend(override.aes = list(alpha = 0.8)))
    
  } else {
    # j.c=0 no journal after filtering
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
            legend.position='top',
            legend.key.height = unit(1,'line'),
            legend.key.width  = unit(1,'line'),
            #legend.key=element_rect(fill='transparent'),
            legend.text=element_text(size=16,face='bold'),
            legend.text.align=0,
            axis.text.x=element_text(size=16, angle = -30, hjust = 0, face='bold',color='black'),
            axis.text.y=element_text(size=16,face='bold',color='black'),
            axis.title.x=element_text(size=18,face='bold'),
            axis.title.y=element_text(size=18,face='bold',angle=90)) +
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
    theme(plot.title=element_text(hjust=0.5,size=18,face='bold'),
          panel.background=element_rect(fill='white'),
          #plot.margin = margin(5, 2, 192, 2, "pt"),
          panel.grid.major=element_line(color='grey',size=0.3),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          legend.title=element_blank(),
          legend.justification=c(0,1),
          legend.position='top',
          legend.key.height = unit(1,'line'),
          legend.key.width  = unit(1,'line'),
          #legend.key=element_rect(fill='transparent'),
          legend.text=element_text(size=16,face='bold'),
          legend.text.align=0,
          axis.text.x=element_text(size=16, angle = -30, hjust = 0, face='bold',color='black'),
          axis.text.y=element_text(size=16,face='bold',color='black'),
          axis.title.x=element_text(size=18,face='bold'),
          axis.title.y=element_text(size=18,face='bold',angle=90)) +
    geom_blank()+
    #facet_wrap(~ region, scales = "fixed") +
    ggtitle("No publication returned, consider change keywords")
}



########################################################################
# select a journal and show its top 20 MeSH term change over time
#######################################################################
# return publication Mesh terms for year in range or the most recent 10000
# whichever is smaller

j.name<-'PLOS GENETICS'

starty<-2018
endy<-2022

min.count.forlabel<-200

# RISmed generate pubmed searching query -- generative

# set time to be publication date
ydat<-paste0('(',starty,':',endy,' [dp])')
myquery<-paste0('(',j.name,'[Journal]) AND ',ydat)

ris.res <- EUtilsSummary(myquery,type="esearch", db="pubmed", retmax=5000)

# save query summary to a txt file for reference
sink('journal_query_summary.txt')
summary(ris.res)
sink()
# add newline by the end of text to avoid warning when open by read.delim
write("\n", file = "journal_query_summary.txt",append = TRUE, sep = "\n")

# check summary to see if any result is returned
res.len<-length(summary(ris.res))

if (res.len>0) {
  print('start search')
  
  q.res <- EUtilsGet(ris.res,type="efetch", db="pubmed")
  print('finished search')
  
  q.pmid<-PMID(q.res)
  q.pubyear<-YearPubDate(q.res)
  q.journal<-ISOAbbreviation(q.res)
  q.mesh<-Mesh(q.res)
  
  dat.df<-data.frame('MeSH'=character(),'MeSHType'=character(),
                     'PMID'=character(),'PubYear'=numeric(),
                     'Journal'=character())
  
  q.pimd<-q.pmid[which(q.pubyear>=starty & q.pubyear<=endy)]
  q.journal<-q.journal[which(q.pubyear>=starty & q.pubyear<=endy)]
  q.mesh<-q.mesh[which(q.pubyear>=starty & q.pubyear<=endy)]
  q.pubyear<-q.pubyear[which(q.pubyear>=starty & q.pubyear<=endy)]
  
  if (sum(!is.na(q.mesh))>0) {
    print ('start df')
    for (m in 1:length(q.mesh)) {
      
      # only process if it has MeSH term
      if (!is.na(q.mesh[m])) {
        
        temp.mesh<-as.data.frame(q.mesh[m])
        
        colnames(temp.mesh)<-c('MeSH','MeSHType')
        
        temp.mesh$PMID<-q.pmid[m]
        temp.mesh$PubYear<-q.pubyear[m]
        temp.mesh$Journal<-q.journal[m]
        
        dat.df<-rbind(dat.df,temp.mesh)
        
      }
      
    }
    
    print('finish df')
    row.names(dat.df)<-c(1:nrow(dat.df))
    
    # save full results for downloading
    write.csv(dat.df,'completejournalresults.csv',row.names = F)
    
    # order mesh by total count regardless of year
    m.list<-sort(table(dat.df$MeSH),decreasing = T)
    
    # filter mesh to 20 or total num whichever is smaller
    tm<-min(length(m.list),20)
    m.list.fl<-names(m.list[1:tm])
    
    dat.df.fl<-dat.df[which(dat.df$MeSH %in% m.list.fl),]
    
    plot.df<-as.data.frame(table(dat.df.fl$MeSH, dat.df.fl$PubYear))
    
    colnames(plot.df)<-c('MeSH','PubYear','Freq')
    
    col9<-c('#e41a1c','#377eb8','#4daf4a','#984ea3',
                     '#ff7f00','#ffff00','#a65628','#f781bf','#999999')
                     
    col12<-c('#e31a1c','#fb9a99','#33a02c','#b2df8a','#1f78b4','#a6cee3',
                      '#ff7f00','#fdbf6f','#6a3d9a','#cab2d6','#b15928','#ffff99')
                      
    col20<-c('#e6194B','#ff7f00','#ffff00','#aaffc3','#0000ff','#800000','#4daf4a',
                      '#ffc0cb','#42d4f4', '#911eb4','#808000','#000067','#f7a068',
                      '#026440','#f032e6','#9A6324','#9a245b','#909090','#ffff99','#000000')
                      
    
    if (tm<=9) {
      cur.col<-col9
    } else if(tm>9 & tm<=12) {
      cur.col<-col12
    } else {
      cur.col<-col20
    }
    
    
    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
      }
      return(fxn)
    }
    
    #print('start plot')
    
    ptitle<-paste0("MeSH terms by published year for journal: ",j.name)
    
    plot.df<-arrange(plot.df,PubYear,desc(Freq))
    
    plevel<-as.character(plot.df$MeSH[which(plot.df$PubYear==endy)])
    
    plot.df$MeSH<-factor(plot.df$MeSH,levels=c(plevel))
    
    
    ggplot(data = plot.df,
           aes(x = PubYear, y = Freq, alluvium = MeSH,
               node=factor(PubYear),stratum=MeSH,
               label=paste0(MeSH,'(',Freq,')'))) +
      xlab("Year of Publication")+
      ylab("Count")+
      scale_y_continuous(breaks= integer_breaks())+
      #scale_y_discrete() +
      theme(plot.title=element_text(hjust=0.5,size=18,face='bold'),
            panel.background=element_rect(fill='white'),
            #plot.margin = margin(5, 2, 192, 2, "pt"),
            panel.grid.major=element_line(color='grey',size=0.3),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5),
            legend.title=element_blank(),
            legend.justification=c(0,1),
            legend.position='top',
            legend.key.height = unit(1,'line'),
            legend.key.width  = unit(1,'line'),
            #legend.key=element_rect(fill='transparent'),
            legend.text=element_text(size=16,face='bold'),
            legend.text.align=0,
            axis.text.x=element_text(size=16, angle = -30, hjust = 0, face='bold',color='black'),
            axis.text.y=element_text(size=16,face='bold',color='black'),
            axis.title.x=element_text(size=18,face='bold'),
            axis.title.y=element_text(size=18,face='bold',angle=90)) +
      geom_alluvium(aes(fill = MeSH, colour = MeSH),
                    alpha = 0.8, width=0.2, curve_type = "xspline") +
      geom_stratum(aes(fill=MeSH),width=0.2,alpha=0.8,color='transparent')+
      geom_label(stat='stratum',size=3,min.y=min.count.forlabel,
                 color='black',fill='white',alpha=0.5,fontface='bold',
                 label.padding = unit(0.1, "lines"),label.size = 0) +
      scale_fill_manual(values=cur.col) +
      scale_color_manual(values=cur.col) +
      #facet_wrap(~ region, scales = "fixed") +
      ggtitle(ptitle)+ 
      guides(colour = guide_legend(override.aes = list(alpha = 0.8)))

    
  } else {
    #print('start else 1')
    # no MeSH term returned for the journal
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
            legend.position='top',
            legend.key.height = unit(1,'line'),
            legend.key.width  = unit(1,'line'),
            #legend.key=element_rect(fill='transparent'),
            legend.text=element_text(size=16,face='bold'),
            legend.text.align=0,
            axis.text.x=element_text(size=16, angle = -30, hjust = 0, face='bold',color='black'),
            axis.text.y=element_text(size=16,face='bold',color='black'),
            axis.title.x=element_text(size=18,face='bold'),
            axis.title.y=element_text(size=18,face='bold',angle=90)) +
      geom_blank()+
      #facet_wrap(~ region, scales = "fixed") +
      ggtitle("Found publicaitons, but no MeSH term returned.")
  }
  
} else {
  #print('start else 2')
  # no publications returned
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
          legend.position='top',
          legend.key.height = unit(1,'line'),
          legend.key.width  = unit(1,'line'),
          #legend.key=element_rect(fill='transparent'),
          legend.text=element_text(size=16,face='bold'),
          legend.text.align=0,
          axis.text.x=element_text(size=16, angle = -30, hjust = 0, face='bold',color='black'),
          axis.text.y=element_text(size=16,face='bold',color='black'),
          axis.title.x=element_text(size=18,face='bold'),
          axis.title.y=element_text(size=18,face='bold',angle=90)) +
    geom_blank()+
    #facet_wrap(~ region, scales = "fixed") +
    ggtitle("No publication returned, consider change journal name or publication year")
  
}
  
  




########################################################################
# input abstract output MeSH terms

input.text<-'The budding yeast Saccharomyces cerevisiae (S. cerevisiae) has relatively short lifespan and is genetically tractable, making it a widely used model organism in aging research. Here, we carried out a systematic and quantitative investigation of yeast aging with single-cell resolution through transcriptomic sequencing. We optimized a single-cell RNA sequencing (scRNA-seq) protocol to quantitatively study the whole transcriptome profiles of single yeast cells at different ages, finding increased cell-to-cell transcriptional variability during aging. The single-cell transcriptome analysis also highlighted key biological processes or cellular components, including oxidation-reduction process, oxidative stress response (OSR), translation, ribosome biogenesis and mitochondrion that underlie aging in yeast. We uncovered a molecular marker of FIT3, indicating the early heterogeneity during aging in yeast. '


ris.res <- EUtilsSummary(input.text,type="esearch", db="pubmed", retmax=100)


out<-capture.output(summary(ris.res))

out2<-gsub('(','',out[2],fixed=T)
out2<-gsub(')','',out2,fixed=T)
out2<-gsub('\"','',out2,fixed=T)

out2<-gsub(' AND ', ' OR ',out2, fixed=T)

out2<-strsplit(out2,' OR ',fixed=T)

out3<-unlist(out2)

# check if any MeSH term matched
mesh.count<-sum(grepl('[MeSH Terms]',out3, fixed=T))

if(mesh.count>0) {
  
  out4<-out3[grep('[MeSH Terms]',out3, fixed=T)]
  
  out5<-gsub('[MeSH Terms]','',out4,fixed=T)
  
  # remove duplicates
  out5<-unique(out5)
  
  write.csv(out5,'MeSH Terms from input.csv',row.names = F)
  
  #fileConn<-file("MeSH Terms from input.txt")
  #writeLines(out5, fileConn)
  #close(fileConn)
  
  ylen<-ceiling(length(out5)/5)
  xx <- rep(seq(1,5,1),times=ylen)
  yy <- rep(seq(ylen,1,-1),each=5)
  
  xx<-xx[1:length(out5)]
  yy<-yy[1:length(out5)]
  
  labelss <-out5
  df<-data.frame(xx,yy,labelss)
  
  ggplot()+
    theme(plot.title=element_text(hjust=0.5,size=10,face='bold'),
          panel.background=element_rect(fill='white'),
          #plot.margin = margin(5, 2, 192, 2, "pt"),
          panel.grid.major=element_blank(),
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
              color ='white', df, size=3,fontface='bold')+
    ggtitle("MeSH terms matched with input text")
  
} else {
  
  write.csv('No matched MeSH terms','MeSH Terms from input.csv',row.names = F)
  
  empty.df<- data.frame()
  
  ggplot(data=empty.df)+
    theme(plot.title=element_text(hjust=0.5,size=18,face='bold'),
          panel.background=element_rect(fill='white'),
          #plot.margin = margin(5, 2, 192, 2, "pt"),
          panel.grid.major=element_blank(),
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




