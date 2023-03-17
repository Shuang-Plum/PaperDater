# generate keyword data
keyword_data<-function(ris.res, starty, endy) {
  starty<-as.numeric(starty)
  endy<-as.numeric(endy)
  # check summary to see if any result is returned
  res.len<-length(summary(ris.res))
  
  if (res.len>0) {
    
    load('Data/ktiral.RData')
    
    #q.res <- EUtilsGet(ris.res,type="efetch", db="pubmed")
    q.pmid<-PMID(q.res)
    q.pubyear<-YearPubDate(q.res)
    q.journal<-ISOAbbreviation(q.res)
    
    dat.df<-as.data.frame(cbind(q.pmid,q.pubyear,q.journal))
    colnames(dat.df)<-c('PMID','PubYear','Journal')
    
    dat.df<-dat.df[which(dat.df$PubYear>=starty & dat.df$PubYear<=endy),]
    
    if (nrow(dat.df)>0) {
      
      dat.df<-dat.df[order(dat.df$PubYear, dat.df$Journal, dat.df$PMID),]
      
    }
    

  } else {
    
    dat.df<-data.frame('PMID'=character(),'PubYear'=character(),'Journal'=character())
    
  }
  
  return(dat.df)
  
  
}