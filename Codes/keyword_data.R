# generate keyword data
keyword_data<-function(ris.res) {
  
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
    

  } else {
    
    dat.df<-data.frame('PMID'=character(),'PubYear'=character(),'Journal'=character())
    
  }
  
  return(dat.df)
  
  
}