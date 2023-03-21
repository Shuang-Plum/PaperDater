# generate keyword data
keyword_data<-function(ris.res, starty, endy) {


  # check summary to see if any result is returned
  res.len<-length(summary(ris.res))
  
  if (res.len>0) {
    
    #load('Data/ktiral.RData')
    
    q.res <- EUtilsGet(ris.res,type="efetch", db="pubmed")
        


    #q.res <- EUtilsGet(ris.res,type="efetch", db="pubmed")
    q.pmid<-PMID(q.res)
    q.pubyear<-YearPubDate(q.res)
    q.journal<-ISOAbbreviation(q.res)
    
    dat.df<-as.data.frame(cbind(q.pmid,q.pubyear,q.journal))
    colnames(dat.df)<-c('PMID','PubYear','Journal')
    
    # otherwise PubYear is a factor
    dat.df$PubYear<-as.character(dat.df$PubYear)
    dat.df<-dat.df[which(dat.df$PubYear >= starty & dat.df$PubYear <= endy),]

    
    if (nrow(dat.df)>0) {

      dat.df$Journal<-as.character(dat.df$Journal)
      dat.df$PMID<-as.character(dat.df$PMID)
      
      dat.df<-dat.df[order(dat.df$PubYear, dat.df$Journal, dat.df$PMID),]
      
    } else {
      dat.df<-data.frame('PMID'=character(),'PubYear'=character(),'Journal'=character())
    }
    

  } else {
    
    dat.df<-data.frame('PMID'=character(),'PubYear'=character(),'Journal'=character())
    
  }
  
  return(dat.df)
  
  
}
