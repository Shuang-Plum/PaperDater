# get journal data
journal_data<-function(ris.res, starty, endy) {
  
  
  # check summary to see if any result is returned
  res.len<-length(summary(ris.res))
  if (res.len>0) {
    
    #load('Data/jtiral.RData')
    
    q.res <- EUtilsGet(ris.res,type="efetch", db="pubmed")
    
    q.pmid<-PMID(q.res)
    q.pubyear<-YearPubDate(q.res)
    q.journal<-ISOAbbreviation(q.res)
    q.mesh<-Mesh(q.res)
    
    q.pubyear<-as.character(q.pubyear)
    
    dat.df<-data.frame('MeSH'=character(),'MeSHType'=character(),
                       'PMID'=character(),'PubYear'=numeric(),
                       'Journal'=character())
    
    q.pimd<-q.pmid[which(q.pubyear >= starty & q.pubyear <= endy)]
    q.journal<-q.journal[which(q.pubyear >= starty & q.pubyear <= endy)]
    q.mesh<-q.mesh[which(q.pubyear >= starty & q.pubyear <= endy)]
    q.pubyear<-q.pubyear[which(q.pubyear >= starty & q.pubyear <= endy)]
    
    if (sum(!is.na(q.mesh))>0) {
      
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
      
      row.names(dat.df)<-c(1:nrow(dat.df))
      
      
    } else {
      # no MeSH term returned for the journal
      
      dat.df<-'Found publicaitons but no associated MeSH terms \n Please change journal or year range'
      
    }
    
    
  } else {
    # no publications returned
    dat.df<-'No publication returned \n Check journal name'
    
    
  }
  
  return(dat.df)
  
}
