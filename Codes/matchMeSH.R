#input abstract output MeSH terms
matchMeSH<-function(input.text) {
  
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
    
    
    #fileConn<-file("MeSH Terms from input.txt")
    #writeLines(out5, fileConn)
    #close(fileConn)
    
    
  } else {
    
    out5<-character()
    
  }
  
  return(out5)
  
}