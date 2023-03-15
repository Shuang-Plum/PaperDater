# keyword search
keyword_search<-function(keywords, starty, endy) {
  
  # set time to be publication date
  ydat<-paste0('(',starty,':',endy,' [dp])')
  myquery<-paste0('(',keywords,') AND ',ydat)
  
  ris.res <- EUtilsSummary(myquery,type="esearch", db="pubmed", retmax=10000)
  
  # # save query summary to a txt file for reference
  # sink('Data/keywords_query_summary.txt')
  # summary(ris.res)
  # sink()
  # 
  # # add newline by the end of text to avoid warning when open by read.delim
  # write("\n", file = "Data/keywords_query_summary.txt",append = TRUE, sep = "\n")
  
  return(ris.res)
  
}