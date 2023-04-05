# return publication Mesh terms for year in range or the most recent 5000
author_search<-function(authorid, starty, endy) {
  
  # set time to be publication date
  ydat<-paste0('(',starty,':',endy,' [dp])')
  myquery<-paste0('(',authorid,') AND ',ydat)
  
  ris.res <- EUtilsSummary(myquery,type="esearch", db="pubmed", retmax=5000)
  
  # # save query summary to a txt file for reference
  # sink('Data/journal_query_summary.txt')
  # summary(ris.res)
  # sink()
  # # add newline by the end of text to avoid warning when open by read.delim
  # write("\n", file = "Data/journal_query_summary.txt",append = TRUE, sep = "\n")
  # 

  
  return(ris.res)
  
}