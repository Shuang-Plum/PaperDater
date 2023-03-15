# prepare keyword data for downloading
keyword_data_dl<-function(dat.df){
  
  if (nrow(dat.df)>0) {
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
    
  } else {
    full.df<-'No publication returned. Consider change keywords.'
  }
  
  return(full.df)
  
}