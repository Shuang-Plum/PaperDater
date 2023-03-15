# extract keyword query
print_query<-function(ris.res){
  q.temp<-capture.output(summary(ris.res))
  q.temp<-gsub('"',"'",q.temp,fixed=T)
  q.temp[1]<-paste0('<strong>',q.temp[1],'</strong>')
  q.temp[4]<-paste0('<strong>',q.temp[4],'</strong>')
  q.temp<-paste(q.temp, collapse = '<br/>')
  return(q.temp)
}