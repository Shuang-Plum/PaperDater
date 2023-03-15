color_pick<-function(m){
  
  col9<-c('#e41a1c','#377eb8','#4daf4a','#984ea3',
                   '#ff7f00','#ffff00','#a65628','#f781bf','#999999')
                   
  col12<-c('#e31a1c','#fb9a99','#33a02c','#b2df8a','#1f78b4','#a6cee3',
                    '#ff7f00','#fdbf6f','#6a3d9a','#cab2d6','#b15928','#ffff99')
                    
  col20<-c('#e6194B','#ff7f00','#ffff00','#aaffc3','#0000ff','#800000','#4daf4a',
                    '#ffc0cb','#42d4f4', '#911eb4','#808000','#000067','#f7a068',
                    '#026440','#f032e6','#9A6324','#9a245b','#909090','#ffff99','#000000')
                    
  
  if (m<=9) {
    cur.col<-col9
  } else if(m>9 & m<=12) {
    cur.col<-col12
  } else {
    cur.col<-col20
  }
  
  return(cur.col)
  
}