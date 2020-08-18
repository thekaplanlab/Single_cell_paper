

commonUnique <- function(genes1, colname){
  genes1<-data.table(genes1)
  genelist<-genes1[, ..colname]
  collist<-c(colnames(genes1))
  collist<-collist[-which(collist %in% colname)]
  genelist1<-Reduce(intersect, genelist)
  
  for (i in collist){
    if (length(colname) < 2){
      genelist<-genelist[!which(genelist[[colname]] %in% genes1[[i]]),]
    }
    else {
      genelist1<-genelist1[-which(genelist1 %in% genes1[[i]])]
    }
  }
  genelist1<-data.frame(commons = genelist1, stringsAsFactors = FALSE)
  
  return(
    if (length(colname)<2) {
      genelist
    }
    else {genelist1}
  )
}
