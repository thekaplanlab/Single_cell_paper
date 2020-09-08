

commonUnique <- function(genes1, colname){
  if (missing(colname)){
    n<-length(genes1)
    lst<-lapply(0:(2^n-1), FUN=function(x) head(as.integer(intToBits(x)),n))
    lst<-lst[-1]
    colnamex<-colnames(genes1)
    last_list<-data.frame(c(1,2,3))
    ref2<-seq(1:length(genes1))
    for (i in 1:length(lst)){
      ref<-unlist(lst[[i]])
      refx<-ref*ref2
      colname<-colnamex[refx]
      
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
          genelist1<-genelist1[!(genelist1 %in% genes1[[i]])]
        }
      }
      genelist1<-data.frame(commons = unique(genelist1), stringsAsFactors = FALSE)
      
      if (length(colname)<2) {
        genelist2<-data.frame(unique(genelist), stringsAsFactors = FALSE)
        
      }
      else {genelist2<-genelist1}
      
      colnames(genelist2)<-paste0(colname, collapse = "_")
      last_list<-cbindX(last_list, genelist2)
    }
    last_list<-last_list[,-1]
    return(last_list)
  }
  else {
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
        genelist1<-genelist1[!(genelist1 %in% genes1[[i]])]
      }
    }
    genelist1<-data.frame(commons = genelist1, stringsAsFactors = FALSE)
    
  }
  
  return(
    if (length(colname)<2) {
      genelist
    }
    else {genelist1}
  )
}
