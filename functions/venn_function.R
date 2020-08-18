
venn = function(genes, numofcol) {
  
  mgenes = as.matrix(genes)        #set as matrix#
  allgenes = as.vector(mgenes)     #set as vector
  
  unallgenes= unique(allgenes, incomparables = FALSE, MARGIN = 1,
                     fromLast = FALSE)    #Unique gene list
  unallgenes = unallgenes[-which(unallgenes == "")]
  
  munallgenes = as.matrix(unallgenes)     
  
  boo = matrix(nrow = length(munallgenes), ncol = numofcol)    #empty matrix
  
  #fill in the matrix with boolean values
  
  m = 1
  for (m in 1:length(munallgenes))
  {
    k = 1
    for (k in 1:numofcol)
    {
      i = 1
      a = 0
      for (i in 1:length(mgenes[,k]))
      {
        if (munallgenes[m] == mgenes[i,k])
        {
          a = a+1
        }
        i = i+1
      }
      if (a > 0)
      {
        boo[m,k] = 1
      }
      else
      {
        boo[m,k] = 0
      }
      k = k+1
    }
    m = m+1
  }
  bood <- as.data.frame(boo)
  colnames(bood) = colnames(mgenes)
  rownames(bood) = munallgenes[,1]
  return(upset(bood, nsets = numofcol, sets.bar.color = "#56B4E9", #main.bar.color = "#0080FF", att.color = "cyan4", 
        order.by = "freq", empty.intersections = "on"))

}
