
library(data.table)
library(dplyr)
library(pheatmap)
library(RColorBrewer)

cgenes<-fread("./data/ciliarygenes.tsv")
cgenes[which(cgenes$C_elegans == "K08D12.2")]<-NA
cgenes<-na.omit(cgenes)
df<-fread("./data/C_elegans_single_cell.csv")
df1<-df[which(df$symbol %in% cgenes$C_elegans),]
a<-cgenes[-which(cgenes$C_elegans %in% df1$symbol),]
df2<-df[match(cgenes$C_elegans, df$symbol),] %>%
  na.omit() %>%
  relocate(Ciliated_sensory_neurons, .after = Intestine) %>%
  relocate(Oxygen_sensory_neurons, .after = Intestine)

df2<-df2[,-1]
df3<-df2[,-1]
mm<-as.matrix(df3)
rownames(mm)<-df2$symbol
colnames(mm)[19]<-"flp-1_interneurons"

mypalette<-brewer.pal(7,"Blues")
annot<-data.frame(genes = cgenes$C_elegans)
annot$`Gene type` <- rep(c("Known ciliary genes", "Putative ciliary genes"), c(51, 26))
rownames(annot)<-annot$genes
annot1<-dplyr::select(annot,`Gene type`)
annotcol<-data.frame("Cell types" = factor(rep(c("Non-ciliated cells", "Ciliated cells"), c(25, 2))))
colnames(annotcol)<-"Cell types"
rownames(annotcol)<-colnames(mm)

ann_color<-list(
  `Cell types` = c(`Non-ciliated cells` = "#808080", `Ciliated cells` = "#FF007F"),
  `Gene type` = c(`Known ciliary genes` = "#FF8C00", `Putative ciliary genes` = "#00BFFF")
)

pheatmap(mm, cluster_cols = FALSE, cluster_rows = FALSE, show_rownames = TRUE, fontsize_row = 7, scale = "row", angle_col = 315, gaps_row = 52, 
         annotation_row = annot1, annotation_names_row = FALSE,
         annotation_col = annotcol, annotation_names_col = FALSE,
         annotation_colors = ann_color)



