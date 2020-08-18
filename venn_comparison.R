
library(devtools)
install_github("mustafapir/geneName")

library(gdata)
library(geneName)
library(UpSetR)
library(data.table)
library(dplyr)
library(readxl)
source("./functions/venn_function.R")
source("./functions/commonUnique.R")

#read csv file
humangenes<-fread("./data/Reyfman_Table1.csv", header=TRUE)

#Venn for human data
venn(humangenes, 14)

# Genes only in ciliated cells
humanciliary<-commonUnique(humangenes, "Ciliated Cells")

colnames(humanciliary)[1]<-"human"

# Convert (if) any gene synonyms to gene names
humanciliary<-gnameConverter(humanciliary, "human")


# Read mouse data
mouseciliarygenes<-read.csv("./data/Du_et_2017_Mouse_scRNA_seq.csv", sep = ";", stringsAsFactors = FALSE)
mouseciliarygenes<-mouseciliarygenes[,seq(1,17,2)]


# Venn for mouse data
venn(mouseciliarygenes, 9)

# Genes only in ciliated cells
mouseciliary<-commonUnique(mouseciliarygenes, "ï..Ciliated_cells")

# Convert (if) any mouse gene synonyms to mouse gene names
mouseciliaryhuman<-mousesynonymConverter(mouseciliary, "ï..Ciliated_cells")

# Convert mouse gene names to human gene names
mouseciliaryhuman<-mousegnameConverter(mouseciliaryhuman, "ï..Ciliated_cells")

# Get only the genes having human homolog
mouseciliaryhuman$human<-mouse_homology$Gene_name[match(mouseciliaryhuman$ï..Ciliated_cells, mouse_homology$Gene_name)]
mouseciliary1<-data.frame(mouse = na.omit(mouseciliaryhuman$human), stringsAsFactors = FALSE)


# Read C. elegans data
celegansciliary<-fread("./data/C_elegans_single_cell_1329_.csv", header = FALSE)

# Read C. elegans homology data
celeghum<-fread("./data/C_elegans_Human_Homologs_20.01.2019 - UPDATED.csv")


# Convert from C. elegans to human gene name
celeghum1<-celeghum[,4:6]
#celeghum1[which(celeghum1$Human_Symbol == "-"),]<-""
#celeghum1<-gnameConverter(celeghum1, "Human_Symbol")
colnames(celegansciliary)<-"Celegans_Symbol"
celeghum2<-celeghum1[which(celeghum1$Celegans_Symbol %in% celegansciliary$Celegans_Symbol),c(1,3)]
colnames(celegansciliary)<-"Celegans_Tag"
celeghum3<-celeghum1[which(celeghum1$Celegans_Tag %in% celegansciliary$Celegans_Tag),c(2,3)]
colnames(celeghum3)[1]<-"Celegans_Symbol"

celeghum4<-rbind(celeghum2, celeghum3)
celegansciliary<-celeghum4[,2]
colnames(celegansciliary)[1]<-"celegans"

# Convert (if) any gene synonyms to gene names
celegansciliary<-gnameConverter(celegansciliary, "celegans")


# Combine all 3 data
allciliary<-cbindX(humanciliary, mouseciliary1, celegansciliary)

# Convert any NA to space
allciliary$mouse<-replace(allciliary$mouse, is.na(allciliary$mouse), "")
allciliary$celegans<-replace(allciliary$celegans, is.na(allciliary$celegans), "")


# Find genes found only in "......"
onlyincelegans<-commonUnique(allciliary, "celegans")
onlyinhuman<-commonUnique(allciliary, "human")
onlyinmouse<-commonUnique(allciliary, "mouse")
mouseandhuman<-commonUnique(allciliary, c("human","mouse"))
humanandcelegans<-commonUnique(allciliary, c("human", "celegans"))
mouseandcelegans<-commonUnique(allciliary, c("mouse", "celegans"))
commoninall<-commonUnique(allciliary, c("human", "mouse", "celegans"))

# Combine all
allciliarylast<-cbindX(onlyinhuman, onlyinmouse, onlyincelegans, mouseandhuman, humanandcelegans, mouseandcelegans, commoninall)
#write.table(allciliarylast, "./data/allciliarylast1.csv", sep = ",", row.names = FALSE, quote = FALSE)


# Venn

venn(allciliary, 3)


# C elegans single cell, only human homologs

celeghum11<-celeghum[,6]
celeghum11[which(celeghum11$Human_Symbol == "-"),]<-""
celeghum11<-gnameConverter(celeghum11, "Human_Symbol")


# Mouse single cell, only human homologs

mouhum<-geneName::mouse_homology
mouhum<-gnameConverter(mouhum, "Gene_name")

# CiliaCarta

ciliacarta<-fread("./data/CiliaCarta_genes.csv", select = "Associated Gene Name")
colnames(ciliacarta)[1]<-"cilia_carta"
ciliacarta<-gnameConverter(ciliacarta, "cilia_carta")
celeghum11<-celeghum[,6]
celeghum11[which(celeghum11$Human_Symbol == "-"),]<-""
celeghum11<-gnameConverter(celeghum11, "Human_Symbol")
ciliacarta_celegans<-ciliacarta[which(ciliacarta$cilia_carta %in% celeghum11$Human_Symbol),]
ciliacarta_mouse<-ciliacarta[which(ciliacarta$cilia_carta %in% mouhum$Gene_name),]

# Gold standard

goldstandard<-fread("./data/Gold_standard_cilia_scgs.v1.tsv", select = "V2")
goldstandard<-goldstandard[-1]
colnames(goldstandard)<-"gold_standard"
goldstandard<-gnameConverter(goldstandard, "gold_standard")
goldstandard_celegans<-goldstandard[which(goldstandard$gold_standard %in% celeghum11$Human_Symbol),]
goldstandard_mouse<-goldstandard[which(goldstandard$gold_standard %in% mouhum$Gene_name),]

# Negative cilia genes

negativecilia<-read_xls("./data/Nevers_2017_NegativeGenesInsightsCiiaryGenes_SuppTable3 .xls") %>%
  select("Gene Name")
negativecilia<-data.table(negativecilia)
colnames(negativecilia)<-"negative_ciliary"
negativecilia<-gnameConverter(negativecilia, "negative_ciliary")
negativecilia_celegans<-negativecilia[which(negativecilia$negative_ciliary %in% celeghum11$Human_Symbol),]
negativecilia_mouse<-negativecilia[which(negativecilia$negative_ciliary %in% mouhum$Gene_name),]


# Venn for human single cell + cilia carta + gold standard + negative cilia

all1<-cbindX(humanciliary, ciliacarta, goldstandard, negativecilia)
all1$cilia_carta<-replace(all1$cilia_carta, is.na(all1$cilia_carta), "")
all1$gold_standard<-replace(all1$gold_standard, is.na(all1$gold_standard), "")
all1$negative_ciliary<-replace(all1$negative_ciliary, is.na(all1$negative_ciliary), "")

venn(all1, 4)


# Venn for mouse single cell + cilia carta + gold standard + negative cilia

all2<-cbindX(mouseciliary1, ciliacarta_mouse, goldstandard_mouse, negativecilia_mouse)
all2$mouse<-replace(all2$mouse, is.na(all2$mouse), "")
all2$cilia_carta<-replace(all2$cilia_carta, is.na(all2$cilia_carta), "")
all2$gold_standard<-replace(all2$gold_standard, is.na(all2$gold_standard), "")
all2$negative_ciliary<-replace(all2$negative_ciliary, is.na(all2$negative_ciliary), "")

venn(all2, 4)


# Venn for c elegans single cell + cilia carta + gold standard + negative cilia

all3<-cbindX(celegansciliary, ciliacarta_celegans, goldstandard_celegans, negativecilia_celegans)
all3$celegans<-replace(all3$celegans, is.na(all3$celegans), "")
all3$cilia_carta<-replace(all3$cilia_carta, is.na(all3$cilia_carta), "")
all3$gold_standard<-replace(all3$gold_standard, is.na(all3$gold_standard), "")
all3$negative_ciliary<-replace(all3$negative_ciliary, is.na(all3$negative_ciliary), "")

venn(all3, 4)


# Venn for all single cell + cilia carta + gold standard + negative cilia

all4<-cbindX(humanciliary, mouseciliary1, celegansciliary, ciliacarta, goldstandard, negativecilia)
all4$celegans<-replace(all4$celegans, is.na(all4$celegans), "")
all4$cilia_carta<-replace(all4$cilia_carta, is.na(all4$cilia_carta), "")
all4$gold_standard<-replace(all4$gold_standard, is.na(all4$gold_standard), "")
all4$negative_ciliary<-replace(all4$negative_ciliary, is.na(all4$negative_ciliary), "")
all4$mouse<-replace(all4$mouse, is.na(all4$mouse), "")
all4$human<-replace(all4$human, is.na(all4$human), "")

venn(all4, 6)

