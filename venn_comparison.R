
library(devtools)
install_github("mustafapir/geneName")

library(gdata)
library(geneName)
library(UpSetR)
library(data.table)
source("venn_function.R")
source("commonUnique.R")

#read csv file
humangenes<-fread("Reyfman_Table1.csv", header=TRUE)

#Venn for human data
venn(humangenes, 14)

# Genes only in ciliated cells
humanciliary<-commonUnique(humangenes, "Ciliated Cells")

colnames(humanciliary)[1]<-"human"

# Convert (if) any gene synonyms to gene names
humanciliary<-gnameConverter(humanciliary, "human")


# Read mouse data
mouseciliarygenes<-read.csv("Du_et_2017_Mouse_scRNA_seq.csv", sep = ";", stringsAsFactors = FALSE)
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
celegansciliary<-fread("C_elegans_single_cell_1329_.csv", header = FALSE)

# Read C. elegans homology data
celeghum<-fread("C_elegans_Human_Homologs_20.01.2019 - UPDATED.csv")


# Convert from C. elegans to human gene name
celeghum1<-celeghum[,4:6]
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
write.table(allciliarylast, "allciliarylast1.csv", sep = ",", row.names = FALSE, quote = FALSE)


# Venn
venn(allciliary, 3)

