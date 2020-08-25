
library(readxl)
library(ggplot2)

awb<-read_xlsx("./data/AWB_Cilia_Morphology_Analysis.xlsx")

colnames(awb)[1]<-"Genotype"
awb1<-awb[,c(1,2)]
awb2<-awb[,c(1,3)]
awb3<-awb[,c(1,4)]
awb4<-awb[,c(1,5)]

colnames(awb1)[2]<-"Count"
colnames(awb2)[2]<-"Count"
colnames(awb3)[2]<-"Count"
colnames(awb4)[2]<-"Count"

awb1$Phenotype<-"Normal"
awb2$Phenotype<-"Fan"
awb3$Phenotype<-"Extra Branch"
awb4$Phenotype<-"Backward Projection"

awb1$Count<-as.numeric(awb1$Count)
awb2$Count<-as.numeric(awb2$Count)
awb3$Count<-as.numeric(awb3$Count)
awb3$Count<-as.numeric(awb3$Count)


awb1$Count_p<-(awb1$Count*100)/(awb3$Count + awb1$Count + awb2$Count + awb4$Count)
awb2$Count_p<-(awb2$Count*100)/(awb3$Count + awb1$Count + awb2$Count + awb4$Count)
awb3$Count_p<-(awb3$Count*100)/(awb3$Count + awb1$Count + awb2$Count + awb4$Count)
awb4$Count_p<-(awb4$Count*100)/(awb3$Count + awb1$Count + awb2$Count + awb4$Count)

awb_last<-rbind(awb1, awb2, awb3, awb4)
awb_last$Phenotype<-factor(awb_last$Phenotype, levels = unique(awb_last$Phenotype))
awb_last$Genotype<-factor(awb_last$Genotype, levels = c(awb1$Genotype))


# Plot for head

xsub0 <- ~ atop(paste(italic("wdr-31"),"(tm10423);",italic("elmod-3")))
xsub1 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("elmod-3"), ";Ex[ELMD-1(+)]"))
xsub2 <- ~ atop(paste(italic("elmd-1"),"(syb630); "), paste(italic("rpi-2"),";(ok1863)"))
xsub3 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("rpi-2")))
xsub4 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("elmd-1; rpi-2")))
xsub5 <- ~ atop(paste(italic("wdr-31"),"(syb1568); "), paste(italic("elmd-1; rpi-2"),";(ok1863)"))


ggplot(data = awb_last, aes(x = Genotype, y = Count_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = c(expression("Wild Type", 
                                                     paste(italic("wdr-31"),"(tm10423)"),
                                                     italic("wdr-31")(syb1568),
                                                     italic("elmd-1"),
                                                     italic("rpi-2")),
                                                     xsub0,xsub1,xsub2,xsub3,xsub4,xsub5
  )) +
  scale_y_continuous("Count (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))

