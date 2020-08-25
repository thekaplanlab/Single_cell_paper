
library(readxl)
library(ggplot2)

awb_data<-function(data){
  
  colnames(data)[1]<-"Genotype"
  data1<-data[,c(1,2)]
  data2<-data[,c(1,3)]
  data3<-data[,c(1,4)]
  data4<-data[,c(1,5)]
  
  colnames(data1)[2]<-"Count"
  colnames(data2)[2]<-"Count"
  colnames(data3)[2]<-"Count"
  colnames(data4)[2]<-"Count"
  
  data1$Phenotype<-"Normal"
  data2$Phenotype<-"Fan"
  data3$Phenotype<-"Extra Branch"
  data4$Phenotype<-"Backward Projection"
  
  data1$Count<-as.numeric(data1$Count)
  data2$Count<-as.numeric(data2$Count)
  data3$Count<-as.numeric(data3$Count)
  data3$Count<-as.numeric(data3$Count)
  
  
  data1$Count_p<-(data1$Count*100)/(data3$Count + data1$Count + data2$Count + data4$Count)
  data2$Count_p<-(data2$Count*100)/(data3$Count + data1$Count + data2$Count + data4$Count)
  data3$Count_p<-(data3$Count*100)/(data3$Count + data1$Count + data2$Count + data4$Count)
  data4$Count_p<-(data4$Count*100)/(data3$Count + data1$Count + data2$Count + data4$Count)
  
  data_last<-rbind(data1, data2, data3, data4)
  data_last$Phenotype<-factor(data_last$Phenotype, levels = unique(data_last$Phenotype))
  data_last$Genotype<-factor(data_last$Genotype, levels = c(data1$Genotype))
  return(data_last)
}

awb<-read_xlsx("./data/AWB_Cilia_Morphology_Analysis.xlsx")
#awb_2<-read_xlsx("./data/AWB_Cilia_Morphology_Analysis.xlsx",2)
#awb_3<-read_xlsx("./data/AWB_Cilia_Morphology_Analysis.xlsx",3)
#awb_4<-read_xlsx("./data/AWB_Cilia_Morphology_Analysis.xlsx",4)

#awb_2t<-awb_data(awb_2)
awb<-awb_data(awb)

# Plot for first table

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

