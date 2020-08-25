
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
awb_2<-read_xlsx("./data/AWB_Cilia_Morphology_Analysis.xlsx",2)
awb_3<-read_xlsx("./data/AWB_Cilia_Morphology_Analysis.xlsx",3)
awb_4<-read_xlsx("./data/AWB_Cilia_Morphology_Analysis.xlsx",4)

awb_4<-awb_4[,1:5]

awb<-awb_data(awb)
awb_2<-awb_data(awb_2)
awb_3<-awb_data(awb_3)
awb_4<-awb_data(awb_4)


# Plot for table 1

xsub_plot1 <- ~ atop(paste(italic("wdr-31"),"(tm10423);",italic("elmod-3")))
xsub1_plot1 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("elmod-3"), ";Ex[ELMD-1(+)]"))
xsub2_plot1 <- ~ atop(paste(italic("elmd-1"),"(syb630); "), paste(italic("rpi-2"),";(ok1863)"))
xsub3_plot1 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("rpi-2")))
xsub4_plot1 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("elmd-1; rpi-2")))
xsub5_plot1 <- ~ atop(paste(italic("wdr-31"),"(syb1568); "), paste(italic("elmd-1; rpi-2"),";(ok1863)"))


awb_plot1<-ggplot(data = awb, aes(x = Genotype, y = Count_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = c(expression("Wild Type", 
                                                     paste(italic("wdr-31"),"(tm10423)"),
                                                     italic("wdr-31")(syb1568),
                                                     italic("elmd-1"),
                                                     italic("rpi-2")),
                                                     xsub_plot1,xsub1_plot1,xsub2_plot1,xsub3_plot1,xsub4_plot1,xsub5_plot1
  )) +
  scale_y_continuous("Count (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))



# Plot for table 2

awb_plot2<-ggplot(data = awb_2, aes(x = Genotype, y = Count_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = expression("Wild Type", 
                                                     paste(italic("wdr-31"),"(261-378del)"),
                                                     paste(italic("wdr-31"),"(261-378del); ", italic("elmd-1; rpi-2")),
                                                     paste(italic("wdr-31"),"(2-138del); ", italic("elmd-1; rpi-2")),
                                                     paste(italic("wdr-31"),"(139-261del); ", italic("elmd-1; rpi-2")),
  )) +
  scale_y_continuous("Count (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))


# Plot for table 3

xsub_plot3 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("elmd-1; rpi-2; ankr-26")))
awb_plot3<-ggplot(data = awb_3, aes(x = Genotype, y = Count_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = c(expression("Wild Type", 
                                                     italic("ankr-26")),
                                          xsub_plot3
  )) +
  scale_y_continuous("Count (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))


# Plot for table 4

xsub_plot4 <- ~ atop(paste(italic("wdr-31"),"(syb1568); ",italic("rpi-2"), ";"), "Ex[WDR-31(+)]")
awb_plot4<-ggplot(data = awb_4, aes(x = Genotype, y = Count_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = c(expression( 
                                                     paste(italic("wdr-31"),"(syb1568)"),
                                                     italic("rpi-2"),
                                                     paste(italic("wdr-31"),"(syb1568); ", italic("rpi-2"))),
                                          xsub_plot4
  )) +
  scale_y_continuous("Count (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))

# Draw all plots

awb_plot1
awb_plot2
awb_plot3
awb_plot4

