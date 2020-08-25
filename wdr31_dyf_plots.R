
library(readxl)
library(ggplot2)
library(patchwork)

dyf<-read_xlsx("./data/Dye_assay.xlsx")
dyf_2<-read_xlsx("./data/Dye_assay.xlsx", 2)
dyf1<-dyf[,c(1,4,5)]
dyf2<-dyf[,c(1,6,7)]
dyf3<-dyf[,c(1,2,3)]

colnames(dyf1)<-dyf1[1,]
colnames(dyf2)<-dyf2[1,]
colnames(dyf3)<-dyf3[1,]
colnames(dyf1)[1]<-"Genotype"
colnames(dyf2)[1]<-"Genotype"
colnames(dyf3)[1]<-"Genotype"

dyf1<-dyf1[-1,]
dyf2<-dyf2[-1,]
dyf3<-dyf3[-1,]

dyf3$Phenotype<-"Normal"
dyf1$Phenotype<-"Partial"
dyf2$Phenotype<-"Dyf"

dyf1$Head<-as.numeric(dyf1$Head)
dyf2$Head<-as.numeric(dyf2$Head)
dyf3$Head<-as.numeric(dyf3$Head)

dyf1$Tail<-as.numeric(dyf1$Tail)
dyf2$Tail<-as.numeric(dyf2$Tail)
dyf3$Tail<-as.numeric(dyf3$Tail)

dyf3$Head_p<-(dyf3$Head*100)/(dyf3$Head + dyf1$Head + dyf2$Head)
dyf1$Head_p<-(dyf1$Head*100)/(dyf3$Head + dyf1$Head + dyf2$Head)
dyf2$Head_p<-(dyf2$Head*100)/(dyf3$Head + dyf1$Head + dyf2$Head)

dyf3$Tail_p<-(dyf3$Tail*100)/(dyf3$Tail + dyf1$Tail + dyf2$Tail)
dyf1$Tail_p<-(dyf1$Tail*100)/(dyf3$Tail + dyf1$Tail + dyf2$Tail)
dyf2$Tail_p<-(dyf2$Tail*100)/(dyf3$Tail + dyf1$Tail + dyf2$Tail)

dyf_last<-rbind(dyf3, dyf1, dyf2)
dyf_last$Phenotype<-factor(dyf_last$Phenotype, levels = unique(dyf_last$Phenotype))
dyf_last$Genotype<-factor(dyf_last$Genotype, levels = c(dyf1$Genotype))


xsub0 <- ~ atop(paste(italic("wdr-31"),"(tm10423);",italic("elmod-3")))
xsub1 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("elmod-3"), ";Ex[ELMD-1(+)]"))
xsub2 <- ~ atop(paste(italic("elmd-1"),"(syb630); "), paste(italic("rpi-2"),";(ok1863)"))
xsub3 <- ~ atop(paste(italic("wdr-31"),"(tm10423); "), paste(italic("elmd-1; rpi-2")))
xsub4 <- ~ atop(paste(italic("wdr-31"),"(tm10423); ",italic("elmd-1")), paste(italic("rpi-2"),";Ex[ELMD-1(+)]"))
xsub5 <- ~ atop(paste(italic("wdr-31"),"(syb1568); "), paste(italic("elmd-1; rpi-2"),";(ok1863)"))
xsub6 <- ~ atop(paste(italic("wdr-31"),"(syb1568);",italic("elmd-1")), paste(italic("rpi-2"),";Ex[WDR-31(+)]"))
xsub3 <- expression(paste(italic("wdr-31"),";syb1568"))


# Plot for head
dyf_head<-ggplot(data = dyf_last, aes(x = Genotype, y = Head_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = c(expression("Wild Type", 
                                                   paste(italic("wdr-31"),"(tm10423)",sep = "\n"),
                                                   italic("wdr-31")(syb1568),
                                                   italic("elmd-1"),
                                                   italic("rpi-2")),
                                          xsub0,xsub1,xsub2,xsub3,xsub4,xsub5,xsub6
                                          )) +
  scale_y_continuous("Head (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))


# Plot for tail
dyf_tail<-ggplot(data = dyf_last, aes(x = Genotype, y = Tail_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = c(expression("Wild Type", 
                                                     paste(italic("wdr-31"),"(tm10423)",sep = "\n"),
                                                     italic("wdr-31")(syb1568),
                                                     italic("elmd-1"),
                                                     italic("rpi-2")),
                                          xsub0,xsub1,xsub2,xsub3,xsub4,xsub5,xsub6
  )) +
  scale_y_continuous("Tail (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))

# Combine plots
combined <- dyf_head + dyf_tail & theme(legend.position = "right")
combined + plot_layout(ncol = 1, nrow = 2, guides = "collect")


# ****** Second Plot ******** #

dyf_21<-dyf_2[,c(1,4,5)]
dyf_22<-dyf_2[,c(1,6,7)]
dyf_23<-dyf_2[,c(1,2,3)]

colnames(dyf_21)<-dyf_21[1,]
colnames(dyf_22)<-dyf_22[1,]
colnames(dyf_23)<-dyf_23[1,]
colnames(dyf_21)[1]<-"Genotype"
colnames(dyf_22)[1]<-"Genotype"
colnames(dyf_23)[1]<-"Genotype"

dyf_21<-dyf_21[-1,]
dyf_22<-dyf_22[-1,]
dyf_23<-dyf_23[-1,]

dyf_23$Phenotype<-"Normal"
dyf_21$Phenotype<-"Partial"
dyf_22$Phenotype<-"Dyf"

dyf_21$Head<-as.numeric(dyf_21$Head)
dyf_22$Head<-as.numeric(dyf_22$Head)
dyf_23$Head<-as.numeric(dyf_23$Head)

dyf_21$Tail<-as.numeric(dyf_21$Tail)
dyf_22$Tail<-as.numeric(dyf_22$Tail)
dyf_23$Tail<-as.numeric(dyf_23$Tail)

dyf_23$Head_p<-(dyf_23$Head*100)/(dyf_23$Head + dyf_21$Head + dyf_22$Head)
dyf_21$Head_p<-(dyf_21$Head*100)/(dyf_23$Head + dyf_21$Head + dyf_22$Head)
dyf_22$Head_p<-(dyf_22$Head*100)/(dyf_23$Head + dyf_21$Head + dyf_22$Head)

dyf_23$Tail_p<-(dyf_23$Tail*100)/(dyf_23$Tail + dyf_21$Tail + dyf_22$Tail)
dyf_21$Tail_p<-(dyf_21$Tail*100)/(dyf_23$Tail + dyf_21$Tail + dyf_22$Tail)
dyf_22$Tail_p<-(dyf_22$Tail*100)/(dyf_23$Tail + dyf_21$Tail + dyf_22$Tail)

dyf2_last<-rbind(dyf_23, dyf_21, dyf_22)
dyf2_last$Phenotype<-factor(dyf2_last$Phenotype, levels = unique(dyf2_last$Phenotype))
dyf2_last$Genotype<-factor(dyf2_last$Genotype, levels = c(dyf_21$Genotype))



# Plot for head
dyf2_head<-ggplot(data = dyf2_last, aes(x = Genotype, y = Head_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = c(expression("Wild Type", 
                                                     paste(italic("wdr-31"),"(2-138del)"),
                                                     paste(italic("wdr-31"),"(139-261del)"),
                                                     paste(italic("wdr-31"),"(261del)"),
                                                     paste(italic("wdr-31"),"(2-138del); ", italic("elmd-1; rpi-2")),
                                                     paste(italic("wdr-31"),"(139-261del); ", italic("elmd-1; rpi-2")),
                                                     paste(italic("wdr-31"),"(261del); ", italic("elmd-1; rpi-2"))
  ))) +
  scale_y_continuous("Head (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))


# Plot for tail
dyf2_tail<-ggplot(data = dyf2_last, aes(x = Genotype, y = Tail_p, fill = Phenotype)) +
  geom_bar(stat = "identity", width = 0.9, color = "black") +
  scale_x_discrete("Genotype", labels = c(expression("Wild Type", 
                                                     paste(italic("wdr-31"),"(2-138del)"),
                                                     paste(italic("wdr-31"),"(139-261del)"),
                                                     paste(italic("wdr-31"),"(261del)"),
                                                     paste(italic("wdr-31"),"(2-138del); ", italic("elmd-1; rpi-2")),
                                                     paste(italic("wdr-31"),"(139-261del); ", italic("elmd-1; rpi-2")),
                                                     paste(italic("wdr-31"),"(261del); ", italic("elmd-1; rpi-2"))
  ))) +
  scale_y_continuous("Tail (%)") +
  theme(legend.text.align = 0, panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), text = element_text(size=12))

# Combine plots
combined2 <- dyf2_head + dyf2_tail & theme(legend.position = "right")
combined2 + plot_layout(ncol = 1, nrow = 2, guides = "collect")

