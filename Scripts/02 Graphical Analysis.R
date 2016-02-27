### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())

# Load packages
library(plyr)
library(ggplot2)
library(lattice)
library(stringr)

# Map paths
setwd('/Users/jmoore523/Dropbox/Graduate School/Q2 - Winter 2016/STATS263/Final Project/STATS363-OnTheRopes')
input <- file.path(".", "Input")
output <- file.path(".", "Output")
temp <- file.path(".", "Temp")

# Load data
load(paste0(temp,'/ropedata.RData'))

### Graphs
# All Data, by Material & Diameter
png(paste0(output,'/All_Data_Plot.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 2.5, 2.1))
qplot(x=Diameter, y=AddtlLen, 
      data=ropedata, 
      colour=Material, 
      xlab = "Diameter",
      ylab = "Extension (Inches)") +
  ylim(0,2) +
  geom_point() +
  geom_jitter(position = position_jitter(width = .004)) +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        legend.text = element_text(size=17),
        axis.text=element_text(size=17, colour="black"),
        axis.title=element_text(size=19),
        axis.title.x=element_text(vjust=-0.05),
        axis.title.y=element_text(vjust=0.7),
        panel.background=element_rect(fill = "white"),
        panel.border=element_rect(fill=NA, colour = "black", size=0.75))
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Boxplot, by Material
png(paste0(output,'/Material_Box_Plot.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
plot(ropedata$Material, ropedata$AddtlLen,
     xlab = "Material",
     xaxt = 'n',
     ylab = "Extension (Inches)",
     ylim=c(0,2),
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("Cotton","Manila","Nylon","Polypropylene"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

png(paste0(output,'/Diameter_Box_Plot.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
plot(ropedata$Diameter, ropedata$AddtlLen,
     xlab = "Diameter",
     xaxt = 'n',
     ylab = "Extension (Inches)",
     ylim=c(0,2),
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("1/4","3/8","1/2","5/8"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Average Length/Diameter, Grouped by Material
avglen.by.diam.mat <- aggregate(ropedata$AddtlLen, by=list(ropedata$Material, ropedata$Diameter), FUN=mean)
avglen.by.diam.mat <- rename(avglen.by.diam.mat, c("Group.1"="Material", "Group.2"="Diameter", "x"="AddtlLen"))

png(paste0(output,'/Avg_Rope_Extension.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 2.5, 2.1))
qplot(x=Diameter, y=AddtlLen, 
      data=avglen.by.diam.mat, 
      colour=Material, 
      xlab = "Diameter",
      ylab = "Average Extension (Inches)") +
  ylim(0,2) +
  geom_point() +
  geom_line(aes(group=Material)) +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        legend.text = element_text(size=17),
        axis.text=element_text(size=17, colour="black"),
        axis.title=element_text(size=19),
        axis.title.x=element_text(vjust=-0.05),
        axis.title.y=element_text(vjust=0.7),
        panel.background=element_rect(fill = "white"),
        panel.border=element_rect(fill=NA, colour = "black", size=0.75))
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()