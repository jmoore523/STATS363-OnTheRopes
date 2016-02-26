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

### Load & Clean up Data
# Load Data
ropedata <- read.csv('input/Rope Data.csv', header=TRUE)
ropedata <- subset(ropedata, ropedata$Additional.Length.with.Weight.Inches!='NA')
ropedata$Rope.Type <- factor(ropedata$Rope.Type)
ropedata$Diameter <- factor(ropedata$Diameter, c(" 1/4", " 3/8", " 1/2", " 5/8"))
ropedata$Block <- factor(ropedata$Block)

# Rename Columns
ropedata <- rename(ropedata, c("Rope.Type"="Material", "Additional.Length.with.Weight.Inches"="AddtlLen", "Order.Within.Block"="BlkOrder"))
summary(ropedata)

## Create New Columns
# Log of output variable
ropedata$lnAddtlLen <- log(ropedata$AddtlLen)

# Numeric diameter variable
numerators <- as.integer(substr(ropedata$Diameter,1,2))
denominators <- as.integer(substr(ropedata$Diameter,4,5))
ropedata$DiameterNum <- numerators/denominators * 16

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

## Subset into data for ANOVA & Regression
ropedata.anova <- data.frame()
ropedata.reg <- data.frame()
set.seed(100)
for (diam in unique(ropedata$Diameter)) {
  for (mat in unique(ropedata$Material)) {
    subs <- subset(ropedata,ropedata$Diameter==diam & ropedata$Material==mat)
    subsnums <- sample(4,2)
    temp1 <- subs[subsnums,]
    temp2 <- subs[-subsnums,]
    ropedata.anova <- rbind(ropedata.anova,temp1)
    ropedata.reg <- rbind(ropedata.reg,temp2)
  }
}

## ANOVA
rope.anova.log <- aov(lnAddtlLen ~ Block + Material + Diameter + Material:Diameter, data = ropedata.anova)
summary(rope.anova.log)

# Calculate residuals
rope.predict.log <- predict(rope.anova.log)
rope.resid.log <- ropedata.anova$lnAddtlLen - rope.predict.log

# Normal probability plot of residuals
png(paste0(output,'/QQPlot.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
qqnorm(rope.resid.log, 
       main='',
       cex.axis=1.35,
       cex.lab=1.5)
qqline(rope.resid.log)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Order in Block
rope.resid.log1 <- rope.resid.log[ropedata.anova$Block==1]
rope.resid.log2 <- rope.resid.log[ropedata.anova$Block==2]
rope.resid.log3 <- rope.resid.log[ropedata.anova$Block==3]
rope.resid.log4 <- rope.resid.log[ropedata.anova$Block==4]
BlkOrder1 <- ropedata.anova$BlkOrder[ropedata.anova$Block==1]
BlkOrder2 <- ropedata.anova$BlkOrder[ropedata.anova$Block==2]
BlkOrder3 <- ropedata.anova$BlkOrder[ropedata.anova$Block==3]
BlkOrder4 <- ropedata.anova$BlkOrder[ropedata.anova$Block==4]

png(paste0(output,'/Order_In_Block.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 4.0, 2.1))
par(las=1)
plot(BlkOrder1, rope.resid.log1, 
     xlab="Order within Block",
     ylab="Residuals",
     ylim=c(-.7,.7),
     cex.axis=1.35,
     cex.lab=1.5,
     col="blue", pch=20)
points(BlkOrder2, rope.resid.log2, col="red", pch=20)
points(BlkOrder3, rope.resid.log3, col="darkorchid", pch=20)
points(BlkOrder4, rope.resid.log4, col="forestgreen", pch=20)
par(xpd=TRUE)
legend(4, .9, c("Block 1","Block 2", "Block 3","Block 4"), pch=c(20,20,20,20), col=c("blue","red","darkorchid","forestgreen"), horiz=TRUE)
par(xpd=FALSE)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Predicted values
png(paste0(output,'/Resid_V_Fitted.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
plot(rope.predict.log, rope.resid.log, 
     xlab="Fitted values",
     ylab="Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Block
png(paste0(output,'/Resid_V_Block.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 3))
par(las=1)
plot(as.numeric(ropedata.anova$Block), rope.resid.log,
     xlab = "Block",
     xaxt = 'n',
     ylab = "Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("1","2","3","3"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Material type
png(paste0(output,'/Resid_V_Material.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 3))
par(las=1)
plot(as.numeric(ropedata.anova$Material), rope.resid.log,
  xlab = "Material",
  xaxt = 'n',
  ylab = "Residuals",
  cex.axis=1.35,
  cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("Cotton","Manila","Nylon","Polypropylene"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Diameter
png(paste0(output,'/Resid_V_Diameter.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
plot(as.numeric(ropedata.anova$Diameter), rope.resid.log,
     xlab = "Diameter",
     xaxt = 'n',
     ylab = "Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("1/4","3/8","1/2","5/8"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

## Linear Regression
ropedata.reg$tempmat <- ropedata.reg$Material
ropedata.reg <- within(ropedata.reg, tempmat <- relevel(tempmat, ref = 2))
rope.lm <- lm(lnAddtlLen ~ Block + tempmat + DiameterNum + tempmat:DiameterNum, data=ropedata)
summary(rope.lm)

# Calculate residuals
rope.predict.logreg <- predict(rope.lm)
rope.resid.logreg <- ropedata.reg$lnAddtlLen - rope.predict.logreg

# Normal probability plot of residuals
png(paste0(output,'/QQPlotReg.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
qqnorm(rope.resid.logreg, 
       main='',
       cex.axis=1.35,
       cex.lab=1.5)
qqline(rope.resid.logreg)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Order in Block
rope.resid.logreg1 <- rope.resid.logreg[ropedata.reg$Block==1]
rope.resid.logreg2 <- rope.resid.logreg[ropedata.reg$Block==2]
rope.resid.logreg3 <- rope.resid.logreg[ropedata.reg$Block==3]
rope.resid.logreg4 <- rope.resid.logreg[ropedata.reg$Block==4]
BlkOrderReg1 <- ropedata.reg$BlkOrder[ropedata.reg$Block==1]
BlkOrderReg2 <- ropedata.reg$BlkOrder[ropedata.reg$Block==2]
BlkOrderReg3 <- ropedata.reg$BlkOrder[ropedata.reg$Block==3]
BlkOrderReg4 <- ropedata.reg$BlkOrder[ropedata.reg$Block==4]

png(paste0(output,'/Order_In_BlockReg.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 4.0, 2.1))
par(las=1)
plot(BlkOrderReg1, rope.resid.logreg1, 
     xlab="Order within Block",
     ylab="Residuals",
     ylim=c(-.7,.7),
     cex.axis=1.35,
     cex.lab=1.5,
     col="blue", pch=20)
points(BlkOrderReg2, rope.resid.logreg2, col="red", pch=20)
points(BlkOrderReg3, rope.resid.logreg3, col="darkorchid", pch=20)
points(BlkOrderReg4, rope.resid.logreg4, col="forestgreen", pch=20)
par(xpd=TRUE)
legend(4, .9, c("Block 1","Block 2", "Block 3","Block 4"), pch=c(20,20,20,20), col=c("blue","red","darkorchid","forestgreen"), horiz=TRUE)
par(xpd=FALSE)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Predicted values
png(paste0(output,'/Resid_V_FittedReg.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
plot(rope.predict.logreg, rope.resid.logreg, 
     xlab="Fitted values",
     ylab="Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Block
png(paste0(output,'/Resid_V_BlockReg.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 3))
par(las=1)
plot(as.numeric(ropedata.reg$Block), rope.resid.logreg,
     xlab = "Block",
     xaxt = 'n',
     ylab = "Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("1","2","3","3"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Material type
png(paste0(output,'/Resid_V_MaterialReg.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 3))
par(las=1)
plot(as.numeric(ropedata.reg$Material), rope.resid.logreg,
     xlab = "Material",
     xaxt = 'n',
     ylab = "Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("Cotton","Manila","Nylon","Polypropylene"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Diameter
png(paste0(output,'/Resid_V_DiameterReg.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
plot(as.numeric(ropedata.reg$Diameter), rope.resid.logreg,
     xlab = "Diameter",
     xaxt = 'n',
     ylab = "Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("1/4","3/8","1/2","5/8"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# DFFITS
png(paste0(output,'/DFFITSReg.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
plot(dffits(rope.lm),
     xlab = "Index",
     ylab = "DFFITS",
     cex.axis=1.35,
     cex.lab=1.5,
     pch=20)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

##

rope.anova.reg <- aov(lnAddtlLen ~ Block + Material + Diameter + Material:Diameter, data = ropedata.reg)
summary(rope.anova.reg)
