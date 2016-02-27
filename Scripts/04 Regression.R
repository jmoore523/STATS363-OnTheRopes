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

## Linear Regression
ropedata$tempmat <- ropedata$Material
ropedata <- within(ropedata, tempmat <- relevel(tempmat, ref = 4))
rope.lm <- lm(lnAddtlLen ~ Block + tempmat + DiameterNum + tempmat:DiameterNum, data=ropedata)
summary(rope.lm)

# Calculate residuals
rope.predict.logreg <- predict(rope.lm)
rope.resid.logreg <- ropedata$lnAddtlLen - rope.predict.logreg

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
rope.resid.logreg1 <- rope.resid.logreg[ropedata$Block==1]
rope.resid.logreg2 <- rope.resid.logreg[ropedata$Block==2]
rope.resid.logreg3 <- rope.resid.logreg[ropedata$Block==3]
rope.resid.logreg4 <- rope.resid.logreg[ropedata$Block==4]
BlkOrderReg1 <- ropedata$BlkOrder[ropedata$Block==1]
BlkOrderReg2 <- ropedata$BlkOrder[ropedata$Block==2]
BlkOrderReg3 <- ropedata$BlkOrder[ropedata$Block==3]
BlkOrderReg4 <- ropedata$BlkOrder[ropedata$Block==4]

png(paste0(output,'/Order_In_BlockReg.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 4.0, 2.1))
par(las=1)
plot(BlkOrderReg1, rope.resid.logreg1, 
     xlab="Order within block",
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
plot(as.numeric(ropedata$Block), rope.resid.logreg,
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
plot(as.numeric(ropedata$Material), rope.resid.logreg,
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
plot(as.numeric(ropedata$Diameter), rope.resid.logreg,
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
     pch=20,
     ylim=c(-1.2,1.2))
text(c(3.2, 49), c(1.05,1.1), labels=c("2","47"))
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()
