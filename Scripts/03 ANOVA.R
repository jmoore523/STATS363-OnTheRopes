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

## ANOVA
rope.anova.log <- aov(lnAddtlLen ~ Block + Material + Diameter + Material:Diameter, data = ropedata)
summary(rope.anova.log)

# Calculate residuals
rope.predict.log <- predict(rope.anova.log)
rope.resid.log <- ropedata$lnAddtlLen - rope.predict.log

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
rope.resid.log1 <- rope.resid.log[ropedata$Block==1]
rope.resid.log2 <- rope.resid.log[ropedata$Block==2]
rope.resid.log3 <- rope.resid.log[ropedata$Block==3]
rope.resid.log4 <- rope.resid.log[ropedata$Block==4]
BlkOrder1 <- ropedata$BlkOrder[ropedata$Block==1]
BlkOrder2 <- ropedata$BlkOrder[ropedata$Block==2]
BlkOrder3 <- ropedata$BlkOrder[ropedata$Block==3]
BlkOrder4 <- ropedata$BlkOrder[ropedata$Block==4]

png(paste0(output,'/Order_In_Block.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 4.0, 2.1))
par(las=1)
plot(BlkOrder1, rope.resid.log1, 
     xlab="Order within block",
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
plot(as.numeric(ropedata$Block), rope.resid.log,
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
plot(as.numeric(ropedata$Material), rope.resid.log,
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
plot(as.numeric(ropedata$Diameter), rope.resid.log,
     xlab = "Diameter",
     xaxt = 'n',
     ylab = "Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4), labels=c("1/4","3/8","1/2","5/8"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()