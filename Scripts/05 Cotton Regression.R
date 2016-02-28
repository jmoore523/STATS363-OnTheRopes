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

## Compile new and old cotton data
load(paste0(temp,'/ropedata.RData'))
cottondata <- subset(ropedata, ropedata$Material=="Cotton")
keeps <- c("Block", "Diameter", "AddtlLen", "BlkOrder")
cottondata <- cottondata[keeps]

newdata <- read.csv('input/Addtl Cotton Data.csv', header=TRUE)
newdata$Block <- "5"
newdata <- newdata[keeps]

cottondata <- rbind(cottondata, newdata)

## Clean compiled data
# Factorize block
cottondata$Block <- factor(cottondata$Block)

# Numeric diameter variable
numerators <- as.integer(substr(cottondata$Diameter,1,2))
denominators <- as.integer(substr(cottondata$Diameter,4,5))
cottondata$DiameterNum <- numerators/denominators * 16

# Generate log of AddtlLen variable
cottondata$lnAddtlLen <- log(cottondata$AddtlLen)

## Linear Regression
cotton.lm <- lm(lnAddtlLen ~ Block + DiameterNum, data=cottondata)
summary(cotton.lm)

# Calculate residuals
rope.predict.logreg <- predict(cotton.lm)
rope.resid.logreg <- cottondata$lnAddtlLen - rope.predict.logreg

# Normal probability plot of residuals
png(paste0(output,'/QQPlotReg2.png'))
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
rope.resid.logreg1 <- rope.resid.logreg[cottondata$Block==1]
rope.resid.logreg2 <- rope.resid.logreg[cottondata$Block==2]
rope.resid.logreg3 <- rope.resid.logreg[cottondata$Block==3]
rope.resid.logreg4 <- rope.resid.logreg[cottondata$Block==4]
rope.resid.logreg5 <- rope.resid.logreg[cottondata$Block==5]
BlkOrderReg1 <- cottondata$BlkOrder[cottondata$Block==1]
BlkOrderReg2 <- cottondata$BlkOrder[cottondata$Block==2]
BlkOrderReg3 <- cottondata$BlkOrder[cottondata$Block==3]
BlkOrderReg4 <- cottondata$BlkOrder[cottondata$Block==4]
BlkOrderReg5 <- cottondata$BlkOrder[cottondata$Block==5]

png(paste0(output,'/Order_In_BlockReg2.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 4.0, 2.1))
par(las=1)
plot(BlkOrderReg1, rope.resid.logreg1, 
     xlab="Order within block",
     xlim=c(1,20),
     ylab="Residuals",
     ylim=c(-.7,.7),
     cex.axis=1.35,
     cex.lab=1.5,
     col="blue", pch=20)
points(BlkOrderReg2, rope.resid.logreg2, col="red", pch=20)
points(BlkOrderReg3, rope.resid.logreg3, col="darkorchid", pch=20)
points(BlkOrderReg4, rope.resid.logreg4, col="forestgreen", pch=20)
points(BlkOrderReg5, rope.resid.logreg5, col="pink", pch=20)
par(xpd=TRUE)
legend(3, .9, c("Block 1","Block 2", "Block 3","Block 4","Block5"), pch=c(20,20,20,20), col=c("blue","red","darkorchid","forestgreen","pink"), horiz=TRUE)
par(xpd=FALSE)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Residuals vs. Predicted values
png(paste0(output,'/Resid_V_FittedReg2.png'))
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
png(paste0(output,'/Resid_V_BlockReg2.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 3))
par(las=1)
plot(as.numeric(cottondata$Block), rope.resid.logreg,
     xlab = "Block",
     xaxt = 'n',
     ylab = "Residuals",
     cex.axis=1.35,
     cex.lab=1.5)
axis(1, at=c(1,2,3,4,5), labels=c("1","2","3","4","5"), cex.axis=1.35)
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# DFFITS
png(paste0(output,'/DFFITSReg2.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 1.0, 2.1))
par(las=1)
plot(dffits(cotton.lm),
     xlab = "Index",
     ylab = "DFFITS",
     cex.axis=1.35,
     cex.lab=1.5,
     pch=20,
     ylim=c(-1.2,1.2))
par(mgp=c(3,1,0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()
