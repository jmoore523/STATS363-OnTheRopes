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
keeps <- c("Diameter", "AddtlLen", "BlkOrder")
newdata <- read.csv('input/Addtl Cotton Data.csv', header=TRUE)
cottondata <- newdata[keeps]

## Clean compiled data
# Numeric diameter variable
numerators <- as.integer(substr(cottondata$Diameter,1,2))
denominators <- as.integer(substr(cottondata$Diameter,4,5))
cottondata$DiameterNum <- numerators/denominators * 16

# Generate log of AddtlLen variable
cottondata$lnAddtlLen <- log(cottondata$AddtlLen)

## Linear Regression
cotton.lm <- lm(lnAddtlLen ~ DiameterNum, data=cottondata)
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
png(paste0(output,'/Order_In_BlockReg2.png'))
par(mgp=c(2.5,1,0))
par(mar=c(5.1, 4.1, 4.0, 2.1))
par(las=1)
plot(cottondata$BlkOrder, rope.resid.logreg, 
     xlab="Order within block",
     xlim=c(1,20),
     ylab="Residuals",
     ylim=c(-.7,.7),
     cex.axis=1.35,
     cex.lab=1.5,
     pch=20)
par(xpd=TRUE)
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
