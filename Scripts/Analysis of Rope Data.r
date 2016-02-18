### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())
#dev.off()

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
ropedata$DiameterNum <- numerators/denominators
ropedata$DiameterNum2 <- ropedata$DiameterNum^2

### Graphs
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

## Logged ANOVA
rope.anova.log <- aov(lnAddtlLen ~ Block + Material + Diameter + Material:Diameter, data = ropedata)
summary(rope.anova.log)

rope.anova.log.noblocks <- aov(lnAddtlLen ~ Material + Diameter + Material:Diameter, data = ropedata)
summary(rope.anova.log.noblocks)

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

## Tukey's test
TukeyHSD(rope.anova.log, which='Material', ordered = FALSE, conf.level = 0.95)
TukeyHSD(rope.anova.log, which='Diameter', ordered = FALSE, conf.level = 0.95)

## Pairwise comparisons with Bonferroni Correction


## Linear model
rope.lm <- lm(lnAddtlLen ~ Material + DiameterNum, data=ropedata)
summary(rope.lm)
