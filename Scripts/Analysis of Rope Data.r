### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())
#dev.off()

# Load packages
library(plyr)
library(ggplot2)
library(lattice)

# Map paths
setwd('/Users/jmoore523/Dropbox/Graduate School/Q2 - Winter 2016/STATS263/Final Project/STATS363-OnTheRopes')
input <- file.path(".", "Input")

### Load & Clean up Data
# Load Data
ropedata <- read.csv('input/Rope Data.csv', header=TRUE)
ropedata <- subset(ropedata, ropedata$Additional.Length.with.Weight.Inches!='NA')
ropedata$Block <- as.factor(ropedata$Block)

# Rename Columns
ropedata <- rename(ropedata, c("Rope.Type"="Material", "Additional.Length.with.Weight.Inches"="AddtlLen", "Order.Within.Block"="BlkOrder"))
summary(ropedata)

### Graphs
# Boxplot, by Material
plot(ropedata$Material, ropedata$AddtlLen)
plot(ropedata$Diameter, ropedata$AddtlLen)

# Average Length/Diameter, Grouped by Material
avglen.by.diam.mat <- aggregate(ropedata$AddtlLen, by=list(ropedata$Material, ropedata$Diameter), FUN=mean)
avglen.by.diam.mat <- rename(avglen.by.diam.mat, c("Group.1"="Material", "Group.2"="Diameter", "x"="AddtlLen"))

qplot(x=Diameter, y=AddtlLen, 
      data=avglen.by.diam.mat, 
      colour=Material, 
      main="Average Rope Extension",
      xlab = "Diameter",
      ylab = "Average Extension (Inches)") +
  geom_point() +
  geom_line(aes(group=Material))

## Non-Logged ANOVA
rope.anova.full <- aov(AddtlLen ~ Block + Material + Diameter + Material:Diameter, data = ropedata)
summary(rope.anova.full)

rope.anova.red <- aov(AddtlLen ~ Material + Diameter, data = ropedata)
summary(rope.anova.red)

rope.anova <- aov(AddtlLen ~ Material, data = ropedata)
summary(rope.anova)

# Calculate residuals
rope.predict <- predict(rope.anova)
rope.resid <- ropedata$AddtlLen - rope.predict

# Normal probability plot of residuals
qqnorm(rope.resid)
qqline(rope.resid)

# Residuals vs. Predicted values
plot(rope.predict, rope.resid)

# Residuals vs. Material type
plot(as.numeric(ropedata$Material), rope.resid)

# Residuals vs. Diameter
plot(as.numeric(ropedata$Diameter), rope.resid)

## Logged ANOVA
rope.anova.log <- aov(log(AddtlLen) ~ Material, data = ropedata)
summary(rope.anova.log)

# Calculate residuals
rope.predict.log <- predict(rope.anova.log)
rope.resid.log <- log(ropedata$AddtlLen) - rope.predict.log

# Normal probability plot of residuals
qqnorm(rope.resid.log)
qqline(rope.resid.log)

# Residuals vs. Predicted values
plot(rope.predict.log, rope.resid.log)

# Residuals vs. Material type
plot(as.numeric(ropedata$Material), rope.resid.log)

# Residuals vs. Diameter
plot(as.numeric(ropedata$Diameter), rope.resid.log)

## Tukey's test
TukeyHSD(rope.anova, which='Material', ordered = FALSE, conf.level = 0.95)

TukeyHSD(rope.anova.log, which='Material', ordered = FALSE, conf.level = 0.95)