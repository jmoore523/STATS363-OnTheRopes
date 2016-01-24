### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())
#dev.off()

# Load packages
library(plyr)
library(ggplot2)
library(lattice)

# Map paths
setwd('/Users/jmoore523/Dropbox/Graduate School/Q2 - Winter 2016/STATS263/Final Project/Code')
input <- file.path(".", "Input")

### Load & Clean up Data
# Load Data
ropedata <- read.csv('input/Rope Data.csv', header=TRUE)
ropedata <- subset(ropedata, ropedata$Additional.Length.with.Weight.Inches!='NA')

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

# Length/Diameter, Separated by Material
cotton <- subset(ropedata, Material=="Cotton")
qplot(x=Diameter, y=AddtlLen, data=cotton,
      main="Cotton Ropes",
      xlab = "Diameter",
      ylab = "Extension (Inches)")

manila <- subset(ropedata, Material=="Manila")
qplot(x=Diameter, y=AddtlLen, data=manila,
      main="Manila Hemp Ropes",
      xlab = "Diameter",
      ylab = "Extension (Inches)")

nylon <- subset(ropedata, Material=="Nylon")
qplot(x=Diameter, y=AddtlLen, data=nylon,
      main="Nylon Ropes",
      xlab = "Diameter",
      ylab = "Extension (Inches)")

polyproylene <- subset(ropedata, Material=="Polyproylene")
qplot(x=Diameter, y=AddtlLen, data=polyproylene,
      main="Polyproylene",
      xlab = "Diameter",
      ylab = "Extension (Inches)")

### ANOVA
rope.anova <- aov(AddtlLen ~ Material + Diameter + Material:Diameter + Block + Material:Block + Diameter:Block + Material:Diameter:Block, data = ropedata)
summary(rope.anova)
