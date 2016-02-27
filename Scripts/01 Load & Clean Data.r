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

## Save data
save(ropedata, file=paste0(temp,'/ropedata.RData'))