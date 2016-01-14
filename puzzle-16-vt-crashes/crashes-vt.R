library(maptools)
library(maps)
library(ggplot2)
library(plyr)
library(dplyr)
setwd("~/Desktop/puzzles-starting-2015/puzzle-16-vt-crashes/BoundaryOther_BNDHASH")

## read in VT town shape files
vt <- readShapeSpatial('Boundary_BNDHASH_region_towns.shp')
#plot(vt)

## read in VT population info
pop  <- read.csv('../2012pop.csv')

vt@data <- merge(vt@data, pop, by.x = "TOWNNAME", by.y = "NAME", all.x=TRUE)
#vt@data  <- arrange(vt@data, TOWNS_)

#plot(vt, col=rgb(256-(256*vt@data$X2012.population/42282),256,256, maxColorValue = 256))

## import crash data
crashes <- read.csv('../Vermont_Crash_Data_2012.csv')
crash_table <- table(crashes$Town)
crash_df  <- data.frame(crash_table)
crash_df <- mutate(crash_df, Var1 = ifelse(Var1 == "Buel's Gore", "Buels Gore", as.character(Var1)))
crash_df <- mutate(crash_df, Var1 = ifelse(Var1 == "Isle Lamotte", "Isle La Motte", as.character(Var1)))
crash_df <- mutate(crash_df, Var1 = ifelse(Var1 == "Mt. Holly", "Mount Holly", as.character(Var1)))
crash_df <- mutate(crash_df, Var1 = ifelse(Var1 == "Mt. Tabor", "Mount Tabor", as.character(Var1)))
crash_df <- mutate(crash_df, Var1 = ifelse(Var1 == "Rutland Town", "Rutland", as.character(Var1)))

## merge crash data with 
vt@data <- merge(vt@data, crash_df, by.x = "TOWNNAMEMC", by.y = "Var1", all.x=TRUE, all.y=TRUE)
vt@data  <- arrange(vt@data, TOWNS_)
vt@data <- vt@data[1:255,]
vt@data$Freq[is.na(vt@data$Freq)] <- 0
vt@data <- mutate(vt@data, acc_per_cap = Freq/X2012.population)
vt@data$acc_per_cap[is.nan(vt@data$acc_per_cap)] <- 0
vt@data <- mutate(vt@data, log_acc = log10(acc_per_cap))
vt@data$log_acc[is.infinite(vt@data$log_acc)] <- -4
vt@data <- mutate(vt@data, log_acc_mod = (log_acc + 4)/3.6)
png('question_log_map.png', height=1200, width=700)
plot(vt, col=rgb(196 + (60*vt@data$log_acc_mod), 196 - (196*vt@data$log_acc_mod), 196 - (196*vt@data$log_acc_mod), maxColorValue = 256))
dev.off()

vt@data <- mutate(vt@data, bacc_per_cap = ifelse(X2012.population > 1000, acc_per_cap, 0))


png('question_log_map.png', height=1200, width=700)
plot(vt, col=rgb(196 + (60*vt@data$log_acc_mod), 196 - (196*vt@data$log_acc_mod), 196 - (196*vt@data$log_acc_mod), maxColorValue = 256))
dev.off()

png('question_lin_map.png', height=1200, width=700)
plot(vt, col=rgb(196 + (60*vt@data$acc_per_cap/.375), 196 - (196*vt@data$acc_per_cap/.375), 196 - (196*vt@data$acc_per_cap/.375), maxColorValue = 256))
dev.off()

png('answer_lin_map.png', height=1200, width=700)
plot(vt, col=rgb(196 - (196*vt@data$bacc_per_cap/.0468), 196 - (196*vt@data$bacc_per_cap/.0468), 196 + (60*vt@data$bacc_per_cap/.0468), maxColorValue = 256))
dev.off()