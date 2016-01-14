## nat'l parks map example
setwd("~/Desktop/puzzles-starting-2015/puzzle-18-natlparks/temp/Current_Shapes/Data_Store/06-06-12_Posting")

## read in packages
library(dplyr)
library(maps)
library(maptools)
library(mapproj)
library(rgdal)
library(broom)
library(ggplot2)

## read in nat'l parks data

nps_raw <- readShapeSpatial('nps_boundary.shp')

nps <- subset(nps_raw, nps_raw@data$UNIT_TYPE == "National Park")
nps_continental <- subset(nps, nps@data$STATE != "AK" & nps@data$STATE != "HI")

## read shapefile
nps_ogr <- readOGR(dsn = ".", layer = "nps_boundary")

## convert to dataframe
nps_df <- tidy(nps_continental)

## create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="#e6e8ed"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

##ggplot() + geom_polygon(data = nps, aes(x=long, y=lat, group = group))

ggplot() + geom_polygon(data = nps_df, aes(x = long, y = lat, group=group))