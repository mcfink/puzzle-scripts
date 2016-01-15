library(dplyr)
library(maps)
library(maptools)
library(mapproj)
library(rgdal)
library(broom)
library(ggplot2)
library(rgeos)
setwd("~/Desktop/puzzles-starting-2015/puzzle-21-dirt-roads/BoundaryOther_BNDHASH")

## read in VT town shape files
vt <- readShapeSpatial('Boundary_BNDHASH_region_towns.shp')

nwvt_roads <- read.csv('../northwest-vt-roads.csv')

vt@data <- merge(vt@data, nwvt_roads, by.x = "TOWNNAMEMC", by.y = "Town", all.x = TRUE, all.y = TRUE)
vt@data <- vt@data[order(vt@data$TOWNS_),]
vt@data$Percent.Dirt[is.na(vt@data$Percent.Dirt)] <- -1

##nwvt <- vt[vt@data$Percent.Dirt > -1,]

plot(vt[vt@data$TOWNNAMEMC %in% nwvt@data$TOWNNAMEMC,], col=rgb(.3*vt@data$Percent.Dirt[vt@data$TOWNNAMEMC %in% nwvt@data$TOWNNAMEMC],vt@data$Percent.Dirt[vt@data$TOWNNAMEMC %in% nwvt@data$TOWNNAMEMC],.7*vt@data$Percent.Dirt[vt@data$TOWNNAMEMC %in% nwvt@data$TOWNNAMEMC]))


plot(vt[vt@data$TOWNNAMEMC %in% nwvt@data$TOWNNAMEMC,], col=rgb(.4*vt@data$Total.Mileage[vt@data$TOWNNAMEMC %in% nwvt@data$TOWNNAMEMC],.3*vt@data$Total.Mileage[vt@data$TOWNNAMEMC %in% nwvt@data$TOWNNAMEMC],vt@data$Total.Mileage[vt@data$TOWNNAMEMC %in% nwvt@data$TOWNNAMEMC], maxColorValue = 136))


## ggplot version -- not working yet
vt <- readShapeSpatial('Boundary_BNDHASH_region_towns.shp')

nwvt_roads <- read.csv('../northwest-vt-roads.csv')

vt@data <- merge(vt@data, nwvt_roads, by.x = "TOWNNAMEMC", by.y = "Town", all.x = TRUE, all.y = TRUE)
vt@data$Percent.Dirt[is.na(vt@data$Percent.Dirt)] <- -1
vt@data <- vt@data[order(vt@data$TOWNS_),]
nwvt <- subset(vt, vt@data$Percent.Dirt > -0.5)

nwvt@data$id <- rownames(nwvt@data)
nwvt.points <- fortify(nwvt, region='id')
nwvt.df = join(nwvt.points, nwvt@data, by="id")

# create a blank ggplot theme
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


ggplot(nwvt.df) + aes(long, lat, group=group, fill=Percent.Dirt) + geom_polygon() + geom_path(color="white") + theme_opts + coord_equal() + scale_fill_continuous(low="#300080", high="#B010FF")

