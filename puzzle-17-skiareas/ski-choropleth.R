library(choroplethr)
library(choroplethrMaps)
library(dplyr)

library(ggplot2)
library(mapproj)

data(state.regions)
ski_states <- state.regions
continental_us = state.regions$region[!state.regions$region %in% c("alaska", "hawaii")]


ski_states$value <- c(9,1,0,4,29,30,5,0,0,0,0,0,4,17,6,2,0,0,0,13,1,18,44,18,2,0,15,6,3,0,26,2,8,4,51,6,0,11,27,1,0,3,1,0,14,5,24,14,31,5,10)

#ski_states$value <- c(rep(0,51))

ski_sans_abbs <- c("region", "value")
ski_final <- ski_states[ski_sans_abbs]

choro = StateChoropleth$new(ski_final)
choro$set_num_colors(1)
choro$show_labels = FALSE
choro$theme_clean()
choro$projection = "albers"
choro$render()

png("northeast-ski-areas.png", height = 1200, width = 1800)
choro2 <- StateChoropleth$new(ski_final)
choro2$show_labels = FALSE
choro2$set_zoom(c("new jersey","pennsylvania","new york","connecticut", "rhode island", "massachusetts", "maine", "new hampshire", "vermont"))
choro2$set_num_colors(1)
choro2$render()
dev.off()

png("all-ski-areas.png", height = 1200, width = 1800)
choro3 <- StateChoropleth$new(ski_final)
choro3$show_labels = FALSE
choro3$set_num_colors(1)
choro3$render()
dev.off()
