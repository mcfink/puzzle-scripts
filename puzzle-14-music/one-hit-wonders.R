library(plyr)
library(dplyr)
library(lubridate)

library(ggplot2)
library(reshape2)
library(scales)

a  <- read.csv('tsort-chart-2-4-0003.csv')
b  <- tbl_df(a)
## change year column to number
c <- mutate(b, nyear = as.numeric(levels(year))[year])

eighties <- filter(c, nyear > 1979 & nyear <1990)
eightiesongs <- filter(eighties, as.character(type) == "song")
esbyartists <- group_by(eightiesongs, artist)
artistsums <- summarize(esbyartists, songs = length(score),total = sum(score), biggesthitscore = max(score), biggesthit = name[which.max(score)])
onehitwonderindex <- mutate(artistsums, bighitpct = biggesthitscore/total)
onehitwonderindex <- filter(onehitwonderindex, biggesthitscore > 1)
onehitwonderindex <- mutate(onehitwonderindex, ohwindex = bighitpct * biggesthitscore)
orderedohw <- arrange(onehitwonderindex, desc(ohwindex))

png("puzzle-question.png", height=1200, width=1800)
g  <- ggplot(onehitwonderindex, aes(x=total, y=bighitpct, size=biggesthitscore, color=ohwindex))
g + geom_point(alpha=.7) + theme_grey(base_family = "Mike-light", base_size=56) + 
    scale_color_gradient2(mid="yellow", high="red", low="blue", space="Lab", limits=c(0, 8), midpoint = 4,breaks=c(7,1), labels=c("High", "Low")) +
    labs(color = "One Hit Wonder Index", size= "Score for Biggest Hit") +
    scale_size_identity(guide = "legend") +
    theme(legend.position=c(.8, .8), 
          legend.direction="horizontal", 
          legend.box="vertical", 
          legend.text.align = 0.5,
          legend.title.align = 1, 
          legend.justification = "center", 
          legend.margin=unit(2, "cm")) + 
    guides(size = guide_legend(title.position = "top"), color=guide_colorbar(title.position = "top",title.hjust=0, barheight = 4, barwidth = 15)) +
    ggtitle("One Hit Wonders of the 1980s") +
    xlab("80s Wondrousness") + ylab("80s One Hit-ness")
    
dev.off()