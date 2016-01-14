library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)

a <- read.csv("640048.csv")
a <- tbl_df(a)

c <- mutate(a, my = as.character(DATE))
d <- mutate(c, year = as.numeric(substr(my, 1,4)), month = as.numeric(substr(my, 5,6)))
e <- filter(d, STATION_NAME == "SAINT JOHNSBURY VT US")
h <- filter(e, year >1949)
k <- filter(h, TSNW > -1)
j <- group_by(k, month)
essential_snow_data  <- select(h, month, year, TSNW)
monthly_means <- summarize(j, mean(TSNW))

el_nino <- read.table("raw_el_nino.txt")
mel_nino <- matrix(el_nino[,1], ncol=13, byrow = TRUE)

## deleting wrapped values:
mel_nino[67,11:13] <- NA

colnames(mel_nino)<- mel_nino[1,]
row.names(mel_nino)<- mel_nino[,1]

mel_nino <- mel_nino[,-1]
mel_nino <- mel_nino[-1,]
class(mel_nino) <- "numeric"

del_nino <- data.frame(mel_nino)

tel_nino <- tbl_df(del_nino)
rel_nino  <- rename(tel_nino, "1" = DJF, "2" = JFM, "3" = FMA, "4" = MAM, "5" = AMJ, "6" = MJJ, "7" = JJA, "8" = JAS, "9" = ASO, "10" = SON, "11" = OND, "12" = NDJ)

row.names(rel_nino)  <- c(1950:2015)
melted_nino <- melt(t(rel_nino))
melted_nino  <- tbl_df(melted_nino)
melted_nino  <- rename(melted_nino, month = Var1, year = Var2)


essential_snow_data  <- mutate(essential_snow_data, deviation = TSNW - monthly_means$'mean(TSNW)'[month])
essential_snow_data <- filter(essential_snow_data, month <5 | month > 10)
#essential_snow_data <- cbind(essential_snow_data, melted_nino)
#melted_snowy_nino  <- filter(melted_nino, Var1 == "11" | Var1 == "December" | Var1 == "January" | Var1 == "February" | Var1 == "March" | Var1 == "April")
melted_snowy_nino <- filter(melted_nino, as.numeric(Var1) >10 | as.numeric(Var1) <5)

merged_snell_nino_data  <- merge(essential_snow_data, melted_nino)
merged_snell_nino_data <- mutate(merged_snell_nino_data, month_name = month.name[month])
merged_snell_nino_data$month_name <- factor(merged_snell_nino_data$month_name, levels = c("November", "December", "January", "February", "March", "April"))



## answer graph
png("mel-nino-puzzle-r-graphic.png", height = 1600, width=2400)
g <- ggplot(merged_snell_nino_data, aes(value, deviation/10))

g + geom_point(aes(color=abs(deviation)^.3), alpha=.8, size=6) + coord_cartesian(ylim=c(-50,100)) + facet_wrap(~month_name, nrow=2) + stat_smooth(method=lm, se=FALSE, color="black") +
    ylab("Deviation From Average Monthly Snowfall (cm), St. Johnsbury, VT") + xlab("Oceanic Nino Index (La Nina - negative, El Nino - positive)") +
    theme_bw(base_family = "Helvetica", base_size=48) + 
    theme(legend.position="none", plot.margin = unit(c(1,1,2,1), "cm"), axis.title.y=element_text(vjust=3), axis.title.x=element_text(vjust=-3))
dev.off()

## question graph
png("mel-nino-question-graphic.png", height=1600, width=2400)
h <- ggplot(merged_snell_nino_data, aes(value, deviation/10))

h + geom_point(aes(color=abs(deviation)^.3), alpha=.8, size=6) + coord_cartesian(ylim=c(-50,100)) + stat_smooth(method=lm, se=FALSE, color="black") +
    ylab("Deviation From Average Monthly Snowfall (cm), St. Johnsbury, VT") + xlab("Oceanic Nino Index (La Nina - negative, El Nino - positive)") +
    theme_bw(base_family = "Helvetica", base_size=48) + 
    theme(legend.position="none", plot.margin = unit(c(1,1,2,1), "cm"), axis.title.y=element_text(vjust=3), axis.title.x=element_text(vjust=-3))
dev.off()



