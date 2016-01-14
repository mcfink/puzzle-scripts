library(dplyr)
library(lubridate)
library(ggplot2)
library(grid)

a <- read.csv("640048.csv")
a <- tbl_df(a)

c <- mutate(a, my = as.character(DATE))
d <- mutate(c, year = as.numeric(substr(my, 1,4)), month = as.numeric(substr(my, 5,6)))
q <- mutate(d, station = as.character(STATION_NAME))
snow <- filter(q, TSNW > -1)

grouped_snow  <- group_by(snow, year, station)
early_snow  <- filter(grouped_snow, month > 9)
sum_early_snow  <- summarize(early_snow, sum(TSNW))

late_snow <- filter(grouped_snow, month < 6)
sum_late_snow <- summarize(late_snow, sum(TSNW))
sum_late_snow_corrected_year  <- mutate(sum_late_snow, corrected_year = year - 1)

names(sum_early_snow)[names(sum_early_snow)=="sum(TSNW)"] <- "earlysnow"
names(sum_late_snow_corrected_year)[names(sum_late_snow_corrected_year)=="sum(TSNW)"] <- "latesnow"

sum_late_snow_corrected_year <- sum_late_snow_corrected_year[,2:4]
sum_late_snow_corrected_year <- rename(sum_late_snow_corrected_year, year = corrected_year)

merged_snow  <- merge(sum_early_snow, sum_late_snow_corrected_year)
merged_snow2 <- merged_snow

levels(merged_snow2$sta_factor)[levels(merged_snow2$sta_factor)=="BOZEMAN MONTANA SU MT US"] <- "Bozeman, MT"
levels(merged_snow2$sta_factor)[levels(merged_snow2$sta_factor)=="DALE ENTERPRISE VA US"] <- "Dale Enterprise, VA"
levels(merged_snow2$sta_factor)[levels(merged_snow2$sta_factor)=="SAINT JOHNSBURY VT US"] <- "St. Johnsbury, VT"
levels(merged_snow2$sta_factor)[levels(merged_snow2$sta_factor)=="WILLOW CITY ND US"] <- "Willow City, ND"

levels(merged_snow2$sta_factor)[levels(merged_snow2$sta_factor)=="GUNNISON 3 SW CO US"] <- "Gunnison, CO"
levels(merged_snow2$sta_factor)[levels(merged_snow2$sta_factor)=="BUFFALO NIAGARA INTERNATIONAL AIRPORT NY US"] <- "Niagara Falls, NY"
levels(merged_snow2$sta_factor)[levels(merged_snow2$sta_factor)=="NOME MUNICIPAL AIRPORT AK US"] <- "Nome, AK"
levels(merged_snow2$sta_factor)[levels(merged_snow2$sta_factor)=="EDMONTON CITY CENTRE A CA"] <- "Edmonton, AB"

png("snow-puzzle-r-graphic.png", height = 1600, width=2400)
g  <- ggplot(merged_snow2, aes(earlysnow/10, latesnow/10))
g + geom_point(aes(color=sta_factor), alpha=.7, size=6) + coord_cartesian(ylim=c(0,300), xlim=c(0,200)) + 
    facet_wrap(~sta_factor, nrow=2) + theme_bw(base_family = "Helvetica", base_size=48) +theme(legend.position="none", plot.margin = unit(c(0.5,0.5,2,1), "cm"), axis.title.y=element_text(vjust=3), axis.title.x=element_text(vjust=-3)) + 
    ylab("Snow After Dec. 31 (cm)") + xlab("Snow Before Dec. 31 (cm)") + scale_x_continuous(breaks = c(50, 150)) + 
    stat_smooth(method=lm, se=FALSE, color="black") + geom_text(data=eq, aes( x= 100, y = 280, label = V1), parse = TRUE, inherit.aes = FALSE, size=12)
dev.off()

lm_eqn = function(df){
    m = lm(latesnow ~ earlysnow, df);
    eq <- substitute(~italic(r)^2~"="~r2, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
}



merged_snow3 <- select(merged_snow2, sta_factor, earlysnow, latesnow)

eq <-  ddply(merged_snow3,.(sta_factor),lm_eqn)