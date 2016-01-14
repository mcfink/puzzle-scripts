lm_eqn = function(df){
    m = lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(a));                 
}

substrLeft <- function(x, n){
    substr(x, 1, nchar(x)-n+1)
}

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
          sep = "", collapse = " ")
}

lm_printer = function(noaa_df){
    lm_matrix <- matrix(nrow = 6, ncol=12)
    pos_count <- 0
    neg_count <- 0
    index_a <- 1
    for (g in levels(noaa_df$STATION_NAME)){
        h <- filter(noaa_df, STATION_NAME == g)
        index_b <- 1
        for (y in c(1:12)){
            print(g, y)
            i <- filter(h, month == index_b)
            q <- lm(MNTM ~ year, i)$coefficients[2]
            lm_matrix[index_a, index_b] <- q
            if (q<0){
                neg_count <- neg_count + 1
            } else {
                pos_count <- pos_count + 1
            }
            index_b <- index_b + 1
            print(q)
        }
        
        
        index_a <- index_a + 1
    }
    print(lm_matrix)
    print(pos_count)
    print(neg_count)
}

## weather data from NOAA
library(plyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)


setwd("~/Desktop/puzzles-starting-2015/puzzle-11-vt-weather")

a <- read.csv("weather_data.csv")
b <- tbl_df(a)
c <- mutate(b, my = as.character(DATE))
d <- mutate(c, year = as.numeric(substr(my, 1,4)), month = as.numeric(substr(my, 5,6)))
q <- mutate(d, station = as.character(STATION_NAME))
u <- filter(q, station != "GILMAN VT US")
v <- filter(u, station != "ROCHESTER VT US")
w <- filter(v, MNTM > -1000)
w <- mutate(w, month_name = month.abb[month])
w$month_name <- factor(w$month_name, levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


ylabel <- bquote('Mean Monthly Temperature ('^o*F*')')
n <- ggplot(w, aes(year, MNTM/10))
n + geom_point(aes(color=station_pname)) + coord_cartesian(ylim=c(0,80)) + facet_grid(station_pname~month_name) + stat_smooth(method=lm, se=FALSE, color="black")+ theme_bw(base_family = "Helvetica", base_size=16) +theme(legend.position="none") + ylab(ylabel) + xlab("Year") + ggtitle("Historical Temperature Trends in Vermont")+scale_y_continuous(breaks=c(10,30, 50, 70))


