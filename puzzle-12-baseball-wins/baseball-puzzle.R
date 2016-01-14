## puzzle 12 or 11 - baseball teams and payroll
library(dplyr)
library(XML)

con = url("http://deadspin.com/2015-payrolls-and-salaries-for-every-mlb-team-1695040045")
htmlCode = readLines(con)
close(con)

baseball_raw <- htmlCode[301]
baseball <- htmlTreeParse(baseball_raw, useInternalNodes = TRUE)
baseball2 <- xpathSApply(baseball, "//td", xmlValue)
baseball3 <- matrix(baseball2, 31, 2, byrow = TRUE)
baseball4 <- tbl_df(as.data.frame(baseball3))
baseball5 <- slice(baseball4, 2:n())
baseball6 <- mutate(baseball5, name=substring(as.character(V1), 4))
baseball6 <- mutate(baseball6, payroll_mils = payroll/1000000, cost_per_win_mils = costperwin/1000000)

baseball6$wins <- c(92, 87, 78, 74, 84, 83, 85, 88, 63, 93, 100, 76, 97, 64, 76, 95, 81, 83, 68, 68, 90, 74, 67, 79, 98, 81, 68, 80, 86, 71)
baseball6$div <- factor(c("NL West", "AL East", "AL East", "AL Central", "NL West", "NL East", "AL West", "AL West", "NL East", "AL East", 
                          "NL Central", "AL West", "NL Central", "NL Central", "AL Central", "AL Central", "AL East", "AL Central", "NL Central", "NL West",
                          "NL East", "NL West", "NL East", "NL West", "NL Central", "AL Central", "AL West", "AL East", "AL West", "NL East"))
baseball6$conf <- factor(c("NL", "AL", "AL", "AL", "NL", "NL", "AL", "AL", "NL", "AL", 
                           "NL", "AL", "NL", "NL", "AL", "AL", "AL", "AL", "NL", "NL",
                           "NL", "NL", "NL", "NL", "NL", "AL", "AL", "AL", "AL", "NL"))
baseball6 <- mutate(baseball6, costperwin = payroll/wins)


baseball_graph <- ggplot(baseball6, aes(payroll, wins))
baseball_graph + geom_point(aes(color=cost_per_win_mils, shape=conf), size=4) + coord_cartesian(ylim=(c(60,105))) + theme_bw(base_family = "Helvetica", base_size=19) + xlab("payroll (millions USD)") + theme(legend.text=element_text(size=19), legend.position=c(.85,.2), legend.box.just="left") + scale_color_gradient("Cost Per Win (millions USD)", low="#432BA3", high="#F74156") + scale_shape("League")

