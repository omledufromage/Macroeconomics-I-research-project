library(pwt10)
data(pwt10.0)
attach(pwt10.0)
help("pwt10.0")
citation(package="pwt10")
library(ggplot2)
require(ggplot2)
# library(directlabels)

names(pwt10.0)
levels(country)

# Function that plots the graphs. That way we only need to call the function and change input countries.

capitagdp.labor <- function(country_1, country_2, date) {
  # Calculating GDP per capita and GDP per worker (in USDs, prices of 2017):
  # Creating a data.frame to use in the ggplot function (y refers to GDP, l refers to labor productivity):  
  x <- year[country==country_1]
  y1 <- rgdpe[country==country_1]/pop[country==country_1]
  y2 <- rgdpe[country==country_2]/pop[country==country_2]
  l1 <- rgdpe[country==country_1]/emp[country==country_1]
  l2 <- rgdpe[country==country_2]/emp[country==country_2]
  df <- data.frame(x,y1,y2,l1,l2)
  
  if(max(l1, na.rm = TRUE) > max(l2, na.rm = TRUE)) {
    y.max <- max(l1, na.rm = TRUE)
  } else {
    y.max <- max(l2, na.rm = TRUE)
  }
  
  legend.y1 = paste(country_1, " - GDP per capita")
  legend.y2 = paste(country_2, " - GDP per capita")
  legend.l1 = paste(country_1, " - GDP per worker")
  legend.l2 = paste(country_2, " - GDP per worker")
  
  lab <- c(legend.y1, legend.y2, legend.l1, legend.l2)
  
#  colors.1 = c("red2", "blue")
#  colors.2 = c("gold", "cyan")
#  colors.1 = c("#c21919", "#18c2c2")
#  colors.2 = c("#6dc218", "#6d18c2")
  colors.1 = c("#db2121", "#7ddb20")
  colors.2 = c("#20dbdb", "#7d20db")

  
  options(scipen=50)
  g <- ggplot(df, aes(x), frame=FALSE) 
  g <- g + geom_line(aes(y=y1, color=legend.y1), linewidth=0.7)
  g <- g + geom_line(aes(y=y2, color=legend.y2), linewidth=0.7)
  g <- g + geom_line(aes(y=l1, color=legend.l1), linewidth=0.7) 
  g <- g + geom_line(aes(y=l2, color=legend.l2), linewidth=0.7)
  
  #g <- g + geom_dl(aes(label = y2), method = list(dl.combine("first.points", "last.points")), cex = 0.8) 
  
  #g <- g + geom_text()
  # Avoid overlaps
  #g <- g + geom_text(check_overlap = TRUE)
  # Labels with background
  #g <- g  geom_label(mapping= TRUE)
  #g <- g + annotate(geom="text",x=2020, vjust="inward", hjust="inward", y=48000,label="GDP/worker Iceland",col="magenta1", family = "serif", size=3)
  
  # Plotting the trend lines for each curve.
  lm1 <- lm(log(y1)~x)
  a <- exp(coef(lm1)[1])
  b <- coef(lm1)[2]
  fitted.values.y1 <- a*exp(b*x)
  g <- g + geom_line(aes(y=fitted.values.y1, color=legend.y1), linetype = "dotted")
  
  lm1 <- lm(log(y2)~x)
  a <- exp(coef(lm1)[1])
  b <- coef(lm1)[2]
  fitted.values.y2 <- a*exp(b*x)
  g <- g + geom_line(aes(y=fitted.values.y2, color=legend.y2), linetype = "dotted")
  
  lm1 <- lm(log(l1)~x)
  a <- exp(coef(lm1)[1])
  b <- coef(lm1)[2]
  fitted.values.l1 <- a*exp(b*x)
  g <- g + geom_line(aes(y=fitted.values.l1, color=legend.l1), linetype = "dotted")
  
  if(max(fitted.values.l1) > y.max) {
    y.max <- max(fitted.values.l1)
  }
  
  lm1 <- lm(log(l2)~x)
  a <- exp(coef(lm1)[1])
  b <- coef(lm1)[2]
  fitted.values.l2 <- a*exp(b*x)
  g <- g + geom_line(aes(y=fitted.values.l2, color=legend.l2), linetype = "dotted")

  # Calculating the step for y-axis. 
  if(max(fitted.values.l2) > y.max) {
    y.max <- max(fitted.values.l2)
  }
  if(y.max >= 50000) {
    step.y <- 20000  
  } else if (y.max <= 20000) {
    step.y <- 2000   
  }
  else {
    step.y <- 10000
  }
  
 # g <- g + labs(color="",
                #title = "Real GDP per capita and GDP per worker (at chained PPPs)", 
                #subtitle=paste(country_1, "and", country_2)) + 
  g <- g + labs(color="") + 
    scale_color_manual(values = c(colors.1[1], colors.1[2], colors.2[1], colors.2[2])) + 
    scale_x_continuous(name = "Year", 
                       breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) + 
    scale_y_continuous(name="US Dollars (prices of 2017 at chained PPPs)", 
                       limits = c(0, y.max), 
                       breaks = seq(0,y.max, by = step.y))
  g <- g + theme(legend.position=c(0.255,0.958),
                 legend.text = element_text(size=12),
                 legend.key.size = unit(1, 'cm'),
#                 legend.justification=c(0,1),
    panel.grid.major = element_line(colour="gray92"), 
    panel.grid.minor = element_line(colour="gray96"), 
    panel.background = element_rect(fill = "grey98",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )
  g <- g + guides(col = guide_legend(ncol = 2))
  
  # Printing GDP ratio between the two countries and Labor Productivity ratio between the two countries.
  gdp.ratio <- y1/y2
  lp.ratio <- l1/l2
  #print(df)
  
  header <- c("GDP in 2017 US dollars (at chained PPPs)",
              "Population:",
              "GDP Per Person in 2017 US dollars (at chained PPPs)",
              "GDP Per Worker in 2017 US dollars (at chained PPPs)")
  c1 <- c(rgdpe[country == country_1 & year == date], 
          pop[country == country_1 & year == date]*1000000, 
          rgdpe[country == country_1 & year == date]/pop[country == country_1 & year == date], 
          rgdpe[country == country_1 & year == date]/emp[country == country_1 & year == date])
  c2 <- c(rgdpe[country == country_2 & year == date], 
          pop[country == country_2 & year == date]*1000000, 
          rgdpe[country == country_2 & year == date]/pop[country == country_2 & year == date], 
          rgdpe[country == country_2 & year == date]/emp[country == country_2 & year == date])
  final <- data.frame(header, c1, c2)
  
  print(final)
  print("Ratio of GDP Per Person:")
  print(gdp.ratio[date-1949])
  print("Ratio of GDP Per Worker:")
  print(lp.ratio[date-1949])
  
  #plotting the graphs
  print(g)
}

capitagdp.labor("Iceland", "Ethiopia", 2019)

