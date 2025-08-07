# Setup
setwd('C:\\Users\\natew\\Documents\\All Files\\R Working Directory\\Ecological Seasonality')
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpattern)

# Get Average Weather Data
Weather <- read.csv("Daily Weather Data - NHW.csv")
head(Weather)
Weather$Day <- as.factor(yday(Weather$Date))
Averages <- data.frame(Day = 1:365)
Averages$SNOW <- NA
Averages$SNWD <- NA
Averages$TMAX <- NA
Averages$TMIN <- NA
Averages$TAVG <- NA
Averages$GDD <- NA
for(i in 1:365){
  temp <- filter(Weather, Weather$Day == i)
  Averages$SNOW[i] <- mean(temp$SNOW)
  Averages$SNWD[i] <- mean(temp$SNWD)
  Averages$TMAX[i] <- mean(temp$TMAX)
  Averages$TMIN[i] <- mean(temp$TMIN)
  Averages$TAVG[i] <- mean(temp$TAVG)
  Averages$GDD[i] <- mean(temp$GDD)
}
Averages$DGDD <- 0
for(i in 2:365){
  Averages$DGDD[i] <- Averages$GDD[i] - Averages$GDD[i-1]
}
Averages$DGD2 <- Averages$DGDD * 23

# Add Dates
Averages$Date <- as.Date(sprintf("%05d", Averages$Day), format = "%y%j")
Averages$Date <- format(Averages$Date, "%m/%d")


#Model of relationship
plot(Averages$DGDD)
plot(Averages$SNWD)
plot(Averages$DGD2 ~ Averages$SNWD)
model <- glm(DGDD ~ SNWD, family="poisson", data=Averages)
summary(model)

# Plot Weather vs Wolves
Seasons <- ggplot(Averages, aes(x = Date)) +
  #wolves
  geom_rect(aes(xmin = as.Date("2000-04-10"), xmax = as.Date("2000-11-07"), ymin = 14, ymax = Inf), alpha = 0.1, fill = "olivedrab") +
  geom_rect(aes(xmin = as.Date("2000-01-01"), xmax = as.Date("2000-04-10"), ymin = 14, ymax = Inf), alpha = 0.1, fill = "skyblue3") +
  geom_rect(aes(xmin = as.Date("2000-11-07"), xmax = as.Date("2000-12-30"), ymin = 14, ymax = Inf), alpha = 0.1, fill = "skyblue3") +
  geom_segment(aes(x = as.Date("2000-04-15"), y = 14, xend = as.Date("2000-04-15"), yend = Inf), linetype = "twodash", color = "gray1", size = 1.5) +

  #moose
  geom_rect(aes(xmin = as.Date("2000-03-11"), xmax = as.Date("2000-04-13"), ymin = 7, ymax = 14), alpha = 0.1, fill = "gray65") +
  geom_rect(aes(xmin = as.Date("2000-04-13"), xmax = as.Date("2000-05-06"), ymin = 7, ymax = 14), alpha = 0.1, fill = "yellow3") +
  geom_rect(aes(xmin = as.Date("2000-05-06"), xmax = as.Date("2000-10-17"), ymin = 7, ymax = 14), alpha = 0.1, fill = "olivedrab") +
  geom_rect(aes(xmin = as.Date("2000-10-17"), xmax = as.Date("2000-12-30"), ymin = 7, ymax = 14), alpha = 0.1, fill = "skyblue3") +
  geom_rect(aes(xmin = as.Date("2000-01-01"), xmax = as.Date("2000-03-11"), ymin = 7, ymax = 14), alpha = 0.1, fill = "skyblue3") +
  geom_segment(aes(x = as.Date("2000-05-12"), y = 7, xend = as.Date("2000-05-12"), yend = 14), linetype = "twodash", color = "gray1", size = 1.5) +

  #deer
  geom_rect(aes(xmin = as.Date("2000-04-24"), xmax = as.Date("2000-11-18"), ymin = -Inf, ymax = 7), alpha = 0.1, fill = "olivedrab") +
  geom_rect(aes(xmin = as.Date("2000-01-01"), xmax = as.Date("2000-04-24"), ymin = -Inf, ymax = 7), alpha = 0.1, fill = "skyblue3") +
  geom_rect(aes(xmin = as.Date("2000-11-18"), xmax = as.Date("2000-12-30"), ymin = -Inf, ymax = 7), alpha = 0.1, fill = "skyblue3") +
  geom_segment(aes(x = as.Date("2000-05-28"), y = -Inf, xend = as.Date("2000-05-28"), yend = 7), linetype = "twodash", color = "gray1", size = 1.5) +
  geom_rect_pattern(aes(xmin = as.Date("2000-04-04"), ymin = -Inf, xmax = as.Date("2000-05-02"), ymax = 7), pattern = "stripe", fill = NA, pattern_fill = "gray1", colour = NA, pattern_colour = "gray1", pattern_density = 0.25) +
  geom_rect_pattern(aes(xmin = as.Date("2000-10-10"), ymin = -Inf, xmax = as.Date("2000-11-28"), ymax = 7), pattern = 'stripe', fill = NA, pattern_fill = "gray1", colour = NA, pattern_colour = "gray1", pattern_density = 0.25) +
  geom_segment(aes(x = as.Date("2000-01-01"), y = 14, xend = as.Date("2000-12-30"), yend = 14), linetype = 'solid', color = 'gray1', size = 0.75) +
  geom_segment(aes(x = as.Date("2000-01-01"), y = 7, xend = as.Date("2000-12-30"), yend = 7), linetype = 'solid', color = 'gray1', size = 0.75) +

  #weather
  labs(x ="Julian Date", y = expression(paste(Delta, "GDD"))) +
  scale_x_date(date_breaks="2 month", date_labels="%b %d", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0.5)) +
  geom_line(aes(y = DGDD), color = "orange3", size = 1.5) + 
  annotate(geom="text", x=as.Date("2000-01-25"), y=17.5, label="Gray wolves", color="gray100") +
  annotate(geom="text", x=as.Date("2000-01-15"), y=10.5, label="Moose", color="gray100") +
  annotate(geom="text", x=as.Date("2000-02-02"), y=3.5, label="White-tailed deer", color="gray100") +
  theme(text = element_text(size = 12), panel.background = element_rect(fill = "grey100", colour = NA), panel.grid = element_blank())
Seasons
ggsave(plot = Seasons, width = 9, height = 4.5, dpi = 800, filename = "Figure 4 - V5.jpg")
