# plotting nests and false crawls

# set working directory
setwd("~/Projects/turtle_behavior")

# load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# load in nesting data
nests <- read.csv("data/nesting_attempts.csv")

# make date column date object
nests$date <- as.POSIXct(nests$date, format = '%m/%d/%Y %H:%M')

# remove rows with NAs and add day of the year and year columns
nests2 <- nests %>%
  na.omit() %>%
  mutate(day = yday(date), 
         year = year(date), 
         season = ifelse(date < '2015-06-01 00:00', '2014/15 Season ', 
                         ifelse(date < '2016-06-01 00:00', '2015/16 Season ', 
                                ifelse(date < '2017-06-01 00:00', '2017/18 Season', 
                                       ifelse(date < '2018-06-01 00:00', '2017/18 Season',
                                       '2018/19 Season')))))

# make ID column and year a factor
nests2$id <- as.factor(nests2$id)
nests2$year <- as.factor(nests2$year)

# # plot all on one plot, by day of the year
# ggplot(data = nests2, 
#        aes(x = day, y = id, color = season, shape = Nesting_attempt)) +
#   geom_point(size = 5) +
#   scale_shape_manual(values = c(1, 16)) +
#   ylab('Turtle ID') +
#   xlab('Day of the Year')
# 
# # plot all on one plot, by date
# ggplot(data = nests2, 
#        aes(x = date, y = id, color = season, shape = Nesting_attempt)) +
#   geom_point(size = 5) +
#   scale_shape_manual(values = c(1, 16)) +
#   ylab('Turtle ID') +
#   xlab('Day of the Year')

# separate facets for seasons
ggplot(data = nests2, 
       aes(x = date, y = id, shape = Nesting_attempt)) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(1, 16)) +
  ylab('Turtle ID') +
  xlab('Day of the Year') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size=15)) +
  theme(legend.text = element_text(size = 10)) +
  facet_grid(cols = vars(season)) +
  theme(strip.text.x = element_text(face = "bold.italic")) +
  scale_x_datetime(name = NULL, labels = label_date("%b"))
