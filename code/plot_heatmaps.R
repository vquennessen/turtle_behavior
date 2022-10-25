# plot heatmaps of results data

# set working directory
setwd("~/Projects/turtle_behavior")

# load libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)

# load in data
behavior <- read.csv('data/behav_by_ID.csv')
internesting <- read.csv('data/subset_multiple_INs.csv')

# force object types
behavior$id <- as.factor(behavior$id)
behavior$behav <- as.factor(behavior$behav)

##### average step size by turtle ID and behavior ##############################
step_size <- behavior %>%
  group_by(id, behav) %>%
  summarize(avg_step_size = mean(step, na.rm = TRUE))

# heatmap for step size
ggplot(data = step_size, 
       aes(x = id, y = behav, fill = avg_step_size)) +
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(5, "viridis", rev = TRUE), 
                       name = 'Average \n Step \n Size') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab('Behavior') +
  xlab('Turtle')

##### average dive time by turtle ID and behavior ##############################
dive_time <- behavior %>%
group_by(id, behav) %>%
  summarize(avg_dive_time = mean(Avg_dive_time_min, na.rm = TRUE))

# heatmap for dive time 
ggplot(data = dive_time, 
       aes(x = id, y = behav, fill = avg_dive_time)) +
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(5, "viridis", rev = TRUE), 
                       name = 'Average \n Dive \n Time \n (min)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab('Behavior') +
  xlab('Turtle')

##### average surface time by turtle ID and behavior ###########################
surface_time <- behavior %>%
  group_by(id, behav) %>%
  summarize(avg_surface_time = mean(Avg_surface_time_sec, na.rm = TRUE))

# heatmap for dive time
ggplot(data = surface_time, 
       aes(x = id, y = behav, fill = avg_surface_time)) +
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(5, "viridis", rev = TRUE), 
                       name = 'Average \n Surface \n Time \n (sec)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab('Behavior') +
  xlab('Turtle')

##### put all three together ###################################################
new_step_size <- step_size %>%
  mutate(category = 'Step Size') %>%
  rename('Value' = 'avg_step_size')

new_dive_time <- dive_time %>%
  mutate(category = 'Dive Time') %>%
  rename('Value' = 'avg_dive_time')

new_surface_time <- surface_time %>%
  mutate(category = 'Surface Time') %>%
  rename('Value' = 'avg_surface_time')

all_data <- rbind(new_step_size, new_dive_time, new_surface_time)

# plot all three things together
ggplot(data = all_data, 
       aes(x = id, y = behav, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(5, "viridis", rev = TRUE)) +
  facet_grid(rows = vars(category)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab('Behavior') +
  xlab('Turtle')

##### plot them all together better ############################################

# heatmap for step size
step_size_plot <- ggplot(data = step_size, 
                         aes(x = id, y = behav, fill = avg_step_size)) +
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(5, "viridis", rev = TRUE), 
                       name = 'Average \n Step \n Size') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab('Step Size') +
  theme(axis.title.x = element_blank())

# heatmap for dive time
dive_time_plot <- ggplot(data = dive_time, 
                         aes(x = id, y = behav, fill = avg_dive_time)) +
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(5, "viridis", rev = TRUE), 
                       name = 'Average \n Dive \n Time \n (min)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab('Dive Time') +
  theme(axis.title.x = element_blank())

# heatmap for dive time
surface_time_plot <- ggplot(data = surface_time, 
                            aes(x = id, y = behav, fill = avg_surface_time)) +
  geom_tile() +
  scale_fill_gradientn(colors = hcl.colors(5, "viridis", rev = TRUE), 
                       name = 'Average \n Surface \n Time \n (sec)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ylab('Surface Time') +
  xlab('Turtle')

# put all three together
final_plot <- step_size_plot / dive_time_plot / surface_time_plot
final_plot
