
library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(patchwork)
library(gridExtra)

file <- "SRA_graf_inf.csv"
file_regions<-"world_regions.csv"
data_regions <- read.csv(file_regions)

data <- read.csv(file)
data_regions <-data_regions%>% rename('Country' = 'name')

data <- merge(data, data_regions, by = "Country")

grouped_data <- data %>%
  group_by(continent) %>%
  summarise(Run = n(),SumSizeTB = round(sum(size_MB)/(1024*1024)))

library(kableExtra)
kable(grouped_data, format = "html", align = "c")


#28124143 total
grouped_data <- grouped_data %>%
  mutate(Run_Percentage = (Run / sum(Run)) * 100)


total_Run_Percentage <- sum(grouped_data$Run_Percentage)


grouped_data$fraction <- grouped_data$Run_Percentage / sum(grouped_data$Run_Percentage)

grouped_data$ymax <- cumsum(grouped_data$fraction)

grouped_data$ymin <- c(0, head(grouped_data$ymax, n=-1))

grouped_data$labelPosition_y <- (grouped_data$ymax + grouped_data$ymin) / 2
grouped_data$labelPosition_x <- 2.3

grouped_data$label <- paste0(grouped_data$continent, "\n ", round(grouped_data$Run_Percentage,1), "%")

grouped_data$labelPosition_x[grouped_data$continent == "Asia"] <-4.7
grouped_data$labelPosition_x[grouped_data$continent == "North America"] <-2.1

grouped_data$labelPosition_x[grouped_data$continent == "Oceania"] <-4.6
grouped_data$labelPosition_y[grouped_data$continent == "Oceania"] <-0.965
grouped_data$labelPosition_y[grouped_data$continent == "South America"] <-0.996

grouped_data$labelPosition_x[grouped_data$continent == "Africa"] <-4.6
grouped_data$labelPosition_y[grouped_data$continent == "Africa"] <-0.01
grouped_data$labelPosition_y_line <- grouped_data$labelPosition_y
grouped_data$labelPosition_x_line <- 3.8

grouped_data$labelPosition_y_line[grouped_data$continent == "South America"] <-0.997
grouped_data$labelPosition_x_line[grouped_data$continent == "South America"] <-3.2
grouped_data$labelPosition_x_line[grouped_data$continent == "North America"] <-3.2
grouped_data$labelPosition_x_line[grouped_data$continent == "Europe"] <-3.2

grouped_data$labelPosition_y_line[grouped_data$continent == "Africa"] <-0.005
grouped_data$labelPosition_y_line[grouped_data$continent == "Oceania"] <-0.985



sum_run <- sum(grouped_data$Run)

ggplot(grouped_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=continent)) +
  geom_rect() +
  geom_segment(aes(x = labelPosition_x, xend = labelPosition_x_line, y = labelPosition_y, yend = labelPosition_y_line),
               color = "black", size = 0.3)+
  geom_label(aes(y=labelPosition_y,x=labelPosition_x, label=label), size=3) +
  scale_fill_manual(  values = c(
    "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f"))+  
  coord_polar(theta="y") +
  xlim(c(1, 5)) +
  theme_void() +
  theme(legend.position = "none")

