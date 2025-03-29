library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(patchwork)
library(gridExtra)
library(ggrepel)

file <- "SRA_graf_inf.csv"

data <- read.csv(file)
data <- data[data$ReleaseDate != 0, ]
data <- data[data$ReleaseDate > 2015, ]



grouped_data <- data %>%
  group_by(ReleaseDate, Country) %>%
  summarise(Run = n())




top_country <- c("Australia", "Canada", "China",'Denmark', 'Spain',"France", "Germany", "Switzerland", "United Kingdom", "United States",'Japan','Sweden')
top_country_all_for_year=grouped_data[grouped_data$Country %in% top_country, ]

ggplot(top_country_all_for_year, aes(x = ReleaseDate, y = Run, color = Country)) +
  geom_vline(xintercept = 2019, size = 0.5, colour = "#333333", linetype = "dashed",alpha=0.2)+  
  geom_vline(xintercept = 2020, size = 0.5, colour = "#333333", linetype = "dashed",alpha=0.2)+  
  geom_vline(xintercept = 2021, size = 0.5, colour = "#333333", linetype = "dashed",alpha=0.2)+  
  geom_vline(xintercept = 2022, size = 0.5, colour = "#333333", linetype = "dashed",alpha=0.2)+  
  geom_vline(xintercept = 2017, size = 0.5, colour = "#333333", linetype = "dashed",alpha=0.2)+  
  geom_vline(xintercept = 2018, size = 0.5, colour = "#333333", linetype = "dashed",alpha=0.2)+  
  geom_vline(xintercept = 2016, size = 0.5, colour = "#333333", linetype = "dashed",alpha=0.2)+  
  
  geom_segment(aes(x = 2016, y = 10000, xend = 2023, yend = 10000), 
               alpha = 0.2,
               size = 0.5,
               linetype = "dashed",
               
  ) +
  
  geom_segment(aes(x = 2016, y = 25000, xend = 2023, yend = 25000), 
               size = 0.5,
               alpha = 0.2,
               
               linetype = "dashed"
  ) +
  
  geom_segment(aes(x = 2016, y = 50000, xend = 2023, yend = 50000), 
               size = 0.5,
               alpha = 0.2,
               
               linetype = "dashed"
  ) +
  
  geom_segment(aes(x = 2016, y = 100000, xend = 2023, yend = 100000), 
               
               size = 0.5,
               alpha = 0.2,
               
               linetype = "dashed"
  ) +
  
  geom_segment(aes(x = 2016, y = 250000, xend = 2023, yend = 250000), 
               
               size = 0.5,
               alpha = 0.2,
               
               linetype = "dashed"
  ) +
  
  geom_segment(aes(x = 2016, y = 500000, xend = 2023, yend = 500000), 
               
               size = 0.5,
               alpha = 0.2,
               
               linetype = "dashed"
  ) +
  
  geom_segment(aes(x = 2016, y = 1000000, xend = 2023, yend = 1000000), 
               
               size = 0.5,
               alpha = 0.2,
               
               linetype = "dashed"
  ) +
  geom_segment(aes(x = 2016, y = 2500000, xend = 2023, yend = 2500000), 
               
               size = 0.5,
               alpha = 0.2,
               
               linetype = "dashed"
  ) +
  
  geom_line(size =0.8, alpha = 0.95,linetype = "solid") +
  
  geom_text_repel(
    aes(label = ifelse(ReleaseDate == 2023, as.character(Country), "")),
    
    size = 5,
    direction = "y",
    xlim = c(2023.6, NA),
    alpha = 3,
    segment.size = 1,
    segment.alpha = .9,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 123
  )+
  labs(title = "",
       x = "",
       y = "Runs") +
  geom_vline(xintercept = 2023, size = 0.7, colour = "#333333")+  
  
  scale_y_continuous(trans = "log10", 
                     breaks = c(10000,25000,50000,100000,250000, 500000, 1000000,2500000),
                     labels = c('10k','25k','50k','100k', '250k', '500k', '1000k', '2500k')) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2016, 2025.5), 
    breaks = seq(2016, 2023.5, by = 1)
  ) + 
  
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.ticks.y = element_line(size = 1), 
        axis.ticks.length = unit(0.1, "cm"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.7),
        panel.grid = element_blank(),        
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20, angle = 0),
        axis.title.y = element_text(size = 24)
  ) + 
  scale_color_manual(breaks = c("Australia", "Canada",'Denmark','Spain',"China", "France", "Germany", "Switzerland", "United Kingdom", "United States",'Japan','Sweden'),
                     values = c(
                       
                       "#fb2199", "#9e001a", "#f43d3f", "#0d9fde", "#a770b5", "#4b8fd1", "#ff9333", "#191a6b", "#b15928", "#135247", "#5fb32d", "#ff93d9"
                       
                     )) 

