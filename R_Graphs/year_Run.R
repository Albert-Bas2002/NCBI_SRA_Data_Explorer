library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(tidyr)
library(knitr)
library(gt)
file <- "SRA_graf_inf.csv"
data <- read.csv(file)

unique_count <- function(column) {
  length(unique(column))
}

unique_counts <- sapply(data, unique_count)

print(unique_counts)
grouped_data <- data %>%
  group_by(ReleaseDate) %>%
  summarise(Run = n()) %>%
  mutate(Percentage = round((Run / sum(Run)) * 100, 1))


ggplot(grouped_data, aes(x = ReleaseDate, y = Run)) +
  geom_line(size = 1, alpha = 0.95, linetype = "solid",color='#8da0cb') +
  scale_y_continuous(trans='sqrt',
                     breaks = c(0,100000,500000,1500000,3000000,4500000,6000000),
                     labels = c("0","100k","500k","1500k","3000k","4500k","6000k"),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(2008, 2023.5), breaks = seq(2008, 2023, by = 1), expand = c(0, 0)) +
  labs(title = "",x='',y='Runs')+
  theme_classic()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 10),    
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_line(size = 0.2, color = 'black', linetype = "dashed"),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    legend.key.height = unit(0.45, "cm"),
    legend.position =c(0.3,0.22),
    
    legend.background = element_rect(color = "black", size = 0.2))+
  guides(color = guide_legend(keywidth = 2,keyheight = 1)) 
