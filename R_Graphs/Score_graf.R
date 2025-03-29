library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(patchwork)
library(gridExtra)
library(ggrepel)

file <- "Run_Score.csv"

data <- read.csv(file)

file <- "Completeness_Data.csv"

com_data <- read.csv(file)
merged_data <- merge(com_data, data, by = "Run")

ggplot(data, aes(x=Score)) +
  theme_classic()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 10),   
    legend.title = element_text(size = 12),  
    
    
    axis.ticks.x = element_line(size=0), 
    
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
  )+
  geom_histogram(fill="#a6bddb", color="black", alpha=0.5, bins = 25)+
  scale_y_continuous(trans='sqrt',
    breaks =c(1000000,3000000,6000000,10000000),
                     labels = c('1000k','3000k','6000k','10000k'),expand = c(0,0))+
  scale_x_continuous(
    breaks =  seq(0, 100, by = 5),
    labels = seq(0, 100, by = 5))+ labs(title = "Score distribution",x='Score',y='Number of Runs')



ggplot(merged_data, aes(x=as.factor(ScientificName), y=Score,fill=as.factor(ScientificName))) +
  geom_boxplot(alpha=0.7) +
  theme_classic()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 10),    
    legend.title = element_text(size = 12),   
    
    axis.ticks.x = element_line(size=0),  
    
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.height = unit(0.40, "cm"),
    legend.position =c(0.5,0.1),
    legend.background = element_rect(color = "black", size = 0.2)
  )+
  scale_fill_manual(values = c("0" = "#9ebcda", "1" = "#fdc086"), labels = c("Metadata", "No metadata"))
)