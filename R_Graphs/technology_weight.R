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
grouped_data <- data %>%
  filter(Platform %in% c('PACBIO_SMRT', 'OXFORD_NANOPORE','ION_TORRENT')) %>%
  group_by(Model,Platform) %>%
  summarise(Sum_Size = sum(size_MB))
nulls <-data.frame(
  Platform = 'OXFORD_NANOPORE',
  Model = '.',
  Sum_Size=0
)
nulls_ <-data.frame(
  Platform = 'OXFORD_NANOPORE',
  Model = '..',
  Sum_Size=0
)

grouped_data <- rbind(grouped_data, nulls)
grouped_data <- rbind(grouped_data, nulls_)
grouped_data$Sum_Size_GB <- round(as.numeric(grouped_data$Sum_Size) / (1024*1024),0)
grouped_data$Model[grouped_data$Platform == "ION_TORRENT"] <- gsub("ION", "", grouped_data$Model[grouped_data$Platform == "ION_TORRENT"])
grouped_data$Model[grouped_data$Platform == "ION_TORRENT"] <- gsub("Ion", "", grouped_data$Model[grouped_data$Platform == "ION_TORRENT"])

grouped_data$Model <- gsub("PacBio", "", grouped_data$Model)

grouped_data$Model <- gsub("Torrent", "", grouped_data$Model)
grouped_data$Model <- ifelse(grouped_data$Sum_Size_GB  < 9 & grouped_data$Platform=='PACBIO_SMRT', 'Another PACBIO_SMRT', grouped_data$Model)
grouped_data$Model <- ifelse(grouped_data$Sum_Size_GB  < 2 & grouped_data$Platform=='ION_TORRENT', 'Another ION_TORRENT', grouped_data$Model)

tmp <-data.frame(
  Platform = 'PACBIO_SMRT',
  Model = 'Another PACBIO_SMRT',
  Sum_Size_GB=sum(grouped_data$Sum_Size_GB[grouped_data$Model == 'Another PACBIO_SMRT'])
)
grouped_data <- grouped_data %>%
  filter(Model != 'Another PACBIO_SMRT')

grouped_data <- rbind(grouped_data, tmp)

tmp <-data.frame(
  Platform = 'ION_TORRENT',
  Model = 'Another ION_TORRENT',
  Sum_Size_GB=sum(grouped_data$Sum_Size_GB[grouped_data$Model == 'Another ION_TORRENT'])
)
grouped_data <- grouped_data %>%
  filter(Model != 'Another ION_TORRENT')

grouped_data <- rbind(grouped_data, tmp)
unique(grouped_data$Model)
grouped_data$Model <- factor(grouped_data$Model, levels = rev(c("GridION",'MinION','PromethION','.','Sequel II',' RS II','Sequel',' RS','Sequel IIe',"Another PACBIO_SMRT" ,'..',"  PGM" ,"  Proton"," GeneStudio S5 Prime","  S5" ,"  S5 XL","Another ION_TORRENT")))
ggplot(grouped_data, aes(x = as.factor(Model), y = Sum_Size_GB,fill=Platform)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity",width = 0.7, position = position_dodge(width = 1), color = "black") +
  coord_flip() + 
  labs(x = "Platform models", y = "Total Size (TB)", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 10),   
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.6, 0.28),
    
    legend.background = element_rect(color = "black", size = 0.2))+
  scale_fill_manual("Technology",breaks=c('PACBIO_SMRT', 'OXFORD_NANOPORE','ION_TORRENT'),
                    values = c("#66c2a5", "#fc8d62", "#8da0cb"
                               
                    )) +
  scale_y_continuous(
    breaks = c(10,50,100,150,200),
    labels = c('10','50','100','150','200'),
    limits=c(0,250)) 

