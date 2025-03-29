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
library(cowplot)
library(scales)


arctanh_trans <- function() {
  trans <- function(x) log((1 + x) / (1 - x)) / 2
  inv <- function(x) (exp(2 * x) - 1) / (exp(2 * x) + 1)
  trans_new("arctanh", trans, inv)
}

file <- "SRA_graf_inf.csv"
data <- read.csv(file)
data$Platform <- ifelse(data$Platform == "ILLUMINA", "ILLUMINA/CG",data$Platform)
data$Platform <- ifelse(data$Platform == "COMPLETE_GENOMICS", "ILLUMINA/CG", data$Platform)

grouped_data <- data %>% 
  filter(ReleaseDate != 0) %>%
  group_by(ReleaseDate, Platform) %>%
  summarise(Run = n()) %>%
  group_by(ReleaseDate) %>%
  mutate(Percent_of_Total = round((Run / sum(Run)) * 100, 1))


grouped_data <- data %>% 
  filter(ReleaseDate != 0) %>%
  group_by(ReleaseDate, Platform) %>%
  summarise(Sum_Size = sum(size_MB)) %>%
  group_by(ReleaseDate) %>%
  mutate(Percent_of_Total = round((Sum_Size / sum(Sum_Size)) * 100, 1))

grouped_data  <- grouped_data [nchar(grouped_data $Platform) <= 35, ]

grouped_data <- grouped_data %>%
  group_by(Platform) %>%
  arrange(ReleaseDate, .by_group = TRUE) %>%
  mutate(Change_from_Previous_Year = Percent_of_Total - lag(Percent_of_Total)) %>%
  ungroup()

grouped_data_f <- grouped_data %>%
  filter(Platform %in% c('ILLUMINA/CG','PACBIO_SMRT', 'OXFORD_NANOPORE','DNBSEQ','LS454','BGISEQ','ION_TORRENT'))



plot1 <- ggplot(grouped_data_f, aes(x = ReleaseDate, y = Change_from_Previous_Year, color = Platform)) +
  geom_line(size = 1, alpha = 0.95, linetype = "solid") +
  scale_y_continuous(limits = c(-5, 3.7),breaks = seq(-4, 3, by = 1),labels=c('-4%','-3%','-2%','-1%','0%','1%','2%','3%'), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2015, 2023.5), breaks = seq(2015, 2023, by = 1), expand = c(0, 0)) +
  labs(title = "Percentage increase/decrease compared to the previous year for Size generations",x='',y='',color='Technology')+
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
  guides(color = guide_legend(keywidth = 2,keyheight = 1)) +

  scale_color_manual(  values = c(
    "#5fbe5d", "#4b8fd1", "#a770b5", "#ff9333", "#f43d3f", "#b96d3b", "#ff93d1"))

print(plot1)


plot1 <- ggplot(grouped_data_f, aes(x = ReleaseDate, y = Change_from_Previous_Year, color = Platform)) +
  geom_line(size = 1, alpha = 0.95, linetype = "solid") +
  scale_y_continuous(limits = c(-5.8, 6),breaks = seq(-5, 6, by = 1),labels=c('-5%','-4%','-3%','-2%','-1%','0%','1%','2%','3%','4%','5%','6%'), expand = c(0, 0)) +
  scale_x_continuous(limits = c(2015, 2023.5), breaks = seq(2015, 2023, by = 1), expand = c(0, 0)) +
  labs(title = "Percentage increase/decrease compared to the previous year for Runs",x='',y='',color='Technology')+
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
    legend.key.width = unit(0.60, "cm"),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    legend.key.height = unit(0.45, "cm"),
    legend.position =c(0.47,0.2),
    
    legend.background = element_rect(color = "black", size = 0.2))+
  guides(color = guide_legend(keywidth = 2,keyheight = 1)) +
  
  scale_color_manual(  values = c(
    "#5fbe5d", "#4b8fd1", "#a770b5", "#ff9333", "#f43d3f", "#b96d3b", "#ff93d1"))

print(plot1)
