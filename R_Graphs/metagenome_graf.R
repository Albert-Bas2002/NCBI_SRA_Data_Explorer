library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(patchwork)
library(gridExtra)
library(ggrepel)

file <- "data_organism_class_plus_metagenome.csv"

data <- read.csv(file)
sum(data$size_MB, na.rm = TRUE) / (1024 * 1024)

data <- data[data$Class_organism == "Metagenome", ]
sum(data$ScientificName == "metagenome")

grouped_data <- data %>%
  group_by(Class_metagenome) %>%
  summarise(Run = n(),SumSizeTB=sum(size_MB)/(1024*1024))%>%
  filter(Run >= 102)   
grouped_data$Class_metagenome[grouped_data$Class_metagenome == "metagenome"] <- "Other Metagenome"

ordered_levels <- unique(grouped_data$Class_metagenome[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Metagenome"])
ordered_levels <- c( ordered_levels,"Other Metagenome")
grouped_data$Class_metagenome <- factor(grouped_data$Class_metagenome, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(Class_metagenome), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Type Metagenome", y = "Runs", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    limits=c(0,2000000),
    expand=c(0.01,0),
    breaks = c(0,500000,1000000,1500000,2000000),
    labels = c("0","500k","1000k",'1500k','2000K')  ) 


grouped_data <- data %>%
  group_by(Class_metagenome) %>%
  summarise(Run = n(),SumSizeTB=sum(size_MB)/(1024*1024))%>%
  filter(Run >= 102)   

grouped_data$Class_metagenome[grouped_data$Class_metagenome == "metagenome"] <- "Other Metagenome"

ordered_levels <- unique(grouped_data$Class_metagenome[order(grouped_data$SumSizeTB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Metagenome"])
ordered_levels <- c( ordered_levels,"Other Metagenome")
grouped_data$Class_metagenome <- factor(grouped_data$Class_metagenome, levels = rev(ordered_levels))



ggplot(grouped_data, aes(x = as.factor(Class_metagenome), y = SumSizeTB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeTB)), " TB ") ,y = SumSizeTB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Type Metagenome", y = "Total Size (TB)", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    limits=c(0,500),
    expand=c(0.01,0),
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450,500
),
    labels = c("0", "50", "100", "150", "200", "250", "300", "350", "400", "450","500")  ) 



data_class <- subset(data, Class_metagenome == "Plant")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)
data_class$ScientificName[data_class$ScientificName == "plant"] <- "Other plant metagenome"

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 1400, "Other plant metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other plant metagenome"])
ordered_levels <- c( ordered_levels,"Other plant metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + # Переворачиваем координаты
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Plant Metagenome", y = "Runs", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0, 2000, 10000, 35000, 90000, 180000),
    labels = c(0, 2000, 10000, 35000, 90000,  180000)  
     ) 


data_class <- subset(data, Class_metagenome == "Plant")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)
data_class$ScientificName[data_class$ScientificName == "plant"] <- "Other plant metagenome"

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse( SumSizeGB < 164, "Other plant metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other plant metagenome"])
ordered_levels <- c( ordered_levels,"Other plant metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() +
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " GB ") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Plant Metagenome", y = "Total Size (GB)", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),   
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0,150, 750, 2000, 9000,19500),
    labels = c(0,150, 750, 2000, 9000, 19500) 
  )





data_class <- subset(data, Class_metagenome == "Human")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)
data_class$ScientificName[data_class$ScientificName == "human"] <- "Other human metagenome"
data_class$ScientificName[data_class$ScientificName == "homo sapience"] <- "Other human metagenome"
data_class$ScientificName[data_class$ScientificName == "gut"] <- "human gut"


grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 5000, "Other human metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other human metagenome"])
ordered_levels <- c( ordered_levels,"Other human metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Human Metagenome", y = "Runs", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0, 5000, 35000, 120000,250000, 1000000),
   labels = c(0, 5000,  35000, 120000,250000, "1000000")  
  ) 


data_class <- subset(data, Class_metagenome == "Human")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)
data_class$ScientificName[data_class$ScientificName == "human"] <- "Other human metagenome"
data_class$ScientificName[data_class$ScientificName == "homo sapience"] <- "Other human metagenome"
data_class$ScientificName[data_class$ScientificName == "gut"] <- "human gut"


grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024*1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 2, "Other human metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other human metagenome"])
ordered_levels <- c( ordered_levels,"Other human metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() +
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " TB ") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Human Metagenome", y = "Total Size (TB)", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0,5, 15,60, 370),
    labels = c(0,5, 15,60, 370) 
  )







data_class <- subset(data, Class_metagenome == "Environment")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 10000, "Other environment metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other environment metagenome"])
ordered_levels <- c( ordered_levels,"Other environment metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Environment Metagenome", y = "Runs", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),  
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0, 5000, 50000, 170000, 700000),
    labels = c(0, 5000,  50000, 170000, "700000")  
  ) 



data_class <- subset(data, Class_metagenome == "Environment")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024*1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 4, "Other environment metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other environment metagenome"])
ordered_levels <- c( ordered_levels,"Other environment metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))





ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " TB ") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Environment Metagenome", y = "Total Size (TB)", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1), 
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),   
    legend.title = element_text(size = 12), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0,5, 15,55, 110),
    labels = c(0,5, 15,55, 110) 
  )









data_class <- subset(data, Class_metagenome == "Animal")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)
data_class$ScientificName <- gsub(" gut", "", data_class$ScientificName)

data_class$ScientificName[data_class$ScientificName == "animal"] <- "Other animal metagenome"


grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 8420, "Other animal metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other animal metagenome"])
ordered_levels <- c( ordered_levels,"Other animal metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() +
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Animal Metagenome", y = "Runs", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),   
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0, 5000, 35000, 120000,250000, 700000),
    labels = c(0, 5000,  35000, 120000,250000, "700000")  
  ) 

data_class <- subset(data, Class_metagenome == "Animal")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)
data_class$ScientificName <- gsub(" gut", "", data_class$ScientificName)

data_class$ScientificName[data_class$ScientificName == "animal"] <- "Other animal metagenome"


grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024*1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 2, "Other animal metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other animal metagenome"])
ordered_levels <- c( ordered_levels,"Other animal metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() +
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " TB ") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Animal Metagenome", y = "Total Size (TB)", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1), 
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0,5, 15,35,60, 280),
    labels = c(0,5, 15,35,60, 280) 
  )







data_class <- subset(data, Class_metagenome == "Artificial ecosystems")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)
data_class$ScientificName <- gsub(" gut", "", data_class$ScientificName)
data_class$ScientificName[data_class$ScientificName == "urban"] <- "Other artificial ecosystems metagenome"
data_class$ScientificName[data_class$ScientificName == "fermentation"] <- "food fermentation"



grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 1000, "Other artificial ecosystems metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other artificial ecosystems metagenome"])
ordered_levels <- c( ordered_levels,"Other artificial ecosystems metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Artificial ecosystems Metagenome", y = "Runs", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),    
    legend.title = element_text(size = 12),   
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0, 5000, 35000, 120000,250000, 700000),
    labels = c(0, 5000,  35000, 120000,250000, "700000")  
  ) 

data_class <- subset(data, Class_metagenome == "Artificial ecosystems")
data_class$ScientificName <- gsub(" metagenome", "", data_class$ScientificName)
data_class$ScientificName <- gsub(" gut", "", data_class$ScientificName)
data_class$ScientificName[data_class$ScientificName == "urban"] <- "Other artificial ecosystems metagenome"
data_class$ScientificName[data_class$ScientificName == "fermentation"] <- "food fermentation"



grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 200, "Other artificial ecosystems metagenome", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other artificial ecosystems metagenome"])
ordered_levels <- c( ordered_levels,"Other artificial ecosystems metagenome")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " GB ") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Artificial ecosystems Metagenome", y = "Total Size (GB)", title = "") +
  theme_minimal()+
  theme(
    axis.ticks.y = element_line(size = 1),  
    axis.ticks.length = unit(-0.1, "cm"),
    plot.title = element_text(size = 25),
    legend.text = element_text(size = 12),   
    legend.title = element_text(size = 12),  
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    
    axis.text.x = element_text(size =15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    expand=c(0.01,0),
    breaks = c(0,500,3500,10000, 50000),
    labels = c(0,500,3500,10000, 50000) 
  )







