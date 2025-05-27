library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(patchwork)
library(gridExtra)
library(ggrepel)

file <- "data_organism_class_plus_metagenome.csv"

data <- read.csv(file)

data$size_MB[is.na(data$size_MB)] <- 0


grouped_data <- data %>%
  group_by(Class_organism) %>%
  summarise(Run = n(),SumSizeTB=sum(size_MB)/(1024*1024))%>%
  filter(Run >= 7)   


ordered_levels <- unique(grouped_data$Class_organism[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Unknown"])
ordered_levels <- c( ordered_levels,"Unknown")
grouped_data$Class_organism <- factor(grouped_data$Class_organism, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(Class_organism), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Type of Organism", y = "Runs", title = "") +
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
    breaks = c(0,500000,1500000,3000000,5000000,8500000),
    labels = c("0","500k",'1500k','3000K','5000K','8500K')  ) 


grouped_data <- data %>%
  group_by(Class_organism) %>%
  summarise(Run = n(),SumSizeTB=sum(size_MB)/(1024*1024))%>%
  filter(Run >= 7)   


ordered_levels <- unique(grouped_data$Class_organism[order(grouped_data$SumSizeTB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Unknown"])
ordered_levels <- c( ordered_levels,"Unknown")
grouped_data$Class_organism <- factor(grouped_data$Class_organism, levels = rev(ordered_levels))


ggplot(grouped_data, aes(x = as.factor(Class_organism), y = SumSizeTB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeTB)), " TB  ") ,y = SumSizeTB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Type of Organism", y = "Total Size (TB)", title = "") +
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
    breaks = c(0,150,600,1500,3000,17500),
    labels = c(0,150,600,1500,3000,17500)
    ) 






data_class <- subset(data, Class_organism == "Vertebrates")

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 5450, "Other Vertebrates", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Vertebrates"])
ordered_levels <- c( ordered_levels,"Other Vertebrates")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Vertebrates", y = "Runs", title = "") +
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
    axis.text.y = element_text(size = 12),
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
    breaks = c(0, 15000,  100000, 2500000,5000000,500000),
    labels = c("0", "15k", "100k", "2500k", "5000k","500k"
               
    )  
  ) 

data_class <- subset(data, Class_organism == "Vertebrates")

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeTB = round(sum(size_MB)/(1024*1024))) %>%
  mutate(ScientificName = ifelse(SumSizeTB < 20, "Other Vertebrates", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeTB = sum(SumSizeTB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeTB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Vertebrates"])
ordered_levels <- c( ordered_levels,"Other Vertebrates")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeTB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeTB)), " TB") ,y = SumSizeTB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Vertebrates", y = "Total Size (TB)", title = "") +
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
    breaks = c(0,100,1000, 2500, 9000,12500),
    labels = c(0, 100, 1000,2500, 9000, 12500) 
  )











data_class <- subset(data, Class_organism == "Plants")
data_class$ScientificName[data_class$ScientificName == "Zea mays subsp. mays"] <- "Zea mays"

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 7587, "Other Plants", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Plants"])
ordered_levels <- c( ordered_levels,"Other Plants")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() +
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Plants", y = "Runs", title = "") +
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
    axis.text.y = element_text(size = 12),
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
    breaks = c(0, 8000,  30000, 100000,170000,750000),
    labels = c("0", "8k", "30k", "100k", "170k","750k"
               
    )  
  ) 

data_class <- subset(data, Class_organism == "Plants")
data_class$ScientificName[data_class$ScientificName == "Zea mays subsp. mays"] <- "Zea mays"

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024*1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 18, "Other Plants", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Plants"])
ordered_levels <- c( ordered_levels,"Other Plants")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " TB") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Plants", y = "Total Size (TB)", title = "") +
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
    axis.text.y = element_text(size = 10),
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
    breaks = c(0,20,50,100,200,1100, 2500, 9000,12500),
    labels = c(0,20,50, 100,200, 1100,2500, 9000, 12500) 
  )






data_class <- subset(data, Class_organism == "Viruses")

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 1000, "Other Viruses", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

grouped_data$Run <-  ifelse(grouped_data$Run > 1000000, 
                            grouped_data$Run / 10, 
                            grouped_data$Run)

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Viruses"])
ordered_levels <- c( ordered_levels,"Other Viruses")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = ifelse(Run < 700000, paste0(format(round(Run / 1000)), "k"), "")), 
            position = position_dodge(width = 0.6), 
            hjust = 1.1, vjust = 0.5, size = 3, color = 'white') +  
  geom_text(aes(label = ifelse(Run > 700000, paste0(format(round(Run / 100)), "k"), "")), 
            position = position_dodge(width = 0.6), 
            hjust = 1.1, vjust = 0.5, size = 3, color = 'white') +  
  labs(x = "Viruses", y = "Runs", title = "") +
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
    axis.text.y = element_text(size = 12),
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
    breaks = c(0, 3000,  15000,50000,750000),
    labels = c("0", "3k", "15k",'50k',"7500k"
               
    )  
  ) 

data_class <- subset(data, Class_organism == "Viruses")

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 190, "Other Viruses", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

grouped_data$SumSizeGB <-  ifelse(grouped_data$Run > 1000000, 
                            grouped_data$SumSizeGB / 100, 
                            grouped_data$SumSizeGB)


ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Viruses"])
ordered_levels <- c( ordered_levels,"Other Viruses")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = ifelse(Run < 1000000, paste0(format(round(SumSizeGB)), " GB "), "")), 
            position = position_dodge(width = 0.6), 
            hjust = 1.1, vjust = 0.5, size = 3, color = 'white') +  
  geom_text(aes(label = ifelse(Run > 1000000, paste0(format(round(SumSizeGB *100)), " GB "), "")), 
            position = position_dodge(width = 0.6), 
            hjust = 1.1, vjust = 0.5, size = 3, color = 'white') + 
  labs(x = "Viruses", y = "Total Size (GB)", title = "") +
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
    axis.text.y = element_text(size = 10),
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
    breaks = c(0,200,1000, 2500, 12500),
    labels = c(0,200,1000, 2500, '1250K') 
  )








data_class <- subset(data, Class_organism == "Invertebrates")

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 2940, "Other Invertebrates", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))


ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Invertebrates"])
ordered_levels <- c( ordered_levels,"Other Invertebrates")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Invertebrates", y = "Runs", title = "") +
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
    axis.text.y = element_text(size = 12),
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
    breaks = c(0, 3000,  15000,50000,150000,300000),
    labels = c("0", "3k", "15k",'50k',"150k",'300k'
               
    )  
  ) 


data_class <- subset(data, Class_organism == "Invertebrates")

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024*1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 5, "Other Invertebrates", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))


ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Invertebrates"])
ordered_levels <- c( ordered_levels,"Other Invertebrates")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " TB") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Invertebrates", y = "Total Size (TB)", title = "") +
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
    axis.text.y = element_text(size = 10),
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
    breaks = c(0,5,15, 50, 125,280),
    labels = c(0,5,15, 50, 125,280) 
  )










data_class <- subset(data, Class_organism == "Bacteria")
rows_to_replace <- grep("salmonella", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Salmonella"
rows_to_replace <- grep("Streptococcus", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Streptococcus"

rows_to_replace <- grep("tuberculosis", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Tuberculosis"
rows_to_replace <- grep("staphylococcus", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Staphylococcus"

rows_to_replace <- grep("neisseria", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Neisseria"
rows_to_replace <- grep("shigella", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Shigella"
rows_to_replace <- grep("escherichia coli", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Escherichia coli"

rows_to_replace <- grep("pseudomonas", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Pseudomonas"

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 10000, "Other Bacteria", ScientificName))

grouped_data <- grouped_data %>%
  mutate(ScientificName = ifelse(ScientificName == "bacterium", "Other Bacteria", ScientificName))
grouped_data <- grouped_data %>%
  mutate(ScientificName = ifelse(ScientificName == "uncultured bacterium", "Other Bacteria", ScientificName))


grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Bacteria"])
ordered_levels <- c( ordered_levels,"Other Bacteria")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Bacteria", y = "Runs", title = "") +
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
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    breaks = c(0, 3000,  15000,50000,150000,300000,600000),
    labels = c("0", "3k", "15k",'50k',"150k",'300k','600k'
               
    )  
  ) 



data_class <- subset(data, Class_organism == "Bacteria")
rows_to_replace <- grep("salmonella", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Salmonella"
rows_to_replace <- grep("Streptococcus", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Streptococcus"

rows_to_replace <- grep("tuberculosis", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Tuberculosis"
rows_to_replace <- grep("staphylococcus", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Staphylococcus"

rows_to_replace <- grep("neisseria", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Neisseria"
rows_to_replace <- grep("shigella", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Shigella"
rows_to_replace <- grep("escherichia coli", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Escherichia coli"

rows_to_replace <- grep("pseudomonas", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Pseudomonas"

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024*1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 3, "Other Bacteria", ScientificName))

grouped_data <- grouped_data %>%
  mutate(ScientificName = ifelse(ScientificName == "bacterium", "Other Bacteria", ScientificName))
grouped_data <- grouped_data %>%
  mutate(ScientificName = ifelse(ScientificName == "uncultured bacterium", "Other Bacteria", ScientificName))


grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))

ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Bacteria"])
ordered_levels <- c( ordered_levels,"Other Bacteria")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " TB") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Bacteria", y = "Total Size (TB)", title = "") +
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
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    breaks = c(0,5,15, 50, 125,280),
    labels = c(0,5,15, 50, 125,280) 
  )












data_class <- subset(data, Class_organism == "Fungi")

rows_to_replace <- grep("cryptococcus neoformans", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Cryptococcus neoformans"
rows_to_replace <- grep("saccharomyces cerevisiae", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Saccharomyces cerevisiae"


grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 1000, "Other Fungi", ScientificName))

grouped_data <- grouped_data %>%
  mutate(ScientificName = ifelse(ScientificName == "Fungi", "Other Fungi", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))


ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Fungi"])
ordered_levels <- c( ordered_levels,"Other Fungi")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() +
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Fungi", y = "Runs", title = "") +
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
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    breaks = c(0, 3000,  15000,50000,150000,300000),
    labels = c("0", "3k", "15k",'50k',"150k",'300k'
               
    )  
  ) 


data_class <- subset(data, Class_organism == "Fungi")

rows_to_replace <- grep("cryptococcus neoformans", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Cryptococcus neoformans"
rows_to_replace <- grep("saccharomyces cerevisiae", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Saccharomyces cerevisiae"


grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024*1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 2, "Other Fungi", ScientificName))

grouped_data <- grouped_data %>%
  mutate(ScientificName = ifelse(ScientificName == "Fungi", "Other Fungi", ScientificName))

grouped_data <- grouped_data %>%
  mutate(ScientificName = ifelse(ScientificName == "[Candida] auris", "Candida auris", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))


ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Fungi"])
ordered_levels <- c( ordered_levels,"Other Fungi")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " TB") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Fungi", y = "Total Size (TB)", title = "") +
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
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    breaks = c(0,5,15, 50, 100,280),
    labels = c(0,5,15, 50, 100,280) 
  )






data_class <- subset(data, Class_organism == "Protists")
rows_to_replace <- grep("plasmodium falciparum", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Plasmodium falciparum"
rows_to_replace <- grep("toxoplasma gondii", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Toxoplasma gondii"


grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(Run < 1000, "Other Protists", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))


ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$Run)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Protists"])
ordered_levels <- c( ordered_levels,"Other Protists")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y =Run)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(Run / 1000)), "k"), y = Run), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Protists", y = "Runs", title = "") +
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
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    breaks = c(0, 3000,  15000,50000,150000,300000),
    labels = c("0", "3k", "15k",'50k',"150k",'300k'
               
    )  
  ) 




data_class <- subset(data, Class_organism == "Protists")
rows_to_replace <- grep("plasmodium falciparum", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Plasmodium falciparum"
rows_to_replace <- grep("toxoplasma gondii", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Toxoplasma gondii"
rows_to_replace <- grep("trypanosoma brucei brucei", data_class$ScientificName, ignore.case = TRUE)
data_class[rows_to_replace, "ScientificName"] <- "Trypanosoma brucei"

grouped_data <- data_class %>%
  group_by(ScientificName) %>%
  summarise(Run = n(), SumSizeGB = round(sum(size_MB)/(1024))) %>%
  mutate(ScientificName = ifelse(SumSizeGB < 1000, "Other Protists", ScientificName))

grouped_data <- grouped_data %>%
  group_by(ScientificName) %>%
  summarise(Run = sum(Run), SumSizeGB = sum(SumSizeGB))


ordered_levels <- unique(grouped_data$ScientificName[order(grouped_data$SumSizeGB)])
ordered_levels <- rev(ordered_levels[ordered_levels != "Other Protists"])
ordered_levels <- c( ordered_levels,"Other Protists")
grouped_data$ScientificName <- factor(grouped_data$ScientificName, levels = rev(ordered_levels))

ggplot(grouped_data, aes(x = as.factor(ScientificName), y = SumSizeGB)) +
  geom_hline(yintercept = c(0), linetype = "solid", color = "black") +
  
  geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.6), color = "black",fill="#8da0cb") +
  coord_flip() + 
  geom_text(aes(label = paste0(format(round(SumSizeGB)), " GB") ,y = SumSizeGB), position = position_dodge(width = 0.6), hjust = 1.1, vjust = 0.5, size = 3,color='white') +
  
  labs(x = "Protists", y = "Total Size (GB)", title = "") +
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
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size =20),
    legend.key.width = unit(0.40, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.position = c(0.65, 0.28),
    panel.grid.major.x = element_line(size = 0.4,color = 'black',linetype = "dashed"),
    
    
    legend.background = element_rect(color = "black", size = 0.2)) +
  scale_y_continuous(
    trans='sqrt',
    breaks = c(0,1000,3500,  8000,50000),
    labels =  c(0,1000,3500,  8000,50000)
  )





















