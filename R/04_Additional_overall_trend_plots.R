pacman::p_load(haven,readxl,tidyverse,plyr,ggplot2,ggpubr,ggsci,ggExtra,cowplot,gghalves,ggthemes,ggforce,showtext,Cairo,gridExtra,gtable,grid,RColorBrewer,
               reshape,scales,NetworkChange,magrittr,apc,cmprsk,fanplot,Epi,bitops,caTools,BAPC,INLA,data.table,
               install = TRUE)

population <- read_csv("D:/GBD_data/air pollution.csv")


data_long<-population%>%mutate(Part=ifelse(Year<=2021, "Solid","Dashed"))


p<-ggplot(data_long, aes(x = Year, y = PM)) +
  geom_line(data = subset(data_long, Year<=2022), size = 1, color = "#8264CC") +
  geom_line(data = subset(data_long, Part == "Dashed"), linetype = "dashed", size = 1, color = "#8264CC") +
  geom_ribbon(data = subset(data_long, Part == "Dashed"), aes(ymin = air_lower, ymax = air_upper), 
              alpha = 0.2, fill = "#8264CC") +
  labs(title = "Air pollution",
       x = "Year",
       y = "Summary exposure values, %") +
  scale_y_continuous(labels = number_format(accuracy = 0.1), limits = c(0, 70),breaks = seq(0, 70, by = 10)) +
  scale_x_continuous(breaks = seq(2010, 2040, by = 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    plot.title = element_text(hjust = 0.5,size = 16,face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1,colour = "black",size = 10),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.line = element_line(colour = "black", size = 0.75),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

setwd("D:/GBD_data")


ggsave("Air pollution.tiff", plot = p, width = 8, height = 6, dpi = 300)


behavior1 <- read_csv("D:/GBD_data/Behavior1.csv")


data_long<-behavior1%>%mutate(Part=ifelse(Year<=2021, "Solid","Dashed"))


p<-ggplot(data_long, aes(x = Year, y = Value, color = type), show.legend = FALSE) +
  geom_line(data = subset(data_long, Year <= 2022), size = 1, show.legend = FALSE) +
  geom_line(data = subset(data_long, Part == "Dashed"), linetype = "dashed", size = 1, show.legend = FALSE) +
  scale_color_manual(values = c('#562e3c','#7d4444','#9e6c69','#cca69c'))+
  geom_ribbon(data = subset(data_long, Part == "Dashed"), aes(ymin = Lower, ymax = Upper), 
              alpha = 0.2, show.legend = FALSE) +
  labs(title = "Behaviors- Smoking et al",
       x = "Year",
       y = "Summary exposure values, %") +
  scale_y_continuous(labels = number_format(accuracy = 0.1), limits = c(0, 70),breaks = seq(0, 70, by = 10)) +
  scale_x_continuous(breaks = seq(2010, 2040, by = 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.line = element_line(colour = "black", size = 0.75),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

setwd("D:/GBD_data")


ggsave("Behavior1.tiff", plot = p, width = 8, height = 6, dpi = 300)




behavior2 <- read_csv("D:/GBD_data/Behavior2.csv")


data_long<-behavior2%>%mutate(Part=ifelse(Year<=2021, "Solid","Dashed"))


p<-ggplot(data_long, aes(x = Year, y = Value, color = type), show.legend = FALSE) +
  geom_line(data = subset(data_long, Year <= 2022), size = 1, show.legend = FALSE) +
  geom_line(data = subset(data_long, Part == "Dashed"), linetype = "dashed", size = 1, show.legend = FALSE) +
  scale_color_manual(values = c('#9ecae1','#5D90BA','#2873B3','#2EBEBE','#223D6C','#7CC767','#004529'))+
  geom_ribbon(data = subset(data_long, Part == "Dashed"), aes(ymin = Lower, ymax = Upper), 
              alpha = 0.2, show.legend = FALSE) +
  labs(title = "Behaviors- Dietary",
       x = "Year",
       y = "Summary exposure values, %") +
  scale_y_continuous(labels = number_format(accuracy = 0.1), limits = c(0, 70),breaks = seq(0, 70, by = 10)) +
  scale_x_continuous(breaks = seq(2010, 2040, by = 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.line = element_line(colour = "black", size = 0.75),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

setwd("D:/GBD_data")


ggsave("Behavior2.tiff", plot = p, width = 8, height = 6, dpi = 300)




Metabolic <- read_csv("D:/GBD_data/Metabolic.csv")


data_long<-Metabolic%>%mutate(Part=ifelse(Year<=2021, "Solid","Dashed"))


p<-ggplot(data_long, aes(x = Year, y = Value, color = type), show.legend = FALSE) +
  geom_line(data = subset(data_long, Year <= 2022), size = 1, show.legend = FALSE) +
  geom_line(data = subset(data_long, Part == "Dashed"), linetype = "dashed", size = 1, show.legend = FALSE) +
  scale_color_manual(values = c('#D8D155','#F1CC2F','#F0E442','#E69F00','#D55E00'))+
  geom_ribbon(data = subset(data_long, Part == "Dashed"), aes(ymin = Lower, ymax = Upper), 
              alpha = 0.2, show.legend = FALSE) +
  labs(title = "Metabolic",
       x = "Year",
       y = "Summary exposure values, %") +
  scale_y_continuous(labels = number_format(accuracy = 0.1), limits = c(0, 70),breaks = seq(0, 70, by = 10)) +
  scale_x_continuous(breaks = seq(2010, 2040, by = 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.line = element_line(colour = "black", size = 0.75),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

setwd("D:/GBD_data")


ggsave("Metabolic.tiff", plot = p, width = 8, height = 6, dpi = 300)


Outcome <- read_csv("D:/GBD_data/Outcomes.csv")


data_long<-Outcome%>%mutate(Part=ifelse(Year<=2021, "Solid","Dashed"))


p<-ggplot(data_long, aes(x = Year, y = Value, color = type), show.legend = FALSE) +
  geom_line(data = subset(data_long, Year <= 2022), size = 1, show.legend = FALSE) +
  geom_line(data = subset(data_long, Part == "Dashed"), linetype = "dashed", size = 1, show.legend = FALSE) +
  scale_color_manual(values = c('#DC0000B2','#E64B35B2','#F39B7FB2'))+
  geom_ribbon(data = subset(data_long, Part == "Dashed"), aes(ymin = Lower, ymax = Upper), 
              alpha = 0.2, show.legend = FALSE) +
  labs(title = "Outcomes",
       x = "Year",
       y = "Incidence rate, per 100,000") +
  scale_y_continuous(labels = number_format(accuracy = 0.1), limits = c(0, 1400),breaks = seq(0, 1400, by = 200)) +
  scale_x_continuous(breaks = seq(2010, 2040, by = 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1, colour = "black", size = 10),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.line = element_line(colour = "black", size = 0.75),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

setwd("D:/GBD_data")


ggsave("Outcomes.tiff", plot = p, width = 8, height = 6, dpi = 300)