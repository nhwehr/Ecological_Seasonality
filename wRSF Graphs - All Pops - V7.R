#### International Manuscript Graphs
### Effect Sizes
## Initial Setup
# Libraries
library(ggplot2)
library(scales)
library(ggpubr)
library(dplyr)

# Data
setwd('C:\\Users\\natew\\Documents\\All Files\\R Working Directory\\Ecological Seasonality')
AllData <- read.csv('Final wRSF Results.csv')
AllData$KDE_Size <- as.factor(AllData$KDE_Size)
AllData$Group <- paste(AllData$Season,  " ", AllData$KDE_Size, "% UD", sep = "")
head(AllData)
#AllData <- filter(AllData, AllData$Group %in% c("95% UD"))

## Graphs
# Mainland Wolf Habitat
Data <- filter(AllData, AllData$Population %in% c("GPIR_Wolf"))
Data <- filter(Data, Data$Variable %in% c('Coniferous', 'Deciduous', 'Mixed', 'Shrubland', 'Wetland'))
MWH <- ggplot(Data, aes(x=factor(Variable, level = c('Coniferous', 'Deciduous', 'Mixed', 'Shrubland', 'Wetland')), y=Mean, 
                        Group=Group, color=Group, shape=Group)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) +
  geom_point(position=position_dodge(0.35), size = 1.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.25, position=position_dodge(0.35), size = 0.5) +
  labs(y = "Gray wolves
GPIR
effect size", x = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values=c('forestgreen', 'darkgreen',  'royalblue1', 'royalblue3')) +
  scale_shape_manual(values = c(18, 16, 18, 16)) +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(axis.text.x = element_blank(), #element_text(angle = 0, vjust = 0, hjust=0.5, size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size=8),
        legend.position = "none",
        panel.background = element_rect(fill = "grey100", colour = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Mainland Wolf Landscape
Data <- filter(AllData, AllData$Population %in% c("GPIR_Wolf"))
Data <- filter(Data, Data$Variable %in% c('Aspect', 'Slope', 'Roads', 'Superior'))
MWL <- ggplot(Data, aes(x=factor(Variable, level = c('Aspect', 'Slope', "Roads", "Superior")), y=Mean, 
                           Group=Group, color=Group, shape=Group)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) +
  geom_point(position=position_dodge(0.35), size = 1.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.25, position=position_dodge(0.35), size = 0.5) +
  labs(y = element_blank(), x = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values=c('forestgreen',  'darkgreen', 'royalblue1', 'royalblue3')) +
  scale_shape_manual(values = c(18, 16, 18, 16)) +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(axis.text.x = element_blank(), #element_text(angle = 0, vjust = 0, hjust=0.5, size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size=8),
        legend.position = "right",
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "grey100", colour = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Mainland Moose Habitat
Data <- filter(AllData, AllData$Population %in% c("GPIR_Moose"))
Data <- filter(Data, Data$Variable %in% c('Coniferous', 'Deciduous', 'Mixed', 'Shrubland', 'Wetland'))
MMH <- ggplot(Data, aes(x=factor(Variable, level = c('Coniferous', 'Deciduous', 'Mixed', 'Shrubland', 'Wetland')), y=Mean, 
                 Group=Group, color=Group, shape=Group)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) +
  geom_point(position=position_dodge(0.35), size = 1.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.25, position=position_dodge(0.35), size = 0.5) +
  labs(y = "Moose
GPIR
effect size", x = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values=c('forestgreen', 'darkgreen',  'royalblue1', 'royalblue3')) +
  scale_shape_manual(values = c(18, 16, 18, 16)) +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(axis.text.x = element_blank(), #element_text(angle = 0, vjust = 0, hjust=0.5, size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size=8),
        legend.position = "none",
        panel.background = element_rect(fill = "grey100", colour = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Mainland Moose Landscape
Data <- filter(AllData, AllData$Population %in% c("GPIR_Moose"))
Data <- filter(Data, Data$Variable %in% c('Aspect', 'Slope', 'Roads', 'Superior'))
MML <- ggplot(Data, aes(x=factor(Variable, level = c('Aspect', 'Slope', "Roads", "Superior")), y=Mean, 
                 Group=Group, color=Group, shape=Group)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) +
  geom_point(position=position_dodge(0.35), size = 1.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.25, position=position_dodge(0.35), size = 0.5) +
  labs(y = element_blank(), x = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values=c('forestgreen',  'darkgreen', 'royalblue1', 'royalblue3')) +
  scale_shape_manual(values = c(18, 16, 18, 16)) +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(axis.text.x = element_blank(), #element_text(angle = 0, vjust = 0, hjust=0.5, size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size=8),
        legend.position = "right",
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "grey100", colour = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Mainland Deer Habitat
Data <- filter(AllData, AllData$Population %in% c("GPIR_Deer"))
Data <- filter(Data, Data$Variable %in% c('Coniferous', 'Deciduous', 'Mixed', 'Shrubland', 'Wetland'))
MDH <- ggplot(Data, aes(x=factor(Variable, level = c('Coniferous', 'Deciduous', 'Mixed', 'Shrubland', 'Wetland')), y=Mean, 
                 Group=Group, color=Group, shape=Group)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) +
  geom_point(position=position_dodge(0.35), size = 1.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.25, position=position_dodge(0.35), size = 0.5) +
  labs(y = "White-tailed deer
GPIR
effect size", x = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values=c('forestgreen', 'darkgreen',  'royalblue1', 'royalblue3')) +
  scale_shape_manual(values = c(18, 16, 18, 16)) +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(axis.text.x = element_blank(), #element_text(angle = 0, vjust = 0, hjust=0.5, size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size=8),
        legend.position = "none",
        panel.background = element_rect(fill = "grey100", colour = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Mainland Deer Landscape
Data <- filter(AllData, AllData$Population %in% c("GPIR_Deer"))
Data <- filter(Data, Data$Variable %in% c('Aspect', 'Slope', 'Roads', 'Superior'))
MDL <- ggplot(Data, aes(x=factor(Variable, level = c('Aspect', 'Slope', "Roads", "Superior")), y=Mean, 
                 Group=Group, color=Group, shape=Group)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) +
  geom_point(position=position_dodge(0.35), size = 1.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.25, position=position_dodge(0.35), size = 0.5) +
  labs(y = element_blank(), x = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values=c('forestgreen',  'darkgreen', 'royalblue1', 'royalblue3')) +
  scale_shape_manual(values = c(18, 16, 18, 16)) +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(axis.text.x = element_blank(), #element_text(angle = 0, vjust = 0, hjust=0.5, size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size=8),
        legend.position = "right",
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "grey100", colour = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Data
setwd('C:\\Users\\natew\\Documents\\All Files\\R Working Directory\\Ecological Seasonality')
AllData <- read.csv('Final wRSF Results.csv')
AllData$KDE_Size <- as.factor(AllData$KDE_Size)
AllData$Group <- paste(AllData$Population, " ", AllData$KDE_Size, "% UD", sep = "")
AllData$Group <- substr(AllData$Group, 6, 80)
head(AllData)

## Graphs
# Island Wolf and Moose Habitat
Data <- filter(AllData, AllData$Population %in% c("IRNP_Wolf", "IRNP_Moose"))
Data <- filter(Data, Data$Variable %in% c('Coniferous', 'Deciduous', 'Mixed', 'Shrubland', 'Wetland'))
IWMH <- ggplot(Data, aes(x=factor(Variable, level = c('Coniferous', 'Deciduous', 'Mixed', 'Shrubland', 'Wetland')), y=Mean, 
                 Group=Group, color=Group, shape=Group)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) +
  geom_point(position=position_dodge(0.35), size = 1.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.25, position=position_dodge(0.35), size = 0.5) +
  labs(y = "Gray wolves and moose
IRNP
effect size", x = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values=c('firebrick1',  'firebrick3', 'goldenrod1', 'goldenrod3')) +
  scale_shape_manual(values = c(18, 16, 18, 16)) +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5, size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size=8),
        legend.position = "none",
        panel.background = element_rect(fill = "grey100", colour = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Island Wolf and Moose Landscape
Data <- filter(AllData, AllData$Population %in% c("IRNP_Wolf", "IRNP_Moose"))
Data <- filter(Data, Data$Variable %in% c('Aspect', 'Slope', 'Trails', 'Superior'))
Data$Variable <- recode_factor(Data$Variable, Trails = "Trail or road")
IWML <- ggplot(Data, aes(x=factor(Variable, level = c('Aspect', 'Slope', "Trail or road", "Superior")), y=Mean, 
                 Group=Group, color=Group, shape=Group)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) +
  geom_point(position=position_dodge(0.35), size = 1.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.25, position=position_dodge(0.35), size = 0.5) +
  labs(y = element_blank(), x = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values=c('firebrick1',  'firebrick3', 'goldenrod1', 'goldenrod3')) +
  scale_shape_manual(values = c(18, 16, 18, 16)) +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5, size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size=8),
        legend.position = "right",
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "grey100", colour = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))


Combo1 <- ggarrange(MWH, MWL, MMH, MML, MDH, MDL, IWMH, IWML, ncol = 2, nrow = 4)
Combo1
ggsave(plot = Combo1, width = 9, height = 6.5, dpi = 800, filename = "Figure 3 - V8.jpg")
