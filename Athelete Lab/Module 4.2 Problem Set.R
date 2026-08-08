##### SCROLL DOWN TO THE VERY BOTTOM TO SEE PROBLEM SET


getwd()
setwd("C:/Users/hsupo/OneDrive/桌面/Sports Analytics/Athelete Lab")


# load packages


library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(ggpubr)


# Import data / CSV & Remove Extra Column "V1"

TestTrackMan=fread("TestTrackMan.csv")



# Filtering Data by Column Value -- Pitcher Name

unique(TestTrackMan$Pitcher)

# Larry

Larry <- TestTrackMan%>%
  filter(Pitcher == "Foster, Larry")%>%
  ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType),size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Larry")



# Carson

Carson <- TestTrackMan%>%
  filter(Pitcher == "Sentz, Carson")%>%
  ggplot(TestTrackMan, mapping = aes(x=PlateLocSide, y= PlateLocHeight)) +
  geom_point(aes(color = TaggedPitchType),size = 3) +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple"))+
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (44.08/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (18.29/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (-11.5/12)+.25, y = (44.08/12), xend = (-11.5/12)+.25, yend = (18.29/12)) +
  geom_segment(x = (5.5/12)+.25, y = (44.08/12), xend = (5.5/12)+.25, yend = (18.29/12)) +
  
  
  geom_segment(x = (-11.5/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (5.5/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-11.5/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (5.5/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (44.08/12), xend = (-5.835/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (35.48/12), xend = (-5.835/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-5.835/12)+.25, y = (26.88/12), xend = (-5.835/12)+.25, yend = (18.29/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (44.08/12), xend = (-0.165/12)+.25, yend = (35.48/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (35.48/12), xend = (-0.165/12)+.25, yend = (26.88/12), size = .3) +
  geom_segment(x = (-0.165/12)+.25, y = (26.88/12), xend = (-0.165/12)+.25, yend = (18.29/12), size = .3) +
  
  
  geom_segment(x = (-.708 -(3/12))+.25, y = (.708/2), xend = (0-(3/12))+.25, yend = (1.417/2)) +
  geom_segment(x = (0-(3/12))+.25, y = (1.417/2), xend = (.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (.708-(3/12))+.25, y = (.708/2), xend = (.708-(3/12))+.25, yend = (0)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (-.708-(3/12))+.25, yend = (.708/2)) +
  geom_segment(x = (-.708-(3/12))+.25, y = (0), xend = (.708-(3/12))+.25, yend = (0)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") + 
  xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Carson")


# Combining Both Graphs

ggarrange(Larry, Carson ,nrow = 1, ncol = 2)










# Line Graph - Velo Per Pitch; by Pitcher, TaggedPitchType

TestTrackMan%>%
  filter(Pitcher == "Foster, Larry") %>%
  ggplot(TestTrackMan, mapping = aes(x=PitchNo, y= RelSpeed, colour = TaggedPitchType)) +
  geom_line() + geom_point() +
  scale_color_manual(values = c(ChangeUp = "blue", Fastball = "black",
                                Slider = "orange", Curveball = "red",
                                Cutter = "green",Sinker = "grey",
                                Splitter = "purple")) +
  ggtitle("Velocity / Pitch") +
  xlab("Pitch") + ylab("Velocity") + theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size = 13, face = "bold"),
    axis.title.y = element_text(color="black" , size = 13, face = "bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank())+
  geom_hline(yintercept = seq(from=70, to=100, by = 5))












# Percentiles - MAX Fastball Velo & Spin; by Pitcher (and TaggedPitchType == "Fastball")


# Finding Max Velo & Max SpinRate for Each Pitcher by Pitch Type

TM_Percentiles <- TestTrackMan[, .(
  'Max Velo' = max(RelSpeed, na.rm = TRUE),
  'Max Spin' = max(SpinRate, na.rm = TRUE)),
  by=.(Date, PitcherTeam, Pitcher, TaggedPitchType)]


# Rounding to Single Decimal Point

TM_Percentiles$'Max Velo' <- round(TM_Percentiles$'Max Velo', digits = 1)
TM_Percentiles$'Max Spin' <- round(TM_Percentiles$'Max Spin', digits = 0)



# Creating Ranking and Percentile for Max Velo -- Fastball Only!

TM_Percentiles$MaxVelo_percentile <- TM_Percentiles$`Max Velo`

TM_Percentiles$MaxVelo_percentile <- round(TM_Percentiles$MaxVelo_percentile, digits = 2)

TM_Percentiles$MaxVelo_percentile[TM_Percentiles$TaggedPitchType != "Fastball"] <- NA


TM_Percentiles$MaxVelo_ranking[order(TM_Percentiles$MaxVelo_percentile,
                                     decreasing = TRUE)] <- 1:nrow(TM_Percentiles)


TM_Percentiles$MaxVelo_ranking[TM_Percentiles$TaggedPitchType != "Fastball"] <- NA


TM_Percentiles$MaxVelo_percentile <-  1 - ((TM_Percentiles$MaxVelo_ranking) / max(TM_Percentiles$MaxVelo_ranking, na.rm = TRUE))

TM_Percentiles$MaxVelo_percentile <- round(TM_Percentiles$MaxVelo_percentile, digits = 2)

TM_Percentiles$MaxVelo_percentile <- TM_Percentiles$MaxVelo_percentile*100






# Creating Ranking and Percentile for Max Spin -- Fastball Only!

TM_Percentiles$MaxSpin_percentile <- TM_Percentiles$`Max Spin`

TM_Percentiles$MaxSpin_percentile <- round(TM_Percentiles$MaxSpin_percentile, digits = 2)

TM_Percentiles$MaxSpin_percentile[TM_Percentiles$TaggedPitchType != "Fastball"] <- NA


TM_Percentiles$MaxSpin_ranking[order(TM_Percentiles$MaxSpin_percentile,
                                     decreasing = TRUE)] <- 1:nrow(TM_Percentiles)


TM_Percentiles$MaxSpin_ranking[TM_Percentiles$TaggedPitchType != "Fastball"] <- NA


TM_Percentiles$MaxSpin_percentile <-  1 - ((TM_Percentiles$MaxSpin_ranking) / max(TM_Percentiles$MaxSpin_ranking, na.rm = TRUE))

TM_Percentiles$MaxSpin_percentile <- round(TM_Percentiles$MaxSpin_percentile, digits = 2)

TM_Percentiles$MaxSpin_percentile <- TM_Percentiles$MaxSpin_percentile*100




# removing rows with NA (non-fastball)

TM_Percentiles <- TM_Percentiles[!is.na(TM_Percentiles$MaxVelo_percentile), ]


# Creating High and Low Pitches for Color Scale

# Low

All_Data_Low=subset(TM_Percentiles, select = c(1:3))

All_Data_Low <- All_Data_Low[!duplicated(All_Data_Low)]

colnames(All_Data_Low)

All_Data_Low$TaggedPitchType <- "Low"
All_Data_Low$`Max Velo` <- 0
All_Data_Low$`Max Spin` <- 0
All_Data_Low$MaxVelo_percentile <- -5
All_Data_Low$MaxVelo_ranking <- 20
All_Data_Low$MaxSpin_percentile <- -5
All_Data_Low$MaxSpin_ranking <- 20


# High

All_Data_High=subset(TM_Percentiles, select = c(1:3))

All_Data_High <- All_Data_High[!duplicated(All_Data_High)]

colnames(All_Data_High)

All_Data_High$TaggedPitchType <- "High"
All_Data_High$`Max Velo` <- 100
All_Data_High$`Max Spin` <- 100
All_Data_High$MaxVelo_percentile <- 105
All_Data_High$MaxVelo_ranking <- 1
All_Data_High$MaxSpin_percentile <- 105
All_Data_High$MaxSpin_ranking <- 1



# Rbind to Combine Low and High

LowHigh <- rbind(All_Data_Low, All_Data_High)


# Rbind to Combine LowHigh with TM_Percentiles

TM_Percentiles <- rbind(TM_Percentiles, LowHigh)

write.csv(TM_Percentiles, "TM_Percentiles.csv")


# FB Max Velo

MaxVelo <- TM_Percentiles%>%
  filter(Pitcher == "Foster, Larry",
         TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
  ggplot(TM_Percentiles, mapping = aes(x= MaxVelo_percentile, y= TaggedPitchType, colour = (MaxVelo_percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("Max Velo") + xlim(0, 100) + ylim("Fastball") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = MaxVelo_percentile, y = TaggedPitchType, fill = MaxVelo_percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=MaxVelo_percentile),hjust=.5, vjust=.4, color = "White",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#cc0000", mid = "#ffffff", high = "#2952a3",
                       na.value = "grey50") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



# FB Max Spin

MaxSpin <- TM_Percentiles%>%
  filter(Pitcher == "Foster, Larry",
         TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
  ggplot(TM_Percentiles, mapping = aes(x= MaxSpin_percentile, y= TaggedPitchType, colour = (MaxSpin_percentile))) +
  geom_line() + geom_point(size = 9)  +
  ggtitle("Max Spin") + xlim(0, 100) + ylim("Fastball") +
  xlab("") + ylab("") + theme(
    plot.title = element_text(color = "black", size = 15, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.y  =element_blank(),
    axis.ticks.x  =element_blank(),
    axis.text.y = element_text(size=12, face="italic", colour = "black"))+
  geom_segment(aes(x = 0, xend = 100, y = TaggedPitchType, yend = TaggedPitchType), color = "#9b9b9b", size = 1) +
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5) +
  geom_point(aes(x = MaxSpin_percentile, y = TaggedPitchType, fill = MaxSpin_percentile), pch = 21, color = "black", size = 10) +
  geom_text(aes(label=MaxSpin_percentile),hjust=.5, vjust=.4, color = "White",
            size = 5)+theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#cc0000", mid = "#ffffff", high = "#2952a3") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggarrange(MaxVelo, MaxSpin ,nrow = 1, ncol = 2)




########## Problem Set ##############

#Create the Following:

# 1) Create a new ggarrange, but this time a 3x3 with either: different metrics OR different pitchers/hitters    
