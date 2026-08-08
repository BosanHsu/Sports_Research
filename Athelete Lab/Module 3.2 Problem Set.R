##### SCROLL DOWN TO THE VERY BOTTOM TO SEE PROBLEM SET


getwd()
setwd("C:/Users/cmurr/OneDrive/Desktop/Coding Course/Data")


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


# Import data / CSV & Remove Extra Column "V1"

TestTrackMan=fread("TestTrackMan.csv")
TestTrackMan <- subset(TestTrackMan, select = -c(V1))



# Create LHP / RHP Split Tables

LHP <- TestTrackMan[TestTrackMan$PitcherThrows == "Left"]
RHP <- TestTrackMan[TestTrackMan$PitcherThrows == "Right"]

# rbind -- LHP & RHP -- has to have the same columns

Total <- rbind(LHP, RHP)





# Create Team and Throws Tables

LHP_Velo <- LHP[, .(
  MPH = mean(RelSpeed, na.rm = TRUE)),
  by=.(PitcherThrows)]

RHP_Velo <- RHP[, .(
  MPH = mean(RelSpeed, na.rm = TRUE)),
  by=.(PitcherThrows)]



# cbind

Velos <- cbind(LHP_Velo, RHP_Velo)





# Create Team and Throws Table

Team <- subset(TestTrackMan, select = c(Pitcher, PitcherTeam))
Throws <- subset(TestTrackMan, select = c(Pitcher, PitcherThrows))

Team <- head(Team, 1)
Throws <- head(Throws, 1)

# merge

Team_Throws <- merge(Team, Throws)





pd <- merge(PitcherData, TestTrackMan)



########## Problem Set ##############

#Create the Following:

# 1) Combine PitcherData with TestTrackMan to add the following
#    column: "is.fastball" - in conjunction with the previous session

