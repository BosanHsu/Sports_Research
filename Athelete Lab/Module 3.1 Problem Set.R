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

# -c = remove the following columns
# c = include the following columns


# Subsetting Data -- Pitcher, TaggedPitchType, RelSpeed

# including just Pitcher, TaggedPitchType, RelSpeed

PitcherData <- subset(TestTrackMan, select = c(Pitcher, TaggedPitchType, RelSpeed))



# Filtering Data by Column Values (==, !=, !is.na()) -- Specific Pitcher

unique(PitcherData$Pitcher)

# ==  to only include Foster, Larry
PitcherData <- PitcherData[PitcherData$Pitcher == "Foster, Larry"]

# !=  to remove only Foster, Larry
PitcherData <- PitcherData[PitcherData$Pitcher != "Foster, Larry"]

# !is.na()  to remove any rows that show up as NA within the column: Pitcher
PitcherData <- PitcherData[!is.na(PitcherData$Pitcher)]

# Adding / Creating Columns -- "is.fastball" Column

# the dollar sign represents the column we want to create/select/work with

PitcherData$is.fastball <- PitcherData$TaggedPitchType


# Changing Values -- FF/SI -> FB

unique(PitcherData$TaggedPitchType)

PitcherData$is.fastball[PitcherData$is.fastball == "Fastball"] <- "yes"
PitcherData$is.fastball[PitcherData$is.fastball == "Sinker"] <- "yes"
PitcherData$is.fastball[PitcherData$is.fastball == "ChangeUp"] <- "no"
PitcherData$is.fastball[PitcherData$is.fastball == "Slider"] <- "no"
PitcherData$is.fastball[PitcherData$is.fastball == "Curveball"] <- "no"

unique(PitcherData$is.fastball)


# Removing non-fastballs

PitcherData <- PitcherData[PitcherData$is.fastball == "yes"]


# Sum, Max, Mean -- RelSpeed

PitcherData1 <- PitcherData[, .(
  MPH_Sum = sum(RelSpeed, na.rm = TRUE),
  MPH_Max = max(RelSpeed, na.rm = TRUE),
  MPH_Mean = mean(RelSpeed, na.rm = TRUE)),
  by=.(Pitcher)]


# Round -- RelSpeed

PitcherData$RelSpeed <- round(PitcherData$RelSpeed, digits = 1)


# Tally -- Pitcher, TaggedPitchType, RelSpeed

PitcherData2 <- PitcherData%>%group_by(Pitcher, TaggedPitchType, RelSpeed)%>%tally()


# Save Data

write.csv(PitcherData2, "PitcherData2.csv")








########## Problem Set ##############

#Create the Following:

# 1) A dataset pulled from PitcherData, where the pitcher is Carson Sentz  

# 2) The average Velocity for Carson Sentz (For each TaggedPitchType)

# 3) A tally of Pitcher, TaggedPitchType, and Batter faced
