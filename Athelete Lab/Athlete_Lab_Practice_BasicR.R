getwd()
setwd("C:/Users/hsupo/OneDrive/桌面/Sports Analytics/Athelete Lab")

# Install Packages
# install.packages("ggplot2")


# load packages
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

# Import Data
TestTrackMan = fread("TestTrackMan.csv")
colnames(TestTrackMan)
unique(TestTrackMan$TaggedPitchType)
table(TestTrackMan$TaggedPitchType)

df_Fastball <- TestTrackMan%>%filter(TaggedPitchType == "Fastball")
unique(df_Fastball$TaggedPitchType)

df_FS <- TestTrackMan%>%filter(TaggedPitchType %in% c("Fastball", "Sinker"))
unique(df_FS$TaggedPitchType)

df_no_Fastball <- TestTrackMan%>%filter(TaggedPitchType != "Fastball")
unique(df_no_Fastball$TaggedPitchType)

# Subset
# -c: remove the following columns; c: include

PitcherData <- subset(TestTrackMan, select = c(Pitcher, TaggedPitchType, RelSpeed))

unique(PitcherData$Pitcher)
PitcherData_Foster <- PitcherData[PitcherData$Pitcher == "Foster, Larry"]

PitcherData_No_NA <- PitcherData[!is.na(PitcherData$Pitcher)]


# Create Column
fastball_levels <- c("Fastball", "Sinker")
PitcherData$is_fastball <- ifelse(PitcherData$TaggedPitchType %in% fastball_levels,
                                  "yes", "no")

# SUM, MAX, MIN, ROUND
# data.table 的語法。關鍵點：
# DT[i, j, by]：data.table 的三格語法
# i：要選的列（row filter）
# j：要對資料做的運算/欄位
# by：分組欄位
PitcherData1 <- PitcherData[
  ,                                                   # i：不篩列（全資料）
  list(                                                  # j：回傳一個 list → 變成新資料表
    MPH_Sum  = sum(RelSpeed,  na.rm = TRUE),          #   分組後的總和
    MPH_Max  = max(RelSpeed,  na.rm = TRUE),          #   分組後的最大
    MPH_Mean = mean(RelSpeed, na.rm = TRUE)           #   分組後的平均
  ),
  by = list(Pitcher, TaggedPitchType)                    # by：依 Pitcher、TaggedPitchType 分組
]

PitcherData1$MPH_Mean <- round(PitcherData1$MPH_Mean, digits = 1)

# dplyr寫法 -> 易懂
PitcherData_1 <- PitcherData %>%
  group_by(Pitcher, TaggedPitchType) %>%
  summarise(
    MPH_Sum  = sum(RelSpeed,  na.rm = TRUE),
    MPH_Max  = max(RelSpeed,  na.rm = TRUE),
    MPH_Mean = round(mean(RelSpeed, na.rm = TRUE), 1),  # ← 一位小數
    .groups = "drop"  #.groups = "drop" 是 dplyr::summarise() 的參數，"drop"＝彙整完把所有分組都拿掉（= ungroup()）。
  )
PitcherData_1


# Tally: 某個特定數據出現幾次
PitcherData2 <- PitcherData%>%group_by(Pitcher, TaggedPitchType, RelSpeed)%>%tally()
head(PitcherData2)

# Save
write.csv(PitcherData, "PitcherData.csv")



### Merge

# 1. Create LHP/RHP
LHP <- TestTrackMan[TestTrackMan$PitcherThrows == "Left"]
RHP <- TestTrackMan[TestTrackMan$PitcherThrows == "Right"]

LHP_Velo <- LHP[
  ,                                                   # i：不篩列（全資料）
  list(                                                  # j：回傳一個 list → 變成新資料表
    MPH_Mean = mean(RelSpeed, na.rm = TRUE)           #   分組後的平均
  ), 
  by = list(PitcherThrows)]

RHP_Velo <- RHP[
  ,                                                   # i：不篩列（全資料）
  list(                                                  # j：回傳一個 list → 變成新資料表
    MPH_Mean = mean(RelSpeed, na.rm = TRUE)           #   分組後的平均
  ),
  by = list(PitcherThrows)]

# cbind
Velos <- cbind(LHP_Velo, RHP_Velo)

# 2. Create Team and Throws Table
Team <- subset(TestTrackMan, select = c(Pitcher, PitcherTeam))
Throws <- subset(TestTrackMan, select = c(Pitcher, PitcherThrows))

Team <- head(Team,1)
Throws <- head(Throws,1)

# merge
Team_Throws <- merge(Team, Throws)
