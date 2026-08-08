getwd()
setwd("C:/Users/hsupo/OneDrive/桌面/Sports Analytics/Athelete Lab")

# load packages
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(ggpubr)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(paletteer)

# Import Data
TestTrackMan = fread("TestTrackMan.csv")

# Basic ggplot
# aes() = aesthetics（視覺對應）
ggplot(TestTrackMan,
       mapping = aes(x = PlateLocSide,
                     y = PlateLocHeight)
       )+ geom_point()

# Change Size
ggplot(TestTrackMan,
       mapping = aes(x = PlateLocSide,
                     y = PlateLocHeight)
      )+ geom_point(size = 3)

# Add Color
ggplot(TestTrackMan,
       mapping = aes(x = PlateLocSide,
                     y = PlateLocHeight)
      )+ geom_point(aes(color = TaggedPitchType),
                    size = 3)

# Set COlor
ggplot(TestTrackMan,
       mapping = aes(x = PlateLocSide,
                     y = PlateLocHeight)
      )+ geom_point(aes(color = TaggedPitchType),
                    size = 3
      )+ scale_color_manual(values = c(ChangeUp = "blue",
                                     Fastball = "black",
                                     Slider = "orange",
                                     Curveball = "red",
                                     Cutter = "green",
                                     Sinker = "grey",
                                     Splitter = "purple"))
# Change Shape
ggplot(TestTrackMan,
       mapping = aes(x = PlateLocSide,
                     y = PlateLocHeight)
      )+ geom_point(aes(shape = TaggedPitchType),
                    size = 3)

# Set COlor
ggplot(TestTrackMan,
       mapping = aes(x = PlateLocSide,
                     y = PlateLocHeight)
        )+ geom_point(aes(shape = TaggedPitchType,
                          color = TaggedPitchType),
                          size = 3
        )+ scale_color_manual(values = c(ChangeUp = "blue",
                                 Fastball = "black",
                                 Slider = "orange",
                                 Curveball = "red",
                                 Cutter = "green",
                                 Sinker = "grey",
                                 Splitter = "purple"))

### Strike Zone
# Width/Length

Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

Width <- (Right - Left)/3
Height <- (Top - Bottom)/3

ggplot(TestTrackMan,
       mapping = aes(x = PlateLocSide,
                     y = PlateLocHeight)
)+ geom_point(aes(shape = TaggedPitchType,
                  color = TaggedPitchType),
              size = 0
)+ xlim(-3,3) + ylim(0,6) +

# The Box
  geom_segment(x = Left, y = Bottom, xend = Right, yend = Bottom)+
  geom_segment(x = Left, y = Top, xend = Right, yend = Top)+
  geom_segment(x = Left, y = Bottom, xend = Left, yend = Top)+
  geom_segment(x = Right, y = Bottom, xend = Right, yend = Top)+

# Horizontal Lines
  geom_segment(x = Left, y = Bottom + Height, xend = Right, yend = Bottom + Height)+
  geom_segment(x = Left, y = Top - Height, xend = Right, yend = Top - Height)+
  
# Vertical Lines
  geom_segment(x = Left + Width, y = Bottom, xend = Left + Width, yend = Top)+
  geom_segment(x = Right - Width, y = Bottom, xend = Right - Width, yend = Top)+
  
# Plate
  geom_segment(x = Left, y = 0, xend = Right, yend = 0)+
  geom_segment(x = Left, y = 0, xend = Left, yend = 4.25/12)+
  geom_segment(x = Left, y = 4.25/12, xend = 0, yend = 8.5/12)+
  geom_segment(x = Right, y = 4.25/12, xend = Right, yend = 0)+
  geom_segment(x = 0, y = 8.5/12, xend = Right, yend = 4.25/12)
  
### Specific Pitcher
Larry <- TestTrackMan[TestTrackMan$Pitcher == "Foster, Larry"]

Larry_Visual <- ggplot(Larry,
                       mapping = aes(x = PlateLocSide,
                                     y = PlateLocHeight)
                       )+ geom_point(aes(shape = TaggedPitchType,
                                         color = TaggedPitchType),
                                     size = 3
                                     )+
  # The Box
  geom_segment(x = Left, y = Bottom, xend = Right, yend = Bottom)+
  geom_segment(x = Left, y = Top, xend = Right, yend = Top)+
  geom_segment(x = Left, y = Bottom, xend = Left, yend = Top)+
  geom_segment(x = Right, y = Bottom, xend = Right, yend = Top)+
  
  # Horizontal Lines
  geom_segment(x = Left, y = Bottom + Height, xend = Right, yend = Bottom + Height)+
  geom_segment(x = Left, y = Top - Height, xend = Right, yend = Top - Height)+
  
  # Vertical Lines
  geom_segment(x = Left + Width, y = Bottom, xend = Left + Width, yend = Top)+
  geom_segment(x = Right - Width, y = Bottom, xend = Right - Width, yend = Top)+
  
  # Plate
  geom_segment(x = Left, y = 0, xend = Right, yend = 0)+
  geom_segment(x = Left, y = 0, xend = Left, yend = 4.25/12)+
  geom_segment(x = Left, y = 4.25/12, xend = 0, yend = 8.5/12)+
  geom_segment(x = Right, y = 4.25/12, xend = Right, yend = 0)+
  geom_segment(x = 0, y = 8.5/12, xend = Right, yend = 4.25/12)+
  
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(-2.5,2.5) + ylim(-.5,5) +ggtitle("Larry")

Carson <- TestTrackMan[TestTrackMan$Pitcher == "Sentz, Carson"]

Carson_Visual <- ggplot(Carson,
                       mapping = aes(x = PlateLocSide,
                                     y = PlateLocHeight)
)+ geom_point(aes(shape = TaggedPitchType,
                  color = TaggedPitchType),
              size = 3
)+
  # The Box
  geom_segment(x = Left, y = Bottom, xend = Right, yend = Bottom)+
  geom_segment(x = Left, y = Top, xend = Right, yend = Top)+
  geom_segment(x = Left, y = Bottom, xend = Left, yend = Top)+
  geom_segment(x = Right, y = Bottom, xend = Right, yend = Top)+
  
  # Horizontal Lines
  geom_segment(x = Left, y = Bottom + Height, xend = Right, yend = Bottom + Height)+
  geom_segment(x = Left, y = Top - Height, xend = Right, yend = Top - Height)+
  
  # Vertical Lines
  geom_segment(x = Left + Width, y = Bottom, xend = Left + Width, yend = Top)+
  geom_segment(x = Right - Width, y = Bottom, xend = Right - Width, yend = Top)+
  
  # Plate
  geom_segment(x = Left, y = 0, xend = Right, yend = 0)+
  geom_segment(x = Left, y = 0, xend = Left, yend = 4.25/12)+
  geom_segment(x = Left, y = 4.25/12, xend = 0, yend = 8.5/12)+
  geom_segment(x = Right, y = 4.25/12, xend = Right, yend = 0)+
  geom_segment(x = 0, y = 8.5/12, xend = Right, yend = 4.25/12)+
  
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  xlim(-2.5,2.5) + ylim(-.5,5) +ggtitle("Carson")

### Combine Both Graphs
ggarrange(Larry_Visual, Carson_Visual, nrow = 1, ncol = 2)


### Line Graph
Larry_Line <- ggplot(Larry,
                       mapping = aes(x = PitchNo,
                                     y = RelSpeed,
                                     colour = TaggedPitchType))+
  geom_line() +
  geom_point(size = 3)+
  scale_color_manual(values = c(ChangeUp = "blue",
                                 Fastball = "black",
                                 Slider = "orange",
                                 Curveball = "red",
                                 Cutter = "green",
                                 Sinker = "grey",
                                 Splitter = "purple"))+
  ggtitle("Velocity/Pitch")+
  xlab("Pitch") + ylab("Velocity") + 
  theme(plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
        axis.title.x = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y = element_text(color = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  geom_hline(yintercept = seq(from = 70, to = 100, by = 5))

Larry_Line

### Percentiles
# Find Max Velo & Max SpinRate for Each Pitcher's Each Pitch Type
TM_percentiles <- TestTrackMan[, list(
  "Max_Velo" = max(RelSpeed, na.rm = TRUE),
  "Max_Spin" = max(SpinRate, na.rm = TRUE)),
  by = list(Pitcher, TaggedPitchType)
]

# Round
TM_percentiles$Max_Velo <- round(TM_percentiles$Max_Velo, digits = 2)
TM_percentiles$Max_Spin <- round(TM_percentiles$Max_Spin, digits = 0)

# Create Ranking
Fastball_TM_percentiles <- TM_percentiles[TM_percentiles$TaggedPitchType == "Fastball"]
Fastball_TM_percentiles$MaxVelo_Ranking <- rank(-Fastball_TM_percentiles$Max_Velo,
                                                 ties.method = "min")
Fastball_TM_percentiles$MaxVelo_Percentile <- 1 - ((Fastball_TM_percentiles$MaxVelo_Ranking/max(Fastball_TM_percentiles$MaxVelo_Ranking, na.rm = TRUE)))
Fastball_TM_percentiles$MaxVelo_Percentile <- round(Fastball_TM_percentiles$MaxVelo_Percentile, digits = 2) * 100
Fastball_TM_percentiles$MaxSpin_Ranking <- rank(-Fastball_TM_percentiles$Max_Spin,
                                                ties.method = "min")
Fastball_TM_percentiles$MaxSpin_Percentile <- 1 - ((Fastball_TM_percentiles$MaxSpin_Ranking/max(Fastball_TM_percentiles$MaxSpin_Ranking, na.rm = TRUE)))
Fastball_TM_percentiles$MaxSpin_Percentile <- round(Fastball_TM_percentiles$MaxSpin_Percentile, digits = 2) * 100
Fastball_TM_percentiles

# Create High/Low Pitchs for Color Scale
# Low
All_Data_Low <- subset(Fastball_TM_percentiles, select = c(1))
All_Data_Low <- All_Data_Low[!duplicated(All_Data_Low)]

All_Data_Low$TaggedPitchType <- "Low"
All_Data_Low$Max_Velo <- 0
All_Data_Low$Max_Spin <- 0
All_Data_Low$MaxVelo_Percentile <- -5
All_Data_Low$MaxVelo_Ranking <- 20
All_Data_Low$MaxSpin_Percentile <- -5
All_Data_Low$MaxSpin_Ranking <- 20

# High
All_Data_High <- subset(Fastball_TM_percentiles, select = c(1))
All_Data_High <- All_Data_High[!duplicated(All_Data_High)]

All_Data_High$TaggedPitchType <- "High"
All_Data_High$Max_Velo <- 100
All_Data_High$Max_Spin <- 100
All_Data_High$MaxVelo_Percentile <- 105
All_Data_High$MaxVelo_Ranking <- 1
All_Data_High$MaxSpin_Percentile <- 105
All_Data_High$MaxSpin_Ranking <- 1

# Rbind to Combine Low/High; Combine Low/High with Fastball TM Percentile
LowHigh <- rbind(All_Data_Low, All_Data_High)
Fastball_TM_percentiles <- rbind(Fastball_TM_percentiles, LowHigh)
write.csv(Fastball_TM_percentiles, "TM_Percentiles.csv")

### Max Velo Chart
MaxVelo_Larry <- Fastball_TM_percentiles[Pitcher == "Foster, Larry"
                                         & TaggedPitchType %in% c("Fastball","High","Low")]
MaxVelo_Larry_Visual <- ggplot(MaxVelo_Larry, mapping = aes(x = MaxVelo_Percentile,
                                    y = TaggedPitchType,
                                    colour = MaxVelo_Percentile))+
  geom_line() + geom_point(size = 9)+
  ggtitle("Max Velo") + xlim(0, 100) + ylim("Fastball") +
  theme(plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 12, face = "italic"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())+
  geom_segment(aes(x = 0, xend = 100,
                   y= TaggedPitchType, yend = TaggedPitchType),
               color = "#9b9b9b",
               size = 1)+
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5)+
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5)+
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5)+
  geom_point(aes(x = MaxVelo_Percentile, y = TaggedPitchType, fill = MaxVelo_Percentile),
             pch = 21, color = "#9b9b9b", size = 10)+
  geom_text(aes(label = MaxVelo_Percentile), hjust = .5, vjust = .4, color = "Black", size = 5)+
  theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#cc0000", mid = "#ffffff", high = "#2952a3", na.value = "grey50")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

MaxVelo_Larry_Visual

### Max Velo Chart
MaxSpin_Larry <- Fastball_TM_percentiles[Pitcher == "Foster, Larry"
                                         & TaggedPitchType %in% c("Fastball","High","Low")]
MaxSpin_Larry_Visual <- ggplot(MaxSpin_Larry, mapping = aes(x = MaxSpin_Percentile,
                                                            y = TaggedPitchType,
                                                            colour = MaxSpin_Percentile))+
  geom_line() + geom_point(size = 9)+
  ggtitle("Max Spin") + xlim(0, 100) + ylim("Fastball") +
  theme(plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 12, face = "italic"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())+
  geom_segment(aes(x = 0, xend = 100,
                   y= TaggedPitchType, yend = TaggedPitchType),
               color = "#9b9b9b",
               size = 1)+
  geom_point(aes(x = 0, y = TaggedPitchType), color = "#9b9b9b", size = 5)+
  geom_point(aes(x = 50, y = TaggedPitchType), color = "#9b9b9b", size = 5)+
  geom_point(aes(x = 100, y = TaggedPitchType), color = "#9b9b9b", size = 5)+
  geom_point(aes(x = MaxSpin_Percentile, y = TaggedPitchType, fill = MaxSpin_Percentile),
             pch = 21, color = "#9b9b9b", size = 10)+
  geom_text(aes(label = MaxSpin_Percentile), hjust = .5, vjust = .4, color = "Black", size = 5)+
  theme(legend.position = "none")+
  scale_fill_gradient2(midpoint = 50, low = "#cc0000", mid = "#ffffff", high = "#2952a3", na.value = "grey50")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

MaxSpin_Larry_Visual
                                         

### Combine Both Graphs
ggarrange(MaxVelo_Larry_Visual, MaxSpin_Larry_Visual, nrow = 1, ncol = 2)


### Heat Plot
# Creates A Color Palette, Reversed (Blue/Less Frequent to Red/More Frequent) with 9 different shades
heat_colors_interpolated <- colorRampPalette(
  paletteer::paletteer_d("RColorBrewer::RdBu", 
                         n = 9,
                         direction = -1)
)(16)

# Shows The Color Scale
heat_colors_interpolated %>% scales::show_col()

# Filter by Pitcher
pp1 <- TestTrackMan %>%
  filter(Pitcher == "Foster, Larry")

# Create A Table For Usage % Per Pitch Type
freq  <- pp1 %>% group_by(Pitcher, TaggedPitchType) %>% tally()
total <- as.numeric(sum(freq$n))
freq$`Usage %` <- round((freq$n / total) * 100, digits = 0)
freq <- subset(freq, select = -c(n))

# Merge Main DataSet with Usage %
pp1 <- merge(pp1, freq)

# Add in Parameters for Strike Zone / Plate
Left   <- -8.5/12
Right  <-  8.5/12
Bottom <- 18.29/12
Top    <- 44.08/12
Width <- (Right - Left)/3
Height <- (Top - Bottom)/3

# Graph, Filtered By Pitch Type == "Fastball"

# Replace the "Fastball" with input$... to Make it Reactive
pp1 <- pp1 %>% filter(TaggedPitchType == "Fastball")

heatplot <- ggplot(pp1, mapping = aes(x = PlateLocSide,
                          y = PlateLocHeight)) +
  stat_density2d_filled() +
  #對 (x = PlateLocSide, y = PlateLocHeight) 做二維核密度估計（KDE），把空間切成等密度的區域（等高線之間的區塊）。
  #和 stat_density2d() 畫線不同，_filled 會直接把區域「填色」。
  #可以用 bins = K 指定區塊數（等級數）；不指定時，ggplot 會自動決定。
  
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill","colour")) +
  # 用我提供的顏色向量去對應這些離散等級。values 必須是一串顏色（"#RRGGBB" 等）。
  # 已經把 heat_colors_interpolated 設為顏色向量（長度 16）。
  
  # The Box
  geom_segment(x = Left, y = Bottom, xend = Right, yend = Bottom)+
  geom_segment(x = Left, y = Top, xend = Right, yend = Top)+
  geom_segment(x = Left, y = Bottom, xend = Left, yend = Top)+
  geom_segment(x = Right, y = Bottom, xend = Right, yend = Top)+
  
  # Horizontal Lines
  geom_segment(x = Left, y = Bottom + Height, xend = Right, yend = Bottom + Height)+
  geom_segment(x = Left, y = Top - Height, xend = Right, yend = Top - Height)+
  
  # Vertical Lines
  geom_segment(x = Left + Width, y = Bottom, xend = Left + Width, yend = Top)+
  geom_segment(x = Right - Width, y = Bottom, xend = Right - Width, yend = Top)+
  
  # Plate
  geom_segment(x = Left, y = 0, xend = Right, yend = 0)+
  geom_segment(x = Left, y = 0, xend = Left, yend = 4.25/12)+
  geom_segment(x = Left, y = 4.25/12, xend = 0, yend = 8.5/12)+
  geom_segment(x = Right, y = 4.25/12, xend = Right, yend = 0)+
  geom_segment(x = 0, y = 8.5/12, xend = Right, yend = 4.25/12)+
  
  xlim(-3, 3) + ylim(0, 5) + ggtitle(paste("Fastball"), paste(pp1$`Usage %`, "%")) +
  theme(
    legend.position = "none", # 隱藏圖例（因為色帶只是背景，不需圖例）
    plot.title = element_text(color = "black", size = 15, face = "bold"), 
    axis.title.x = element_blank(), # 隱藏 X 軸標題
    axis.title.y = element_blank(), # 隱藏 Y 軸標題
    panel.grid.major = element_blank(), # 拿掉主要網格線
    panel.grid.minor = element_blank(), # 拿掉次要網格線
    panel.background = element_blank(), # 去掉灰底（透明），讓你的好球帶線條更乾淨
    panel.border = element_rect(color = "black", size = 1.5, fill = NA) # 外框畫黑邊，透明填滿
  )

heatplot


### Spider Graph
### 我沒有下面的csv
pitcher_data = fread("pitcher_data.csv")
league_date = fread("league_data.csv")

st <- pitcher_data[pitcher_data$Pitcher == "Smith, John", ]

st <- subset(st, select = c(TaggedPitchType, Grade))

st <- st[, list(Grade = mean(Grade, na.rm = TRUE) / 2.5),
         by = "TaggedPitchType"]

st_lg <- league_data[league_data$Starter == "Starter" &
                       league_data$Level  == "MLB", ]

st_lg <- subset(st_lg, select = c(TaggedPitchType, Composite_Grade))

st_lg$Composite_Grade <- st_lg$Composite_Grade / 2.5

r_lg <- recast(st_lg, id.var = "TaggedPitchType",
               TaggedPitchType ~ TaggedPitchType)

r_lg <- subset(r_lg, select = -c(TaggedPitchType))

r_lg <- r_lg %>%
  summarise_each(funs(sum(., na.rm = TRUE)))

r_lg <- as.data.table(r_lg)

r <- recast(st, id.var = "TaggedPitchType", TaggedPitchType ~ TaggedPitchType)

r <- subset(r, select = -c(TaggedPitchType))

r <- r %>%
  summarise_each(funs(sum(., na.rm = TRUE)))

r <- as.data.table(r)

min <- r[nrow(r) + 1, ]
min[is.na(min)] <- 0

max <- r[nrow(r) + 1, ]
max[is.na(max)] <- 100

minmax <- rbind(min, max)

spider <- rbind(minmax, r)

spider <- rbind(spider, r_lg, fill = TRUE)


spider_columns <- colnames(spider)
spider_rows    <- spider[3, ]
spider_rows_lg <- spider[4, ]
spider_rows    <- as.factor(spider_rows)
spider_rows_lg <- as.factor(spider_rows_lg)

fig <- plot_ly(
  type  = 'scatterpolar',
  r     = spider_rows,
  theta = spider_columns,
  fill  = 'toself',
  mode  = 'markers',
  name  = 'Collin Stuff Factor'
)

fig <- fig %>%
  add_trace(
    r     = spider_rows_lg,
    theta = spider_columns,
    fill  = 'toself',
    mode  = 'markers',
    name  = 'Collin Stuff Factor - Potential'
  )

fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range   = c(0, 30)
      )
    ),
    showlegend = TRUE
  )

fig

### Spray Chart
# 1) 依仰角 Angle 建立擊球型態 HitType
#   <  8°  = GB (Ground Ball)
#   8–15°  = LD (Line Drive)
#   > 15°  = FB (Fly Ball)
TestTrackMan$HitType <- TestTrackMan$Angle
TestTrackMan$HitType[TestTrackMan$HitType < 8] <- "GB"
TestTrackMan$HitType[TestTrackMan$HitType >= 8 & TestTrackMan$HitType <= 15] <- "LD"
TestTrackMan$HitType[TestTrackMan$HitType > 15] <- "FB"

# 2) 篩出打者 & 投手慣用手
left <- TestTrackMan %>%
  filter(Batter == "Neto, Clifford", PitcherThrows == "Left")
right <- TestTrackMan %>%
  filter(Batter == "Neto, Clifford", PitcherThrows == "Right")


# 移除沒有 HitType 的列
left <- left[!is.na(left$HitType), ]
right <- right[!is.na(left$HitType), ]

# 3) 畫 Spray Chart （把方位角 Bearing/距離 Distance 轉成 X-Y）
# 已經先得到 left 資料：
# left <- TestTrackMan %>% 
#   filter(Batter == "Neto, Clifford", PitcherThrows == "Left") %>% 
#   filter(!is.na(HitType))

left_pitcher_spray <- ggplot(
  left,
  aes(
    x = (sin(pi/180 * Bearing) * Distance),
    y = (cos(pi/180 * Bearing) * Distance)
  )
) +
  geom_point(aes(color = ExitSpeed), size = 3) +
  scale_color_gradient2(
    low = "blue", high = "red",
    midpoint = mean(90)        # 這行等同 midpoint = 90；想動態可改 mean(left$ExitSpeed, na.rm = TRUE)
  ) +
  xlim(-295, 295) + ylim(0, 450) +
  
  # 界外線（左右各一條）
  geom_segment(x = 0, y = 0, xend = -315, yend = 315, size = 1.2) +
  geom_segment(x = 0, y = 0, xend =  315, yend = 315, size = 1.2) +
  
  # 外野牆弧線（外圍）
  geom_curve(x = -315, y = 315, xend = 315, yend = 315,
             curvature = -0.35, size = 1.2) +
  
  # 內野弧線（內圍）
  geom_curve(x =  -90, y =  90, xend =  90, yend =  88,
             curvature = -0.45, size = 1.2) +
  
  coord_fixed() +                     # 維持等比例座標
  theme_bw() +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  labs(color = " ", title = "Spray Chart")

left_pitcher_spray



right_pitcher_spray <- ggplot(
  right,
  aes(
    x = (sin(pi/180 * Bearing) * Distance),
    y = (cos(pi/180 * Bearing) * Distance)
  )
) +
  geom_point(aes(color = ExitSpeed), size = 3) +
  scale_color_gradient2(
    low = "blue", high = "red",
    midpoint = mean(90)        # 這行等同 midpoint = 90；想動態可改 mean(right$ExitSpeed, na.rm = TRUE)
  ) +
  xlim(-295, 295) + ylim(0, 450) +
  
  # 界外線（左右各一條）
  geom_segment(x = 0, y = 0, xend = -315, yend = 315, size = 1.2) +
  geom_segment(x = 0, y = 0, xend =  315, yend = 315, size = 1.2) +
  
  # 外野牆弧線（外圍）
  geom_curve(x = -315, y = 315, xend = 315, yend = 315,
             curvature = -0.35, size = 1.2) +
  
  # 內野弧線（內圍）
  geom_curve(x =  -90, y =  90, xend =  90, yend =  88,
             curvature = -0.45, size = 1.2) +
  
  coord_fixed() +                     # 維持等比例座標
  theme_bw() +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  labs(color = " ", title = "Spray Chart")

right_pitcher_spray




