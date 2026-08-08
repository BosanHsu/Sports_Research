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
library(shiny)
library(shinythemes)
library(shinydashboard)
library(lubridate)


# Import Data
TestTrackMan = fread("TestTrackMan.csv")
TM_percentiles = fread("TM_percentiles.csv")

# Convert Character Date into Proper Format
TestTrackMan$Date <- mdy(TestTrackMan$Date)

# Part 1 of APP: UI

ui <- navbarPage(
  "Bosan Baseball Lab", theme = shinytheme("flatly"),
  tabPanel("Players",
           tabsetPanel(
             tabPanel("Pitchers",
                      tabsetPanel(
                        tabPanel("Ploty 2D",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("Pitcher Team",
                                                 label = "Choose Team",
                                                 choices = levels(as.factor(TestTrackMan$PitcherTeam))),
                                     selectInput("Pitcher",
                                                 label = "Choose Pitcher",
                                                 choices = levels(as.factor(TestTrackMan$Pitcher))),
                                     dateRangeInput("Date",
                                                    label = "Choose Date Range",
                                                    start = min(TestTrackMan$Date),
                                                    end = max(TestTrackMan$Date),
                                                    min = min(TestTrackMan$Date),
                                                    max = max(TestTrackMan$Date),
                                                    format = "yyyy-mm-dd",
                                                    separator = "to"),
                                     checkboxGroupInput("Pitch",
                                                        label = "Choose Pitch Type(s)",
                                                        choices = levels(as.factor(TestTrackMan$TaggedPitchType))),
                                     radioButtons("BatterSide",
                                                  label = "Choose Batter Side",
                                                  choices = levels(as.factor(TestTrackMan$BatterSide))),
                                     width = 3),
                                   
                                   mainPanel(
                                     fluidRow(plotOutput("percentiles")),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     fluidRow(DT::DTOutput("percentiles_Data")),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     fluidRow(plotOutput("Velo_Chart")),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     fluidRow(plotOutput("Strike_Zone")),
                                   )
                                 )
                        ),
                        tabPanel("Ploty 3D",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("Pitcher Team 3D",
                                                 label = "Choose Team 3D",
                                                 choices = levels(as.factor(TestTrackMan$PitcherTeam))),
                                     selectInput("Pitcher",
                                                 label = "Choose Pitcher 3D",
                                                 choices = levels(as.factor(TestTrackMan$Pitcher))),
                                     dateRangeInput("Date",
                                                    label = "Choose Date Range 3D",
                                                    start = min(TestTrackMan$Date),
                                                    end = max(TestTrackMan$Date),
                                                    min = min(TestTrackMan$Date),
                                                    max = max(TestTrackMan$Date),
                                                    format = "yyyy-mm-dd",
                                                    separator = "to"),
                                     checkboxGroupInput("Pitch 3D",
                                                        label = "Choose Pitch Type(s)",
                                                        choices = levels(as.factor(TestTrackMan$TaggedPitchType))),
                                     width = 2),
                                   
                                   mainPanel(
                                     br(),
                                     fluidRow(
                                       box(
                                         plotlyOutput("SampleOutput", height = "500px"),
                                         width = 12
                                       )
                                     ),
                                     br(),
                                     br(),
                                   )
                                 )
                        ),
                      )
             )
           )
  )
)


# Part 2: Server
server = function(input, output, session){
  # Input Reactions: Pitcher Tab
  # Pitcher Based on Team
  observeEvent(
    input$"Pitcher Team",
    updateSelectInput(session,
                      "Pitcher",
                      "Choose Pitcher",
                      choices = levels(factor(filter(TestTrackMan,
                                                     PitcherTeam == isolate(input$"Pitcher Team"))$Pitcher))
    )
  )
  # Date Range (Based on Pitcher)
  observeEvent(
    input$Pitcher,
    updateDateRangeInput(session,
                         "Date",
                         "Choose Date Range",
                         start = min(TestTrackMan$Date),
                         end = max(TestTrackMan$Date)
    )
  )
  
  # Pitch Type (Based on Pitcher)
  observeEvent(
    input$Pitcher,
    updateCheckboxGroupInput(session,
                             "Pitch",
                             "Choose Pitch Type(s)",
                             choices = levels(factor(filter(TestTrackMan,
                                                            Pitcher == isolate(input$Pitcher))$TaggedPitchType))
    )
  )
  
  # Batter Side
  observeEvent(
    input$Pitcher,
    {
      bs_choices <- TestTrackMan %>%
        dplyr::filter(
          PitcherTeam == isolate(input$`Pitcher Team`),
          Pitcher == isolate(input$Pitcher)
        ) %>%
        dplyr::pull(BatterSide) %>%
        unique() %>%
        na.omit() %>%
        as.character()
      
      updateRadioButtons(
        session,
        inputId = "BatterSide",
        label   = "Choose Batter Side",
        choices = bs_choices,
        selected = if (length(bs_choices)) bs_choices[1] else character(0)
      )
    }
  )
  
  # percentiles
  
  output$percentiles <- renderPlot({
    
    # FB Max Velo
    
    MaxVelo <- TM_percentiles%>%
      filter(PitcherTeam == input$"Pitcher Team",
             Pitcher == input$Pitcher,
             TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
      ggplot(aes(x = MaxVelo_percentile, y = TaggedPitchType, colour = MaxVelo_percentile)) +
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
    
    MaxSpin <- TM_percentiles%>%
      filter(PitcherTeam == input$"Pitcher Team",
             Pitcher == input$Pitcher,
             TaggedPitchType %in% c("Fastball", "High", "Low")) %>%
      ggplot(aes(x = MaxSpin_percentile, y = TaggedPitchType, colour = MaxSpin_percentile)) +
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
    
    
  })
  
  
  
  
  # percentiles_Data
  
  output$percentiles_Data <- renderDT({
    
    TMP <- TM_percentiles%>%
      filter(TaggedPitchType == "Fastball",
             PitcherTeam == input$"Pitcher Team",
             Pitcher == input$Pitcher)
    
    
    TMP <- subset(TMP, select = -c(1,8,10))
    
    datatable(TMP, caption = htmltools::tags$caption( style = 'caption-side: top; 
                                                  text-align: center; color:black; font-size:200% ;',
                                                      'Fastball percentiles'), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1), `border-left` = "solid 1px") %>% formatStyle(c(7), `border-right` = "solid 1px")
    
    
  })
  
  
  
  # Velo_Chart
  
  output$Velo_Chart <- renderPlot({
    
    TestTrackMan%>%
      filter(PitcherTeam == input$"Pitcher Team",
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             BatterSide %in% input$BatterSide) %>%
      ggplot(aes(x = PitchNo, y = RelSpeed, colour = TaggedPitchType)) +
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
    
  })
  
  
  
  # Strike_Zone
  
  output$Strike_Zone <- renderPlot({
    
    TestTrackMan%>%
      filter(PitcherTeam == input$"Pitcher Team",
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             BatterSide %in% input$BatterSide) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
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
      xlim(-2.5,2.5) + ylim(-.5, 5) + ggtitle("Strike Zone")
    
    
  })
  
}

# Part 3: ShinyApp

shinyApp(ui = ui, server = server)

