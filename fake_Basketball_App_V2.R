#setwd("/Users/scottdaughtry/Desktop/MSHoops")
library(shiny)
library(data.table)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(devtools)
library(DataCombine)
library(geomtextpath)
library(readr)
library(gt)
library(RMySQL)
library(jsonlite)
library(reactable)
library(shinydashboard)
library(shinymanager)

library(DT)
library(RMySQL)
library(jsonlite)
library(httr)
library(rsconnect)
library(readxl)
library(shinythemes)
library(DT)



credentials <- data.frame(
  user = c("sdaughtr", "adam","coachg","utstaff","managers"),
  password = c("volhoops", "profadam","savage","volnumbers", "govols"),
  permissions = c("admin", "admin","user","user", "user")
)


#data 
shotsfliiped <- read_csv("fakeshotsthree.csv")
Pred_Table<- read_csv("Pred_Table.csv")
lucas_table<- read_csv("lucas_table.csv")



# Define the user interface (UI)
ui <- dashboardPage(
  skin = "black",
  title = "",
  dashboardHeader(
    title = "Tennessee Basketball Analytics App",
    titleWidth = "500px"
  ),#dashboad headder
  dashboardSidebar(
    sidebarMenu(
      menuItem("Shot Charts", tabName = "plot1"),
      menuItem("Rankings Page", tabName = "plot2"),
      menuItem("Transfer Portal Predictions", tabName = "plot3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "plot1",
        fluidRow(
          column(
            12,
            selectInput(
              inputId = "player",
              label = "Select Athlete",
              choices = c("Giannis Antetokounmpo", "Nikola Jokic","Shai Gilgeous-Alexander", "Luka Doncic", "Tyrese Haliburton")
            ) # select imput
          ),
        ),#fluid row 1 
        fluidRow(
          column(6,
                 align = "center",
                 box(
                   width = 20,
                   title = "Shot Charts",
                   plotOutput("plot1_left_output")
                 )
          ),#column 1 row 2
          column(
            6,
            align = "center",
            box(
              width = 12,
              title = "Percentage Charts",
              plotOutput("plot1_right_output")
            )
          ), # column 2
        ),#fluid row 2
        fluidRow(
          column(
            12,
            align = "center",
            box(
              width = 12,
              title = "Shot Load Table",
              gt_output("shot_load_table")
            )
          )#column 1 row 3
        )#fluid row 3 
      ),#tab item 1
      tabItem(
        tabName = "plot2",
        fluidRow(
          column(
            6,
            align = "center",
            selectInput(
              inputId = "location",
              label = "Select Specific Location",
              choices = c("Right Block"   ,  "Rim Shots"    ,    "Left Block"  , "Right Short Corner" , "Left Short Corner" ,
                          "Right Elbow"   ,     "High Paint"  ,    "Left Elbow"    ,    "Short Key",  "Right Corner",
                          "Right Wing" ,    "Top Of Key"  ,   "Left Wing" ,"Left Corner")
            ), # select imput
            selectInput(
              inputId = "takout",
              label = "Player Type To Take Out",
              choices = c("None", "Guard", "Big")
            ), # select imput
            numericInput("cutnumber", label = "Enter Minimum Number of Shots", value = 0),
            box(
              width = 12,
              title = "Specific Rankings Table",
              gt_output("rankings_table")
            )#column 1 box 
          ),#column 1 
          column(
            6,
            align = "center",
            box(
              width = 12,
              title = "Location Reference",
              plotOutput("refrence_table")
            )#column 2 box
          )#column 2
        ),#tab 2 fluid row 1
        fluidRow(
          column(
            6, 
            align = "center",
            selectInput(
              inputId = "broad_location",
              label = "Select Broad Location",
              choices = c("Three Pointers"  , "Mid Range" ,"Around the Rim")
            ),
            selectInput(
              inputId = "type_takout_two",
              label = "Player Type To Take Out",
              choices = c("None", "Guard", "Big")
            ), # select imput
            numericInput("cutnumber_two", label = "Enter Minimum Number of Shots", value = 0),
            box(
              width = 12,
              title = "Broad Rankings Table",
              gt_output("rankings_table_two")
            )#box
          )# colum 
        )# tab 2 fuild row 2 
      ),#tab item 2
      tabItem(
        tabName = "plot3",
        fluidRow(
          column(
            12,
            align = "center",
            selectInput(
              inputId = "vcs",
              label = "On Verbal Commits Filter",
              choices = c("Any", "Yes", "No")
            ),
            selectInput(
              inputId = "postion",
              label = "Position Filter",
              choices = c("Any", "G", "F")
            ),
            dateInput("tdate", "Filter To Recent Additions", value = "2024-01-01"),
            numericInput("pppvalue", label = "Enter Minimum PPG In 2023", value = 0),
            checkboxInput("checkbox", "Check if You Want to use 3P% Filter", FALSE),
            numericInput("p3pvalue", label = "Enter Minimum 3P% In 2023", value = 0),
            box( 
              width = 12,
              title = "Transfer Portal Predictions",
              DTOutput("ptable")
            )#box for tab 3
          )#tab 3 column 1 
        ),#tab 3 fluid row 1 
        fluidRow(
          column(
            12,
            align = "center",
            box( 
              width = 12,
              title = "2023 Player Stats",
              DTOutput("boxstats")
            )#box
          )#column 1 row 2
        )#Fluid row 2 tab 3
      )#tab item 3 
    )#tab items 
  )#dashboad body 
)#dashboard page


# Define the server logic
server <- function(input, output) {
  # Generate and render the left plot for Plot 1
  output$plot1_left_output <- renderPlot({
    
    # ------------------------- Creating Court and Plotting
    circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
      angles = seq(0, 2 * pi, length.out = npoints)
      return(data_frame(x = center[1] + radius * cos(angles),
                        y = center[2] + radius * sin(angles)))
    }
    
    width = 50
    height = 94 / 2
    key_height = 19
    inner_key_width = 12
    outer_key_width = 16
    backboard_width = 6
    backboard_offset = 4
    neck_length = 0.5
    hoop_radius = 0.75
    hoop_center_y = backboard_offset + neck_length + hoop_radius
    three_point_radius = 23.75
    three_point_side_radius = 22
    three_point_side_height = 14
    
    court_themes = list(
      light = list(
        court = 'floralwhite',
        lines = 'black',
        text = '#222222',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 1,
        hex_border_color = "#000000"
      ),
      dark = list(
        court = '#000004',
        lines = '#999999',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "#000000"
      ),
      ppt = list(
        court = 'gray15',
        lines = 'white',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "gray15"
      ),
      white = list(
        court = 'white',
        lines = 'black',
        text = 'black',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "gray15"
      )
    )
    
    
    plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
      if (use_short_three) {
        three_point_radius = 22
        three_point_side_height = 0
      }
      
      court_points = data_frame(
        x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
        y = c(height, 0, 0, height, height),
        desc = "perimeter"
      )
      
      court_points = bind_rows(court_points , data_frame(
        x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
        y = c(0, key_height, key_height, 0),
        desc = "outer_key"
      ))
      
      court_points = bind_rows(court_points , data_frame(
        x = c(-backboard_width / 2, backboard_width / 2),
        y = c(backboard_offset, backboard_offset),
        desc = "backboard"
      ))
      
      court_points = bind_rows(court_points , data_frame(
        x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
      ))
      
      foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
      
      foul_circle_top = filter(foul_circle, y > key_height) %>%
        mutate(desc = "foul_circle_top")
      
      foul_circle_bottom = filter(foul_circle, y < key_height) %>%
        mutate(
          angle = atan((y - key_height) / x) * 180 / pi,
          angle_group = floor((angle - 5.625) / 11.25),
          desc = paste0("foul_circle_bottom_", angle_group)
        ) %>%
        filter(angle_group %% 2 == 0) %>%
        select(x, y, desc)
      
      hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
        mutate(desc = "hoop")
      
      restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
        filter(y >= hoop_center_y) %>%
        mutate(desc = "restricted")
      
      three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
        filter(y >= three_point_side_height, y >= hoop_center_y)
      
      three_point_line = data_frame(
        x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
        y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
        desc = "three_point_line"
      )
      
      court_points = bind_rows(
        court_points,
        foul_circle_top,
        foul_circle_bottom,
        hoop,
        restricted,
        three_point_line
      )
      
      
      court_points <- court_points
      
      ggplot() +
        geom_path(
          data = court_points,
          aes(x = x, y = y, group = desc),
          color = court_theme$lines
        ) +
        coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
        theme_minimal(base_size = 22) +
        theme(
          text = element_text(color = court_theme$text),
          plot.background = element_rect(fill = 'gray15', color = 'gray15'),
          panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
          legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.text = element_text(size = rel(1.0))
        )
    }
    
    name<- input$player
    
    shotsfliiped<- shotsfliiped %>% filter(Full_name==name)
    
    # Assuming you have a predefined basketball court theme in court_themes
    plot_court(court_themes$ppt, use_short_three = FALSE) +
      
      # Plot shot "points" with x & y locations
      geom_point(data = shotsfliiped, 
                 aes(x = maybeX, y = maybeY, color = shotResult, fill = shotResult), 
                 size = 2, shape = 21, stroke = 0.5) +
      
      # Customize the plot appearance
      labs(title = paste(name,"Shot Data"),
           x = "X-Axis Label",
           y = "Y-Axis Label",
           color = "Shot Result",
           fill = "Shot Result") +
      
      # Manually set colors for makes (white) and misses (red)
      scale_fill_manual(values = c("MAKE" = "white", "MISS" = "red")) +
      scale_color_manual(values = c("MAKE" = "white", "MISS" = "red")) +
      
      # Customize the legend appearance
      theme(legend.position = "right",  # Adjust legend position
            legend.title = element_text(face = "bold"),  # Bold legend title
            legend.text = element_text(size = 10),  # Adjust legend text size
            legend.key.size = unit(1.5, "lines"))
    
  })
  
  output$plot1_right_output <- renderPlot({
    # ------------------------- Creating Court and Plotting
    circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
      angles = seq(0, 2 * pi, length.out = npoints)
      return(data_frame(x = center[1] + radius * cos(angles),
                        y = center[2] + radius * sin(angles)))
    }
    
    width = 50
    height = 94 / 2
    key_height = 19
    inner_key_width = 12
    outer_key_width = 16
    backboard_width = 6
    backboard_offset = 4
    neck_length = 0.5
    hoop_radius = 0.75
    hoop_center_y = backboard_offset + neck_length + hoop_radius
    three_point_radius = 23.75
    three_point_side_radius = 22
    three_point_side_height = 14
    
    court_themes = list(
      light = list(
        court = 'floralwhite',
        lines = 'black',
        text = '#222222',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 1,
        hex_border_color = "#000000"
      ),
      dark = list(
        court = '#000004',
        lines = '#999999',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "#000000"
      ),
      ppt = list(
        court = 'gray15',
        lines = 'white',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "gray15"
      ),
      white = list(
        court = 'white',
        lines = 'black',
        text = 'black',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "gray15"
      )
    )
    
    
    plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
      if (use_short_three) {
        three_point_radius = 22
        three_point_side_height = 0
      }
      
      court_points = data_frame(
        x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
        y = c(height, 0, 0, height, height),
        desc = "perimeter"
      )
      
      court_points = bind_rows(court_points , data_frame(
        x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
        y = c(0, key_height, key_height, 0),
        desc = "outer_key"
      ))
      
      court_points = bind_rows(court_points , data_frame(
        x = c(-backboard_width / 2, backboard_width / 2),
        y = c(backboard_offset, backboard_offset),
        desc = "backboard"
      ))
      
      court_points = bind_rows(court_points , data_frame(
        x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
      ))
      
      foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
      
      foul_circle_top = filter(foul_circle, y > key_height) %>%
        mutate(desc = "foul_circle_top")
      
      foul_circle_bottom = filter(foul_circle, y < key_height) %>%
        mutate(
          angle = atan((y - key_height) / x) * 180 / pi,
          angle_group = floor((angle - 5.625) / 11.25),
          desc = paste0("foul_circle_bottom_", angle_group)
        ) %>%
        filter(angle_group %% 2 == 0) %>%
        select(x, y, desc)
      
      hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
        mutate(desc = "hoop")
      
      restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
        filter(y >= hoop_center_y) %>%
        mutate(desc = "restricted")
      
      three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
        filter(y >= three_point_side_height, y >= hoop_center_y)
      
      three_point_line = data_frame(
        x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
        y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
        desc = "three_point_line"
      )
      
      court_points = bind_rows(
        court_points,
        foul_circle_top,
        foul_circle_bottom,
        hoop,
        restricted,
        three_point_line
      )
      
      
      court_points <- court_points
      
      ggplot() +
        geom_path(
          data = court_points,
          aes(x = x, y = y, group = desc),
          color = court_theme$lines
        ) +
        coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
        theme_minimal(base_size = 22) +
        theme(
          text = element_text(color = court_theme$text),
          plot.background = element_rect(fill = 'gray15', color = 'gray15'),
          panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
          legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.text = element_text(size = rel(1.0))
        )
    }
    
    name<- input$player
    
    shotsfliiped<- shotsfliiped %>% filter(Full_name==name)
    DKallthree<- shotsfliiped  %>% 
      filter(shotOrigin=="FIELD" )
    
    paste(round(sum(DKallthree$shotResult == "MAKE") / nrow(DKallthree) * 100, 0), "%", sep = "")
    
    DKleftcorner<- shotsfliiped %>% 
      filter(shotOrigin=="FIELD" & maybeX< -22 & maybeY< 16)
    
    paste(round(sum(DKleftcorner$shotResult == "MAKE") / nrow(DKleftcorner) * 100, 0), "%", sep = "")
    
    DKleftwing<- shotsfliiped %>% 
      filter(shotOrigin=="FIELD" & maybeX< -10 & maybeX>-22 &maybeY>16)
    
    paste(round(sum(DKleftwing$shotResult == "MAKE") / nrow(DKleftwing) * 100, 0), "%", sep = "")
    
    DKthreekey<- shotsfliiped %>% 
      filter(shotOrigin=="FIELD" & maybeX> -10 & maybeX< 10  &maybeY>16)
    
    paste(round(sum(DKthreekey$shotResult == "MAKE") / nrow(DKthreekey) * 100, 0), "%", sep = "")
    
    DKrightwing<- shotsfliiped  %>% 
      filter(shotOrigin=="FIELD" & maybeX> 10 & maybeX<22 &maybeY>16)
    
    paste(round(sum(DKrightwing$shotResult == "MAKE") / nrow(DKrightwing) * 100, 0), "%", sep = "")
    
    DKrightcorner<- shotsfliiped  %>% 
      filter(shotOrigin=="FIELD" & maybeX> 22 & maybeY< 16)
    
    paste(round(sum(DKrightcorner$shotResult == "MAKE") / nrow(DKrightcorner) * 100, 0), "%", sep = "")
    
    shortcornerright<- shotsfliiped  %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX< -10  &maybeY<12)
    
    paste(round(sum(shortcornerright$shotResult == "MAKE") / nrow(shortcornerright) * 100, 0), "%", sep = "")
    
    rightblock<- shotsfliiped   %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX> -10 & maybeX< -5 &maybeY<12)
    
    paste(round(sum(rightblock$shotResult == "MAKE") / nrow(rightblock) * 100, 0), "%", sep = "")
    
    rimshot<- shotsfliiped   %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX> -5 & maybeX< 5 &maybeY<12)
    
    paste(round(sum(rimshot$shotResult == "MAKE") / nrow(rimshot) * 100, 0), "%", sep = "")
    
    leftblock<- shotsfliiped  %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX> 5 & maybeX< 10 &maybeY<12)
    
    paste(round(sum(leftblock$shotResult == "MAKE") / nrow(leftblock) * 100, 0), "%", sep = "")
    
    leftshortcorner<- shotsfliiped  %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX> 10  &maybeY<12)
    
    paste(round(sum(leftshortcorner$shotResult == "MAKE") / nrow(leftshortcorner) * 100, 0), "%", sep = "")
    
    highpaint<- shotsfliiped %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX> -10 & maybeX< 10 &maybeY>12 & maybeY<18)
    
    paste(round(sum(highpaint$shotResult == "MAKE") / nrow(highpaint) * 100, 0), "%", sep = "")
    
    rightelbow<- shotsfliiped %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX< -10  &maybeY>12 )
    
    paste(round(sum(rightelbow$shotResult == "MAKE") / nrow(rightelbow) * 100, 0), "%", sep = "")
    
    
    leftelbow<- shotsfliiped %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX> 10  &maybeY>12 )
    
    paste(round(sum(leftelbow$shotResult == "MAKE") / nrow(leftelbow) * 100, 0), "%", sep = "")
    
    shortkey<- shotsfliiped %>% 
      filter(shotOrigin %in% c("MID_RANGE","PAINT")& maybeX> -10 & maybeX< 10  & maybeY>18 &maybeY<28.16)
    
    paste(round(sum(shortkey$shotResult == "MAKE") / nrow(shortkey) * 100, 0), "%", sep = "")
    
    
    # Create the basketball court plot
    plot_court(court_themes$dark, use_short_three = FALSE)+
      #right corner
      geom_text(aes(x = -22, y = 5, label = paste(round(sum(DKleftcorner$shotResult == "MAKE") / nrow(DKleftcorner) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #right corner count 
      geom_text(aes(x = -22, y = 2, label = paste(sum(DKleftcorner$shotResult == "MAKE"),"/", nrow(DKleftcorner), sep = "")), size = 4, color = "white", fontface = "bold")+
      #right wing 
      geom_text(aes(x = -20, y = 24, label = paste(round(sum(DKleftwing$shotResult == "MAKE") / nrow(DKleftwing) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #right wing count 
      geom_text(aes(x = -20, y = 21, label = paste(sum(DKleftwing$shotResult == "MAKE"),"/", nrow(DKleftwing), sep = "")), size = 4, color = "white", fontface = "bold")+
      #key 
      geom_text(aes(x = 0, y = 33, label = paste(round(sum(DKthreekey$shotResult == "MAKE") / nrow(DKthreekey) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #key count 
      geom_text(aes(x = 0, y = 30, label = paste(sum(DKthreekey$shotResult == "MAKE"),"/", nrow(DKthreekey), sep = "")), size = 4, color = "white", fontface = "bold")+
      #left wing 
      geom_text(aes(x = 21, y = 24, label = paste(round(sum(DKrightwing$shotResult == "MAKE") / nrow(DKrightwing) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #left wing count 
      geom_text(aes(x = 21, y = 21, label = paste(sum(DKrightwing$shotResult == "MAKE"),"/", nrow(DKrightwing), sep = "")), size = 4, color = "white", fontface = "bold")+
      #left corner
      geom_text(aes(x = 22, y = 5, label = paste(round(sum(DKrightcorner$shotResult == "MAKE") / nrow(DKrightcorner) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #left corner count 
      geom_text(aes(x = 22, y = 2, label = paste(sum(DKrightcorner$shotResult == "MAKE"),"/", nrow(DKrightcorner), sep = "")), size = 4, color = "white", fontface = "bold")+
      #short conrer 
      geom_text(aes(x = -15, y = 11, label = paste(round(sum(shortcornerright$shotResult == "MAKE") / nrow(shortcornerright) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #short Coner count
      geom_text(aes(x = -15, y = 8, label = paste(sum(shortcornerright$shotResult == "MAKE"),"/", nrow(shortcornerright), sep = "")), size = 4, color = "white", fontface = "bold")+
      #rightblock 
      geom_text(aes(x = -7, y = 6, label = paste(round(sum(rightblock$shotResult == "MAKE") / nrow(rightblock) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #rightblock count
      geom_text(aes(x = -7, y = 3, label = paste(sum(rightblock$shotResult == "MAKE"),"/", nrow(rightblock), sep = "")), size = 4, color = "white", fontface = "bold")+
      #rimshot
      geom_text(aes(x = 0, y = 10, label = paste(round(sum(rimshot$shotResult == "MAKE") / nrow(rimshot) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #rim count
      geom_text(aes(x = 0, y = 7, label = paste(sum(rimshot$shotResult == "MAKE"),"/", nrow(rimshot), sep = "")), size = 4, color = "white", fontface = "bold")+
      #left block
      geom_text(aes(x = 7, y = 6, label = paste(round(sum(leftblock$shotResult == "MAKE") / nrow(leftblock) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #left block count
      geom_text(aes(x = 7, y = 3, label = paste(sum(leftblock$shotResult == "MAKE"),"/", nrow(leftblock), sep = "")), size = 4, color = "white", fontface = "bold")+
      #left short corner
      geom_text(aes(x = 15, y = 11, label = paste(round(sum(leftshortcorner$shotResult == "MAKE") / nrow(leftshortcorner) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #left short corner count
      geom_text(aes(x = 15, y = 8, label = paste(sum(leftshortcorner$shotResult == "MAKE"),"/", nrow(leftshortcorner), sep = "")), size = 4, color = "white", fontface = "bold")+
      #highpaint
      geom_text(aes(x = 0, y = 16, label = paste(round(sum(highpaint$shotResult == "MAKE") / nrow(highpaint) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #high paint  count
      geom_text(aes(x = 0, y = 13, label = paste(sum(highpaint$shotResult == "MAKE"),"/", nrow(highpaint), sep = "")), size = 4, color = "white", fontface = "bold")+
      #rightelbow
      geom_text(aes(x = -13, y = 19, label = paste(round(sum(rightelbow$shotResult == "MAKE") / nrow(rightelbow) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #rightelbow  count
      geom_text(aes(x = -13, y = 16, label = paste(sum(rightelbow$shotResult == "MAKE"),"/", nrow(rightelbow), sep = "")), size = 4, color = "white", fontface = "bold")+
      #leftelbow
      geom_text(aes(x = 13, y = 19, label = paste(round(sum(leftelbow$shotResult == "MAKE") / nrow(leftelbow) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #leftelbow count
      geom_text(aes(x = 13, y = 16, label = paste(sum(leftelbow$shotResult == "MAKE"),"/", nrow(leftelbow), sep = "")), size = 4, color = "white", fontface = "bold")+
      #shortkey
      geom_text(aes(x = 0, y = 24, label = paste(round(sum(shortkey$shotResult == "MAKE") / nrow(shortkey) * 100, 0), "%", sep = "")), size = 7, color = "white", fontface = "bold")+
      #shortkey count
      geom_text(aes(x = 0, y = 21, label = paste(sum(shortkey$shotResult == "MAKE"),"/", nrow(shortkey), sep = "")), size = 4, color = "white", fontface = "bold")
    
    
  })
  
  output$shot_load_table <- render_gt({
    playername<- input$player
    
    arow<- matrix(nrow = 1, ncol = 3)
    
    empty_matrix <- matrix(nrow = 14, ncol = 9)
    
    powers<- c("High","Medium","Low")
    areas<- c("Right Block"   ,  "Rim Shots"    ,    "Left Block"  , "Right Short Corner" , "Left Short Corner" ,
              "Right Elbow"   ,     "High Paint"  ,    "Left Elbow"    ,    "Short Key",  "Right Corner",
              "Right Wing" ,    "Top Of Key"  ,   "Left Wing" ,"Left Corner"  )
    
    for (f in 1:length(areas)) {
      
      
      for (i in 1:length(powers)) {
        
        
        make<-shotsfliiped %>% 
          filter(Full_name== playername & Specific_Location== areas[f]& Load_Value== powers[i]& shotResult== "MAKE")
        
        tot<- shotsfliiped %>% 
          filter(Full_name== playername & Specific_Location== areas[f]& Load_Value== powers[i])
        
        #print(nrow(make)/nrow(tot))
        
        a<-nrow(make)/nrow(tot)
        b<- nrow(make)
        c<- nrow(tot)
        
        arow<- round(c(a,b,c),2)
        if (i==1) {
          empty_matrix[f,1:3]<- arow
        }
        if (i==2) {
          empty_matrix[f,4:6]<- arow
        }
        if (i==3) {
          empty_matrix[f,7:9]<- arow
        }
      }
    }
    
    empty_matrix<- cbind(areas,empty_matrix)
    
    
    column_names <- c("Shot Location","High Load %", "High Makes", "High Shots","Medium Load %", "Medium Makes", "Medium Shots","Low Load %", "Low Makes", "Low Shots")
    colnames(empty_matrix) <- column_names
    
    shot_load_table<- as.data.frame(empty_matrix)
    
    #colnames(shot_load_table)
    
    shot_load_table %>% 
      gt()%>%
      cols_align(
        align = "center"
      ) %>%
      tab_header(
        title = "Shot Percentage by Shot Load",
        subtitle = "From Kinexon Shot Tracking Data"
      )  %>% tab_style(
        style = list(
          cell_fill(color = "#2ECC71")
        ),
        locations = cells_body(
          columns = `High Load %`,
          rows = `High Load %` >= .60
        ))%>% tab_style(
          style = list(
            cell_fill(color = "#F8C471")
          ),
          locations = cells_body(
            columns = `High Load %`,
            rows = `High Load %` > .35 & `High Load %` < .6
          ))%>% tab_style(
            style = list(
              cell_fill(color = "#EC7063")
            ),
            locations = cells_body(
              columns = `High Load %`,
              rows = `High Load %` > 0.01 & `High Load %` <= .35
            )) %>% tab_style(
              style = list(
                cell_fill(color = "#2ECC71")
              ),
              locations = cells_body(
                columns = `Medium Load %`,
                rows = `Medium Load %` >= .60
              ))%>% tab_style(
                style = list(
                  cell_fill(color = "#F8C471")
                ),
                locations = cells_body(
                  columns = `Medium Load %`,
                  rows = `Medium Load %` > .35 & `Medium Load %` < .6
                ))%>% tab_style(
                  style = list(
                    cell_fill(color = "#EC7063")
                  ),
                  locations = cells_body(
                    columns = `Medium Load %`,
                    rows = `Medium Load %` > 0.01 & `Medium Load %` <= .35
                  ))%>% tab_style(
                    style = list(
                      cell_fill(color = "#2ECC71")
                    ),
                    locations = cells_body(
                      columns = `Low Load %`,
                      rows = `Low Load %` >= .60
                    ))%>% tab_style(
                      style = list(
                        cell_fill(color = "#F8C471")
                      ),
                      locations = cells_body(
                        columns = `Low Load %`,
                        rows = `Low Load %` > .35 & `Low Load %` < .6
                      ))%>% tab_style(
                        style = list(
                          cell_fill(color = "#EC7063")
                        ),
                        locations = cells_body(
                          columns = `Low Load %`,
                          rows = `Low Load %` > 0.01 & `Low Load %` <= .35
                        ))
  })
  
  output$rankings_table <- render_gt({
    
    rankings_table<- shotsfliiped %>%
      group_by(Specific_Location, Full_name) %>%
      summarise(makes = sum(shotResult == "MAKE"), total_shots = sum(type=="shot"))
    
    rankings_table$precentage<- round( rankings_table$makes / rankings_table$total_shots, 4)*100
    
    
    rankings_table <- rankings_table %>%
      mutate(player_type = case_when(
        Full_name %in% c("Shai Gilgeous-Alexander", "Luka Doncic", "Tyrese Haliburton") ~ "Guard",
        Full_name %in% c("Giannis Antetokounmpo", "Nikola Jokic") ~ "Big",
        TRUE ~ NA  # Default condition
      ))
    
    
    
    column_names <- c("Shot Location","Player Name", "Total Makes", "Total Shots", "Shooting Percentage", "Player Type")
    
    colnames(rankings_table) <- column_names
    
    
    location <- input$location
    
    typetakeout<- input$takout
    
    cutoff<- input$cutnumber
    
    ranking_output<- rankings_table %>% 
      filter(`Shot Location`==location & `Player Type` !=typetakeout & `Total Shots`>cutoff) %>% 
      select(`Player Name`, `Total Makes`, `Total Shots`, `Shooting Percentage`)  %>%
      arrange(desc(`Shooting Percentage`))
    
    ranking_output<- as.data.frame(ranking_output) 
    
    ranking_output$`Shot Location`<- NULL
    
    ranking_output %>% 
      gt()%>%
      cols_align(
        align = "center"
      ) %>%
      tab_header(
        title = "Rankings Table",
        subtitle = paste( " For",location, "Location" )
      )%>% tab_style(
        style = list(
          cell_fill(color = "#2ECC71")
        ),
        locations = cells_body(
          columns = `Shooting Percentage`,
          rows = `Shooting Percentage` >= 75
        ))%>% tab_style(
          style = list(
            cell_fill(color = "#F8C471")
          ),
          locations = cells_body(
            columns = `Shooting Percentage`,
            rows = `Shooting Percentage` < 75 & `Shooting Percentage` >= 45
          ))%>% tab_style(
            style = list(
              cell_fill(color = "#F9E3D6")
            ),
            locations = cells_body(
              columns = `Shooting Percentage`,
              rows = `Shooting Percentage` < 45
            ))
  })
  
  output$refrence_table <- renderPlot({
    # ------------------------- Creating Court and Plotting
    circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
      angles = seq(0, 2 * pi, length.out = npoints)
      return(data_frame(x = center[1] + radius * cos(angles),
                        y = center[2] + radius * sin(angles)))
    }
    
    width = 50
    height = 94 / 2
    key_height = 19
    inner_key_width = 12
    outer_key_width = 16
    backboard_width = 6
    backboard_offset = 4
    neck_length = 0.5
    hoop_radius = 0.75
    hoop_center_y = backboard_offset + neck_length + hoop_radius
    three_point_radius = 23.75
    three_point_side_radius = 22
    three_point_side_height = 14
    
    court_themes = list(
      light = list(
        court = 'floralwhite',
        lines = 'black',
        text = '#222222',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 1,
        hex_border_color = "#000000"
      ),
      dark = list(
        court = '#000004',
        lines = '#999999',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "#000000"
      ),
      ppt = list(
        court = 'gray15',
        lines = 'white',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "gray15"
      ),
      white = list(
        court = 'white',
        lines = 'black',
        text = 'black',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "gray15"
      )
    )
    
    
    plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
      if (use_short_three) {
        three_point_radius = 22
        three_point_side_height = 0
      }
      
      court_points = data_frame(
        x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
        y = c(height, 0, 0, height, height),
        desc = "perimeter"
      )
      
      court_points = bind_rows(court_points , data_frame(
        x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
        y = c(0, key_height, key_height, 0),
        desc = "outer_key"
      ))
      
      court_points = bind_rows(court_points , data_frame(
        x = c(-backboard_width / 2, backboard_width / 2),
        y = c(backboard_offset, backboard_offset),
        desc = "backboard"
      ))
      
      court_points = bind_rows(court_points , data_frame(
        x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
      ))
      
      foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
      
      foul_circle_top = filter(foul_circle, y > key_height) %>%
        mutate(desc = "foul_circle_top")
      
      foul_circle_bottom = filter(foul_circle, y < key_height) %>%
        mutate(
          angle = atan((y - key_height) / x) * 180 / pi,
          angle_group = floor((angle - 5.625) / 11.25),
          desc = paste0("foul_circle_bottom_", angle_group)
        ) %>%
        filter(angle_group %% 2 == 0) %>%
        select(x, y, desc)
      
      hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
        mutate(desc = "hoop")
      
      restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
        filter(y >= hoop_center_y) %>%
        mutate(desc = "restricted")
      
      three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
        filter(y >= three_point_side_height, y >= hoop_center_y)
      
      three_point_line = data_frame(
        x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
        y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
        desc = "three_point_line"
      )
      
      court_points = bind_rows(
        court_points,
        foul_circle_top,
        foul_circle_bottom,
        hoop,
        restricted,
        three_point_line
      )
      
      
      court_points <- court_points
      
      ggplot() +
        geom_path(
          data = court_points,
          aes(x = x, y = y, group = desc),
          color = court_theme$lines
        ) +
        coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
        theme_minimal(base_size = 22) +
        theme(
          text = element_text(color = court_theme$text),
          plot.background = element_rect(fill = 'gray15', color = 'gray15'),
          panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
          legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.text = element_text(size = rel(1.0))
        )
    }
    
    # Create the basketball court plot
    plot_court(court_themes$dark, use_short_three = FALSE)+
      #right corner
      geom_text(aes(x = -21, y = 5, label = "Right Corner"), size = 5, color = "white", fontface = "bold")+
      #right wing 
      geom_text(aes(x = -20, y = 24, label = "Right Wing"), size = 5, color = "white", fontface = "bold")+
      #key 
      geom_text(aes(x = 0, y = 33, label = "Top of Key"), size = 5, color = "white", fontface = "bold")+
      #left wing 
      geom_text(aes(x = 21, y = 24, label = "Left Wing"), size = 5, color = "white", fontface = "bold")+
      #left corner
      geom_text(aes(x = 21, y = 5, label = "Left Corner"), size =5, color = "white", fontface = "bold")+
      #short conrer 
      geom_text(aes(x = -15, y = 9, label = "Right Short Corner"), size = 5, color = "white", fontface = "bold")+
      #rightblock 
      geom_text(aes(x = -7, y = 6, label = "Right Block"), size = 5, color = "white", fontface = "bold")+
      #rimshot
      geom_text(aes(x = 0, y = 11, label = "Rim Shots"), size = 5, color = "white", fontface = "bold")+
      #left block
      geom_text(aes(x = 7, y = 6, label = "Left Block"), size = 5, color = "white", fontface = "bold")+
      #left short corner
      geom_text(aes(x = 15, y = 9, label = "Left Short Corner"), size = 5, color = "white", fontface = "bold")+
      #highpaint
      geom_text(aes(x = 0, y = 16, label = "High Paint"), size = 5, color = "white", fontface = "bold")+
      #rightelbow
      geom_text(aes(x = -13, y = 19, label = "Right Elbow"), size = 5, color = "white", fontface = "bold")+
      #leftelbow
      geom_text(aes(x = 13, y = 19, label = "Left Elbow"), size = 5, color = "white", fontface = "bold")+
      #shortkey
      geom_text(aes(x = 0, y = 24, label = "Short Key"), size = 5, color = "white", fontface = "bold")
    
  })
  
  output$rankings_table_two <- render_gt({
    
    rankings_table<- shotsfliiped %>%
      group_by(Broad_location_, Full_name) %>%
      summarise(makes = sum(shotResult == "MAKE"), total_shots = sum(type=="shot"))
    
    rankings_table$precentage<- round( rankings_table$makes / rankings_table$total_shots, 4)*100
    
    
    rankings_table <- rankings_table %>%
      mutate(player_type = case_when(
        Full_name %in% c("Shai Gilgeous-Alexander", "Luka Doncic", "Tyrese Haliburton") ~ "Guard",
        Full_name %in% c("Giannis Antetokounmpo", "Nikola Jokic") ~ "Big",
        TRUE ~ NA  # Default condition
      ))
    
    
    
    column_names <- c("Broad Location","Player Name", "Total Makes", "Total Shots", "Shooting Percentage", "Player Type")
    
    colnames(rankings_table) <- column_names
    
    
    location <- input$broad_location
    
    typetakeout<- input$type_takout_two
    
    cutoff<- input$cutnumber_two
    
    ranking_output<- rankings_table %>% 
      filter(`Broad Location`==location & `Player Type` !=typetakeout & `Total Shots`>cutoff) %>% 
      select(`Player Name`, `Total Makes`, `Total Shots`, `Shooting Percentage`)  %>%
      arrange(desc(`Shooting Percentage`))
    
    ranking_output<- as.data.frame(ranking_output) 
    
    ranking_output$`Broad Location`<- NULL
    
    ranking_output %>% 
      gt()%>%
      cols_align(
        align = "center"
      ) %>%
      tab_header(
        title = "Rankings Table",
        subtitle = paste( " For",location, "Location" )
      )%>% tab_style(
        style = list(
          cell_fill(color = "#2ECC71")
        ),
        locations = cells_body(
          columns = `Shooting Percentage`,
          rows = `Shooting Percentage` >= 75
        ))%>% tab_style(
          style = list(
            cell_fill(color = "#F8C471")
          ),
          locations = cells_body(
            columns = `Shooting Percentage`,
            rows = `Shooting Percentage` < 75 & `Shooting Percentage` >= 45
          ))%>% tab_style(
            style = list(
              cell_fill(color = "#F9E3D6")
            ),
            locations = cells_body(
              columns = `Shooting Percentage`,
              rows = `Shooting Percentage` < 45
            ))
  })
  
  filtered_data <- reactive({
    
    chnage<- input$vcs
    
    if (chnage == "Yes") {
      chnage <- "No"
    } else if (chnage == "No") {
      chnage <- "Yes"
    } else {
      chnage
    }
    
    ppp <- input$postion
    
    if (ppp == "G") {
      ppp <- "F"
    } else if (ppp == "F") {
      ppp <- "G"
    } else {
      ppp
    }
    
    if(input$checkbox==T) {
      l_filt <- lucas_table %>% filter(Position!=ppp & PPG >= input$pppvalue & `3FG%` >= input$p3pvalue )
      
      l_filt$`Player Name` <- toupper(l_filt$`Player Name`)
      Pred_Table$`Player Name` <- toupper(Pred_Table$`Player Name`)
    } else {
      l_filt <- lucas_table %>% filter(Position!=ppp & PPG >= input$pppvalue )
      
      l_filt$`Player Name` <- toupper(l_filt$`Player Name`)
      Pred_Table$`Player Name` <- toupper(Pred_Table$`Player Name`)
    }
    
    
    
    l_filt$`Player Name` <- gsub("[.,]", "", l_filt$`Player Name`)
    Pred_Table$`Player Name` <- gsub("[.,]", "", Pred_Table$`Player Name`)
    
    
    Pred_Table<- Pred_Table %>% filter(`On Verbal Commits?`!= chnage & `Player Name` %in% c(l_filt$`Player Name`)
                                       & Date_Added >= input$tdate)
    
    
    l_filt$`Player Name` <- stringr::str_to_title((l_filt$`Player Name`))
    Pred_Table$`Player Name` <- stringr::str_to_title((Pred_Table$`Player Name`))
    
    
    Pred_Table
  })
  
  output$ptable<- renderDT({
    
    Pred_Table_filt <- filtered_data()
    
    
    Pred_Table_filt_nodate <-Pred_Table_filt %>%
      select(-Date_Added)
    
    #Pred_Table<- read_csv("Pred_Table.csv")
    datatable(Pred_Table_filt_nodate, 
              options = list(
                dom = 'lfrtip',
                ordering = TRUE,
                pageLength = 10, # Set the number of rows per page
                language = list(
                  info = "Showing _START_ to _END_ of _TOTAL_ entries",
                  infoEmpty = "Showing 0 to 0 of 0 entries",
                  infoFiltered = "(filtered from _MAX_ total entries)",
                  lengthMenu = "Show _MENU_ entries",
                  search = "Search:"
                )
              ),
              caption = list(
                title = "Predicted Next Year Stats for Transfers"
              ),
              rownames = FALSE
    )
    
  })
  
  
  
  output$boxstats<- renderDT({
    
    Pred_Table_filt <- filtered_data()
    
    # lucas_table<- read_csv("lucas_table.csv")
    # Pred_Table<- read_csv("Pred_Table.csv")
    # Pred_Table_filt<-   Pred_Table
    # 
    
    
    # lucas_table$`Player Name` <- toupper(lucas_table$`Player Name`)
    # Pred_Table$`Player Name` <- toupper(Pred_Table$`Player Name`)
    # 
    # 
    # lucas_table <- lucas_table %>%
    #   filter(`Player Name` %in% Pred_Table$`Player Name`)
    # 
    # lucas_table$`Player Name` <- stringr::str_to_title((lucas_table$`Player Name`))
    # Pred_Table$`Player Name` <- stringr::str_to_title((Pred_Table$`Player Name`))
    
    
    lucas_table$`Player Name` <- toupper(lucas_table$`Player Name`)
    Pred_Table_filt$`Player Name` <- toupper(Pred_Table_filt$`Player Name`)
    
    lucas_table$`Player Name` = gsub(" JR.", "", x = lucas_table$`Player Name`, fixed = TRUE)
    lucas_table$`Player Name` = gsub(".", "", x = lucas_table$`Player Name`, fixed = TRUE)
    Pred_Table_filt$`Player Name` = gsub(" JR", "", x = Pred_Table_filt$`Player Name`, fixed = TRUE)
    Pred_Table_filt$`Player Name` = gsub(".", "", x = Pred_Table_filt$`Player Name`, fixed = TRUE)
    
    # Pred_Table$`Player Name` <- toupper(Pred_Table$`Player Name`)
    # Pred_Table$`Player Name` = gsub(" JR", "", x = Pred_Table$`Player Name`, fixed = TRUE)
    #Pred_Table$`Player Name` = gsub(".", "", x = Pred_Table$`Player Name`, fixed = TRUE)
    
    # lucas_table <- lucas_table %>%
    # filter(`Player Name` %in% Pred_Table$`Player Name`)
    
    # setdiff(Pred_Table$`Player Name`, lucas_table$`Player Name`)
    
    
    # Columns to join on in df1, note now we are using "Second_School","transferYear"
    
    lucas_table <-  left_join(Pred_Table_filt, lucas_table, by = c("Player Name"="Player Name","First School Name"="School Name"))
    
    
    
    
    
    
    vc_join<-  Pred_Table_filt %>%  select(`Player Name`, `On Verbal Commits?`)
    
    lucas_table<-  right_join(lucas_table,vc_join,"Player Name")
    
    lucas_table$`Player Name` <- stringr::str_to_title((lucas_table$`Player Name`))
    Pred_Table_filt$`Player Name` <- stringr::str_to_title((Pred_Table_filt$`Player Name`))
    
    lucas_table<- lucas_table %>% select("Player Name","First School Name","Position","Height","Year","Games Started","PPG","RebPG",
                                         "AstPG","+/-","FG%","3FG%","FT%","On Verbal Commits?.y" )
    lucas_table$`On Verbal Commits?` <- lucas_table$`On Verbal Commits?.y`
    lucas_table$`On Verbal Commits?.y`<-NULL
    #setdiff(Pred_Table$`Player Name`, lucas_table$`Player Name`)
    
    library(DT)
    
    
    # render_link <- JS(
    #   "function(data, type, row, meta) {",
    #   "  if(type === 'display') {",
    #   "    data = '<a href=\"' + data + '\" target=\"_blank\">' + data + '</a>';",
    #   "  }",
    #   "  return data;",
    #   "}")
    
    datatable(lucas_table, 
              options = list(
                dom = 'lfrtip',
                ordering = TRUE,
                pageLength = 10,
                language = list(
                  info = "Showing _START_ to _END_ of _TOTAL_ entries",
                  infoEmpty = "Showing 0 to 0 of 0 entries",
                  infoFiltered = "(filtered from _MAX_ total entries)",
                  lengthMenu = "Show _MENU_ entries",
                  search = "Search:"
                )
              ),
              caption = list(
                title = "2023 Player Stats"
              ),
              rownames = FALSE
    ) 
    
  })
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
}



# Create the Shiny app
secure_ui <- secure_app(ui)


shinyApp(ui = secure_ui, server = server)
# shinyApp(ui = secure_ui, server = secure_server)
