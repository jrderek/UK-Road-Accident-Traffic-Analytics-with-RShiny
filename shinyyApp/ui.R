library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(fresh)
library(plotly)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library("RColorBrewer")
library(geojsonR)
library("purrr")
library(chron)
library("lubridate")
library(leaflet)
library(leaflet.extras)
library(tmap)
library("sf")
library("rgdal")
library(mapview)
library(ggmap)
library(plyr)
library(leafsync)
library(leaflegend)
library(raster)
library(magick)
library(caTools)


uk_vehicles_region_summary <- read.csv("uk-data/uk_vehicles_region_summary.csv",
                                       header = T, sep = ",")
uk_vehicles_lad_summary <- read.csv("uk-data/uk_vehicles_lad_summary.csv",
                                    header = T, sep = ",")
uk_casualties_per_accident <- read.csv("uk-data/uk_casualties_per_accident.csv",
                                       header = T, sep = ",")


# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#434C5E",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#d6d8da", ##D8DEE9
    box_bg = "#d6d8da", 
    info_box_bg = "#d6d8da"
  )
)


## 1. header -------------------------------
title <- tags$a(href = "https://www.gov.uk/government/organisations/department-for-transport",
                icon("road"),
                "UK Road Traffic and Accident Insights",
                style = "color: white; font-size: 25px")
header <- 
  dashboardHeader( title = title, 
                   disable = FALSE, 
                   titleWidth  = 550,
                   tags$li(class = "dropdown",
                           tags$style(".navbar {min-height:100px}
                                      .main-header .navbar {
                                      margin-left: 500px;
                                      }
                                      .main-header .logo {
                                      width: 500px;
                                      }"),
                           
                   ),
                   dropdownMenu( type = 'message',
                                 messageItem(
                                   from = "TR_SharedMailbox@mbie.govt.uk",#'Feedback and suggestions',
                                   message =  "",
                                   icon = icon("envelope"),
                                   href = "mailto:TR_SharedMailbox@mbie.govt.uk"
                                 ),
                                 icon = icon('comment')
                   ),
                   dropdownMenu( type = 'message',
                                 icon = icon("share-alt"),
                                 messageItem(
                                   from = 'Twitter',
                                   message = "",
                                   icon = icon("twitter"),
                                   href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                 ),
                                 messageItem(
                                   from = 'Facebook',
                                   message = "",
                                   icon = icon("facebook"),
                                   href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                                 ),
                                 messageItem(
                                   from = 'Google+',
                                   message = "",
                                   icon = icon("google-plus"),
                                   href = "https://plus.google.com/share?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                                 ),
                                 messageItem(
                                   from = 'LinkedIn',
                                   message = "",
                                   icon = icon("linkedin"),
                                   href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&title=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                 ),
                                 messageItem(
                                   from = 'Tumblr',
                                   message = "",
                                   icon = icon("tumblr"),
                                   href = "http://www.tumblr.com/share?v=3&u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&t=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                 )
                   )
                   
  )


#### 2 - Sidebar
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",
    ## 1st tab show the Main dashboard -----------
    menuItem( "Traffic Map Analytics", tabName = 'dashboard', icon = icon('car-side')),
    conditionalPanel(condition = "input.sidebar === 'dashboard'",
                     selectInput("Region", "Select Region:", choices = unique(uk_vehicles_region_summary$region_name),
                                 selected = "London"),
                     radioButtons("Vehicles",
                                  tags$p("Select Vehicles:",tags$br()), 
                                  choices = c("All vehicles", "All Motor Vehicles", "Cars and Taxis", 
                                              'Buses and Coaches', 'Lorries and Vans',
                                              "All Heavy Goods Vehicles", "Two wheeled motor vehicles", 
                                              'Pedal Cycles'),
                                  selected = "All vehicles",
                                  inline = F,
                                  width = "300px"),
                     sliderInput("Year", label = "Select Year", min = 2000, max = 2018, 
                                 step = 1, sep = '', value = 2000, width = "400px")),
    menuItem( "Accident Map Analytics", tabName = '2nddashboard', icon = icon('car-crash')),
    conditionalPanel(condition = "input.sidebar === '2nddashboard'",
                     selectInput("region", "Select Region:", choices = unique(uk_casualties_per_accident$Region.Country),
                                 selected = "London"),
                     radioButtons("condition",
                                  tags$p("Accident Condition:",tags$br()), 
                                  choices = c("All Conditions", "One way street", "Single carriageway",
                                              "Dual carriageway", "Slip road", "30 Speed Limit", "40 Speed Limit",
                                              "60 Speed Limit", "70 Speed Limit", "Daylight: Street light present",
                                              "Darkness: Street lights present", "Darkness: No street lighting"),
                                  selected = "All Conditions",
                                  inline = F,
                                  width = "300px"),
                     sliderInput("year", label = "Select Year", min = 2005, max = 2014, 
                                 step = 1, sep = '', value = 2005, width = "400px")),
    menuItem( "Accident Charts", tabName = '3rddashboard', icon = icon('truck-monster')),
    conditionalPanel(condition = "input.sidebar === '3rddashboard'",
                     sliderInput("yr", label = "Select Year", min = 2005, max = 2014, 
                                 step = 1, sep = '', value = 2005, width = "400px")
                     
    ),
    menuItem( "Traffic Charts", tabName = '4thdashboard', icon = icon('tachometer-alt')),
    conditionalPanel(condition = "input.sidebar === '4thdashboard'",
                     radioButtons("Vehs",
                                  tags$p("Select Vehicles:",tags$br()), 
                                  choices = c("All vehicles", "All Motor Vehicles", "Cars and Taxis", 
                                              'Buses and Coaches', 'Lorries and Vans',
                                              "All Heavy Goods Vehicles", "Two wheeled motor vehicles", 
                                              'Pedal Cycles'),
                                  selected = "All vehicles",
                                  inline = F,
                                  width = "300px"),
                     sliderInput("Yr", label = "Select Year", min = 2000, max = 2018, 
                                 step = 1, sep = '', value = 2000, width = "400px")),
    menuItem( "Gifs & Images", tabName = '5thdashboard', icon = icon('images'))
  )
)

### 3 - dashboardBody
body <- dashboardBody(
  use_theme(mytheme),
  tags$head(
    tags$style(HTML(".fa-car-crash { padding-left: 10px; padding-right: 30px; }")),
    tags$style(HTML(".fa-car-side { padding-left: 10px; padding-right: 30px; }")),
    tags$style(HTML(".fa-tachometer-alt { padding-left: 10px; padding-right: 30px; }")),
    tags$style(HTML(".fa-images { padding-left: 10px; padding-right: 30px; }")),
    tags$style(HTML(".fa-truck-monster { padding-left: 10px; padding-right: 30px; }")),
    tags$style(HTML(".fa-flag-checkered { font-size: 24px; color: white; }")),
    tags$style(HTML(".fa-flag { font-size: 24px; color: white; }")),
    
    ## modify the dashboard's skin color
    tags$style(HTML('.main-sidebar { font-size: 20px; font-weight: bold; }
                    ')
    ),
    tags$style(type = "text/css", "
      .irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
      .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
      .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
      .irs-grid-text {font-family: 'arial'; color: white; bottom: 40px; z-index: 1;}
      .irs-grid-pol {display: none;}
      .irs-max {font-family: 'arial'; color: black;}
      .irs-min {font-family: 'arial'; color: black;}
      .irs-single {color:black; background:#6666ff;}
      .irs-slider {width: 30px; height: 30px; top: 22px;}
    "),
  ),
  # tabitems
  tabItems(
    ## 3.1 Main dashboard ----------------------------------------------------------
    tabItem( tabName = 'dashboard',
             h2(strong(textOutput("map1title")), style = "color:#3c4564" , align = 'center'),
             fluidRow( 
               uiOutput('map',  width = "100%", height = "100vh") 
             ),
             h2(paste0("")),
             fluidRow( 
               uiOutput('map2') 
             )
             
    ),
    tabItem( tabName = '2nddashboard',
             h2(strong(textOutput("mapAcctitle")), style = "color:#3c4564" , align = 'center'),
             fluidRow( 
               uiOutput('mapAcc',  width = "100%", height = "100vh")
             ),
             h2(paste0("")),
             fluidRow( 
               uiOutput('mapAcc2') 
             )
    ),
    tabItem( tabName = '3rddashboard',
             h2(strong(textOutput("Acccharttitle")), style = "color:#3c4564" , align = 'center'),
             fluidRow(
               valueBoxOutput("London"),
               valueBoxOutput("NorthEast"),
               valueBoxOutput("NorthWest")
             ),
             fluidRow(
               valueBoxOutput("EastMidlands"),
               valueBoxOutput("WestMidlands"),
               valueBoxOutput("SouthWest")
             ),
             fluidRow(
               valueBoxOutput("Wales"),
               valueBoxOutput("Eastern"),
               valueBoxOutput("SouthEast")
             ),
             fluidRow(
               valueBoxOutput("Yorkshire"),
               valueBoxOutput("Scotland")
             ),
             h3(paste0("")),
             fluidRow( column( width = 12,h4("", align = 'center'), 
                               plotlyOutput("regiongraph1"),
                               div(style = "height:240px;"), align="center")
             ),
             fluidRow( column( width = 12,h4("", align = 'center'), 
                               plotlyOutput("regiongraph2"),
                               div(style = "height:240px;"), align="center")
             ),
             fluidRow( column( width = 6,h4("", align = 'center'), 
                               plotlyOutput("highwaygraph1"),
                               div(style = "height:350px;")),
                       column( width = 6,h4("", align = 'center'), 
                               plotlyOutput("highwaygraph2"),
                               div(style = "height:350px;"))
             ),
             h3(paste0("")),
             fluidRow( column( width = 12,h4("", align = 'center'), 
                               plotlyOutput("monthgraph"),
                               div(style = "height:350px;"), align="center")),
             fluidRow(column( width = 12,h4("", align = 'center'), 
                               plotlyOutput("hourgraph"),
                               div(style = "height:350px;"),align="center")
             ),
             fluidRow(column( width = 12,h4("", align = 'center'), 
                               plotlyOutput("piechart1"),
                               div(style = "height:350px;"), align="center"),
                      ),
             fluidRow(column( width = 12,h4("", align = 'center'), 
                              plotlyOutput("piechart2"),
                              div(style = "height:350px;")), align="center")
    ),
    tabItem( tabName = '4thdashboard',
             h2(strong(textOutput("Trafficcharttitle")), style = "color:#3c4564" , align = 'center'),
             fluidRow(
               valueBoxOutput("Lond"),
               valueBoxOutput("NE"),
               valueBoxOutput("NW")
             ),
             fluidRow(
               valueBoxOutput("EM"),
               valueBoxOutput("WM"),
               valueBoxOutput("SW")
             ),
             fluidRow(
               valueBoxOutput("Ws"),
               valueBoxOutput("East"),
               valueBoxOutput("SE")
             ),
             fluidRow(
               valueBoxOutput("Yorks"),
               valueBoxOutput("Scot")
             ),
             h3(paste0("")),
             fluidRow( column( width = 12,h4("", align = 'center'), 
                               plotlyOutput("trafficgraph1"),
                               div(style = "height:240px;"), align="center")
             ),
             fluidRow( column( width = 12,
                               plotlyOutput("trafficgraph2"),
                               div(style = "height:240px;"), align="center")),
             fluidRow( column( width = 6,h4("", align = 'center'), 
                               plotlyOutput("trafficgraph3"),
                               div(style = "height:350px;")),
                       column( width = 6,h4("", align = 'center'), 
                               plotlyOutput("trafficgraph4"),
                               div(style = "height:350px;"))
             ),
             fluidRow( column( width = 6,h4("", align = 'center'), 
                               plotlyOutput("trafficgraph5"),
                               div(style = "height:350px;")),
                       column( width = 6,h4("", align = 'center'), 
                               plotlyOutput("trafficgraph6"),
                               div(style = "height:350px;"))
             ),
             fluidRow(column( width = 12,h4("", align = 'center'), 
                              plotlyOutput("trafficpiechart1"),
                              div(style = "height:350px;"), align="center"),
             ),
             fluidRow(column( width = 12,h4("", align = 'center'), 
                              plotlyOutput("trafficgraph7"),
                              div(style = "height:350px;")), align="center")
    ),
    tabItem( tabName = '5thdashboard',
             h2(strong(textOutput("Giftitle")), style = "color:#3c4564" , align = 'center'),
             fluidRow( column( width = 6,h4("", align = 'center'), 
                               img(src="uk_reg_traffic_animation.gif", align = "center",height='750px',width='650px'),
                               div(style = "height:115px;")),
                       column( width = 6,h4("", align = 'center'), 
                               img(src="uk_traffic_lad_animation.gif", align = "center",height='750px',width='650px'),
                               div(style = "height:115px;"))
                       ),
             fluidRow( column( width = 6,h4("", align = 'center'), 
                               img(src="uk_reg_acc_animation2.gif", align = "center",height='750px',width='650px'),
                               div(style = "height:115px;")),
                       column( width = 6,h4("", align = 'center'), 
                               img(src="uk_lad_acc_animation.gif", align = "center",height='750px',width='650px'),
                               div(style = "height:115px;"))
             ),
             h3(strong(paste0("London Maps as Gifs")), style = "color:#3c4564" , align = 'center'),
             fluidRow( column( width = 6,h4("", align = 'center'), 
                               img(src="uk_traffic_lond_animation.gif", align = "center",height='750px',width='650px'),
                               div(style = "height:115px;")),
                       column( width = 6,h4("", align = 'center'), 
                               img(src="uk_lond_acc_animation.gif", align = "center",height='750px',width='650px'),
                               div(style = "height:115px;"))
             ))
    
  )
)


ui <- dashboardPage(
  header,
  sidebar,
  body
)