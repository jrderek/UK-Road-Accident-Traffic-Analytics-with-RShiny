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
library(scales)


##readin json map
uk_json <- rgdal::readOGR("uk_shape_file/map.topojson")
uk_json_lad <- rgdal::readOGR("uk_shape_file/uk_lad.topojson")
uk_json_acc <- uk_json
names(uk_json)[names(uk_json) == "EER13NM"] <- "region_name"
names(uk_json_acc)[names(uk_json_acc) == "EER13NM"] <- "Region.Country"
##rename region in json df--the order needs to be the same
json_regions_names = c("Eastern")
region_names = c("East of England")
#merge
uk_json$region_name <- mapvalues(uk_json$region_name, from=json_regions_names, to=region_names)
##traffic
uk_rd_cat_summary <- read.csv("uk-data/uk_rd_cat_summary.csv",
                              header = T, sep = ",")
uk_hw_traffic_miles <- read.csv("uk-data/uk_hw_traffic_miles.csv",
                                header = T, sep = ",")
uk_rd_traffic_miles <- read.csv("uk-data/uk_rd_traffic_miles.csv",
                                header = T, sep = ",")
uk_vehicles_region <- read.csv("uk-data/uk_vehicles_region.csv",
                               header = T, sep = ",")
uk_vehicles_region_summary <- read.csv("uk-data/uk_vehicles_region_summary.csv",
                                       header = T, sep = ",")
uk_json_map <- merge(uk_json, uk_vehicles_region_summary, by = "region_name", duplicateGeoms = T)
##accident
uk_casualties_per_accident <- read.csv("uk-data/uk_casualties_per_accident.csv",
                                       header = T, sep = ",")
uk_json_acc_map <- merge(uk_json_acc, uk_casualties_per_accident, by = "Region.Country", duplicateGeoms = T)
##lad manipulation
cam_json_names <- c("East Cambridgeshire", "South Cambridgeshire", "Fenland", "Cambridge", "Huntingdonshire")
cum_json_names <- c("Carlisle", "South Lakeland", "Allerdale", "Copeland", "Barrow-in-Furness", "Eden")
derb_json_names <- c("Chesterfield", "High Peak", "Amber Valley", "Derbyshire Dales", "North East Derbyshire", 
                     "Bolsover", "Erewash", "South Derbyshire")
dev_json_names <- c("East Devon", "North Devon", "Exeter", "Torridge", "South Hams", "West Devon", "North Dorset",
                    "Mid Devon", "Teignbridge")
dor_json_names <- c("East Dorset", "West Dorset")
estsuss_json_names <- c("Hastings", "Lewes", "Wealden", "Eastbourne", "Rother")
ess_json_names <- c("Brentwood", "Colchester", "Maldon", "Basildon", "Uttlesford", "Castle Point", "Epping Forest",
                    "Rochford", "Braintree", "Chelmsford", "Harlow", "Tendring")
glou_json_names <- c("Forest of Dean", "Cheltenham", "Tewkesbury", "Gloucester", "Cotswold", "Stroud")
hamp_json_names <- c("Eastleigh", "Hart", "Basingstoke and Deane", "Rushmoor", "Fareham", 
                     "Havant", "Test Valley", "East Hampshire", "Gosport", "New Forest", "Winchester")
kent_json_names <- c("Ashford", "Dover", "Sevenoaks", "Canterbury", "Thanet", "Gravesham", "Tonbridge and Malling",
                     "Dartford", "Maidstone", "Swale", "Tunbridge Wells")
lanch_json_names <- c("Burnley", "Hyndburn", "Preston", "South Ribble", "Chorley", "Lancaster", "Ribble Valley",
                      "West Lancashire", "Fylde", "Pendle", "Rossendale", "Wyre")
leich_json_names <- c("Blaby", "Hinckley and Bosworth", "Charnwood", "Oadby and Wigston", "Melton",
                      "Harborough", "North West Leicestershire")
linc_json_names <- c("Lincoln", "South Kesteven", "Boston", "North Kesteven", "West Lindsey", "East Lindsey", 
                     "South Holland")
nor_json_names <- c("Broadland", "North Norfolk", "Great Yarmouth", "Norwich", "Breckland", "King's Lynn and West Norfolk", "South Norfolk")
north_json_names <- c("Corby", "Kettering", "Wellingborough", "Daventry", "Northampton", "East Northamptonshire", 
                      "South Northamptonshire")
nyork_json_names <- c("Harrogate", "Scarborough", "Craven", "Richmondshire", "Selby", "Hambleton", "Ryedale")
nott_json_names <- c("Bassetlaw", "Mansfield", "Broxtowe", "Newark and Sherwood", "Ashfield", "Gedling", 
                     "Rushcliffe")
ofd_json_names <- c("Cherwell", "Vale of White Horse", "Oxford", "West Oxfordshire", "South Oxfordshire")
som_json_names <- c("Sedgemoor", "West Somerset", "South Somerset", "Mendip", "Taunton Deane")
staff_json_names <- c("Lichfield", "Stafford", "Cannock Chase", "Newcastle-under-Lyme", "Staffordshire Moorlands",
                      "East Staffordshire", "South Staffordshire", "Tamworth")
suf_json_names <- c("Babergh", "Mid Suffolk", "Ipswich", "Suffolk Coastal")
surr_json_names <- c("Waveney", "Guildford", "Runnymede", "Tandridge", "Elmbridge", "Mole Valley", "Spelthorne",
                     "Waverley", "Epsom and Ewell", "Reigate and Banstead", "Surrey Heath", "Woking")
warw_json_names <- c("North Warwickshire", "Stratford-on-Avon", "Nuneaton and Bedworth", "Warwick", "Rugby")
wstsuss_json_names <- c("Arun", "Horsham", "Chichester", "Mid Sussex", "Adur", "Crawley", "Worthing")
wors_json_names <- c("Bromsgrove", "Worcester", "Malvern Hills", "Wychavon", "Redditch", "Wyre Forest")
hert_json_names <- c("St Albans", "Stevenage", "Welwyn Hatfield", "East Hertfordshire", "Three Rivers",
                     "North Hertfordshire", "Watford", "Broxbourne", "Dacorum", "Hertsmere")
pert_json_names <- c("Perth and Kinross")
rh_json_names <- c("Rhondda Cynon Taf")
chseast_json_names <- c("Cheshire East")
dumm_json_names <- c("Dumfries and Galloway")
arg_json_names <- c("Argyll and Bute")
chswst_json_names <- c("Cheshire West and Chester")
chs_json_names <- c("Christchurch")
vale_json_names <- c("Vale of Glamorgan")
dur_json_names <- c("County Durham")
corn_json_names <- c("Cornwall")

##
lad_list <- list(cam_json_names, cum_json_names, derb_json_names, dev_json_names, dor_json_names, estsuss_json_names, 
                 ess_json_names, glou_json_names, hamp_json_names, kent_json_names, lanch_json_names,
                 leich_json_names, linc_json_names, nor_json_names, north_json_names, nyork_json_names, nott_json_names,
                 ofd_json_names, som_json_names, staff_json_names, suf_json_names, surr_json_names, warw_json_names, 
                 wstsuss_json_names, wors_json_names, hert_json_names, pert_json_names, rh_json_names,
                 chseast_json_names, dumm_json_names, arg_json_names, chswst_json_names, chs_json_names, vale_json_names,
                 dur_json_names, corn_json_names)
lad_names <- c("Cambridgeshire", "Cumbria", "Derbyshire", "Devon", "Dorset", "East Sussex", "Essex", "Gloucestershire", "Hampshire", "Kent", "Lancashire", "Leicestershire", "Lincolnshire",
               "Norfolk", "Northamptonshire", "North Yorkshire", "Nottinghamshire", "Oxfordshire", "Somerset",
               "Staffordshire", "Suffolk", "Surrey", "Warwickshire", "West Sussex", "Worcestershire", "Hertfordshire",
               "Perth & Kinross", "Rhondda, Cynon, Taff", "East Cheshire", "Dumfries & Galloway", "Argyll & Bute",
               "West Cheshire", "Bournemouth", "The Vale of Glamorgan", "Durham", "Cornwall excluding Isles of Scilly")

for (i in seq_along(lad_list)) {
  uk_json_lad[uk_json_lad$LAD13NM %in% unlist(lad_list[i]),"LAD13NM"] <- lad_names[i]
}
names(uk_json_lad)[names(uk_json_lad) == "LAD13NM"] <- "local_authority_name"
##traffic
uk_vehicles_lad_summary <- read.csv("uk-data/uk_vehicles_lad_summary.csv",
                                    header = T, sep = ",")
uk_lad_json_map <- merge(uk_json_lad, uk_vehicles_lad_summary, by = "local_authority_name", duplicateGeoms = T)
##accident
uk_accident_lad <- read.csv("uk-data/uk_accident_lad.csv",
                            header = T, sep = ",")
uk_json_acc_lad_map <- merge(uk_json_lad, uk_accident_lad, by ="local_authority_name", duplicateGeoms = T)

uk_accident_data <- read.csv("uk-data/uk_accident_data.csv",
                             header = T, sep = ",")
uk_acc_long_lat <- read.csv("uk-data/uk_acc_long_lat.csv",
                            header = T, sep = ",")

##traffics road
uk_traffic_long_lat <- read.csv("uk-data/uk_traffic_long_lat.csv",
                                header = T, sep = ",")
uk_vehicles_rd_summary <- read.csv("uk-data/uk_vehicles_rd_summary.csv",
                                   header = T, sep = ",")
uk_shape_file <- rgdal::readOGR("uk_shape_file/2018-MRDB-minimal.shp",
                                layer = "2018-MRDB-minimal")
PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
uk_shape_file <- spTransform(uk_shape_file, PRO)
names(uk_shape_file)[names(uk_shape_file) == "RoadNumber"] <- "road_name"
uk_road_shp_map <- merge(uk_shape_file, uk_vehicles_rd_summary, by = "road_name", duplicateGeoms = T)

##highway accident data
uk_notorious_acc_highway <- read.csv("uk-data/uk_notorious_acc_highway.csv",
                                     header = T, sep = ",")
uk_safe_acc_highway <- read.csv("uk-data/uk_safe_acc_highway.csv",
                                header = T, sep = ",")
#months & hrs accident data
uk_hour_acc <- read.csv("uk-data/uk_hour_acc.csv",
                        header = T, sep = ",")
uk_month_acc <- read.csv("uk-data/uk_month_acc.csv",
                         header = T, sep = ",")

uk_month_acc$month <- factor(uk_month_acc$month, levels = month.abb)

##speed limit table
uk_accident_spd_limit <- read.csv("uk-data/uk_accident_spd_limit.csv",
                                  header = T, sep = ",")

##order uk_veh_summary table
uk_vehicles_region_order <- uk_vehicles_region_summary[
  order(uk_vehicles_region_summary$year,
        uk_vehicles_region_summary$avg_traffic_flow,
        uk_vehicles_region_summary$region_name,
        uk_vehicles_region_summary$vehicles_names
  ),]

light_list <- unique(uk_accident_data$Light_Conditions)
rd_list <- unique(uk_accident_data$Road_Type)
spd_list <- unique(uk_accident_data$Speed_limit)

legend_font <- list(
  font = list(
    family = "sans-serif",
    size = 16,
    color = "#434C5E"),
  bgcolor = "#d6d8da")

region_symbols <- c('diamond-open-dot', 'cross', 'cross-open', 'triangle-left-open', 'hourglass', 'hourglass-open',
                    'star-triangle-down-dot', 'star-square','star-square-open', 'hourglass', 'bowtie')

server <- function(input, output) {
  
  filtered_acc_data <- reactive({
    
    if (input$condition %in% light_list){
      uk_accident_data[which((uk_accident_data$Year == input$year) & (uk_accident_data$Region.Country == input$region) 
                                            & (uk_accident_data$Light_Conditions == input$condition)),]
    } else if (input$condition %in% rd_list) {
      uk_accident_data[which((uk_accident_data$Year == input$year) & (uk_accident_data$Region.Country == input$region) 
                                            & (uk_accident_data$Road_Type == input$condition)),]
    } else if (input$condition %in% spd_list) {
      uk_accident_data[which((uk_accident_data$Year == input$year) & (uk_accident_data$Region.Country == input$region) 
                                            & (uk_accident_data$Speed_limit == input$condition)),]
    } else {
      uk_accident_data[which((uk_accident_data$Year == input$year) & (uk_accident_data$Region.Country == input$region)),]
    }
  })
  
  filtered_region_summary <- reactive({
    uk_vehicles_region_summary[which((uk_vehicles_region_summary$year == input$Yr) & (uk_vehicles_region_summary$vehicles_names == input$Vehs)),]
  })
  
  filtered_casualties_data <- uk_casualties_per_accident[which((uk_casualties_per_accident$accident_condition == "All Conditions")),]
  
  filtered_uk_json <- reactive({
    uk_json_map[which((uk_json_map$year == input$Year) & (uk_json_map$vehicles_names == input$Vehicles)),]
  })
  
  filtered_uk_json_acc <- reactive({
    uk_json_acc_map[which((uk_json_acc_map$Year == input$year) & 
                            (uk_json_acc_map$accident_condition == input$condition)),]
  })
  
  filtered_uk_json_lad <- reactive({
    uk_lad_json_map[which((uk_lad_json_map$year == input$Year) & (uk_lad_json_map$vehicles_names == input$Vehicles)),]
  })
  
  filtered_uk_json_lad_acc <- reactive({
    uk_json_acc_lad_map[which((uk_json_acc_lad_map$Year == input$year) & 
                                (uk_json_acc_lad_map$accident_condition == input$condition)),]
  })
  
  filtered_uk_json_region <- reactive({
    uk_lad_json_map[which((uk_lad_json_map$year == input$Year) & (uk_lad_json_map$vehicles_names == input$Vehicles) 
                          & (uk_lad_json_map$region_name == input$Region)),]
  })
  
  filtered_uk_json_acc_region <- reactive({
    uk_json_acc_lad_map[which((uk_json_acc_lad_map$Year == input$year) & (uk_json_acc_lad_map$accident_condition == input$condition) 
                          & (uk_json_acc_lad_map$Region.Country == input$region)),]
  })
  
  filtered_uk_rd_region <- reactive({
    uk_road_shp_map[which((uk_road_shp_map$year == input$Year) & (uk_road_shp_map$vehicles_names == input$Vehicles) 
                          & (uk_road_shp_map$region_name == input$Region)),]
  })
  
  filtered_uk_lat_long <- reactive({
    uk_traffic_long_lat[which((uk_traffic_long_lat$year == input$Year) 
                              & (uk_traffic_long_lat$region_name == input$Region)),]
  })
  
  filtered_uk_acc_lat_long <- reactive({
    uk_acc_long_lat[which((uk_acc_long_lat$Year == input$year) 
                              & (uk_acc_long_lat$Region.Country == input$region)),]
  })
  
  output$map1title <- renderText(
    paste("Miles covered by ", input$Vehicles, " in ", input$Year, " in UK")
  )
  
  output$map <- renderUI({
    pal1 <- colorNumeric("plasma", filtered_uk_json()$avg_traffic_flow)
    map1 <- leaflet(filtered_uk_json()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-1.9,54.5, zoom = 5) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                  fillColor = ~pal1(avg_traffic_flow),
                  label = ~paste0(region_name, ": ", formatC(avg_traffic_flow, big.mark = ",")),
                  labelOptions = labelOptions(style = list(
                    "color" = "black",
                    "font-family" = "Cambria",
                    "font-style" = "italic",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "15px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  ), direction = "auto")) %>%
      addLegend(data = filtered_uk_json(), pal =colorNumeric("plasma", filtered_uk_json()$avg_traffic_flow), 
                values = ~avg_traffic_flow, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(x)), 
                title = ~paste0("# Miles")) %>%
      addResetMapButton()
    
    pal2 <- colorNumeric("plasma", filtered_uk_json_lad()$avg_traffic_flow)
    map2 <- leaflet(filtered_uk_json_lad()) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-1.9,54.5, zoom = 5) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal2(avg_traffic_flow),
                  label = ~paste0(local_authority_name,"(",region_name,")", ": ", formatC(avg_traffic_flow, big.mark = ",")),
                  labelOptions = labelOptions(style = list(
                    "color" = "black",
                    "font-family" = "Cambria",
                    "font-style" = "italic",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  ), direction = "auto")) %>%
      addLegend(data = filtered_uk_json_lad(), pal = colorNumeric("plasma", filtered_uk_json_lad()$avg_traffic_flow), 
                values = ~avg_traffic_flow, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(x)),
                title = ~paste0("# Miles")) %>%
      addResetMapButton()
    
    sync(map1, map2, 
         sync = "all", no.initial.sync = TRUE)
    
  })
  

  output$mapAcctitle <- renderText(
    paste("Num of Casualties in ", input$year, " in UK under ", input$condition)
  )
  
  output$mapAcc <- renderUI({
    pal1 <- colorNumeric("YlOrRd", filtered_uk_json_acc()$num_of_casualties)
    map1 <- leaflet(filtered_uk_json_acc()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-1.9,54.5, zoom = 5) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                  fillColor = ~pal1(num_of_casualties),
                  label = ~paste0(Region.Country, ": ", formatC(num_of_casualties, big.mark = ",")),
                  labelOptions = labelOptions(style = list(
                    "color" = "black",
                    "font-family" = "Cambria",
                    "font-style" = "italic",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "15px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  ), direction = "auto")) %>%
      addLegend(data = filtered_uk_json_acc(), pal =colorNumeric("YlOrRd", filtered_uk_json_acc()$num_of_casualties), 
                values = ~num_of_casualties, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(x)), 
                title = ~paste0("# Casualties")) %>%
      addResetMapButton()
    
    pal2 <- colorNumeric("YlOrRd", filtered_uk_json_lad_acc()$num_of_casualties)
    map2 <- leaflet(filtered_uk_json_lad_acc()) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-1.9,54.5, zoom = 5) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal2(num_of_casualties),
                  label = ~paste0(Highway.Authority,"(",Region.Country,")", ": ", formatC(num_of_casualties, big.mark = ",")),
                  labelOptions = labelOptions(style = list(
                    "color" = "black",
                    "font-family" = "Cambria",
                    "font-style" = "italic",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  ), direction = "auto")) %>%
      addLegend(data = filtered_uk_json_lad_acc(), pal = colorNumeric("YlOrRd", 
                                                                      filtered_uk_json_lad_acc()$num_of_casualties), 
                values = ~num_of_casualties, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(x)),
                title = ~paste0("# Casualties")) %>%
      addResetMapButton()
    
    sync(map1, map2, 
         sync = "all", no.initial.sync = TRUE)
    
  })
  
  
  output$map2 <- renderUI({
    # define center of map
    lat_center <- filtered_uk_lat_long()$latitude
    long_center <- filtered_uk_lat_long()$longitude
    
    pal1 <- colorNumeric("plasma", filtered_uk_json_region()$avg_traffic_flow)
    map1 <- leaflet(filtered_uk_json_region()) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(long_center,lat_center, zoom = 10) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                  fillColor = ~pal1(avg_traffic_flow),
                  label = ~paste0(local_authority_name,"(",region_name,")", ": ", formatC(avg_traffic_flow, big.mark = ",")),
                  labelOptions = labelOptions(style = list(
                    "color" = "black",
                    "font-family" = "Cambria",
                    "font-style" = "italic",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  ), direction = "auto")) %>%
      addLegend(data = filtered_uk_json_region(), 
                pal = colorNumeric("plasma", filtered_uk_json_region()$avg_traffic_flow), 
                values = ~avg_traffic_flow, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(x)),
                title = ~paste0("# Miles ")) %>%
      addResetMapButton()
    
    pal2 <- colorNumeric("RdBu", NULL)
    avg_traffic_flow <- filtered_uk_rd_region()$avg_traffic_flow
    map2 <- leaflet(data = filtered_uk_rd_region()) %>% 
      addTiles() %>% 
      addPolylines(stroke = TRUE, weight = 4.0, color = pal2(avg_traffic_flow),
                   fillColor = pal2(avg_traffic_flow),
                   fillOpacity = 1.5, smoothFactor = 0.5,
                   label = ~paste0(road_name,"(",region_name,")", ": ", formatC(avg_traffic_flow, big.mark = ",")),
                   labelOptions = labelOptions(style = list(
                     "color" = "black",
                     "font-family" = "Cambria",
                     "font-style" = "italic",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                     "font-size" = "14px",
                     "border-color" = "rgba(0,0,0,0.5)"
                   ), direction = "auto")) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(long_center,lat_center, zoom = 10) %>%
      addLegend(data = filtered_uk_rd_region(), pal = colorNumeric("RdBu", filtered_uk_rd_region()$avg_traffic_flow),
                values = ~avg_traffic_flow, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(x)),
                title = ~paste0("# Miles ")) %>%
      addResetMapButton()
    
    sync(map1, map2, 
         sync = "all", no.initial.sync = TRUE)
    
  })
  
  output$mapAcc2 <- renderUI({
    # define center of map
    lat_center <- filtered_uk_acc_lat_long()$latitude
    long_center <- filtered_uk_acc_lat_long()$longitude
    
    pal1 <- colorNumeric("YlOrRd", filtered_uk_json_acc_region()$num_of_casualties)
    map1 <- leaflet(filtered_uk_json_acc_region()) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(long_center,lat_center, zoom = 10) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                  fillColor = ~pal1(num_of_casualties),
                  label = ~paste0(Highway.Authority,"(",Region.Country,")", ": ", formatC(num_of_casualties, big.mark = ",")),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"),
                  highlight = highlightOptions(color = "#666",
                                               bringToFront = TRUE)) %>%
      addLegend(data = filtered_uk_json_acc_region(), 
                pal =colorNumeric("YlOrRd", filtered_uk_json_acc_region()$num_of_casualties), 
                values = ~num_of_casualties, opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(x)),
                title = ~paste0("# Casualties")) %>%
      addResetMapButton()
    
    
    map2 <- leaflet(data = filtered_acc_data()) %>% 
      addTiles() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(long_center,lat_center, zoom = 10) %>%
      addHeatmap(lng=~Longitude,lat=~Latitude,intensity=~Number_of_Casualties,max=100,radius=10,blur=5) %>%
      addResetMapButton()
    
    sync(map1, map2, 
         sync = "all", no.initial.sync = TRUE)
    
  })
  
  output$Acccharttitle <- renderText(
    paste("Num of Casualties across Regions in UK in ", input$yr)
  )
  
  output$London <- renderValueBox({
    valueBox(
      tags$p(paste0("London: ", label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                              & (filtered_casualties_data$Region.Country == "London") 
                                              ),]$num_of_casualties)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
    width = 4, color = "maroon"
    )
  })
  
  output$NorthWest <- renderValueBox({
    valueBox(
      tags$p(paste0("North West: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                              & (filtered_casualties_data$Region.Country == "North West") 
                                              ),]$num_of_casualties)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"),
      width = 4, color = "navy"
    )
  })
  
  output$NorthEast <- renderValueBox({
    valueBox(
      tags$p(paste0("North East: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                              & (filtered_casualties_data$Region.Country == "North East") 
                                              ),]$num_of_casualties)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
    width = 4, color = "purple"
    )
  })
  
  output$EastMidlands <- renderValueBox({
    valueBox(
      tags$p(paste0("East Midlands: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                                          & (filtered_casualties_data$Region.Country == "East Midlands") 
                                                          ),]$num_of_casualties)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
      width = 4, color = "teal"
    )
  })
  
  output$WestMidlands <- renderValueBox({
    valueBox(
      tags$p(paste0("West Midlands: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                              & (filtered_casualties_data$Region.Country == "West Midlands") 
                                              ),]$num_of_casualties)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
      width = 4, color = "navy"
    )
  })
  
  output$SouthWest <- renderValueBox({
    valueBox(
      tags$p(paste0("South West: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                              & (filtered_casualties_data$Region.Country == "South West") 
                                              ),]$num_of_casualties)), style = "font-size: 75%;"), 
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
      width = 4, color = "olive"
    )
  })
  
  output$Yorkshire <- renderValueBox({
    valueBox(
      tags$p(paste0("Yorkshire and The Humber: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                                          & (filtered_casualties_data$Region.Country == "Yorkshire and The Humber") 
                                                          ),]$num_of_casualties)), style = "font-size: 67%;"), 
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 22px; color: white"), 
      width = 5, color = "olive"
    )
  })
  
  output$Eastern <- renderValueBox({
    valueBox(
      tags$p(paste0("Eastern: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                              & (filtered_casualties_data$Region.Country == "Eastern") 
                                              ),]$num_of_casualties)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), width = 4,
      color = "teal"
    )
  })
  
  output$SouthEast <- renderValueBox({
    valueBox(
      tags$p(paste0("South East: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                              & (filtered_casualties_data$Region.Country == "South East") 
                                              ),]$num_of_casualties)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), width = 4,
      color = "aqua"
    )
  })
  
  output$Wales <- renderValueBox({
    valueBox(
      tags$p(paste0("Wales: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                              & (filtered_casualties_data$Region.Country == "Wales") 
                                              ),]$num_of_casualties)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), width = 4,
      color = "purple"
    )
  })
  
  output$Scotland <- renderValueBox({
    valueBox(
      tags$p(paste0("Scotland: ", 
                    label_number_si(accuracy=0.5)(filtered_casualties_data[which((filtered_casualties_data$Year == input$yr) 
                                                   & (filtered_casualties_data$Region.Country == "Scotland") 
                    ),]$num_of_casualties)), style = "font-size: 75%;"), 
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"),
      color = "maroon"
    )
  })
  
  output$regiongraph1 <- renderPlotly({
    colors <- c("#6b3969", "#c4446b", "#b77168", "#123429", "#2c8160", "#462c1e", "#e15497", "#083b5f", "#9f9e15",
                "#1b667e", "#c40041")
    filtered_casualties_data %>% 
      ungroup() %>% 
      plot_ly(x = ~Year, y = ~num_of_casualties, type = 'scatter', 
              mode = 'lines+markers', symbol = ~Region.Country, symbols = region_symbols,
              color = ~Region.Country, colors = colors,
              marker = list(size = 10), 
              width = 1300, height = 600) %>% 
      layout(title = "<b> Road Accidents across Regions in UK </b>",
             titlefont = list(size = 20, color = "#434C5E", family = "Arial"), 
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             xaxis = list(title="<b> Region in UK </b>", color='#434C5E', font = list(family = "Cambria", size = 18),
                          tickfont = list(color = '#434C5E', family = "Cambria", size = 18),
                          titlefont = list(color = '#434C5E', family = "Cambria", size = 18)),
             yaxis = list(title = '<b> Number of Casualties </b>', color='#434C5E', 
                          font = list(size = 18),
                          tickfont = list(color = '#434C5E', family = "Cambria", size = 18),
                          titlefont = list(color = '#434C5E', family = "Cambria", size = 18)), legend = legend_font,
             margin = list(b = 80, t=80, l=180, r=180))
  })
  
  output$regiongraph2 <- renderPlotly({
    colors <- c('#962c39', '#a26e53', '#a1c078', 'darkslategrey', 'darkslateblue', '#7803d5',
                "#6b3969", "#c4446b", "#b77168", "#123429", "#2c8160")
    
    filtered_casualties_data %>% 
      ungroup() %>% 
      plot_ly(y = ~num_of_casualties, type = "box", 
              color = ~Region.Country, colors = colors, width = 1200, height = 650, showlegend = FALSE) %>%
      layout(title = "",
             titlefont = list(size = 20, color = "#434C5E", family = "Arial"), 
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             xaxis = list(title="", color='#434C5E', font = list(family = "Cambria", size = 18),
                          tickangle = 45, tickfont = list(color = '#434C5E', family = "Cambria", size = 18),
                          titlefont = list(color = '#434C5E', family = "Cambria", size = 18)),
             yaxis = list(title = '<b> Number of Casualties </b>', color='#434C5E', 
                          font = list(size = 18),
                          tickfont = list(color = '#434C5E', family = "Cambria", size = 18),
                          titlefont = list(color = '#434C5E', family = "Cambria", size = 18)), legend = legend_font,
             margin = list(b = 140, t=100, l=100, r=100), showlegend = FALSE)
  })
  
  output$highwaygraph1 <- renderPlotly({
    uk_notorious_acc_highway %>% 
      ungroup() %>% plot_ly(x =~num_of_casualties, y =~Highway.Authority,
                            orientation = 'h',
                            width=900, height = 750, type = 'bar',
                            marker = list(color = colorRampPalette(brewer.pal(10,"OrRd"))(20)),
                            text = ~paste("Highway Authority:", Highway.Authority,
                                          '</br></br>', "Number of Casualties:", num_of_casualties,
                                          '</br></br>',"Year:", Year),
                            hoverinfo = text,
                            frame = ~Year,texttemplate = '%{x:.2s}',
                            showlegend = FALSE) %>%
      layout(title = "<b> Road Accidents across Notorious Highways in UK </br>",
             titlefont = list(size = 18, color = "#434C5E", family = "Cambria"),
             margin = list(b = 90, t=90, l=180, r=180),
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             yaxis = list(title = "",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          categoryorder = "array",
                          categoryarray = ~uk_notorious_acc_highway$Highway.Authority),
             xaxis = list(title = "<b> Number of Casualties </br>",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickangle = 45),
             showlegend = FALSE) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 16))
                        ) %>% 
      animation_opts(900, easing = "elastic", redraw = FALSE)
    
  })
  
  output$highwaygraph2 <- renderPlotly({
    uk_safe_acc_highway %>% 
      ungroup() %>% plot_ly(x =~num_of_casualties, y =~Highway.Authority,
                            orientation = 'h',
                            width=900, height = 750, type = 'bar',
                            marker = list(color = colorRampPalette(brewer.pal(10,"Blues"))(20)),
                            text = ~paste("Highway Authority:", Highway.Authority,
                                          '</br></br>', "Number of Casualties:", num_of_casualties,
                                          '</br></br>',"Year:", Year),
                            hoverinfo = text,
                            frame = ~Year, texttemplate = '%{x:.2s}',
                            showlegend = FALSE) %>%
      layout(title = "<b> Road Accidents across Safest Highways in UK </br>",
             titlefont = list(size = 18, color = "#434C5E", family = "Cambria"),
             margin = list(b = 90, t=90, l=180, r=180),
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             yaxis = list(title = "",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          categoryorder = "array",
                          categoryarray = ~uk_notorious_acc_highway$Highway.Authority),
             xaxis = list(title = "<b> Number of Casualties </br>",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickangle = 45),
             showlegend = FALSE) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 16))) %>% 
      animation_opts(900, easing = "elastic", redraw = FALSE)
    
  })
  
  output$monthgraph <- renderPlotly({
    colors <- c('#962c39', '#a26e53', '#a1c078', 'darkslategrey', 'darkslateblue', '#7803d5',
                "#6b3969", "#c4446b", "#b77168", "#123429", "#2c8160")
    uk_month_acc %>%
      ungroup() %>% plot_ly(x = ~month, y = ~num_of_casualties, type = 'scatter',
                            mode = 'lines+markers', symbol = ~Region.Country, symbols = region_symbols,
                            frame = ~Year, color = ~Region.Country, colors = colors,
                            marker = list(size = 10), 
                            width = 1200, height = 650) %>%
      layout(title = "<b> Road Accidents across all months in UK </br>",
             titlefont = list(size = 20, color = "#434C5E", family = "Arial"), 
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             xaxis = list(title="<b> Months </br>", color="#434C5E", 
                          font = list(family = "Cambria", size = 18),
                          tickfont = list(color = '#434C5E', family = "Cambria", size = 18),
                          titlefont = list(color = '#434C5E', family = "Cambria", size = 18)),
             yaxis = list(title = '<b> Number of Casualties </br>', color="#434C5E", 
                          font = list(family = "Cambria", size = 18),
                          tickfont = list(color = '#434C5E', family = "Cambria", size = 18),
                          titlefont = list(color = '#434C5E', family = "Cambria", size = 18)), 
             legend = legend_font, margin = list(b = 80, t=80, l=180, r=180)) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 18))
      ) %>% 
      animation_opts(1000, easing = "elastic", redraw = FALSE)
  })
  
  output$hourgraph <- renderPlotly({
    colors <- c('#962c39', '#a26e53', '#a1c078', 'darkslategrey', 'darkslateblue', '#7803d5',
                "#6b3969", "#c4446b", "#b77168", "#123429", "#2c8160")
    uk_hour_acc %>%
      ungroup() %>% plot_ly(x = ~hour, y = ~num_of_casualties, type = 'scatter',
                            mode = 'lines+markers', symbol = ~Region.Country, symbols = region_symbols,
                            frame = ~Year, color = ~Region.Country, colors = colors,
                            marker = list(size = 10), 
                            width = 1200, height = 650) %>%
      layout(title = "<b> Road Accidents across all hours in UK </br>",
             titlefont = list(size = 20, color = "#434C5E", family = "Arial"), 
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             xaxis = list(title="<b> Hours of Day </br>", color="#434C5E", font = list(family = "Cambria", size = 18),
                          tickfont = list(color = '#434C5E', family = "Cambria", size = 18),
                          titlefont = list(color = '#434C5E', family = "Cambria", size = 18),
                          autotick = F, dtick = 1),
             yaxis = list(title = '<b> Number of Casualties </br>', color="#434C5E", 
                          font = list(family = "Cambria", size = 18),
                          tickfont = list(color = '#434C5E', family = "Cambria", size = 18),
                          titlefont = list(color = '#434C5E', family = "Cambria", size = 18)), legend =legend_font,
             margin = list(b = 80, t=80, l=180, r=180)) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 18))
      ) %>% 
      animation_opts(1000, easing = "elastic", redraw = FALSE)
  })
  
  output$piechart1 <- renderPlotly({
    colors <- c('#962c39', '#a26e53', '#a1c078', 'darkslategrey', 'darkslateblue', '#7803d5',
                "#6b3969", "#c4446b", "#b77168", "#123429", "#2c8160")
    
    filtered_casualties_data %>%
      ungroup() %>% 
      plot_ly(labels = ~Region.Country, values = ~num_of_casualties,
              marker = list(colors = colors),
              frame = ~Year,
              insidetextfont = list(color = '#FFFFFF', size = 14),
              width=1000, height=650) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "<b> Road Accidents in UK across all Regions </br>",  
             titlefont = list(size = 18, color = '#434C5E', family = "Arial"),
             showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, color='#434C5E'),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, color='#434C5E'),
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             legend = legend_font,
             margin = list(b = 80, t=80, l=180, r=180)) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 16))
      ) %>% 
      animation_opts(1000, easing = "elastic", redraw = FALSE)
  })
  
  output$piechart2 <- renderPlotly({
    colors <- c("#6b3969", "#c4446b", "#b77168", "#123429", "#a9368f", "#350a3e", "#242424", "#462c1e")
    
    plot_ly(uk_accident_spd_limit, labels = ~Speed_limit, values = ~num_of_casualties,
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 0.5)),
            frame = ~Year,
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF', size = 14),
            outsidetextfont = list(color = '#FFFFFF', size = 14),
            width=1000, height=650) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "<b> Road Accidents in UK by Speed Limits </br>",  
             titlefont = list(size = 18, color = '#434C5E', family = "Arial"),
             showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, color='#434C5E'),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, color='#434C5E'),
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             legend = legend_font,
             margin = list(b = 80, t=80, l=180, r=180)) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 16))
      ) %>% 
      animation_opts(1000, easing = "elastic", redraw = FALSE)
  })
  
  
  output$Trafficcharttitle <- renderText(
    paste("Miles covered by ", input$Vehs, " in ", input$Yr, " in UK")
  )
  
  output$Lond <- renderValueBox({
    valueBox(
      tags$p(paste0("London: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "London")
                            ),]$avg_traffic_flow)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
      width = 4, color = "maroon"
    )
  })
  
  output$NW <- renderValueBox({
    valueBox(
      tags$p(paste0("North West: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "North West") 
                    ),]$avg_traffic_flow)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"),
      width = 4, color = "navy"
    )
  })
  
  output$NE <- renderValueBox({
    valueBox(
      tags$p(paste0("North East: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "North East") 
                    ),]$avg_traffic_flow)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
      width = 4, color = "purple"
    )
  })
  
  output$EM <- renderValueBox({
    valueBox(
      tags$p(paste0("East Midlands: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "East Midlands")
                            ),]$avg_traffic_flow)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
      width = 4, color = "teal"
    )
  })
  
  output$WM <- renderValueBox({
    valueBox(
      tags$p(paste0("West Midlands: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "West Midlands") 
                    ),]$avg_traffic_flow)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
      width = 4, color = "navy"
    )
  })
  
  output$SW <- renderValueBox({
    valueBox(
      tags$p(paste0("South West: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "South West") 
                    ),]$avg_traffic_flow)), style = "font-size: 75%;"), 
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), 
      width = 4, color = "olive"
    )
  })
  
  output$Yorks <- renderValueBox({
    valueBox(
      tags$p(paste0("Yorkshire and The Humber: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "Yorkshire and The Humber")
                            ),]$avg_traffic_flow)), style = "font-size: 67%;"), 
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 22px; color: white"), 
      width = 5, color = "olive"
    )
  })
  
  output$East <- renderValueBox({
    valueBox(
      tags$p(paste0("Eastern: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "East of England") 
                    ),]$avg_traffic_flow)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), width = 4,
      color = "teal"
    )
  })
  
  output$SE <- renderValueBox({
    valueBox(
      tags$p(paste0("South East: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "South East") 
                    ),]$avg_traffic_flow)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), width = 4,
      color = "aqua"
    )
  })
  
  output$Ws <- renderValueBox({
    valueBox(
      tags$p(paste0("Wales: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "Wales") 
                    ),]$avg_traffic_flow)), style = "font-size: 75%;"),
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"), width = 4,
      color = "purple"
    )
  })
  
  output$Scot <- renderValueBox({
    valueBox(
      tags$p(paste0("Scotland: ", 
                    label_number_si(accuracy=0.5)(filtered_region_summary()[
                      which((filtered_region_summary()$region_name == "Scotland") 
                    ),]$avg_traffic_flow)), style = "font-size: 75%;"), 
      "", icon = tags$i(class = "fas fa-flag-checkered", style="font-size: 24px; color: white"),
      color = "maroon"
    )
  })
  
  output$trafficgraph1 <- renderPlotly({
    colors <- c('#962c39', '#a26e53', '#a1c078', 'darkslategrey', 'darkslateblue', '#7803d5',
                "#6b3969", "#c4446b", "#b77168", "#123429", "#2c8160")
    uk_vehicles_region_summary %>% 
      filter(vehicles_names == input$Vehs) %>%
      ungroup() %>% 
      plot_ly(x = ~year, y = ~avg_traffic_flow, type = 'scatter',
              mode = 'markers', symbol = ~region_name, 
              symbols = region_symbols,
              color = ~region_name, colors = colors,
              marker = list(size = 10, colors = colors), 
              width = 1200, height = 650) %>%
      layout(title = list(text = '<b> Traffic Flow across Regions in UK </b>', 
                          font = list(size = 22, color = '#434C5E', family = "Arial")),
             margin = list(b = 120, t=100, l=180, r=180),
             paper_bgcolor = "#d6d8da",
             plot_bgcolor = "#d6d8da",
             yaxis = list(title = 'Volume of taffic (miles)',
                          titlefont = list(color='#434C5E', family = "Cambria", size = 18),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 18)),
             xaxis = list(title = 'Years',
                          titlefont = list(color='#434C5E', family = "Cambria", size = 18),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 18)),
             legend = legend_font)
  })
  
  output$trafficgraph2 <- renderPlotly({
    uk_vehicles_region_summary %>% 
      filter(vehicles_names == input$Vehs) %>%
      plot_ly(x = ~year, y = ~avg_traffic_flow, 
              type = 'box',
              marker = list(size = 10,
                            line = list(outliercolor = 'rgb(107,174,214)',
                                        outlierwidth = 2)), 
              width = 1200, height = 650,
              line = list(color = 'rgb(7,40,89)'),
              fillcolor ='rgb(107,174,214)', showlegend = FALSE) %>%
      layout(title = list(text = '<b> Traffic Flow across Regions in UK </b>', 
                          font = list(size = 22, color = '#434C5E', family = "Arial")),
             xaxis = list(title = 'Years',
                          titlefont = list(color='#434C5E', family = "Cambria", size = 18),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 18)),
             yaxis = list(title = 'Volume of taffic (miles)',
                          titlefont = list(color='#434C5E', family = "Cambria", size = 18),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 18)),
             paper_bgcolor = "#d6d8da",
             plot_bgcolor = "#d6d8da",
             margin = list(b = 140, t=100, l=100, r=100), showlegend = FALSE)
  })
  
  output$trafficgraph3 <- renderPlotly({
    uk_vehicles_region_order %>% 
      filter(vehicles_names == input$Vehs) %>%
      ungroup() %>% plot_ly(x =~avg_traffic_flow, y =~region_name,
                            orientation = 'h',
                            marker = list(color = colorRampPalette(brewer.pal(10, "PuRd"))(11)),
                            frame = ~year,
                            width=900, height = 600, type = 'bar',
                            text = ~paste("Region:", region_name,
                                          '</br></br>', "Miles:", avg_traffic_flow,
                                          '</br></br>',"Year:", year),
                            hoverinfo = text, texttemplate = '',
                            showlegend = FALSE) %>%
      layout(title = "<b> Miles Driven in UK by Region </b>",
             titlefont = list(size = 18, color = '#434C5E', family = "Arial"),
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             margin = list(b = 90, t=90, l=180, r=180),
             yaxis = list(title = "",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          categoryorder = "array",
                          categoryarray = ~region_name),
             xaxis = list(title = "Miles driven",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickangle = 45),
             showlegend = FALSE)%>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 16))
      ) %>%
      
      animation_opts(
        
        850, easing = "elastic", redraw = FALSE
        
      )
  })
  
  output$trafficgraph4 <- renderPlotly({
    colors <- c('#962c39', '#a26e53', '#a1c078', 'darkslategrey', 'darkslateblue', '#7803d5',
                "#6b3969", "#c4446b", "#b77168", "#123429", "#2c8160")
    
    uk_vehicles_region %>% 
      filter((vehicles_names == input$Vehs)) %>%
      plot_ly(x = ~avg_traffic_flow, y = ~vehicles_flow, type = 'scatter',
              mode = 'markers', symbol = ~region_name, symbols = region_symbols,
              color = ~region_name, colors = colors,
              marker = list(size = 10, colors = colors),
              text = ~paste("Num of vehicles:", vehicles_flow,
                            '</br></br>', "Miles:", avg_traffic_flow,
                            '</br></br>',"Year:", year,
                            '</br></br>',"Region:", region_name),
              hoverinfo = text,
              width = 900, height = 550, showlegend = FALSE) %>%
      layout(title = list(text = '<b> Traffic Flow across Region in UK </b>', 
                          font = list(size = 18, color = '#434C5E', family = "Arial")),
             paper_bgcolor = "#d6d8da",
             plot_bgcolor = "#d6d8da",
             margin = list(b = 90, t=90, l=180, r=180),
             yaxis = list(title = '<b>Num of vehicles</b>',
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16)),
             xaxis = list(title = '<b>Volume of taffic (miles)</b>',
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16)),
             legend=legend_font, showlegend = FALSE)
  })
  
  output$trafficgraph5 <- renderPlotly({
    uk_hw_traffic_miles %>%
      filter(vehicles_names == input$Vehs) %>%
      ungroup() %>% plot_ly(x =~traffic_flow, y =~local_authority_name,
                            orientation = 'h',
                            width=900, height = 650, type = 'bar',
                            marker = list(color = colorRampPalette(brewer.pal(9,"YlGnBu"))(20)),
                            text = ~paste("Highway Authority:", local_authority_name,
                                          '</br></br>', "Miles:", traffic_flow,
                                          '</br></br>',"Year:", year),
                            hoverinfo = text,
                            frame = ~year,texttemplate = '',
                            showlegend = FALSE) %>%
      layout(title = "<b> Miles covered by Busiest Highways in UK </br>",
             titlefont = list(size = 18, color = "#434C5E", family = "Cambria"),
             margin = list(b = 90, t=90, l=180, r=180),
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             yaxis = list(title = "",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          categoryorder = "array", categoryarray = ~local_authority_name),
             xaxis = list(title = "<b> Volume of traffic (miles) </br>",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickangle = 45),
             showlegend = FALSE) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 16))
      ) %>%
      
      animation_opts(
        
        850, easing = "elastic", redraw = FALSE
        
      )
  })
  
  output$trafficgraph6 <- renderPlotly({
    uk_rd_traffic_miles %>%
      ungroup() %>% 
      filter(vehicles_names == input$Vehs) %>%
      plot_ly(x =~traffic_flow, y =~road_name,
              orientation = 'h',
              width=900, height = 650, type = 'bar',
              marker = list(color = colorRampPalette(brewer.pal(10,"RdPu"))(14)),
              text = ~paste("Highway Authority:", road_name,
                            '</br></br>', "Miles:", traffic_flow,
                            '</br></br>',"Year:", year),
              hoverinfo = text,texttemplate = '',
              frame = ~year,
              showlegend = FALSE) %>%
      layout(title = "<b> Miles covered by Major Road Networks in UK </br>",
             titlefont = list(size = 18, color = "#434C5E", family = "Cambria"),
             margin = list(b = 90, t=90, l=180, r=180),
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             yaxis = list(title = "",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          categoryorder = "array", categoryarray = ~road_name),
             xaxis = list(title = "<b> Volume of traffic (miles) </br>",
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickangle = 45),
             showlegend = FALSE) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 16))
      ) %>%
      
      animation_opts(
        
        850, easing = "elastic", redraw = FALSE
        
      )
  })
  
  output$trafficpiechart1 <- renderPlotly({
    colors <- c("#d4adea", "#701637", "#dda252", "#01848a", "#b94460", "#292f48", "#8da280", "#c04456")
    uk_rd_cat_summary %>% 
      filter(vehicles_names == input$Vehs) %>%
      ungroup() %>% 
      plot_ly(labels = ~road_category, values = ~avg_traffic_flow,
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 0.5)),
            frame = ~year,
            textinfo = 'percent',
            insidetextfont = list(color = '#FFFFFF', size = 14),
            outsidetextfont = list(color = '#FFFFFF', size = 14),
            width=1000, height=650) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "<b> Percent of Miles covered by different Road Categories in UK </b>",  
             titlefont = list(size = 20, color = '#434C5E', family = "Arial"),
             showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, color='#434C5E'),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, color='#434C5E'),
             plot_bgcolor  = "#d6d8da",
             paper_bgcolor = "#d6d8da",
             legend = legend_font,
             margin = list(b = 100, t=100, l=180, r=180)) %>%
      animation_slider( currentvalue = list(prefix = "Year: ", 
                                            font = list(color='#434C5E', family = "Cambria", size = 16))
      ) %>% 
      animation_opts(1000, easing = "elastic", redraw = FALSE)
  })
  
  output$trafficgraph7 <- renderPlotly({
    colors <- c("#701637", "#d4adea", "#8da280", "#c04456")
    cat_syms <- c('square-open', 'square-dot', 'diamond','cross')
    
    uk_rd_cat_summary %>% 
      filter(vehicles_names == input$Vehs) %>%
      ungroup() %>%
      plot_ly(x = ~year, y = ~avg_traffic_flow, type = 'scatter',
              mode = 'lines+markers', symbol = ~road_category, symbols =cat_syms,
              color = ~road_category, colors = colors,
              marker = list(size = 10),
              width = 1200, height = 750) %>%
      layout(title = list(text = '<b> Traffic Flow by Road Categories in UK </b>', font = t), 
             paper_bgcolor = "#d6d8da",
             plot_bgcolor = "#d6d8da",
             margin = list(b = 90, t=90, l=180, r=180),
             yaxis = list(title = '<b> Volume of traffic (miles) </b>',
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16)),
             xaxis = list(title = 'Years',
                          titlefont = list(color='#434C5E', family = "Cambria", size = 16),
                          tickfont = list(color='#434C5E', family = "Cambria", size = 16)),
             legend = legend_font)
  })
  
  output$Giftitle <- renderText(
    paste("Maps as Gif")
  )
  
}
