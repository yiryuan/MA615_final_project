library(tidyverse)
library(ggplot2)
library(leaflet)
library(rgdal)
library(sf)
library(dplyr)
library(magrittr)
library(viridis)
library(hrbrthemes)
library(shiny)
library(shinydashboard)
library(shinyjs)

MBTA_icon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/MBTA.svg/240px-MBTA.svg.png",iconWidth = 15, iconHeight = 15)

sub1<-tibble(station=c("Kenmore","Blandford Street","Boston University East",
                       "Boston University Central"),
             latitude =c(42.34881,42.3492,42.34972,42.35004),
             longitude=c(-71.09535,-71.10006,-71.10443,-71.10704))
sub2<-data_frame(station=c("Davis","Porter","Harvard","Central","Kendall/MIT",
                           "Charles/MGH"),
                 latitude=c(42.39675,42.38832,42.37403,42.36544,42.36242,42.36117),
                 longitude=c(-71.12223,-71.11911,-71.11891,-71.10383,	
                             -71.08619,-71.07108))
sub3<-data_frame(station=c("Chinatown","Downtown Crossing","State","Haymarket",
                           "North Station"),
                 latitude=c(42.35247,42.35545,42.3588,42.36247,42.36514),
                 longitude=c(-71.06258,-71.06049,-71.05782,-71.05785,-71.05998))
Bus_Stops <- read.csv("PATI_Bus_Stops.csv")


ui <- dashboardPage(
  dashboardHeader(title = "MA615 Final Project",titleWidth=800),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",tabName ="Overview",icon=icon("road","fa-solid fa-road")),
      menuItem("Subway",tabName ="Subway",icon=icon("train-subway","fa-solid fa-train-subway")),
      menuItem("Bus",tabName ="Bus",icon=icon("bus","fa-solid fa-bus")),
      menuItem("Ferry",tabName ="Ferry",icon=icon("ship","fa-solid fa-ship"))
    )
  ),
  skin = "purple",
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem("Page 1",
              h5("This report mainly studies the difference between the travel
              time predicted by Google and the actual travel time. I compare 
              the difference between Google's estimated time and the actual 
              time for a certain route by randomly selecting a day. Also, 
              choose dates with holiday events and extreme weather to see if 
              holidays and weather affect the difference between the travel 
              time predicted by Google and the actual travel time."
              ),
              tabName = "Overview",
              fluidRow(
                box(leafletOutput("Sub"), width = 12 ),
                box(leafletOutput("Bus"), width = 12 ),
                box(leafletOutput("Fer"), width = 12 )
              )),
      tabItem("Page 2",
              tabName = "Subway",
              fluidRow(
                box(leafletOutput("GreenB1"), width = 12 ),
                box(leafletOutput("Red"), width = 12 ),
                box(leafletOutput("Orange"), width = 12 )
             )),
    tabItem("Page 3",
      tabName = "Bus",
              fluidRow(
                box(leafletOutput("Bus83"), width = 12 ),
                box(leafletOutput("Bus01"), width = 12 ),
                box(leafletOutput("Bus100"), width = 12 )
              )),
  tabItem("Page 3",
    tabName = "Ferry",
              fluidRow(
                box(leafletOutput("Ferry01"), width = 12 ),
                box(leafletOutput("Ferry02"), width = 12 ),
                box(leafletOutput("Ferry03"), width = 12 )
                
             )))))
  

server <- function(input, output, session) {
  output$GreenB1<-renderLeaflet({
    con1 <-paste(sep = "<br/>",
                 "Kenmore to BU Central",
                 "MBTA Actual time",
                 "206.0667 sec",
                 "Using data of a snowy day(2022-01-07)",
                 "Google estimate time",
                 "160 sec")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub1,lng=~longitude,lat =~latitude,color="green") %>%
      addMarkers(data =sub1,lng=~longitude,lat =~latitude,icon=MBTA_icon,label = ~station) %>%
      addPopups(-71.10704, 42.35004, con1,
                options = popupOptions(closeButton = FALSE)
      )
  })
  output$Red<-renderLeaflet({
    con2 <-paste(sep = "<br/>",
                 "Charles/MGH to Davis",
                 "MBTA Actual time",
                 "817.0333 sec",
                 "Using data of 2022-04-08",
                 "Google Estimate time",
                 "840 sec")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub2,lng=~longitude,lat =~latitude,color="red") %>%
      addMarkers(data =sub2,lng=~longitude,lat =~latitude,icon=MBTA_icon,label = ~station) %>%
      addPopups(-71.12223, 42.39675, con2,
                options = popupOptions(closeButton = FALSE)
      )
  })
  output$Orange<-renderLeaflet({
    
    con3 <-paste(sep = "<br/>",
                 "Chinatown to North Station",
                 "MBTA Actual time",
                 "322.3846 sec",
                 "Using data of 2022-07-03",
                 "Google estimate time",
                 "360 sec")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub3,lng=~longitude,lat =~latitude,color="orangered") %>%
      addMarkers(data =sub3,lng=~longitude,lat =~latitude,icon=MBTA_icon,label = ~station) %>%
      addPopups(-71.05998, 42.36514, con3,
                options = popupOptions(closeButton = FALSE)
      )
  })
  output$Bus83<-renderLeaflet({
    sub4<-Bus_Stops %>% filter(grepl("83",Routes)) %>% 
      filter(!grepl("Massachusetts Ave @ Wendell St",Stop_Name))%>%
      filter(!grepl("Massachusetts Ave @ Exeter Park",Stop_Name))%>%
      filter(!grepl("Broadway @ Felton St",Stop_Name))%>%
      filter(!grepl("Massachusetts Ave opp Waterhouse St",Stop_Name))%>%
      select(Longitude,Latitude,Stop_Name) %>%
      arrange(Latitude)
    
    
    con4 <-paste(sep = "<br/>",
                 "From Rindge Ave @ Russell Field",
                 "To Magazine St @ Green St",
                 "MBTA 83 BUS Actual time",
                 "2.224167 hours",
                 "Using data of Independence Day 2022",
                 "Google estimate time",
                 "25 min")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub4,lng=~Longitude,lat =~Latitude,color="yellow") %>%
      addMarkers(data =sub4,lng=~Longitude,lat =~Latitude,icon=MBTA_icon,label = ~Stop_Name) %>%
      addPopups(-71.10431, 42.36511, con4,
                options = popupOptions(closeButton = FALSE)
      )
    
  })
  
  output$Bus01<-renderLeaflet({
    sub5<-Bus_Stops %>% filter(Routes=="1")%>%
      distinct(Stop_Name,.keep_all = TRUE)%>%
      select(Longitude,Latitude,Stop_Name) %>%
      arrange(Latitude)
    
    con5 <-paste(sep = "<br/>",
                 "From Massachusetts Ave @ Bow St",
                 "To Albany St @ Randall St",
                 "MBTA 01 BUS Actual time",
                 "49.58333 mins",
                 "Using data of 2021-12-09",
                 "Google estimate time",
                 "37 mins")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub5,lng=~Longitude,lat =~Latitude,color="yellow") %>%
      addMarkers(data =sub5,lng=~Longitude,lat =~Latitude,icon=MBTA_icon,label = ~Stop_Name) %>%
      addPopups(-71.07635, 42.33167, con5,
                options = popupOptions(closeButton = FALSE)
      )
  })
  
  output$Bus100<-renderLeaflet({
    sub6<-Bus_Stops %>% filter(grepl("100",Routes))%>%
      distinct(Stop_Name,.keep_all = TRUE)%>%
      select(Longitude,Latitude,Stop_Name) %>%
      arrange(Latitude)
    
    con6 <-paste(sep = "<br/>",
                 "From Fellsway W @ Elm St",
                 "To Fellsway @ Wellington Circle Shopping Ctr",
                 "MBTA 100 BUS Actual time",
                 "1.166667 hours",
                 "Using data of 2022-06-07",
                 "Google estimate time",
                 "11 min")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub6,lng=~Longitude,lat =~Latitude,color="yellow") %>%
      addMarkers(data =sub6,lng=~Longitude,lat =~Latitude,icon=MBTA_icon,label = ~Stop_Name) %>%
      addPopups(-71.08239, 42.40584, con6,
                options = popupOptions(closeButton = FALSE)
      )
    
  })
  output$Ferry01<-renderLeaflet({
    sub7<-tibble(Stop_Name=c("Hingham","Rowes Wharf"),
                 Longitude=c(-70.91967,-71.05005),
                 Latitude =c(42.25296,42.3569))
    con7 <-paste(sep = "<br/>",
                 "From Hingham to Rowes Wharf",
                 "MBTA Ferry Actual time",
                 "33 mins",
                 "Using data of 2021-01-08",
                 "Google estimate time",
                 "35 mins")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub7,lng=~Longitude,lat =~Latitude,color="darkblue") %>%
      addMarkers(data =sub7,lng=~Longitude,lat =~Latitude,icon=MBTA_icon,label = ~Stop_Name) %>%
      addPopups(-71.05005, 42.3569, con7,
                options = popupOptions(closeButton = FALSE)
      )
  })
  
  output$Ferry02<-renderLeaflet({
    sub8<-tibble(Stop_Name= c("Hull","Long Wharf"),
                 Longitude=c(-70.9196,-71.05065),
                 Latitude =c(42.30364,42.35982))
    con8 <-paste(sep = "<br/>",
                 "From Hull to Long Wharf",
                 "MBTA Ferry Actual time",
                 "27 mins",
                 "Using data of 2021-08-04",
                 "Google estimate time",
                 "71 mins")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub8,lng=~Longitude,lat =~Latitude,color="darkblue") %>%
      addMarkers(data =sub8,lng=~Longitude,lat =~Latitude,icon=MBTA_icon,label = ~Stop_Name) %>%
      addPopups(-71.05065, 42.35982, con8,
                options = popupOptions(closeButton = FALSE)
      )
  })
  
  output$Ferry03<-renderLeaflet({
    sub9<-tibble(Stop_Name= c("Navy Yard","Long Wharf"),
                 Longitude=c(-71.05476,-71.05065),
                 Latitude =c(42.37311,42.35982))
    
    con9<-paste(sep = "<br/>",
                "From Navy Yard to Long Wharf",
                "MBTA Ferry Actual time",
                "10 mins",
                "Using data of 2021-06-10",
                "Google estimate time",
                "12 mins")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data =sub9,lng=~Longitude,lat =~Latitude,color="darkblue") %>%
      addMarkers(data =sub9,lng=~Longitude,lat =~Latitude,icon=MBTA_icon,label = ~Stop_Name) %>%
      addPopups(-71.05065, 42.35982, con9,
                options = popupOptions(closeButton = FALSE)
      )
  })
  output$Sub<-renderLeaflet({
    MBTA_sub <- readOGR(dsn="mbta_rapid_transit", layer="MBTA_ARC")
    MBTA_sub <- spTransform(MBTA_sub, CRS("+proj=longlat +ellps=GRS80"))
    MBTA_sub_stops <- readOGR(dsn="mbta_rapid_transit", layer="MBTA_NODE")
    MBTA_sub_stops <- spTransform(MBTA_sub_stops, CRS("+proj=longlat +ellps=GRS80"))
    pal <- colorFactor(palette = c('blue','green','orange','red','grey'),
                       domain = c('BLUE','GREEN','ORANGE','RED','SILVER'))
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = MBTA_sub, color=~pal(LINE)) %>%
      addMarkers(data=MBTA_sub_stops,icon=MBTA_icon)

  })
  
  output$Bus<-renderLeaflet({
    MBTA_bus <- readOGR(dsn="mbtabus", layer="MBTABUSROUTES_ARC")
    MBTA_bus <- spTransform(MBTA_bus, CRS("+proj=longlat +ellps=GRS80"))
    MBTA_bus_stops <- readOGR(dsn="mbtabus", layer="MBTABUSSTOPS_PT")
    MBTA_bus_stops <- spTransform(MBTA_bus_stops, CRS("+proj=longlat +ellps=GRS80"))
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = MBTA_bus, color="orange") %>%
      addMarkers(data=MBTA_bus_stops,icon=MBTA_icon)
  })
  
  output$Fer<-renderLeaflet({
    MBTA_ferry <- readOGR(dsn="Ferry_Routes", layer="Ferry_Routes")
    MBTA_ferry <- spTransform(MBTA_ferry, CRS("+proj=longlat +ellps=GRS80"))
    MBTA_ferry_stops <- readOGR(dsn="Seaports", layer="Seaports")
    MBTA_ferry_stops <- spTransform(MBTA_ferry_stops, CRS("+proj=longlat +ellps=GRS80"))
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = MBTA_ferry, color="blue")%>%
      addMarkers(data=MBTA_ferry_stops,icon=MBTA_icon)
  })
  }
shinyApp(ui, server)
