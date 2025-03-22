#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!requireNamespace("markdown", quietly = TRUE)) {
  install.packages("markdown")
}
library(markdown)


library(shiny)
library(DT)
dataTableOutput <- shiny::dataTableOutput
renderDataTable <- shiny::renderDataTable


library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(leaflet.extras)
library(dplyr)



loc <- read.csv("data/Loc.csv", header = TRUE, sep = ",")

ui <- dashboardPage(
  dashboardHeader(title = "Effect of Weather on Bicycle Traffic in Melbourne",
                  titleWidth = 500
  ),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bicycle", tabName = "bicycle", icon = icon("bicycle")),
      
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      
      menuItem("ReadMe", tabName = "readme", icon = icon("mortar-board")),
      
      menuItem("Code", tabName = "code", icon = icon("code")),
      
      br(),
      br(),
      
      menuItem(selectInput("weather", "Weather Conditions:", 
                           c("Rain" = "RAINFALL_MM", "Temp" = "MAXTEMP_C"),
                           selected = c("Rain" = "RAINFALL_MM"), multiple = FALSE)),
      
      
      menuItem(selectInput("yearlyTraffic", "Yearly Bicycle Traffic:", 
                           c("All Bike Paths" = "all",  "Locations" = "location", "Holidays" = "holidays"),
                           selected = c("All Bike Paths" = "all"), multiple = FALSE)),
      
      menuItem(title = "Select Month:", status = "primary", solidHeader = TRUE,
               sliderInput("mon", "Month:", 1, 12, 1, step = 1, animate =
                             animationOptions(interval = 2500, loop = TRUE))),
      
      menuItem(title = "Select Year:", status = "primary", solidHeader = TRUE,
               sliderInput("year", "Year:", 2006, 2012, 2006, step = 1, sep = "", animate =
                             animationOptions(interval = 2500, loop = TRUE)))
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tags$head(tags$style(HTML('
                              .skin-blue .main-header .logo {
                              background-color: #3c8dbc;
                              }
                              .skin-blue .main-header .logo:hover {
                              background-color: #3c8dbc;
                              }
                              '))
    ),
    
    
    tabItems(
      # Bicycle tab content
      tabItem(tabName = "bicycle",
              fluidRow(
                box(title = "Bicycle Paths", status = "primary",
                    solidHeader = TRUE, leafletOutput("melMap", height = 410), 
                    width = 6, absolutePanel(top=60, right=10,
                                             selectInput("location", h4(strong("Select Location")), c(as.character(loc$LOCATION)), 
                                                         selected = "Anniversary Trail No.1"))),
                
                box(title = "Monthly Bicycle Traffic", status = "primary", 
                    solidHeader = TRUE, plotlyOutput("five", height = 370), width = 6, 
                    footer = "Source: Bicycle Volumes - VicRoads"),
                
                box(title = "Yearly Bicycle Traffic", status = "primary", 
                    solidHeader = TRUE, plotlyOutput("four", height = 360), width = 6, 
                    footer = "Source: Bicycle Volumes - VicRoads"),
                
                box(title = "Monthly Weather Conditions", status = "primary", 
                    solidHeader = TRUE, plotlyOutput("six", height = 360), width = 6, 
                    footer = "Source: Australian Government Bureau of Meterology")
              )
      ),
      
      # Data Table tab content
      tabItem(tabName = "table",
              fluidPage(title = "Data Table",
                        div(DT::dataTableOutput("dataTable"), 
                            style = "font-size: 85%; width: 80%"))
      ),
      
      # ReadMe tab content
      tabItem(tabName = "readme",
              fluidRow(includeMarkdown("markdown/readMe.Rmd")
              )
      ),
      
      # Code tab content
      tabItem(tabName = "code",
              fluidRow(includeMarkdown("markdown/code.Rmd")
              )
      )
    )
    )
    )




server <- function(input, output, session) {
  
  loc <- read.csv("data/Loc.csv", header = TRUE, sep = ",")
  dataBike_Loc <- read.csv("./data/combCondLoc.csv", header = TRUE, sep = ",")
  dataBike_Loc1 <- read.csv("./data/dataBike_Loc1.csv")
  dataBike_Loc2 <- read.csv("./data/dataBike_Loc2.csv")
  dataBike_Loc_hol <- read.csv("./data/dataBike_Loc_hol.csv")
  dataBike_Day <- read.csv("./data/dataBike_Day.csv")
  
  dataBike_Loc$DAY_NAME <- dataBike_Loc$DAY_NAME %>% factor(levels = c("Sun", "Mon","Tue", "Wed", "Thu", "Fri", "Sat"), ordered = TRUE)
  dataBike_Loc$MONTH_NAME <- dataBike_Loc$MONTH_NAME %>% factor(levels = c("January", "February", "March", "April", "May", "June", "July",
                                                                           "August", "September", "October", "November", "December"), ordered = TRUE)
  dataBike_Day$DAY_NAME <- dataBike_Day$DAY_NAME %>% factor(levels = c("Sun", "Mon","Tue", "Wed", "Thu", "Fri", "Sat"), ordered = TRUE)
  dataBike_Day$MONTH_NAME <- dataBike_Day$MONTH_NAME %>% factor(levels = c("January", "February", "March", "April", "May", "June", "July",
                                                                           "August", "September", "October", "November", "December"), ordered = TRUE)
  
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="blue", fillColor="orange", fillOpacity=1, opacity=1, weight=2, layerId="Selected")
  
  
  # Box 1. Map of Melbourne highlighting bike lanes
  output$melMap <- renderLeaflet({
    
    leaflet() %>%
      setView(lng = 145, lat = -37.8, zoom = 11) %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>%
      addCircleMarkers(data=loc, radius=6, color="blue", stroke=FALSE, fillOpacity=0.5, group="marker", layerId = ~LOCATION) %>% 
      addLayersControl(baseGroups = c("Street Map", "Satellite Map"), overlayGroups = c("marker"), 
                       options = layersControlOptions(collapsed = TRUE), position = "bottomright") %>% 
      setMaxBounds (lng1 = 139.9631, 
                    lat1 = -42.8136, 
                    lng2 = 149.9631, 
                    lat2 = -32.8136) 
  })
  
  observeEvent(input$melMap_marker_click, { # update the map markers and view on map clicks
    p <- input$melMap_marker_click
    proxy <- leafletProxy("melMap")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, 11) %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  observeEvent(input$melMap_marker_click, { # update the location selectInput on map clicks
    p <- input$melMap_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected = p$id)
    }
  })
  
  observeEvent(input$location, { # update the map markers and view on location selectInput changes
    p <- input$melMap_marker_click
    p2 <- subset(loc, LOCATION==input$location)
    proxy <- leafletProxy("melMap")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
    } else if(length(p$id) && input$location!=p$id){
      proxy %>% setView(lng=p2$LONG, lat=p2$LAT, 11) %>% acm_defaults(p2$LONG, p2$LAT)
    } else if(!length(p$id)){
      proxy %>% setView(lng=p2$LONG, lat=p2$LAT, 11) %>% acm_defaults(p2$LONG, p2$LAT)
    }
  })
  
  # Box 2. Plot of Day vs Mean 24 Hour Volume for each Month
  output$five <- renderPlotly({
    mainLabel = paste("Average Bicycle Traffic", month.name[input$mon], input$year)
    p1 <- ggplot(data = filter(dataBike_Day, YEAR == input$year & MONTH == input$mon), aes(x = DAY, y = MEAN_24HOUR)) +
      ggtitle(mainLabel)
    p1 + geom_line() +
      geom_point(aes(colour = WEEKEND)) +
      labs(x = "Day of Month", y = "Average 24hr Volume") +
      guides(colour = guide_legend(override.aes = list(size = 4))) +
      guides(size = "none") +
      theme_bw()
  })
  
  # Box 3. Plot of Holiday vs Mean 24 Hour Volume for each Year
  output$four <- renderPlotly({
    if (input$yearlyTraffic == "all") {
      
      p3 <- ggplot(data = filter(dataBike_Loc1, YEAR == input$year), aes(x = MONTH, y = MEAN_24HOUR)) +
        ggtitle(paste("Average Bicycle Traffic", input$year))
      p3 + geom_line() +
        geom_point(aes(colour = SEASON)) +
        labs(x = "Month", y = "Average 24hr Volume") +
        scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                           labels=c("Jan", "Feb", "Mar", "Apr",
                                    "May", "Jun", "Jul", "Aug",
                                    "Sep", "Oct", "Nov", "Dec")) +
        guides(colour = guide_legend(override.aes = list(size = 4))) +
        guides(size = FALSE) +
        theme_bw()
      
    } else if (input$yearlyTraffic == "holidays") {
      
      p3 <- ggplot(data = filter(dataBike_Loc_hol, YEAR == input$year), aes(x = MEAN_24HOUR, y = HOLIDAYS)) + 
        ggtitle(paste("Holidays: Average Bicycle Traffic", input$year))
      p3 + geom_point() + 
        geom_segment(aes(x = 0, y = HOLIDAYS, xend = MEAN_24HOUR, yend = HOLIDAYS), linetype = 2) +
        labs(x = "Average 24hr Volume", y = "") + 
        theme(axis.text=element_text(size=7)) +
        theme_bw()
      
    } else {
      
      p3 <- ggplot(data = filter(dataBike_Loc2, YEAR == input$year), aes(x = MEAN_24HOUR, y = LOCATION)) +
        ggtitle(paste("Location: Average Bicycle Traffic", input$year))
      p3 + geom_point() +
        geom_segment(aes(x = 0, y = LOCATION, xend = MEAN_24HOUR, yend = LOCATION), linetype = 2) +
        labs(x = "Average 24hr Volume", y = "") +
        theme(axis.text=element_text(size=7)) +
        theme_bw()
    }
  })
  
  
  # Box 4. Plot of Day vs Rainfall or Maximum Temperature for each Month
  output$six <- renderPlotly({
    if (input$weather == "RAINFALL_MM") {
      ylabel = "Rainfall (mm)"
      mainLabel = paste("Daily Rainfall", month.name[input$mon], input$year)
    } else {
      ylabel = "Maximum Temperature (C)"
      mainLabel = paste("Daily Max Temp", month.name[input$mon], input$year)
    }
    
    p3 <- ggplot(data = filter(dataBike_Day, YEAR == input$year & MONTH == input$mon), aes(x = DAY, y = get(input$weather))) +
      ggtitle(label = mainLabel)
    p3 + geom_line() +
      geom_point(aes(colour = WEEKEND)) +
      labs(x = "Day of Month", y = ylabel) +
      guides(colour = guide_legend(override.aes = list(size = 4))) +
      guides(size = FALSE) +
      theme_bw()
  })
  
  output$dataTable <- DT::renderDataTable({
    dataBike_Loc
  })
  
}
shinyApp(ui, server)