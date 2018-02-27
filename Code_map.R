library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggmap)
library(googlesheets)

Data <- gs_title("New_methods")

Project <- gs_read(ss=Data, ws = "Sheet_1", skip=0)

colnames(Project)[2] <- "title"
colnames(Project)[3] <- "pi_name"
colnames(Project)[4] <- "pi_institute"
colnames(Project)[5] <- "pi_city"
colnames(Project)[6] <- "coi_names"
colnames(Project)[7] <- "coi_institutes"
colnames(Project)[8] <- "level"
colnames(Project)[9] <- "intro"
colnames(Project)[10] <- "methods"
colnames(Project)[11] <- "publications"
colnames(Project)[12] <- "website"

Project$coords <- geocode(as.character(Project$pi_city), source = "dsk")

Project$lat <- as.numeric(Project$coords$lat)
Project$long <- as.numeric(Project$coords$lon)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "80%"),
  #put textOutput() here with 
  absolutePanel(top = 10, right = 10)
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    pal <- colorFactor(c("navy", "red", "orange", "purple"), domain = unique(Project$level))
    
    leaflet(Project) %>%
      setView(lng = -0.1278328, lat = 51.50726, zoom = 2) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(clusterOptions = markerClusterOptions(),
                       color = ~pal(level),
                       fillOpacity = 0.9,
                       popup = ~paste0("<b>Project title:</b> ", title, 
                                       "<br> <b> PI name: </b>", pi_name,
                                       "<br> <b> PI institution </b>", pi_institute,
                                       "<br> <b> Other researchers: </b>", coi_names,
                                       "<br> <b> Research level: </b>", level),
                       radius = 10)
  })
  
##Add text output "<b>Project title:</b> ", title, 
#  "<br> <b> PI institution </b>", pi_institute,
#  "<br> <b> PI name: </b>", pi_name,
#  "<br> <b> Other researchers: </b>", coi_names,
#  "<br> <b> Other researchers institutes: </b>", coi_institutes,
#  "<br> <b> Research level: </b>", level,
#  "<br> <b> Introduction: </b>", intro,
#  "<br> <b> Methodology: </b>", methods,
#  "<br> <b> Reports and publications: </b>", publications,
#  "<br> <b> Website: </b>", website
  
  
}

shinyApp(ui, server)