library(shiny)
library(dplyr)
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
#add these when figure out writing to google sheet:
#colnames(Project)[13] <- "coords"
#colnames(Project)[14] <- "lat"
#colnames(Project)[15] <- "long"

#check if we didn't already geocode and save back to the google docs
#Project$coords <- ifelse(is.na(Project$coords), geocode(as.character(Project$pi_city), source = "dsk", Project$coords))
Project$coords <- geocode(as.character(Project$pi_city), source="dsk")

Project$lat <- as.numeric(Project$coords$lat)
Project$long <- as.numeric(Project$coords$lon)


#write this column back to the goole docs, so we don't have to geocode everything every time someone loads the app
#figure out how to write back to cols in the google docs
#probably making use of gs_edit_cells() ?


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "80%"),
  #put textOutput() here with 
  uiOutput("selected_proj"),
  absolutePanel(top = 10, right = 10)
)

server <- function(input, output, session) {
  
  #create the text output with HTML 
  output$selected_proj <- renderUI({
    HTML(paste0(
      '<td valign="top" style = "padding-left:30px; padding-top:15px;">',
      "Click on a project on the map for more information",
      "</td>"
    ))
  })
  
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
  
  # sel_project <- reactive(input$map_marker_click, {
  #   click <- input$map_marker_click
  #   sel_project <- Project[which(Project$lat == click$lat & Project$long == click$lng),]
  #   sel_project[is.na(sel_project)] <- ""
  #   
  # })
  # 
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    sel_project <- Project %>% 
         filter(lat == click$lat & long == click$lng)

    output$selected_proj <- renderUI({
      HTML(paste0(
        '<td valign="top" style = "padding-left:30px; padding-top:15px;">',
        "<b>Project title:</b> ", sel_project$title, 
        "<br> <b> PI institution </b>", sel_project$pi_institute,
        "<br> <b> PI name: </b>", sel_project$pi_name,
        "<br> <b> Other researchers: </b>", sel_project$coi_names,
        "<br> <b> Other researchers institutes: </b>", sel_project$coi_institutes,
        "<br> <b> Research level: </b>", sel_project$level,
        "<br> <b> Introduction: </b>", sel_project$intro,
        "<br> <b> Methodology: </b>", sel_project$methods,
        "<br> <b> Reports and publications: </b>", sel_project$publications,
        "<br> <b> Website: </b>", sel_project$website, 
        "</td>"
      ))
    })
    
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