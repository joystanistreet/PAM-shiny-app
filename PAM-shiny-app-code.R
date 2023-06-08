#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(here)
library(RColorBrewer)
library(leaflet)
library(sf)

# set up PAM metadata

pam_metadata<-read_csv(here("deployment_summary.csv")) %>% 
    mutate(project = factor(Project))

# set up other layers

eez<-read_sf("C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/Canada_EEZ/EEZ_WGS.shp") 
all_mpas<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/MPAs_WGS1984/MPAs_WGS1984.shp')
fcbb_aoi<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/FCBB_AOI_WGS1984/FCBB_AOI_WGS1984.shp')
nbw_ch<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/NBW_CH_WGS1984/NBW_CH_WGS1984.shp')
narw_ch<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/NARW_CH_WGS1984/NARW_CH_WGS1984.shp')
oecms<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/DFO_OECM_MPO_AMCEZ.shp')%>% 
  st_transform(4326)
network_sites<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/networksites_proposed_OEM_MPA_20221128/networksites_proposed_OEM_MPA_20221128.shp')%>% 
  st_transform(4326)

# assign colors to projects for plotting

colpal <- colorFactor("Set1", pam_metadata$project)


# Define UI for application


ui <- bootstrapPage(theme = shinytheme("flatly"),
                    
                    tags$style(type = "text/css", "html, body {width:100%;height:100%}",
                               "#controls {background-color: white;
                                    padding: 0 20px 20px 20px;
                       }"),
                    
                    leafletOutput("map", width = "100%", height = "100%"),
                    
                    absolutePanel(id="controls", class = "panel panel-default",
                                  # tabPanel(id="controls", class = "panel panel-default",
                                  fixed = TRUE, draggable = TRUE,
                                  top = 10, left = "auto",
                                  right = 10, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  br(),
                                  
                                  selectInput(inputId = "selectview", 
                                              label = "Select view", 
                                              choices = c('Deployments (by project)','Recording days collected')
                                  ),
                                  
                                  sliderInput("year", h4("Deployment Year"),
                                              min = min(pam_metadata$Year), 
                                              max = max(pam_metadata$Year), 
                                              value = range(pam_metadata$Year),
                                              step = "1",
                                              sep = "",
                                              ticks=FALSE),
                                  
                                  conditionalPanel(
                                      
                                      condition = "input.selectview == 'Deployments (by project)'",
                                      
                                      checkboxGroupInput("project", h4("Project"), 
                                                         choices = NULL),
                                  ),
                                  
                                  br(),
                                  
                                  h4("Other Layers"),
                                  
                                  checkboxInput("all_mpas","Marine Protected Areas",
                                                value = F),
                                  
                                  checkboxInput("fcbb_aoi","Fundian Channel-Browns Bank AOI",
                                                value = F),
                                  
                                  checkboxInput("oecms","Other Effective Area-based Conservation Measures",
                                                value = F),
                                  
                                  checkboxInput("nbw_ch","Northern Bottlenose Whale Critical Habitat",
                                                value = F),
                                  
                                  checkboxInput("narw_ch","North Atlantic Right Whale Critical Habitat",
                                                value = F),
                                  
                                  checkboxInput("network_sites","Proposed Conservation Network Sites",
                                                value = F),
                                  
                                  checkboxInput("eez","EEZ",
                                                 value = F)
                                  #checkboxInput("hague_line","Canada/USA EEZ Boundary",
                                  #              value = F)
                                  #  ),
                                  
                                  #conditionalPanel(
                                  
                                  #   condition = "input.selectview == 'Recording days collected'",
                                  #     
                                  #     sliderInput("year", h4("Deployment Year"),
                                  #                 min = min(pam_metadata$Year), 
                                  #                 max = max(pam_metadata$Year), 
                                  #                 value = range(pam_metadata$Year),
                                  #                 step = "1",
                                  #                 sep = "",
                                  #                 ticks=FALSE),
                                  # )
                                  # 
                                  # h4("Other Layers"),
                                  # 
                                  # checkboxInput("all_mpas","Marine Protected Areas",
                                  #               value = F),
                                  # 
                                  # checkboxInput("fcbb_aoi","Fundian Channel-Browns Bank AOI",
                                  #               value = F),
                                  # 
                                  # checkboxInput("nbw_ch","Northern Bottlenose Whale Critical Habitat",
                                  #               value = F),
                                  # 
                                  # checkboxInput("narw_ch","North Atlantic Right Whale Critical Habitat",
                                  #               value = F),
                                  # 
                                  # checkboxInput("canada_eez","Canada EEZ",
                                  #               value = F)
                                  # )
                                  
                    )
)



# Define server logic

server <- function(input, output, session) {
    
    #outputOptions(output, "selectview", suspendWhenHidden=FALSE)
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            #addProviderTiles(providers$Esri.OceanBasemap) %>%
            addProviderTiles("Esri.OceanBasemap",
                             options = providerTileOptions(
                               variant = "Ocean/World_Ocean_Base")) %>%
            clearShapes() %>%
            clearMarkers() %>% 
            
            # focus map in a certain area / zoom level
            setView(lng = -58, lat = 42, zoom = 5.5) %>% 
            
            # add measuring tool
            addMeasure(
                position = "bottomright",
                primaryLengthUnit = "kilometers",
                primaryAreaUnit = "sqkilometers") 
        
    })  
    
    
    # subset data based on year(s) selected
    filtered_years <- reactive({
        pam_metadata[pam_metadata$Year>=input$year[1] & pam_metadata$Year<=input$year[2],]
    })
    
    # update project checkboxes based on year(s) selected
    observeEvent(filtered_years(), {
        
        # projects included in selected range of years
        project_choices<-unique(filtered_years()$project)
        
        # update project checkbox options
        updateCheckboxGroupInput(session, "project",
                                 choices = project_choices,
                                 selected = project_choices)
    })
    
    # show PAM sites based on user input
    
    observe({
        
        selected_data<-filtered_years() %>% 
            filter(project %in% input$project) %>% 
            arrange(Year)
        
        leafletProxy("map", data = selected_data)%>% 
            
            clearMarkers() %>%
            addCircleMarkers(
                lat = ~Latitude,
                lng = ~Longitude,
                col = "black",
                fillColor = ~colpal(project),
                fillOpacity = 1,
                weight = 2.5,
                radius = 5,
                stroke = T,
                popup = ~Deployment)
    })
    
    #}
    
    # add other layers based on user input
    
    observe({
        
        proxy <- leafletProxy("map")
        
        proxy %>% clearGroup("MPA")
        
        if (input$all_mpas) {
            
            proxy %>% addPolygons(data = all_mpas,
                                  group = "MPA",
                                  stroke = T,
                                  weight = 1,
                                  color = 'green',
                                  fill = T,
                                  fillColor = 'green',
                                  fillOpacity = 0.25,
                                  smoothFactor = 3)}
    })
    
    observe({
        
        proxy <- leafletProxy("map")
        
        proxy %>% clearGroup("AOI")
        
        if (input$fcbb_aoi) {
            
            proxy %>% addPolygons(data = fcbb_aoi,
                                  group = "AOI",
                                  stroke = T,
                                  weight = 1,
                                  color = 'orange',
                                  fill = T,
                                  fillColor = 'orange',
                                  fillOpacity = 0.25,
                                  smoothFactor = 3)}
    })
    
    observe({

      proxy <- leafletProxy("map")

      proxy %>% clearGroup("OECM")

      if (input$oecms) {

        proxy %>% addPolygons(data = oecms,
                              group = "OECM",
                              stroke = T,
                              weight = 1,
                              color = 'blue',
                              fill = T,
                              fillColor = 'blue',
                              fillOpacity = 0.25,
                              smoothFactor = 3)}
    })

    observe({
      
      proxy <- leafletProxy("map")
      
      proxy %>% clearGroup("NETWORK")
      
      if (input$network_sites) {
        
        proxy %>% addPolygons(data = network_sites,
                              group = "NETWORK",
                              stroke = T,
                              weight = 1,
                              color = 'purple',
                              fill = T,
                              fillColor = 'purple',
                              fillOpacity = 0.25,
                              smoothFactor = 3)}
    })
    
    
    observe({
        
        proxy <- leafletProxy("map")
        
        proxy %>% clearGroup("NBWCH")
        
        if (input$nbw_ch) {
            
            proxy %>% addPolygons(data = nbw_ch,
                                  group = "NBWCH",
                                  stroke = T,
                                  weight = 1,
                                  color = 'red',
                                  fill = T,
                                  fillColor = 'red',
                                  fillOpacity = 0.25,
                                  smoothFactor = 3)}
    }) 
    
    observe({
        
        proxy <- leafletProxy("map")
        
        proxy %>% clearGroup("NARWCH")
        
        if (input$narw_ch) {
            
            proxy %>% addPolygons(data = narw_ch,
                                  group = "NARWCH",
                                  stroke = T,
                                  weight = 1,
                                  color = 'red',
                                  fill = T,
                                  fillColor = 'red',
                                  fillOpacity = 0.25,
                                  smoothFactor = 3)}
    }) 
    
    observe({

        proxy <- leafletProxy("map")

        proxy %>% clearGroup("EEZ")

        if (input$eez) {

            proxy %>% addPolylines(data = eez,
                                  group = "EEZ",
                                  #stroke = T,
                                  weight = 2,
                                  color = 'blue',
                                  #fill = F,
                                  smoothFactor = 0.5)}
    })
    
    # observe({
    #     
    #     proxy <- leafletProxy("map")
    #     
    #     proxy %>% clearGroup("EEZ")
    #     
    #     if (input$hague_line) {
    #         
    #         proxy %>% addPolylines(data = hague_line,
    #                               group = "EEZ",
    #                               stroke = T,
    #                               weight = 2,
    #                               color = 'blue',
    #                               fill = F,
    #                               smoothFactor = 3)}
    # })
}
#    
#    } else if (sel_view == "B") {
#        
#        leafletProxy("map") %>% 
#            
#            clearMarkers() %>% 
#            clearShapes()
#    }
# )
#    


# observe({
#              
#              proxy<-leafletProxy("map")
#              proxy<-clearGroup() 
#                  
#                  if (input$nbw_ch) {
# 
#                      proxy %>% addPolygons(data = nbw_ch,
#                                  group = "CH",
#                                  stroke = T,
#                                  weight = 1,
#                                  color = 'grey',
#                                  fill = T,
#                                  fillColor = 'grey',
#                                  fillOpacity = 0.25,
#                                  smoothFactor = 3)}
#              })
#              
#              #else{ clearGroup(., "CH")}
#              
#              



# Run the application 
shinyApp(ui = ui, server = server)
