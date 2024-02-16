library(gt)
library(shiny)
library(tidyverse)


function(input, output, session) {
  shiny::observeEvent(input$update, {
    shiny::req(input$company, input$fluid, input$depth, r$wellsWithInfo)
    r$wellsWithInfo <- r$wellsWithInfo  %>% 
      dplyr::filter(Licensee == input$company, 
                    Fluid %in% input$fluid, 
                    FinalTotalDepth <= input$depth)
    
    # fieldName <- unique(r$wellsWithInfo$FieldName)
    # 
    # r$poolRegion <- r$poolRegion %>% 
    #   dplyr::filter(Field %in% FIELD_NAME)
    # data filters - 
    # Filter companies based on user input for the wellsWithInfo
  })
  
  output$map <- leaflet::renderLeaflet({
    shiny::req(input$company, input$fluid, input$depth)
    color_palette <- leaflet::colorFactor(palette = "Set1", domain = poolRegion$FIELD_NAME)
    
    leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::setView(lng = -99.818720, lat = 48.780712, zoom = 4) %>% 
      leaflet::addCircleMarkers(data = r$wellsWithInfo, 
                                popup = ~paste(paste0("Licence: ", Licence), "<br>", paste0("DLS: ", SurfLoc)),
                                #labelOptions = labelOptions(direction = 'auto', noHide = TRUE, textOnly = TRUE),
                                col = 'red',
                                radius = 0.07) %>% 
      # leaflet::addPolygons(data = poolRegion,
      #                      fillColor = ~color_palette(FIELD_NAME),
      #                      color = "#00000F",
      #                      fillOpacity = 0.08,
      #                      weight = 1) %>% 
      leaflet::addPolygons(data = abandonmentArea,
                           fillColor = ~"pink",
                           color = "#0573e3",
                           fillOpacity = 0.08) %>% 
      leaflet::addPolygons(data = reclamationMap,
                           fillColor = ~"white",
                           color = "white",
                           fillOpacity = 0.08)
  })
  
  output$industryComp <- plotly::renderPlotly({
    
  })
  
  output$fluidChart <- plotly::renderPlotly({
    
  })
  
  output$depthCost <- plotly::renderPlotly({
    
  })
  
  # fluid type but well depths and price 
  
  # Creat dataCost and recCost GT
  output$links <- shiny::renderUI({
    dir6 <- shiny::a("Directive 006", href = "https://static.aer.ca/prd/documents/directives/Directive006.pdf")
    dir11 <- shiny::a("Directive 011", href = "https://static.aer.ca/prd/documents/directives/Directive011_March2015.pdf")
    shiny::tagList("To view the costs please visit", dir11, 
                   "<br> For further information regarding the Licensee Liability Rating Program, please visit", dir6)
  })
}