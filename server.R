library(gt)
library(shiny)
library(tidyverse)


function(input, output, session) {
  shiny::observeEvent(input$update, {
    shiny::req(input$company, input$fluid, input$depth)
    r$wellsWithInfo <- wellsWithInfo %>% 
      dplyr::filter(Licensee == input$company, 
                    Fluid %in% input$fluid, 
                    FinalTotalDepth %in% input$depth)
    
    # fieldName <- unique(r$wellsWithInfo$FieldName)
    # 
    # r$poolRegion <- r$poolRegion %>% 
    #   dplyr::filter(Field %in% FIELD_NAME)
    # data filters - 
    # Filter companies based on user input for the wellsWithInfo
  })
  
  shiny::observeEvent(input$company, {
    browser()
    shiny::updateSelectInput(session, "fluid",
                                choices = sort(unique(wellsWithInfo$Fluid[which(wellsWithInfo$Licensee == input$company)])))
  })
  
  shiny::observeEvent(list(input$company, input$fluid), {
    shiny::updateSelectInput(session, "depth", # Change to ranges 
                                choices = sort(unique(wellsWithInfo$FinalTotalDepth[which(wellsWithInfo$Fluid %in% input$fluid)])))
  })
  
  output$map <- leaflet::renderLeaflet({
    shiny::req(r$wellsWithInfo)
    #color_palette <- leaflet::colorFactor(palette = "Set1", domain = poolRegion$FIELD_NAME)
    
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
  
  shiny::observe({
    shiny::req(r$wellsWithInfo)
    r$data <- r$wellsWithInfo %>% 
      dplyr::tibble() %>% 
      dplyr::select(Licensee, FinalTotalDepth, reclamationArea, Fluid, grep("_Cost", colnames(.), value = TRUE)) %>% 
      dplyr::mutate(maxCost = Empty_perforated_Cost + Tubing_and_rods_Cost + Groundwater_Protection_Cost + Vent_Flow_Repair_Cost + Gas_Migration_Cost)
    
  })
  
  output$industryComp <- plotly::renderPlotly({
    shiny::req(r$data)
    r$data %>% 
      dplyr::select(-reclamationArea, -Fluid, -FinalTotalDepth) %>% 
      tidyr::pivot_longer(cols = -Licensee, names_to = "category", values_to = "costs") %>% 
      dplyr::group_by(category) %>%
      dplyr::summarise(costs = sum(costs)) %>% 
      plotly::plot_ly(x = ~category, y = ~costs, type = "bar", color = ~category) %>% 
      plotly::layout(xaxis = list(title = "Cost Category"), yaxis = list(title = "Costs ($)"), 
                     title = paste("Sum of Costs for", input$company))
  })
  
  output$fluidChart <- plotly::renderPlotly({
    shiny::req(r$data)
    r$data %>% 
      dplyr::select(-reclamationArea, -FinalTotalDepth, -grep("_Cost", colnames(.), value = TRUE)) %>% 
      dplyr::group_by(Fluid) %>%
      dplyr::summarise(maxCosts = sum(maxCost)) %>% 
      plotly::plot_ly(x = ~Fluid, y = ~maxCosts, type = "bar", color = ~Fluid) %>% 
      plotly::layout(xaxis = list(title = "Fluid"), yaxis = list(title = "Maximum Costs ($)"), 
                     title = paste("Max Costs for", input$company), barmode = "stacked")
  })
  
  output$depth <- plotly::renderPlotly({
    shiny::req(r$data)

    r$data %>% 
      dplyr::select(FinalTotalDepth, maxCost) %>% 
      dplyr::group_by(FinalTotalDepth) %>% 
      dplyr::arrange(FinalTotalDepth) %>% 
      plotly::plot_ly(x = ~FinalTotalDepth, y = ~maxCost, type = "scatter", mode = "markers", 
                      marker = list(color = ~maxCost)) %>% 
      plotly::layout(xaxis = list(title = "Fluid"), yaxis = list(title = "Maximum Costs ($)"), 
                     title = paste("Max Costs for", input$company))
  })
  
  output$links <- shiny::renderUI({
    dir6 <- shiny::a("Directive 006", href = "https://static.aer.ca/prd/documents/directives/Directive006.pdf")
    dir11 <- shiny::a("Directive 011", href = "https://static.aer.ca/prd/documents/directives/Directive011_March2015.pdf")
    shiny::tagList("To view the costs please visit", dir11, 
                   "<br> For further information regarding the Licensee Liability Rating Program, please visit", dir6)
  })
}