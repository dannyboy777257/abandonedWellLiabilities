library(dplyr)
library(gt)
library(leaflet)
library(plotly)
library(shiny)
library(tidyr)

function(input, output, session) {
  shiny::observeEvent(input$update, {
    shiny::req(input$company, input$fluid, input$depth)
    r$wellsWithInfo <- wellsWithInfo %>% 
      dplyr::filter(Licensee == input$company, 
                    Fluid %in% input$fluid, 
                    FinalTotalDepth %in% input$depth)
    
    fieldName <- unique(r$wellsWithInfo$FieldName)

    r$poolRegion <- poolRegion %>% 
      dplyr::filter(FIELD_NAME %in% fieldName)
  })
  
  shiny::observeEvent(input$company, {
    shiny::updateSelectInput(session, "fluid",
                                choices = sort(unique(wellsWithInfo$Fluid[which(wellsWithInfo$Licensee == input$company)])))
  })
  
  shiny::observeEvent(list(input$company, input$fluid), {
    shiny::req(input$company, input$fluid)
    shiny::updateSelectInput(session, "depth",
                                choices = wellsWithInfo %>% 
                                           dplyr::filter(Fluid %in% input$fluid,
                                                         Licensee == input$company) %>%
                                           dplyr::distinct() %>%
                                           dplyr::arrange(desc(FinalTotalDepth)) %>% 
                                           dplyr::pull(FinalTotalDepth))
  })
  
  output$map <- leaflet::renderLeaflet({
    shiny::req(r$wellsWithInfo, r$poolRegion)
    color_palette <- leaflet::colorFactor(palette = "Set1", domain = r$poolRegion$FIELD_NAME)
    
    leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::setView(lng = -99.818720, lat = 48.780712, zoom = 4) %>% 
      leaflet::addPolygons(data = r$poolRegion,
                           fillColor = ~color_palette(FIELD_NAME),
                           color = "#00000F",
                           fillOpacity = 0.08,
                           weight = 1) %>%
      leaflet::addPolygons(data = abandonmentArea,
                           fillColor = ~"pink",
                           color = "#0573e3",
                           fillOpacity = 0.08) %>% 
      leaflet::addPolygons(data = reclamationMap,
                           fillColor = ~"white",
                           color = "white",
                           fillOpacity = 0.08) %>% 
      leaflet::addCircleMarkers(data = r$wellsWithInfo, 
                                popup = ~paste(paste0("Licence: ", Licence), "<br>", paste0("DLS: ", SurfLoc)),
                                col = 'red',
                                radius = 0.07) 
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
      dplyr::select(-reclamationArea, -Fluid, -maxCost, -FinalTotalDepth) %>% 
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
      dplyr::select(-reclamationArea, -grep("_Cost", colnames(.), value = TRUE)) %>% 
      dplyr::group_by(Fluid, FinalTotalDepth) %>%
      dplyr::summarise(maxCosts = sum(maxCost)) %>% 
      plotly::plot_ly(x = ~Fluid, y = ~maxCosts, type = "bar", color = ~FinalTotalDepth) %>% 
      plotly::layout(xaxis = list(title = "Fluid"), yaxis = list(title = "Maximum Costs ($)"), 
                     title = paste("Max Costs for", input$company), barmode = "stack")
  })
  
  output$depthChart <- plotly::renderPlotly({
    shiny::req(r$data)

    r$data %>% 
      dplyr::select(FinalTotalDepth, maxCost) %>% 
      dplyr::group_by(FinalTotalDepth) %>% 
      dplyr::arrange(FinalTotalDepth) %>% 
      plotly::plot_ly(x = ~FinalTotalDepth, y = ~maxCost, type = "scatter", mode = "markers", 
                      marker = list(color = ~maxCost)) %>% 
      plotly::layout(xaxis = list(title = "Well Depth (m)"), yaxis = list(title = "Maximum Costs ($)"), 
                     title = paste("Max Costs for", input$company))
  })
  
  output$links <- shiny::renderUI({
    dir6 <- shiny::a("Directive 006", href = "https://static.aer.ca/prd/documents/directives/Directive006.pdf")
    dir11 <- shiny::a("Directive 011", href = "https://static.aer.ca/prd/documents/directives/Directive011_March2015.pdf")
    shiny::tagList("To view the costs please visit", dir11, 
                   ". For further information regarding the Licensee Liability Rating Program, please visit", dir6, ".")
  })
  
  output$cost <- gt::render_gt({
    shiny::req(input$costArea)
    dataCost %>% 
      dplyr::filter(Location == input$costArea) %>% 
      dplyr::select(-Location, -key) %>% 
      dplyr::rename(`Depth (m)` = Depth, 
                    `Empty Not Perforated` = Empty_not_perforated_Cost,
                    `Empty Perforate` = Empty_perforated_Cost, 
                    `Tubing Only` = Tubing_only_Cost, 
                    `Tubing and Rods` = Tubing_and_rods_Cost, 
                    `Groundwater Protection` = Groundwater_Protection_Cost, 
                    `Vent Flow Repair` = Vent_Flow_Repair_Cost, 
                    `Gas Migration` = Gas_Migration_Cost) %>% 
      gt::gt() %>% 
      # Need to use USD as CAD not in currencies
      gt::cols_align(columns = -`Depth (m)`, align = "center") %>% 
      gt::fmt_currency(columns = -`Depth (m)`, 
                       currency = "USD") %>% 
      gt::tab_header(gt::md("**Regional Well Abandonment Cost Table**"))
  })
  
  output$recGT <- gt::render_gt({
    recCost %>% 
      dplyr::rename(`Reclamation Area` = reclamationArea, 
                    `Reclamation Cost` = reclamationCost) %>% 
      gt::gt() %>% 
      gt::cols_align(columns = `Reclamation Cost`, align = "center") %>% 
      gt::fmt_currency(columns = `Reclamation Cost`, 
                       currency = "USD") %>% 
      gt::tab_header(gt::md("**Well and Facility Regional Reclamation Cost Table**"))
  })
}