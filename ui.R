library(gt)
library(leaflet)
library(plotly)
library(shiny)

ch <- 400

shiny::fluidPage(
  shiny::titlePanel("Abandoned Wells Liabilities"),
  shiny::br(),
  shiny::sidebarLayout(
    fluid = TRUE, 
    shiny::sidebarPanel(
      width = 2,
      shiny::selectInput("company", "Select Company: ",
                            choices = sort(unique(wellsWithInfo$Licensee))), 
      shiny::selectInput("fluid", "Select Fluid: ",
                            choices = NULL, multiple = TRUE, selectize = FALSE), 
      shiny::selectInput("depth", "Select Depth: ", # Change to ranges 
                            choices = NULL, multiple = TRUE, selectize = FALSE), 
      shiny::actionButton("update", "Update Data")
      ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Analysis", 
                        shiny::br(),
                        plotly::plotlyOutput("industryComp", height = ch), 
                        shiny::br(), 
                        shiny::br(), 
                        plotly::plotlyOutput("fluidChart", height = ch), 
                        shiny::br(), 
                        shiny::br(), 
                        plotly::plotlyOutput("depthChart", height = ch) 
        ), 
        shiny::tabPanel("Map", 
                        leaflet::leafletOutput("map", height = "90vh") 
                        ),
        shiny::tabPanel("Cost Tables", 
                        shiny::column(10, align = "center", 
                          shiny::selectInput("costArea", "Select Area", 
                                           choices = sort(unique(dataCost$Location)))
                          ), 
                        shiny::column(10, align = "center", 
                          shiny::uiOutput("links"),
                          gt::gt_output("cost"), 
                          shiny::br(), 
                          shiny::br(), 
                          gt::gt_output("recGT")
                        )
                      )
        )
      )
    )
  )
