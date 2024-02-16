library(shiny)
library(gt)

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
      shiny::actionButton("update", "Update Data")),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Analysis", 
                        shiny::mainPanel(
                          plotly::plotlyOutput("industryComp", height = ch), 
                          shiny::br(), 
                          shiny::br(), 
                          plotly::plotlyOutput("fluidChart", height = ch), 
                          shiny::br(), 
                          shiny::br(), 
                          plotly::plotlyOutput("depth", height = ch), 
                        )
        ), 
        shiny::tabPanel("Map", 
                        # Pull unique fields + filter poolRegion 
                        leaflet::leafletOutput("map", height = "90vh") 
                        ),
        shiny::tabPanel("Cost Tables", 
                        shiny::sidebarPanel(
                          #Select Inputs
                        ),
                        shiny::mainPanel(
                          shiny::fluidRow(
                            shiny::uiOutput("links"),
                            gt::gt_output("cost"), 
                            gt::gt_output("depth")
                          )
                          
                          # Create GT from dataCost and recCost 
                          # Have link to both Directive 006 + Directive 011 
                        )
                        )
      )
    )
  )
)
