library(shiny)

shiny::fluidPage(
  shiny::titlePanel("Abandoned Wells Liabilities"),
  shiny::br(),
  shiny::sidebarLayout(
    fluid = TRUE, 
    shiny::sidebarPanel(
      width = 2,
      shiny::selectizeInput("company", "Select Company: ",
                            choices = sort(unique(wellsWithInfo$Licensee))), 
      shiny::selectizeInput("fluid", "Select Fluid: ",
                            choices = sort(unique(wellsWithInfo$Fluid))), 
      shiny::selectizeInput("depth", "Select Depth: ", # Change to ranges 
                            choices = sort(unique(wellsWithInfo$FinalTotalDepth))), 
      shiny::actionButton("update", "Update Data")),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Analysis", 
                        shiny::sidebarPanel(
                          #Select Inputs
                        ),
                        shiny::mainPanel(
                          #
                        )
        ), 
        shiny::tabPanel("Map", 
                        # Pull unique fields + filter poolRegion 
                        shiny::mainPanel(
                          leaflet::leafletOutput("map")
                        )), 
        shiny::tabPanel("Cost Tables", 
                        shiny::sidebarPanel(
                          #Select Inputs
                        ),
                        shiny::mainPanel(
                          shiny::fluidRow(
                            shiny::uiOutput("links"),
                            gt::gt_output("cost"), 
                            gt::gt_output("recCost")
                          )
                          
                          # Create GT from dataCost and recCost 
                          # Have link to both Directive 006 + Directive 011 
                        )
                        )
      )
    )
  )
)
