library(shiny)
library(leaflet)
library(rgdal)
library(raster)

ukCRS <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448, 
           -125.157,542.060,0.1502,0.2470,0.8421,-20.4894 +units=m +no_defs"
url_met <- a("here.", 
             href = "https://www.metoffice.gov.uk/binaries/content/assets/metofficegovuk/pdf/research/ukcp/ukcp18-guidance---representative-concentration-pathways.pdf", 
             target = "_blank")
url_git <- a("GitHub page", href = "https://github.com/zibbini/minor_projects/tree/master/SLR_Plymouth/shiny", target = "_blank")

ui <- fluidPage(
    titlePanel("The impact of sea level rise on Plymouth"),
    
    sidebarPanel(
        selectInput(inputId = "model", 
                    label = "Select an RCP model:", 
                    choices = c("2.6", "4.5", "8.5")),
        sliderInput(inputId = "slider",
                    label = "Select a year:",
                    min = 2019, 
                    max = 2299,
                    value = 2019, 
                    step = 1, 
                    sep = ""),
        tags$div(header = TRUE, checked = NA,
                 tags$b("Information:"),
                 br(),
                 tagList("Guidance on how to intepret the various representative concentration pathways (RCP) can be found ", url_met),
                 br(),
                 br(),
                 tagList("For more information on this dashboard, please refer to the following ", url_git, icon("github"))
        )
    ),
    mainPanel(
        leafletOutput(outputId = "map")
    )
)

server <- function(input, output){
    
    layer <- reactive({
        raster(paste0("./Data/", paste0("rcp", input$model, "/"), input$slider, ".tif"), crs = ukCRS)
    })
    
    output$map <- renderLeaflet({
        withProgress(message = 'Creating map...', value = 0.5, {
            leaflet() %>%
                addTiles() %>%
                addRasterImage(layer(), colors = "red", opacity = 0.8)
        })
    })
}

shinyApp(ui, server)