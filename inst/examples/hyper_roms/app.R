library(raadtools)
library(dplyr)
library(shiny)
library(tidync)
library(mapview)
files <- cpolarfiles()
vars <- tidync::tidync(files$fullname[1])$variable %>% filter(ndims > 3)

ui <- fluidPage(
   
   # Application title
   titlePanel("Southern Ocean ROMS"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("file", "File:", choices = basename(files$fullname)), 
        uiOutput("variableUI"),
        uiOutput("z_indexUI")
        
              ),
      
      mainPanel(
         cubeViewOutput("cubePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  get_nc <- reactive({
   nc <- tidync::tidync(get_file()) %>%  activate(input$variable)
  #print(nc)
   nc
   })
  get_depth <- reactive({
    as.integer(input$z_index)
  })
  get_slab <- reactive({
    print(get_depth())
#    if (is.null(input$z_index)) return(NULL)
    get_nc() %>% hyper_slice(s_rho = step == get_depth(), xi_rho  = between(step, 200, 500), 
                             eta_rho = between(eta_rho, 100, 200))
  })
  get_file <- reactive({
    file <- file.path(dirname(files$fullname[1]), input$file)
  file
    })
   output$cubePlot <- renderCubeView({
     b <- flip(brick(get_slab(), transpose = TRUE), "y")
     #plot(subset(b, 1:2), main = get_depth())
   mapview::cubeView(b)
     })
   output$variableUI <- shiny::renderUI({
     selectInput("variable", "Variable:", choices = vars$name)
   })
   output$z_indexUI <- shiny::renderUI({
     numericInput("z_index", "S level:", min = 1, max = 31, value = 31)
   })
   
   }

# Run the application 
shinyApp(ui = ui, server = server)

