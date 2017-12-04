#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30), 
         textInput("controlstring", "Controls", value = "abc,def")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"), 
         uiOutput("controls")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  get_input <- function() {
    strsplit(input, ",")
  }
  output$controls <- renderUI({
    x <- get_input()
  })
  
   output$distPlot <- renderPlot({
      inp <- get_input()
      pts <- seq_along(inp)
      xy <- cbind(pts, pts)
      plot(xy)
      lapply(seq(nrow(xy)), function(x) text(xy[x, , drop = FALSE], lab = inp[[x]]))
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

