#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidync)
library(dplyr)
f <- "/rdsi/PRIVATE/raad/data_local/acecrc.org.au/ROMS/s_corney/cpolar/ocean_his_3101.nc"
varname <- "temp"

rnc <- tidync(f) %>% activate(varname) %>% 
  hyper_filter(xi_rho = between(index, 500, 700), eta_rho = between(index, 300, 392), 
               ocean_time = index == 31)
scl <- function(x) (x + 2)/(30 + 2)
breaks <- seq(-2, 30, length = 100)
cols <- viridis::viridis(99)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("slice",
                     "Slice",
                     min = 1,
                     max = 31,
                     value = 1, animate = animationOptions(interval = 500))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("romsPlot")
      )
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$romsPlot <- renderPlot({
     mat <- (rnc %>% hyper_filter(s_rho = index == input$slice) %>% hyper_slice(select_var = varname))[[1]]
      mat[] <- cols[scl(mat[])*99 + 1]
      mat[is.na(mat)] <- cols[1]
      plot(NA, xlim = c(0, 100), ylim = c(0, 100))
#      image(lmat[[1]], col = cols, breaks = breaks, useRaster = TRUE)
      rasterImage(t(mat[,ncol(mat):1]),xleft = 0, ybottom = 0, xright = 100, ytop = 100 )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

