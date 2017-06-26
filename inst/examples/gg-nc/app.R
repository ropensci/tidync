library(dplyr)
files <- raadfiles:::get_raw_raad_filenames() 
root <- files$root[1]

files <- files %>% dplyr::filter(stringr::str_detect(file, "nc$")) %>% 
  distinct(basename(file), .keep_all = TRUE) %>% transmute(fullname = file.path(root, file))
library(shiny)
library(tidync)
library(ggplot2)
ui <- fluidPage(
   
   titlePanel("ggplot2 for netcdf"),
   sidebarLayout(
      textInput("regex", "file name search", value = "nc$"), 
      shiny::selectInput("method", "file choice method", choices = c("first", "last", "random"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabPanel("file",
         textOutput("tidy_summary"),
         textOutput("filter_summary"),
         DT::dataTableOutput("nctable"))
      )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   cat <- message
   output$tidy_summary <- renderPrint({
    print(get_tidync())
   })
    output$filter_summary <- renderPrint({
    print(get_filter())
    })
   get_files <- reactive({
      files %>% dplyr::filter(stringr::str_detect(basename(fullname), input$regex))
   })
   get_tidync <- reactive({
     afiles <- get_files()
     ii <- switch(input$method, 
            first = 1L, 
            last =  nrow(afiles),
            random = sample(nrow(afiles), 1L))
     tidync::tidync(afiles$fullname[ii])
   })
   get_filter <- reactive({
     tnc <- get_tidync()
     tidync::hyper_filter(tnc)
   })
   output$nctable <- DT::renderDataTable({
     pat <- sprintf("%s/", root)
     f <- get_files() %>% dplyr::transmute( 
                                   file = basename(fullname), 
                                   path = dirname(gsub(pat, "", fullname)))
     
#     tibble::as_tibble(path = gsub(pat, "", f$fullname), 
 #                      file = basename(f$fullname))
     f
   }, options = list(lengthChange = FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)

