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
   bookmarkButton(),
   sidebarLayout(
      textInput("regex", "file name search", value = "nc$"), 
      shiny::selectInput("method", "file choice method", choices = c("first", "last", "random"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        DT::dataTableOutput("nctable"),
        verbatimTextOutput("tidy_summary"),
        verbatimTextOutput("filter_summary"),
        
        tabsetPanel( 
        tabPanel("plot", 
                 uiOutput("filter_controls"),
                 verbatimTextOutput("index_summary"),
                 plotOutput("ggplot")),
        tabPanel("help", verbatimTextOutput("help_text"))
      )
   
))


server <- function(input, output) {
   output$help_text <- renderPrint({
     writeLines(c(
       "1. Enter regex searches to hone in on files/paths.", 
       "2. See set of matching files in table below, explore them.",
       "3. Modify method to resolve final (single) file choice from available matches.", 
       "4. Found a problem? Got ideas? Get in touch: https://github.com/hypertidy/tidync/issues", 
       "", "", 
       "A rancid production, powered by raadsync, raadtools, tidync, RNetCDF, ncdf4, shiny and the tidyverse.",
       "Michael Sumner, Australian Antarctic Division, Antarctic Climate and Ecosystems CRC, Hobart."))
   })
   output$filter_controls <- renderUI({
     #cities <- getNearestCities(input$lat, input$long)
     #checkboxGroupInput("cities", "Choose Cities", cities)
    s_tab <- get_summary_table()
    ndims <- nrow(s_tab)
    ord <- c(t(matrix(seq_len(nrow(s_tab) * 2), 2)))
  list(  lapply(seq_len(ndims), function(i) 
       numericInput(sprintf("%sminval", s_tab$name[i]), sprintf("%s min",  s_tab$name[i]), 
                                             value =  s_tab$min[i], min = s_tab$min[i], max = s_tab$max[i]))
    ,
    lapply(seq_len(ndims), function(i) 
      numericInput(sprintf("%smaxval",  s_tab$name[i]), sprintf("%s max",  s_tab$name[i]), 
                   value = s_tab$max[i], min = s_tab$min[i], max = s_tab$max[i]))
  )[ord]  
#     numericInput("minvals", "minval", value = 0, min = -2, max = 4)
   })
   get_index <- reactive({
    # data.frame(x = 1)
      index <- get_filter() 
      
       summ <- get_summary_table()
   #     nms <- summ$name
   #     print(nms)
   #    for (i in seq_along(nms)) {
   #      vals <- index[[nms[i]]][[nms[i]]] 
   #       index[[nms[i]]] <- index[[nms[i]]][ vals  >= summ$min[i] & vals <= summ$max[i], ]
   #     }
   #  hyper_index(index)
   # summ
   })
   get_summary_table <- reactive({
     index <- get_filter()
     nms <- names(index)
     minimums <- unlist(lapply(nms, function(x) min(index[[x]][[x]])))
     maximums <- unlist(lapply(nms, function(x) max(index[[x]][[x]])))
     input_min_names <- sprintf("%sminval", nms)
     input_max_names <- sprintf("%smaxval", nms)
     
     ## STUCK here, not sure the input object is responding well 
     #minimums <- pmin(minimums, unlist(lapply(input_min_names, function(a) input[[a]])))
     #maximums <- pmax(maximums, unlist(lapply(input_max_names, function(a) input[[a]])))
     tibble::tibble(name = nms, min = minimums, max = maximums, imx = input_max_names, imn = input_min_names)
   })
   output$index_summary <- renderPrint({
     print(get_index())
   })
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

