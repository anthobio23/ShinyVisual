

# aplication for heatmap about cases deaths
ui = fluidPage("Test",
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),
                 checkboxInput("header", "Header", TRUE)
               ),
               tabPanel('map', 
                        sidebarLayout(
                          sidebarPanel('side',
                                       actionButton('getHmap', 'get heatmap')
                          ),
                          mainPanel('main',
                                    plotOutput("themap"),
                                    tableOutput("table.output")
                          )
                        ))
)

server = function(input, output, session) {
  a <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tbl <- read.csv(inFile$datapath, header=input$header) #, sep=input$sep,  dec = input$dec)
    return(tbl)
  })
  
  output$table.output <- renderTable({
    a()
  })
  
  plotdata <- eventReactive(input$getHmap, {
    a <- as.matrix(a()[-1])
    row.names(a) <- a()$Name
    a[is.na(a)] <- 0
    a
  })
  
  output$themap = renderPlot({ 
    pheatmap(plotdata())
  })
}


shinyApp(ui, server)
