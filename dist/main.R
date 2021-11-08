# aplication shiny
shinyApp(
  ui = navbarPage("US COVID-19 Explorer", theme = shinytheme("flatly"),
                  tabPanel("View Map",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Choose options to visualise"),
                               selectInput("state", 
                                           label = "States:",
                                           choices = c(unique(df$State)),
                                           selected = state.name[1],
                                           width = 350),
                               selectInput("sex", 
                                           label = "Choose sex:",
                                           choices = c(unique(df$Sex)),
                                           selected = "Male",
                                           width = 350),
                               
                               selectInput("age", 
                                           label = "Choose age group:",
                                           choices = unique(df$`Age group`),
                                           selected = unique(df$`Age group`)[7],
                                           width = 350),
                               
                               selectInput("variable", 
                                           label = "Choose variable to chart:",
                                           choices = colnames(df)[-(1:4)],
                                           selected = colnames(df)[-(1:4)][3],
                                           width = 350),
                               width = 3
                             ),
                             mainPanel(
                               fluidRow(
                                 # Map
                                 box(
                                   title = "USA Map",
                                   width = 12,
                                   plotlyOutput("graph")
                                 ),
                               )
                             ) #plotly
                           )
                  ),
                  tabPanel("View bar for age group",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Bar visualization"),
                               selectInput("state2",
                                           label = "States:",
                                           choices = c(unique(df$State)),
                                           selected = state.name[1],
                                           width = 350),
                               selectInput("variable2",
                                           label = "Choose variable to chart ",
                                           choices = colnames(df)[-(1:4)],
                                           selected = colnames(df)[-(1:4)[3]],
                                           width = 350),
                               width = 30
                             ),
                             mainPanel(
                               fluidRow(
                                 column(
                                   12, 
                                   h3("Variables by Age group"),
                                   title = "Age Groups",
                                   plotlyOutput("chart_1")
                                 ),
                               )
                             )
                           )
                  ), # plotly view bar visualization
                  tabPanel("View bar for state",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Bar visualization by Cases Deaths"),
                               width = 30
                             ),
                             mainPanel(
                               fluidRow( 
                                 column(
                                   12,
                                   h3("Cases of Death for state"),
                                   title = "Cases of Deaths",
                                   plotlyOutput("chart_2"))
                               )
                             )
                           )
                  ), # heatmaply 
                  tabPanel("View heatmap",  
                           sidebarLayout(
                             sidebarPanel(
                               h3("Bar visualization"),
                               selectInput("state2",
                                           label = "States:",
                                           choices = c(unique(df$State)),
                                           selected = state.name[1],
                                           width = 350),
                               selectInput("casesdeath",
                                           label = "Cases Deaths ",
                                           choices = colnames(df_structure)[1:5],
                                           selected = unique(df_structure)[1:5][3],
                                           width = 350),
                               width = 30
                             ),
                             mainPanel(
                               "Heatmaply",
                               plotOutput("themap"),
                               tableOutput("table.output")
                             ) 
                           )
                  )
  ),
  server <- function(input, output) {
    
    output$graph <- renderPlotly({
      data_state <- melt(df, id = c("State", "Sex", "Age group"))
      data_state$code <- state.abb[match(df$State,state.name)]
      data_state <- df %>%
        filter(code != "NA") %>%
        filter(Sex == input$sex) %>%
        filter(`Age group` == input$age) %>%
        filter(variable == input$variable)
      
      colorbar_label = input$variable
      map_title = paste("United States Statistics on", input$variable)
      
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
      fig <- plot_geo(df, locationmode = 'USA-states')
      fig <- fig %>% 
        add_trace(z = ~value, locations = ~code,
                  text = ~State,
                  color = ~value, colors = 'YlOrRd' #viridis
        )
      
      fig <- fig %>% 
        colorbar(title = colorbar_label)
      fig <- fig %>% layout(
        title = map_title,
        annotations = 
          list(x = 1,
               y = -0.1,
               text = "Provisional COVID-19 Death Counts by Sex, Age, and State by https://data.cdc.gov/", 
               showarrow = F, xref = 'paper', yref = 'paper', 
               xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
               font = list(size = 10, color = "blue"))
      )
      
      fig <- fig %>% 
        layout(geo = g)
      fig
    })
    ##################
    #
    output$chart_1 <- renderPlotly({
      data_state <- df %>%
        filter(State == input$state2) %>%
        filter(Sex != "All Sexes") %>%
        filter(`Age group` != "All Ages")
      chart_1_title = paste0(input$state2, ", ", input$variable2)
      casted <- dcast(df, State + `Age group` ~ Sex, value.var = "Total Deaths")
      casted$`Age group` <- factor(casted$`Age group`, 
                                   levels = c("Under 1 year", "1-4 years",  
                                              "5-14 years", "0-17 years", 
                                              "15-24 years", "18-29 years", 
                                              "25-34 years", "30-49 years", 
                                              "35-44 years", "45-54 years",
                                              "50-64 years", "55-64 years", 
                                              "65-74 years", "75-84 years",
                                              "85 years and over"))
      
      fig <- plot_ly(casted, x = ~`Age group`, y = ~Female, type = 'bar', name = 'Female')
      fig <- fig %>%
        add_trace(y = ~Male, name = "Male")
      fig <- fig %>% layout(legend = list(x = 0.1, y = 0.9),
                            title = chart_1_title)
      fig
    })
    
    ###########
    output$chart_2 <- renderPlotly({
      data_state <- df %>%
        select(State, `Total Deaths`, 
               `Pneumonia Deaths`,
               `Pneumonia and COVID-19 Deaths`, 
               `Influenza Deaths`,
               `Pneumonia, Influenza, or COVID-19 Deaths`,
               `COVID-19 Deaths`) %>%
        group_by(State) %>%
        filter(State != 'United States') %>%
        filter(`Total Deaths` != 'NA') %>%
        filter(`COVID-19 Deaths` != 'NA') %>%
        filter(`Pneumonia Deaths` != 'NA') %>%
        filter(`Pneumonia and COVID-19 Deaths` != 'NA') %>%
        filter(`Influenza Deaths` != 'NA') %>%
        filter(`Pneumonia, Influenza, or COVID-19 Deaths` != 'NA') %>%
        summarise(`Total Deaths` = sum(`Total Deaths`),
                  `COVID-19 Deaths` = sum(`COVID-19 Deaths`),
                  `Pneumonia Deaths` = sum(`Pneumonia Deaths`),
                  `Pneumonia and COVID-19 Deaths` = sum(`Pneumonia and COVID-19 Deaths`),
                  `Influenza Deaths` = sum(`Influenza Deaths`),
                  `Pneumonia, Influenza, or COVID-19 Deaths` = sum(`Pneumonia, Influenza, or COVID-19 Deaths`)) %>%
        mutate(State = as.factor(State),
               `Total Deaths` = as.integer(`Total Deaths`),
               `COVID-19 Deaths` = as.integer(`COVID-19 Deaths`),
               `Pneumonia Deaths` = as.integer(`Pneumonia Deaths`),
               `Pneumonia and COVID-19 Deaths` = as.integer(`Pneumonia and COVID-19 Deaths`),
               `Influenza Deaths` = as.integer(`Influenza Deaths`),
               `Pneumonia, Influenza, or COVID-19 Deaths` = as.integer(`Pneumonia, Influenza, or COVID-19 Deaths`)) 
      chart_2_tilte = paste("Cases of Deaths ")
      #casted <- dcast(data_state, State ~ Pneumonia_Deaths)
      #casted$State <- factor(casted$State)
      
      fig <- plot_ly(df, x = ~State, y = ~`COVID-19 Deaths`, type = 'bar', name = 'COVID-19 Deaths')
      fig <- fig %>%
        add_trace(y = ~`Pneumonia Deaths`, name = "Pneumonia Deaths") %>%
        add_trace(y = ~`Pneumonia and COVID-19 Deaths`, name = "Pneumonia and COVID-19 Deaths") %>%
        add_trace(y = ~`Influenza Deaths`, name = "Influenza Deaths") %>%
        add_trace(y = ~`Pneumonia, Influenza, or COVID-19 Deaths`, name = "Pneumonia, INfluenza, or COVID-10 Deaths")
      fig <- fig  %>%
        layout(legend = list(x = 0.5, y = 0.9), 
               title = chart_2_tilte)
      fig
      
    })
  }
)


