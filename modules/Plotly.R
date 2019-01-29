PlotlyUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(div(
      column(
        width = 9, 
        fluidRow(
          box(
            width = 12,
            title = "Visuals",
            status = "primary",
            solidHeader = T,
            uiOutput(ns("visual"))))),
      column(
        width = 3,
        fluidRow(
          box(
            width = 13, 
            title = "Inputs",
            status = "primary",
            solidHeader = T,
            div(id = ns("Inputs"),
                
                radioButtons(inputId = ns('plottype'),
                             label = 'Choose plot type:',
                             choices = c("Scatter plot" = "scatterplot", 
                                         "Bar chart" = "barchart",
                                         "Line chart" = "linechart"),
                             selected = 'scatterplot'),
                
                conditionalPanel(condition = paste0("input['", ns("plottype"), "'] == 'scatterplot' "),
                                 selectInput(
                                   inputId = ns("feature_x"),
                                   label = "Choose x variable:",
                                   choices = c("Sepal.Width", "Sepal.Length", "Petal.Length", "Petal.Width"),
                                   selected = "Sepal.Width"),
                                 
                                 selectInput(
                                   inputId = ns("feature_y"),
                                   label = "Choose y variable:",
                                   choices = c("Sepal.Width", "Sepal.Length", "Petal.Length", "Petal.Width"),
                                   selected = "Sepal.Length")),
                
                conditionalPanel(condition = paste0("input['", ns("plottype"), "'] == 'barchart' "),
                                 
                                 radioButtons(
                                   inputId = ns('stackorparallel'),
                                   label = 'Choose display mode:',
                                   choices = c("Parallel", "Stacked"),
                                   selected = 'Parallel')
                )
            )
          )
        )
      )
    )
    )
  )
}

PlotlyFunction <- function(input, output, session) {
  
  # Scatter plot codes
  output$scatterplot_plotly <- renderPlotly({
    
    plot_ly(data = iris,
            x = ~get(input$feature_x),
            y = ~get(input$feature_y),
            color = ~Species,
            type = "scatter",
            mode = "markers") %>%
      layout(legend = list(x = 100, y = 0.5,
                           font = list(size = 14)),
             yaxis = list(zeroline = F, title = as.character(input$feature_y)),
             xaxis = list(zeroline = F, title = as.character(input$feature_x)),
             title = "Exploring the Iris dataset")
  })
  
  # Bar chart codes
  output$barchart_plotly <- renderPlotly({
    
    if (input$plottype == "barchart" & input$stackorparallel == "Stacked") {
      
      plot_ly(data = melt(energydata, id.vars = "annee"),
              x = ~annee, y = ~value, color = ~variable, type = "bar") %>% 
        layout(barmode = "stack",
               yaxis = list(title = "Electricity production (terawatt-hours)"),
               title = "Annual French electricity production by branch")
      
    } 
    else if (input$plottype == "barchart" & input$stackorparallel == "Parallel") {
      plot_ly(data = melt(energydata, id.vars = "annee"),
              x = ~annee, y = ~value, color = ~variable, type = "bar") %>% 
        layout(yaxis = list(title = "Electricity production (terawatt-hours)"),
               title = "Annual French electricity production by branch")
    }
    
  })
  
  # Line chart codes
  output$linechart_plotly <- renderPlotly({
    plot_ly(data = melt(french_electricity, id.var = "date"),
            x = ~date, y = ~value, color = ~variable,
            type = "scatter", mode ="line") %>% 
      layout(yaxis = list(title = "Megawatt (MW)"),
             title = "Monthly supply / demand balance in France (2007 - 2017)"
      )
  })
  
  # Show output based on user selection 
  observe({
    output$visual <- renderUI({
      div(
        div(
          if (input$plottype == "scatterplot") {
            plotlyOutput(session$ns("scatterplot_plotly"), width = "auto")
          }
          else if (input$plottype == "barchart") {
            plotlyOutput(session$ns("barchart_plotly"), width = "auto")
          }
          else if (input$plottype == "linechart") {
            plotlyOutput(session$ns("linechart_plotly"), width = "auto")
          }
        ))
    })
  })
}