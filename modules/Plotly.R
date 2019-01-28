PlotlyUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "min-height: 400px;",
        div(
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
                style = "min-height: 530px; overflow-x: auto",
                div(id = ns("Inputs"),
                    style = "min-width: 220px; padding-right: 5px;",
                    
                    radioButtons(inputId = ns('plottype'),
                                 label = 'Choose plot type:',
                                 choices = c("Scatter plot" = "scatterplot", 
                                             "Bar chart" = "barchart"),
                                 selected = 'scatterplot'),
                    
                    conditionalPanel(condition = paste0("input['", ns("plottype"), "'] == 'scatterplot' "),
                                     selectInput(
                                       inputId = ns("feature_x"),
                                       label = "Choose x variable:",
                                       choices = c("pregnant", "glucose", "pressure", "triceps",
                                                   "insulin", "mass", "pedigree", "age"),
                                       selected = "mass"),
                                     
                                     selectInput(
                                       inputId = ns("feature_y"),
                                       label = "Choose y variable:",
                                       choices = c("pregnant", "glucose", "pressure", "triceps",
                                                   "insulin", "mass", "pedigree", "age"),
                                       selected = "glucose")),
                    
                    conditionalPanel(condition = paste0("input['", ns("plottype"), "'] == 'barchart' "),
                                     selectInput(
                                       inputId = ns("feature_hist"),
                                       label = "Choose x variable:",
                                       choices = c("pregnant", "glucose", "pressure", "triceps",
                                                   "insulin", "mass", "pedigree", "age", "diabetes"),
                                       selected = "pregnant"),
                                     
                                     radioButtons(
                                       inputId = ns('stackorparallel'),
                                       label = 'Choose display mode:',
                                       choices = c("Stacked", "Dodge"),
                                       selected = 'Stacked')
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
    
    plot_ly(data = PimaIndiansDiabetes,
            x = ~get(input$feature_x),
            y = ~get(input$feature_y),
            color = ~diabetes,
            colors = c(Bright.Blue, Gold),
            type = "scatter",
            mode = "markers",
            hoverinfo = 'text',
            text = ~paste0('diabetes: ', diabetes,
                           '<br>',
                           as.character(input$feature_x), ': ', get(input$feature_x),
                           '</br>',
                           as.character(input$feature_y), ': ', get(input$feature_y))) %>%
      layout(legend = list(x = 100, y = 0.5,
                           font = list(size = 14)),
             yaxis = list(zeroline = F, title = as.character(input$feature_y)),
             xaxis = list(zeroline = F, title = as.character(input$feature_x)))
  })
  
  # Bar chart codes
  output$barchart_plotly <- renderPlotly({
    
    if (input$plottype == "barchart" & input$stackorparallel == "Stacked") {
      plot_ly(data = PimaIndiansDiabetes,
              x = ~get(input$feature_hist),
              color = ~diabetes,
              colors = c(Bright.Blue, Gold),
              type = "histogram") %>% 
        layout(barmode = "stack",
               legend = list(x = 100, y = 0.5,
                             font = list(size = 14)),
               yaxis = list(zeroline = F, title = "count"),
               xaxis = list(zeroline = F, title = as.character(input$feature_hist)))
    } 
    else if (input$plottype == "barchart" & input$stackorparallel == "Dodge") {
      plot_ly(data = PimaIndiansDiabetes,
              x = ~get(input$feature_hist),
              color = ~diabetes,
              colors = c(Bright.Blue, Gold),
              type = "histogram") %>% 
        layout(legend = list(x = 100, y = 0.5,
                             font = list(size = 14)),
               yaxis = list(zeroline = F, title = "count"),
               xaxis = list(zeroline = F, title = as.character(input$feature_hist)))
    }
    
  })

  # Show output based on user selection 
  observe({
    output$visual <- renderUI({
      div(
        style = "height: 530px; overflow-x: auto;",
        div(
          style = "min-width: 500px;",
          if (input$plottype == "scatterplot") {
            plotlyOutput(session$ns("scatterplot_plotly"), width = "auto")
          }
          else if (input$plottype == "barchart") {
            plotlyOutput(session$ns("barchart_plotly"), width = "auto")
          }
        ))
    })
  })
}