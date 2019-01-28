BillboarderUI <- function(id) {
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
                                       choices = c("Parellel", "Stacked"),
                                       selected = 'Parellel')
                                     )
                    )
                )
              )
            )
          )
        )
    )
}

BillboarderFunction <- function(input, output, session) {
  
  # Scatter plot codes
  output$scatterplot_billboarder <- renderBillboarder({
    
    billboarder() %>% 
      bb_scatterplot(data = iris,
                     x = input$feature_x,
                     y = input$feature_y,
                     group = "Species") %>% 
      bb_x_grid(show = TRUE) %>% 
      bb_y_grid(show = TRUE) %>% 
      bb_colors_manual() %>% 
      bb_x_axis(label = list(text = as.character(input$feature_x)),
                tick = list(fit = FALSE)) %>% 
      bb_y_axis(label = list(text = as.character(input$feature_y))) %>%
      bb_zoom(enabled = list(type = "drag"), resetButton = list(text = "Unzoom")) %>% 
      bb_labs(title = "Exploring the Iris dataset") %>% 
      bb_legend(position = "right")
  })
  
  # Bar chart codes
  output$barchart_billboarder <- renderBillboarder({
    if(input$plottype == "barchart" & input$stackorparallel == "Stacked") {
      
      billboarder() %>%
        bb_barchart(data = energydata_wide, stacked = T) %>%
        bb_y_grid(show = TRUE) %>%
        bb_y_axis(label = list(text = "Electricity production (terawatt-hours)")) %>% 
        bb_labs(title = "Annual French electricity production by branch") %>% 
        bb_legend(position = "right") %>% 
        bb_zoom(enabled = list(type = "drag"), resetButton = list(text = "Unzoom")) 
    }
    else if (input$plottype == "barchart" & input$stackorparallel == "Parellel") {
      billboarder() %>%
        bb_barchart(data = energydata_wide, stacked = F) %>%
        bb_y_grid(show = TRUE) %>%
        bb_y_axis(label = list(text = "Electricity production (terawatt-hours)")) %>% 
        bb_labs(title = "Annual French electricity production by branch") %>% 
        bb_legend(position = "right") %>% 
        bb_zoom(enabled = list(type = "drag"), resetButton = list(text = "Unzoom")) 
     
    }
 })
  
  # Line chart codes
  output$linechart_billboarder <- renderBillboarder({
    billboarder() %>% 
      bb_linechart(data = french_electricity, type = "spline") %>% 
      bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>% 
      bb_x_grid(show = TRUE) %>% 
      bb_y_grid(show = TRUE) %>% 
      bb_colors_manual() %>% 
      bb_legend(position = "right") %>%
      bb_zoom(
        enabled = list(type = "drag"),
        resetButton = list(text = "Unzoom")
      ) %>% 
      bb_labs(title = "Monthly supply / demand balance in France (2007 - 2017)",
              y = "Megawatt (MW)")
  })
  
  
  # Show output based on user selection 
  observe({
    output$visual <- renderUI({
      div(
        style = "height: 530px; overflow-x: auto;",
        div(
          style = "min-width: 500px;",
          if (input$plottype == "scatterplot") {
            billboarderOutput(session$ns("scatterplot_billboarder"), width = "auto")
          }
          else if (input$plottype == "barchart") {
            billboarderOutput(session$ns("barchart_billboarder"), width = "auto")
          }
          
          else if (input$plottype == "linechart") {
            billboarderOutput(session$ns("linechart_billboarder"), width = "auto")
          }
        ))
    })
  })
}