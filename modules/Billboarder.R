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
                                                   "insulin", "mass", "pedigree", "age"),
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

BillboarderFunction <- function(input, output, session) {
  
  # Scatter plot codes
  output$scatterplot_billboarder <- renderBillboarder({
    
    billboarder() %>% 
      bb_scatterplot(data = PimaIndiansDiabetes,
                     x = input$feature_x,
                     y = input$feature_y,
                     group = "diabetes") %>% 
      bb_colors_manual("pos" = Gold, "neg" = Bright.Blue) %>% 
      bb_x_axis(label = list(text = as.character(input$feature_x)),
                tick = list(fit = FALSE)) %>% 
      bb_y_axis(label = list(text = as.character(input$feature_y))) %>%
      bb_theme() %>%
      bb_zoom(enabled = list(type = "drag"), resetButton = list(text = "Unzoom")) 
  })
  
  # Bar chart codes
  output$barchart_billboarder <- renderBillboarder({
    if(input$plottype == "barchart" & input$stackorparallel == "Stacked") {
      billboarder() %>% 
        bb_histogram(data = PimaIndiansDiabetes, 
                     x = input$feature_hist,
                     group = "diabetes",
                     stacked = T) %>% 
        bb_colors_manual("pos" = Gold, "neg" = Bright.Blue) %>% 
        bb_x_axis(label = list(text = as.character(input$feature_hist)),
                  tick = list(fit = FALSE)) %>% 
        bb_y_axis(label = list(text = "count")) %>%
        bb_theme() %>%
        bb_zoom(enabled = list(type = "drag"), resetButton = list(text = "Unzoom")) 
    }
    else if (input$plottype == "barchart" & input$stackorparallel == "Dodge") {
      billboarder() %>% 
        bb_histogram(data = PimaIndiansDiabetes, 
                     x = input$feature_hist, 
                     group = "diabetes",
                     stacked = F) %>% 
        bb_colors_manual("pos" = Gold, "neg" = Bright.Blue) %>% 
        bb_x_axis(label = list(text = as.character(input$feature_hist)),
                  tick = list(fit = FALSE)) %>% 
        bb_y_axis(label = list(text = "count")) %>%
        bb_theme() %>%
        bb_zoom(enabled = list(type = "drag"), resetButton = list(text = "Unzoom")) 
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
            billboarderOutput(session$ns("scatterplot_billboarder"), width = "auto")
          }
          else if (input$plottype == "barchart") {
            billboarderOutput(session$ns("barchart_billboarder"), width = "auto")
          }
        ))
    })
  })
}