source("global.R")
source("modules/Plotly.R")
source("modules/Billboarder.R")

ui <- dashboardPage(
  dashboardHeader(title = "Interactive graphics"),
  dashboardSidebar(width = 230,
                   sidebarMenu(id = 'sidebarmenu',
                               tags$br(),
                               menuItem('Plotly', tabName = "plotly"),
                               menuItem('Billboarder', tabName = "billboarder"))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plotly", PlotlyUI(id = "plotlygraphs")),
      tabItem(tabName = "billboarder", BillboarderUI(id = "billboardergraphs"))
    ))
)

server <- function(input, output, session) {
  callModule(PlotlyFunction, id = "plotlygraphs")
  callModule(BillboarderFunction, id = "billboardergraphs")
}

shinyApp(ui, server)