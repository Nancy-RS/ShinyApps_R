library("shiny")
library("shinydashboard")
library("smooth")
library("rAmCharts")

#Estructura de un shiny dashboard 
ui <- dashboardPage(skin="purple",
  dashboardHeader(title="Análisis de ventas"),
  dashboardSidebar(),
  dashboardBody(
  
      fluidRow(
      box(sliderInput("numeros", "Numero de observaciones:", 30, 100, 100)),
      box(plotOutput("uno"))
    )
    
    
  )
)

server <- function(input, output) {
  
  output$uno<-renderPlot({
    valores<-rnorm(input$numeros)
    hist(valores)
  })
  
}

shinyApp(ui = ui, server = server)