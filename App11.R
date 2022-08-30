library("shiny")
library("shinydashboard")
library("smooth")
library("rAmCharts")

#Estructura de un shiny dashboard 
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Continua", tabName="dashboard1", icon=icon("chart-bar")),
      menuItem("Discreta", tabName="dashboard2",icon=icon("th"))
    )
    
  ),
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "dashboard1",
        fluidRow(
          box(title="Controles",sliderInput("numeros", "Numero de observaciones:", 30, 100, 100)),
          box(title="Grafica", plotOutput("uno"), solidHeader = FALSE)
        )
      ),
      
      tabItem(tabName="dashboard2",
              fluidRow(
                box(sliderInput("numeros2", "Numero de observaciones:", 30, 80, 50)),
                box(plotOutput("dos"),collapsible = TRUE, collapsed = FALSE)
              ))
    
    )
  )
)

server <- function(input, output) {
  
  output$uno<-renderPlot({
    valores<-rnorm(input$numeros)
    hist(valores)
  })
  
  output$dos<-renderPlot({
    valores<-rbinom(input$numeros2, 100, p=0.5)
    hist(valores)
  })
  
}

shinyApp(ui = ui, server = server)