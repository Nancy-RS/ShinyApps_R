library("shiny")
library("shinydashboard")
library("smooth")
library("googleVis")
library("plotly")
library("ggplot2")
library("data.table")


#Estructura de un shiny dashboard 
ui <- dashboardPage(skin="black", 
  dashboardHeader(title="Tablero"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Tablero", tabName="dashboard1", icon=icon("bar-chart-o")),
      menuItem("Controles", tabName="controles", icon=icon("cog"),
               selectInput("sucursal", 
                           label = HTML("Seleccione una cadena:"), 
                           choices= c("Seleccione una cadena:", "COSTCO", "FARMACIAS GUADALAJARA", "SUPERAMA")),
               selectInput("item", "Seleccione un producto:", 
                           choices=c("Seleccione un producto")) ,
               actionButton("go", "Actualizar", icon("refresh")))
    )
    
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard1",
        fluidRow(
          column(12,box(
              # Aquí vamos a poner un output, que es lo que visualizaremos
              htmlOutput(outputId = "ejemplo1"), solidHeader = FALSE, height="auto", width = "100%")
          )
        ),
        fluidRow(
          column(12,box(
            # Aquí vamos a poner un output, que es lo que visualizaremos
            plotlyOutput(outputId = "ejemplo2"), solidHeader = FALSE, height="auto", width = "100%")
            
          )
        )
        
      )
    
    )
  )
)

server <- function(input, output) {
  
 
  
  
}

shinyApp(ui = ui, server = server)