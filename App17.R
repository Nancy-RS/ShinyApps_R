library("shiny")
library("shinydashboard")
library("smooth")
library("rAmCharts")
library("dplyr")
library("data.table")


#Estructura de un shiny dashboard 
ui <- dashboardPage(skin="black", 
  dashboardHeader(title="Tablero"),
  dashboardSidebar(
    sidebarMenuOutput("menu")
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard1",
        fluidRow(
          column(12,box(
              # Aquí vamos a poner un output, que es lo que visualizaremos
            amChartsOutput(outputId = "historico"), solidHeader = FALSE, height="auto", width = "100%")
          )
        ),
        fluidRow(
          column(12,box(
            # Aquí vamos a poner un output, que es lo que visualizaremos
            dataTableOutput(outputId = "tabla"), solidHeader = FALSE, height="auto", width = "100%")
            
          )
        )
        
      ),
      tabItem(tabName = "dashboard2",
              fluidRow(
                column(12,box(
                  # Aquí vamos a poner un output, que es lo que visualizaremos
                  amChartsOutput(outputId = "grafica"), solidHeader = FALSE, height="auto", width = "100%")
                )
              )
      )
    
    )
  )
)

server <- function(input, output, session) {
  
  #################################################
  ####Primero creo mi menú para mi sidebar
  ################################################
  output$menu <- renderMenu({
    
    sidebarMenu(
      menuItem("Análisis de Ventas", tabName="dashboard1", icon = icon("bar-chart-o")),
      menuItem("Controles", tabName="controles", icon = icon("bar-chart-o"),
              
      selectInput(inputId="cadena", 
                  label = HTML("Seleccione una cadena:"), 
                  choices= c("Seleccione una cadena:", "COSTCO", "FARMACIAS GUADALAJARA", "SUPERAMA")),
      selectInput(inputId= "producto", "Seleccione un producto:", 
                  choices=c("Seleccione un producto:"), selected="Seleccione un producto:") ,
      actionButton("go", "Actualizar", icon("refresh"))),
      
      menuItem("Analisis por cateogira", tabName="dashboard2", icon = icon("bar-chart-o")),
      menuItem("Controles", tabName="controles2", icon = icon("bar-chart-o"),
               
               selectInput(inputId="cadena2", 
                           label = HTML("Seleccione una cadena:"), 
                           choices= c("Seleccione una cadena:", "COSTCO", "FARMACIAS GUADALAJARA", "SUPERAMA")),
               actionButton("go2", "Actualizar", icon("refresh")))
      
    )
  })
  
  observe({
    p<-input$cadena
    z<-read.csv("Ejemplo.csv", header=T)
    recorte<-subset(z, CADENA==input$cadena)
    updateSelectInput(session, inputId = "producto",label = "Seleccione un producto:", choices= c("Seleccione un producto:",unique(as.character(recorte$DESCRIPCION))))
  })
  

  
  #################################################
  ###################Base de datos original para los boxes
  #################################################
  base<-eventReactive(input$go, {
    datos<-read.csv("Ejemplo.csv", header=TRUE)
    recorte<-subset(datos, CADENA==input$cadena)
    recorte2<-subset(recorte, DESCRIPCION==input$producto)
    return(recorte2)
  })
  
  #################################################
  ###################Grafica de seris de tiempo
  #################################################
  output$historico <- renderAmCharts({
    datos<-base()
    datos$Fecha<-as.POSIXct(datos$Fecha, format="%d/%m/%y")
    linea<-amTimeSeries(datos, 
                        'Fecha', 
                        c('Ventas'),
                        color=c("Black"), linetype=0)
    plot(linea)
  })
  
  
  output$tabla <- renderDataTable({
    datos<-base()
    return(datos)
  })
  
  
  #################################################
  ########Base de datos original para los boxes del segundo dashboard
  #################################################
  
  base2<-eventReactive(input$go2, {
    datos<-read.csv("Ejemplo.csv", header=TRUE)
    recorte<-subset(datos, CADENA==input$cadena2)
    return(recorte)
  })
  
  
  output$grafica <- renderAmCharts({
    
    datos<-base2()
    resumen<- as.data.frame(datos %>%
      group_by(CATEGORIA) %>%
      summarise(Venta=sum(Ventas)))
    barras<-amBarplot(x = "CATEGORIA", y = "Venta", data = resumen,
                      labelRotation = -45) 
    
    plot(barras)
  })
  
  
  
  

  
  
}

shinyApp(ui = ui, server = server)