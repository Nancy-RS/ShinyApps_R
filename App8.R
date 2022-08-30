library("shiny")
library("smooth")
library("rAmCharts")

#Siempre se usara un fluidPage 
ui <- fluidPage(
  
  # Con esta función agregamos un título
  titlePanel("Cotización de empresas!"),
  
  # Con esta función creamos un layout en fomar de sidebar, que a su vez estará
  #divido en dos partes. 
  sidebarLayout(
    
    # La parte izquierda que sirve para los controles 
    sidebarPanel(
      
      # Dentro del sidebar izquierdo, ingresamos los inputos. 
      selectInput("empresas", h3("Seleccione una empresa:"), 
                  choices = list("Elektra" = 1, "Cemex" = 2,
                                 "Alsea" = 3), selected = 1),
      actionButton("actualizar", "Actualizar", icon("refresh"))
      
      
    ),
    
    # La parte derecha o central que sirve para los resultads
    mainPanel(
      
      tabsetPanel(type="tabs",
      # Aquí vamos a poner un output, que es lo que visualizaremos
      tabPanel("Tabla",dataTableOutput("historicotabla")),
      tabPanel("Grafica", amChartsOutput("historico")),
      tabPanel("Regresion", verbatimTextOutput("regresion"))
      )
    )
  )
)




server <- function(input, output) {
  
  
  datos<-eventReactive(input$actualizar,{
    if(input$empresas==1){
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/ELEKTRA.MX?period1=1578441600&period2=1610064000&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    }else if(input$empresas==2){
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/ALSEA.MX?period1=1578441600&period2=1609977600&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    } else {
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/CX?period1=1578441600&period2=1609977600&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    }
   })
  
  output$historico <- renderAmCharts({

    pg<-datos()
    pg$Date<-as.POSIXct(pg$Date, format="%Y-%m-%d")
    pg$Close<-as.numeric(pg$Close)
    pg$Open<-as.numeric(pg$Open)
    pg$High<-as.numeric(pg$High)
    linea<-amTimeSeries(pg, 
                        'Date', 
                        c('Close', 'Open', 'High'),
                        color=c("Black", "Red","Blue"), linetype=0)
    
    plot(linea)
  })
  
  
  output$historicotabla<-renderDataTable({
    base<-datos()
    base$Date<-as.POSIXct(base$Date, format="%Y-%m-%d")
    base$Close<-as.numeric(base$Close)
    base$Open<-as.numeric(base$Open)
    base$High<-as.numeric(base$High)
    return(base)
    
  })
  
  output$regresion<-renderPrint({
    base<-datos()
    base$Periodo<-1:nrow(base)
    modelo<-lm(Close~Periodo,data=base)
    summary(modelo)
  })
  
}


shinyApp(ui = ui, server = server)