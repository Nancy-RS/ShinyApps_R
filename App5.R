#Ejercicio 1.
#Construir una shiny app que lea el histórico de 3 acciones desde 
#yahoo finanzas, en el menu sidebar, debe de haber un select input que te 
#permita elegir entre cual de las 3 empresas quieres visualizar, debajo 
#debe de tener un single check box, que diga si se desea graficar la tendencia y 
#debajo debe de haber un radio buttons, que tenga las opciones de ver la tendencia 
#con una regresión lineal, un promedio movil de orden 3 o ambos; en el lado derecho 
#se debe de visualizar la gráfica y los elementos de acuerdo a lo seleccionado. 
#Ademas, debajo del seleccionador de la empresa, debe de haber otro select input 
#que permita elegir si queremos ver los resultados con el cierre o con 
#el precio de apertura. 












library("shiny")
library("smooth")

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
      selectInput("tipoprecio", h3("Seleccione un tipo de precio:"), 
                  choices = list("Cierre" = 1, "Apertura" = 2), selected = 1),
      
      checkboxInput("tendencia", "Ver tendencia", value = FALSE),
      radioButtons("radio", h3("Radio buttons"),
                   choices = list("Regresion lineal" = 1, "Promedio movil" = 2, "Ambos"=3),
                   selected = 1)
      
    ),
    
    
    # La parte derecha o central que sirve para los resultads
    mainPanel(
      h5("Resultado:"),
      # Aquí vamos a poner un output, que es lo que visualizaremos
      plotOutput(outputId = "historico")
      
    )
  )
)




server <- function(input, output) {
  
  # Aquí es donde programamos el histograma que queremos generar. 
  #
  # 1. Es "reactive"  y por lo tanto debe de re-ejecutarce 
  #    en automático cuando los inputs cambien 
  # 2. Su output es de tipo salida
  output$historico <- renderPlot({
    
  
    if(input$empresas==1){
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/ELEKTRA.MX?period1=1578441600&period2=1610064000&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    }else if(input$empresas==2){
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/ALSEA.MX?period1=1578441600&period2=1609977600&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    } else {
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/CX?period1=1578441600&period2=1609977600&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    }
    
    if(input$tipoprecio == 1){
      nuevo<-base[, c(1,5)]
    }else{
      nuevo<-base[, c(1,2)]
    }
    nuevo$periodo<-1:nrow(nuevo)
    colnames(nuevo)<-c("Fecha", "Precio", "Periodo")
    
    
  
    
    if(input$tendencia == FALSE){
    plot(nuevo$Precio, type="l", col="black")
    } else{
    
      if(input$radio == 1){
        modelo<-lm(Precio~Periodo,data=nuevo)
        plot(Precio~Periodo,data=nuevo, type="l", col="black")
        abline(modelo, col="green")
        
      }else if (input$radio == 2){
        modelo<-sma(nuevo$Precio, order=3)
        plot(Precio~Periodo,data=nuevo, type="l", col="black")
        lines(modelo$fitted, type="l", col="red")
        
      }else{
        modelo1<-lm(Precio~Periodo,data=nuevo)
        modelo2<-sma(nuevo$Precio, order=3)
        plot(Precio~Periodo,data=nuevo, type="l", col="black")
        abline(modelo1, col="green")
        lines(modelo2$fitted, type="l", col="red")
      }
      
    }
  })
  
}


shinyApp(ui = ui, server = server)