library("shiny")


#Siempre se usara un fluidPage 
ui <- fluidPage(
  
  # Con esta función agregamos un título
  titlePanel("Mi primer aplicación Shinny App de mi curso de R"),
  
  # Con esta función creamos un layout en fomar de sidebar, que a su vez estará
  #divido en dos partes. 
  sidebarLayout(
    
    # La parte izquierda que sirve para los controles 
    sidebarPanel(
      
      # Dentro del sidebar izquierdo, ingresamos los inputos. 
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "numeros",
                  label = "Selecciona la cantidad de numeros a generar:",
                  min = 50,
                  max = 1000,
                  value = 500)
      
    ),
    
    # La parte derecha o central que sirve para los resultads
    mainPanel(
      
      # Aquí vamos a poner un output, que es lo que visualizaremos
      strong("Aquí se genera la grafica del primer input:"),
      plotOutput(outputId = "grafica1"),
      em("Aquí se genera la grafica del segunda input:"),
      plotOutput(outputId = "grafica2")
      
    )
  )
)




server <- function(input, output) {
  
  # Aquí es donde programamos el histograma que queremos generar. 
  #
  # 1. Es "reactive"  y por lo tanto debe de re-ejecutarce 
  #    en automático cuando los inputs cambien 
  # 2. Su output es de tipo salida
  output$grafica1 <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  output$grafica2<-renderPlot({
    
    numero<- input$numeros
    aleatorios<-rnorm(numero)
    
    hist(aleatorios)
    
  })
  
}


shinyApp(ui = ui, server = server)