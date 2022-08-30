library("shiny")


#Siempre se usara un fluidPage 
ui <- fluidPage(
  
  # Con esta función agregamos un título
  titlePanel("Hola Mundo!"),
  
  # Con esta función creamos un layout en fomar de sidebar, que a su vez estará
  #divido en dos partes. 
  sidebarLayout(
    
    # La parte izquierda que sirve para los controles 
    sidebarPanel(
      
      # Dentro del sidebar izquierdo, ingresamos los inputos. 
      sliderInput(inputId = "cantidad",
                  label = "Cantidad de números a generar:",
                  min = 1,
                  max = 100,
                  value = 50),
      sliderInput(inputId = "media",
                  label = "Ingrese la media",
                  min = 1,
                  max = 100,
                  value = 50),
      sliderInput(inputId = "sd",
                  label = "Ingrese la desviacion estándar",
                  min = 1,
                  max = 100,
                  value = 50)
      
    ),
    
    # La parte derecha o central que sirve para los resultads
    mainPanel(
      
      # Aquí vamos a poner un output, que es lo que visualizaremos
      plotOutput(outputId = "distPlot"),
      img(src = "uady2.png", height = 140, width = 400)
      
    )
  )
)




server <- function(input, output) {
  
  # Aquí es donde programamos el histograma que queremos generar. 
  #
  # 1. Es "reactive"  y por lo tanto debe de re-ejecutarce 
  #    en automático cuando los inputs cambien 
  # 2. Su output es de tipo salida
  output$distPlot <- renderPlot({
    
    numero<-input$cantidad
    media<-input$media
    sd<-input$sd
    
    aleatorios<-rnorm(numero, media, sd)
    
    hist(aleatorios, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogramas de números aleatorios de la normal")
    
  })
  
}


shinyApp(ui = ui, server = server)