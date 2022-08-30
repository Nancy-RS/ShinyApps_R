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
      selectInput(inputId ="empresas", label= h3("Seleccione una empresa:"), 
                  choices = list("Elektra" = 1, "Cemex" = 2,
                                 "Alsea" = 3), selected = 1),
      checkboxInput("tendencia", "Ver tendencia", value = FALSE)
      
    ),
    
    
    # La parte derecha o central que sirve para los resultads
    mainPanel(
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
    
    if(input$empresas == 1){
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/ELEKTRA.MX?period1=1578441600&period2=1610064000&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    } else if(input$empresas == 2){
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/CX?period1=1578441600&period2=1609977600&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    } else {
      base<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/ALSEA.MX?period1=1578441600&period2=1609977600&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
    }
    base$periodo<-1:nrow(base)
    
    
    if(input$tendencia == FALSE){
      plot(base$Close, type="l")
    } else {
      tend<-lm(Close~periodo, data=base)
      plot(Close~periodo, data=base, type="l")
      abline(tend, color="green")
    }
    
 })
  
}


shinyApp(ui = ui, server = server)