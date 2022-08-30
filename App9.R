#Utilizar la base Cartera1 para crear una shiny app que tenga lo siguiente: 
#-Un filtro para elegir el tipo de credito
#-Un botón para actualizar
#Una vez elegido el crédito, se debe de tener 3 tabs en el main panel, uno que se llame
#mensual, uno anual y uno graficas
#En el tab mensual se debe de mostrar la siguiente informacion
#En el tab anual la siguiente 
#En el tab graficas se deben de generar las siguientes gráficas.


















library("shiny")
library("smooth")
library("rAmCharts")
library("bsts")
library("dplyr")


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
      selectInput("credito", h3("Seleccione un tipo de crédito:"), 
                  choices = list("Nomina", "Auto", "Consumo", "Medico"), selected = 1),
      actionButton("actualizar", "Actualizar", icon("refresh"))
      
      
    ),
    
    # La parte derecha o central que sirve para los resultads
    mainPanel(
      
      tabsetPanel(type="tabs",
      # Aquí vamos a poner un output, que es lo que visualizaremos
      tabPanel("Mensual",dataTableOutput("historicomensual")),
      tabPanel("Anual", dataTableOutput("historicoanual")),
      tabPanel("Graficas", verbatimTextOutput("tdos"), amChartsOutput("dos"), verbatimTextOutput("tuno"), amChartsOutput("uno"))
      )
    )
  )
)




server <- function(input, output) {
  
  
  datos<-eventReactive(input$actualizar,{
    base<-read.csv("Cartera1.csv", header=TRUE)
    base<-subset(base, Credito == input$credito)
    base$Fecha<-paste(base$Ano, base$Mes, "01", sep="-")
    base$Fecha<-LastDayInMonth(base$Fecha)
    return(base)
   })
  
  
  
  output$historicomensual <- renderDataTable({
    mensual<-datos()
    mensual<-mensual[,c(9,1,4,5,8)]
    return(mensual)
  })
  
  output$historicoanual<- renderDataTable({
    mensual<-datos()
    anual<- as.data.frame(mensual %>%
      group_by(Ano) %>%
      summarise(Creditos=sum(Creditos), Cartera.Total=sum(Cartera.Total),
                 Cartera.Vencida=sum(Cartera.Vencida)))
    return(anual)
  })
  
   output$uno<-renderAmCharts({
    mensual<-datos()
    mensual<-mensual[,c(9,1,4,5,8)]
    mensual$Fecha<-as.character(mensual$Fecha)
    grafica<-amBarplot(x = "Fecha", y = c("Creditos", "Cartera.Vencida"), data = mensual)
    plot(grafica)
  })
   
   output$dos<-renderAmCharts({
     mensual<-datos()
     anual<- as.data.frame(mensual %>%
                             group_by(Ano) %>%
                             summarise(Cartera.Total=sum(Cartera.Total),
                                       Cartera.Vencida=sum(Cartera.Vencida)))
     anual$Ano<-as.character(anual$Ano)
     grafica<-amBarplot(x = "Ano", y = c("Cartera.Total", "Cartera.Vencida"), data = anual)
     plot(grafica)
   })
   
  
  
  output$tuno<-renderPrint({
    x<-"Información mensual"
    return(x)
  })
  
  output$tdos<-renderPrint({
    x<-"Información anual"
    return(x)
  })
  
 
}


shinyApp(ui = ui, server = server)