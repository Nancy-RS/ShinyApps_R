library("shiny")
library("shinydashboard")
library("smooth")
library("googleVis")


#Estructura de un shiny dashboard 
ui <- dashboardPage(skin="black", 
  dashboardHeader(title="Cotizaciones BMV"),
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Tablero", tabName="dashboard1", icon=icon("bar-chart-o"))
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
  

  output$ejemplo1<-renderGvis({
    empresas<-data.frame(empresa=c("Empresa1","Empresa2", "Empresa3", "Empresa4", "Empresa5"),
                         ventas=c(23000, 25000, 19000, 18500, 13000),
                         gastos=c(3400, 1850, 6500, 3500, 860)
    )
    empresas$utilidad<-((empresas$ventas-empresas$gastos)/empresas$ventas)*100
    
    burbuja <- gvisBubbleChart(empresas, idvar="empresa", 
                               xvar="ventas", yvar="gastos",
                               colorvar="utilidad", sizevar="utilidad",
                               options=list(
                                 hAxis='{minValue:75, maxValue:125}'))
    return(burbuja)
  })
  
  
 output$ejemplo2<-renderPlotly({
   
   ventas<-read.csv("https://www.dropbox.com/s/ys464p5h52mer0h/ejemplo.csv?dl=1",
                    header=TRUE)
   
   ventas<-as.data.frame(select(ventas, Fecha, Carnation, Clasico, Stick, Doypack))
   nventas<-melt(ventas, 
                 id=c("Fecha"), 
                 measure.vars=c("Carnation","Clasico", "Stick", "Doypack"))
   colnames(nventas)<-c("Fecha", "Producto", "Venta")
   nventas$Fecha<-as.Date(nventas$Fecha, format="%d/%m/%y" )
   ef<-ggplot(data=nventas, aes(x=Fecha, y=Venta))+geom_line()
   ef<-ef+facet_wrap(~Producto)
   ef
   
   ggplotly(ef)
 })
  
 
  
  
  
}

shinyApp(ui = ui, server = server)