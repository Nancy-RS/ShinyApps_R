#googleVisejemplos 
library("googleVis")
#Grafico de linea 
ejemplobarras<-data.frame(salon=c("A","B","C"), aprobados = c(28,31, 19),reprobados=c(3,5,2))
linea<-gvisLineChart(ejemplobarras, "salon", c("aprobados","reprobados"),
                     options=list(
                       series="[{targetAxisIndex: 0},
                       {targetAxisIndex:1}]",
                       vAxes="[{title:'Aprobados'}, {title:'Reprobados'}]"
                     ))
plot(linea)

linea<-gvisLineChart(ejemplobarras, "salon", c("aprobados","reprobados"),
                     options=list(
                       vAxes="[{title:'Aprobados'}, {title:'Reprobados'}]"
                     ))
plot(linea)

#Grafico de barras
barra<-gvisBarChart(ejemplobarras)
plot(barra)

barra2<-gvisColumnChart(ejemplobarras)
plot(barra2)

#Grafico de areas
Area <- gvisAreaChart(ejemplobarras, "salon")
plot(Area)

#Combinada 
ambas <- gvisComboChart(ejemplobarras, xvar="salon",
                        yvar=c("aprobados", "reprobados"),
                        options=list(seriesType="bars",
                                     series='{1: {type:"line"}}'))
plot(ambas)

#Dispersión 
women
dispersion <- gvisScatterChart(women, 
                            options=list(
                              legend="none",
                              lineWidth=2, pointSize=0,
                              title="Mujeres", vAxis="{title:'peso (lbs)'}",
                              hAxis="{title:'altura (in)'}", 
                              width=600, height=600))
plot(dispersion)


dispersion <- gvisScatterChart(women, 
                               options=list(
                                 legend="none",
                                 lineWidth=0, pointSize=4,
                                 title="Women", vAxis="{title:'weight (lbs)'}",
                                 hAxis="{title:'height (in)'}", 
                                 width=400, height=400))
plot(dispersion)


#Gráfico de burbujas 
empresas<-data.frame(empresa=c("Empresa1","Empresa2", "Empresa3", "Empresa4", "Empresa5"),
                     ventas=c(23000, 25000, 19000, 18500, 13000),
                     gastos=c(3400, 1850, 6500, 3500, 860)
                     )
empresas$utilidad<-((empresas$ventas-empresas$gastos)/empresas$ventas)*100

burbuja <- gvisBubbleChart(empresas, idvar="empresa", 
                          xvar="ventas", yvar="gastos",
                          colorvar="empresa", sizevar="utilidad",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))
plot(burbuja)


#Grafico de velas 
elektra<-read.csv("https://query1.finance.yahoo.com/v7/finance/download/ELEKTRA.MX?period1=1580074345&period2=1611696745&interval=1d&events=history&includeAdjustedClose=true", header=TRUE)
elektra<-tail(elektra[,c(1,4,2,5,3)],n=15)
velas <- gvisCandlestickChart(elektra, 
                               options=list(legend='none'))
plot(velas)



#Grfico de pie 
filtro<-empresas[,c(1,2)]
pie<- gvisPieChart(filtro)
plot(pie)


#Mapa 
Geo=gvisGeoChart(Exports, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)


#Arboles 
Tree <- gvisTreeMap(Regions,  
                    "Region", "Parent", 
                    "Val", "Fac", 
                    options=list(fontSize=16))
plot(Tree)

#Mas ejemplos
#https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html



##GGplot 
library("ggplot2")
library("plotly")
library("data.table")

#ejemplo con plotly
head(diamonds)
p<-ggplot(diamonds, aes(carat, price))+geom_point()
ggplotly(p) 



#otro ejemplo
ventas<-read.csv("https://www.dropbox.com/s/ys464p5h52mer0h/ejemplo.csv?dl=1",
                 header=TRUE)

ventas<-as.data.frame(select(ventas, Fecha, Carnation, Clasico, Stick, Doypack))
nventas<-melt(ventas, 
              id=c("Fecha"), 
              measure.vars=c("Carnation","Clasico", "Stick", "Doypack"))
colnames(nventas)<-c("Fecha", "Producto", "Venta")
nventas$Fecha<-as.Date(nventas$Fecha, format="%d/%m/%y" )
p2<-ggplot(nventas, aes(x=Fecha, y=Venta, group=Producto))+geom_line()
p2
ggplotly(p2)


#con boxplot 
p3<-ggplot(nventas, aes(x=Producto, y=Venta, colour=Producto))+geom_boxplot()+geom_jitter(alpha=0.2)
ggplotly(p3)


#con facets
ef<-ggplot(data=nventas, aes(x=Fecha, y=Venta))+geom_line()
ef<-ef+facet_wrap(~Producto)
ef
ggplotly(ef)

#Mas ejemplos 
#https://plotly-r.com/improving-ggplotly.html