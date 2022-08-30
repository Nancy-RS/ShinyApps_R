#ADA 11
#Calificacion 1.9 pts. 

#Pruebas de hipótesis para la media de muestras grandes
#1. Crea una función que se llame pruebahipm que
#calcule pruebas de hipótesis para muestras 
#grandes, los argumentos de la función deben de ser: 
#x: el conjunto de observaciones 
#h0: el valor propuesto de la hipótesis nula 
#h1: cadena de texto que tome los valores sup, inf, dos, para indicar 
#que se trata de una prueba de la cola superior, inferior o de dos colas,
#respectivamente (por defecto "dos")
#alpha: el nivel de significacia de la prueba (por defecto 0.05)
#el resultado, debe de ser una lista que te de
#en su primer componente el estadístico de prueba, la segunda la región
#de rechazo y la tercer si se rechaza o no H0. 
#Nota: Usar 4 decimales en el estadístico de prueba y  region de rechazo 
pruebahipm1<-function(x, h0, h1="dos", significancia=0.05){
  muestra<-x
  rr<-numeric()
  final<-list()
  estadisticoprueba<-round((mean(x)-h0)/(sd(x)/sqrt(length(x))),4)
  
  if(h1=="inf"){
    respuesta<- estadisticoprueba < -qnorm(1-significancia)
    rr<- -qnorm(1-significancia)
  }else if(h1== "sup"){
    respuesta<-estadisticoprueba > qnorm(1-significancia)
    rr<- qnorm(1-significancia)
  } else{
    respuesta<-abs(estadisticoprueba) > qnorm(1-significancia/2)
    rr<- qnorm(1-significancia/2)
  }
  
  if(respuesta==TRUE){
    resultado<-"Rechaza H0"
  }else{
    resultado<-"No rechaza H0"
  }
  
  
  if(h1=="inf"){
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de z menores a ",round(rr,4), sep=" "),
      decision=resultado)
  }else if(h1== "sup"){
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de z mayores a ",round(rr,4), sep=" "),
      decision=resultado)
  } else{
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de |z| mayores a ",round(rr,4), sep=" "),
      decision=resultado)
  }
  return(final)
  
}

#0.5 pts.

#Pruebas de hipótesis para la proporcion de muestras grandes
#2. Crea una función que se llame pruebahipp que
#calcule pruebas de hipótesis para muestras 
#grandes, los argumentos de la función deben de ser: 
#x: el conjunto de observaciones 
#h0: el valor propuesto de la hipótesis nula 
#h1: cadena de texto que tome los valores sup, inf, dos, para indicar 
#que se trata de una prueba de la cola superior, inferior o de dos colas,
#respectivamente (por defecto "dos")
#alpha: el nivel de significacia de la prueba (por defecto 0.05)
#el resultado, debe de ser una lista que te de
#en su primer componente el estadístico de prueba, la segunda la región
#de rechazo y la tercer si se rechaza o no H0. 
#Nota: Usar 4 decimales en el estadístico de prueba y  region de rechazo 
pruebahipp1<-function(x, h0, h1="dos", significancia=0.05){
  muestra<-x
  rr<-numeric()
  final<-list()
  proporcion<-sum(x)/length(x)
  estadisticoprueba<-(proporcion-h0)/sqrt(((proporcion*(1-proporcion))/length(x)))
  
  if(h1=="inf"){
    respuesta<- estadisticoprueba < -qnorm(1-significancia)
    rr<- -qnorm(1-significancia)
  }else if(h1== "sup"){
    respuesta<-estadisticoprueba > qnorm(1-significancia)
    rr<- qnorm(1-significancia)
  } else{
    respuesta<-abs(estadisticoprueba) > qnorm(1-significancia/2)
    rr<- qnorm(1-significancia/2)
  }
  
  if(respuesta==TRUE){
    resultado<-"Rechaza H0"
  }else{
    resultado<-"No rechaza H0"
  }
  
  
  if(h1=="inf"){
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de z menores a ",round(rr,4), sep=" "),
      decision=resultado)
  }else if(h1== "sup"){
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de z mayores a ",round(rr,4), sep=" "),
      decision=resultado)
  } else{
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de |z| mayores a ",round(rr,4), sep=" "),
      decision=resultado)
  }
  return(final)
  
}
#0.5 pts.


##Pruebas de hipótesis para la proporcion de muestras grandes
#3. Crea una función que se llame pruebahip que
#calcule pruebas de hipótesis para muestras 
#grandes, los argumentos de la función deben de ser: 
#x: el conjunto de observaciones de la poblacion 1
#y: el conjunto de observaciones de la poblacion 2
#h0: el valor propuesto de la hipótesis nula (D0) 
#h1: cadena de texto que tome los valores sup, inf, dos, para indicar 
#que se trata de una prueba de la cola superior, inferior o de dos colas,
#respectivamente (por defecto "dos")
#alpha: el nivel de significacia de la prueba (por defecto 0.05)
#el resultado, debe de ser una lista que te de
#en su primer componente el estadístico de prueba, la segunda la región
#de rechazo y la tercer si se rechaza o no H0. 
#Nota: Usar 4 decimales en el estadístico de prueba y  region de rechazo 
#0.5 pts.
pruebadif1<-function(x, y, h0, h1="dos", significancia=0.05){
  muestra<-x
  rr<-numeric()
  final<-list()
  gl<-length(x)+length(y)-2
  sp<- sqrt(((length(x)-1)*var(x)+(length(y)-1)*var(y))/(gl))
  estadisticoprueba<-(mean(x)-mean(y)-h0)/(sp*sqrt(1/length(x)+1/length(y)))
  
  
  if(h1=="inf"){
    respuesta<- estadisticoprueba < -qt(1-significancia,gl )
    rr<- -qnorm(1-significancia)
  }else if(h1== "sup"){
    respuesta<-estadisticoprueba > qt(1-significancia,gl)
    rr<- qnorm(1-significancia)
  } else{
    respuesta<-abs(estadisticoprueba) > qt(1-significancia/2,gl)
    rr<- qnorm(1-significancia/2)
  }
  
  if(respuesta==TRUE){
    resultado<-"Rechaza H0"
  }else{
    resultado<-"No rechaza H0"
  }
  
  
  if(h1=="inf"){
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de t menores a ",round(rr,4), sep=" "),
      decision=resultado)
  }else if(h1== "sup"){
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de t mayores a ",round(rr,4), sep=" "),
      decision=resultado)
  } else{
    final<-list(
      estadistico.prueba=estadisticoprueba,
      region.rechazo=paste("Valores de |t| mayores a ",round(rr,4), sep=" "),
      decision=resultado)
  }
  return(final)
  
} 
#0.5 pts.

#4. Escribir una función, que reciba 2 o 3 argumentos, que sean los siguientes: 
#conf= 0.95: indica el nivel de confianza 
#x: un vector de datos x
#y: un vector de datos y 
#La función debe de hacer lo siguiente: si solo se le ingresa conf  y x 
#entonces calcula el estimador de la media y calcula el intervalo de confianza 
#de la media, si la funcion recibe conf, x e y, entonces calcula el estimador 
#de la diferencia de medias y el intervalo de confianza de la diferencia de 
#medias, en ambos casos, con el nivel de confianza ingresado en conf (por defecto 95%)
#en caso de que se ingrese solo un vector, la función debe de regresar una lista con 2 componentes, la componente uno 
#se debe de llamar estimador y debe de tener la frase: "El estimador de la media es: X"
#la componente 2 se debe de llamar intervalo y debe de tener la frase:
#"El intervalo de X% de confianza es: (X1,X2)", en caso de que se ingresen 2 vectores
#la componente uno regresa "El estimador de la diferencia de medias es: X" y la componente 
#2 regresa "El intervalo de X% de confianza es: (X1,X2)".

intervalomedia1<-function(conf=0.95,x, y=NULL){
  x<-x
  y<-y
  if(length(y) > 1){
    estimador<-round(mean(x)-mean(y),2)
    gl<-length(x)+length(y)-2
    sp= sqrt(((length(x)-1)*var(x)+(length(y)-1)*var(y))/gl)
    inf<-round(mean(x)-mean(y)-qt(1-(1-conf)/2, gl)*sp*sqrt(1/length(x)+1/length(y)),2)
    sup<-round(mean(x)-mean(y)+qt(1-(1-conf)/2, gl)*sp*sqrt(1/length(x)+1/length(y)),2)
    comp1<-paste("El estimador de la diferencia de medias es:", estimador, sep=" ")
    comp2<-paste("El intervalo de ", conf*100, "% de confianza es: (",inf,",",sup,")" )
    lista<-list(estimador=comp1,
                intervalo=comp2)
    return(lista)
    
  }else{
    
    estimador<-round(mean(x),2)
    inf<-round(mean(x)-qt(1-(1-conf)/2, length(x)-1)*(sd(x)/sqrt(length(x))),2)
    sup<-round(mean(x)+qt(1-(1-conf)/2, length(x)-1)*(sd(x)/sqrt(length(x))),2)
    comp1<-paste("El estimador de la media es:", estimador, sep=" ")
    comp2<-paste("El intervalo de ", conf*100, "% de confianza es: (",inf,",",sup,")" )
    lista<-list(estimador=comp1,
                intervalo=comp2)
    return(lista)
  }
  
  
}

#Los intervalos salen al revez,primero el superior y luego el inferior 
#0.4 pts