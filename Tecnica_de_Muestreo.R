#ADA 12
#Técnicas de muestreo 

#Bases de entrada
#ejemplo 1
x<-rnorm(10, mean=14500, sd=1356)
#ejemplo 2
y<-round(runif(15,0,1),0)

calculamuestra<-function(x, N, e, a=.05, tipo="media"){
  z<-qnorm(1-a/2)
  
  if(tipo == "media"){
    if( sum(x)<=length(x)){
      n<-"Ingresaste datos de muestreo de proporciones"
    } else {
      n<- ceiling(((z^2)*N*(var(x)))/(N*(e^2)+(z^2)*var(x)))
    }
  } else if (tipo == "total"){
    if( sum(x)<=length(x)){
      n<-"Ingresaste datos de muestreo de total de clase"
    } else {
      n<- ceiling(((z^2)*(N^2)*(var(x)))/((e^2)+(z^2)*var(x)*N))
    }
  } else if (tipo == "proporcion"){
    if( sum(x)>length(x)){
      n<-"Ingresaste datos de muestreo para la media"
    } else {
      p<-mean(x)
      n<- ceiling(((z^2)*N*p*(1-p))/((N-1)*(e^2)+(z^2)*p*(1-p)))
    }
  } else if (tipo == "totalc"){
    if( sum(x)>length(x)){
      n<-"Ingresaste datos de muestreo para la total"
    } else{
      p<-mean(x)
      n<- ceiling(((z^2)*(N^3)*p*(1-p))/((N-1)*(e^2)+(z^2)*p*(1-p)*(N^2)))
    }
  }
  
  
  return(n)
}


#Pruebas
calculamuestra(x,1500,100, 0.05, tipo="media") 
calculamuestra(x,1500,1500000, 0.05, tipo="total") 
calculamuestra(x,1500,100, 0.05, tipo="proporcion") 
calculamuestra(x,1500,100, 0.05, tipo="totalc") 

calculamuestra(y,1500, 0.05, 0.05, tipo="proporcion")
calculamuestra(y,1500, 100, 0.05, tipo="totalc")
calculamuestra(y,1500, 0.5, 0.05, tipo="media") 
calculamuestra(y,1500, 0.5, 0.05, tipo="total") 



