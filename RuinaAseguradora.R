#Proceso Poisson no homogeneo

# Quitamos notacion cientifica
options(scipen = 999)

#Funcion Lambda
flambda <- function(x) {log(1+x)/20}
PPlambda <- function(t){
  lambdat <- integrate(flambda,
            lower = 0, 
            upper = t)
  return(lambdat$value)
}

# Generamos el proceso Poisson
T_n <- function(t){
  T_i <- NULL
  i <- 1
  while(sum(T_i) < t){
    samp <- rexp(n = 1, rate = PPlambda(t))
    T_i[i] <- samp
    i <- i+1
  }
  return(head(T_i, -1))
}

tau_n <- function(t){
  T_i <- T_n(t)
  tau_i <- NULL
  cumSum <- 0
  for(i in 1:length(T_i)){
    tau_i[i] <- sum(T_i[1:i])
  }
  return(tau_i)
}

ppGen <- function(t){
  tau_i <- tau_n(t)
  pp <- NULL
  pp1 <- NULL
  for(i in 1:t){
    pp1[i] <- sum(tau_i <= i)
    if(pp1[i]<=numaseg){
      pp[i] <- pp1[i]
    } else {
      pp[i] <- pp[i-1]
    }
  }
  return(c(0,pp))
}


#Generamos primas aleatorias para los asegurados
pasegurados <- NULL
sumaprimas <-0
for(i in 1:numaseg) {
  pasegurados[i] <- sample(1500:3000)[i]
  sumaprimas <- sumaprimas+pasegurados[i] 
}

fondos <- function(II,t){
  resultado <- NULL
  for(i in 1:t){
    resultado[i] <- II+sumaprimas*(i-1)
  }
  return(resultado)
}



# ¿Cual es la probabilidad de que la compañia de seguros quede en ruina durante
# los años de prueba (primoros 20 años)


# Capital Inicial 100,000,000
CI <- 100000000

#Tiempo de Prueba: 20 años
tiempoPrueba <- 20

#Numero de Asegurados
numaseg <- 21

#Todos los asegurados reciben una SA de 5,000,000 pesos
SA <- 5000000

Fondos_Aseguradora <- fondos(CI,tiempoPrueba+1)

cfavorables <-  0 
for(i in 1:1000){
  
  # Generamos el numero de reclamaciones con un PP homogeneo
  Reclamaciones <-ppGen(tiempoPrueba)
  Monto_Reclamaciones <- SA*Reclamaciones
  
  Total <- NULL
  for(t in 0:tiempoPrueba){
    Total[t+1] <- Fondos_Aseguradora[t+1] - Monto_Reclamaciones[t+1]
  }
  
  if(Total[tiempoPrueba]<0){
    cfavorables <- cfavorables+1
  }
}
respuesta <- cfavorables/1000
respuesta



#Graficamos el comportamiento de una simulación
x2 <- 0:(length(Total)-1)

plot(x2,
     Total,
     type="s",
     xlab = "Tiempo (años)",
     ylab = "Dinero",
     col = "red")
title("Capital en la empresa")


#Mostramos el comportamiento del capital de la aseguradora en una tabla
tabla <- data.frame (cbind(Fondos_Aseguradora,
                           Reclamaciones,
                           Monto_Reclamaciones,
                           Total))
tabla

