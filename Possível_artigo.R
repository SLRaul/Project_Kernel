library(tidyverse)
data("faithful")

### estimando faithful$waiting via kernel ###
tempo <- faithful$waiting
# escolhi o h pelo que o comando density() entrega
bandwidth <- density(tempo)

h <- bandwidth$bw
n <- as.numeric(length(tempo))
denshat <- function(x){
  m <-  length(x)
  a <- c()
  for(i in 1:m){
    a[i] <- sum(dnorm((x[i]-tempo)/h)/(n*h))
  }
  return(a)
}

hist(tempo, freq = FALSE, ylim = c(0,0.05))


# gerando a primeira metado da normal
tempo1 <- faithful %>% filter(waiting <= 70) %>% select(waiting)
tempo1 <- as_vector(tempo1)

denshalfhat <- function(x){
  m <-  length(x)
  a <- c()
  for(i in 1:m){
    a[i] <- sum(dnorm((x[i]-(tempo1))/h)/(n*h))
  }
  return(a)
}

#gerando o outro lado da normal
tempo2 <- faithful %>% filter(waiting > 65) %>% select(waiting)
tempo2 <- as_vector(tempo2)

densotherhalfhat <- function(x){
  m <-  length(x)
  a <- c()
  for(i in 1:m){
    a[i] <- sum(dnorm((x[i]-(tempo2))/h)/(n*h))
  }
  return(a)
}
#mostrando graficamente
hist(tempo, freq = FALSE, ylim = c(0,0.05))
curve(denshat, add = TRUE, lwd = 2)
curve(denshalfhat, add = TRUE, col= 'darkgreen', lty= 2)
curve(densotherhalfhat, add = TRUE, col= 'orange', lty= 2)

# entendendo as distancias  do da empirica e estimada
#empirica
 oi<- hist(tempo, freq = FALSE, breaks = "FD", ylim = c(0,0.05))

 
 ### estimando com faithful$eruptions via kernel ###
cabum <- faithful$eruptions
h <- 0.334
n <- as.numeric(length(cabum))
denscabum <- function(x){
  b <- c()
  n <- length(x)
  for (i in 1:n) {
    b[i] <- sum(dnorm((x[i] - cabum)/h)/(n*h)) 
  }
  return(b)
}
hist(cabum, freq = FALSE, breaks = "FD", ylim = c(0,2), xlim = c(0,6))
#a densidade ficou meio estranha
curve(denscabum, add = TRUE)
denscabum(c(1, 7))


