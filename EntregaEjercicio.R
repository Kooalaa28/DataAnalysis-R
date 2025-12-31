#
# Ejercicio Práctico Inferencia Estadística, Curso 2025-2026
# Facultad de Matemáticas de la Universidad de Murcia
#
# Autor: Ignacio Sánchez Andreu

# Paquetes para la descarga de precios
library(quantmod)
library(xts)

# Descargar cotizaciones diarias de Coca-Cola (precios de cierre)
start_date <- as.Date("2023-01-01")
end_date   <- as.Date("2025-07-01")

getSymbols("KO", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# Extraemos el precio de cierre
KO_close <- Cl(KO)   # xts con una columna

# Dejamos los datos en un único vector
CocaCola <- as.numeric(KO_close)

# Función de retornos semanales
weekly.return <- function(X) {
  ret <- NULL
  n <- length(X) - 5
  for (k in 1:n) {
    ret <- c(ret, (X[k+5] - X[k]) / X[k])
  }
  ret
}

# Retornos semanales a partir de la cotización
retornos_KO <- weekly.return(CocaCola)

# Histogramas con el paquete fdth
library(fdth)
tabla_intervalos_semanal_CocaCola <- fdt(retornos_KO, start = min(retornos_KO), end = max(retornos_KO), h=0.01)
tabla_intervalos_semanal_CocaCola

plot(tabla_intervalos_semanal_CocaCola, 
     type="fh",
     main="Histograma retornos semanales Coca-Cola", 
     ylab="Frecuencia absoluta", 
     col="blue")

# Medidas descriptivas y diagrama de caja y bigote.
summary(retornos_KO)

boxplot(retornos_KO)
boxplot(retornos_KO)$out

# Ajuste normal y lognormal

shapiro.test(retornos_KO)

shapiro.test(log(retornos_KO + abs(min(retornos_KO))+1))

# Ajuste gamma
library(MASS)
fitdistr(retornos_KO + abs(min(retornos_KO))+1, "gamma")

library(fitdistrplus)
ajusteCocaCola <- fitdist(retornos_KO + abs(min(retornos_KO))+1, "gamma", start=list(shape=2694.2042, rate=2535.6009))

summary(ajusteCocaCola)

gofstat(ajusteCocaCola)$chisq
gofstat(ajusteCocaCola)$chisqpvalue

plot(ajusteCocaCola)

# Ajuste t de student

fitdistr(retornos_KO, "t")
0.0177170252*sqrt((8.2228490811)/(8.2228490811-2))
library(fGarch)

ajusteTstudentCocaCola <- fitdist(retornos_KO, "std", start = list(mean=0.0007332938,sd=0.02036607,nu=8.2228490811)) 

summary(ajusteTstudentCocaCola)

gofstat(ajusteTstudentCocaCola)$chisq
gofstat(ajusteTstudentCocaCola)$chisqpvalue


################### IBM #################################

getSymbols("IBM", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# Extraemos el precio de cierre
IBM_close <- Cl(IBM)   # xts con una columna

IBM <- as.numeric(IBM_close)

# Retornos semanales
retornos_IBM <- weekly.return(IBM)

# Histogramas con el paquete fdth
tabla_intervalos_semanal_IBM <- fdt(retornos_IBM, start = min(retornos_IBM), end = max(retornos_IBM), h=0.01)
tabla_intervalos_semanal_IBM

plot(tabla_intervalos_semanal_IBM, 
     type="fh",
     main="Histograma retornos semanales IBM",
     xlab="",
     ylab="Frecuencia absoluta", 
     col="blue")

# Medidas descriptivas y diagrama de caja y bigote.
summary(retornos_IBM)

boxplot(retornos_IBM)
boxplot(retornos_IBM)$out

# Ajuste normal y lognormal

shapiro.test(retornos_IBM)

shapiro.test(log(retornos_IBM + abs(min(retornos_IBM))+1))

# Ajuste gamma
fitdistr(retornos_IBM + abs(min(retornos_IBM))+1, "gamma")

ajusteIBM <- fitdist(retornos_IBM + abs(min(retornos_IBM))+1, "gamma", start=list(shape=1145.09160, rate=1016.93655))

summary(ajusteIBM)

gofstat(ajusteIBM)$chisq
gofstat(ajusteIBM)$chisqpvalue

plot(ajusteIBM)

# Ajuste t de student
fitdistr(retornos_IBM, "t")
0.025530359*sqrt((4.985766437)/(4.985766437-2))

ajusteTstudentIBM <- fitdist(retornos_IBM, "std", start = list(mean=0.005994205, sd=0.03299096, nu=4.985766437))
summary(ajusteTstudentIBM)

gofstat(ajusteTstudentIBM)$chisq
gofstat(ajusteTstudentIBM)$chisqpvalue


################### Contraste T de Student ###########################

var.test(retornos_KO, retornos_IBM, paired=FALSE)

t.test(retornos_KO, retornos_IBM, alternative = "two.sided", var.equal = FALSE)
