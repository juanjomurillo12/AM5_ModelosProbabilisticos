# Importa la librería para leer excel
library(readxl)
# Importa la librería para pruebas de bondad de ajuste
library(fitdistrplus)


### ----------------------------------------------------------------------------------------------------------------------------###

#leer los datos en excel para llegadas
datosll <- read_excel("datos.xlsx", sheet = "Valorllegadas")
#seleccionar la columna de datos
datosll <- datosll$Llegadas
#impresion de los datos
summary(datosll)
print(datosll)

# Ajuste de bondad exponencial
fit <- fitdist(datosll, "exp")

# Prueba de bondad de ajuste
gofstat(fit, fitnames = "exp")

# imprimir p value
resultados <- gofstat(fit)
resultados$chisqpvalue

###----------------------------------------------------------------------------------------------------------------------------###


#leer los datos en excel para etiquetado
datoset <- read_excel("datos.xlsx", sheet = "ValorEtiquetado")
#seleccionar la columna de datos
datoset <- datoset$Valor
#impresion de los datos
summary(datoset)
print(datoset)

# Ajuste de bondad exponencial
fit <- fitdist(datoset, "exp")

# Prueba de bondad de ajuste
gofstat(fit, fitnames = "exp")

# imprimir p value
resultados <- gofstat(fit)
resultados$chisqpvalue

###----------------------------------------------------------------------------------------------------------------------------###


#leer los datos en excel para validacion
datosval <- read_excel("datos.xlsx", sheet = "ValorValidacion")
#seleccionar la columna de datos
datosval <- datosval$Valor
#impresion de los datos
summary(datosval)
print(datosval)

# Ajuste de bondad exponencial
fit <- fitdist(datosval, "exp")

# Prueba de bondad de ajuste
gofstat(fit, fitnames = "exp")

# imprimir p value
resultados <- gofstat(fit)
resultados$chisqpvalue

###----------------------------------------------------------------------------------------------------------------------------###


#leer los datos en excel para calidad
datoscal <- read_excel("datos.xlsx", sheet = "ValorCalidad")
#seleccionar la columna de datos
datoscal <- datoscal$Valor
#impresion de los datos
summary(datoscal)
print(datoscal)

# Ajuste de bondad exponencial
fit <- fitdist(datoscal, "exp")

# Prueba de bondad de ajuste
gofstat(fit, fitnames = "exp")

# imprimir p value
resultados <- gofstat(fit)
resultados$chisqpvalue

###----------------------------------------------------------------------------------------------------------------------------###


#leer los datos en excel para empaquetado
datosemp <- read_excel("datos.xlsx", sheet = "ValorEmpaquetado")
#seleccionar la columna de datos
datosemp <- datosemp$Valor
#impresion de los datos
summary(datosemp)
print(datosemp)

# Ajuste de bondad exponencial
fit <- fitdist(datosemp, "exp")

# Prueba de bondad de ajuste
gofstat(fit, fitnames = "exp")

# imprimir p value
resultados <- gofstat(fit)
resultados$chisqpvalue

