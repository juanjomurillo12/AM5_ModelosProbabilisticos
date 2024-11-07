library(markovchain)

# Sx etiquetado
estados_etiquetado<-c(0:4)

# Sy validación
estados_validacion<-c(0:3)

# Sz calidad
estados_calidad<-c(0:3)

# Sw empaquetado
estados_empaquetado<-c(0:3)

# Sh todas las combinaciones
estados_conjuntos <- expand.grid(Sx = estados_etiquetado,
                                 Sy = estados_validacion,
                                 Sz = estados_calidad,
                                 Sw = estados_empaquetado)

