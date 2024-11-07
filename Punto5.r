library(markovchain)

# tasas y probabilidades
tasa_llegadas <- 10.23
tasa_etiquetado <- 2.83
tasa_validacion <- 3.83
p_error_validacion <- 0.307
tasa_calidad <- 11.03
p_error_calidad <- 0.188
tasa_empaquetado <- 0.32


# Sx etiquetado
estados_etiquetado<-c(0:4)

# Sy validación
estados_validacion<-c(0:3)

# Sz calidad
estados_calidad<-c(0:3)

# Sw empaquetado
estados_empaquetado<-c(0:3)

# Sh todas las combinaciones
estados = c()

for(i in estados_etiquetado){
  for(j in estados_validacion){
    for(k in estados_calidad){
      for(l in estados_empaquetado){
        estados = c(estados,paste(i,j,k,l, sep = ","))
      }
      
    }
  }
}

matrizQ=matrix(0, nrow=length(estados), ncol=length(estados))

dimnames(matrizQ) = list(estados, estados)

for (fila in estados){
  for (columna in estados){
    
    #Para conocer los valores de cada variable de estado, vamos a dividir cada estado en sus tres componentes, utilizando strsplit. Para mayor facilidad, convertimos los estados de cada una de las variables en un valor numérico.
    
    i = as.numeric(strsplit(fila, ",")[[1]][1])
    j = as.numeric(strsplit(fila, ",")[[1]][2])
    k = as.numeric(strsplit(fila, ",")[[1]][3])
    l = as.numeric(strsplit(fila, ",")[[1]][4])
    
    #Una vez más, realizamos la división de cada estado en sus tres componentes.
    
    m = as.numeric(strsplit(columna, ",")[[1]][1])
    n = as.numeric(strsplit(columna, ",")[[1]][2])
    o = as.numeric(strsplit(columna, ",")[[1]][3])
    p = as.numeric(strsplit(columna, ",")[[1]][4])
    
    #Definición de la matriz de tasas de transición, de acuerdo con la formulación general mostrada antes.
    
    #Llegadas a la primera zona (prensas presión)
    if(m == i + 1 & n == j & o == k & p == l & i < 4){
      matrizQ[fila,columna] = tasa_llegadas
    }
    
    #Procesamiento máquina satinadora (zona 3)
    if(m == i-1 & n == j + 1 & o == k & p == l & i > 0 & j < 3){
      matrizQ[fila,columna] = min(i,4)*tasa_etiquetado
    }
    
    #Procesamiento zona recubrimiento (zona 2)
    if(m == i & n == j - 1 & o == k + 1 & p == l & j > 0 & k < 3){
      matrizQ[fila,columna] = min(j,3)*tasa_validacion*(1-p_error_validacion)
    }
    
    #Procesamiento prensa presión (zona 1)
    if(m == i & n == j  & o == k - 1 & p == l + 1 & k > 0 & l < 15){
      matrizQ[fila,columna] = tasa_calidad*(1-p_error_calidad)
    }
    
    if(m == i & n == j  & o == k - 1 & p == l & k > 0 ){
      matrizQ[fila,columna] = tasa_calidad*(p_error_calidad)
    }
    
    
    if (m == i & n == j & o == k & p == l-min(l,12)) {
      matrizQ[fila, columna] = tasa_empaquetado
    }
    
    
  }
}

row_sums <- rowSums(matrizQ)
row_sums
for(fila in estados){
  for(columna in estados){
    if(fila==columna){
      suma = -rowSums(matrizQ)[fila]
      matrizQ[fila,columna] = suma
      
    }
  }
}


# Calculate row sums of the generator matrix
row_sums <- rowSums(matrizQ)
row_sums

cadenaContinua <- new("ctmc", states = estados,
                      byrow = TRUE, generator = matrizQ)

