library(markovchain)

# Parámetros
tasa_llegadas <- 10.23
tasa_etiquetado <- 2.83
tasa_validacion <- 3.83
p_error_validacion <- 0.307
tasa_calidad <- 11.03
p_error_calidad <- 0.188
tasa_empaquetado <- 0.32

# Estados posibles para cada etapa
estados_etiquetado <- c(0:4)
estados_validacion <- c(0:3)
estados_calidad <- c(0:3)
estados_empaquetado <- c(0:15)

# Generación de todos los estados posibles
estados <- c()
for(i in estados_etiquetado) {
  for(j in estados_validacion) {
    for(k in estados_calidad) {
      for(l in estados_empaquetado) {
        estados <- c(estados, paste(i, j, k, l, sep = ","))
      }
    }
  }
}

# Inicialización de la matriz de transición
matrizQ <- matrix(0, nrow = length(estados), ncol = length(estados))
dimnames(matrizQ) <- list(estados, estados)

# Llenado de la matriz de transición según las condiciones dadas
for (fila in estados) {
  for (columna in estados) {
    
    # Descomposición del estado actual (fila) y el estado de transición (columna)
    i <- as.numeric(strsplit(fila, ",")[[1]][1])
    j <- as.numeric(strsplit(fila, ",")[[1]][2])
    k <- as.numeric(strsplit(fila, ",")[[1]][3])
    l <- as.numeric(strsplit(fila, ",")[[1]][4])
    
    m <- as.numeric(strsplit(columna, ",")[[1]][1])
    n <- as.numeric(strsplit(columna, ",")[[1]][2])
    o <- as.numeric(strsplit(columna, ",")[[1]][3])
    p <- as.numeric(strsplit(columna, ",")[[1]][4])
    
    # Condiciones de transición según la formulación
    # Llegadas a la primera zona (etiquetado)
    if (m == i + 1 & n == j & o == k & p == l & i < 4) {
      matrizQ[fila, columna] <- tasa_llegadas
    }
    
    # Procesamiento en la máquina satinadora (etiquetado a validación)
    if (m == i - 1 & n == j + 1 & o == k & p == l & i > 0 & j < 3) {
      matrizQ[fila, columna] <- min(i, 4) * tasa_etiquetado
    }
    
    # Procesamiento en la zona de recubrimiento (validación a calidad)
    if (m == i & n == j - 1 & o == k + 1 & p == l & j > 0 & k < 3) {
      matrizQ[fila, columna] <- min(j, 3) * tasa_validacion * (1 - p_error_validacion)
    }
    
    # Procesamiento en la prensa de presión (calidad a empaquetado, éxito)
    if (m == i & n == j & o == k - 1 & p == l + 1 & k > 0 & l < 15) {
      matrizQ[fila, columna] <- tasa_calidad * (1 - p_error_calidad)
    }
    
    # Procesamiento en la prensa de presión (calidad, error)
    if (m == i & n == j & o == k - 1 & p == l & k > 0) {
      matrizQ[fila, columna] <- tasa_calidad * p_error_calidad
    }
    
    # Procesamiento de empaquetado 
    if (m == i & n == j & o == k & p == l - min(l, 12) & l > 0 ) {
      matrizQ[fila, columna] <- tasa_empaquetado
    }
  }
}

# Ajuste de la diagonal para que la suma de cada fila sea cero
for(fila in estados){
  for(columna in estados){
    if(fila==columna){
      matrizQ[fila,columna] = -rowSums(matrizQ)[fila]
    }
  }
}

# Verificación de que las filas sumen cero
row_sums <- rowSums(matrizQ)
print(row_sums)  # Esto debería mostrar un vector de ceros (o valores muy cercanos a cero)

# Creación de la cadena de Markov continua
cadenaContinua <- new("ctmc", states = estados, byrow = TRUE, generator = matrizQ)


# Punto 6

probabilidades_lp <- steadyStates(cadenaContinua)

# Inicializar el valor esperado
valor_esperado_ordenes <- 0

# Calcular el valor esperado
for (idx in seq_along(estados)) {
  valor <- estados[idx]
  
  # Extraer i, j, k, l
  i <- as.numeric(strsplit(valor, ",")[[1]][1])
  j <- as.numeric(strsplit(valor, ",")[[1]][2])
  k <- as.numeric(strsplit(valor, ",")[[1]][3])
  l <- as.numeric(strsplit(valor, ",")[[1]][4])
  
  # Calcular i + j + k + l
  suma_componentes <- i + j + k + l
  
  # Multiplicar por la probabilidad a largo plazo correspondiente y acumular
  valor_esperado_ordenes <- valor_esperado_ordenes + (suma_componentes * probabilidades_lp[idx])
}

# Imprimir el valor esperado a largo plazo
print(round(valor_esperado_ordenes,2))


# Punto 7

# Definir los estados vacío y lleno
estado_vacio <- which(estados == "0,0,0,0")
estado_lleno <- which(estados == "4,3,3,15")


# Crear una copia de la matriz generadora y remover la fila y columna del estado final
matrizQrecortada <- matrizQ[-estado_lleno, -estado_lleno]

# Calcular la inversa de la matriz recortada
inversaQ <- solve(-matrizQrecortada)

# Calcular el tiempo esperado de primera pasada usando la suma de la primera fila de la matriz inversa
tiempoPrimeraPasada <- round(sum(inversaQ[estado_vacio, ]),2)

# Imprimir el resultado
cat("El tiempo esperado para que el sistema pase de vacío a lleno es de:", tiempoPrimeraPasada, "\n")


