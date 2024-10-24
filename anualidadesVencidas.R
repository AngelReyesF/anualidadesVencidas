


valor_futuro = function(A, i, n) {
  FV = A*(((1+i)^n -1)/i)
  return(FV)
}


anualidad_futuro = function(FV, i, n) {
  A = (FV * i) / ((1 + i)^n - 1)
  return(A)
}



numero_pagos_futuro = function(FV, A, i) {
  n = log((FV * i) / A + 1) / log(1 + i)
  return(n)
}



tasa_futuro = function(FV, A, n) {
  func = function(i) {
    FV - A * (((1 + i)^n - 1) / i)
  }
  
  lower = 0.0001  
  upper = 1
  f.lower = func(lower)
  f.upper = func(upper)
  
  if (is.na(f.lower) || is.na(f.upper)) {
    stop("No se puede calcular en el intervalo dado.")
  }
  
  if (f.lower * f.upper > 0) {
    stop("La función no cambia de signo en el intervalo dado.")
  }
  
  i = uniroot(func, c(lower, upper))$root
  return(i)
}



valor_actual = function(A, i, n) {
  PV = A * ((1 - (1 + i)^(-n)) / i)
  return(PV)
}



anualidad_actual = function(PV, i, n) {
  A = (PV * i) / (1 - (1 + i)^(-n))
  return(A)
}



numero_pagos_actual = function(PV, A, i) {
  n = log(A / (A - PV * i)) / log(1 + i)
  return(n)
}



# Función auxiliar para expandir el intervalo de búsqueda
expandir_intervalo <- function(func, lower, upper, step = 0.1, max_iter = 10) {
  for (i in 1:max_iter) {
    f.lower <- func(lower)
    f.upper <- func(upper)
    
    if (!is.na(f.lower) && !is.na(f.upper) && f.lower * f.upper <= 0) {
      return(c(lower, upper))  # Si hay cambio de signo, devuelve el intervalo
    }
    
    # Expande el intervalo
    lower <- lower - step
    upper <- upper + step
  }
  stop("No se encontró un intervalo válido para el cambio de signo.")
}


tasa_actual <- function(PV, PMT, n) {
  func <- function(i) {
    PV - PMT * ((1 - (1 + i)^(-n)) / i)
  }
  
  # Establecer un intervalo inicial
  lower <- 0.0001
  upper <- 1
  
  # Intentar expandir el intervalo si no hay cambio de signo
  intervalo <- expandir_intervalo(func, lower, upper)
  
  # Resolver usando uniroot con el intervalo ajustado
  i <- uniroot(func, intervalo)$root
  return(i)
}




