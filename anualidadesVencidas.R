


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



tasa_actual = function(PV, A, n) {
  func = function(i) {
    PV - A * ((1 - (1 + i)^(-n)) / i)
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


