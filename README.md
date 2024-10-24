# Anualidades Vencidas

```
source("https://raw.githubusercontent.com/AngelReyesF/anualidadesVencidas/refs/heads/main/anualidadesVencidas.R")
```

# 1. Valor Futuro (FV) conociendo la anualidad (A), tasa de interés (i) y número de pagos (n)
valor_futuro = function(PMT, i, n) {
  FV = A*(((1+i)^n -1)/i)
  return(FV)
}

# Ejemplo
valor_futuro(1000, 0.05, 10)

# 2. Anualidad (A) conociendo el valor futuro (FV), tasa de interés (i) y número de pagos (n)
anualidad_futuro = function(FV, i, n) {
  A = (FV * i) / ((1 + i)^n - 1)
  return(PMT)
}

# Ejemplo
anualidad_futuro(15000, 0.05, 10)

# 3. Número de pagos (n) conociendo el valor futuro (FV), anualidad (A) y la tasa de interés (i)
numero_pagos_futuro = function(FV, A, i) {
  n = log((FV * i) / PMT + 1) / log(1 + i)
  return(n)
}

# Ejemplo
numero_pagos_futuro(15000, 1000, 0.05)

# 4. Tasa de interés (i) conociendo el valor futuro (FV), número de pagos (n) y la anualidad (A)
tasa_futuro <- function(FV, A, n) {
  func <- function(i) {
    FV - A * (((1 + i)^n - 1) / i)
  }
  
  # Verifica que los límites del intervalo sean válidos (función cambia de signo)
  lower <- 0.0001  # Evitar división por cero en el límite inferior
  upper <- 1
  f.lower <- func(lower)
  f.upper <- func(upper)
  
  if (is.na(f.lower) || is.na(f.upper)) {
    stop("No se puede calcular en el intervalo dado.")
  }
  
  # Usar uniroot solo si los signos son diferentes
  if (f.lower * f.upper > 0) {
    stop("La función no cambia de signo en el intervalo dado.")
  }
  
  i <- uniroot(func, c(lower, upper))$root
  return(i)
}

# 5. Valor actual (PV) conociendo la anualidad (A), tasa de interés (i) y número de pagos (n)
valor_actual = function(PMT, i, n) {
  PV = A * ((1 - (1 + i)^(-n)) / i)
  return(PV)
}

# Ejemplo
valor_actual(1000, 0.05, 10)

# 6. Anualidad (A) conociendo el valor actual (PV), tasa de interés (i) y número de pagos (n)
anualidad_actual = function(PV, i, n) {
  PMT = (PV * i) / (1 - (1 + i)^(-n))
  return(A)
}

# Ejemplo
anualidad_actual(10000, 0.05, 10)

# 7. Número de pagos (n) conociendo el valor actual (PV), anualidad (A) y la tasa de interés (i)
numero_pagos_actual = function(PV, A, i) {
  n = log(A / (A - PV * i)) / log(1 + i)
  return(n)
}

# Ejemplo
numero_pagos_actual(10000, 1000, 0.05)

# 8 Tasa de interés (i) conociendo el valor actual (PV), número de pagos (n) y la anualidad (A)
tasa_actual <- function(PV, A, n) {
  func <- function(i) {
    PV - A * ((1 - (1 + i)^(-n)) / i)
  }
  
  # Verifica que los límites del intervalo sean válidos (función cambia de signo)
  lower <- 0.0001  # Evitar división por cero en el límite inferior
  upper <- 1
  f.lower <- func(lower)
  f.upper <- func(upper)
  
  if (is.na(f.lower) || is.na(f.upper)) {
    stop("No se puede calcular en el intervalo dado.")
  }
  
  # Usar uniroot solo si los signos son diferentes
  if (f.lower * f.upper > 0) {
    stop("La función no cambia de signo en el intervalo dado.")
  }
  
  i <- uniroot(func, c(lower, upper))$root
  return(i)
}

# Ejemplo para tasa de interés futura
tasa_futuro(15000, 1000, 10)
