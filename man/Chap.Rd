\name{PartosPoisson}
\alias{PartosPoisson}
\title{DISTRIBUCION POISSON DE  PARTOS}
\usage{
   PartosPoisson(x, tiempo, probabilidad = 1)
   }
\arguments{
\item{x}{Un vector numérico con los datos de eventos observados.}
\item{tiempo}{El tiempo en unidades de tiempo en el que se desea calcular la probabilidad.}
\item{probabilidad}{El número mínimo de eventos deseados.}
 }
\description{
  Esta función calcula la probabilidad de tener al menos cierta cantidad de eventos en un tiempo determinado utilizando la distribución de Poisson.
 \examples{
# Datos de partos gemelares
 partos_gemelares <- c(1, 1, 0, 1, 1, 1, 1, 1, 1, 2)

  # Calcular la probabilidad de al menos 2 partos gemelares en 5 unidades de tiempo
 resultado <- PartosPoisson(partos_gemelares, tiempo = 5, probabilidad = 2)
 print(resultado)
 }

 @export
PartosPoisson <- function(x, tiempo, probabilidad = 1) {
  sum_valores <- sum(x, na.rm = TRUE)
  cantidad_valores <- length(x) - sum(is.na(x))

  if (cantidad_valores > 0) {
    # Calcular el valor estimado de lambda
    lambda <- sum_valores / cantidad_valores
  } else {
    mensaje <- "No hay valores para calcular lambda estimado."
    return(mensaje)
  }

  if (!is.numeric(x) || any(x %% 1 != 0) || any(x < 0)) {
    stop("x debe ser un número entero no negativo.")
  }

  if (!is.numeric(lambda) || any(lambda <= 0)) {
    stop("lambda debe ser un número positivo.")
  }

  # Calcular la probabilidad de tener al menos "probabilidad" eventos en "tiempo" unidades de tiempo
  probabilidad_al_menos_uno <- 1 - sum((lambda * tiempo)^0 * exp(-lambda * tiempo) / factorial(0:(probabilidad-1)))

  # Convertir la probabilidad a porcentaje
  probabilidad_porcentaje <- probabilidad_al_menos_uno * 100

  mensaje <- paste("La probabilidad de al menos", probabilidad, "eventos es del", probabilidad_porcentaje, "% en", tiempo, "unidades de tiempo")

  # Crear un vector de tiempo para la gráfica
  t <- seq(0, tiempo, length.out = 100)

  # Calcular la probabilidad en cada punto de tiempo
  probabilidad_t <- 1 - ((lambda * t)^0 * exp(-lambda * t) / factorial(0:(length(t)-1)))

  # Graficar la probabilidad en función del tiempo
  plot(t, probabilidad_t, type = "h", col = "blue", xlab = "Tiempo", ylab = "Probabilidad", main = "Parto Gemelar Poisson (PGP)")

  return(mensaje)
}
