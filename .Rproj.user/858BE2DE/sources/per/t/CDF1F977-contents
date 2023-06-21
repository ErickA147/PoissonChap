#' PartosPoisson
#'
#' Calcula la probabilidad de tener al menos cierta cantidad de eventos en un tiempo determinado utilizando la distribución de Poisson.
#'
#' @param x Un vector numérico con los datos de eventos observados.
#' @param tiempo El tiempo en unidades de tiempo en el que se desea calcular la probabilidad.
#' @param probabilidad El número mínimo de eventos deseados.
#' @return Un mensaje que indica la probabilidad de tener al menos la cantidad especificada de eventos en el tiempo dado.
#'
#' @details La función toma como entrada un vector numérico \code{x} que contiene la cantidad de eventos observados en diferentes momentos.
#' Utiliza estos datos para calcular un valor estimado del parámetro de tasa \code{lambda} utilizando la fórmula \code{lambda <- sum_valores / cantidad_valores},
#' donde \code{sum_valores} es la suma de los valores de \code{x} y \code{cantidad_valores} es la cantidad de valores no faltantes.
#' Luego, verifica que \code{x} sea un vector numérico de enteros no negativos y que \code{lambda} sea un número positivo.
#' A continuación, calcula la probabilidad de tener al menos \code{probabilidad} eventos en \code{tiempo} unidades de tiempo utilizando la fórmula de la distribución de Poisson.
#' La probabilidad se presenta en forma de porcentaje.
#'
#' La función también genera una gráfica que muestra la probabilidad en función del tiempo.
#' El eje x representa el tiempo y el eje y representa la probabilidad.
#'
#' @examples
#' # Datos de partos gemelares
#' partos_gemelares <- c(1, 1, 0, 1, 1, 1, 1, 1, 1, 2)
#'
#' # Calcular la probabilidad de al menos 2 partos gemelares en 5 unidades de tiempo
#' resultado <- PartosPoisson(partos_gemelares, tiempo = 5, probabilidad = 2)
#' print(resultado)
#'
#' @export
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

