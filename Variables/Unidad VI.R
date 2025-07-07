#################### indice de capacidad de proceso (Cp)################
#Imaginemos que estamos trabajando en una fábrica de botellas de plástico, 
#donde el diámetro de la boca de las botellas debe estar dentro de un rango específico 
#para asegurar que las tapas encajen correctamente.

# Definir los parámetros
LIL <- 28  # Límite inferior de especificación
LUS <- 32  # Límite superior de especificación
desviacion_estandar <- 1  # Desviación estándar

# Calcular el índice de capacidad del proceso (Cp)
Cp <- (LUS - LIL) / (6 * desviacion_estandar)

# Mostrar el resultado
cat("El índice de capacidad del proceso (Cp) es:", Cp, "\n")

#un índice de capacidad del proceso (Cp) de 0.67 sugiere que el proceso de fabricación 
#de botellas de plástico necesita atención y mejoras significativas para garantizar 
#que los productos cumplan consistentemente con las especificaciones. 
#Esto es crucial para mantener la calidad del producto y la satisfacción del cliente.



#################### indice de capacidad de proceso (Cpk)##################
#El helado que se sirve en una heladería debe estar a una temperatura 
#entre -15 °C y -35 °C. El proceso de refrigeración que mantiene la temperatura 
#tiene una desviación estándar (DE) de 2 °C. Y el valor medio de esta temperatura 
#es de -25 °C. Con estos datos, obtenga el índice de capacidad de proceso (ICP) 
#para este proceso.


# Definir los parámetros
media <- -25  # Media de la temperatura
desviacion_estandar <- 2  # Desviación estándar
limite_inferior <- -35  # Límite inferior
limite_superior <- -15  # Límite superior

# Calcular el índice de capacidad de proceso (ICP)
ICP <- (limite_superior - limite_inferior) / (6 * desviacion_estandar)

# Mostrar el resultado
cat("El índice de capacidad de proceso (ICP) es:", ICP, "\n")

#el valor de Cpk es mayor a 1, esto sugiere que el proceso es capaz de mantener 
#la temperatura del helado dentro de los límites especificados, lo que es deseable 
#para garantizar la calidad del producto.


#################### indice de capacidad de proceso (Cpm) #####################

#Supongamos que estamos fabricando un componente que debe tener un diámetro entre 10 mm y 12 mm.
#La media del proceso es de 11 mm, la desviación estándar es de 0.5 mm, y el objetivo 
#es que el diámetro sea exactamente 11 mm.

# Definir los parámetros
LIL <- 10  # Límite inferior
LUS <- 12  # Límite superior
media <- 11  # Media del proceso
desviacion_estandar <- 0.5  # Desviación estándar
objetivo <- 11  # Valor objetivo

# Calcular la variación total
variacion_total <- desviacion_estandar^2 + (media - objetivo)^2

# Calcular Cpm
Cpm <- (LUS - LIL) / (6 * sqrt(variacion_total))

# Mostrar el resultado
cat("El índice de capacidad del proceso (Cpm) es:", Cpm, "\n")

#Un valor de Cpm de aproximadamente 0.67 indica que el proceso tiene una capacidad moderada 
#para cumplir con el objetivo y los límites de especificación.
#Un valor de Cpm mayor a 1 sería deseable, ya que indicaría que el proceso es capaz de 
#producir dentro de las especificaciones y cerca del objetivo.

#################### intervalos de confianza para indices de capacidad###########


#Supongamos que estamos trabajando con un proceso de fabricación de tornillos, 
#donde los límites de especificación son 9.9 mm y 10.1 mm, la media del diámetro es de 10.0 mm 
#y la desviación estándar es de 0.05 mm.

# Definir los parámetros
LIL <- 9.9  # Límite inferior de especificación
LUS <- 10.1  # Límite superior de especificación
desviacion_estandar <- 0.05  # Desviación estándar
n <- 30  # Tamaño de la muestra

# Calcular el índice de capacidad del proceso (Cp)
Cp <- (LUS - LIL) / (6 * desviacion_estandar)

# Calcular el Error Estándar (SE)
SE_Cp <- (6 * desviacion_estandar) / sqrt(n)

# Valor crítico Z para un nivel de confianza del 95%
Z <- 1.96

# Calcular el intervalo de confianza para Cp
IC_Cp_lower <- Cp - Z * SE_Cp
IC_Cp_upper <- Cp + Z * SE_Cp

# Mostrar los resultados
cat("El índice de capacidad del proceso (Cp) es:", Cp, "\n")
cat("El intervalo de confianza al 95% para Cp es: [", IC_Cp_lower, ", ", IC_Cp_upper, "]\n")

#La información proporcionada por el intervalo de confianza permite a los 
#gerentes y responsables de calidad tomar decisiones informadas sobre la capacidad 
#del proceso. Un intervalo que no incluye valores menores a 1 indica que el proceso 
#está bien controlado y cumple con los estándares de calidad.

#################### Análisis de Capacidad Utilizando un Histograma #####################

#Vamos a realizar un análisis de capacidad utilizando un histograma para visualizar 
#la distribución de los diámetros de tornillos producidos en una fábrica. 
#Supongamos que hemos tomado una muestra de 100 tornillos y hemos medido sus diámetros. 
#Los límites de especificación son 9.9 mm y 10.1 mm.
#sigue una distribución normal con una media de 10.0 mm y una desviación 
#estándar de 0.05 mm.

# Cargar las librerías necesarias
library(ggplot2)

# Establecer la semilla para reproducibilidad
set.seed(123)

# Generar datos simulados para los diámetros de los tornillos
n <- 100  # Tamaño de la muestra
media <- 10.0  # Media del diámetro
desviacion_estandar <- 0.05  # Desviación estándar

# Generar la muestra
diametros <- rnorm(n, mean = media, sd = desviacion_estandar)

# Crear un histograma utilizando ggplot2
histograma <- ggplot(data = data.frame(diametros = diametros), aes(x = diametros)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = 9.9), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 10.1), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histograma de Diámetros de Tornillos",
       x = "Diámetro (mm)",
       y = "Frecuencia") +
  theme_minimal()

# Mostrar el histograma

print(histograma)

# Calcular y mostrar el índice Cp
LIL <- 9.9  # Límite inferior de especificación
LUS <- 10.1  # Límite superior de especificación
Cp <- (LUS - LIL) / (6 * desviacion_estandar)
cat("El índice de capacidad del proceso (Cp) es:", Cp, "\n")

#El histograma proporciona una visualización clara de la distribución de los diámetros 
#de los tornillos. Puedes observar cómo se distribuyen los datos en relación 
#con los límites de especificación. Si la mayoría de los datos se encuentran 
#dentro de los límites y la forma del histograma es aproximadamente simétrica, 
#esto indica que el proceso está bien controlado.


############ Análisis de Capacidad Utilizando un una Gráfica de Probabilidad #############

#evaluar si los diámetros de los tornillos producidos en una fábrica siguen una 
#distribución normal. tomado una muestra de 100 tornillos y hemos medido sus diámetros.
#generaremos datos simulados con una media de 10.0 mm y una desviación estándar de 0.05 mm.

# Cargar las librerías necesarias
library(ggplot2)

# Establecer la semilla para reproducibilidad
set.seed(123)

# Generar datos simulados para los diámetros de los tornillos
n <- 100  # Tamaño de la muestra
media <- 10.0  # Media del diámetro
desviacion_estandar <- 0.05  # Desviación estándar

# Generar la muestra
diametros <- rnorm(n, mean = media, sd = desviacion_estandar)

# Crear una gráfica de probabilidad (Q-Q plot)
qqnorm(diametros, main = "Gráfica de Probabilidad (Q-Q Plot)")
qqline(diametros, col = "red", lwd = 2)  # Línea de referencia

# Calcular y mostrar el índice Cp
LIL <- 9.9  # Límite inferior de especificación
LUS <- 10.1  # Límite superior de especificación
Cp <- (LUS - LIL) / (6 * desviacion_estandar)
cat("El índice de capacidad del proceso (Cp) es:", Cp, "\n")

#Si los puntos se alinean cerca de la línea roja, esto indica que los datos siguen 
#una distribución normal. Si hay desviaciones significativas, esto sugiere que los 
#datos no son normalmente distribuidos.

############ Análisis de Capacidad de Proceso Utilizando Diagramas de Control##############

#Monitorear la variabilidad en los diámetros de tornillos producidos en una fábrica. 
#hemos tomado varias muestras de 5 tornillos cada una y hemos medido sus diámetros. 
#Los límites de especificación son 9.9 mm y 10.1 mm. 
#siguen una distribución normal con una media de 10.0 mm y una desviación estándar de 0.05 mm. 
#Tomaremos 10 muestras de 5 tornillos cada una.

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Establecer la semilla para reproducibilidad
set.seed(123)

# Parámetros del proceso
n_muestras <- 10  # Número de muestras
tamaño_muestra <- 5  # Tamaño de cada muestra
media <- 10.0  # Media del diámetro
desviacion_estandar <- 0.05  # Desviación estándar

# Generar datos simulados
diametros <- matrix(rnorm(n_muestras * tamaño_muestra, mean = media, sd = desviacion_estandar), 
                    nrow = n_muestras, ncol = tamaño_muestra)

# Calcular las medias y rangos de cada muestra
medias <- rowMeans(diametros)
rangos <- apply(diametros, 1, function(x) max(x) - min(x))

# Calcular la media de las medias y la media de los rangos
media_general <- mean(medias)
rango_promedio <- mean(rangos)

# Calcular los límites de control
LCL_media <- media_general - 2 * (desviacion_estandar / sqrt(tamaño_muestra))
UCL_media <- media_general + 2 * (desviacion_estandar / sqrt(tamaño_muestra))
LCL_rango <- 0  # Límite inferior del rango no puede ser negativo
UCL_rango <- 2.223 * rango_promedio  # Aproximación para n=5

# Crear un data frame para graficar
resultados <- data.frame(Muestra = 1:n_muestras, Media = medias, Rango = rangos)

# Gráfico de control para las medias
ggplot(resultados, aes(x = Muestra, y = Media)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = media_general, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = UCL_media, color = "red", linetype = "dashed") +
  geom_hline(yintercept = LCL_media, color = "red", linetype = "dashed") +
  labs(title = "Diagrama de Control para las Medias de Diámetros de Tornillos",
       x = "Muestra",
       y = "Media del Diámetro (mm)") +
  theme_minimal()

# Gráfico de control para los rangos
ggplot(resultados, aes(x = Muestra, y = Rango)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = UCL_rango, color = "red", linetype = "dashed") +
  labs(title = "Diagrama de Control para los Rangos de Diámetros de Tornillos",
       x = "Muestra",
       y = "Rango del Diámetro (mm)") +
  theme_minimal()

# Si alguno de los puntos en los gráficos de control cae fuera de los límites de control, 
#esto indica que el proceso puede estar fuera de control y se deben investigar las causas.


#Si los gráficos muestran que el proceso está bajo control, se puede proceder a 
#calcular los índices de capacidad del proceso (Cp, Cpk) para evaluar la capacidad 
#del proceso.





############ Estimación de los Límites de Tolerancia Natural de un Proceso #################

"En una empresa que manufactura piezas de inyección de plástico se
proyecta la producción de una nueva pieza. Son varias sus características de calidad:
  peso de la preforma, contenido, resistencia, etc. Por ello, con la idea de tener
información para establecer las tolerancias se produce, a manera de prueba, un
pequeño lote de 30 piezas. A continuación se muestran los datos del peso (en gramos)
de las preformas."


# Datos del peso de las preformas
pesos <- c(35.513, 35.903, 36.084, 35.848,
           35.736, 36.302, 36.041, 36.095,
           36.056, 35.897, 36.297, 35.757,
           36.104, 36.102, 36.022, 35.891,
           35.863, 36.084, 36.252, 36.124,
           36.194, 35.880, 36.089, 36.141)

# Calcular la media y la desviación estándar
media <- mean(pesos)
desviacion_estandar <- sd(pesos)

# Parámetros del análisis
n <- length(pesos)  # Tamaño de la muestra
K <- 3.170  # Valor de K(90, 99)

# Estimación de los límites de tolerancia natural
LTL <- media - K * (desviacion_estandar / sqrt(n))  # Límite inferior
UTL <- media + K * (desviacion_estandar / sqrt(n))  # Límite superior

# Mostrar resultados
cat("Media de la muestra:", media, "\n")
cat("Desviación estándar de la muestra:", desviacion_estandar, "\n")
cat("Límite inferior de tolerancia natural (LTL):", LTL, "\n")
cat("Límite superior de tolerancia natural (UTL):", UTL, "\n")

# Graficar los resultados
hist(pesos, breaks = 10, main = "Histograma de Pesos de Preformas",
     xlab = "Peso (gramos)", col = "lightblue", border = "black")
abline(v = media, col = "blue", lwd = 2)  # Media
abline(v = LTL, col = "red", lwd = 2, lty = 2)  # Límite inferior
abline(v = UTL, col = "red", lwd = 2, lty = 2)  # Límite superior
legend("topright", legend = c("Media", "LTL", "UTL"), 
       col = c("blue", "red", "red"), lty = c(1, 2, 2), lwd = 2)

"De esta manera, con una confianza de 90%, el 99% de la distribución del peso de la
preforma de la pieza se encuentra entre 35,4239 y 36,6096 g. Por lo que estos límites
y las necesidades funcionales del producto pueden ser usados como información por
el diseñador del producto para establecer las especificaciones."


############ Análisis de Capacidad de Procesos Usando Experimento Diseñados #########

"evaluar cómo diferentes factores afectan el peso de piezas de inyección de plástico. 

estamos interesados en dos factores: la temperatura de inyección y la presión de inyección.

hemos realizado un experimento con dos niveles para cada factor (bajo y alto) 
y hemos medido el peso de las piezas resultantes. Los datos son los siguientes:

Temperatura de inyección: 180 °C (bajo), 220 °C (alto)
Presión de inyección: 500 psi (bajo), 700 psi (alto)

datos:
180, 500: 35.5, 35.7, 35.6
180, 700: 36.0, 36.1, 36.2
220, 500: 36.5, 36.4, 36.6
220, 700: 37.0, 37.1, 37.2
"
# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Crear un data frame con los datos del experimento
data <- data.frame(
  Temperatura = rep(c(180, 220), each = 6),
  Presion = rep(c(500, 500, 500, 700, 700, 700), times = 2),
  Peso = c(35.5, 35.7, 35.6, 36.0, 36.1, 36.2,
           36.5, 36.4, 36.6, 37.0, 37.1, 37.2)
)

# Resumen de los datos
summary_data <- data %>%
  group_by(Temperatura, Presion) %>%
  summarise(Media = mean(Peso), Desviacion = sd(Peso), .groups = 'drop')

# Mostrar el resumen
print(summary_data)

# Análisis de varianza (ANOVA)
anova_result <- aov(Peso ~ Temperatura * Presion, data = data)
summary(anova_result)

# Gráfico de interacción
interaction_plot <- ggplot(data, aes(x = factor(Temperatura), y = Peso, color = factor(Presion))) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_line(stat = "summary", fun = mean, aes(group = factor(Presion))) +
  labs(title = "Gráfico de Interacción: Peso vs Temperatura y Presión",
       x = "Temperatura (°C)",
       y = "Peso (gramos)",
       color = "Presión (psi)") +
  theme_minimal()

# Mostrar el gráfico de interacción
print(interaction_plot)

# Calcular los límites de tolerancia natural para el peso
media_global <- mean(data$Peso)
desviacion_global <- sd(data$Peso)
n <- nrow(data)
K <- 3.170  # Valor de K(90, 99) como ejemplo

LSTN <- media_global + K * (desviacion_global / sqrt(n))  # Límite Superior
LISTN <- media_global - K * (desviacion_global / sqrt(n))  # Límite Inferior

# Mostrar límites de tolerancia
cat("Límite Superior de Tolerancia Natural (LSTN):", LSTN, "\n")
cat("Límite Inferior de Tolerancia Natural (LISTN):", LISTN, "\n")

" El resultado del análisis de varianza nos indica si hay diferencias significativas 
en el peso de las piezas debido a los factores de temperatura y presión. 
Un valor p menor a 0.05 sugiere que al menos uno de los factores tiene un efecto 
significativo."

