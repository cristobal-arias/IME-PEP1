#Fecha: 05-04-2022
#Grupo 3
#Integrantes: 
#Gerardo Lucero
#Cristobal Arias
#Luis González

library(ggpubr)
library(TeachingDemos)
library(plotrix)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(pwr)
library(tidyverse)


##############################################PREGUNTA 1 ###########################
# Error tipo I: Rechazar la hipotesis nula cuando en realidad es verdadera

#definiendo datos iniciales:

#volumen medio
u0 <- 10
#desviacion estandar
s <- 1
# tamano muestra
n <- 100
# error estandar
se <- s/sqrt(n)
#nivel de significacion = 0.01 => puntuacion critica = 2.58
#alpha <- 0.01
alpha <- 0.01
# Para prueba de dos colas, se utiliza alpha/2
valorz <- qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)

z <- valorz


cant = n
#calculando el intervalo de confianza
x <- seq(u0 - z*se, u0 + z*se, 0.01)

#calculando datos funcion de densidad
y <- dnorm(x, mean = u0, sd = se)

#construyendo data.frame
datos <- data.frame(x, y)

# se procede a graficar la muestra
grafico1 <- ggplot(data = datos, aes(x))

grafico1 <- grafico1 + stat_function(
  fun = dnorm,
  args = list(mean = u0, sd = se),
  colour = "red",
  size = 1
)

# se describe tanto el eje x como el eje y de la grafica
grafico1 <- grafico1 + scale_x_continuous(name = "Litros",
                                          breaks = seq(9.5, 12, 1)) 
grafico1 <- grafico1 + scale_y_continuous(breaks = NULL)
grafico1 <- grafico1 + theme_pubr()

# mediante subset se establecen las condiciones para sombrear aquellas
# areas que pertenecen a rechazar la hipotesis nula cuando esta es
# en realidad verdadera

# area de color rojo que representa aquellos valores de los "Litros" superiores
# a 10 litros
grafico1 <- grafico1 + geom_area(data = subset(datos, x > 10), aes(y = y),
                                 colour = "red", fill = "red", alpha = 0.5)

# area de color rojo que representa aquellos valores de los "Litros" inferiores
# a 9,7 litros
grafico1 <- grafico1 + geom_area(data = subset(datos, x < 9.7), aes(y = y),
                                 colour = "red", fill = "red", alpha = 0.5)

#Se puede observar en el grafico que las zonas coloreadas de color rojo representan la probabilidad de que el
#ingeniero cometa un error de tipo I
grafico1

# se usa pnorm para calcular la probabilidad acumulada, siendo esta
# el area bajo la curva hasta un valor de x especifico

# Area cola inferior
area_inferior <- pnorm(9.7, mean = u0, sd = se, lower.tail = TRUE)


# Area cola superior
area_superior <- pnorm(10, mean = u0, sd = se, lower.tail = FALSE)

#Calculando la probabilidad (%)
Area <- (area_inferior + area_superior)*100


cat("La probabilidad de que el ingeniero cometa un Error de tipo I es del: ",Area, "%")
##############################################PREGUNTA 2 ###########################

u1 = 9.8

# se disminuye el valor de alpha para aumentar la probabilidad de cometer Error
# de tipo II:
alpha <- 0.0001
# Para prueba de dos colas, se utiliza alpha/2
valorz <- qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)

z <- valorz


##codigo para ver grafico
#calculando el intervalo de confianza
x1 <- seq(u1 - z*se, u1 + z*se, 0.01)

#calculando datos funcion de densidad
y1 <- dnorm(x1, mean = u1, sd = se)

#construyendo data.frame
datos1 <- data.frame(x = x1, y = y1)
datos1

# se procede a graficar la muestra
grafico2 <- ggplot(data = datos1, aes(x))


grafico2 <- grafico1 + stat_function(
  fun = dnorm,
  args = list(mean = u1, sd = se),
  colour = "steelblue",
  size = 1
)
grafico2 <- grafico2 + theme_pubr()

# mediante subset se establecen las condiciones para sombrear aquellas
# areas que pertenecen a no rechazar la hipotesis nula cuando esta es
# en realidad verdadera
grafico2 <- grafico2 + geom_area(data = subset(datos1, x >= 9.7 & x <= 10 ), aes(y = y),
                                 colour = "steelblue", fill = "steelblue", alpha = 0.5)

# se muestra el grafico con la parte sombreada de color azul aquella area que representa la probabilidad
#de cometer un error de tipo II cuando el verdadero volumen medio es de 9.7 litros
grafico2


# se usa pnorm para calcular la probabilidad acumulada, siendo esta
# el area bajo la curva hasta un valor de x especifico

# Area cola inferior 1
area_inferior_1 <- pnorm(9.7, mean = u1, sd = se, lower.tail = TRUE)


# Area cola inferior 2
area_inferior_2 <- pnorm(10, mean = u1, sd = se, lower.tail = TRUE)

# Se procede a calcular la diferencia entre ambas areas
dif_Area <- area_inferior_2 - area_inferior_1
# Area en porcentaje
Area2 <- (area_inferior_2 - area_inferior_1)*100

cat("La probabilidad de que el ingeniero cometa un Error de tipo II es del: ",Area2, "%")

##############################################PREGUNTA 3 ###########################
#Vector con los valores del rango del efecto de la dureza media, sabiendo que el valor nulo es 10 y el rango de valores
#va de 9.5 hasta 10

efecto <- seq(-8,8,0.01)

#Calculo del poder de acuerdo a los datos entregados en el enunciado
poder <- power.t.test(n = 100,
                      delta = efecto,
                      sd = 1,
                      sig.level = alpha, # CAMBIAR
                      type = "one.sample",
                      alternative = "two.sided")$power

#Construccion del data.frame
datos <- data.frame(x = efecto, y = poder)

#Grafico de la curva de poder
G <- ggplot(datos, aes(efecto, poder))
G <- G + geom_line()
G <- G + geom_line()
G <- G + ylab("Poder Estadístico")
G <- G + xlab("Volumen [Litros]")

G <- G + theme_pubr()
G <- G + ggtitle("Gráfico de poder estadístico")
G <- G + geom_vline(xintercept = 0, linetype = "dashed")


#Se muestra como respuesta el grafico que representa al poder estadistico de la muestra suponiendo que las 
#verdaderas durezas medias pueden variar entre 9.5 y 10 [litros]. 
print(G)

##############################################PREGUNTA 4 ###########################
#Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían 
#revisarse para conseguir un poder estadístico de 0,75 y un nivel de significación de 0,05?

# Para encontrar la cantidad de bidones con un volumen medio de 10 litros que deben ocuparse para conseguir un 
# poder estadistico de 0.75 y un nivel de significacion de 0.05 se hace uso
# de la funcion "pwr.t.test()". Para ello, se encontrara como primera instancia
# el tamano de efecto (d de Cohen):

cohen <- (u1-u0)/s

#Se aplica pwr.t.test
cant_bidones <- pwr.t.test(n = NULL,
                              d = cohen,
                              sig.level = 0.05,
                              power = 0.75,
                              type = "one.sample",
                              alternative = "two.sided")

#Al redondear
valor <- ceiling(cant_bidones[["n"]])
#RESPUESTA P4
#La cantidad de bidones a revisar son aproximadamente 176 con el objetivo de conseguir un poder estadistico de 0.75
#y un nivel de significacion de 0.05

##############################################PREGUNTA 5 ###########################
#¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad 
#de cometer un error de tipo I a un 1% solamente?

# Al usar un nivel de confianza de 99%
# Se tiene un nivel de significancia %1

# Al aplicar el pwr.t.test
cant_bidones <- pwr.t.test(n = NULL,
                              d = cohen,
                              sig.level = 0.01,
                              power = 0.75,
                              type = "one.sample",
                              alternative = "two.sided")

#Al redondear
valor2 <- ceiling(cant_bidones[["n"]])
valor2
#RESPUESTA P5

#La cantidad de bidones a revisar son aproximadamente 268 
# si la probabilidad se baja a un 1%


