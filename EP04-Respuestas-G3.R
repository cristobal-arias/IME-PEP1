#Fecha: 28-03-2022
#Grupo 3
#Integrantes: 
#Gerardo Lucero
#Cristobal Arias
#Luis González

library(ggpubr)
library(TeachingDemos)
library(plotrix)
library(dplyr)
############################################################
#############Ejercicio Practico 04##########################
############################################################
#Cambiar directorio si se prueba en otro computador
setwd ("C:\\Users\\Cristobal\\Downloads")
#Se leen los datos
datos <- read.csv2("EP04 datos.csv")

# Se define una semilla para que los números sean aleatorios
set.seed(44)


# 1. El Comité Olímpico cree que el mejor tiempo medio de los atletas negros después de ingresar al programa de
# entrenamiento es de 9,63 segundos. ¿Soportan los datos esta afirmación?

# se procede a verificar si la muestra se acerca a una distribucion normal mediante
# un grafico Q-Q

razaN <- datos %>% filter(Raza == "Negra")
grafico_N <- ggqqplot(
  razaN,
  x = "Posterior",
  title = "Grafico Q-Q para etapa de Post-Entrenamiento de Raza Negra"
)
grafico_N

datos_N <- shapiro.test(razaN$Posterior)
datos_N

#Debido a que las muestras analizadas son independientes. Luego, con el gráfico Q-Q
#se puede observar que las muestras se acercan al modelo normal, además con el valor p
#de shapiro.test, podemos realizar una prueba t de student a la pregunta.

#Tomando un nivel de confianza del 95%
alpha <- 0.05

#Procedemos a definir la hipotesis nula y alternativa
# H0: El mejor tiempo medio de los atletas negros es igual a 9,63 segundos.
# Ha: El mejor tiempo medio de los atletas negros es distinto a 9,63 segundos.

# H0: u = 9,63
# Ha: u != 9,63

#Valor nulo = 9,63
# prueba t Student
prueba1 <- t.test(razaN$Posterior, mu= 9.63, conf.level = 1 - alpha, alternative = 'two.sided')
prueba1

#RESPUESTA P1:
#Como p-value es mayor al nivel de significancia se falla al rechazar la 
#hipotesis nula. Por lo tanto, se puede afirmar con 95% de confianza 
#que el mejor tiempo medio de los atletas negros es 9,63 segundos.

#############################################################################################################
#############################################################################################################
#############################################################################################################


# 2. ¿Sugieren los datos que la mejor marca de los atletas blancos se reduce en menos de 1,16 segundos tras el
# entrenamiento?

razaB <- datos %>% filter(Raza == "Blanca")

diferencia <- (razaB$Previo - razaB$Posterior)


#Hipotesis nula: La diferencia de los tiempos de los atletas blancos previo y posterior al entrenamiento es igual a 1.16 segundos
#Hipotesis alternativa: La diferencia de los tiempos de los atletas blancos previo y posterior al entrenamiento es menor a 1.16 segundos 

##### Matematicamente #####
#HO = (razaB$Previo - razaB$Posterior) = 1,16 seg.
#HA = (razaB$Previo - razaB$Posterior) < 1,16 seg.


# Verificar si la distribución es normal
datos_dif <- shapiro.test(diferencia)
datos_dif

# Se fija el nivel de significación (Nivel de confianza 95%)
alpha <- 0.05

# Como se comprueba que la distribución es cercana a la normal 
# Se procede a hacer la prueba t de student

prueba_2 <- t.test(diferencia,
                   alternative = "less",
                   mu = 1.16,
                   conf.level = 1-alpha)
prueba_2
#Como p-value es mayor al nivel de significancia se falla al rechazar la 
#hipotesis nula. Por lo tanto, se puede afirmar con 95% de confianza 
#que la diferencia del entrenamiento previo y posterior de los atletas blancos no es menor a 1,16 segundos.


#################################################################################################################
################################################################################################################
################################################################################################################

# 3. ¿Es posible afirmar que, en promedio, los atletas blancos superan a los orientales por más de 1,16 segundos
#antes del entrenamiento?


#Hipotesis nula =  El tiempo promedio de los atletas blancos antes del entrenamiento es igual a los orientales en 1,16 seg.
#Hipotesis alternativa = El tiempo promedio de los atletas blancos no superen a los orientales en más de 1,16 seg.

##### Matematicamente #####
#HO = (mean(Raza blanca) - mean(Raza Oriental)) = 1,16 seg.
#HA = (mean(Raza blanca) - mean(Raza Oriental)) > 1,16 seg.



#Obtenemos solo la tabla de los de raza oriental
razaO <- datos %>% filter(Raza == "Oriental")

#De acuerdo a los datos de las tablas, se puede observar claramente que los datos son independientes entre puesto 
#que las muestras utilizadas son de diferente tamaños, ademas de que no son las mismas personas encuestadas al ser de
#diferente raza.





# se procede a verificar si la muestra de los atletas de raza blanca 
#se acerca a una distribucion normal a traves de un grafico Q-Q
grafico_razaB <- ggqqplot(
  razaB,
  x = "Previo",
  title = "Grafico Q-Q para tiempos raza Blanca"
)
grafico_razaB

datos_B <- shapiro.test(razaB$Previo)
datos_B

# se procede a verificar si la muestra de los atletas de raza oriental
#se acerca a una distribucion normal a traves de un grafico Q-Q
grafico_razaO <- ggqqplot(
  razaO,
  x = "Previo",
  title = "Grafico Q-Q para tiempos raza Oriental"
)
grafico_razaO


datos_O <- shapiro.test(razaO$Previo)
datos_O

#Tomando un nivel de confianza del 95%
alpha <- 0.05

# Aplicar la prueba t para dos muestras independientes .
prueba <- t.test(x = razaB$Previo,
                 y = razaO$Previo,
                 paired = FALSE,
                 alternative ="greater",
                 mu = 1.16,
                 conf.level = 1 - alpha )
print(prueba)

#Se obtiene el promedio de los atletas blancos y los atletas orientales
promedioAB <- mean(razaB$Previo)
promedioAO <- mean(razaO$Previo)


#Finalmente se tiene la diferencia del promedio entre los atletas blancos y orientales
diferenciaBO <-(promedioAB - promedioAO) 
cat("La diferencia de los promedios de los tiempos de los atletas Blancos menos los Orientales: ",diferenciaBO, "seg")


#Respuesta P3:
#Por los datos obtenidos anteriormente, con la prueba de t.test más el resultado de la diferencia de los promedios de la raza Blanca menos
#los orientales, se falla al rechazar la hipotesis nula, es por esto que podemos concluir con un 95% de confianza
#que los atletas de raza Blanca no superan en 1,16 segundos a los Orientales previo al entrenamiento.
