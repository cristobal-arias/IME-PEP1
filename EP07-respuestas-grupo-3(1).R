#Fecha: 05-04-2022
#Grupo 3
#Integrantes: 
#Gerardo Lucero
#Cristobal Arias
#Luis González

library(dplyr)

#----------------------------------------------------------
#----------  PREGUNTA 1 -----------------------------------
#----------------------------------------------------------

# Una plataforma de streaming desea estudiar si hay diferencia en la popularidad de que gozan las series de
# superhéroes entre adolescentes y entre adultos jóvenes. Tras analizar los datos de un grupo de usuarios
# conformado por 12 adolescentes y 15 adultos jóvenes, ha comprobado que 8 de los primeros y 5 de los
# segundos son seguidores de este tipo de series. ¿Cuál debería ser la conclusión de este estudio?
  


# Debido a las caracteristicas de la pregunta se procedera a usar una prueba exacta de Fisher, pues la cantidad
# de muestras es pequena y se estudian dos variables dicotomicas.

# Debido a que es un experimento y a como se señala el enunciado se dividio un grupo de 27 personas
# aleatoriamente, lo cual nos indica que las dos muestras fueron elegidas de forma aleatoria y por 
# lo tanto son independientes entre si y se puede proceder con la prueba.


# Ahora se definiran las hipotesis a contrastar en este caso tanto en lenguaje natural como matematico:

# Natural:

# H0: No xiste una relacion de la edad de la persona con el seguimiento de una serie de superheroes.
# HA: Existe una relacion de la edad de la persona con el seguimiento de una serie de superheroes.

#Matematicamente: 

# Donde </=> da a conocer que no existe relacion entre las variables; <=> da a conocer que existe relacion entre 
# las variables

# H0: edad  </=> serie
# HA: edad <=> serie


#Creacion de tabla.

edad <- c(rep("adolecente",12), rep("adulto",15))
serie <- c(rep("Prefieren",13),rep("No prefieren",14))

datos <- data.frame(edad,serie)
tabla <- xtabs(~.,datos)
print(tabla)

# Nivel de significacion
alfa <- 0.05

# Se aplica la prueba de Fisher 
prueba <- fisher.test(tabla, 1-alfa)
print(prueba)

## RESPUESTA 1 ##

# Dado que el valor p=7.478e-07 es menor al nivel de significancia definido, se rechaza la hipotesis nula
# a favor de la alternativa. Por lo tanto se puede concluir que, con 95% de confianza, la edad
# del individuo no tiene relación en que si prefiere o no una serie de superheroes.
#=============================================================================================================

#----------------------------------------------------------
#----------  PREGUNTA 2 -----------------------------------
#----------------------------------------------------------

# 
# En un estudio acerca del efecto de la deficiencia de vitamina B1 durante la infancia temprana Katz, Haltus &
#   Friedmann (2022) (Journal of Neurolinguistics, 62(5), 101042), se ha concluido que tal carencia se traduce en
# severos problemas de lenguaje. Un nuevo estudio busca verificar este hallazgo, para lo cual ha evaluado el
# desarrollo del lenguaje a los 5 años de vida de 35 parejas de gemelos idénticos donde, por razones médicas,
# los bebés son alimentados con diferentes fórmulas (una carente de vitamina B1 y la otra, no). Los datos
# registrados muestran que:
#   ??? En 10 parejas de gemelos, ninguno presenta trastornos del lenguaje.
# ??? En 2 parejas de gemelos, ambos presentan trastornos del lenguaje.
# ??? En 6 parejas de gemelos, solo aquel que fue alimentado con la fórmula que sí contiene vitamina B1
# desarrolla trastornos del lenguaje.
# ??? En las 17 parejas de gemelos restantes, solo el gemelo con carencia de vitamina B1 presenta trastornos
# del lenguaje.
# ¿Soportan los nuevos datos la conclusión del estudio original?

#Se procede a formular la hipotesis alternativa y nula
# H0: La deficiencia de vitamina B1 durante la infancia no se traduce en trastornos del lenguaje
# H1: La deficiencia de vitamina B1 durante la infancia se traduce en trastornos del lenguaje

#La variable es dicotomica, si la carencia de b1 presenta trastornos de lenguaje o no. Además
# se puede deducir que las muestras son pareadas, por lo que aplicaremos la prueba de mcnemar

# Se construye la tabla
gemelos <- seq(1:35)
carente_B1 <- c(rep("No",10),rep("Si",2),rep("No",6),rep("Si",17))
contiene_B1 <- c(rep("No",10),rep("Si",2),rep("Si",6),rep("No",17))
tabla <- table(carente_B1,contiene_B1)
print(tabla)

# Nivel de significacion
alfa <- 0.05

#Se aplica la prueba
prueba <- mcnemar.test(tabla)
print(prueba)

# RESPUESTA 2

# Como el valor p= 0.03706 con 1 grado de libertad es menor al nivel de significacion establecido (0.05) se 
# rechaza la hipotesis nula en favor de la alternativa. Se puede concluir con el 95% de confianza que 
# la deficiencia de vitamina B1 se traduce en trastornos del lenguaje






#----------------------------------------------------------
#----------  PREGUNTA 3 -----------------------------------
#----------------------------------------------------------
#En su permanente afán de comprender mejor a sus enemigos eternos, los vampiros, Van Helsing desea saber
#si existe relación entre el continente de procedencia de estos seres y su tipo de sangre predilecto. ¿Qué puede
#concluir a partir de los datos recolectados?







#----------------------------------------------------------
#----------  PREGUNTA 4 -----------------------------------
#----------------------------------------------------------
#La Facultad de Ingenieria desea saber si existe diferencia significativa en el desempeno de los estudiantes en
#asignaturas criticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3 asignaturas,
#indica si una muestra de 50 estudiantes aprobo o reprobo. ¿Que puede concluir la Facultad? Indicacion:
#obtenga la muestra a partir del archivo EP07 Datos.csv, usando la semilla 278. Considere un nivel de
#significacion a=0,05.

#Cambiar directorio si se prueba en otro computador
setwd ("C:\\Users\\Cristobal\\Downloads")
#Se leen los datos
datos <- read.csv2("EP07 Datos.csv")
#Se define la semilla
set.seed(278)

#Se procede a formular las hipotesis
# H0: No existe diferencia significativa en el desempenho de los estudiantes en las asignaturas criticas
# H1: Existe diferencia significativa en el desempenho de los estudiantes en las asignaturas criticas

# Las condiciones que se deben verificar para hacer uso de la prueba chi-cuadrado son: 

# 1.- Las observaciones deben ser independientes entre si 
# 2.- Debe haber a lo menos 5 observaciones esperadas en cada grupo

#Las muestras se tomarán como independientes, aunque sean diferentes asignaturas del mismo estudiante, se pueden trabajar como muestras independientes entre sí, ya que no tienen influencia
#Al observar la tabla de datos, se verifica la condicion 2.


#Se crea tabla de contingencia

aprobo_C <- count(datos %>% filter(Calculo == "A"))
aprobo_A <- count(datos %>% filter(Algebra == "A"))
aprobo_F <- count(datos %>% filter(Fisica == "A"))
reprobo_C <- count(datos %>% filter(Calculo == "R"))
reprobo_A <- count(datos %>% filter(Algebra == "R"))
reprobo_F <- count(datos %>% filter(Fisica == "R"))

#orden Calculo, Algebra, Fisica
aprobo <- c(aprobo_C$n,aprobo_A$n,aprobo_F$n)
reprobo <- c(reprobo_C$n,reprobo_A$n,reprobo_F$n)

tabla_p4 <- as.table(rbind(aprobo, reprobo))
dimnames(tabla_p4) <- list(situacion = c("Aprobo", "Reprobo"),
                            asignatura = c("Calculo", "Algebra", "Fisica" ))
print(tabla_p4)

# Nivel de significacion
alfa <- 0.05

# Prueba chi cuadrado de homogeneidad

prueba_p4 <- chisq.test(tabla_p4)

print(prueba_p4)

# RESPUESTA 4 

# Luego de realizar la prueba se obtiene el valor de p = 9.4953-8 con 2 grados de libertad, como p
# es menor al nivel de significancia. Esto trae como consecuencia rechazar la hipotesis nula, en favor de
# la hipotesis alternativa, por lo cual se puede concluir con un nivel del 95% de confianza que existe diferencia en el desempenho de 
# los estudiantes en las asignaturas criticas.