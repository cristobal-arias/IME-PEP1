#Fecha: 07-04-2022
#Grupo 3
#Integrantes: 
#Gerardo Lucero
#Cristobal Arias
#Luis González


#PREGUNTA 1
#Estudios previos habían determinado que la proporción de autoras en la especialidad 
#de psiquiatría era de 48%. ¿Respaldan estos datos tal estimación?

#Los siguientes datos se basan en un artículo publicado por Hart & Perlis (2019) (JAMA Internal Mededicine, 179(9),
#1285-1287) acerca de la proporción de mujeres autoras de artículos científicos en el área médica. La tabla muestra
#la cantidad de autoras y autores para diferentes especialidades.

#Valor nulo p0
p0 <- 0.48
#tamano de la muestra
muestra <- 72
#proporción de exito segun la tabla de datos
p_ex <- 30/muestra
#estimador
alpha <- 0.05
#se define la cantidad de exitos y fracasos
exito <- 30
fracaso <- 42


#Definiendo hipotesis en lenguaje natural
#HIPOTESIS NULA
#La estimacion de incidencia de la proporcion de autoras en el area de psiquiatria es igual a 0.48

#HIPOTESIS ALTERNATIVA
#La estimacion de incidencia de la proporcion de autoras en el area de psiquiatria es distinto a 0.48


#Definiendo hipotesis matematicamente:
#Hipotesis Nula:
#H0 : p = 0.48
#Hipotesis alternativa
#HA : p /= 0.48


#Calculando el intervalo de confianza
error_est <- sqrt((p_ex * (1 - p_ex))/muestra)
Z_critico <- qnorm(alpha/ 2, lower.tail = FALSE)
inferior <- p_ex - Z_critico * error_est
superior <- p_ex + Z_critico * error_est

cat("Intervalo de confianza = [", inferior, ", ", superior, "]\n")

error_est_hip <- sqrt((p0 * (1-p0))/muestra)
Z <- (p_ex -p0)/error_est_hip
p <- pnorm(Z, lower.tail = FALSE)


#RESPUESTA PREGUNTA 1

#Dado que el valor p = 0.858961 es mayor al nivel de significancia definido (a=0.05) 
#se puede inferir que no se rechaza la hipotesis nula, por lo tanto los datos entregados
#respaldan la estimacion dada (0.48).




