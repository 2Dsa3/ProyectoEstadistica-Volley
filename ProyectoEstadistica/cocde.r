# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(reshape2)

# Leer el archivo CSV
data <- read.csv("data_clean.csv", stringsAsFactors = FALSE, na.strings = "NA")


# Aplanar los datos y eliminar NA
conteos <- na.omit(c(data$Viernes.1, data$Lunes.1, data$Martes.1, data$Miercoles.1, data$Jueves.1, data$Lunes.2, data$Martes.2))
conteos

#Encontrar la media de los datos



lambda <- mean(conteos)
Sd <- sd(conteos)
mediana <- median(conteos)
moda <- as.numeric(names(sort(table(conteos), decreasing = TRUE)[1]))
moda
lambda
mediana
Sd

# Perform t-test to get confidence interval
result <- t.test(conteos, conf.level = 0.95, , mu = 2.5)

# Display confidence interval
result$conf.int
result
#el resultado indica que la media de 2.5 no puede ser descartada



#Establecer las frecuencias para cada cantidad de personas que llegan a frutanga
observed_frequencies <- table(conteos)
observed_frequencies

#Encontrar las frecuencias esperadas por la distribucion de poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda)
expected_frequencies <- prob * length(conteos)
expected_frequencies

#sumar intervalos para conseguir que todas las fe>5
combined_observed <- c(observed_frequencies[1:6], sum(observed_frequencies[7:length(observed_frequencies)]))
combined_expected <- c(expected_frequencies[1:6], sum(expected_frequencies[7:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(prob[1:6], sum(prob[7:length(prob)]))

#Tomar en consideracion valores de personas > 12 y agregarlos con el septimo intervalo
combined_prob[7] <- combined_prob[7] + (1-sum(combined_prob))

#verificar que la suma de las probabilidades de 1
sum(combined_prob)
combined_prob

Xiq <- sum(((combined_observed - combined_expected)^2)/combined_expected)

#Prueba Xi cuadrado
result <- pchisq(Xiq, df = length(combined_observed)-1, lower.tail = FALSE)
result

datos<- conteos
as.numeric(datos)
# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórrico y el observado
qqplot(x = qpois(ppoints(as.numeric(datos)), lambda = mean(datos)),
       y = as.numeric(datos),
       main = "Gráfico QQ para poisson: personas que llegan a frutanga",
       xlab = "Cuantiles teóricos segun poisson",
       ylab = "Cuantiles de número de personas observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()


#Dias individuales

#Viernes 1

# Leer el archivo CSV
data <- read.csv("data_clean.csv", stringsAsFactors = TRUE, na.strings = "NA")

# Aplanar los datos y eliminar NA
conteos <- na.omit(c(data$Viernes.1))
conteos

#Encontrar la media de los datos



lambda <- mean(conteos)
Sd <- sd(conteos)
mediana <- median(conteos)
moda <- as.numeric(names(sort(table(conteos), decreasing = TRUE)[1]))
moda
lambda
mediana
Sd

#Establecer las frecuencias para cada cantidad de personas que llegan a frutanga
observed_frequencies <- table(conteos)
observed_frequencies

#Encontrar las frecuencias esperadas por la distribucion de poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda)
expected_frequencies <- prob * length(conteos)
expected_frequencies

#sumar intervalos para conseguir que todas las fe>5
combined_observed <- c(observed_frequencies[1:4], sum(observed_frequencies[5:length(observed_frequencies)]))
combined_expected <- c(expected_frequencies[1:4], sum(expected_frequencies[5:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(prob[1:4], sum(prob[5:length(prob)]))

#Tomar en consideracion valores de personas > 12 y agregarlos con el septimo intervalo
combined_prob[5] <- combined_prob[5] + (1-sum(combined_prob))

#verificar que la suma de las probabilidades de 1
sum(combined_prob)
combined_prob

Xiq <- sum(((combined_observed - combined_expected)^2)/combined_expected)

#Prueba Xi cuadrado
result <- pchisq(Xiq, df = length(combined_observed)-1, lower.tail = FALSE)
result

datos<- conteos

# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórrico y el observado
qqplot(x = qpois(ppoints(datos), lambda = mean(datos)),
       y = datos,
       main = "Gráfico QQ para poisson: personas que llegan a frutanga (primer viernes)",
       xlab = "Cuantiles teóricos segun poisson",
       ylab = "Cuantiles de número de personas observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()

#Lunes 1

# Aplanar los datos y eliminar NA
conteos <- na.omit(c(data$Lunes.1))
conteos

#Encontrar la media de los datos


lambda <- mean(conteos)
Sd <- sd(conteos)
mediana <- median(conteos)
moda <- as.numeric(names(sort(table(conteos), decreasing = TRUE)[1]))
moda
lambda
mediana
Sd

#Establecer las frecuencias para cada cantidad de personas que llegan a frutanga
observed_frequencies <- table(conteos)
observed_frequencies

#Encontrar las frecuencias esperadas por la distribucion de poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda)
expected_frequencies <- prob * length(conteos)
expected_frequencies

#sumar intervalos para conseguir que todas las fe>5
combined_observed <- c(sum(observed_frequencies[1:2]), observed_frequencies[3:4], sum(observed_frequencies[5:length(observed_frequencies)]))
combined_expected <- c(sum(expected_frequencies[1:2]), expected_frequencies[3:4], sum(expected_frequencies[5:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(sum(prob[1:2]), prob[3:4], sum(prob[5:length(prob)]))

#Tomar en consideracion valores de personas > 12 y agregarlos con el septimo intervalo
combined_prob[4] <- combined_prob[4] + (1-sum(combined_prob))

#verificar que la suma de las probabilidades de 1
sum(combined_prob)
combined_prob

Xiq <- sum(((combined_observed - combined_expected)^2)/combined_expected)

#Prueba Xi cuadrado
result <- pchisq(Xiq, df = length(combined_observed)-1, lower.tail = FALSE)
result

datos<- conteos

# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórrico y el observado
qqplot(x = qpois(ppoints(datos), lambda = mean(datos)),
       y = datos,
       main = "Gráfico QQ para poisson: personas que llegan a frutanga (primer Lunes)",
       xlab = "Cuantiles teóricos segun poisson",
       ylab = "Cuantiles de número de personas observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()

#Martes 1

# Aplanar los datos y eliminar NA
conteos <- na.omit(c(data$Martes.1))
conteos

#Encontrar la media de los datos



lambda <- mean(conteos)
Sd <- sd(conteos)
mediana <- median(conteos)
moda <- as.numeric(names(sort(table(conteos), decreasing = TRUE)[1]))
moda
lambda
mediana
Sd

#Establecer las frecuencias para cada cantidad de personas que llegan a frutanga
observed_frequencies <- table(conteos)
observed_frequencies

#Encontrar las frecuencias esperadas por la distribucion de poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda)
expected_frequencies <- prob * length(conteos)
expected_frequencies

#sumar intervalos para conseguir que todas las fe>5
combined_observed <- c(observed_frequencies[1:4], sum(observed_frequencies[5:length(observed_frequencies)]))
combined_expected <- c(expected_frequencies[1:4], sum(expected_frequencies[5:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(prob[1:4], sum(prob[5:length(prob)]))

#Tomar en consideracion valores de personas > 12 y agregarlos con el septimo intervalo
combined_prob[5] <- combined_prob[5] + (1-sum(combined_prob))

#verificar que la suma de las probabilidades de 1
sum(combined_prob)
combined_prob

Xiq <- sum(((combined_observed - combined_expected)^2)/combined_expected)

#Prueba Xi cuadrado
result <- pchisq(Xiq, df = length(combined_observed)-1, lower.tail = FALSE)
result

datos<- conteos

# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórrico y el observado
qqplot(x = qpois(ppoints(datos), lambda = mean(datos)),
       y = datos,
       main = "Gráfico QQ para poisson: personas que llegan a frutanga (primer Martes)",
       xlab = "Cuantiles teóricos segun poisson",
       ylab = "Cuantiles de número de personas observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()


#Jueves 1


# Leer el archivo CSV
data <- read.csv("data_clean.csv", stringsAsFactors = TRUE, na.strings = "NA")

# Aplanar los datos y eliminar NA
conteos <- na.omit(c(data$Jueves.1))
conteos

#Encontrar la media de los datos

lambda <- mean(conteos)
Sd <- sd(conteos)
mediana <- median(conteos)
moda <- as.numeric(names(sort(table(conteos), decreasing = TRUE)[1]))
moda
lambda
mediana
Sd

#Establecer las frecuencias para cada cantidad de personas que llegan a frutanga
observed_frequencies <- table(conteos)
observed_frequencies

#Encontrar las frecuencias esperadas por la distribucion de poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda)
expected_frequencies <- prob * length(conteos)
expected_frequencies

#sumar intervalos para conseguir que todas las fe>5
combined_observed <- c(observed_frequencies[1:5], sum(observed_frequencies[6:length(observed_frequencies)]))
combined_expected <- c(expected_frequencies[1:5], sum(expected_frequencies[6:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(prob[1:5], sum(prob[6:length(prob)]))

#Tomar en consideracion valores de personas > 12 y agregarlos con el septimo intervalo
combined_prob[6] <- combined_prob[6] + (1-sum(combined_prob))
#verificar que la suma de las probabilidades de 1
sum(combined_prob)
combined_prob

Xiq <- sum(((combined_observed - combined_expected)^2)/combined_expected)

#Prueba Xi cuadrado
result <- pchisq(Xiq, df = length(combined_observed)-1, lower.tail = FALSE)
result

datos<- conteos

# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórrico y el observado
qqplot(x = qpois(ppoints(datos), lambda = mean(datos)),
       y = datos,
       main = "Gráfico QQ para poisson: personas que llegan a frutanga (Jueves)",
       xlab = "Cuantiles teóricos segun poisson",
       ylab = "Cuantiles de número de personas observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()


#Lunes 2

# Aplanar los datos y eliminar NA
conteos <- na.omit(c(data$Lunes.2))
conteos

#Encontrar la media de los datos


lambda <- mean(conteos)
Sd <- sd(conteos)
mediana <- median(conteos)
moda <- as.numeric(names(sort(table(conteos), decreasing = TRUE)[1]))
moda
lambda
mediana
Sd


#Establecer las frecuencias para cada cantidad de personas que llegan a frutanga
observed_frequencies <- table(conteos)
observed_frequencies

#Encontrar las frecuencias esperadas por la distribucion de poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda)
prob
expected_frequencies <- prob * length(conteos)
expected_frequencies

#sumar intervalos para conseguir que todas las fe>5
combined_observed <- c(sum(observed_frequencies[1:2]), observed_frequencies[3:4], sum(observed_frequencies[5:length(observed_frequencies)]))
combined_expected <- c(sum(expected_frequencies[1:2]), expected_frequencies[3:4], sum(expected_frequencies[5:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(sum(prob[1:2]), prob[3:4], sum(prob[5:length(prob)]))
combined_prob
#Tomar en consideracion valores de personas > 12 y agregarlos con el septimo intervalo
combined_prob[4] <- combined_prob[4] + (1-sum(combined_prob))

#verificar que la suma de las probabilidades de 1
sum(combined_prob)
combined_prob

Xiq <- sum(((combined_observed - combined_expected)^2)/combined_expected)

#Prueba Xi cuadrado
result <- pchisq(Xiq, df = length(combined_observed)-1, lower.tail = FALSE)
result #valor p

datos<- conteos

# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórrico y el observado
qqplot(x = qpois(ppoints(datos), lambda = mean(datos)),
       y = datos,
       main = "Gráfico QQ para poisson: personas que llegan a frutanga (Segundo Lunes)",
       xlab = "Cuantiles teóricos segun poisson",
       ylab = "Cuantiles de número de personas observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()


# Analisis en la mañana
conteosLm <- na.omit(c(data$Lunes.1)[1:60])
conteosVm <- na.omit(c(data$Viernes.1)[1:60])
conteosMam <- na.omit(c(data$Martes.1)[1:60])
conteosMim <- na.omit(c(data$Miercoles.1)[1:60])
conteosJm <- na.omit(c(data$Jueves.1)[1:60])
conteosLm2 <- na.omit(c(data$Lunes.2)[1:60])
conteos_morning <- c(conteosVm, conteosLm, conteosMam, conteosMim, conteosJm, conteosLm2)
conteos_morning

lambda_morning <- mean(conteos_morning)
lambda_morning
Sdm <- sd(conteos_morning)
medianam <- median(conteos_morning)
modam <- as.numeric(names(sort(table(conteos_morning), decreasing = TRUE)[1]))
modam
lambda_morning
medianam
Sdm

#Establecer las frecuencias para cada cantidad de personas que llegan a frutanga
observed_frequencies <- table(conteos_morning)
observed_frequencies

#Encontrar las frecuencias esperadas por la distribucion de poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda_morning)
expected_frequencies <- prob * length(conteos_morning)
expected_frequencies

#sumar intervalos para conseguir que todas las fe>5
combined_observed <- c(observed_frequencies[1:6], sum(observed_frequencies[7:length(observed_frequencies)]))
combined_expected <- c(expected_frequencies[1:6], sum(expected_frequencies[7:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(prob[1:6], sum(prob[7:length(prob)]))

#Tomar en consideracion valores de personas > 12 y agregarlos con el septimo intervalo
combined_prob[7] <- combined_prob[7] + (1-sum(combined_prob))

#verificar que la suma de las probabilidades de 1
sum(combined_prob)
combined_prob

Xiq <- sum(((combined_observed - combined_expected)^2)/combined_expected)

#Prueba Xi cuadrado
result <- pchisq(Xiq, df = length(combined_observed)-1, lower.tail = FALSE)
result

datos<- conteos_morning

# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórrico y el observado
qqplot(x = qpois(ppoints(datos), lambda = mean(datos)),
       y = datos,
       main = "Gráfico QQ para poisson: personas que llegan a frutanga (En la mañana)",
       xlab = "Cuantiles teóricos segun poisson",
       ylab = "Cuantiles de número de personas observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()

#Analisis en la tarde
conteosLa <- na.omit(c(data$Lunes.1)[61:120])
conteosVa <- na.omit(c(data$Viernes.1)[61:120])
conteosMaa <- na.omit(c(data$Martes.1)[61:120])
conteosMia <- na.omit(c(data$Miercoles.1)[61:120])
conteosJa <- na.omit(c(data$Jueves.1)[61:120])
conteosLa2 <- na.omit(c(data$Lunes.2)[61:120])
conteos_afternoon <- c(conteosVa, conteosLa, conteosMaa, conteosMia, conteosJa, conteosLa2)
conteos_afternoon

lambda_afternoon <- mean(conteos_afternoon)
Sda <- sd(conteos_afternoon)
medianaa <- median(conteos_afternoon)
modaa <- as.numeric(names(sort(table(conteos_afternoon), decreasing = TRUE)[1]))
modaa
lambda_afternoon
medianaa
Sda

#Establecer las frecuencias para cada cantidad de personas que llegan a frutanga
observed_frequencies <- table(conteos_afternoon)
observed_frequencies

#Encontrar las frecuencias esperadas por la distribucion de poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda_afternoon)
expected_frequencies <- prob * length(conteos_afternoon)
expected_frequencies

#sumar intervalos para conseguir que todas las fe>5
combined_observed <- c(observed_frequencies[1:6], sum(observed_frequencies[7:length(observed_frequencies)]))
combined_expected <- c(expected_frequencies[1:6], sum(expected_frequencies[7:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(prob[1:6], sum(prob[7:length(prob)]))

#Tomar en consideracion valores de personas > 12 y agregarlos con el septimo intervalo
combined_prob[7] <- combined_prob[7] + (1-sum(combined_prob))

#verificar que la suma de las probabilidades de 1
sum(combined_prob)
combined_prob

Xiq <- sum(((combined_observed - combined_expected)^2)/combined_expected)

#Prueba Xi cuadrado
result <- pchisq(Xiq, df = length(combined_observed)-1, lower.tail = FALSE)
result

datos<- conteos_afternoon

# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórrico y el observado
qqplot(x = qpois(ppoints(datos), lambda = mean(datos)),
       y = datos,
       main = "Gráfico QQ para poisson: personas que llegan a frutanga (En la tarde)",
       xlab = "Cuantiles teóricos segun poisson",
       ylab = "Cuantiles de número de personas observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()



#Pruebas de independcia de Ji cuadrado


# Crear vectores con los conteos
# Nota: Ajusta los conteos según los datos que tengas

# Datos de la mañana
conteos_morning <- c(length(na.omit(data$Lunes.1[1:60])),
                     length(na.omit(data$Martes.1[1:60])),
                     length(na.omit(data$Miercoles.1[1:60])),
                     length(na.omit(data$Jueves.1[1:60])),
                     length(na.omit(data$Viernes.1[1:60])))

# Datos de la tarde
conteos_afternoon <- c(length(na.omit(data$Lunes.1[61:120])),
                       length(na.omit(data$Martes.1[61:120])),
                       length(na.omit(data$Miercoles.1[61:120])),
                       length(na.omit(data$Jueves.1[61:120])),
                       length(na.omit(data$Viernes.1[61:120])))

# Crear una tabla de contingencia
tabla_contingencia <- matrix(c(conteos_morning, conteos_afternoon), nrow = 5, byrow = TRUE,
                             dimnames = list(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes"),
                                             c("Mañana", "Tarde")))

# Ver la tabla de contingencia
print(tabla_contingencia)


# Realizar la prueba de Chi-Cuadrado
resultado_chi <- chisq.test(tabla_contingencia)

# Mostrar el resultado
print(resultado_chi)

# Convertir la tabla a un formato adecuado para ggplot
tabla_melt <- melt(tabla_contingencia)

# Crear el mapa de calor
ggplot(data = tabla_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Día de la Semana vs. Momento del Día (clientes de frutanga)",
       x = "Momento del Día", y = "Día de la Semana", fill = "Frecuencia") +
  theme_minimal()