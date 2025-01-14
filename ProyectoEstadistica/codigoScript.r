# %%
library(ggplot2)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(tibble)
library(plotly)
library(dplyr)
library(reshape2)

data <- read.csv("ds.csv", fileEncoding = "UTF-8")



# %% [markdown]
# # Proyecto Saques guiados de Voleibol: tiro a Hula Hulas

# %% [markdown]
# ## Comparación de valores estadisticos teóricos-prácticos

# %% [markdown]
# Hemos asumido como valor teórico que la probabilidad de éxito sería de **p=0.35**

# %%
#probabilidad de éxito práctica

p_exito <- mean(data$`Éxitos`)/5
p_exito

# %% [markdown]
# Como valor práctico tenemos que la probabilidad de éxito es de **p=0.378**, teniendo en cuenta que el número promedio de éxitos para un intento fue de **1.89**

# %% [markdown]
# **_Veamos cuanto afectaron las variables a nuestra probabilidad de éxito en el experimento!_**

# %% [markdown]
# ## Variable: Tipo de Saque

# %% [markdown]
# ### Cuadro 1

# %%
frecTipoDeSaque <- table(data$Tipo.de.Saque) # Frecuencia de cada tipo de saque
proporcionTipoDeSaque <- prop.table(frecTipoDeSaque) # Proporción de cada tipo de saque


tabla_frecTipoDeSaque <- data.frame(
  Categoria = names(frecTipoDeSaque),
  Proporcion = round(as.vector(proporcionTipoDeSaque), 3)
) # Creación de la tabla

totalTS <- data.frame(Categoria = "**Total**", Proporcion = sprintf("**%.3f**", sum(proporcionTipoDeSaque)))

# Agregar la nueva fila al data.frame original
tabla_frecTipoDeSaque <- rbind(tabla_frecTipoDeSaque, totalTS)

tabla_frecTipoDeSaque

# Diagrama de barras

fig <- plot_ly(
  x = names(proporcionTipoDeSaque),
  y = as.vector(proporcionTipoDeSaque),
  type = 'bar',
  marker = list(color = '#9bdef9')
)
fig <- fig %>% layout(
  title = "Diagrama de barras: Proporción de Tipo de Saque",
  xaxis = list(title = "Tipo de Saque"),
  yaxis = list(title = "Proporción")
)
fig


# %% [markdown]
# Se ha decidido que los intentos fueran distribuidos uniformemente para cada tipo de saque

# %% [markdown]
# Veamos como fue afectada la probabilidad de éxito por cada tipo de saque

# %% [markdown]
# ### Cuadro 2

# %%
#Tabla de frecuencia de éxito por tipo de saque

promedioExitoPorTipoDeSaque <- aggregate(data$`Éxitos`, by = list(data$Tipo.de.Saque), FUN = mean)

promedioExitoPorTipoDeSaque <- data.frame(
  Tipo = promedioExitoPorTipoDeSaque$Group.1,
  Éxitos = promedioExitoPorTipoDeSaque$x
)

frecuenciaExitoPorTipoDeSaque <- (promedioExitoPorTipoDeSaque$Éxitos)/5
frecuenciaExitoPorTipoDeSaque

tabla_frecuenciaExitoPorTipoDeSaque <- data.frame(
  Tipo = promedioExitoPorTipoDeSaque$Tipo,
  Frecuencia = round(frecuenciaExitoPorTipoDeSaque, 3)
)

tabla_frecuenciaExitoPorTipoDeSaque

fig <- plot_ly(
    x = promedioExitoPorTipoDeSaque$Tipo,
    y = frecuenciaExitoPorTipoDeSaque,
    type = 'bar',
    marker = list(color = '#b6cad2'),
    text = round(frecuenciaExitoPorTipoDeSaque, 3),
    textposition = 'outside'
)
fig <- fig %>% layout(
    title = "Frecuencia de Éxito por Tipo de Saque",
    xaxis = list(title = "Tipo de Saque"),
    yaxis = list(title = "Frecuencia de Éxito")
)
fig



# %% [markdown]
# ### Cuadro 3

# %% [markdown]
# Proporcion de probabilidad de cierto número de éxitos en un intento, ejemplo de todos los intentos que se realizó con el tipo de saque: Mano derecha, tan solo en 2 de esos intentos obtuvimos la cantidad de éxitos de 5/5
# 

# %%
# Frecuencia de éxito por tipo de saque

frecuenciaExito <- table(data$Tipo.de.Saque, data$Éxito) # Frecuencia de cada tipo de saque
proporcionExito <- prop.table(frecuenciaExito, margin = 1) # Proporción de cada tipo de saque

proporcionExitoDF <- as.data.frame(proporcionExito)

# Gráfico de barras de frecuencia de éxito por tipo de saque

fig <- plot_ly(proporcionExitoDF, x= ~Var1, y= ~Freq, color= ~Var2, type = 'bar') 
fig <- fig %>% layout(title = "Frecuencia de x éxitos por intento en cada tipo de saque ", xaxis = list(title = "Tipo de saque"), yaxis = list(title = "Frecuencia de éxito"))

fig

#stackbarplot de frecuencia de éxito por tipo de saque

fig <- plot_ly(proporcionExitoDF, x = ~Var1, y = ~Freq, color = ~Var2, type = 'bar', colors = c("#d3d3d3", "#5b9bd5", "#70ad47", "#f1c232", "#e67e22", "#fb0303")) 
fig <- fig %>% layout(barmode = 'stack', title = "Frecuencia de x éxitos por intento en cada tipo de saque (Apilado)", xaxis = list(title = "Tipo de saque"), yaxis = list(title = "Frecuencia de éxito"))
fig

print("Cuadro 3")

# %% [markdown]
# ## Variable: Nivel de Cansancio

# %% [markdown]
# ### Cuadro 4

# %% [markdown]
# Proporción del nivel de cansancio respecto a los intentos realizados

# %%
frecNivelCansancio <- table(data$Nivel.de.Cansancio) # Frecuencia de cada nivel de cansancio
proporcionNivelCansancio <- prop.table(frecNivelCansancio) # Proporción de cada nivel de cansancio

tabla_frecNivelCansancio <- data.frame(
  row.names = NULL,
  Categoria = names(frecNivelCansancio),
  Proporcion = round(as.vector(proporcionNivelCansancio), 3)
) # Creación de la tabla


totalNC <- data.frame(Categoria = "**Total**", Proporcion = sprintf("**%.3f**", sum(proporcionNivelCansancio)))

# Agregar la nueva fila al data.frame original
tabla_frecNivelCansancio <- rbind(tabla_frecNivelCansancio, totalNC)


# %%
View(tabla_frecNivelCansancio)


# %%
fig <- plot_ly(
    x = names(proporcionNivelCansancio),
    y = as.vector(proporcionNivelCansancio),
    type = 'bar',
    marker = list(color = '#9bdef9'),
    text = round(as.vector(proporcionNivelCansancio), 3),
    textposition = 'outside'
)
fig <- fig %>% layout(
    title = "Proporción de Nivel de Cansancio",
    xaxis = list(title = "Nivel de Cansancio"),
    yaxis = list(title = "Proporción")
)
fig

# %% [markdown]
# ### Cuadro 5

# %% [markdown]
# Probabilidad de éxito por nivel de cansancio

# %%
# Tabla de frecuencia de éxito por nivel de cansancio

promedioExitoPorNivelCansancio <- aggregate(data$`Éxitos`, by = list(data$Nivel.de.Cansancio), FUN = mean)

promedioExitoPorNivelCansancio <- data.frame(
    Nivel = promedioExitoPorNivelCansancio$Group.1,
    Éxitos = promedioExitoPorNivelCansancio$x
)

probabilidadExitoPorNivelCansancio <- (promedioExitoPorNivelCansancio$Éxitos)/5


tabla_probabilidadExitoPorNivelCansancio <- data.frame(
    Nivel = promedioExitoPorNivelCansancio$Nivel,
    Frecuencia = round(probabilidadExitoPorNivelCansancio, 3)
)

tabla_probabilidadExitoPorNivelCansancio

fig <- plot_ly(
        x = promedioExitoPorNivelCansancio$Nivel,
        y = probabilidadExitoPorNivelCansancio,
        type = 'bar',
        marker = list(color = '#b6cad2'),
        text = round(probabilidadExitoPorNivelCansancio, 3),
        textposition = 'outside'
)
fig <- fig %>% layout(
        title = "Frecuencia de Éxito por Nivel de Cansancio",
        xaxis = list(title = "Nivel de Cansancio"),
        yaxis = list(title = "Frecuencia de Éxito")
)
fig

# %%
# Prueba de hipótesis para diferencia de proporciones, que tan diferente significativamente son las proporciones de éxito entre los niveles de cansancio

# H0: p1 = p2
# H1: p1 != p2

# Nivel de cansancio alto y medio
# Filtrar los datos para los niveles de cansancio alto y medio
data_alto <- data %>% filter(Nivel.de.Cansancio == "Alto")
data_medio <- data %>% filter(Nivel.de.Cansancio == "Medio")

# Calcular las proporciones de éxito para cada nivel de cansancio
p1 <- mean(data_alto$`Éxitos`) / 5
p2 <- mean(data_medio$`Éxitos`) / 5

# Tamaños de muestra
n1 <- nrow(data_alto)
n2 <- nrow(data_medio)

# Proporción combinada
p_comb <- (sum(data_alto$`Éxitos`) + sum(data_medio$`Éxitos`)) / (5 * (n1 + n2))

# Estadístico de prueba z
z <- (p1 - p2) / sqrt(p_comb * (1 - p_comb) * (1/n1 + 1/n2))

# Valor p para la prueba bilateral
valor_p <- 2 * pnorm(-abs(z))

alpha <- 0.05

# Resultados
cat("Prueba de hipótesis para diferencia de proporciones:\n")
cat("1. Proporción de éxito (Nivel de Cansancio Alto):", p1, "\n")
cat("2. Proporción de éxito (Nivel de Cansancio Medio):", p2, "\n")
cat("3. Estadístico z:", z, "\n")
cat("4. Valor p:", valor_p, "\n")
cat("5. Nivel de significancia:", alpha, "\n")
cat("\n")
# Conclusión

if (valor_p < alpha) {
    cat("Conclusión: Rechazamos la hipótesis nula. Las proporciones de éxito entre los niveles de cansancio alto y medio son significativamente diferentes.\n")
} else {
    cat("Conclusión: No rechazamos la hipótesis nula. No hay evidencia suficiente para afirmar que las proporciones de éxito entre los niveles de cansancio alto y medio son significativamente diferentes.\n")
}





# %% [markdown]
# ## Variable: duracion de intento
# 

# %% [markdown]
# Obtenemos los intervalos a dividir el tiempo de duración de cada intento mediante Sturges

# %%
# Calcular el número de observaciones
n <- length(data$Duración.del.intento) # Número de observaciones

# Calcular el número de intervalos según la regla de Sturges
k <- ceiling(log2(n) + 1)

# Calcular los límites de los intervalos
min_value <- min(data$Duración.del.intento)
max_value <- max(data$Duración.del.intento)

#Calculamos los intervalos
intervals <- seq(min_value, max_value, length.out = k + 1)

intervals #Nos basamos en estos intervalos para crear los intervalos de la tabla de frecuencias
intervalos = c("[15 - 20)", "[20 - 25)", "[25 - 30)", "[30 - 35)", "[35 - 40)", "[40 - 45)", "[45 - 50)", "[50 - 55)", "[55 - 60)")
intervalos
data_intervals <- cut(data$Duración.del.intento, breaks = seq(15, 60, by = 5), right = FALSE, labels = intervalos)
interval_count <- table(data_intervals)
proporciones <- interval_count / sum(interval_count)
tabla_frecDuracion <- data.frame(Intervalo = names(interval_count), Cantidad = as.vector(interval_count), Proporcion = round(proporciones, 3))


# %% [markdown]
# ### Cuadro 6

# %%
histograma_plotly <- plot_ly(
  x = data_intervals, 
  type = "histogram",
  histnorm = "probability", # Normaliza el histograma para mostrar la proporción
  nbinsx = length(intervalos), # Establece el número de intervalos basado en tu conjunto
  name = "Frecuencia",
  marker = list(color = "skyblue", line = list(color = "black", width = 1)), # Agrega margen a las barras
  text = ~round(proporciones, 3), # Agrega las proporciones como texto
  textposition = "auto" # Posiciona el texto automáticamente
) %>% layout(
  title = "Histograma: Proporción de Duración del Intento",
  xaxis = list(title = "Intervalos de Duración del Intento"),
  yaxis = list(title = "Proporcion"), 
  bargap = 0 # Ajusta el espacio entre las barras para que estén unidas
)
histograma_plotly

# %%
boxplot(data$Duración.del.intento, horizontal = TRUE, main = "Box Plot de Duración del Intento", xlab = "Duración del Intento")
points(mean(data$Duración.del.intento), 1, col = "red", pch = 19) # Marca la media
text(mean(data$Duración.del.intento), 1.2, labels = round(mean(data$Duración.del.intento), 2), col = "red") # Etiqueta de la media

fig <- plot_ly(y = data$Duración.del.intento, type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8)
fig <- fig %>% layout(title = "Box Plot de Duración del Intento", xaxis = list(title = "Duración del Intento"))
fig

# %%
library(e1071)  # Cargar la librería e1071 para la función skewness

# Calcular las estadísticas descriptivas
media <- mean(data$Duración.del.intento)
moda <- as.numeric(names(sort(table(data$Duración.del.intento), 
                              decreasing = TRUE)[1]))  # Moda
desviacion_estandar <- sd(data$Duración.del.intento)
sesgo <- skewness(data$Duración.del.intento)  # Calcular el sesgo
minimo <- min(data$Duración.del.intento)
maximo <- max(data$Duración.del.intento)
cuartil1 <- quantile(data$Duración.del.intento, 0.25)
mediana <- median(data$Duración.del.intento)
cuartil3 <- quantile(data$Duración.del.intento, 0.75)

# Crear la tabla con los resultados
tabla_resultados <- data.frame(
  Media = media,
  Moda = moda,
  `Desviación Estándar` = desviacion_estandar,
  Sesgo = sesgo,
  Mínimo = minimo,
  Máximo = maximo,
  `Cuartil 1 (Q1)` = cuartil1,
  Mediana = mediana,
  `Cuartil 3 (Q3)` = cuartil3
)

# Mostrar la tabla
tabla_resultados


# %% [markdown]
# ## 5. Análisis Estadístico Bivariado 

# %% [markdown]
# Distribución conjunta de la variable tipo de saque vs nivel de cansancio

# %% [markdown]
# ### Cuadro 7

# %%
# Frecuencia conjunta de la variable tipo de saque vs nivel de cansancio

frecuenciaConjunta <- table(data$Tipo.de.Saque, data$Nivel.de.Cansancio) 
proporcionConjunta <- prop.table(frecuenciaConjunta, margin = 1) 

# Convertir a dataframe
proporcionConjuntaDF <- as.data.frame(proporcionConjunta)

# tabla de frecuencia conjunta

tabla_frecuenciaConjuntaTS_NC <- as.data.frame.matrix(frecuenciaConjunta)


# Calculo de marginales 

# Marginal de tipo de saque

marginalTS <- margin.table(frecuenciaConjunta, margin = 1) 
marginalTSVector <- as.vector(marginalTS)
marginalTSVectorProporcion <- prop.table(marginalTSVector)
marginalTSVectorProporcion <- round(marginalTSVectorProporcion, 3)
marginalTSVectorProporcionNegrita <- c(sprintf("**%.3f**", marginalTSVectorProporcion))


# Marginal de nivel de cansancio

marginalNC <- margin.table(frecuenciaConjunta, margin = 2)
marginalNCVector <- as.vector(marginalNC)
marginalNCVectorProporcion <- prop.table(marginalNCVector)
marginalNCVectorProporcion <- round(marginalNCVectorProporcion, 3)
marginalNCVectorProporcion



# Añadir a la tabla de frecuencia conjunta

# Añadir fila y columna de marginales a la tabla de frecuencia conjunta

tabla_frecuenciaConjuntaTS_NC <- as.data.frame.matrix(frecuenciaConjunta)

# Añadir columna de marginales
tabla_frecuenciaConjuntaTS_NC$"**Marginal Tipo de Saque**" <- marginalTSVectorProporcionNegrita

# Añadir fila de marginales
marginalNCVectorProporcionNegrita <- c(sprintf("**%.3f**",marginalNCVectorProporcion), sprintf("**%.3f**", sum(marginalNCVectorProporcion)))


tabla_frecuenciaConjuntaTS_NC <- rbind(tabla_frecuenciaConjuntaTS_NC, "**Marginal Nivel de Cansancio**" = marginalNCVectorProporcionNegrita)

tabla_frecuenciaConjuntaTS_NC


# %%
#Tabla de frecuencia de éxito por tipo de saque y nivel de cansancio

# Calcular la frecuencia de éxito por tipo de saque y nivel de cansancio
tablaDeFrecuenciaConjunta_TS_NC <- aggregate(data$`Éxitos`, by = list(data$Tipo.de.Saque, data$Nivel.de.Cansancio), FUN = mean)

# Dividir por 5 para obtener la frecuencia
tablaDeFrecuenciaConjunta_TS_NC$`x` <- tablaDeFrecuenciaConjunta_TS_NC$x / 5

# Renombrar las columnas
colnames(tablaDeFrecuenciaConjunta_TS_NC) <- c("Tipo.de.Saque", "Nivel.de.Cansancio", "Frecuencia.de.éxito")

# Convertir la tabla a formato ancho
tablaDeFrecuenciaConjunta_TS_NC_wide <- dcast(tablaDeFrecuenciaConjunta_TS_NC, Tipo.de.Saque ~ Nivel.de.Cansancio, value.var = "Frecuencia.de.éxito")

# Mostrar la tabla
tablaDeFrecuenciaConjunta_TS_NC_wide

# Gráfico de barras de la frecuencia de éxito por tipo de saque y nivel de cansancio

fig <- plot_ly(tablaDeFrecuenciaConjunta_TS_NC, x = ~Tipo.de.Saque, y = ~Frecuencia.de.éxito, color = ~Nivel.de.Cansancio, type = 'bar', colors = c("#d3d3d3", "#5b9bd5", "#70ad47", "#f1c232", "#e67e22", "#fb0303"))
fig <- fig %>% layout(title = "Frecuencia de éxito por Tipo de Saque y Nivel de Cansancio", xaxis = list(title = "Tipo de Saque"), yaxis = list(title = "Frecuencia de éxito"))
fig


# %% [markdown]
# ### Cuadro 8

# %%
# Crear boxplots que relacionen el tiempo y los niveles de cansancio
boxplot(data$Duración.del.intento ~ data$Nivel.de.Cansancio, 
    main = "Boxplots de Duración del Intento por Nivel de Cansancio",
    xlab = "Nivel de Cansancio",
    ylab = "Duración del Intento",
    col = "lightblue")

# Agregar puntos de media a los boxplots
means <- tapply(data$Duración.del.intento, data$Nivel.de.Cansancio, mean)
points(means, col = "red", pch = 19)
text(x = 1:length(means), y = means, labels = round(means, 2), pos = 3, col = "red")

# %% [markdown]
# ### Cuadro 9

# %%
# Frecuencia conjunta de la variable nivel de cansancio vs intervalos de tiempo

frecuenciaConjuntaNC_TI <- table(data$Nivel.de.Cansancio, data_intervals) 
proporcionConjuntaNC_TI <- prop.table(frecuenciaConjuntaNC_TI, margin = 1) 

# Convertir a dataframe
proporcionConjuntaNC_TIDF <- as.data.frame(proporcionConjuntaNC_TI)

# tabla de frecuencia conjunta

tabla_frecuenciaConjuntaNC_TI <- as.data.frame.matrix(frecuenciaConjuntaNC_TI)


# Calculo de marginales 

# Marginal de nivel de cansancio

marginalNC_TI <- margin.table(frecuenciaConjuntaNC_TI, margin = 1) 
marginalNCVector_TI <- as.vector(marginalNC_TI)
marginalNCVectorProporcion_TI <- prop.table(marginalNCVector_TI)
marginalNCVectorProporcion_TI <- round(marginalNCVectorProporcion_TI, 3)
marginalNCVectorProporcionNegrita_TI <- c(sprintf("**%.3f**", marginalNCVectorProporcion_TI))


# Marginal de intervalos de tiempo

marginalTI <- margin.table(frecuenciaConjuntaNC_TI, margin = 2)
marginalTIVector <- as.vector(marginalTI)
marginalTIVectorProporcion <- prop.table(marginalTIVector)
marginalTIVectorProporcion <- round(marginalTIVectorProporcion, 3)
marginalTIVectorProporcion



# Añadir a la tabla de frecuencia conjunta

# Añadir fila y columna de marginales a la tabla de frecuencia conjunta

tabla_frecuenciaConjuntaNC_TI <- as.data.frame.matrix(frecuenciaConjuntaNC_TI)

# Añadir columna de marginales
tabla_frecuenciaConjuntaNC_TI$"**Marginal Nivel de Cansancio**" <- marginalNCVectorProporcionNegrita_TI

# Añadir fila de marginales
marginalTIVectorProporcionNegrita <- c(sprintf("**%.3f**",marginalTIVectorProporcion), sprintf("**%.3f**", sum(marginalNCVectorProporcion)))


tabla_frecuenciaConjuntaNC_TI <- rbind(tabla_frecuenciaConjuntaNC_TI, "**Marginal Intervalos de Tiempo**" = marginalTIVectorProporcionNegrita)

tabla_frecuenciaConjuntaNC_TI

# %%
# tabla de frecuencia conjunta de la variable nivel de cansancio vs intervalos de tiempo

proporcionConjuntaNC_TI <- prop.table(frecuenciaConjuntaNC_TI)

# Convertir a dataframe
proporcionConjuntaNC_TIDF <- as.data.frame(proporcionConjuntaNC_TI)


# frecuencia de éxito por nivel de cansancio e intervalos de tiempo
tablaDeFrecuenciaConjunta_NC_IT <- aggregate(data$`Éxitos`, by = list(data$Nivel.de.Cansancio, data_intervals), FUN = mean)

#dividir por 5 
tablaDeFrecuenciaConjunta_NC_IT$`x` <- tablaDeFrecuenciaConjunta_NC_IT$x / 5

tablaDeFrecuenciaConjunta_NC_IT

#creacion del heatmap

heatmap <- ggplot(tablaDeFrecuenciaConjunta_NC_IT, aes(x = Group.1, y = Group.2, fill = x)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Frecuencia de éxito por nivel de cansancio e intervalos de tiempo",
    caption = "Fuente: Datos del experimento",
    x = "Nivel de cansancio",
    y = "Intervalos de tiempo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

heatmap





# %% [markdown]
# ### Cuadro 10

# %%
ggplot(data = data, 
    mapping = aes(x = Duración.del.intento, y = `Éxitos`, 
            color = Tipo.de.Saque, shape = as.factor(Nivel.de.Cansancio))) +   
  geom_point() +  
  scale_x_log10() +
  labs(title = "Éxitos, respecto a las 3 variables que influyeron en el experimento",
    caption = "Fuente: Datos del experimento",
    x = "Duración del intento \n (segundos)",
    y = "Número de éxitos",
    shape = "Nivel de cansancio",
    color = "Tipo de saque") +
  facet_wrap(~Tipo.de.Saque)

# %% [markdown]
# ## 6. Estadística Inferencial

# %% [markdown]
# Se tiene una prueba de bondad de ajuste para la variable éxito

# %%
# Generar las frecuencias observadas (en este caso, usando los datos binomiales generados)
frecuencias_observadas <- table(data$`Éxitos`)

# Calcular las frecuencias esperadas para cada número de éxitos (de 0 a n)
valores_k <- 0:5  # Número de éxitos
probabilidades_esperadas <- dbinom(valores_k, size = 5, prob = mean(data$`Éxitos`)/5) # Probabilidades esperadas
frecuencias_esperadas <- probabilidades_esperadas * 300  # Frecuencias esperadas

# Realizar la prueba de Chi-cuadrado de bondad de ajuste
prueba_chi <- chisq.test(frecuencias_observadas, p = probabilidades_esperadas, rescale.p = TRUE)

# Resultados de la prueba de Chi-cuadrado
print(prueba_chi)



# %% [markdown]
# Comenzamos haciendo una prueba de bondad de ajuste 

# %%
# prueba kolmogorov-smirnov para la variable continua de duracion del intento

# pero antes veamos el grafico de la variable continua de duracion del intento

graficoDI <- ggplot(data = data, aes(x = Duración.del.intento)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Densidad de la variable continua de duración del intento",
    caption = "Fuente: Datos del experimento",
    x = "Duración del intento",
    y = "Densidad")
graficoDI
# veamos ahora que distribucion se ajusta mejor a la variable continua de duracion del intento

# Prueba de Kolmogorov-Smirnov
prueba_ks <- ks.test(data$Duración.del.intento, "pnorm", mean = mean(data$Duración.del.intento), sd = sd(data$Duración.del.intento))
prueba_ks




# %%
# Prueba de hipótesis: Bondad de Ajuste
# determinar si la variable aleatoria tiempo, sigue una distribucion normal mediante pruebas de bondad de ajuste, usando pruebas ji cuadrado

# Generar las frecuencias observadas (en este caso, usando los datos binomiales generados)
observed <- table(data_intervals)
observed.df <- as.data.frame(frecuencias_observadas)
observed.df
# Calcular las frecuencias esperadas para cada intervalo de tiempo que seguiria una distribucion normal
valores_k <- 1:9  # Número de intervalos
probabilidades_esperadas <- dnorm(valores_k, mean = mean(frecuencias_observadas.df$Freq), sd = sd(frecuencias_observadas.df$Freq) ) # Probabilidades esperadas
probabilidades_esperadas
frecuencias_esperadas <- probabilidades_esperadas * 300  # Frecuencias esperadas

frecuencias_esperadas.df <- as.data.frame(frecuencias_esperadas)
frecuencias_esperadas.df
# Realizar la prueba de Chi-cuadrado de bondad de ajuste
prueba_chi <- chisq.test(frecuencias_observadas, p = frecuencias_esperadas, rescale.p = TRUE)
prueba_chi

# %% [markdown]
# 

# %%

# Aplanar los datos y eliminar NA
conteos <- na.omit(c(data$`Éxitos`))
conteos

# Encontrar la media de los datos
lambda <- mean(conteos)
Sd <- sd(conteos)
mediana <- median(conteos)
moda <- as.numeric(names(sort(table(conteos), decreasing = TRUE)[1]))
moda
lambda
mediana
Sd

# Perform t-test to get confidence interval
result <- t.test(conteos, conf.level = 0.95, mu = 2.5)

# Display confidence interval
result$conf.int


# Establecer las frecuencias para cada cantidad de éxitos
observed_frequencies <- table(conteos)


# Encontrar las frecuencias esperadas por la distribución de Poisson
prob <- dpois(as.numeric(names(observed_frequencies)), lambda)
expected_frequencies <- prob * length(conteos)


# Sumar intervalos para conseguir que todas las fe > 5
combined_observed <- c(observed_frequencies[1:6], sum(observed_frequencies[7:length(observed_frequencies)]))
combined_expected <- c(expected_frequencies[1:6], sum(expected_frequencies[7:length(expected_frequencies)]))
combined_observed
combined_expected
combined_prob <- c(prob[1:6], sum(prob[7:length(prob)]))

# Tomar en consideración valores de éxitos > 12 y agregarlos con el séptimo intervalo
combined_prob[7] <- combined_prob[7] + (1 - sum(combined_prob))

# Verificar que la suma de las probabilidades sea 1
sum(combined_prob)


Xiq <- sum(((combined_observed - combined_expected)^2) / combined_expected)

# Prueba Chi-cuadrado
result <- pchisq(Xiq, df = length(combined_observed) - 1, lower.tail = FALSE)


datos <- conteos

# Crear un gráfico cuantil cuantil para ver la relación entre el modelo teórico y el observado
qqplot(x = qpois(ppoints(as.numeric(datos)), lambda = mean(datos)),
    y = as.numeric(datos),
    main = "Gráfico QQ: Éxitos en el experimento",
    xlab = "Cuantiles teóricos según Poisson",
    ylab = "Cuantiles de éxitos observados")
abline(a = 0, b = 1, col = "dodgerblue", lwd = 2)
grid()

# %%
muestra <- data$`Éxitos`  


# Parámetro de la hipótesis nula
mu0 <- 1.75  # Hipótesis nula: la prob de exito media como digimos en el experimento es de p=0.35 o sea que acertaria 1.75 veces por intento

# 1. Media de la muestra
media_muestral <- mean(muestra)

# 2. Desviación estándar muestral
desviacion_estandar <- sd(muestra)

# 3. Error estándar de la media
error_estandar <- desviacion_estandar / sqrt(length(muestra))

# 4. Mediana
mediana_muestra <- median(muestra)

# 5. Moda (aproximada usando tabla de frecuencias)
moda_muestra <- as.numeric(names(sort(table(muestra), decreasing = TRUE)[1]))

# 6. Sesgo (aproximado, aunque no es una medida comúnmente usada en este tipo de pruebas)
sesgo_muestra <- (mean(muestra) - mediana_muestra) / desviacion_estandar

# 7. Estadístico de prueba t
estadistico_t <- (media_muestral - mu0) / error_estandar

# 8. Valor p asociado al estadístico de prueba
valor_p <- 2 * pt(-abs(estadistico_t), df = length(muestra) - 1)  # Prueba bilateral

# 9. Intervalo de confianza del 95% para la media
IC_inferior <- media_muestral - qt(0.975, df = length(muestra) - 1) * error_estandar
IC_superior <- media_muestral + qt(0.975, df = length(muestra) - 1) * error_estandar

# Resultados
cat("Resultados de la prueba de hipótesis:\n")
cat("1. Media de la muestra:", media_muestral, "\n")
cat("2. Desviación estándar:", desviacion_estandar, "\n")
cat("3. Error estándar de la media:", error_estandar, "\n")
cat("4. Mediana:", mediana_muestra, "\n")
cat("5. Moda:", moda_muestra, "\n")
cat("6. Sesgo:", sesgo_muestra, "\n")
cat("7. Estadístico t:", estadistico_t, "\n")
cat("8. Valor p:", valor_p, "\n")
cat("9. Intervalo de confianza (95%): [", IC_inferior, ", ", IC_superior, "]\n")

# Conclusión
alpha <- 0.05
if (valor_p < alpha) {
  cat("Conclusión: Rechazamos la hipótesis nula. La media muestral es significativamente diferente de", mu0, ".\n")
} else {
  cat("Conclusión: No rechazamos la hipótesis nula. No hay evidencia suficiente para afirmar que la media muestral es diferente de", mu0, ".\n")
}

# %%
# Prueba de hipótesis para una proporción
# Hipótesis nula (H0): La proporción de intentos con una probabilidad de éxito mayor a 0.378 es igual a 0.5 o sea la mitad
# Hipótesis alternativa (H1): La proporción de intentos con una probabilidad de éxito mayor a 0.378 es diferente de 0.5

# Número de éxitos con probabilidad mayor a 0.378
x <- sum(data$`Éxitos` / 5 > 0.378)

n <- 300  # Tamaño de la muestra
p0 <- 0.5  # Proporción poblacional hipotética
p_hat <- x / n  # Proporción muestral 

# Estadístico de prueba z
z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)

# Valor p para la prueba bilateral
valor_p <- 2 * pnorm(-abs(z))

# Intervalo de confianza del 95% para la proporción
IC_inferior <- p_hat - qnorm(0.975) * sqrt(p_hat * (1 - p_hat) / n)
IC_superior <- p_hat + qnorm(0.975) * sqrt(p_hat * (1 - p_hat) / n)

# Resultados
cat("Prueba de hipótesis para una proporción:\n")
cat("1. Proporción muestral:", p_hat, "\n")
cat("2. Estadístico z:", z, "\n")
cat("3. Valor p:", valor_p, "\n")
cat("4. Intervalo de confianza (95%): [", IC_inferior, ", ", IC_superior, "]\n")

# Conclusión
alpha <- 0.05
if (valor_p < alpha) {
    cat("Conclusión: Rechazamos la hipótesis nula. La proporción de intentos con una probabilidad de éxito mayor a 0.378 es significativamente diferente de 0.5.\n")
} else {
    cat("Conclusión: No rechazamos la hipótesis nula. No hay evidencia suficiente para afirmar que la proporción de intentos con una probabilidad de éxito mayor a 0.378 es diferente de 0.5.\n")
}




# %%
datos <- data$Duración.del.intento

# Media de los datos
media_muestral <- mean(datos)
print(media_muestral)

# Desviación estándar de los datos
desviacion_estandar <- sd(datos)

# Número de observaciones
n <- length(datos)

# Hipótesis nula (mu < mu0), donde mu0 es el valor hipotético de la media
mu0 <- 36 # Un valor ligeramente mayor a la media muestral

# Realizar la prueba t para una media
resultado_prueba <- t.test(datos, mu = mu0, alternative = "less")

# Mostrar los resultados
print(resultado_prueba)

#filtrar Nivel de cansancio por Alto y por Medio
datatestprop2 <- data %>% filter(Nivel.de.Cansancio == "Alto")
datatestprop3 <- data %>% filter(Nivel.de.Cansancio == "Medio")

#prueba de hipotesis de dos medias usando los niveles de cansancio y su respectiva duración de intento
#primero hay que hacer la prueba de las 2 varianzas
resultado_prueba_varianza <- var.test(datatestprop2$Duración.del.intento, datatestprop3$Duración.del.intento)
print(resultado_prueba_varianza)

#prueba de hipotesis de dos medias
resultado_prueba_t <- t.test(datatestprop2$Duración.del.intento, datatestprop3$Duración.del.intento, alternative = "two.sided")
print(resultado_prueba_t)


#prueba para tabla de contingencia (comparar 2 grupos de datos discretos)
#Comparar si el nivel de cansancio depende del tipo de saque
#comparar cansancio y tipo de saque
resultado_prueba_contingencia2 <- chisq.test(data$Nivel.de.Cansancio, data$Tipo.de.Saque)
print(resultado_prueba_contingencia2)


