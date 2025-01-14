# %%
library(ggplot2)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(tibble)
library(plotly)
library(dplyr)
library(reshape2)

data <- read.csv("ProyectoEstadistica/ds.csv", fileEncoding = "UTF-8")

head(data)


# %%


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
# ### CUADRO 1

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

# %%
#Tabla de probabilidad de éxito por tipo de saque

promedioExitoPorTipoDeSaque <- aggregate(data$`Éxitos`, by = list(data$Tipo.de.Saque), FUN = mean)

promedioExitoPorTipoDeSaque <- data.frame(
  Tipo = promedioExitoPorTipoDeSaque$Group.1,
  Éxitos = promedioExitoPorTipoDeSaque$x
)

probabilidadExitoPorTipoDeSaque <- (promedioExitoPorTipoDeSaque$Éxitos)/5
probabilidadExitoPorTipoDeSaque

tabla_probabilidadExitoPorTipoDeSaque <- data.frame(
  Tipo = promedioExitoPorTipoDeSaque$Tipo,
  Probabilidad = round(probabilidadExitoPorTipoDeSaque, 3)
)

tabla_probabilidadExitoPorTipoDeSaque

fig <- plot_ly(
    x = promedioExitoPorTipoDeSaque$Tipo,
    y = probabilidadExitoPorTipoDeSaque,
    type = 'bar',
    marker = list(color = '#b6cad2'),
    text = round(probabilidadExitoPorTipoDeSaque, 3),
    textposition = 'outside'
)
fig <- fig %>% layout(
    title = "Probabilidad de Éxito por Tipo de Saque",
    xaxis = list(title = "Tipo de Saque"),
    yaxis = list(title = "Probabilidad de Éxito")
)
fig


# %% [markdown]
# Proporcion de probabilidad de cierto número de éxitos en un intento, ejemplo de todos los intentos que se realizó con el tipo de saque: Mano derecha, tan solo en 2 de esos intentos obtuvimos la cantidad de éxitos de 5/5
# 

# %%
#Probabilidad de éxito por tipo de saque

probabilidadExito <- table(data$Tipo.de.Saque, data$Éxito) # Frecuencia de cada tipo de saque
proporcionExito <- prop.table(probabilidadExito, margin = 1) # Proporción de cada tipo de saque

proporcionExitoDF <- as.data.frame(proporcionExito)

# Gráfico de barras de probabilidad de éxito por tipo de saque

fig <- plot_ly(proporcionExitoDF, x= ~Var1, y= ~Freq, color= ~Var2, type = 'bar') 
fig <- fig %>% layout(title = "Probabilidad de x éxitos por intento en cada tipo de saque ", xaxis = list(title = "Tipo de saque"), yaxis = list(title = "Probabilidad de éxito"))

fig

#stackbarplot de probabilidad de éxito por tipo de saque

fig <- plot_ly(proporcionExitoDF, x = ~Var1, y = ~Freq, color = ~Var2, type = 'bar', colors = c("#d3d3d3", "#5b9bd5", "#70ad47", "#f1c232", "#e67e22", "#fb0303")) 
fig <- fig %>% layout(barmode = 'stack', title = "Probabilidad de x éxitos por intento en cada tipo de saque (Apilado)", xaxis = list(title = "Tipo de saque"), yaxis = list(title = "Probabilidad de éxito"))
fig

print("Cuadro 3")

# %%



# %% [markdown]
# ### Variable: Nivel de Cansancio

# %% [markdown]
# ### Cuadro 2

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
# Probabilidad de éxito por nivel de cansancio

# %%
# Tabla de probabilidad de éxito por nivel de cansancio

promedioExitoPorNivelCansancio <- aggregate(data$`Éxitos`, by = list(data$Nivel.de.Cansancio), FUN = mean)

promedioExitoPorNivelCansancio <- data.frame(
    Nivel = promedioExitoPorNivelCansancio$Group.1,
    Éxitos = promedioExitoPorNivelCansancio$x
)

probabilidadExitoPorNivelCansancio <- (promedioExitoPorNivelCansancio$Éxitos)/5
probabilidadExitoPorNivelCansancio

tabla_probabilidadExitoPorNivelCansancio <- data.frame(
    Nivel = promedioExitoPorNivelCansancio$Nivel,
    Probabilidad = round(probabilidadExitoPorNivelCansancio, 3)
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
        title = "Probabilidad de Éxito por Nivel de Cansancio",
        xaxis = list(title = "Nivel de Cansancio"),
        yaxis = list(title = "Probabilidad de Éxito")
)
fig

# %% [markdown]
# ## Variable: duracion de intento
# 

# %% [markdown]
# ### Cuadro 3

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


# %%
intervals #Nos basamos en estos intervalos para crear los intervalos de la tabla de frecuencias

# %%
intervalos = c("[15 - 20)", "[20 - 25)", "[25 - 30)", "[30 - 35)", "[35 - 40)", "[40 - 45)", "[45 - 50)", "[50 - 55)", "[55 - 60)")
intervalos

# %%
data_intervals <- cut(data$Duración.del.intento, breaks = seq(15, 60, by = 5), right = FALSE, labels = intervalos)
interval_count <- table(data_intervals)
proporciones <- interval_count / sum(interval_count)
tabla_frecDuracion <- data.frame(Intervalo = names(interval_count), Cantidad = as.vector(interval_count), Proporcion = round(proporciones, 3))


# %%
data_intervals

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
  yaxis = list(title = "Proporción"),
  bargap = 0 # Ajusta el espacio entre las barras para que estén unidas
)
histograma_plotly

# %%
boxplot(data$Duración.del.intento, horizontal = TRUE, main = "Box Plot de Duración del Intento", xlab = "Duración del Intento")
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


# %%


# %% [markdown]
# ## 5. Análisis Estadístico Bivariado 

# %% [markdown]
# Distribución conjunta de la variable tipo de saque vs nivel de cansancio

# %% [markdown]
# ### Cuadro 4

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
#Tabla de probabilidad de éxito por tipo de saque y nivel de cansancio

# Calcular la probabilidad de éxito por tipo de saque y nivel de cansancio
tablaDeProbabilidadConjunta_TS_NC <- aggregate(data$`Éxitos`, by = list(data$Tipo.de.Saque, data$Nivel.de.Cansancio), FUN = mean)

# Dividir por 5 para obtener la probabilidad
tablaDeProbabilidadConjunta_TS_NC$`x` <- tablaDeProbabilidadConjunta_TS_NC$x / 5

# Renombrar las columnas
colnames(tablaDeProbabilidadConjunta_TS_NC) <- c("Tipo.de.Saque", "Nivel.de.Cansancio", "Probabilidad.de.éxito")

# Convertir la tabla a formato ancho
tablaDeProbabilidadConjunta_TS_NC_wide <- dcast(tablaDeProbabilidadConjunta_TS_NC, Tipo.de.Saque ~ Nivel.de.Cansancio, value.var = "Probabilidad.de.éxito")

# Mostrar la tabla
tablaDeProbabilidadConjunta_TS_NC_wide

# Gráfico de barras de la probabilidad de éxito por tipo de saque y nivel de cansancio

fig <- plot_ly(tablaDeProbabilidadConjunta_TS_NC, x = ~Tipo.de.Saque, y = ~Probabilidad.de.éxito, color = ~Nivel.de.Cansancio, type = 'bar', colors = c("#d3d3d3", "#5b9bd5", "#70ad47", "#f1c232", "#e67e22", "#fb0303"))
fig <- fig %>% layout(title = "Probabilidad de éxito por Tipo de Saque y Nivel de Cansancio", xaxis = list(title = "Tipo de Saque"), yaxis = list(title = "Probabilidad de éxito"))
fig


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
# 

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
# tabla de probabilidad conjunta de la variable nivel de cansancio vs intervalos de tiempo

proporcionConjuntaNC_TI <- prop.table(frecuenciaConjuntaNC_TI)

# Convertir a dataframe
proporcionConjuntaNC_TIDF <- as.data.frame(proporcionConjuntaNC_TI)


# probabilidad de exito por nivel de cansancio e intervalos de tiempo
tablaDeProbabilidadConjunta_NC_IT <- aggregate(data$`Éxitos`, by = list(data$Nivel.de.Cansancio, data_intervals), FUN = mean)

#dividirpor 5 
tablaDeProbabilidadConjunta_NC_IT$`x` <- tablaDeProbabilidadConjunta_NC_IT$x/5

tablaDeProbabilidadConjunta_NC_IT

#creacion del heatmap

heatmap <- ggplot(tablaDeProbabilidadConjunta_NC_IT, aes(x = Group.1, y = Group.2, fill = x)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Probabilidad de éxito por nivel de cansancio e intervalos de tiempo",
    caption = "Fuente: Datos del experimento",
    x = "Nivel de cansancio",
    y = "Intervalos de tiempo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

heatmap




# %% [markdown]
# ## 6. Estadística Inferencial

# %% [markdown]
# Comenzamos haciendo una prueba de bondad de ajuste 

# %%
# Prueba de hipótesis: Bondad de Ajuste

# determinar si la variable aleatoria tiempo, sigue una distribucion normal mediante pruebas de bondad de ajuste, usando pruebas ji cuadrado

# intervalos

# Datos observados
observed <- hist(data$Duración.del.intento, breaks = intervals, plot = FALSE)$counts

# Frecuencias esperadas en una distribución normal, el valor esperado es igual a n por p
expected <- dnorm(intervals, mean = mean(data$Duración.del.intento), sd = sd(data$Duración.del.intento)) * n
expected

# Prueba de bondad de ajuste
chi_sq_test <- chisq.test(x = expected, p = expected / sum(expected))





# %%
#necesito que crees un grafico de barras para los valores observados 
#y otro para los valores esperados

# Grafico de barras para los valores observados
barplot(height = observed)
barplot(height = expected)

# %%
# Ahora probemos si sigue una distribución uniforme discreta

# Frecuencias esperadas (teoría uniforme discreta)

expected_uniform <- rep(1 / length(observed), length(observed)) * 
  sum(observed)

expected_uniform
# Prueba de bondad de ajuste
chi_sq_test_uniform <- chisq.test(x = observed, 
  p = expected_uniform / sum(expected_uniform))

# Mostrar resultados
print(chi_sq_test_uniform)


# %%
# Datos observados
observed <- c(50, 30, 20)

# Frecuencias esperadas
expected <- c(40, 40, 20)

# Prueba de bondad de ajuste chi-cuadrado
chi_sq_test <- chisq.test(x = observed, p = expected / sum(expected))

#necesito que les aniadas la curva para ver si se ajusta a la distribucion normal

# Grafico de barras para los valores observados
# Grafico de barras para los valores observados
barplot(height = observed, main = "Valores Observados", col = "skyblue", ylim = c(0, max(c(observed, expected)) + 10))
curve(dnorm(x, mean = mean(data$Duración.del.intento), sd = sd(data$Duración.del.intento)) * sum(observed) * diff(range(observed)) / length(observed), 
    from = min(data$Duración.del.intento), to = max(data$Duración.del.intento), add = TRUE, col = "red")

# Grafico de barras para los valores esperados
barplot(height = expected, main = "Valores Esperados", col = "lightgreen", ylim = c(0, max(c(observed, expected)) + 10))
curve(dnorm(x, mean = mean(data$Duración.del.intento), sd = sd(data$Duración.del.intento)) * sum(expected) * diff(range(expected)) / length(expected), 
    from = min(data$Duración.del.intento), to = max(data$Duración.del.intento), add = TRUE, col = "red")


# %% [markdown]
# 

# %%

