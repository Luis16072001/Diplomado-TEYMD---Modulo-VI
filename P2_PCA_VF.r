# Librerías necesarias
library(readxl)
library(ggplot2)
library(corrplot)

# Cargar los datos
data <- read_excel("/Users/jajimenez/Desktop/Módulo 6/riesgo.xls", sheet = 1, col_names = FALSE)
# Renombrar las columnas de 1 a 12

colnames(data) <- c("country","nacanual", "anticoncep", "abort", "anemia", "atnprenat",
                           "partoasit", "vihsida", "infecun", "nacpormuj", "muertes", "calif")

# Eliminar filas con NA en las variables de interés o imputar valores (según necesidad)
data[,2:11] <- apply(data[,2:11], 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Matriz de varianzas y covarianzas (S)
S <- var(data[,2:11], use = "pairwise.complete.obs")
print(S)

# Matriz de correlaciones (R)
R <- cor(data[,2:11], use = "pairwise.complete.obs")
print(R)

# Visualizar la matriz de correlaciones
# Create the correlation heatmap

# Define a custom color scale: Blue (-1) → White (0) → Red (+1)
# Define a custom color scale: Blue (-1) → White (0) → Red (+1)
col_palette <- colorRampPalette(c("#F9696A", "white", "#7ABC81" ))(200)
corrplot(R, method = "color", type = "upper",
         col = col_palette, tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7)

# Add title
title(main = "Matriz de Correlación (R)", col.main = "black", font.main = 2, cex.main = 1.5)

# Decisión: Trabajar con la matriz R (correlación) porque las variables tienen escalas distintas
pairs(data[,2:11])

# Análisis de Componentes Principales (ACP)
pca_r <- princomp(x = data[,2:11], cor = TRUE, scores = TRUE)
summary_pca <- summary(pca_r)
print(summary_pca)

# Eigenvalores y eigenvectores
eigenvalues <- pca_r$sdev^2
eigenvectors <- pca_r$loadings
print(eigenvalues)
print(eigenvectors)

# Varianza explicada por cada componente y varianza total
var_exp <- eigenvalues / sum(eigenvalues) * 100  # Porcentaje de varianza explicada
var_acumulada <- cumsum(var_exp)  # Varianza acumulada
print(var_exp)
print(var_acumulada)

# Graficar la varianza explicada por los componentes principales
barplot(var_exp, main = "Varianza Explicada por Componente", 
        xlab = "Componentes Principales", ylab = "Porcentaje de Varianza Explicada")

# Selección del número de componentes:
# - Criterio de Kaiser: Mantener componentes con eigenvalores > 1
# - Regla del 70-80%: Elegir suficientes componentes para alcanzar ese umbral
num_componentes <- sum(eigenvalues > 1)
print(paste("Número de componentes seleccionados según Kaiser:", num_componentes))

# Representación gráfica de los componentes principales
pca_data <- as.data.frame(pca_r$scores)

ggplot(pca_data, aes(x = Comp.1, y = Comp.2)) +
  geom_point() +
  labs(x = "Componente Principal 1", y = "Componente Principal 2")

ggplot(pca_data, aes(x = Comp.2, y = Comp.3)) +
  geom_point() +
  labs(x = "Componente Principal 2", y = "Componente Principal 3")

# Interpretación de las cargas de los componentes principales
cargas <- pca_r$loadings[,1:3]  # Primeros dos componentes principales
print(cargas)

# Identificar variables con pesos positivos y negativos en la primera componente
carga_cp1 <- cargas[,1]
print(carga_cp1)

# Correlación entre el primer componente y la calificación de riesgo (V11)
correlacion_cp1_v11 <- cor(pca_data$Comp.1, data$calif)
print(paste("Correlación entre CP1 y la calificación de riesgo:", correlacion_cp1_v11))

# Reflexión sobre el índice de riesgo
# Se puede observar qué variables tienen mayor carga en CP1 y CP2
# para inferir cómo pudo haber sido construido el índice de riesgo


