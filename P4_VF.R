############# CLUSTER PARA DEFINIR LAS AGRUPACIONES DE LOS ESTADOS #############

# Instalar paquetes necesarios
#install.packages(c("readxl", "factoextra", "cluster", "tidyverse"))

# Cargar librerías
library(readxl)
library(factoextra)
library(cluster)
library(tidyverse)

# 1. Leer datos desde el archivo Excel
datos <- read_excel("C:\\Users\\PC PARROT\\Downloads\\estados.xlsx", sheet = "Normalizado")

# 2. Preparar datos (eliminar columna de texto y escalar)
datos_num <- datos %>% 
  select(-1) %>%  # Eliminar columna de estados
  scale() %>%     # Estandarizar variables
  as.data.frame()

# 3. Determinar número óptimo de clusters (Método del codo)
fviz_nbclust(datos_num, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(title = "Método del Codo")

# 4. Aplicar K-means (usando 3 clusters como ejemplo)
set.seed(123)  # Para reproducibilidad
km_result <- kmeans(datos_num, centers = 3, nstart = 25)

# 5. Visualizar clusters
fviz_cluster(km_result, data = datos_num,
             palette = "Set2", 
             ggtheme = theme_minimal(),
             main = "Clusters de Estados Mexicanos")

# 6. Ver resultados por estado
resultados <- datos %>%
  mutate(Cluster = km_result$cluster) %>%
  arrange(Cluster)

# Mostrar tabla con clusters asignados
print(resultados)

# 7. Analizar centroides de los clusters
centroides <- as.data.frame(km_result$centers) %>%
  mutate(Cluster = row_number()) %>%
  gather(Variable, Valor, -Cluster)

ggplot(centroides, aes(x = Variable, y = Valor, fill = factor(Cluster))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Cluster) +
  coord_flip() +
  labs(title = "Perfiles de los Clusters", 
       subtitle = "Valores estandarizados",
       fill = "Cluster") +
  theme_minimal()

# Obtener estadísticas descriptivas por cluster
summary_clusters <- resultados %>%
  select(-1) %>%  # Eliminar columna de estados
  group_by(Cluster) %>%
  summarise(across(everything(), 
                   list(Media = mean, 
                        Desviación = sd, 
                        Mínimo = min, 
                        Máximo = max,
                        Mediana = median)))

# Mostrar el resumen con 2 decimales
options(pillar.sigfig = 4)
print(summary_clusters, n = Inf)

########################## ANALISIS DE DISCRIMINANTE ###########################

# 8. Análisis Discriminante Lineal (LDA)
library(MASS)

# Crear dataframe con datos escalados y clusters
datos_lda <- datos_num %>% 
  mutate(Cluster = factor(km_result$cluster))  # Convertir clusters a factor

# Aplicar LDA
modelo_lda <- lda(Cluster ~ ., data = datos_lda)

# Mostrar proporción de varianza explicada
prop_var <- modelo_lda$svd^2 / sum(modelo_lda$svd^2)
cat("Proporción de varianza explicada por cada función discriminante:\n")
print(prop_var)

# Visualizar discriminantes
lda_scores <- predict(modelo_lda)$x %>% 
  as.data.frame() %>% 
  mutate(Cluster = datos_lda$Cluster,
         Estado = datos$Estado)

# Definir colores personalizados
colores_clusters <- c("1" = "#63c5a7", "2" = "#ff8a5c", "3" = "#8a9fd2")

# Crear el gráfico
ggplot(lda_scores, aes(x = LD1, y = LD2, color = as.factor(Cluster), label = Estado)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = colores_clusters) +  # Aplicar colores personalizados
  theme_minimal() +
  labs(title = "Análisis Discriminante Lineal",
       subtitle = "Visualización de los dos primeros componentes",
       x = "Primera función discriminante (LD1)",
       y = "Segunda función discriminante (LD2)",
       color = "Cluster")  # Etiqueta de la leyenda de colores

# Mostrar coeficientes de las funciones discriminantes
cat("\nCoeficientes de las funciones discriminantes:\n")
print(modelo_lda$scaling)

# Matriz de clasificación
predicciones <- predict(modelo_lda)
cat("\nMatriz de confusión:\n")
print(table(Predicho = predicciones$class, Real = datos_lda$Cluster))
