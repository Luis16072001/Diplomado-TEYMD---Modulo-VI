# Cargar librerías necesarias
library(readxl)
library(cluster)
library(aplpack)
library(tidyr)
library(dplyr)
library(ggplot2)

##############################Aplicación de K-Means#############################
# Cargar la base de datos desde el archivo Excel
datos <- read_excel("C:\\Users\\PC PARROT\\Downloads\\practica01elecciondeoptativasGrupo30_limpio.xlsx", sheet = "Base completa de eleccion")

# Ver las primeras filas de la base de datos
head(datos)

# Transformar los datos para que cada estudiante tenga una fila con sus preferencias
datos_wide <- spread(datos, key = "Orden_de_preferencia", value = "Clave")

# Ver las primeras filas de la base de datos transformada
head(datos_wide)

# Convertimos el dataframe a formato largo
datos_largos <- datos_wide %>%
  pivot_longer(cols = -c(Individuo, Nombre_de_la_Materia), 
               names_to = "Preferencia", values_to = "Clave") %>%
  drop_na()  # Eliminamos los valores NA

# Convertimos la columna "Preferencia" a numérica para ordenar
datos_largos$Preferencia <- as.numeric(datos_largos$Preferencia)
datos_largos$Clave <- as.character(datos_largos$Clave)

# Asignar un número único secuencial a cada materia
materias_unicas <- unique(datos_largos$Clave)  # Obtener todas las materias únicas
codigos_materias <- setNames(1:length(materias_unicas), materias_unicas)  # Asignar un código numérico secuencial

# Reemplazar los nombres de las materias con los códigos numéricos
datos_largos$Clave <- codigos_materias[datos_largos$Clave]

# Convertir a formato wide nuevamente
datos_final <- datos_largos %>%
  arrange(Individuo, Preferencia) %>%
  select(Individuo, Clave, Preferencia) %>%
  pivot_wider(names_from = Preferencia, values_from = Clave) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%  # Reemplazar NA por 0
  mutate(across(-Individuo, as.numeric))

# Ver las primeras filas de la base de datos final
print(datos_final)

# Convertir las preferencias en una matriz numérica
preferencias <- as.matrix(datos_final[, -1])

# Aplicar k-means con 5 grupos
set.seed(123)  # Para reproducibilidad
kmeans_result <- kmeans(preferencias, centers = 5)

# Añadir los grupos a la base de datos
datos_final$Grupo <- kmeans_result$cluster

# Ver los grupos asignados
table(datos_final$Grupo)

# Función para obtener las materias más preferidas por grupo
obtener_materias_preferidas <- function(grupo) {
  grupo_data <- datos_final[datos_final$Grupo == grupo, ]
  materias <- table(unlist(grupo_data[, c("1","2","3","4","5")]))
  materias_ordenadas <- sort(materias, decreasing = TRUE)[1:5]  # Top 5 materias
  nombres_materias <- names(codigos_materias)[match(names(materias_ordenadas), codigos_materias)]
  setNames(materias_ordenadas, nombres_materias)
}

# Obtener las materias preferidas por cada grupo
for (i in 1:5) {
  cat("Materias preferidas del grupo", i, ":\n")
  print(obtener_materias_preferidas(i))
  cat("\n")
}

############################GRFICACMOS EL DENDOGRAMA############################
# Calcular la distancia euclidiana
distancias <- dist(preferencias)

# Aplicar el método jerárquico
hc <- hclust(distancias, method = "ave")

# Graficar el dendrograma
plot(hc, main = "Dendrograma de preferencias de materias", xlab = "", sub = "")
