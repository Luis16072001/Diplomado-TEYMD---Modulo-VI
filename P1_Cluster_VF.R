# Cargar librerías necesarias
install.packages("readxl")
install.packages("cluster")
install.packages("aplpack")
library(readxl)
library(cluster)
library(aplpack)

# Cargar la base de datos desde el archivo Excel
datos <- read_excel("C:\\Users\\PC PARROT\\Downloads\\practica01elecciondeoptativasGrupo30.xlsx", sheet = "Base completa de eleccion")

# Ver las primeras filas de la base de datos
head(datos)

# Transformar los datos para que cada estudiante tenga una fila con sus preferencias
install.packages("tidyr")
library(tidyr)
datos_wide <- spread(datos, key = "Orden_de_preferencia", value = "Clave")

# Ver las primeras filas de la base de datos transformada
head(datos_wide)


library(tidyr)
library(dplyr)

# Convertimos el dataframe a formato largo
datos_largos <- datos_wide %>%
  pivot_longer(cols = -c(Individuo, Nombre_de_la_Materia), 
               names_to = "Preferencia", values_to = "Clave") %>%
  drop_na()  # Eliminamos los valores NA

# Convertimos la columna "Preferencia" a numérica para ordenar
datos_largos$Preferencia <- as.numeric(datos_largos$Preferencia)

datos_final <- datos_largos %>%
  arrange(Individuo, Preferencia) %>%
  select(Individuo, Clave, Preferencia) %>%
  pivot_wider(names_from = Preferencia, values_from = Clave) %>%
  mutate(across(everything(), ~replace_na(.x, "0")))%>%  # Reemplazar NA por "0"
  mutate(across(-Individuo, as.numeric))
print(datos_final)


# Convertir las preferencias en una matriz numérica
preferencias <- as.matrix(datos_final[, -1])

# Aplicar k-means con 5 grupos
set.seed(123)  # Para reproducibilidad
kmeans_result <- kmeans(preferencias, centers = 7)

# Añadir los grupos a la base de datos
datos_final$Grupo <- kmeans_result$cluster

# Ver los grupos asignados
table(datos_final$Grupo)

# Aplicar k-means con 5 grupos
set.seed(123)  # Para reproducibilidad
kmeans_result <- kmeans(preferencias, centers = 5)

# Añadir los grupos a la base de datos
datos_wide$Grupo <- kmeans_result$cluster

# Ver los grupos asignados
table(datos_wide$Grupo)
