# Cargar librerías necesarias
library(dplyr)
library(psych)
library(readxl)

analisis_factorial_varimax <- function(datos, col_categoria, columnas_analisis, desempeno_filtro = c("Excepcional", "Sobresaliente"), n_factores = NULL, guardar_resultados = FALSE, ruta_resultados = "resultados_factorial", cutoff = 0.5) {
  # Filtrar datos por desempeño
  datos_filtrados <- datos[datos$Desempeño %in% desempeno_filtro, ]
  
  # Particionar el dataframe por la columna de categoría
  datos_agrup <- split(datos_filtrados, datos_filtrados[[col_categoria]])
  
  # Crear la carpeta para guardar los resultados (si se solicita)
  if (guardar_resultados && !dir.exists(ruta_resultados)) {
    dir.create(ruta_resultados)
  }
  
  # Lista para almacenar los resultados
  resultados <- list()
  
  # Iterar sobre cada grupo particionado
  for (nombre_grupo in names(datos_agrup)) {
    data <- datos_agrup[[nombre_grupo]]
    
    # Seleccionar las columnas para el análisis factorial
    var_mod <- data[columnas_analisis]
    
    # Verificar que haya suficientes filas válidas
    if (nrow(var_mod) < 2) {
      warning(paste("Grupo", nombre_grupo, "tiene menos de 2 filas válidas. Se omitirá."))
      next  # Saltar a la siguiente iteración
    }
    
    # Aplicar análisis factorial con rotación VARIMAX
    tryCatch({
      # Determinar el número de factores si no se proporciona
      if (is.null(n_factores)) {
        # Usar el método de paralelo para sugerir el número de factores
        paralelo <- fa.parallel(var_mod, fa = "fa", plot = FALSE)
        n_factores <- paralelo$nfact
      }
      
      # Aplicar análisis factorial
      resultado_fa <- fa(var_mod, nfactors = n_factores, rotate = "varimax")
      
      # Extraer resultados importantes
      cargas_factoriales <- resultado_fa$loadings  # Cargas factoriales
      varianza_explicada <- resultado_fa$Vaccounted  # Varianza explicada
      comunalidades <- resultado_fa$communality  # Comunalidades
      
      # Aplicar cutoff a las cargas factoriales
      cargas_factoriales_cutoff <- ifelse(abs(cargas_factoriales) >= cutoff, cargas_factoriales, "NA")
      
      # Guardar resultados en una lista
      resultados[[nombre_grupo]] <- list(
        cargas_factoriales = cargas_factoriales_cutoff,
        varianza_explicada = varianza_explicada,
        comunalidades = comunalidades,
        n_factores = n_factores
      )
      
      # Guardar resultados en archivos (si se solicita)
      if (guardar_resultados) {
      nombre_archivo <- file.path(ruta_resultados, paste0("resultados_", nombre_grupo, ".txt"))
      capture.output(
        print(paste("Resultados para el grupo:", nombre_grupo)),
        print("Cargas factoriales (cutoff >= 0.5):"),
        print(cargas_factoriales_cutoff),
        print("Varianza explicada:"),
        print(varianza_explicada),
        print("Comunalidades:"),
        print(comunalidades),
        file = nombre_archivo
      )
    }
    }, error = function(e) {
      warning(paste("Error en el análisis factorial para el grupo", nombre_grupo, ":", e$message))
    })
  }
  
  # Devolver la lista de resultados
  return(resultados)
}

datos <- read_excel("C:\\Users\\PC PARROT\\Downloads\\Competencias y Desempeño.xlsx", sheet = "COMPETENCIAS Y DESEMPEÑO")

resultados <- analisis_factorial_varimax(
  datos = datos,
  col_categoria = "Segmento 2",  # Columna para segmentar
  columnas_analisis = c(13:83),
  desempeno_filtro = c("Excepcional", "Sobresaliente"),  # Filtro de desempeño
  n_factores = NULL,  # Determinar automáticamente
  guardar_resultados = TRUE,  # Guardar resultados en archivos
  ruta_resultados = "C:\\Users\\PC PARROT\\Downloads\\Resultados Factorial",
  cutoff = 0.5  # Aplicar cutoff de 0.5
)

# Ver los resultados
print(resultados)


resultados[["LIDERES"]][["comunalidades"]]

# Filtra los valores menores a 0.8
Lideres_F <- resultados[["LIDERES"]][["comunalidades"]][resultados[["LIDERES"]][["comunalidades"]] < 0.8]
Lideres_OC <- resultados[["Operativo - C"]][["comunalidades"]][resultados[["Operativo - C"]][["comunalidades"]] < 0.8]
Lideres_OCF <- resultados[["Operativo - CF"]][["comunalidades"]][resultados[["Operativo - CF"]][["comunalidades"]] < 0.8]
Lideres_T120 <- resultados[["Top 120"]][["comunalidades"]][resultados[["Top 120"]][["comunalidades"]] < 0.8]
Lideres_T60 <- resultados[["Top 60"]][["comunalidades"]][resultados[["Top 60"]][["comunalidades"]] < 0.8]

# Mostrar resultados
valores_filtrados

Lideres_OC

Lideres_OCF

Lideres_T120

Lideres_T60
