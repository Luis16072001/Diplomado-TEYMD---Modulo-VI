'***********************(LIMPIAMOS EL ENVIRONMENT)*************************************************'
ls()
rm(list = ls())

'******************* ANÁLISI FACTORIAL EN COMPETENCIAS LABORALES **********************************'

'***** 1.- Carga de librerías y datos *************************************************************'

library(psych)
library(readxl)
library(GPArotation)
library(ggplot2)
library(reshape2)

# Cargamos datos
datos <- read_excel("calif.xlsx", sheet = "califm")
head(datos)
datos <- datos[,2:14]
variables <- datos 


'***** 2.- Análisis descriptivo ******************************************************************'

cat("\n--- RESUMEN ESTADÍSTICO ---\n")
print(summary(variables))

pca_r <- princomp(x = variables, cor = TRUE, scores = TRUE)
summary_pca <- summary(pca_r)
print(summary_pca)

cat("\n--- MATRIZ DE CORRELACIONES ---\n")
cor_matrix <- cor(variables, use = "complete.obs")
print(round(cor_matrix, 2))

## Visualización de la matriz de correlaciones
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2C7BB6", mid = "white", high = "#D7191C", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  labs(title = "Matriz de Correlación - Competencias", x = "", y = "")

## Posible descarte de variables con baja correlación
descarte <- apply(cor_matrix, 2, function(x) all(abs(x) < 0.2))
if (any(descarte)) {
  cat("\nLas siguientes variables podrían no aportar al modelo y podrían descartarse:\n")
  print(names(descarte[descarte]))
}

factanal(factors = 2, covmat = cov(variables))

'******* 3. Extracción de factores **********************************************'

# Determinamos número de factores
valores_propios <- eigen(cor_matrix)$values
factores_kaiser <- sum(valores_propios > 1)

# Evaluación con diferentes números de factores
for (n in 4:5) {
  fa_model <- fa(variables, nfactors = n, fm = "pa", rotate = "none", scores = "regression")
  print(loadings(fa_model), cutoff = 0.4)
}

'************* ( MÉTODOS DE ROTACIÓN ) ****************************************************'

# Aplicación de diferentes rotaciones y evaluación de cargas factoriales
rotaciones <- c("none","varimax","promax")
n_factores <- c(2, 3, 4, 5, 6)

for (rot in rotaciones) {
  for (n in n_factores) {
    fa_rot <- fa(variables, nfactors = n, fm = "pa", rotate = rot)
    cat("\n--- CARGA FACTORIAL (", rot, ") para", n, "factores ---\n")
    print(fa_rot)  ## Fijamos el umbral en 0.51 para coeficientes
  }
}
## Análizando la salida del ciclo, determinamos que el modelo más óptimo para fines de la investigación
## el mejor modelo es aquel que considera una rotación con el método "varimax" y  4 factores

'******* 4. Puntuaciones factoriales **********************************************'

fa_varimax <- fa(variables, nfactors = 3, fm = "pa", rotate = "varimax")
scores <- factor.scores(variables, fa_varimax)
cat("\n--- EJEMPLO DE PUNTUACIONES ---\n")
print(head(scores$scores))

'******* 5. Visualización **********************************************'

# Gráfico de cargas factoriales
factor.plot(fa_varimax, labels = colnames(variables), main = "Cargas Factoriales (varimax)")

'*****************( RELACIÓN DE FACTORES POR MÉTODO DE ROTACIÓN )***************************'

# Comparación de factores con diferentes métodos
fa_varimax <- fa(variables, nfactors = 4, fm = "pa", rotate = "varimax")
fa_promax <- fa(variables, nfactors = 4, fm = "pa", rotate = "promax")
fa_none <- fa(variables, nfactors = 4, fm = "pa", rotate = "none")

par(mfrow=c(1,3))
fa.diagram(fa_varimax, main = "Relaciones entre Factores (Varimax)")
fa.diagram(fa_promax, main = "Relaciones entre Factores (Promax)")
fa.diagram(fa_none, main = "Relaciones entre Factores (None)")

print(loadings(fa_varimax), cutoff = 0.51) 
print(loadings(fa_promax), cutoff = 0.51) 
print(loadings(fa_none), cutoff = 0.51) 

'***************** (SCORES) **************************************'

# Cálculo y visualización de puntuaciones factoriales
scores <- factor.scores(variables, fa_varimax)
cat("\n--- EJEMPLO DE PUNTUACIONES ---\n")
print(scores$scores)
####################################################################################################
###################################################################################################

'***************************** MODELO REDUCIDO ****************************************************'
'***********************(LIMPIAMOS EL ENVIRONMENT)*************************************************'
ls()
rm(list = ls())

'***** 1.- Carga de librerías y datos *************************************************************'

library(psych)
library(readxl)
library(GPArotation)
library(ggplot2)
library(reshape2)

# Cargamos datos
datos <- read_excel("calif.xlsx", sheet = "califm")
head(datos)
datos <- datos[,c(3:8,10:14)]
variables <- datos 


'***** 2.- Análisis descriptivo ******************************************************************'

cat("\n--- RESUMEN ESTADÍSTICO ---\n")
print(summary(variables))

pca_r <- princomp(x = variables, cor = TRUE, scores = TRUE)
summary_pca <- summary(pca_r)
print(summary_pca)

cat("\n--- MATRIZ DE CORRELACIONES ---\n")
cor_matrix <- cor(variables, use = "complete.obs")
print(round(cor_matrix, 2))

## Visualización de la matriz de correlaciones
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2C7BB6", mid = "white", high = "#D7191C", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  labs(title = "Matriz de Correlación - Competencias", x = "", y = "")

## Posible descarte de variables con baja correlación
descarte <- apply(cor_matrix, 2, function(x) all(abs(x) < 0.2))
if (any(descarte)) {
  cat("\nLas siguientes variables podrían no aportar al modelo y podrían descartarse:\n")
  print(names(descarte[descarte]))
}

factanal(factors = 2, covmat = cov(variables))

'******* 3. Extracción de factores **********************************************'

# Determinamos número de factores
valores_propios <- eigen(cor_matrix)$values
factores_kaiser <- sum(valores_propios > 1)

# Evaluación con diferentes números de factores
for (n in 4:5) {
  fa_model <- fa(variables, nfactors = n, fm = "pa", rotate = "none", scores = "regression")
  print(loadings(fa_model), cutoff = 0.4)
}

'************* ( MÉTODOS DE ROTACIÓN ) ****************************************************'

# Aplicación de diferentes rotaciones y evaluación de cargas factoriales
rotaciones <- c("none","varimax","promax")
n_factores <- c(2, 3, 4, 5, 6)

for (rot in rotaciones) {
  for (n in n_factores) {
    fa_rot <- fa(variables, nfactors = n, fm = "pa", rotate = rot)
    cat("\n--- CARGA FACTORIAL (", rot, ") para", n, "factores ---\n")
    print(fa_rot)  ## Fijamos el umbral en 0.51 para coeficientes
  }
}
## Análizando la salida del ciclo, determinamos que el modelo más óptimo para fines de la investigación
## el mejor modelo es aquel que considera una rotación con el método "varimax" y  4 factores

'******* 4. Puntuaciones factoriales **********************************************'

fa_varimax <- fa(variables, nfactors = 3, fm = "pa", rotate = "varimax")
scores <- factor.scores(variables, fa_varimax)
cat("\n--- EJEMPLO DE PUNTUACIONES ---\n")
print(head(scores$scores))

'******* 5. Visualización **********************************************'

# Gráfico de cargas factoriales
factor.plot(fa_varimax, labels = colnames(variables), main = "Cargas Factoriales (varimax)")

'*****************( RELACIÓN DE FACTORES POR MÉTODO DE ROTACIÓN )***************************'

# Comparación de factores con diferentes métodos
fa_varimax <- fa(variables, nfactors = 3, fm = "pa", rotate = "varimax")
fa_promax <- fa(variables, nfactors = 3, fm = "pa", rotate = "promax")
fa_none <- fa(variables, nfactors = 3, fm = "pa", rotate = "none")

par(mfrow=c(1,3))
fa.diagram(fa_varimax, main = "Relaciones entre Factores (Varimax)")
fa.diagram(fa_promax, main = "Relaciones entre Factores (Promax)")
fa.diagram(fa_none, main = "Relaciones entre Factores (None)")

print(loadings(fa_varimax), cutoff = 0.51) 
print(loadings(fa_promax), cutoff = 0.51) 
print(loadings(fa_none), cutoff = 0.51) 

'***************** (SCORES) **************************************'

# Cálculo y visualización de puntuaciones factoriales
scores <- factor.scores(variables, fa_varimax)
cat("\n--- EJEMPLO DE PUNTUACIONES ---\n")
print(scores$scores)

