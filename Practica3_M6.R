'***********************(LIMPIAMOS EL ENVIRONMENT)*************************************************'
ls()
rm(list = ls())

'******************* AN�LISI FACTORIAL EN COMPETENCIAS LABORALES **********************************'

'***** 1.- Carga de librer�as y datos *************************************************************'

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


'***** 2.- An�lisis descriptivo ******************************************************************'

cat("\n--- RESUMEN ESTAD�STICO ---\n")
print(summary(variables))

pca_r <- princomp(x = variables, cor = TRUE, scores = TRUE)
summary_pca <- summary(pca_r)
print(summary_pca)

cat("\n--- MATRIZ DE CORRELACIONES ---\n")
cor_matrix <- cor(variables, use = "complete.obs")
print(round(cor_matrix, 2))

## Visualizaci�n de la matriz de correlaciones
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2C7BB6", mid = "white", high = "#D7191C", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlaci�n") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  labs(title = "Matriz de Correlaci�n - Competencias", x = "", y = "")

## Posible descarte de variables con baja correlaci�n
descarte <- apply(cor_matrix, 2, function(x) all(abs(x) < 0.2))
if (any(descarte)) {
  cat("\nLas siguientes variables podr�an no aportar al modelo y podr�an descartarse:\n")
  print(names(descarte[descarte]))
}

factanal(factors = 2, covmat = cov(variables))

'******* 3. Extracci�n de factores **********************************************'

# Determinamos n�mero de factores
valores_propios <- eigen(cor_matrix)$values
factores_kaiser <- sum(valores_propios > 1)

# Evaluaci�n con diferentes n�meros de factores
for (n in 4:5) {
  fa_model <- fa(variables, nfactors = n, fm = "pa", rotate = "none", scores = "regression")
  print(loadings(fa_model), cutoff = 0.4)
}

'************* ( M�TODOS DE ROTACI�N ) ****************************************************'

# Aplicaci�n de diferentes rotaciones y evaluaci�n de cargas factoriales
rotaciones <- c("none","varimax","promax")
n_factores <- c(2, 3, 4, 5, 6)

for (rot in rotaciones) {
  for (n in n_factores) {
    fa_rot <- fa(variables, nfactors = n, fm = "pa", rotate = rot)
    cat("\n--- CARGA FACTORIAL (", rot, ") para", n, "factores ---\n")
    print(fa_rot)  ## Fijamos el umbral en 0.51 para coeficientes
  }
}
## An�lizando la salida del ciclo, determinamos que el modelo m�s �ptimo para fines de la investigaci�n
## el mejor modelo es aquel que considera una rotaci�n con el m�todo "varimax" y  4 factores

'******* 4. Puntuaciones factoriales **********************************************'

fa_varimax <- fa(variables, nfactors = 3, fm = "pa", rotate = "varimax")
scores <- factor.scores(variables, fa_varimax)
cat("\n--- EJEMPLO DE PUNTUACIONES ---\n")
print(head(scores$scores))

'******* 5. Visualizaci�n **********************************************'

# Gr�fico de cargas factoriales
factor.plot(fa_varimax, labels = colnames(variables), main = "Cargas Factoriales (varimax)")

'*****************( RELACI�N DE FACTORES POR M�TODO DE ROTACI�N )***************************'

# Comparaci�n de factores con diferentes m�todos
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

# C�lculo y visualizaci�n de puntuaciones factoriales
scores <- factor.scores(variables, fa_varimax)
cat("\n--- EJEMPLO DE PUNTUACIONES ---\n")
print(scores$scores)
####################################################################################################
###################################################################################################

'***************************** MODELO REDUCIDO ****************************************************'
'***********************(LIMPIAMOS EL ENVIRONMENT)*************************************************'
ls()
rm(list = ls())

'***** 1.- Carga de librer�as y datos *************************************************************'

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


'***** 2.- An�lisis descriptivo ******************************************************************'

cat("\n--- RESUMEN ESTAD�STICO ---\n")
print(summary(variables))

pca_r <- princomp(x = variables, cor = TRUE, scores = TRUE)
summary_pca <- summary(pca_r)
print(summary_pca)

cat("\n--- MATRIZ DE CORRELACIONES ---\n")
cor_matrix <- cor(variables, use = "complete.obs")
print(round(cor_matrix, 2))

## Visualizaci�n de la matriz de correlaciones
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2C7BB6", mid = "white", high = "#D7191C", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlaci�n") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  labs(title = "Matriz de Correlaci�n - Competencias", x = "", y = "")

## Posible descarte de variables con baja correlaci�n
descarte <- apply(cor_matrix, 2, function(x) all(abs(x) < 0.2))
if (any(descarte)) {
  cat("\nLas siguientes variables podr�an no aportar al modelo y podr�an descartarse:\n")
  print(names(descarte[descarte]))
}

factanal(factors = 2, covmat = cov(variables))

'******* 3. Extracci�n de factores **********************************************'

# Determinamos n�mero de factores
valores_propios <- eigen(cor_matrix)$values
factores_kaiser <- sum(valores_propios > 1)

# Evaluaci�n con diferentes n�meros de factores
for (n in 4:5) {
  fa_model <- fa(variables, nfactors = n, fm = "pa", rotate = "none", scores = "regression")
  print(loadings(fa_model), cutoff = 0.4)
}

'************* ( M�TODOS DE ROTACI�N ) ****************************************************'

# Aplicaci�n de diferentes rotaciones y evaluaci�n de cargas factoriales
rotaciones <- c("none","varimax","promax")
n_factores <- c(2, 3, 4, 5, 6)

for (rot in rotaciones) {
  for (n in n_factores) {
    fa_rot <- fa(variables, nfactors = n, fm = "pa", rotate = rot)
    cat("\n--- CARGA FACTORIAL (", rot, ") para", n, "factores ---\n")
    print(fa_rot)  ## Fijamos el umbral en 0.51 para coeficientes
  }
}
## An�lizando la salida del ciclo, determinamos que el modelo m�s �ptimo para fines de la investigaci�n
## el mejor modelo es aquel que considera una rotaci�n con el m�todo "varimax" y  4 factores

'******* 4. Puntuaciones factoriales **********************************************'

fa_varimax <- fa(variables, nfactors = 3, fm = "pa", rotate = "varimax")
scores <- factor.scores(variables, fa_varimax)
cat("\n--- EJEMPLO DE PUNTUACIONES ---\n")
print(head(scores$scores))

'******* 5. Visualizaci�n **********************************************'

# Gr�fico de cargas factoriales
factor.plot(fa_varimax, labels = colnames(variables), main = "Cargas Factoriales (varimax)")

'*****************( RELACI�N DE FACTORES POR M�TODO DE ROTACI�N )***************************'

# Comparaci�n de factores con diferentes m�todos
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

# C�lculo y visualizaci�n de puntuaciones factoriales
scores <- factor.scores(variables, fa_varimax)
cat("\n--- EJEMPLO DE PUNTUACIONES ---\n")
print(scores$scores)

