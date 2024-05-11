# No menu do RStudio: 
# selecione Session > Set Working Directory > Choose Directory...

if (!requireNamespace("plot.matrix", quietly = TRUE)) install.packages("plot.matrix")
if (!requireNamespace("dbscan", quietly = TRUE)) install.packages("dbscan")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")

library(plot.matrix)
library(dbscan)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(stats)
library(pROC)

plotfigure <<- function(row,dataset)
{
  X = NULL
  if(!is.null(nrow(dataset)))
  {
    X = data.frame(matrix(dataset[row,2:785],nrow=28))
  }
  else
  {
    X = data.frame(matrix(dataset[row,2:785],nrow=28))
  }
  m1 = data.matrix(X)
  plot(m1, cex=0.5)
}

#https://www.kaggle.com/datasets/crawford/emnist
#Read the zip codes dataset.
#Each line corresponds to a handwritten figure.
#The first column shows the corresponding symbol.
#The next 256 (16x16) columns correspond to the orange color of each píxel in the fig
dataset <<- read.csv("emnist-balanced-train.csv",sep=",",header = FALSE)
#Function that plots the figure
#corresponding to a specific dataset row
plotfigure(13,dataset)
plotfigure(4,dataset)

# Contar o número de elementos (linhas) no dataset
num_elementos_total <- nrow(dataset)

# Mostrar o número de elementos
print(paste("Número de elementos no dataset:", num_elementos_total))

# Filtar o dataset apenas para os caracteres do meu grupo
label_D <- 13
label_F <- 15

# Filtrar o dataset para conter apenas os registros de "D" e "F"
filtered_dataset <- dataset[dataset$V1 %in% c(label_D, label_F), ]

#criar modelo de árvore de decisão
modelo_arvore <- rpart(V1 ~ ., data = filtered_dataset, method = "class")

#mosrar árvore de decisão
rpart.plot(modelo_arvore, main="Árvore de Decisão - 'D' vs 'F'")

# checks the accuracy of the model
data_test <<- read.csv("emnist-balanced-test.csv",sep=",",header = FALSE)

# Filtrar o data_test para conter apenas os registros de "D" e "F"
filtered_data_test <- data_test[data_test$V1 %in% c(label_D, label_F), ]

# Efetuar a previsão
prediction_probs <- predict(modelo_arvore, newdata = filtered_data_test, type = "prob")[, as.character(label_D)]

# Gerar a curva ROC
roc_obj <- roc(filtered_data_test$V1, prediction_probs)
# Mostrar a curva ROC
plot(roc_obj)
# Encontrar o threshold ótimo usando o critério Youden
coords(roc_obj, "best", ret = "threshold", best.method = "youden")

# Definir o threshold de probabilidade
threshold <- 0.5 # Obtido da curva ROC e arredondado 
# Fica mais bonito e dá o mesmo valor na matriz de confusão

# Converter probabilidades em previsões binárias com base no threshold
predicted_classes <- ifelse(prediction_probs > threshold, label_D, label_F)
# Criar a matriz de confusão
conf_matrix <- table(Predicted = predicted_classes, Actual = filtered_data_test$V1)
# Mostrar a matriz de confusão
conf_matrix_detailed <- confusionMatrix(as.factor(predicted_classes), as.factor(filtered_data_test$V1))
print(conf_matrix_detailed)

# 10 PIXEIS MAIS IMPORTANTES
# Identificar os 10 pixeis mais importantes
importancia <- as.data.frame(modelo_arvore$variable.importance)
names(importancia) <- c("Importância")
importancia <- importancia[order(-importancia$Importância), , drop = FALSE]
pixeis_mais_importantes <- head(importancia, 10)
print(pixeis_mais_importantes)
print(rownames(pixeis_mais_importantes))
# Extrair apenas os números dos nomes dos pixeis mais importantes e convertê-los para valores inteiros
nomes_pixeis_inteiros <- as.integer(gsub("V", "", rownames(pixeis_mais_importantes)))
# Mostrar os nomes dos pixeis mais importantes como valores inteiros
print(nomes_pixeis_inteiros)

# 10 PIXEIS MAIS IMPORTANTES
# Identificar os 10 pixeis mais importantes para a letra D
importancia_D <- as.data.frame(importancia[filtered_dataset$V1 == label_D, ])
names(importancia_D) <- c("Importância")
importancia_D <- importancia_D[order(-importancia_D$Importância), , drop = FALSE]
pixeis_mais_importantes_D <- head(importancia_D, 10)
print(pixeis_mais_importantes_D)
print(rownames(pixeis_mais_importantes_D))

# Mostrar BOXPLOT para os 10 pixeis
# Configuração do tamanho do gráfico e centralização
par(mfrow=c(1,1), mar=c(3,3,1,1))
# Loop para criar os boxplots para cada pixel mais importante
for (pixel in nomes_pixeis_inteiros) {
  # Criar um vetor de rótulos correspondentes a cada linha no dataset
  character_labels <- ifelse(filtered_dataset$V1 == label_D, "D", "F")
  # Plot do boxplot
  boxplot(as.matrix(filtered_dataset[, pixel]) ~ character_labels, 
          main=paste("Distribuição dos Valores dos Pixels para 'D' e 'F' (Pixel: V", pixel, ")"), 
          col=c("red", "blue"), 
          las=2,
          xlab="Letras",
          ylab="Intensidade dos pixeis",
          #outline=FALSE
  )
}

# ****************************************************************************************************************

# Aplicar o DBSCAN
# Definir parâmetros DBSCAN
eps <- 0.5  # Raio de vizinhança
minPts <- 5  # Número mínimo de pontos por cluster
# Agrupar dados
# Aplicar DBSCAN ao filtered_dataset
agrupamentos_dbscan <- dbscan(filtered_dataset[, c(label_D)], eps = eps, minPts = minPts)
# Adicionar rótulos de cluster aos dados originais
filtered_dataset$cluster_dbscan <- agrupamentos_dbscan
# Verificar a distribuição dos clusters
table(filtered_dataset$cluster_dbscan)
# Criar gráfico de dispersão
ggplot(filtered_dataset, aes(x = V2, y = V3, color = cluster_dbscan)) +
  geom_point() +
  labs(title = "Clusters de letras DBSCAN")

# Aplicar o PCA
filtered_dataset_pca <- prcomp(filtered_dataset)
# Ver o resultado do PCA
summary(filtered_dataset_pca)