# No menu do RStudio: 
# selecione Session > Set Working Directory > Choose Directory...

if (!requireNamespace("plot.matrix", quietly = TRUE)) install.packages("plot.matrix")
if (!requireNamespace("dbscan", quietly = TRUE)) install.packages("dbscan")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart")
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")

library(plot.matrix)
library(dbscan)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)

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

#contar o número de elementos (linhas) no dataset
num_elementos_total <- nrow(dataset)
# mostrar o número de elementos
print(paste("Número de elementos no dataset:", num_elementos_total))

#filtar o dataset apenas para os caracteres do meu grupo
label_D <- 13
label_F <- 15

#filtrar o dataset para conter apenas os registros de "D" e "F"
filtered_dataset <- dataset[dataset$V1 %in% c(label_D, label_F), ]

#v498, v526, v554

# Agora, somamos os valores de cada coluna para "D" e "F" separadamente
sums_D <- colSums(filtered_dataset[filtered_dataset$V1 == label_D, -1])
sums_F <- colSums(filtered_dataset[filtered_dataset$V1 == label_F, -1])
# Calculamos a diferença absoluta entre as somas
differences <- abs(sums_D - sums_F)
# Encontramos a posição com a maior diferença
max_diff_position <- which.max(differences)
# Imprimimos o resultado
print(max_diff_position)


#código para relatório
#plotfigure(3,filtered_dataset)
#print(filtered_dataset[554:554])
#filtered_dataset[554:554] = 255


#primeiro registro filtrado
plotfigure(1, filtered_dataset)
plotfigure(3, filtered_dataset)

plotfigure(1264, filtered_dataset)
plotfigure(1268, filtered_dataset)

plotfigure(1764, filtered_dataset)
plotfigure(1768, filtered_dataset)

plotfigure(2264, filtered_dataset)
plotfigure(2268, filtered_dataset)

plotfigure(3264, filtered_dataset)
plotfigure(3268, filtered_dataset)

#contar o número de elementos (linhas) no dataset filtrado
num_elementos <- nrow(filtered_dataset)
#mostrar o número de elementos
print(paste("Número de elementos no dataset filtrado:", num_elementos))

#dividir dados em conjunto de treino e teste
set.seed(123) # Para reprodutibilidade
trainIndex <- createDataPartition(filtered_dataset$V1, p = .8, list = FALSE, times = 1)
data_train <- filtered_dataset[trainIndex, ]
data_test <- filtered_dataset[-trainIndex, ]

#criar modelo de árvore de decisão
modelo_arvore <- rpart(V1 ~ ., data = filtered_dataset, method = "class")

#mosrar árvore de decisão
rpart.plot(modelo_arvore, main="Árvore de Decisão - 'D' vs 'F'")

# checks the accuracy of the model
prediction <- predict(modelo_arvore, data_test, type='class')
check <- mean(prediction==filtered_dataset$V1)
check

#identificar o pixel mais importante
importancia <- as.data.frame(modelo_arvore$variable.importance)
names(importancia) <- c("Importância")
importancia <- importancia[order(-importancia$Importância), , drop = FALSE]
print(head(importancia, 35))

#criar um vetor de rótulos correspondentes a cada linha no dataset
character_labels <- ifelse(filtered_dataset$V1 == label_D, "D", "F")

boxplot(as.matrix(filtered_dataset[,499:499]) ~ character_labels, 
        main="Distribuição dos Valores dos Pixels para 'D' e 'F'", 
        col=c("red", "blue"), 
        las=2,
        xlab="Letras",
        ylab="Intensidade dos pixeis",
        #outline=FALSE
)
