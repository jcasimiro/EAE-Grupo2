@ -0,0 +1,333 @@
# No menu do RStudio:
# selecione Session > Set Working Directory > Choose Directory...

if (!requireNamespace("plot.matrix", quietly = TRUE))
  install.packages("plot.matrix")
if (!requireNamespace("dbscan", quietly = TRUE))
  install.packages("dbscan")
if (!requireNamespace("ggplot2", quietly = TRUE))
  install.packages("ggplot2")
if (!requireNamespace("rpart", quietly = TRUE))
  install.packages("rpart")
if (!requireNamespace("rpart.plot", quietly = TRUE))
  install.packages("rpart.plot")
if (!requireNamespace("caret", quietly = TRUE))
  install.packages("caret")
if (!requireNamespace("stats", quietly = TRUE))
  install.packages("stats")
if (!requireNamespace("pROC", quietly = TRUE))
  install.packages("pROC")
if (!requireNamespace("e1071", quietly = TRUE))
  install.packages("e1071")
if (!requireNamespace("foreach", quietly = TRUE)) 
  install.packages("foreach")
if (!requireNamespace("doParallel", quietly = TRUE)) 
  install.packages("doParallel")
if (!requireNamespace("doParallel", quietly = TRUE)) 
  install.packages("factoextra")


library(plot.matrix)
library(dbscan)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(stats)
library(pROC)
library(e1071)
library(foreach)
library(doParallel)
library(factoextra)

# Function to show an image
plotfigure <<- function(row, dataset)
{
  X = NULL
  if (!is.null(nrow(dataset)))
  {
    X = data.frame(matrix(dataset[row, 2:785], nrow = 28))
  }
  else
  {
    X = data.frame(matrix(dataset[row, 2:785], nrow = 28))
  }
  m1 = data.matrix(X)
  plot(m1, cex = 0.5)
}

# Read the training and testing dataset
train_data <<-
  read.csv("emnist-balanced-train.csv",
           sep = ",",
           header = FALSE)
test_data <<-
  read.csv("emnist-balanced-test.csv",
           sep = ",",
           header = FALSE)

# Labels of the characters we are going to work on
label_D <- 13
label_F <- 15

# Filter the training and testing datasets to contain only the "D" and "F" records
filtered_train_data <-
  train_data[train_data$V1 %in% c(label_D, label_F),]
filtered_test_data <-
  test_data[test_data$V1 %in% c(label_D, label_F),]

# ***** QUESTION 1 *****

# Create a decision tree on the training data set to obtain the most important
# pixels in separating the letters "D" and "F"
decision_tree_model <-
  rpart(V1 ~ .,
        data = filtered_train_data,
        method = "class",
        cp = 0.005)

# Show the decision tree
rpart.plot(
  decision_tree_model,
  main = "Árvore de Decisão - 'D' vs 'F'",
  cex.sub = 1.5,
  cex.main = 1
)

# Let's perform prediction to check the capability of the decision tree model
prediction_probs <-
  predict(decision_tree_model, newdata = filtered_test_data, type = "prob")[, as.character(label_D)]
roc_obj <- roc(filtered_test_data$V1, prediction_probs)
plot(roc_obj)
coords(roc_obj, "best", ret = "threshold", best.method = "youden")
threshold <- 0.5
predicted_classes <-
  ifelse(prediction_probs > threshold, label_D, label_F)
conf_matrix <-
  table(Predicted = predicted_classes, Actual = filtered_test_data$V1)
conf_matrix_detailed <-
  confusionMatrix(as.factor(predicted_classes),
                  as.factor(filtered_test_data$V1))
print(conf_matrix_detailed)

# Study of descriptive statistics based on two pixels:
#      one corresponding to the root node and the other to one of the leaves

print("Root node:")
root_node <- decision_tree_model$frame[1,]
print(root_node)

par(mfrow = c(1, 1), mar = c(3, 3, 1, 1))
character_labels <-
  ifelse(filtered_train_data$V1 == label_D, "D", "F")
boxplot(
  as.matrix(filtered_train_data[root_node$var]) ~ character_labels,
  main = paste(
    "Distribuição dos Valores dos Pixels para 'D' e 'F' (Nó raiz: ",
    root_node$var,
    ")"
  ),
  col = c("red", "blue"),
  las = 2,
  xlab = "Letras",
  ylab = "Intensidade dos pixeis"
)

print("Parent leaf node:")
leaf_nodes <-
  decision_tree_model$frame[decision_tree_model$frame$var == "<leaf>",]
leaf_indices <- as.numeric(rownames(leaf_nodes))
parent_leaf_indices <- floor(leaf_indices / 2)
parent_leaf_nodes <-
  decision_tree_model$frame[as.character(parent_leaf_indices),]
parent_leaf_node <- parent_leaf_nodes[5,]
print(parent_leaf_node)

boxplot(
  as.matrix(filtered_train_data[parent_leaf_node$var]) ~ character_labels,
  main = paste(
    "Distribuição dos Valores dos Pixels para 'D' e 'F' (Nó folha: ",
    parent_leaf_node$var,
    ")"
  ),
  col = c("red", "blue"),
  las = 2,
  xlab = "Letras",
  ylab = "Intensidade dos pixeis"
)

# ***** QUESTION 2 *****

variable_importance <- decision_tree_model$variable.importance
sorted_importance <- sort(variable_importance, decreasing = TRUE)
top_10_pixels <- names(sorted_importance)[1:10]
print("Os 10 pixeis mais importantes:")
print(top_10_pixels)

# Existem algumas razões pelas quais alguns dos pixels mais importantes podem não ser utilizados na tomada de decisão final da árvore:
# 1. Poda: As árvores de decisão são frequentemente podadas para evitar o sobreajuste e melhorar a generalização. Isso envolve a remoção de nós e ramos menos informativos da árvore. Como resultado, mesmo que um pixel tenha alta importância geral, ele pode ser eliminado durante a poda se sua contribuição para a precisão da árvore não for significativa em um nó específico.
# 2. Efeitos de Interação: A importância de um pixel pode ser influenciada por suas interações com outros pixels. Por exemplo, um pixel pode ter alta importância por si só, mas seu poder preditivo pode ser diminuído quando combinado com outros pixels em um determinado nó de decisão. Nesses casos, a árvore de decisão pode priorizar outros pixels que têm efeitos independentes mais fortes ou interações mais relevantes.
# 3. Divisão de Dados: As árvores de decisão dividem os dados com base nos recursos mais relevantes em cada nó. Se um pixel com alta importância não for o recurso mais informativo para uma divisão específica, ele pode não ser usado nesse nó, mesmo que possa ser valioso em outras partes da árvore.
# 4. Ruído e Redundância: As pontuações de importância atribuídas aos pixels podem não refletir perfeitamente sua verdadeira contribuição para a tarefa de classificação. Se alguns pixels forem altamente correlacionados ou contiverem informações redundantes, a árvore de decisão pode priorizar outros pixels que fornecem informações únicas e mais discriminativas.
# 5. Complexidade do Modelo: A complexidade da árvore de decisão também pode influenciar o uso de pixels importantes. Uma árvore mais simples com menos nós pode não incluir todos os pixels potencialmente importantes, mesmo que eles possam melhorar a precisão em um modelo mais complexo.
# Em resumo, o processo final de tomada de decisão da árvore de decisão é uma interação complexa de vários fatores, incluindo importância da variável, efeitos de interação, estratégias de divisão de dados, considerações de ruído e complexidade do modelo. Embora a métrica de 'importância da variável' forneça uma noção geral da importância do pixel, é crucial considerar esses fatores para entender por que pixels específicos podem não ser usados na árvore de decisão final.

# ***** QUESTION 3 *****

for (pixel in top_10_pixels) {
  boxplot(as.matrix(filtered_train_data[pixel]) ~ character_labels, 
          main=paste("Distribuição dos Valores dos Pixels para 'D' e 'F' (Pixel: ", pixel, ")"), 
          col=c("red", "blue"), 
          las=2,
          xlab="Letras",
          ylab="Intensidade dos pixeis",
          #outline=FALSE
  )
}

# ***** QUESTION 4 *****

filtered_train_data_dbscan <- filtered_train_data

train_data_DF <- filtered_train_data_dbscan[, -1]

dbscan_result <- dbscan(train_data_DF, eps = 0.1, minPts = 2)

filtered_train_data_dbscan$cluster <- dbscan_result$cluster

ggplot(filtered_train_data_dbscan, aes(x = V439, y = V496, color = as.factor(V1))) +
  geom_point() +
  labs(title = "DBSCAN Clustering para as Letras 'D' e 'F'",
       x = "Pixel V439",
       y = "Pixel V496",
       color = "Cluster")

# classify_dbscan_parallel <- function(test_data, train_data, dbscan_model, eps, minPts) {
# 
#   numCores <- detectCores() - 1  
#   cl <- makeCluster(numCores)
#   registerDoParallel(cl)
#   
#   new_clusters <- foreach(i = 1:nrow(test_data), .combine = c, .packages = "dbscan") %dopar% {
#     distances <- apply(train_data, 1, function(row) sum((row - test_data[i, ])^2))
#     neighbors <- which(distances <= eps^2)
#     if (length(neighbors) >= minPts) {
#       cluster_labels <- dbscan_model$cluster[neighbors]
#       cluster_labels <- cluster_labels[cluster_labels != 0] 
#       if (length(cluster_labels) > 0) {
#         as.numeric(names(sort(table(cluster_labels), decreasing = TRUE)[1]))
#       } else {
#         0  
#       }
#     } else {
#       0  
#     }
#   }
#   
#   stopCluster(cl)
#   return(new_clusters)
# }
# 
# filtered_test_data_dbscan <- filtered_test_data
# test_data_DF <- filtered_test_data_dbscan[, -1]
# new_clusters <- classify_dbscan_parallel(test_data_DF, train_data_DF, dbscan_result, 0.1, 2)
# 
# filtered_test_data_dbscan$cluster <- new_clusters
# 
# ggplot(filtered_test_data_dbscan, aes(x = V439, y = V496, color = as.factor(V1))) +
#   geom_point() +
#   labs(title = "DBSCAN Clustering para as Letras 'D' e 'F'",
#        x = "Pixel V439",
#        y = "Pixel V496",
#        color = "Cluster")

# ***** QUESTION 5 *****

train_data_nb <- filtered_train_data[, -1]
train_labels_nb <- as.factor(filtered_train_data$V1)

model_nb <- naiveBayes(train_data_nb, train_labels_nb)

test_data_nb <- filtered_test_data[, -1]
test_labels_nb <- as.factor(filtered_test_data$V1)

predictions_nb <- predict(model_nb, test_data_nb)

conf_matrix_detailed <- confusionMatrix(predictions_nb, test_labels_nb)
print(conf_matrix_detailed)

# ***** QUESTION 6 *****

train_data_pca <- filtered_test_data[, -1]
test_labels_pca <- as.factor(filtered_test_data$V1)

train_data_pca <- train_data_pca[, apply(train_data_pca, 2, var) != 0]
train_data_pca = prcomp(train_data_pca, scale. = TRUE)

#explained_variance <- train_data_pca$sdev^2 / sum(train_data_pca$sdev^2) * 100
#print(paste0("Variância explicada por cada componente: ", round(explained_variance, 2), "%"))
#fviz_eig(train_data_pca, choice=c("variance"), ggtheme = theme_minimal(), title='Variância explicada')

train_data_pca_df <- data.frame(V1 = test_labels_pca, train_data_pca$x)

#names(train_data_pca$center)

decision_tree_model_pca <-
  rpart(V1 ~ .,
        data = train_data_pca_df,
        method = "class",
        cp = 0.01)

rpart.plot(
  decision_tree_model_pca,
  main = "Árvore de Decisão PCA - 'D' vs 'F'",
  cex.sub = 1.5,
  cex.main = 1
)

pca_coeficients <- train_data_pca$rotation
top_pca_pixels <- apply(abs(pca_coeficients), 1, sum)
top_pca_pixels <- sort(top_pca_pixels, decreasing = TRUE)
top_pca_10_pixels <- head(top_pca_pixels, 10)
print(top_pca_10_pixels)
top_pca_10_pixels_labels <- names(top_pca_10_pixels)
print(top_pca_10_pixels_labels)

for (pixel in top_pca_10_pixels_labels) {
  boxplot(as.matrix(filtered_train_data[,pixel]) ~ character_labels, 
          main=paste("Distribuição dos Valores dos Pixels para 'D' e 'F' (Pixel: ", pixel, ")"), 
          col=c("red", "blue"), 
          las=2,
          xlab="Letras",
          ylab="Intensidade dos pixeis",
          #outline=FALSE
  )
}

# ***** QUESTION 7 *****

execute_svm <<- function(train_data, train_labels, test_data, test_labels)
{
  svm_model <- svm(train_data, train_labels, type = 'C-classification', kernel="linear")
  predictions <- predict(svm_model, test_data)
  
  conf_matrix_detailed <- confusionMatrix(predictions, test_labels)
  print(conf_matrix_detailed)
}

train_data_svm <- filtered_train_data[, -1]
train_labels_svm <- as.factor(filtered_train_data$V1)

test_data_svm <- filtered_test_data[, -1]
test_labels_svm <- as.factor(filtered_test_data$V1)

execute_svm(train_data_svm, train_labels_svm, test_data_svm, test_labels_svm)

train_data_svm <- train_data_svm[, apply(train_data_svm, 2, var) != 0]
test_data_svm <- test_data_svm[, apply(test_data_svm, 2, var) != 0]

train_data_svm_pca <- prcomp(train_data_svm, scale. = TRUE)
test_data_svm_pca <- prcomp(test_data_svm, scale. = TRUE)

execute_svm(train_data_svm_pca$x, train_labels_svm, test_data_svm_pca$x, test_labels_svm)