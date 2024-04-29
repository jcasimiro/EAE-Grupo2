# No menu do RStudio: 
# selecione Session > Set Working Directory > Choose Directory...
library(plot.matrix)
library(dbscan)
library(ggplot2)

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
label_A <- 10
label_C <- 12

#filtrar o dataset para conter apenas os registros de "A" e "C"
filtered_dataset <- dataset[dataset$V1 %in% c(label_A, label_C), ]

#primeiro registro filtrado
plotfigure(6, filtered_dataset)

#contar o número de elementos (linhas) no dataset filtrado
num_elementos <- nrow(filtered_dataset)
#mostrar o número de elementos
print(paste("Número de elementos no dataset filtrado:", num_elementos))

#calcular a mediana para 'A'
mediana_A <- apply(filtered_dataset[filtered_dataset$V1 == label_A, -1], 2, median)
#calcular a mediana para 'C'
mediana_C <- apply(filtered_dataset[filtered_dataset$V1 == label_C, -1], 2, median)
#calcular a variação para 'A'
variancia_A <- apply(filtered_dataset[filtered_dataset$V1 == label_A, -1], 2, var)
#calcular a variação para 'C'
variancia_C <- apply(filtered_dataset[filtered_dataset$V1 == label_C, -1], 2, var)
#calcular a média para 'A'
media_A <- colMeans(filtered_dataset[filtered_dataset$V1 == label_A, -1])
#calcular a média para 'C'
media_C <- colMeans(filtered_dataset[filtered_dataset$V1 == label_C, -1])

#criar um data frame para medianas
df_mediana <- data.frame(Pixel = 1:784, Mediana_A = mediana_A, Mediana_C = mediana_C)
#gráfico de medianas
ggplot(df_mediana, aes(x = Pixel)) +
  geom_line(aes(y = Mediana_A, colour = "A")) +
  geom_line(aes(y = Mediana_C, colour = "C")) +
  labs(title = "Medianas dos Pixels para 'A' e 'C'", y = "Mediana", x = "Pixel") +
  scale_colour_manual("", values = c("A" = "red", "C" = "blue"))

#criar um data frame para variações
df_variancia <- data.frame(Pixel = 1:784, Variancia_A = variancia_A, Variancia_C = variancia_C)
#gráfico de variações
ggplot(df_variancia, aes(x = Pixel)) +
  geom_line(aes(y = Variancia_A, colour = "A")) +
  geom_line(aes(y = Variancia_C, colour = "C")) +
  labs(title = "Variações dos Pixels para 'A' e 'C'", y = "Variação", x = "Pixel") +
  scale_colour_manual("", values = c("A" = "red", "C" = "blue"))

#criar um data frame para médias
df_media <- data.frame(Pixel = 1:784, Media_A = media_A, Media_C = media_C)
#gráfico de médias
ggplot(df_media, aes(x = Pixel)) +
  geom_line(aes(y = Media_A, colour = "A")) +
  geom_line(aes(y = Media_C, colour = "C")) +
  labs(title = "Médias dos Pixels para 'A' e 'C'", y = "Média", x = "Pixel") +
  scale_colour_manual("", values = c("A" = "red", "C" = "blue"))

#analisar e preparar o dataset
column_types <- sapply(filtered_dataset, class)
#mostrar os tipos de dados das colunas
print(column_types)
#verificar se todas as colunas são numéricas
all_numeric <- all(sapply(filtered_dataset, is.numeric))
if(all_numeric) {
  cat("Todas as colunas são numéricas.\n")
} else {
  cat("Nem todas as colunas são numéricas.\n")
}

#análise dos pixeis
#boxplots
#criar um vetor de rótulos correspondentes a cada linha no dataset
character_labels <- ifelse(filtered_dataset$V1 == label_A, "A", "C")

boxplot(as.matrix(filtered_dataset[,2:785]) ~ character_labels, 
        main="Distribuição dos Valores dos Pixels para 'A' e 'C'", 
        col=c("red", "blue"), 
        las=2,
        xlab="Letras",
        ylab="Intensidade dos pixeis",
        #outline=FALSE
        )

#correlação
#calcular a média dos pixels para "A" e "C"
media_A <- colMeans(filtered_dataset[filtered_dataset$V1 == label_A, -1])
media_C <- colMeans(filtered_dataset[filtered_dataset$V1 == label_C, -1])
#calcular a correlação entre as médias de "A" e "C"
correlacao <- cor(media_A, media_C)
# Visualização da correlação em gráfico
# usar ggplot2 para criar um scatter plot das médias
df <- data.frame(Pixel=1:length(media_A), Media_A=media_A, Media_C=media_C)
ggplot(df, aes(x=Media_A, y=Media_C)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  theme_minimal() +
  labs(
    x="Média de Intensidade dos Pixels de 'A'", 
    y="Média de Intensidade dos Pixels de 'C'", 
    title=paste("Correlação entre 'A' e 'C':", round(correlacao, 2)
  )
)

#cálculo de distância para os primeiros 100 exemplos
dist_matrix <- dist(filtered_dataset[1:100, 2:785], method="euclidean")
#realizar clusterização hierárquica
hc <- hclust(dist_matrix)
#gerar heatmap com os dados ordenados pela clusterização hierárquica
heatmap(as.matrix(dist_matrix), Rowv=as.dendrogram(hc), Colv=as.dendrogram(hc))

#dbscan
#remover a coluna do rótulo
data_for_clustering <- filtered_dataset[, -1]
#identificar colunas com variação zero
cols_zero_var <- sapply(data_for_clustering, function(x) var(x, na.rm = TRUE) == 0)
#remover colunas com variação zero
data_for_pca <- data_for_clustering[, !cols_zero_var]
#aplicar a PCA ao conjunto de dados limpo
pca_result <- prcomp(data_for_pca, scale. = TRUE)
#visualizar a variação explicada por cada componente principal
#ajustar as margens: bottom, left, top, right
par(mar=c(1, 1, 1, 1) + 0.1)
plot(pca_result$sdev^2 / sum(pca_result$sdev^2), type='b', xlab="Componente Principal", ylab="Variação Explicada", main="PCA Variação Explicada")
#visualizar os resultados dos primeiros dois componentes principais
plot(pca_result$x[,1], pca_result$x[,2], asp=1, xlab="Primeiro Componente Principal", ylab="Segundo Componente Principal")

#soma cumulativa da proporção de variância explicada
prop_var_explicada <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
#encontrar o número de componentes para explicar a variância desejada. 80%
num_componentes <- which(prop_var_explicada >= 0.8)[1]
print(paste("Número de componentes principais a escolher:", num_componentes))
#obter os dados transformados
pca_data <- pca_result$x[, 1:num_componentes]

#calcular a distância do k-ésimo vizinho mais próximo
k <- 4 # Escolha de k depende do dataset
k_distances <- kNNdist(pca_data, k = k)
#mostrar as distâncias para identificar o 'eps'
kNNdistplot(k_distances, k)
abline(h = 0.05, col = 'red') # Exemplo de linha horizontal para ajudar a escolher 'eps'

#executar o DBSCAN
#eps é o raio do vizinho e minPts é o número mínimo de pontos para formar um cluster denso
dbscan_result <- dbscan(data_for_clustering, eps = 1750, minPts = 5)
# Acessar os rótulos dos clusters
clusters <- dbscan_result$cluster
#quantidade de pontos considerados ruído
cat("Pontos considerados ruído:", sum(clusters == 0), "\n")
#número de clusters encontrados (excluindo ruído)
cat("Número de clusters encontrados:", max(clusters), "\n")
#visualizar os resultados
plot(data_for_clustering[, c(1, 2)], col = clusters + 1L, main = "Resultados do DBSCAN", xlab = "Característica 1", ylab = "Característica 2", pch = 20)
legend("topright", legend = c("Ruído", paste("Cluster", 1:max(clusters))), fill = 1:(max(clusters)+1))
