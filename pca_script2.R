#pca script
#borrar la columna de nombres para tener una matriz de datos, no un dataframe
library(dplyr)
svm_92_<-select(svm_92, -name)
library(FactoMineR)
#plotea los scores y las variables por separado, con el grafico por defecto
svm_92<-PCA(svm_92_, scale.unit = TRUE, ncp = 5, graph = TRUE)
#tuneamos los graficos
#loadings los 20 y los 10 mas importantes
plot(svm_92, axes=c(1,2), choix="var", cex=0.6, col.var="blue", select="contrib 20", legend=list(bty="y", x="x"))
plot(svm_92, axes=c(1,2), choix="var", cex=0.9, col.var="red", select="contrib 10")
#individuos
groups <- as.factor(lig_03$Group[1:18])
View(groups)
library(factoextra)
fviz_pca_ind(svm_92,
             col.ind = class, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel=TRUE
)

#gapped cluster heatmap de ligandos
#calculate the distance matrix
library(gapmap)
distxy <- dist(svm_92_)
hc <- hclust(distxy)
dend <- as.dendrogram(hc)
gapmap(m = as.matrix(distxy), d_row= rev(dend), d_col=dend)

#heatmap de variables
library(corrplot)
M<-cor(svm_92_)
corrplot(M, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.2)
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
corrplot(M, order = "hclust", addrect = 2, col = col2(50), cl.pos = "n", tl.pos = "n", tl.srt = 45,tl.cex = 0.5)
#con rotulo
corrplot(M, order = "hclust", addrect = 2, col = col2(50), tl.cex = 0.4)
corrplot(M, method = "ellipse", tl.cex = 0.1, addrect=2)
corrplot(M, method = "square", tl.cex = 0.3) #esta es la mejor visualizacion
corrplot(M, method = "shade", tl.cex = 0.3)
corrplot(M, method = "color", tl.cex = 0.3)

#crear un vector con los loadings y que solo aparezcan esos casos
