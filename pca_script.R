#pca discriminando por grupos
data(lig_03)
lig_04<-select(lig_03, -Group)
library(FactoMineR)
PCA(lig_04, scale.unit = TRUE, ncp = 5, graph = TRUE)

library(factoextra)
res.pca <-PCA(lig_04, scale.unit = TRUE, ncp = 5, graph = TRUE)
fviz_eig(res.pca)


groups <- as.factor(lig_03$Group[1:18])
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)
View(groups)


#para sacar las contribuciones
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}

loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 2, var_coord_func, sdev)) 
#error
head(var.coord[, 1:6])