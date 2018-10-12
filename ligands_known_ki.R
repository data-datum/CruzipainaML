#pca second run
#select ligands with Ki
library(dplyr)
cr_1<-slice(cr, 1:6)
cr_2<-slice(cr, 14:16)
cr_3<-slice(cr, 9L)
cr_4<-slice(cr, 19L)
cr_5<-slice(cr, 21L)
cr_6<-slice(cr, 23:26)
cr_7<-slice(cr, 31L)
cr_8<-slice(cr, 33L)
#now merge
cr_tot<-dplyr::bind_rows(cr_1, cr_2, cr_3, cr_4, cr_5, cr_6, cr_7, cr_8)

cr_tot_1<-subset(cr_tot, select=-X)
View(cr_tot_1)
#pca with known Ki ligands
library(FactoMineR)
PCA(svm_57, scale.unit = TRUE, ncp = 5, graph = TRUE)
res.pca <- PCA(svm_57, graph = FALSE,scale.unit = TRUE )
print(res.pca)
library(factoextra)
eig.val <- get_eigenvalue(res.pca)
eig.val



fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var
head(var$coord)

fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)


fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = cr_sin_12$X, # color by groups
             addEllipses = FALSE, # Concentration ellipses
             legend.title = "X"
)
