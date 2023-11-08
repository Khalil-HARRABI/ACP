#importation des packages
library(FactoMineR)
library(factoextra)

library("corrplot")

#importation et preparation de tableau de donnée 

temperature <- read.table("D:/ACP/temperat.csv",header=TRUE, sep=";", dec=".", row.names=1)
head(temperature) # visualisation de la base 


#Extraction des individus et variables actifs:

temperature.active <- temperature[1:30, 1:12]

head(temperature.active)
summary(temperature.active)
apply(temperature.active[,1:12],2,FUN=sd)# calcul des ecart-type

#histogrammes 
for ( i in 1 : 4 )
{ hist ( temperature.active[,i] , main=paste ( " histogramme de composante ",names(
    temperature.active ) [i] ) ) }
M=cor(temperature.active[,1:12]) #matrice des corrélations
heatmap(M) #heatmap pour mieu visualiser les corrélations

#relation entre données
pairs(temperature.active)

ggpairs (temperature.active) + theme_bw ()

#Calculer l'ACP sur les individus/variables actifs:

res.pca <- PCA(temperature.active, scale.unit = TRUE, graph = F)


#Extraction des valeurs propres / variances des composantes principales

eig.val <- get_eigenvalue(res.pca)
eig.val
#valeurpropre=dec a thl on_ acpn $eig ??????2
print(eig.val[ , 1 ] ) # v a l e u r s p r o p r e s
plot ( 1 : 12 , eig.val[ ,1] , xlb=" numéro des axes " , ylab=" valeur
propre " , type="b" )

abline ( h=1) # étude de s v a l e u r s p r o p r e s

#Visualisation des valeurs propres
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

res.ind=res.pca$ind
res.ind$coord
res.pca$ind$cos2 
res.pca$ind$contrib

#graphique des individus
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Colorer par le cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
#fviz_pca_ind (res.pca, pointsize = "cos2",
#              pointshape = 21, fill = "#E7B800",
 #             repel = TRUE # Évite le chevauchement de texte
#)

dimdesc(res.pca, axes=c(1,2))
#graphique des variables
fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)


#Biplot des individus et des variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969"  
)


#Extraction des résultats pour les individus et les variables, respectivement.
var <- get_pca_var(res.pca)

# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

#visualiser le cos2 des variables sur toutes les dimensions
corrplot(var$cos2, is.corr=FALSE)

# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)


corrplot(var$contrib, is.corr=FALSE) 


# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)




res=HCPC(res.pca, graph = FALSE)
fviz_dend(res, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
            rect_border = "jco",           # Rectangle color
            labels_track_height = 0.8      # Augment the room for labels
  )

fviz_cluster(res,repel = TRUE, show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",ggtheme = theme_minimal(),main = "Factor map"
)

#Variables quantitatives décrivant le plus chaque cluster:
res$desc.var$quanti

#Axes principaux associées aux clusters:
res$desc.axes$quanti


#Individus représentatifs de chaque groupe:
res$desc.ind$para



#Données suplemantaires;


temperature.passive<-temperature[25:35,1:12]

res.pca1 <- PCA(temperature.passive, graph=F)




#graphique des individus
fviz_pca_ind(res.pca1,
             col.ind = "cos2", # Colorer par le cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

#graphique des variables
fviz_pca_var(res.pca1,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

#Biplot des individus et des variables
fviz_pca_biplot(res.pca1, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969"  
)





#REGRESSION LINEARE



data <- read.csv2("D:/ACP/trempe.csv") 
head(data)
tail(data)
str(data)
summary(data)
nrow(data)
length(data)

train=data[1:25,]
test=data[26:32,]

attach(train)

model=lm(y~X1+X2+X3+X4)
summary(model)

pred=predict(model,test)

S=0

for( i in 1:20)
{
  print(i)
  print(pred[i])
  S=S+(pred[i]-test$y[i])^2
}
mse=S/19
mse

library(corrplot)
corrplot(cor(data))

library(GGally)
ggpairs(data)

