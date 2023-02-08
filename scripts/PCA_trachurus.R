
#---------------------------------------------------------
# Cargar paquetes
#----------------------------------------------------------------------------
library(adegenet)
library(poppr)
library(dplyr)
library(hierfstat)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(genepop)
library(tibble)
library (gridExtra)

#---------------------------------------------------------
# Cargar datos
#----------------------------------------------------------------------------
genind <- read.genepop("data/data.gen", ncode = 3L)
pop <- read.table("data/popmap.txt", sep="\t", stringsAsFactors = TRUE)
ind=as.character(pop$V1) # ID por individuo
site = as.character(pop$V2) # ID por sitio


#---------------------------------------------------------
# PCA and DAPC
#----------------------------------------------------------------------------
#do pca
x = scaleGen(genind, NA.method = "mean")

# Realizar el PCA
pca1 = dudi.pca(x, scannf = FALSE, scale = FALSE, nf = 4)

# Porcentaje de la varianza genética se explica por cada eje
percent = pca1$eig/sum(pca1$eig)*100
barplot(percent, ylab = "Variance explained (%)", ylim = c(0,1.5), 
         xlim = c(0,70), names.arg = round(percent, 1))

# Crear un data.frame con coordenadas individuales
ind_coords = as.data.frame(pca1$li) 
# Renombrar las columnas del dataframe
colnames(ind_coords) = c("Axis1","Axis2","Axis3") 
# Adherir una columna que contiene los individuos
ind_coords$Ind = indNames(genind) 
# Adherir una columna que contiene los sitios
ind_coords$Locality = genind$pop 
# Calcular la posicion del centroide (promedio) por cada población
centroid = aggregate(cbind(Axis1, Axis2, Axis3) ~ Locality, data = ind_coords, FUN = mean)
# Adherir las coordenadas del centroide al dataframe ind_coords
ind_coords = left_join(ind_coords, centroid, by = "Locality", suffix = c("",".cen"))
cols = brewer.pal(nPop(genind), "Set3") # Define la paleta de colores
# Personalizar ejes x e y
xlab = paste("Axis 1 (", format(round(percent[1], 1), nsmall=1)," %)", sep="")
ylab = paste("Axis 2 (", format(round(percent[2], 1), nsmall=1)," %)", sep="")
# Personalizar el tema para ggplot2
ggtheme = theme(axis.text.y = element_text(colour="black", size=12),
                axis.text.x = element_text(colour="black", size=12),
                axis.title = element_text(colour="black", size=12),
                panel.border = element_rect(colour="black", fill=NA, size=1),
                panel.background = element_blank(),
                plot.title = element_text(hjust=0.5, size=15))
# Grafico de dispersion usando eje 1 vs. 2
ggplot(data = ind_coords, aes(x = Axis1, y = Axis2))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  # spider segments
  #geom_segment(aes(xend = Axis1.cen, yend = Axis2.cen, colour = Site), show.legend = F)+
  # Puntos
  geom_point(aes(fill = Locality), shape = 21, size = 3, show.legend = T)+
  # Centroides
  #geom_label(data = centroid, aes(label = Site, fill = Site), size = 4, show.legend = F)+
  # Colorear
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  # Persoanlizar ejes
  labs(x = xlab, y = ylab)+
  ggtitle("")+
  # custom theme
  ggtheme

