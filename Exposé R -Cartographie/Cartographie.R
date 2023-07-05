library(dplyr)
library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(raster)
library("igraph")
library(RColorBrewer)


#--- Cas pratique1 1 : Généralités---------------------

setwd("G:/ISEP3/Semestre 6/R")
#Chargement des données
world_sp <- readOGR(dsn = "Donnees/World WGS84",
                    layer = "Pays_WGS84")
world_sf <- st_read("Donnees/World WGS84/Pays_WGS84.shp")

#Renomer les obs
row.names(world_sp) <- as.character(world_sp@data$NOM)
View(world_sp@data)
#Sélectionner les pays de l'uemoa
View(arrange(world_sp@data, NOM))# Voir les noms des pays par ordre alphabétique
#Pour la classe sp
uemoa_sp <- world_sp[c("Benin", "Burkina Faso", "Guinea-Bissau",
                         "Ivory Coast", "Mali",
                         "Niger", "Senegal", "Togo"),]
#Pour la classe sf
#Pour la classe sf, on peut utiliser la même syntaxe que pour sp, mais en plus, on peut utiliser la syntaxe à la mode dplyr :
uemao_sf <- world_sf %>%
  filter(NOM %in% c("Benin", "Burkina Faso", "Guinea-Bissau",
                    "Ivory Coast", "Mali",
                    "Niger", "Senegal", "Togo"))

zmao_sf <- world_sf %>%
  filter(NOM %in% c("Gambia, The", "Ghana", 
      "Guinea", "Liberia", "Nigeria", "Sierra Leone"))

#Convertir un fichier shapefile et json
st_write(zmao_sf, "Donnees/GEOjson/zmao_sf.geojson", driver = "GeoJSON")
st_write(uemao_sf, "Donnees/GEOjson/uemao_sf.geojson", driver = "GeoJSON")

# I. Passage des données des data non spaciale vers des data spaciale -----
## Importer les données Excle
uemoa <- read_excel("Donnees/uemoa/uemoa.xlsx")

## Fonction pour créer les polygones à partir des coordonnées
createPolygon <- function(coords_str) {
  ### Diviser la chaîne de caractères en un vecteur de coordonnées
  coords <- strsplit(coords_str, "\\], \\[")
  
  ## Convertir les coordonnées en nombres
  coords <- lapply(coords, function(coord) {
    coord <- strsplit(coord, ", ")
    coord <- sapply(coord, function(x) as.numeric(x))
    return(coord)
  })
  
  ## Créer un objet Polygon à partir des coordonnées
  coords<- matrix(coords[[1]], ncol = 2, byrow = TRUE)
  polygon <- Polygon(coords)
  
  return(polygon)
}

## Appliquer la fonction createPolygon à chaque élément la variable géométrique 
polygones <- lapply(uemoa$geometry, function(coords_str) {
  polygon <- createPolygon(coords_str)
  return(polygon)
})
## convertir polygones en un objet de la class spacialPolygon
polygones <- SpatialPolygons(list(Polygons(polygones, ID = 1)))

## Création d'un objet SpatialPolygonsDataFrame vide
uemoa.sdf <- SpatialPolygonsDataFrame(polygones, data = data.frame(ID = 1))

## Ajout des données aux attributs du SpatialPolygonsDataFrame
uemoa.sdf@data <- uemoa["NOM"]

## Nous pouvons définir un crs pour notre objet uemoa.sdf
crs <- CRS("+proj=longlat +datum=WGS84")
uemoa.sdf@proj4string <- crs
plot(uemoa.sdf, col="pink")

## II. Des cartes sur R
#Chargement des données du Sénégal
Sen.sp <-readOGR("Donnees/Sen/Limite_Région.shp")
Sen.sf <- read_sf("Donnees/Sen/Limite_Région.shp")
centre <-coordinates(Sen.sp)
noms <-Sen.sp$NOMREG
# Des carte avec ggplot
crs_ue <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m"
ggplot(data = Sen.sf) +geom_sf(aes(fill = H)) + geom_sf_text(data=Sen.sf, aes(label=NOMREG), size=2)+
  coord_sf(crs = st_crs(crs_ue)) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "bl", 
                         which_north = "true",pad_x = unit(0.05, "in"), 
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(title="Incidence de pauvreté multidimentionnelle au Sénégal")

## Discrétion d'une variable quantitative

## Varia

plot(Sen.sp, border = NA, col = "NA", bg = "#A6CAE0")
plot(Sen.sp, col = "#E3DEBF", border = NA, add = T)
plot(Sen.sp, col = "#D1914D", border = "grey80", add = T)
propSymbolsLayer(spdf = Sen.sp, var = "H",inch=0.1 )
title("Incidence de pauvreté multidimentionnelle au Sénégal")
text(centre[,1],centre[,2],noms ,cex=.5)
layoutLayer( sources = "EHCVM(2018),calculs de l'auteur", frame = TRUE,
             col = "NA", scale = NULL)



##--------Cas pratique 2 :Cartographie des incendies

nasa <- raster("Donnees/NASA/FIRMS_24hrs[@7.8,7.9,2z].tif")

## Extraire les pixels 
uemoa.bb <- crop(nasa, extent(bbox(uemoa_sp)))
uemoa.pix <- rasterize(uemoa_sp, uemoa.bb, mask = TRUE)
## Affichange du fond de la carte
plot(uemoa_sp, border = NA, col = "NA", bg = "#A6CAE0")
plot(world_sp, col = "#E3DEBF", border = NA, add = T)
image(uemoa.pix, add = TRUE)
plot(uemoa_sp, col = "NA", add = T)
## Ajouter les noms des pays
text(uemoa_sp, labels = uemoa_sp@data$NOM, cex = 1.3, col = "black", font = 1)
## Ajout de la légende
levels <- c("Low", "Medium", "High")
colors <- c("#FFFFCC", "#FF9900", "#CC0000")
legend("bottomright", legend = levels, fill = colors, title = "Degree", bg = "white")
## Ajouter un titre
title <- "Carte des incendies dans la zone UEMOA"
title(main = title, font.main = 1, cex.main = 1.5)



# ------Cas pratique 3 : Carte interactive et calcul d'itinéraire
library(leaflet)
sen_com = getData("GADM", country = "senegal", level = 3)
## Sélectionner la région de Dakar 
dakar <-  sen_com[sen_com@data$NAME_1 == "Dakar",]
### Centroide des arrondisselents de la région de Dakar
arrondi <- cbind(dakar@data[,c("NAME_3")],
                   as.data.frame(coordinates(dakar)))
names(arrondi)<-c("arrondissement","long","lat")
### Création d'une carte interactive
# Création de la carte interactive
m <- leaflet(dakar) %>%
  addTiles() %>%
  addPolygons() %>%
  addMarkers(data = arrondi[c(2,3)], label = arrondi$arrondissement)

m

### Calcul d'itinéraire
#### La fonction osrmRoute()permet de calculer des itinéraires
library(osrm)
library("maptiles")
Almadies = coordinates(dakar[dakar@data$NAME_3=="Almadies",])
Rufisque = coordinates(dakar[dakar@data$NAME_3=="Rufisque",])
route <- osrmRoute(src = Almadies, 
                   dst = Rufisque)
route <-st_geometry(route)
osm <- get_tiles(route, crop = TRUE)
plot_tiles(osm)
plot(route, col = "grey10", lwd = 6, add = T)
plot(route, col = "grey90", lwd = 1, add = T)

# On peux aussi ajouter l'itinéraire à l'objet m

m <- m %>%
  addPolylines(data = route, color = "grey10", weight = 6) %>%
  addPolylines(data = route, color = "grey90", weight = 1)
# Affichage de la carte
m



#------ Cas pratique 4 : Carte des flux des échanges commerciaux dans la zone uemoa

# extraction des coordonnées des centroides des pays de l'UEMOA
centres <- cbind(uemoa_sp@data[,c("NOM")],
                 as.data.frame(coordinates(uemoa_sp)))
names(centres)<-c("id","X","Y")

# Charger les fichiers des flux des échanges commerciaux entre les pays de l'uemoa de 2010 à 2015
export<- read_excel("Donnees/uemoa/umeoa_echange_commerce_2010_2015.xlsx")
export <-aggregate(fij ~ origine + destination, data = export, FUN = sum, na.rm = TRUE)
# Totale importation par pays
global.import <-aggregate(fij ~ destination, data = export, FUN = sum, na.rm = TRUE)
# Création d'un graphe d'arête orientée entre les différents pays
g <- data.frame(export[,c("origine", "destination")])
g <- as.matrix(g)
g <- graph.edgelist(g, directed=TRUE)
# extraire la liste des noeuds de l'objet g
vert <- data.frame(id = V(g)$name)
# attribuer à chaque noeud des coordonnées géographiques
coords <- centres[match(vert$id, table = centres$id), ]
coords <- as.matrix(coords[,2:3])
g$layout <- coords
# ajouter un attribut poids à chaque noeuds : la valeur des importations totale du pays
V(g)$weights <- global.import[match(vert$id, table = global.import$destination), "fij"]

# ajouter un attribut d'épaisseur aux arêtes (edge) : la valeur des exportation du noeud d'origine vers le noeud de destination
x <- get.edgelist(g) # recupérer la liste d'arête de g
E(g)$fij <- export[match(x = paste(x[,1],x[,2],sep="_"),
                         table = paste(export$origine,export$destination,sep="_")),"fij"]
View(E(g)$fij)
# Définir la palette de couleurs
color.palette <- colorRampPalette(c("red", "blue"))
# Obtenir l'ordre des indices pour trier les couleur correspondantes
order.indices <- order(order(E(g)$fij, decreasing = TRUE), method = "radix")
# Mapper les valeurs d'épaisseur des flèches à la palette de couleurs triée
edge.colors <- color.palette(length(E(g)$fij))[order.indices]

# affichage du fond de carte
par(mar = c(5, 4, 4, 2) + 0.1)
plot(uemoa_sp, border = NA, col = "NA", bg = "#A6CAE0")
plot(world_sp, col = "#E3DEBF", border = NA, add = T)
plot(uemoa_sp, col = "#D1914D", border = "grey80", add = T)

# affichage du graph
plot.igraph(g, add = TRUE, rescale = FALSE, vertex.size = as.numeric(V(g)$weights)/8000,
            vertex.color = "green",
            edge.curved = 0.8, 
            edge.width = E(g)$fij/90000,
            edge.color = edge.colors)

title("Flux des échanges commerciaux dans la zone UEMOA")
# Ajouter une légende pour l'intensité des flux

legend("bottomright", legend = c("Élevée", "Moyenne", "Faible"),
       col = color.palette(3), lwd = 1,
       title = "flux des exportations", bty = "n")
# Ajouter la légende pour la taille des nœuds
legend("topleft", legend = c("Faible", "Moyenne", "Élevée"),
       pch = 21, col = "white", pt.bg = "green", pt.cex = c(0.5, 1, 1.5),
       title = "Importations totales")


