# instalation des packages 
install.packages("cartography")
install.packages("maptools")
install.packages("tmap")
install.packages("rgdal")
install.packages("readxl")
install.packages("remotes")
remotes::install_github("riatelab/mapinsetr")
install.packages("cartogram")
install.packages("sp")
install.packages("sf")
install.packages("mapview")

# instalation de librarys 

library(leaflet.providers)
library(mapview)
library(maptools)
library(rgdal)
library(cartography)
library(sf)
library(readxl)
library(mapinsetr)
library(cartogram)
library(sp)
library(tmap)

# Charger la couche géographique des communes d'Occitanie 
reg_occ <- st_read(dsn = "data/Occitanie.gpkg", stringsAsFactors = FALSE)

# Vérifiez le système de projection 
st_crs(reg_occ)

# Conversion de la projection en lambert91
occ <- st_transform(x = reg_occ, crs = 2154)

# Charger les fichier de données data/base_cc_comparateur.xls
occ_df <- read_excel(path = "data/base_cc_comparateur.xls", sheet = 1, skip = 5) 

# Réaliser une jointure
join <- merge(x = occ, y = occ_df, by.x = "INSEE_COM", by.y = "CODGEO")
head(join) # ID commun au deux tables est le code département

# afficher la liste des departements
unique(reg_occ$INSEE_DEP)

# Séléctionnez toutes les communes d'un seul département 
com_Tarn <- join[join$INSEE_DEP==81, ]
com_Tarn
plot(st_geometry(com_Tarn))

# Réaliser des cartes thématiques

# 1 - carte de localisation
REG76 <- st_union(occ)
DEP81 <- aggregate(com_Tarn["POPULATION"],by = list(com_Tarn$INSEE_DEP), sum)
DEP76 <- aggregate(occ["POPULATION"], by = list(occ$INSEE_DEP), sum)

  # Configuration de la zone graphique
  par(mar=c(0,0,1.2,0))

  # affichage des couches 
  plot(st_geometry(DEP76), col = NA, border = "#797D7F", lwd = 1)

  # affichage du tarn
  plot(st_geometry(DEP81),col = "skyblue",border = "red",lwd = .5, 
     add = TRUE)

  # ettiquetage des departements
  labelLayer(x = DEP76, txt = "Group.1", col= "black", cex = 0.7, 
    font = 1, r = 0.1, overlap = FALSE, show.lines = FALSE)
  
  # affichage du coutour de la region
  plot(REG76, col = NA, border = "black", lwd = 1.5, add = TRUE)

  # Affichage de la légende
  layoutLayer("PLAN DE SITUATION DU DÉPARTEMENT DU TARN EN OCCITANIE", 
      postitle = "center",
      sources = "Carte realisée par K.Lacina",
      author = "Sources: Admin Express,IGN;Base Comparateur de territoires,INSEE",
      scale = 50,
      north = TRUE)
  
# 2 - carte représentant une variable quantitative absolue 
  
  # Configuration de la zone graphique
  par(mar=c(0,0,1.2,0))
  
  plot(
  st_geometry(com_Tarn), 
  col = "lightblue4", 
  border = "lightblue3", 
  bg = "lightblue1")
  
  # Symboles proportionnels
  propSymbolsLayer(
  x = com_Tarn, 
  var = "P16_POP", 
  legend.pos = "topleft",
  legend.title.txt = "Populations")
 
  # Affichage de la légende
  layoutLayer("CARTE DE REPARTITION DE LA POPULATION 2016 DANS LE TARN", 
              postitle = "center",
              sources = "Carte réalisée par K.Lacina",
              author = "Sources: Admin Express,IGN;Base Comparateur de territoires,INSEE",
              scale = 10,
              north = TRUE)
  
# 3 - carte représentant une variable quantitative relative
  
  # Configuration de la zone graphique
  par(mar=c(0,0,1.2,0))
  
  # Densité de population 
  com_Tarn$POPDENS <- com_Tarn$P16_POP / com_Tarn$SUPERF
  
  # Carte clorophete
  choroLayer(x = com_Tarn, var = "POPDENS",
  breaks = c(3,62,184,372,757,1114),
  col = carto.pal(pal1 = "orange.pal", n1 = 5),
  border = "grey40",
  legend.pos = "topleft",
  legend.title.txt = "Habitants/km²")

  # Affichage de la légende
  layoutLayer("DISTRIBUTION DE LA DENSITÉ DE LA POPULATION 2016 DANS LE TARN", 
            postitle = "center",
            sources = "Carte réalisée par K.Lacina",
            author = "Sources: Admin Express,IGN;Base Comparateur de territoires,INSEE",
            posscale = "bottomright",
            scale = 10,
            north = TRUE)

# 4 - Variable quantitative absolue et une Variable quantitative relative

  # Configuration de la zone graphique
  par(mar=c(0,0,1.2,0))
  
  plot(
    st_geometry(com_Tarn), 
    col="darkseagreen3", 
    border="darkseagreen4",  
    bg = "lightblue1")
  
  propSymbolsChoroLayer(
    x = com_Tarn, 
    var = "P16_POP", 
    border = "grey50",
    lwd = 1,
    legend.var.pos = "topleft", 
    legend.var.title.txt = "Populations",
    
    var2 = "POPDENS",
    method = "equal", 
    nclass = 5, 
    col = carto.pal(pal1 = "sand.pal", n1 = 5),
    legend.var2.values.rnd = -2,
    legend.var2.pos = "left", 
    legend.var2.title.txt = "Densité") 
  
  layoutLayer("Population et Densité", 
              postitle = "center",
              sources = "Carte réalisée par K.Lacina",
              author = "Sources: Admin Express,IGN;Base Comparateur de territoires,INSEE",
              posscale = "bottomright",
              scale = 10,
              north = TRUE)

# 5 - carte représentant une variable qualitive
  
  # Configuration de la zone graphique
  par(mar=c(0,0,1.2,0))

  # Variable qualitative : part des chomeurs 15-64 en 2016
  com_Tarn$PART_CHOM <- 100 * com_Tarn$P16_CHOM1564 / com_Tarn$P16_POP1564
  summary(com_Tarn$PART_CHOM)
  
  com_Tarn$PART_CHOM <- cut(com_Tarn$PART_CHOM, breaks = c(1.5,6.5,11,24), 
                            labels = c("Faible", "Moyenne", "Forte"),
                            include.lowest = TRUE)
  
  typoLayer(com_Tarn, var = "PART_CHOM", col= c("#EC7063", "#64B5F6", "#BFC9CA"),
            legend.pos = "topleft", 
            legend.title.txt = "Part des chomeurs")
  
  layoutLayer("PART DES CHOMEURS DE 15-64 ANS DANS LE TARN", 
              postitle = "center",
              sources = "Carte réalisée par K.Lacina",
              author = "Sources: Admin Express,IGN;Base Comparateur de territoires,INSEE",
              posscale = "bottomright",
              scale = 10,
              north = TRUE)
  
# 6 - carte utilisant une anamorphose
  
  # Réalisons une carte cloroplèthe
  par(mar=c(0,0,1.2,0))
  
    # Choix de la palette
    display.carto.all(5)
    color <- carto.pal(pal1 = "turquoise.pal", n1 = 5)
    
    # Methode discretisation (quantiles)
    breaks <- getBreaks(v = com_Tarn$POPDENS, nclass = 5, method = "quantile")
  
    # Cartographie (ratio)
    plot(st_geometry(com_Tarn))
    choroLayer(x = com_Tarn, var = "POPDENS",method ="quantile",
               nclass = 5,col = color, border = NA,add = TRUE,
               legend.pos = "left",
               legend.title.txt = "NB Habitants/km²",
               legend.values.rnd = 0)
    
    plot(com_Tarn, col = NA, border = "gray", lwd = 0.2, add = TRUE)
    
  # création de cartogram
  crt_disc <- cartogram_ncont(com_Tarn, "POPDENS", k = 1)
    
  # Affichage du cartograme de discontinuité
  plot(st_geometry(crt_disc), col = color)
  plot(com_Tarn, col = NA, border = "gray", lwd = 0.2, add = TRUE)
    
  legendChoro(title.txt = "NB Habitants/km²",
                pos = "topleft",
                col = color,
                nodata.txt = "0",
                breaks = breaks)
  
  layoutLayer("CARTOGRAMME DE DISCONTINUITE DE LA DENSITÉ", 
                postitle = "center",
                sources = "Carte réalisée par K.Lacina",
                author = "Sources: Admin Express,IGN;Base Comparateur de territoires,INSEE",
                posscale = "bottomright",
                scale = 10,
                north = TRUE)
 
# 7 - carte interactive
  
mapview(com_Tarn, layer.name="DEPARTEMENT DU TARN", label=com_Tarn$NOM_COM)

# 8 - carte utilisant une grille régulière ou représentant des discontinuités

par(mar=c(0,0,1.2,0))

  # Définition du pas de grille et la variable a transformer
  grille <- getGridLayer(x = com_Tarn, cellsize = 5000 * 5000, type = "regular", var = c("P11_POP"))

  # conversion des grilles en km²
  grille$DENSITE <- grille$P11_POP * 1000 *1000 /grille$gridarea

  # Choix de la palette
  display.carto.all(5)
  color <- carto.pal(pal1 = "turquoise.pal", n1 = 5)
  
  # affichage de la carte 
  plot(st_geometry(com_Tarn), col = NA, border = "gray" )
  
  choroLayer(x = grille, var = "DENSITE", 
             border = "grey80",col = color, method ="quantile",
             nclass = 5, legend.pos = "topleft", legend.values.rnd = 1,
             lwd=0.5, add = T,
             legend.title.txt = "Densité")
  
  layoutLayer("DENSITÉ DE LA POPULATION 2011 DANS UNE GRILLE REGULIERE DE 50 KM", 
            postitle = "center",
            sources = "Carte réalisée par K.Lacina",
            author = "Sources: Admin Express,IGN;Base Comparateur de territoires,INSEE",
            posscale = "bottomright",
            scale = 10,
            north = TRUE)
