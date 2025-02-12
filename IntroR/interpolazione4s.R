#setwd("...")

#### lettura dello shapefile della Lombardia --------------------------------  
library(sf)
lomb.poly<-st_read("Lombardia.shp",quiet=TRUE) 
lomb.poly= st_set_crs(lomb.poly, 3003)  # EPSG=Gauss-Boaga
                                        # EPSG =European Petroleum Survey Group. 
                                        #codice che identifica i sistemi di riferimento 
library(ggplot2)
ggplot(data = st_boundary(lomb.poly)) + 
  geom_sf()

#### lettura del dataset e rappresentazione su mappa delle centraline  ---------
PM <- read.csv("PM.csv", sep=";");  
  summary(PM)

obj = st_as_sf(PM, coords = c("Longitude","Latitude"),crs=4326)  # EPSG=WGS84
  head(obj)  # breve descrizione dei dati
  summary(obj)
  
obj.gb <- obj %>%
  st_transform(crs = 3003)  # cambio di sistema di riferimento a GB
obj.gb    

#### plot delle centraline sulla mappa ####
ggplot(data = st_boundary(lomb.poly)) + 
  geom_sf() + 
  geom_sf(data = obj,  size = 2.5) + 
  theme_bw()  +
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br")

#### discretizzazione superfice su griglia ritagliata dentro lo shape  --------
library(stars)
# crea griglia in formato raster
st_bbox(lomb.poly) %>%
  st_as_stars(dx = 500) %>%    #dimensioni cella nelle direzioni x e y 
  st_crop(lomb.poly) -> gr     #ritaglia la griglia nello shape
gr
plot(gr ) # check griglia
# stima idw
require(gstat)
idw.c=gstat::idw(formula= AQ_PM10 ~ 1, locations=obj.gb, newdata=gr, 
                 nmax = 15, idp = 3)
# estrazione stime superficie  
hist(idw.c$var1.pred); summary(as.vector(idw.c$var1.pred))
# plot mappa
ggplot() + geom_stars(data = idw.c, aes(fill = var1.pred, x = x, y = y)) + 
  geom_sf(data = st_cast(lomb.poly, to="MULTILINESTRING"),lwd=2) + 
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="tr") +
  geom_sf(data = obj,col="white")

#####  usando una regressione lineare -----------------------------
colnames(PM)[1:2]=c("y","x")
a=lm(AQ_PM10 ~ x+y, PM)
gr$values=predict(a, as.data.frame(gr))

ggplot() + geom_stars(data = gr, aes(fill = values, x = x, y = y)) + 
  geom_sf(data = st_cast(lomb.poly, to="MULTILINESTRING"),lwd=2) + 
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="tr")
