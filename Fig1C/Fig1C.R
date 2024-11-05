library(rgdal)      
library(ggplot2)   
library(ggrepel)    
library(data.table)

load("C:/Users/xxx/Desktop/KSB1/Rplotdata/NaturalEarth.RData")
PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

NE_countries.prj  <- spTransform(NE_countries, CRSobj = PROJ)
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)
NE_box.prj        <- spTransform(NE_box, CRSobj = PROJ)

prj.coord <- project(cbind(lbl.Y$lon, lbl.Y$lat), proj = PROJ)
lbl.Y.prj <- cbind(prj.coord, lbl.Y)
names(lbl.Y.prj)[1:2] <- c("X.prj","Y.prj")

prj.coord <- project(cbind(lbl.X$lon, lbl.X$lat), proj = PROJ)
lbl.X.prj <- cbind(prj.coord, lbl.X)
names(lbl.X.prj)[1:2] <- c("X.prj","Y.prj")

df <- read.table("C:/Users/xxx/Desktop/KSB1-new/plot_data/environment_distribution.txt",
                 header = T,sep = "\t")
df <- df[df$Latitude != 'missing',]
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
prj.coord <- project(cbind(df$Longitude, df$Latitude), proj = PROJ)

NE_places.df.prj <- cbind(prj.coord, df)
names(NE_places.df.prj)[1:2] <- c("X.prj","Y.prj")
NE_places.dt.prj <- data.table(NE_places.df.prj)

NE_places.dt.prj$point <- as.numeric(cut(NE_places.dt.prj$P97,
                                         labels=c(1:5),
                                         breaks = c(0,10,50,100,1000,5000))) 


p <- ggplot() +
  geom_polygon(data = NE_countries.prj, 
               aes(long,lat, group = group), 
               colour = "gray70", fill = "gray90", size = .25) +
  geom_polygon(data = NE_box.prj, 
               aes(x = long, y = lat), 
               colour = "black", fill = "transparent", size = .25) +
  geom_point(data = NE_places.dt.prj, 
             aes(x = X.prj, y = Y.prj, size = point, colour = Order, fill = Order), 
             alpha = 0.5)+
  scale_color_manual(values = c("#ff355c", "#ffdf6a", "#3b6fe1","#00aa5a","#68A24F", "#D56463"))+
  scale_size(range = c(3,7))+
  # add graticules
  geom_path(data = NE_graticules.prj, 
            aes(long, lat, group = group), 
            linetype = "dotted", colour = "grey50", size = .25) +
  # add graticule labels - latitude and longitude
  geom_text(data = lbl.Y.prj, # latitude
            aes(x = X.prj, y = Y.prj, label = lbl), 
            colour = "grey50", size = 2) +
  geom_text(data = lbl.X.prj, # longitude
            aes(x = X.prj, y = Y.prj, label = lbl), 
            colour = "grey50", size = 2)

ggsave(p,width = 11,height = 7, filename = "distribution_map.pdf")
