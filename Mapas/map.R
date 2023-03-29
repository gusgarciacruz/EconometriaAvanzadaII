library(sf); library(tmap); library(tidyverse); library(readxl); library(tmaptools)
library(viridis); library(summarytools); library(terra)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría avanzada pregrado/Mapas R")

map_comunas_corr <- st_read("LimiteComunaCorregimiento_2014.shp") %>%
  rename(idcomuna=CODIGO) %>%
  filter(NOMBRE!='NA')

ggplot(map_comunas_corr) +
  geom_sf(color = "red", size = 0.05)

map_comunas <- st_read("LimiteComunaCorregimiento_2014.shp") %>%
  rename(idcomuna=CODIGO) %>%
  filter(NOMBRE!='NA', !idcomuna %in% c('50','60','70','80','90')) %>%
  mutate(areakm2 = case_when(NOMBRE=='Popular'~2.3,
                             NOMBRE=='Santa Cruz'~2.21,
                             NOMBRE=='Manrique'~5.495,
                             NOMBRE=='Aranjuez'~4.8772,
                             NOMBRE=='Castilla'~6.096,
                             NOMBRE=='Doce de Octubre'~3.835,
                             NOMBRE=='Robledo'~9.382,
                             NOMBRE=='Villa Hermosa'~5.78,
                             NOMBRE=='Buenos Aires'~5.9963,
                             NOMBRE=='La Candelaria'~7.3563,
                             NOMBRE=='Laureles Estadio'~7.42,
                             NOMBRE=='La América'~3.89,
                             NOMBRE=='San Javier'~7,
                             NOMBRE=='El Poblado'~23,
                             NOMBRE=='Guayabal'~7.64,
                             NOMBRE=='Belén'~8.312)) 

names(map_comunas)

summarytools::freq(map_comunas$idcomuna)

ggplot(map_comunas) +
  geom_sf(color = "red", size = 0.05)

map_metro <- st_read("LineasSistema_07-20.shp") %>%
            filter(LINEA != "La Aldea")  

ggplot(map_metro) +
  geom_sf(color = "blue", size = 0.05)

data <- read_excel("ProyeccionComuna.xlsx") %>%
  rename(comuna=Comuna, totalpob="Total Población",
         percent="% de total SUM([hombres]) + SUM([mujeres]) junto con Comuna...4") %>%
  select(comuna, totalpob, percent) %>%
  filter(comuna!="Total") %>%
  mutate(idcomuna = case_when(comuna=='Popular'~'01',
                              comuna=='Santa Cruz'~'02',
                              comuna=='Manrique'~'03',
                              comuna=='Aranjuez'~'04',
                              comuna=='Castilla'~'05',
                              comuna=='Doce de Octubre'~'06',
                              comuna=='Robledo'~'07',
                              comuna=='Villa Hermosa'~'08',
                              comuna=='Buenos Aires'~'09',
                              comuna=='La Candelaria'~'10',
                              comuna=='Laureles - Estadio'~'11',
                              comuna=='La América'~'12',
                              comuna=='San Javier'~'13',
                              comuna=='El Poblado'~'14',
                              comuna=='Guayabal'~'15',
                              comuna=='Belén'~'16',
                              comuna=='Palmitas'~'50',
                              comuna=='San Cristóbal'~'60',
                              comuna=='Altavista'~'70',
                              comuna=='San Antonio'~'80',
                              comuna=='Santa Elena'~'90'))
                              
names(data)
head(map_comunas[,c("NOMBRE","idcomuna")])

map_comunas_merge <- left_join(map_comunas,data) %>%
                     mutate(densidad = as.integer(totalpob/areakm2)) 

m1 <-  tm_shape(map_comunas_merge) +
     tm_polygons("densidad",
                 title="",
                 border.col = "gray35",
                 lwd = .4,
                 palette = "BuPu",
                 n = 4,
                 style = "quantile"
                 #labels = c('8,194 - 97,698','97,698 - 132,991', '132,991 - 152,158', '152,158 - 197,625')
                 ) + 
    tm_text("comuna", col="gray30", scale=.7) +
    tm_compass(type = "8star", position = c("right", "bottom")) +
    tm_shape(map_metro)+tm_lines(col = 'red', lwd = 1.5, legend.show = TRUE) +
    tm_add_legend("line", col='red', labels=c("Línea de Metro")) +
    tm_layout(legend.format = list(text.separator = "-"), 
            legend.position = c("left","bottom"),legend.text.size = .8)
m1

tmap_save(m1, "popmap.png", width=4000, height=2000)

m2 <- 
tm_shape(map_comunas_merge, bbox = map_comunas) +
  tm_polygons("densidad",
              title="",
              border.col = "gray35",
              lwd = .4,
              palette = viridis(5, direction = -1),
              n = 5,
              style = "quantile", id="densidad") + 
  tm_text("comuna", col="gray30", scale=.7) +
  tm_shape(map_metro)+tm_lines(col = 'red', lwd = 1.5, legend.show = TRUE, id="NOMBRE") +
  tm_add_legend("fill", col='red', labels=c("Metro")) +
  tm_layout(legend.format = list(text.separator = "-"), legend.text.size = .5) +
  tm_scale_bar(just = "left", text.size = 0.5, position = c("right", "bottom")) +
  tm_basemap(server="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",alpha=0.5) +
  tm_view(view.legend.position = c('left', 'bottom')) 

ttm()
m2

theme_set(theme_void())

ggplot(map_comunas_merge) +
  geom_sf(aes(fill = densidad), color = "#ffffff", size = 0.05)  +
  geom_sf_text(aes(label = comuna), size=0.8) +
  geom_sf(data = map_metro, fill = "grey40", aes(color = "Líneal de metro"), size=.2) +
  scale_fill_viridis_c(direction = -1) + labs(fill=NULL) +
  scale_color_manual(values = c("Líneal de metro" = "grey40"), name=NULL) +
  theme(legend.position = c(.8,.25),
        legend.key.size = unit(0.15,"cm"),
        legend.text=element_text(size=2),
        legend.spacing = unit(-.1,"cm"),
        legend.spacing.x = unit(.04, 'cm'))

ggsave("figure.png", dpi = 2000)

