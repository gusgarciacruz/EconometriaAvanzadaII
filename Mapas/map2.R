library(sf); library(tmap); library(tidyverse); library(readxl); library(tmaptools)
library(viridis); library(summarytools); library(terra); library(ggview)
library(ggmap); library(mapview); library(leaflet)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometría avanzada pregrado/Mapas R")

map_comunas_corr <- st_read("LimiteComunaCorregimiento_2014.shp")  |> 
  rename(idcomuna=CODIGO)  |> 
  filter(NOMBRE!='NA')

crs(map_comunas_corr)

ggplot(map_comunas) +
  geom_sf(color = "blue", size = 0.05) +
  canvas(11,9, units = 'in')

map_comunas <- st_read("LimiteComunaCorregimiento_2014.shp")  |> 
  rename(idcomuna=CODIGO) |> 
  filter(NOMBRE!='NA', !idcomuna %in% c('50','60','70','80','90'))  %>% 
  mutate(areakm2 = as.numeric(st_area(.)/1000000))  

names(map_comunas)

summarytools::freq(map_comunas$idcomuna)

ggplot(map_comunas) +
  geom_sf(color = "orange", size = 0.05)

map_metro <- st_read("LineasSistema_07-20.shp") |> 
            filter(LINEA != "La Aldea")  

ggplot(map_metro) +
  geom_sf(color = "blue", size = 0.05)

data <- read_excel("ProyeccionComuna.xlsx") |> 
  rename(comuna=Comuna, totalpob="Total Población",
         percent="% de total SUM([hombres]) + SUM([mujeres]) junto con Comuna...4") %>%
  select(comuna, totalpob, percent) |> 
  filter(comuna!="Total") |> 
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

map_comunas_merge <- merge(map_comunas,data, by="idcomuna")  |> 
                     mutate(densidad = as.integer(totalpob/areakm2)) 

ggplot(map_comunas_merge) +
  geom_sf(aes(fill = densidad), color = "#ffffff", size = 0.05)  +
  geom_sf_text(aes(label = comuna), size=1.2) +
  geom_sf(data = map_metro, fill = "grey40", aes(color = "Líneal de metro"), linewidth=.3) +
  scale_fill_viridis_c(direction = -1) + labs(fill=NULL) +
  scale_color_manual(values = c("Líneal de metro" = "grey40"), name=NULL) +
  theme_void() +
  theme(legend.position = c(.8,.25),
        legend.key.size = unit(0.15,"cm"),
        legend.text=element_text(size=4),
        legend.spacing = unit(.1,"cm"),
        legend.spacing.x = unit(.04, 'cm')) +
  canvas(3,4, units = 'in')

ggsave("figure.png", width = 3, height = 4, dpi = 300, bg="white")
