# Geographically Weighted Regression
# Se va a trabajar con información a nivel de bloque/manzana para Filadelfia (US). Se tiene información sobre
# infracciones al código de construcción y características demográficas y socieconómicas de los barrios
# La descripción detallada de los datos puede verse en el siguiente link: https://raw.githubusercontent.com/crd230/data/master/ph_tract_record_layout.txt

# La idea es analizar la relación enter las características de los barrios y las tasas de violación
# al código de construcción

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria II Maestría Eco/R/2021-II/Tema 5")
library(sf); library(tidyverse); library(summarytools); library(sp); library(spgwr); library(ggview); library(ggpubr)
library(gwrr); library(car); library(GWmodel); library(mctest)

download.file(url = "https://geodacenter.github.io/data-and-lab/data/columbus.zip", destfile = "columbus.zip")

unzip(zipfile = "columbus.zip")

map <- st_read("./columbus/columbus.shp") %>%  
  rename_all(., .funs = tolower)

ggplot(map) + 
  geom_sf(colour = "gray95", fill = "gray75") +
  theme_void()

# OLS
fit.ols<-lm(crime ~ inc + hoval, map) 
summary(fit.ols)

# Graficando los residuales
map <- map |> 
  mutate(residuals = fit.ols$residuals)

ggplot(map) + 
  geom_sf(aes(fill = residuals), color = "gray95", size = 0.05) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill="Residuales OLS") +
  theme_void() +
  theme(legend.position = c(.2,.5),
        legend.key.size = unit(0.3,"cm"),
        legend.text=element_text(size=4),
        legend.title=element_text(size=6)) +
  canvas(units = "cm", width = 10, height = 9, dpi = 300, bg="white")

ggview(units = "cm", width = 10, height = 9, dpi = 300, bg="white")  

# Si existe un patron geográfico en los residuales, es posible que alguna variable inobservable puede estar influenciando 
# la variable dependiente

# El modelo OLS asume homogeneidad espacial en la relación. Analicemos ahora si el supuesto de 
# homogeneidad es apropiado estimando un GWR

# Convirtiendo el mapa en un objeto sp
map.sp <- as(map, "Spatial")

# Se calcula el ancho de banda óptimo, por defecto se hace CV (se pude cambiar con method="aic")
# Se utiliza el paquete spgwr
gwr.b1<-gwr.sel(crime ~ inc + hoval, map.sp) 
gwr.b1

# Esta es la distancia (en metros, porque nuestros datos se proyectan en un sistema medido en metros),
# que la función de ponderación buscará, e incluirá todas las observaciones dentro de este radio. 
# Esto también representa el valor h.

# Estimando el modelo GWR
# La función de ponderación por defecto es la Gaussiana, la cual puede cambiarse con la opción
# gweight, y debe especificarse tanto en el cálculo del ancho de banda como en la estimación
gwr.fit1<-gwr(crime ~ inc + hoval, data = map.sp, bandwidth = gwr.b1, se.fit=T, hatmatrix=T)
gwr.fit1

# Kernel adaptativo
# Los modelos GWR que ejecutamos anteriormente arrojaron una distancia fija para buscar vecinos que 
# incluir en la regresión local. Pero hay lugares en nuestros datos donde los bloques/manzanas son más densos. 
# Esto significa que en algunas zonas, concretamente en el centro de Filadelfia, se incluirá un mayor
# número de bloques/manzanas vecinos en la regresión local en comparación con otras zonas, como los 
# grandes bloques/manzanas de la periferia de los límites de la ciudad. En este caso, resulta adecuado
# un kernel adaptativo. La idea es que el kerne se adapta para capturar el mismo número de observaciones
# en las regresiones locales
gwr.b2<-gwr.sel(crime ~ inc + hoval, data = map.sp, adapt = TRUE)
gwr.b2

# Este valor es la proporción de todos los casos que la función de ponderación buscará, e incluirá esta
# fracción de observaciones en un modelo para cada bloque/manzana. La distancia de ancho de banda 
# cambiará en función de la densidad espacial de las características en la clase de características
# de entrada. El ancho de banda se convierte en una función del número de vecinos más próximos, de modo
# que cada estimación local se basa en el mismo número de características. En lugar de una distancia 
# específica, se indica el número de vecinos utilizados para el análisis.

gwr.fit2<-gwr(crime ~ inc + hoval, data = map.sp, adapt=gwr.b2, se.fit=T, hatmatrix=T)
gwr.fit2

# Presentando los resultados de un GWR
# Graficando los coeficientes estimados
sdf <- st_as_sf(gwr.fit2$SDF) |> 
  mutate(dfree = gwr.fit2$results$edf,
         inc.t = inc/inc_se,
         inc.pval = 2*pt(-abs(inc.t), dfree), 
         inc.sig.5p = case_when(inc.pval<0.05 ~ 1,
                                inc.pval>=0.05 ~ 0),
         inc.sig = case_when(inc.pval<0.01~"p1",
                             inc.pval>=0.01 & inc.pval<0.05~"p5",
                             inc.pval>=0.05 & inc.pval<0.1~"p10",
                             inc.pval>=0.1~"No sig."))

# % de significancia
freq(sdf$inc.sig.5p)

inc.b <- ggplot(sdf) + 
  geom_sf(aes(fill = inc), color = "gray95", size = 0.05) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill="Efecto del ingreso") +
  theme_void() +
  theme(legend.position = c(.2,.5),
        legend.key.size = unit(0.3,"cm"),
        legend.text=element_text(size=4),
        legend.title=element_text(size=6))
inc.b + canvas(units = "cm", width = 10, height = 9, dpi = 300, bg="white")

# Graficando la significancia estadística
inc.pval <- ggplot(sdf) + 
  geom_sf(aes(fill = inc.sig), color = "white", size = 0.05, show.legend=TRUE) +
  scale_fill_manual(values = c("p1"="#dc143c", "p5"="#f1666d", "p10"="#ff9ea2","No sig."="gray95"),
                    labels = c("P<0.01", "P<0.05", "P<0.10", "No significante"),
                    limits = c("p1", "p5", "p10", "No sig.")) +
  labs(fill="Significancia estadística") +
  theme_void() +
  theme(legend.position = c(.2,.5),
        legend.key.size = unit(0.2,"cm"),
        legend.text=element_text(size=3),
        legend.title=element_text(size=5))
inc.pval + canvas(units = "cm", width = 10, height = 9, dpi = 300, bg="white")  

ggarrange(inc.b, inc.pval, 
          ncol = 2, nrow = 1) +
          canvas(units = "in", width = 8, height = 4, dpi = 300, bg="white")

# Diagnósticos de multicolinealidad
# VIF>10, número de condición>30 y proportion de varianza-descomposición>0.5 indican colinealidad

# Multicolinealidad en el modelo general
vif(fit.ols)
omcdiag(fit.ols)
# La multicolinealidad parece no ser un problema

# Multicolinealidad en el modelo GWR
betas <- as(gwr.fit2$SDF, "data.frame")[,2:4]
pairs(betas)
cor(betas)

# Se utiliza el paquete gwrr
locs <- cbind(map$x, map$y)
diag <- gwr.vdp(crime ~ inc + hoval, locs, data.frame(map), gwr.b1, "exp")
hist(diag$condition)
summary(diag$condition)
freq(diag$flag.cond)

hist(diag$vdp[,1])
summary(diag$vdp[,1])
hist(diag$vdp[,2])
summary(diag$vdp[,2])
hist(diag$vdp[,3])
summary(diag$vdp[,3])
freq(diag$flag.vdp)

# Solución a la multicolinealidad (del paquete gwrr)
# Geographically weighted ridge regression
gwr.fit3 <- gwrr.est(crime ~ inc + hoval, locs, data.frame(map), "exp")
summary(gwr.fit3)

map <- map |> 
  mutate(inc.br = gwr.fit3$beta[2,])

inc.br <- ggplot(map) + 
  geom_sf(aes(fill = inc.br), color = "gray95", size = 0.05) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill="Efecto del ingreso") +
  theme_void() +
  theme(legend.position = c(.2,.5),
        legend.key.size = unit(0.3,"cm"),
        legend.text=element_text(size=4),
        legend.title=element_text(size=6))
inc.br + canvas(units = "cm", width = 10, height = 9, dpi = 300, bg="white")  

# Geographically weighted lasso regression
gwr.fit4 <- gwl.est(crime ~ inc + hoval, locs, data.frame(map), "exp")
gwr.fit4

map <- map |> 
  mutate(inc.bl = gwr.fit4$beta[2,])

inc.bl <- ggplot(map) + 
  geom_sf(aes(fill = inc.bl), color = "gray95", size = 0.05) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill="Efecto del ingreso") +
  theme_void() +
  theme(legend.position = c(.2,.5),
        legend.key.size = unit(0.3,"cm"),
        legend.text=element_text(size=4),
        legend.title=element_text(size=6))
inc.bl + canvas(units = "cm", width = 10, height = 9, dpi = 300, bg="white")  

# Tets comparando OLS y GWR (del paquete spgwr)
# H0: se prefiere OLS
BFC02.gwr.test(gwr.fit2)
BFC99.gwr.test(gwr.fit2)
LMZ.F1GWR.test(gwr.fit2)
LMZ.F2GWR.test(gwr.fit2)
LMZ.F3GWR.test(gwr.fit2)

# Multi-scale GWR (del paquete GWmodel)
gw.ms <- gwr.multiscale(crime ~ inc + hoval,
                        data = map.sp,
                        adaptive = T,
                        max.iterations = 1000,
                        criterion = "CVR",
                        kernel = "bisquare",
                       bws0 = c(100,100))

# En particular, se debe examinar los resultados en cuanto al grado en el que los
# coeficientes estimados varían en relación a su ancho de banda
# Note que más pequeños bandwidths (una ventana móvil más pequeña) resulta en
# mayores variaciones y esto se reduce cuando el bandwidth tiende al valor global
# (se tienen 49 regiones en los datos). Así que, los resultados muestran que
# existen relaciones altamente locales (el efecto de hoval), mientras que hay
# otras altamente globales (el efecto de inc)
bws <- gw.ms$GW.arguments$bws
bws

coef_msgwr <- apply(gw.ms$SDF@data[,1:3],2,summary)
round(coef_msgwr,1)

tab.mgwr <- data.frame(Bandwidth = bws,
                       t(round(coef_msgwr,1)))
names(tab.mgwr)[c(3,6)] = c("Q1","Q3")

tab.mgwr

# Mapendo los coeficientes y su significancia estadística
mgwr_sf <- st_as_sf(gw.ms$SDF)

inc_bms <- ggplot(mgwr_sf) + 
  geom_sf(aes(fill = inc), color = "gray95", size = 0.05) +
  scale_fill_viridis_c() +
  labs(fill="Efecto del ingreso") +
  theme_void() +
  theme(legend.position = c(.2,.5),
        legend.key.size = unit(0.3,"cm"),
        legend.text=element_text(size=4),
        legend.title=element_text(size=6))

dfree<-gw.ms$GW.diagnostic$edf

mgwr_sf <- mgwr_sf |> 
  mutate(inc.t = inc/inc_SE,
         inc.pval = 2*pt(-abs(inc.t), dfree),
         sig = case_when(inc.pval<0.01~"p1",
                         inc.pval>=0.01 & inc.pval<0.05~"p5",
                         inc.pval>0.05 & inc.pval<0.1~"p10",
                         inc.pval>=0.1~"no sig"))

inc.pvalms <- ggplot(mgwr_sf) + 
  geom_sf(aes(fill = sig), color = "white", size = 0.05, show.legend=TRUE) +
  scale_fill_manual(values = c("p1"="#dc143c", "p5"="#f1666d", "p10"="#ff9ea2","No sig."="gray95"),
                    labels = c("P<0.01", "P<0.05", "P<0.10", "No significante"),
                    limits = c("p1", "p5", "p10", "No sig.")) +
  labs(fill="Significancia estadística") +
  theme_void() +
  theme(legend.position = c(.2,.5),
        legend.key.size = unit(0.2,"cm"),
        legend.text=element_text(size=3),
        legend.title=element_text(size=5))

ggarrange(inc_bms, inc.pvalms, 
          ncol = 2, nrow = 1) +
          canvas(units = "in", width = 8, height = 4, dpi = 300, bg="white")

# Para datos panel está el paquete GWPR.light: https://cran.r-project.org/web/packages/GWPR.light/GWPR.light.pdf