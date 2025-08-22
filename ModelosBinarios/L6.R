library(tidyverse); library(summarytools); library(camerondata); library(stargazer)
library(ggview); library(margins); library(marginaleffects) 
library(pscl); library(vcdExtra); library(ResourceSelection); library(generalhoslem); library(epiDisplay)



data("fishing")
freq(fishing$mode)

# Nos quedamos con mode=2, 4

data <- fishing |> filter(mode %in% c(2,4))|> 
  mutate(mode = case_when(mode==4 ~ 1,
                          mode==2 ~ 0),
         lnrelp  = log(pcharter/ppier),
         lincome = log(income))
freq(data$mode)

df <- data |> group_by(mode) |> 
  summarise(precio_bote   = mean(pcharter),
            precio_muelle = mean(ppier),
            lnrelp        = mean(lnrelp),
            probabilidad  = (n()/630)*100,
            n             = n())
df

# Modelos de regresión
# OLS
ols <- lm(mode ~ lnrelp, data=data)
summary(ols)
data$y_est_ols <- predict(ols)
summary(data$y_est_ols)

# Logit
logit <- glm(mode ~ lnrelp, data=data,
              family=binomial(link="logit"))
summary(logit)
data$y_est_logit <- predict(logit, type = "response")
summary(data$y_est_logit)

# Probit
probit <- glm(mode ~ lnrelp, data=data,
              family=binomial(link="probit"))
data$y_est_probit <- predict(probit, type = "response")
summary(data$y_est_probit)

stargazer(ols, logit, probit, 
          type="text", 
          dep.var.labels=c("Modo de pesca (1=bote; 0=muelle)"),
          model.names = FALSE,
          model.numbers = FALSE,
          column.labels = c("OLS", "Logit", "Probit"), 
          omit.stat = c("ll","aic","ser","f"),
          align=TRUE)

ggplot(data) +
  geom_point(aes(x = lnrelp, y = mode, color = "Observado")) +
  geom_line(aes(x = lnrelp, y = y_est_ols, color = "OLS")) +
  geom_line(aes(x = lnrelp, y = y_est_logit, color = "Logit")) +
  geom_line(aes(x = lnrelp, y = y_est_probit, color = "Probit"), linetype = "dashed") +
  scale_color_manual(name = "", 
                     breaks = c("Observado", "OLS", "Logit", "Probit"),
                     values = c("Observado" = "blue", "OLS" = "red", "Logit" = "green", "Probit" = "orange")) +
  labs(x = "Ln precio relativo", y = "Probabilidad predicha") +
  theme(legend.position = c(0.9, 0.9), legend.background = element_rect(fill = "transparent")) +
  canvas(width = 7, height = 5)

# Efectos marginales
# Efecto parcial promedio (Average marginal effects)
summary(margins(probit))
avg_slopes(probit)

# Efecto parcial en el promedio
summary(margins(probit, at = list(lnrelp=mean(data$lnrelp))))

slopes(probit, newdata = datagrid(lnrelp=mean(data$lnrelp)))
slopes(probit, newdata = "mean")

# Gráficos
cplot(probit, "lnrelp", what = "prediction", main = "Efecto parcial promedio de ln relp", data=data)
cplot(probit, "lnrelp", what = "effect", main = "Efecto parcial promedio de ln relp", data=data)

# Curvas de probabilidad

data <- data |> 
  mutate(bing = case_when(income>=mean(income)~1,
                          income<mean(income)~0))
freq(data$bing)

probit2 <- glm(mode ~ lnrelp + factor(bing), data=data,
              family=binomial(link="probit"))
summary(probit2)

newdata <- data.frame("lnrelp" = seq(from = -2.2, to = 4.1, length.out = 20),
                      "bing" = 1)

newdata[,c("pbing1","se1")] <- predict(probit2, 
                                       newdata,
                                       type = "response", se.fit = TRUE)[-3]

newdata[,c("bing")] <- 0

newdata[,c("pbing0","se0")] <- predict(probit2, 
                                       newdata,
                                       type = "response", se.fit = TRUE)[-3]

newdata$pbing1_lb <- newdata$pbing1-qt(0.025, 20-1, lower.tail = FALSE)*newdata$se1
newdata$pbing1_ub <- newdata$pbing1+qt(0.025, 20-1, lower.tail = FALSE)*newdata$se1
newdata$pbing0_lb <- newdata$pbing0-qt(0.025, 20-1, lower.tail = FALSE)*newdata$se0
newdata$pbing0_ub <- newdata$pbing0+qt(0.025, 20-1, lower.tail = FALSE)*newdata$se0

ggplot(newdata) +
  geom_line(aes(x = lnrelp, y =pbing1), colour="blue") +
  geom_line(aes(x = lnrelp, y =pbing0), color="red") +
  geom_ribbon(aes(ymin=pbing1_lb,ymax=pbing1_ub, x=lnrelp, fill="IC 95%"),alpha=0.3) +
  geom_ribbon(aes(ymin=pbing0_lb,ymax=pbing0_ub, x=lnrelp, fill="IC 95%"),alpha=0.3) +
  labs(x="Ln precio relativo", y="Prob(Modo=bote)") +
  scale_fill_manual(name = "",  values=c("IC 95%" = "grey12")) +
  theme(legend.position = c(0.85, 0.3), legend.background = element_rect(fill = "transparent")) +
  geom_text(x=2, y=0.1, label="Ricos", size=4, color="blue") +
  geom_text(x=2.6, y=0.15, label="Pobres", size=4, color="red") +
  canvas(width=7, height=5)

# Medidas de bondad de ajuste
# Pseudo R2 = 1 - (LogL_nr / LogL_r)
pR2(probit)

# Proporción de predicciones correctas
hitmiss(probit)

freq(data$mode)

hitmiss(probit, k=0.7175)

# Hosmer-Lemeshow goodness-of-fit test
# Este test nos dice qué tan bien se ajustan sus datos al modelo
# Ho: el modelo ajuste bien vs Ha: el modelo ajusta pobremente 
HLtest(probit)


hoslem.test(data$mode, fitted(probit))
logitgof(data$mode, fitted(probit))

# La curva ROC
# La elección se realiza mediante la comparación del área bajo la curva (AUC) de 
# ambas pruebas. Esta área posee un valor comprendido entre 0.5 y 1, donde 1 
# representa un valor de predicción perfecto y 0.5 mala predicción. Es decir, si AUC
# es 0.8 significa que existe un 80% de probabilidad de predecir el evento
auc <- lroc(probit, auc.coords=c(.5,.1))
auc["auc"]

