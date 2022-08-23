# GMM IN LINEAR DATA PANEL MODELS
# We will use data from a subsample of the NLSY data on young women aged 14-26 years in 1968.
# We have a longitudinal dataset of women surveyed each 2 years during the period 1968-1988 
# (except for six years). The data is saved in the file nlswork.dta.

library(haven); library(plm); library(tidyverse); library(summarytools)
library(AER); library(gmm); library(modelsummary); library(gt)

# We have 4711 women employed, not enrrolled in school and who have completed their education
# Descripción de las variables: https://rdrr.io/rforge/sampleSelection/man/nlswork.html
nlswork <- read_dta("http://www.stata-press.com/data/r10/nlswork.dta") %>% # leemos la base de datos
  mutate(age2 = age*age) %>%     # construimos la variables age2
  select(idcode, year, ln_wage, age, age2, tenure, union, not_smsa, south) |> # seleccionando var
  filter(union !='NA', south !='NA')

# Set panel structure 
nlswork <- pdata.frame(nlswork, c("idcode","year"))

# IV
# Pool
ols_pool <- lm(ln_wage ~ tenure+age+I(age2)+not_smsa, data=nlswork)
summary(ols_pool)

iv1_pool <- ivreg(ln_wage ~ tenure+age+I(age2)+not_smsa |
                      union+south+age+I(age2)+not_smsa, data=nlswork)
summary(iv1_pool)

iv2_pool <- tsls(ln_wage ~ tenure+age+I(age2)+not_smsa,
                         ~  union+south+age+I(age2)+not_smsa, data=nlswork)
summary(iv2_pool)

iv3_pool <- plm(ln_wage ~ tenure + age + I(age2) + not_smsa |
              union + south + age + I(age2) + not_smsa, data = nlswork, model = "pooling")
summary(iv3_pool)

gmm_iid <- gmm(ln_wage ~ tenure+age+I(age2)+not_smsa,
           ~  union+south+age+I(age2)+not_smsa, data=nlswork,
           vcov="iid")
summary(gmm_iid)

gmm_hac <- gmm(ln_wage ~ tenure+age+I(age2)+not_smsa,
               ~  union+south+age+I(age2)+not_smsa, data=nlswork,
               vcov="HAC")
summary(gmm_hac)

# Tabla de resultados
models <- list("OLS" = lm(ln_wage ~ tenure+age+I(age2)+not_smsa, data=nlswork),
               "IV1" = ivreg(ln_wage ~ tenure+age+I(age2)+not_smsa |
                               union+south+age+I(age2)+not_smsa, data=nlswork),
               "IV2" = tsls(ln_wage ~ tenure+age+I(age2)+not_smsa,
                            ~  union+south+age+I(age2)+not_smsa, data=nlswork),
               "IV3" = plm(ln_wage ~ tenure + age + I(age2) + not_smsa |
                             union + south + age + I(age2) + not_smsa, data = nlswork, model = "pooling"),
               "GMM_iid" = gmm(ln_wage ~ tenure+age+I(age2)+not_smsa,
                           ~  union+south+age+I(age2)+not_smsa, data=nlswork, vcov="iid"),
               "GMM_hac" = gmm(ln_wage ~ tenure+age+I(age2)+not_smsa,
                           ~  union+south+age+I(age2)+not_smsa, data=nlswork, vcov="HAC"))
modelsummary(models, 
             output="gt",
             coef_map = c('tenure' = 'Tenure',
                          'age' = 'Age',
                          'I(age2)' = 'Age2',
                          'not_smsa' = 'Not SMSA (=1)',
                          '(Intercept)' = 'Constant'),
             statistic = "std.error",
             title = "Table 1. Effects of tenure on wages",
             gof_omit='IC|Log|F|R2',
             stars = c('*'=.1,'**'=.05,'***'=.01)) |> 
             tab_style(style = cell_text(color='red'), 
                       location = cells_body(row=1:2)) |>
             tab_style(style = cell_text(color='black', size='x-small'), locations = cells_source_notes()) |> 
             tab_source_note(source_note = "Note: standard errors in parentheses")

# Diagnósticos de los instrumentos
linearHypothesis(lm(tenure ~ union+south+age+I(age2)+not_smsa, data=nlswork), 
                 c("union = 0", "south = 0"))
summary(iv1_pool, diagnostics=TRUE)
summary(iv2_pool)

# Weak instruments: La H0 es que los instrumentos son débiles, así que un rechazo
# significa que los instrumento no son débiles, lo cual es bueno

# El F es bastante grande con los cual rehazamos H0 que los instrumentos no tienen efecto
# sobre tenure, con lo cual son relevantes

# Wu-Hausman: Es un test de endogeneidad, donde H0: Cov(educ, error) = 0. Rechazando H0
# indicate la existencia de endogeniedad la necesidad por variables instrumentales
# En otras palabras, prueba la consistencia de las estimaciones OLS bajo el supuesto que el
# IV es consistente. Cuando se rechaza H0, indica que OLS es no cosistente, sugiriendo
# que la endogeneidad es presente. Si no se rechaza H0, significa que OLS y IV son similares
# y la endogeneidad no es un problema

# Sargan: sirve para probar la validez de los instrumentos (los instrumentos no están
# correlacionados con los errores). Este test sólo puede calcularse si los instrumentos
# exceden el número de variables endógenas. Este test también es llamado test de
# restricciones de sobre-identificación. H0: Cov(z,error)=0. Lo bueno sería no rechazar

# Panel
# RE
iv_re <- plm(ln_wage ~ tenure + age + I(age2) + not_smsa |
                  union + south + age + I(age2) + not_smsa, 
             data = nlswork, model = "random")
summary(iv_re)

# Unobserved effects test: Ho: Var_unobservedeffects=0
pwtest(iv3_pool, data = nlswork)

# Breusch-Pagan test: Ho: Var_unobservedeffects=0
plmtest(iv3_pool, type="bp")

# FE
iv_fe <- plm(ln_wage ~ tenure + age + I(age2) + not_smsa |
               union + south + age + I(age2) + not_smsa, 
             data = nlswork, model = "within")
summary(iv_fe)

# Tests of poolability: Ho: all intercepts are equals
pooltest(iv3_pool, iv_fe)
pFtest(iv_fe, iv3_pool)

# Hausman test
phtest(iv_fe, iv_re)