# Modelos de datos panel
library(haven); library(plm); library(tidyverse); library(summarytools)
#options(scipen = 999) # Force R not to use exponential notation (e.g. e+10)

rm(list=ls(all=TRUE)) # clear the environment

# Data
# nlswork: National Longitudinal Survey of Young Working Women
# Descripción de las variables: https://rdrr.io/rforge/sampleSelection/man/nlswork.html
nlswork <- read_dta("http://www.stata-press.com/data/r17/nlswork.dta") %>% # leemos la base de datos
           mutate(age2 = age*age) %>%     # construimos la variables age2
           select(idcode, year, ln_wage, age, age2, not_smsa, south) # seleccionando var
View(nlswork)
head(nlswork) # take a quick peak at the data
names(nlswork)

summary(nlswork)
dfSummary(nlswork, valid.col = FALSE, graph.col=F, silent=FALSE)

# Número de idcode y años
length(unique(nlswork$idcode))
length(unique(nlswork$year))

freq(nlswork$year)
freq(nlswork$idcode)

# Determinando si el panel se encuentra balanceado
pdim(nlswork)$balanced
is.pbalanced(nlswork)

# Balanceando el panel
nlswork_balanced <- make.pbalanced(nlswork, 
                                   balance.type = "shared.individuals",
                                   index = c("idcode","year"))
pdim(nlswork_balanced)$balanced

freq(nlswork_balanced$year)
freq(nlswork_balanced$idcode)

# Dando estructura panel a la data
nlswork_balanced <-  pdata.frame(nlswork_balanced, c("idcode","year"))

# Pool OLS
pool <- plm(ln_wage ~ age + age2 + not_smsa + south + factor(year), 
            data = nlswork_balanced, model = "pooling")
summary(pool)
print(summary(pool), subset = c("age", "age2", "not_smsa", "south"))

poolreg <- lm(ln_wage ~ age + age2 + not_smsa + south + factor(year), 
              data = nlswork_balanced)
summary(poolreg)
sum <- summary.lm(poolreg)
sum$coefficients <- sum$coefficients[1:5,]
print(sum)

# RE
re <- plm(ln_wage ~ age + age2 + not_smsa + south + factor(year), 
          data = nlswork_balanced, model = "random")
print(summary(re), subset = c("age", "age2", "not_smsa", "south"))

# Unobserved effects test
# The unobserved effects test a la Wooldridge (see Wooldridge (2010) 10.4.4), is a semiparametric test for
# the null hypothesis that Var_unobservedeffects=0, i.e. that there are no unobserved effects in the residuals.
pwtest(ln_wage ~ age + age2 + not_smsa + south, data = nlswork_balanced)

# Breusch-Pagan test: the LM test helps you decide between a random effects regression and a pooled OLS regression.
# The null hypothesis in the LM test is that variances across entities is zero. This is, no
# significant difference across units (i.e. no panel effect)
plmtest(pool, type="bp")

# FE
fe <- plm(ln_wage ~ age + age2 +  not_smsa + south, 
          data = nlswork_balanced, model = "within", effect = "twoway")
summary(fe)

# Tests of poolability
# Ho: all intercepts are equals
pooltest(pool, fe)
pFtest(fe, pool)

# Hausman test
phtest(fe, re)