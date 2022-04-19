# Modelos dinámicos con datos de panel
# Se utilizan los datos de Arellano y Bond (1991)
# Se estima una ecuación de empleo utilizando una muestra de empresas en UK
# Se tiene información para 140 empresas en el periodo 1976 a 1984, en un panel desbalanceado
# emp: empleo; wage: salarios; capital: capital; output: producto 

library(plm); library(tidyverse); library(summarytools)
library(modelsummary); library(gt); library(pdynmc)

# Se considera la siguiente ecuaci?n de empleo din?mica
# ln(empleo_it) = a1 ln(empleo_it-1) + a2 ln(empleo_it-2) +
#                 b1 ln(wage_it) + b2 ln(wage_it-1) +
#                 c1 ln(capital_it) + c2 ln(capital_it-1) + c3 ln(capital_it-2) +
#                 d1 ln(output_it) + d2 ln(output_it-1) + d3 ln(output_it-2) + e_it

data("EmplUK", package = "plm")
data <- EmplUK
data[,c(4:7)] <- log(data[,c(4:7)])
data <- pdata.frame(data, c("firm","year"))

## OLS
OLS <- plm(emp ~ lag(emp, 1) + lag(emp, 2) +
            wage + lag(wage, 1) +
            capital + lag(capital, 1) + lag(capital, 2) +
            output + lag(output, 1) + lag(output, 2), 
            model = "within", effect = "twoway", data=data)
summary(OLS)

data$L1.emp <- plm::lag(data$emp, 1)
data$L2.emp <- plm::lag(data$emp, 2)
data$L0.wage <- data$wage
data$L1.wage <- plm::lag(data$wage, 1)
data$L0.capital <- data$capital
data$L1.capital <- plm::lag(data$capital, 1)
data$L2.capital <- plm::lag(data$capital, 2)
data$L0.output <- data$output
data$L1.output <- plm::lag(data$output, 1)
data$L2.output <- plm::lag(data$output, 2)

OLS <- plm(emp ~ L1.emp + L2.emp +
             L0.wage + L1.wage +
             L0.capital + L1.capital + L2.capital +
             L0.output + L1.output + L2.output, 
           model = "within", effect = "twoway", data=data)
summary(OLS)
nobs(OLS)
nrow(data[complete.cases(data), ])

## Arellano and Bond (1991) estimation in Table 4, column (a1)

m1 <- pdynmc(dat = data, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE,    # incluir los instrumentos en diferencias 
             use.mc.lev = FALSE,    # no incluir los instrumento en niveles  
             use.mc.nonlin = FALSE, # no incluir instrumento en formas no lineales (cuadrados)
             include.y = TRUE,      # instrumentos derivados de los lag var dependiente
             varname.y = "emp",     # nombre de la var dependiente
             lagTerms.y = 2,        # se incluyen dos rezagos de emp
             fur.con = TRUE,        # se incluyen covariables 
             fur.con.diff = TRUE,   # incluir las covariables en diferencias
             fur.con.lev = FALSE,   # no incluir las covariables en niveles
             varname.reg.fur = c("wage", "capital", "output"), # Covariables
             lagTerms.reg.fur = c(1,2,2), # num de rezagos en las covariables
             include.dum = TRUE,    # incluir variables dummies de año 
             dum.diff = TRUE,       # incluir variables dummies en primeras diferencias  
             dum.lev = FALSE,       # no incluir variables dummies en niveles 
             varname.dum = "year",  # variable de años
             w.mat = "iid.err",     # indica el tipo de matriz de pesos utilizada
             std.err = "corrected", # corrección en los errores estándar
             estimation = "onestep",# num de iteraciones en el procedimiento
             opt.meth = "none")     # proceso de optimización numérica
summary(m1)
x <- summary(m1)
nrow(x$data$dat.na[complete.cases(x$data$dat.na),])

## Arellano and Bond (1991) estimation in Table 4, column (a2)
m2 <- pdynmc(dat = data, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE,    # incluir los instrumentos en diferencias 
             use.mc.lev = FALSE,    # no incluir los instrumento en niveles  
             use.mc.nonlin = FALSE, # no incluir instrumento en formas no lineales (cuadrados)
             include.y = TRUE,      # instrumentos derivados de los lag var dependiente
             varname.y = "emp",     # nombre de la var dependiente
             lagTerms.y = 2,        # se incluyen dos rezagos de emp
             fur.con = TRUE,        # se incluyen covariables 
             fur.con.diff = TRUE,   # incluir las covariables en diferencias
             fur.con.lev = FALSE,   # no incluir las covariables en niveles
             varname.reg.fur = c("wage", "capital", "output"), # Covariables
             lagTerms.reg.fur = c(1,2,2), # num de rezagos en las covariables
             include.dum = TRUE,    # incluir variables dummies de año 
             dum.diff = TRUE,       # incluir variables dummies en primeras diferencias  
             dum.lev = FALSE,       # no incluir variables dummies en niveles 
             varname.dum = "year",  # variable de años
             w.mat = "iid.err",     # indica el tipo de matriz de pesos utilizada
             std.err = "corrected", # corrección en los errores estándar
             estimation = "twostep",# num de iteraciones en el procedimiento
             opt.meth = "none")     # proceso de optimización numérica
summary(m2)
#y <- summary(m2)
#nrow(y$data$dat.na[complete.cases(y$data$dat.na),])

## Editar en tabla con modelsummary
# Como modelsummary no soporta directamente 'pdynmc' se debe generar un procedimiento
# adicional para que lo soporte
# Ver https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

tidy.pdynmc <- function(x, ...) {
  s <- summary(x, ...)
  ret <- data.frame(
    term      = row.names(s$coefficients),
    estimate  = s$coefficients[, 1],
    std.error = s$coefficients[, 2],
    p.value   = s$coefficients[, 4])
  ret
}

glance.pdynmc <- function(x, ...) {
  s <- summary(x, ...)
  ret <- data.frame(
    JTest = s$hansenj[["statistic"]],
    pval  = s$hansenj[["p.value"]])
  ret
}

tidy_AB_one <- tidy.pdynmc(m1)
glance_AB_one <- glance.pdynmc(m1)
tidy_AB_two <- tidy.pdynmc(m2)
glance_AB_two <- glance.pdynmc(m2)

AB_one <- list(
  tidy = tidy_AB_one,
  glance = glance_AB_one)

AB_two <- list(
  tidy = tidy_AB_two,
  glance = glance_AB_two)

class(AB_one) <- "modelsummary_list"
class(AB_two) <- "modelsummary_list"

models <- list("OLS" = OLS,
               "AB_one" = AB_one,
               "AB_two" = AB_two)

modelsummary(models, 
             output="gt",
             coef_map = c('L1.emp' = 'Emp(-1)',
                          'L2.emp' = 'Emp(-2)',
                          'L0.wage' = 'Wage',
                          'L1.wage' = 'Wage(-1)',
                          'L0.capital' = 'Capital',
                          'L1.capital' = "Capital(-1)",
                          'L2.capital' = 'Capital(-2)',
                          'L0.output' = 'Output',
                          'L1.output' = "Output(-1)",
                          'L2.output' = 'Output(-2)'),
             statistic = "std.error",
             title = "Table 1. Employment equations",
             gof_omit='IC|Log|F|R2',
             stars = c('*'=.1,'**'=.05,'***'=.01)) |> 
  tab_style(style = cell_text(color='red'), 
            location = cells_body(row=1:4)) |>
  tab_style(style = cell_text(color='black', size='x-small'), locations = cells_source_notes()) |> 
  tab_source_note(source_note = "Note: standard errors in parentheses")
