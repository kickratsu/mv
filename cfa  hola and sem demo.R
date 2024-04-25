




























































# Load required packages
library(lavaan)
library(semPlot)


# Specify the model
HS.model <- ' 
  visual  =~ x1 + x2 + x3      
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9 
'

# Fit the model
fit <- cfa(HS.model, data = HolzingerSwineford1939)

summary(fit, fit.measures = TRUE)

# Visualize the model
semPaths(fit, whatLabels = "est", layout = "tree2", rotation = 2)
#########################################################################
#sem




library(lavaan)
library(semPlot)

model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'
fit <- sem(model, data = PoliticalDemocracy)
summary(fit, standardized = TRUE)
semPaths(fit, whatLabels = "est", layout = "tree2", rotation = 2)

