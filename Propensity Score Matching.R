# PROPENSITY SCORE MATCHING (PSM)
# Seguindo o Handbook on Impact Evaluation: Quantitative Methods and Practices

# Carregar pacotes necessários
library(dplyr)
library(foreign)
library(Matching)
library(MatchIt)
library(ggplot2)
library(survey)
library(MatchIt)

# 1. Carregar e Preparar os Dados

# Carregar os dados
hh_98.df <- read.dta("C:/Users/sergi/Downloads/hh_98.dta")

# Criar variáveis log-transformadas para normalizar a distribuição
hh_98.df <- hh_98.df %>%
  mutate(
    lexptot = log(1 + exptot),  # Log do gasto total
    lnland = log(1 + hhland / 100)  # Log da terra da família
  )

# 2. Estimando o Escore de Propensão (Probit)

# Criar um desenho amostral considerando pesos da pesquisa
des1 <- svydesign(id = ~weight, weights = ~weight, data = hh_98.df)

# Modelo de regressão Probit para estimar o escore de propensão
prog.lm <- svyglm(
  dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
  design = des1, 
  family = quasibinomial(link = "probit")
)

# Obter os valores ajustados (escore de propensão)
X <- prog.lm$fitted
Tr <- hh_98.df$dmmfd
Y <- hh_98.df$lexptot

# 3. Matching via Vizinho Mais Próximo

m.out <- Match(Tr = Tr, X = X, Y = Y, caliper = 0.001)
summary(m.out)


# 4. Visualizar Distribuição do Escore de Propensão

fit <- prog.lm$data
fit$fvalues <- prog.lm$fitted.values 

# Separando grupos de controle e tratados
fit.control <- filter(fit, dmmfd == 0)
fit.treated <- filter(fit, dmmfd == 1)

# 5. Matching com Suporte Comum

m.out <- Match(Tr = Tr, X = X, Y = Y, caliper = 0.001, M = 1, CommonSupport = TRUE)
summary(m.out)

# 6. Pareamento por Gênero

# 6.1 Matching para Homens
prog.lm <- svyglm(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil, 
                  design = des1, family = quasibinomial(link = "probit"))

X <- prog.lm$fitted.values
m.out <- Match(Tr = Tr, X = X, Y = Y, M = 1, caliper = 0.001, replace = TRUE)
summary(m.out)

# 6.2 Matching para Mulheres
glm.female <- glm(dfmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil, 
                  family = binomial, data = hh_98.df)

X <- glm.female$fitted
m.out <- Match(Tr = Tr, X = X, Y = Y, caliper = 0.001, M = 1, replace = TRUE)
summary(m.out)

# 7. Usando pscore() para estimar escore de propensão 

psm_m.lm <- matchit(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,
                    data = hh_98.df, 
                    method = "nearest",   # Método de emparelhamento: "nearest" (mais próximo)
                    distance = "logit")   # Tipo de modelo para calcular os escores de propensão (logit)

# Resumo do modelo de escore de propensão
summary(psm_m.lm)

# Realizando o pareamento usando o escore de propensão estimado
psm_m.match <- matchit(dmmfd ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,
                       data = hh_98.df, 
                       method = "nearest",  # Método de emparelhamento: "nearest" (mais próximo)
                       caliper = 0.001)     # Caliper para restringir a diferença máxima entre os escores de propensão emparelhados


# Resumo do pareamento
summary(psm_m.match)
