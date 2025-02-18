# Carregar pacotes necessários
library(dplyr)
library(haven)  # Para carregar arquivos .dta
library(survey)
library(plm)  # Para modelos de painel
library(MatchIt)  # Para Propensity Score Matching
library(foreign)    # Para carregar arquivos .dta

# Carregar os dados
hh_9198 <- read.dta("C:/Users/sergi/Downloads/hh_9198.dta")

# Criar log do gasto total e log da terra possuída
hh_9198 <- hh_9198 %>%
  mutate(lexptot = log(1 + exptot),
         lnland = log(1 + hhland / 100))  # Transformação da terra possuída

# Criar variável do gasto total apenas para 1991
hh_9198 <- hh_9198 %>%
  group_by(nh) %>%
  mutate(exptot91 = ifelse(year == 0, exptot, NA)) %>%
  summarise(exptot91 = max(exptot91, na.rm = TRUE)) %>%
  right_join(hh_9198, by = "nh") %>%
  mutate(lexptot91 = log(1 + exptot91),
         lexptot9891 = lexptot - lexptot91)

# Teste t para diferença de médias entre os grupos
t.test(lexptot9891 ~ dfmfd, data = hh_9198)

# Criar variáveis de tratamento e interação
hh_9198 <- hh_9198 %>%
  mutate(dfmfd1 = ifelse(dfmfd == 1 & year == 1, 1, 0)) %>%
  group_by(nh) %>%
  mutate(dfmfd98 = max(dfmfd1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dfmfdyr = dfmfd98 * year)

# Regressão de Dupla Diferença (DiD)
DiD_model <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = hh_9198)
summary(DiD_model)

# Regressão com covariáveis adicionais
DiD_cov_model <- lm(lexptot ~ year + dfmfd98 + dfmfdyr + sexhead + agehead + 
                      educhead + lnland + vaccess + pcirr + rice + wheat + 
                      milk + oil + egg, weights = weight, data = hh_9198)
summary(DiD_cov_model)

# Modelo de efeitos fixos no nível do agregado (nh)
DiD_fe_model <- plm(lexptot ~ year + dfmfd98 + dfmfdyr,
                    data = hh_9198, index = "nh", model = "within")
summary(DiD_fe_model)

# Modelo com covariáveis
DiD_fe_cov_model <- plm(lexptot ~ year + dfmfd98 + dfmfdyr + sexhead + agehead + 
                          educhead + lnland + vaccess + pcirr + rice + wheat + 
                          milk + oil + egg, 
                        data = hh_9198, index = "nh", model = "within")
summary(DiD_fe_cov_model)


