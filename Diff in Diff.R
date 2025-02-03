# Diff in Diff com PSM

# Carregar bibliotecas necessárias

library(dplyr)   # Para manipulação de dados
library(foreign) # Para leitura de arquivos .dta (Stata)
library(ggplot2) # Para visualização de dados
library(plm)     # Para modelos de painel
library(Matching) # Para Propensity Score Matching (PSM)
library(survey)  # Para análise com pesos

# Carregar dados

# Substitua o endereço pelo destino onde o arquivo está salvo

hh_9198.df <- read.dta("C:/Users/sergi/Downloads/hh_9198.dta")


###########
# Subset e Transformações
###########

# Criar variáveis necessárias para o Diff-in-Diff
hh_9198.df <- hh_9198.df %>%
  mutate(
    lexptot = log(1 + exptot),  # Log da despesa total
    lnland = log(1 + hhland / 100),  # Log da área de terra
    dmmfd1 = ifelse(dmmfd == 1 & year == 1, 1, 0),  # Tratamento para homens em 1998
    dfmfd1 = ifelse(dfmfd == 1 & year == 1, 1, 0)   # Tratamento para mulheres em 1998
  ) %>%
  group_by(nh) %>%
  mutate(
    dmmfd98 = max(dmmfd1),  # Indicador de tratamento para homens no nível do domicílio
    dfmfd98 = max(dfmfd1)   # Indicador de tratamento para mulheres no nível do domicílio
  ) %>%
  ungroup() %>%
  mutate(
    dmmfdyr = dmmfd98 * year,  # Interação entre tratamento e ano para homens
    dfmfdyr = dfmfd98 * year   # Interação entre tratamento e ano para mulheres
  )

###########################
# Diff-in-Diff Regression
###########################

# Modelo Básico de Diff-in-Diff
lm_basic <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = hh_9198.df)
summary(lm_basic)

# Modelo com Efeitos Fixos no nível do domicílio (nh)
lm_fe <- lm(lexptot ~ year + dfmfdyr + factor(nh), data = hh_9198.df)
summary(lm_fe)

# Usar plm para modelo de efeitos fixos
plm_fe <- plm(lexptot ~ year + dfmfdyr, data = hh_9198.df, model = "within", index = "nh")
summary(plm_fe)

###############
# PSM com Diff-in-Diff
###############

# Preparação dos dados para PSM
hh_9198_psm <- hh_9198.df %>%
  filter(year == 0) %>%  # Usar apenas o ano base (1991) para o PSM
  mutate(
    X = 1:nrow(.)  # Criar um índice único para cada observação
  )

# Modelo Probit para estimar o Propensity Score
prog.lm <- glm(dfmfd98 ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,
               data = hh_9198_psm, family = binomial(link = "probit"))

summary(prog.lm)

# Extrair os Propensity Scores
X <- prog.lm$fitted.values
Tr <- hh_9198_psm$dfmfd98

# Realizar o Matching com caliper de 0.01
m.out <- Match(Tr = Tr, X = X, caliper = 0.01)
summary(m.out)

# Verificar o balanceamento após o Matching
MatchBalance(dfmfd98 ~ sexhead + agehead + educhead + lnland + vaccess + pcirr + rice + wheat + milk + oil,
             data = hh_9198_psm, nboots = 1000)

# Visualizar a densidade dos Propensity Scores
fit.control <- filter(hh_9198_psm, dfmfd98 == 0)
fit.treated <- filter(hh_9198_psm, dfmfd98 == 1)

ggplot() +
  geom_density(aes(x = fit.control$dfmfd98, linetype = 'Control')) +
  geom_density(aes(x = fit.treated$dfmfd98, linetype = 'Treated')) +
  xlim(-0.3, 1) +
  xlab("Propensity Score") +
  scale_linetype_discrete(name = "Grupo") +
  ggtitle("Densidade dos Propensity Scores para Controle e Tratados")

# Criar base de dados com os matches
ps_dropped <- m.out$index.dropped
ps_hh_9198.df <- data.frame(psm = prog.lm$fitted.values, nh = hh_9198_psm$nh)
ps_hh_9198.df <- ps_hh_9198.df[-ps_dropped, ]

# Juntar os dados originais com os Propensity Scores
psm_hh_9198.df <- right_join(hh_9198.df, ps_hh_9198.df, by = "nh")

# Re-estimar o modelo Diff-in-Diff com os dados matched
psm_hh_9198.df <- psm_hh_9198.df %>%
  mutate(
    lexptot = log(1 + exptot),
    lnland = log(1 + hhland / 100),
    dfmfd1 = ifelse(dfmfd == 1 & year == 1, 1, 0),
    dfmfd98 = max(dfmfd1),
    dfmfdyr = dfmfd98 * year
  )

# Modelo Diff-in-Diff com dados matched
lm_psm <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = psm_hh_9198.df)
summary(lm_psm)

# Adicionar pesos analíticos e re-estimar
psm_hh_9198.df <- psm_hh_9198.df %>%
  mutate(
    a_weight = ifelse(dfmfd == 0, psm / (1 - psm), 1)
  )

lm_psm_weighted <- lm(lexptot ~ year + dfmfd98 + dfmfdyr, data = psm_hh_9198.df, weights = a_weight)
summary(lm_psm_weighted)