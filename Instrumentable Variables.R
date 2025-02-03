# Regressão com Variáveis Instrumentais (IV)
# Estimação do impacto da participação feminina nas decisões financeiras
# sobre os gastos domiciliares, usando fchoice como instrumento


# 1. Carregar pacotes necessários 

library(dplyr)    # Manipulação de dados
library(AER)      # Implementação de variáveis instrumentais (ivreg)
library(sandwich) # Erros padrão robustos para testes de diagnóstico

# 2. Carregar e Preparar os Dados

# Substitua o endereço pelo destino onde o arquivo está salvo

hh_98.df <- read.dta("C:/Users/sergi/Downloads/hh_98.dta")

# Criar variáveis transformadas
hh_98.df <- mutate(hh_98.df, 
                   lexptot = log(1 + exptot),   # Log dos gastos totais
                   lnland = log(1 + hhland / 100)) # Log da terra possuída (normalização)

# Criar identificador único do vilarejo
hh_98.df <- mutate(hh_98.df, vill = thanaid * 10 + villid)

# Criar variável indicadora de empoderamento feminino local
hh_98.df <- group_by(hh_98.df, vill) %>%
  mutate(villmmf = max(dmmfd))

# Criar variável binária para escolha masculina baseada na posse de terra
hh_98.df <- mutate(hh_98.df, mchoice = ifelse(villmmf == 1 & hhland < 50, 1, 0))

# Criar interações entre mchoice e variáveis demográficas
var <- c("agehead", "sexhead", "educhead", "lnland", "vaccess", "pcirr", 
         "rice", "wheat", "milk", "potato", "egg", "oil")

for (i in 1:length(var)) {
  hh_98.df[[paste("mch", var[i], sep = "")]] <- hh_98.df$mchoice * hh_98.df[[var[i]]]
}

# Criar variável indicadora de empoderamento feminino a nível de vilarejo
hh_98.df <- group_by(hh_98.df, vill) %>%
  mutate(villfmf = max(dfmfd))

# Criar variável binária para escolha feminina baseada na posse de terra
hh_98.df <- mutate(hh_98.df, fchoice = ifelse(villfmf == 1 & hhland < 50, 1, 0))

# Criar interações entre fchoice e variáveis demográficas
for (i in 1:length(var)) {
  hh_98.df[[paste("fch", var[i], sep = "")]] <- hh_98.df$fchoice * hh_98.df[[var[i]]]
}

# 3. Estimação do modelo sem interações 

# Utiliza fchoice como instrumento para dfmfd


# 3.1. Primeira etapa (First Stage)
# Regressão de dfmfd em fchoice e variáveis exógenas

fsls <- lm(dfmfd ~ agehead + sexhead + educhead + lnland + vaccess + pcirr + 
             rice + wheat + milk + egg + oil + fchoice, data = hh_98.df)
summary(fsls)

# 3.2. Segunda etapa (IV regression)

# Estima o impacto de dfmfd sobre lexptot usando fchoice como instrumento
ivreg1 <- ivreg(lexptot ~ dfmfd + agehead + sexhead + educhead + lnland + vaccess + pcirr + 
                  rice + wheat + milk + egg + oil |
                  agehead + sexhead + educhead + lnland + vaccess + pcirr + 
                  rice + wheat + milk + egg + oil + fchoice, data = hh_98.df)
summary(ivreg1)


# 4. Estimação do modelo com interações

# Agora incluindo interações entre fchoice e variáveis demográficas


# 4.1. Primeira etapa (First Stage) 

# Inclui interações entre fchoice e covariáveis
fsls2 <- lm(dfmfd ~ agehead + sexhead + educhead + lnland + vaccess + pcirr + 
              rice + wheat + milk + potato + egg + oil + fchoice + 
              fchagehead + fchsexhead + fcheduchead + fchlnland + 
              fchvaccess + fchpcirr + fchrice + fchwheat + fchmilk + 
              fchegg + fchoil, data = hh_98.df)
summary(fsls2)

# 4.2. Segunda etapa (IV regression) 

# Omite potato devido à colinearidade observada na primeira etapa
ivreg2 <- ivreg(lexptot ~ dfmfd + agehead + sexhead + educhead + lnland + vaccess + pcirr + 
                  rice + wheat + milk + egg + oil + fchoice |
                  agehead + sexhead + educhead + lnland + vaccess + pcirr + 
                  rice + wheat + milk + egg + oil + fchoice +
                  fchagehead + fchsexhead + fcheduchead + fchlnland + 
                  fchvaccess + fchpcirr + fchrice + fchwheat + fchmilk + 
                  fchegg + fchoil, data = hh_98.df)
summary(ivreg2)

# 4.3. Testes de Diagnóstico 

# Avaliação da validade dos instrumentos e robustez dos resultados
summary(ivreg2, vcov = sandwich, df = Inf, diagnostics = TRUE)

