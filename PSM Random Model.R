# PSM Random Model

library(dplyr)
library(survey)
library(foreign)    # Para carregar arquivos .dta

# Carregando os dados
# Substitua o endereço pelo destino onde o arquivo está salvo

hh_98.df <- read.dta("C:/Users/sergi/Downloads/hh_98.dta")


# Pré-processamento dos dados

# Transformar variáveis, criar identificador único de vill e calcular valores máximos de indicadores por vill.

hh_98.df <- mutate(hh_98.df, lexptot = log(1 + exptot)) %>%
  mutate(lnland = log((1 + hhland/100))) %>%
  mutate(vill = thanaid * 10 + villid) %>%
  group_by(vill) %>%
  mutate(progvillm = max(dmmfd), progvillf = max(dfmfd))

# Testes de impacto da localização do programa

# Teste t para verificar diferenças de médias
attach(hh_98.df)
t.test(lexptot ~ progvillf, var.equal = TRUE)
t.test(lexptot ~ progvillm, var.equal = TRUE)
detach(hh_98.df)

# Criando desenho amostral

des1 <- svydesign(id = ~weight, weights = ~weight, data = hh_98.df)

# Regressão 1: Impacto da presença do programa na vila (Homens)
prog_place_1.lm <- lm(lexptot ~ progvillm, data = hh_98.df)
summary(prog_place_1.lm)

# Regressão 2: Impacto da presença do programa na vila (Mulheres)
prog_place_2.lm <- lm(lexptot ~ progvillf, data = hh_98.df)
summary(prog_place_2.lm)

# Regressão 3: Ajustando para variáveis de controle (Homens)
prog_place_3.svyglm <- svyglm(lexptot ~ progvillm + sexhead + agehead + educhead + lnland + vaccess + 
                                pcirr + rice + wheat + milk + oil + egg, design = des1)
summary(prog_place_3.svyglm)

# Regressão 4: Ajustando para variáveis de controle (Mulheres)
prog_place_4.svyglm <- svyglm(lexptot ~ progvillf + sexhead + agehead + educhead + lnland + vaccess + 
                                pcirr + rice + wheat + milk + oil + egg, design = des1)
summary(prog_place_4.svyglm)


# Regressão 5: Efeito da participação masculina
prog_part_1.lm <- lm(lexptot ~ dmmfd, data = hh_98.df)
summary(prog_part_1.lm)

# Regressão 6: Efeito da participação feminina
prog_part_2.lm <- lm(lexptot ~ dfmfd, data = hh_98.df)
summary(prog_part_2.lm)

# Regressão 7: Ajustando para variáveis de controle (Homens)
prog_part_3.svyglm <- svyglm(lexptot ~ dmmfd + sexhead + agehead + educhead + lnland + vaccess + 
                               pcirr + rice + wheat + milk + oil + egg, design = des1)
summary(prog_part_3.svyglm)

# Regressão 8: Ajustando para variáveis de controle (Mulheres)
prog_part_4.svyglm <- svyglm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
                               pcirr + rice + wheat + milk + oil + egg, design = des1)
summary(prog_part_4.svyglm)

# Regressão 9: Impacto combinado da localização e participação (Homens)
prog_place_part_1.svyglm <- svyglm(lexptot ~ dmmfd + progvillm + sexhead + agehead + educhead +
                                     lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
                                   design = des1)
summary(prog_place_part_1.svyglm)

# Regressão 10: Impacto combinado da localização e participação (Mulheres)
prog_place_part_2.svyglm <- svyglm(lexptot ~ dfmfd + progvillm + sexhead + agehead + educhead +
                                     lnland + vaccess + pcirr + rice + wheat + milk + oil + egg, 
                                   design = des1)
summary(prog_place_part_2.svyglm)


# Impactos dentro das vilas com programa


# Criando amostras filtradas
progvill_1 <- filter(hh_98.df, progvillm == 1)
des2 <- svydesign(id = ~weight, weights = ~weight, data = progvill_1)

progvill_2 <- filter(hh_98.df, progvillf == 1)
des3 <- svydesign(id = ~weight, weights = ~weight, data = progvill_2)

# Regressão 11: Impacto da participação dentro da vila (Homens)
progvill_1.lm <- lm(lexptot ~ dmmfd, data = progvill_1)
summary(progvill_1.lm)

# Regressão 12: Impacto da participação dentro da vila (Mulheres)
progvill_2.lm <- lm(lexptot ~ dmmfd, data = progvill_2)
summary(progvill_2.lm)

# Regressão 13: Ajustando para variáveis de controle (Homens)
progvill_3.svyglm <- svyglm(lexptot ~ dmmfd + sexhead + agehead + educhead + lnland + vaccess + 
                              pcirr + rice + wheat + milk + oil + egg, design = des2)
summary(progvill_3.svyglm)

# Regressão 14: Ajustando para variáveis de controle (Mulheres)
progvill_4.svyglm <- svyglm(lexptot ~ dfmfd + sexhead + agehead + educhead + lnland + vaccess + 
                              pcirr + rice + wheat + milk + oil + egg, design = des3)
summary(progvill_4.svyglm)


# Efeitos de Spillover


progplace_1 <- filter(hh_98.df, dmmfd == 0)
des4 <- svydesign(id = ~weight, weights = ~weight,data = progplace_1)

progplace_2 <- filter(hh_98.df, dfmfd == 0)
des5 <- svydesign(id = ~weight, weights = ~weight,data = progplace_2)

# Regressão 15: Spillover masculino
progplace_1.lm <- lm(lexptot ~ progvillm, data = progplace_1)
summary(progplace_1.lm)

# Regressão 16: Spillover feminino
progplace_2.lm <- svyglm(lexptot ~ progvillf, design = des5)
summary(progplace_2.lm)
