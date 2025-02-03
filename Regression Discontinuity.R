# Regression Discontinuity

# Carregar bibliotecas necessárias
library(dplyr)   # Para manipulação de dados
library(ggplot2) # Para visualização de dados
library(foreign) # Para leitura de arquivos .dta (Stata)
library(rdrobust) # Para análise de Regressão Descontínua
library(boot)    # Para bootstrapping

# Carregar e Preparar os Dados

# Substitua o endereço pelo local onde o arquivo está salvo
hh_98.df <- read.dta("C:/Users/sergi/Downloads/hh_98.dta")

# Subsetting dos dados

# Filtrar observações onde 'hhland' é menor ou igual a 500
hh_98.df <- filter(hh_98.df, hhland <= 500)

# Transformações de variáveis
# Criar logaritmo das variáveis 'exptot' e 'hhland'
hh_98.df$lexptot <- log(1 + hh_98.df$exptot)  # Log da despesa total
hh_98.df$lnland <- log(1 + hh_98.df$hhland / 100)  # Log da área de terra

# Filtrar dados para homens e mulheres tratados
males_hh_98.df <- filter(hh_98.df, dmmfd == 1 | dmmfd == 0)  # Homens
females_hh_98.df <- filter(hh_98.df, dfmfd == 1)  # Mulheres

# Visualização da densidade de tratamento para homens e mulheres
ggplot(hh_98.df, aes(x = dmmfd)) + 
  geom_density() + 
  ggtitle("Densidade de Tratamento para Homens") + 
  xlim(-0.5, 1.5)
ggsave("male_density.png", width = 3, height = 3)  # Salvar gráfico

ggplot(hh_98.df, aes(x = dfmfd)) + 
  geom_density() + 
  ggtitle("Densidade de Tratamento para Mulheres") + 
  xlim(-0.5, 1.5)
ggsave("female_density.png", width = 3, height = 3)  # Salvar gráfico

# Análise de Regressão Descontínua (RDD) com rdrobust
# Configurar os dados para RDD
rdd_data <- rdrobust::rdrobust(y = hh_98.df$lexptot, x = hh_98.df$hhland, c = 50)

# Exibir resultados da RDD
summary(rdd_data)

# Plotar resultados da RDD
rdrobust::rdplot(y = hh_98.df$lexptot, x = hh_98.df$hhland, c = 50,
                 x.label = "hhland", y.label = "lexptot",
                 title = "Regressão Descontínua (RDD)")

# Bootstrapping para estimar a incerteza dos coeficientes

# Função para rodar rdrobust no bootstrap
rd <- function(data, i) {
  d <- data[i, ]
  fit <- rdrobust::rdrobust(y = d$lexptot, x = d$hhland, c = 50)
  return(fit$coef)  # Retornar coeficientes
}

# Rodar bootstrap com 1000 reamostragens
boot_results <- boot(hh_98.df, statistic = rd, R = 1000)
print(boot_results)  # Exibir resultados do bootstrap