# 📌 Carregando pacotes necessários
library(tidyverse)   # Manipulação de dados
library(haven)       # Leitura de arquivos Stata (.dta)
library(Synth)       # Método de controle sintético
library(devtools)    # Ferramentas para pacotes R
library(SCtools)     # Ferramentas para testes placebo no SCM

# Função para ler os dados diretamente do GitHub do autor
read_data <- function(df) {
  full_path <- paste0("https://github.com/scunning1975/mixtape/raw/master/", df)
  df <- read_dta(full_path)
  return(df)
}

# Importando os dados do Texas
texas <- read_data("texas.dta") %>%
  as.data.frame(.)  # Convertendo para data.frame

# Preparando os dados para o método de Controle Sintético
dataprep_out <- dataprep(
  foo = texas,  # Base de dados
  predictors = c("poverty", "income"),  # Variáveis preditoras principais
  predictors.op = "mean",  # Operação usada nas preditoras (média)
  time.predictors.prior = 1985:1993,  # Período pré-tratamento para otimizar pesos
  
  special.predictors = list(  # Variáveis adicionais com períodos específicos
    list("bmprison", c(1988, 1990:1992), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990:1991, "mean"),
    list("black", 1990:1992, "mean"),
    list("perc1519", 1990, "mean")
  ),
  
  dependent = "bmprison",  # Variável dependente: taxa de encarceramento
  unit.variable = "statefip",  # Identificador dos estados
  unit.names.variable = "state",  # Nome dos estados
  time.variable = "year",  # Variável de tempo
  
  treatment.identifier = 48,  # Texas como unidade tratada
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),  # Estados de controle
  
  time.optimize.ssr = 1985:1993,  # Período pré-tratamento para ajuste do modelo
  time.plot = 1985:2000  # Período de análise do estudo
)

# Rodando o método de Controle Sintético
synth_out <- synth(data.prep.obj = dataprep_out)

# Gráfico da trajetória da variável dependente: Texas real vs. Texas sintético
path.plot(synth_out, dataprep_out)

# Gráfico das diferenças (GAP) entre Texas real e Texas sintético
gaps.plot(synth_out, dataprep_out)

# Gerando placebos (falsos tratamentos em estados de controle)
placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 3)

# Plotando os placebos para ver a distribuição do efeito
plot_placebos(placebos)

# Teste de significância: Média do Erro Quadrático de Previsão (MSPE)
mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)
