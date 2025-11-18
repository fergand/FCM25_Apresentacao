################################################################################
## 1. CONFIGURAÇÃO E CARREGAMENTO DE DADOS
################################################################################

# Instalar pacotes (opcional, manter comentado se já instalados)
# install.packages(c("palmerpenguins", "tidyverse", "ggthemes", "tsibble", "feasts", "imputeTS", "knitr"))

# Carregar todas as bibliotecas no início (Boa Prática)
library(palmerpenguins)
library(tidyverse)
library(ggthemes); theme_set(theme_clean())
library(stringr)
library(tsibble)
library(feasts)
library(imputeTS)
library(knitr)

# Carregar dados
data_pinguins <- penguins_raw # Usar a versão BRUTA (com isótopos)
caminho_dados_clima <- "C:/Users/elias/OneDrive/Documentos/FCM_Thomas/projeto FCM/FCM25_Apresentacao/Data/PalmerStation_Daily_Weather.csv"
data_clima <- read_csv(caminho_dados_clima, show_col_types = FALSE)

################################################################################
## 2. PREPARAÇÃO DE DADOS (LIMPEZA E IMPUTAÇÃO)
################################################################################

### 2.1. Pinguins: Simplificação de Nomes
data_pinguins <- data_pinguins %>%
  mutate(
    # Simplificar nomes para melhor leitura de gráficos e tabelas
    Species = str_replace(Species, " \\(.*\\)", ""),
    Species = as.factor(Species)
  )

### 2.2. Clima: Correção de Estrutura e Imputação (Pré-Análise)
# Este bloco gera o dataset final limpo para ser usado pelo colega estatístico
data_clima_final <- data_clima %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  
  # Preencher lacunas implícitas no calendário
  tsibble::fill_gaps() %>%
  
  # Imputar Sea Ice (Categórica) com LOCF (Last Observation Carried Forward)
  tidyr::fill(`Sea Ice (WMO Code)`, .direction = "down") %>% 
  tidyr::fill(`Sea Ice (WMO Code)`, .direction = "up") %>% # Correção do primeiro NA
  
  # Re-aplicar a imputação nas variáveis numéricas (Interpolação Linear)
  mutate(
    `Temperature Average (C)` = imputeTS::na_interpolation(`Temperature Average (C)`, option = "linear"),
    `Sea Surface Temperature (C)` = imputeTS::na_interpolation(`Sea Surface Temperature (C)`, option = "linear")
  )

################################################################################
## 3. ANÁLISE EXPLORATÓRIA DE DADOS 
################################################################################

### 3.1. Inspeção de Qualidade e NAs (Final da Preparação)

# Inspecionar estrutura e tipos
print("--- 3.1. Inspeção de Pinguins ---")
glimpse(data_pinguins)
print("--- Inspeção de Clima Final ---")
glimpse(data_clima_final)

# Quantificar NAs nos Pinguins (para variáveis chave)
na_percentage_pinguins <- data_pinguins %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "Variavel", values_to = "Percentual_NA") %>%
  filter(Percentual_NA > 0)
print("--- NAs Remanescentes (Pinguins) ---")
print(na_percentage_pinguins)

# Verificação de NAs no Clima (Deve ser ZERO)
print("--- Verificação Final de NAs (Clima) ---")
data_clima_final %>%
  summarise(
    Temp_Media = sum(is.na(`Temperature Average (C)`)),
    Gelo_Marinho = sum(is.na(`Sea Ice (WMO Code)`))
  )

### 3.2. Análise Descritiva: Resumo Numérico (Gerando Tabelas)

