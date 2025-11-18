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

# 1. Gerar o resumo numérico completo (base para todas as tabelas)
summary_numerico <- data_pinguins %>%
  select(ends_with("(mm)"), ends_with("(g)"), starts_with("Delta")) %>%
  summarise(
    across(everything(), list(
      Min = ~min(., na.rm = TRUE), Median = ~median(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE), NAs = ~sum(is.na(.))
    ))
  )
summary_numerico_longo <- summary_numerico %>%
  pivot_longer(
    everything(), names_to = c("Variável", "Estatística"), names_sep = "_", values_to = "Valor"
  )

# 2. Tabela 1: Biometria (Massa e Nadadeira)
tabela_biometria_resumo <- summary_numerico_longo %>%
  filter(Variável %in% c("Body Mass (g)", "Flipper Length (mm)")) %>%
  filter(Estatística %in% c("Min", "Median", "Max", "NAs")) %>%
  pivot_wider(names_from = Estatística, values_from = Valor) %>%
  select(Variável, Mínimo = Min, Mediana = Median, Máximo = Max, NAs)
print("--- Tabela 1: Estatísticas de Tamanho ---")
knitr::kable(tabela_biometria_resumo, digits = 1)


#...  Gerar Tabela com Estatísticas Descritivas Consolidadas

library(dplyr)
library(tidyr)
library(knitr)

# 1. Definir o vetor 'variaveis_essenciais' (CORREÇÃO DO ERRO)
variaveis_essenciais <- c(
  "Body Mass (g)", 
  "Flipper Length (mm)", 
  "Culmen Length (mm)", 
  "Culmen Depth (mm)",
  "Delta 15 N (o/oo)", 
  "Delta 13 C (o/oo)"
)

# Gerar a Tabela Resumo consolidada
tabela_resumo_consolidada <- summary_numerico_longo %>%
  # 1. Incluir todas as 6 variáveis essenciais
  filter(Variável %in% variaveis_essenciais) %>%
  
  # 2. Filtrar as estatísticas (Linhas) que você deseja manter
  filter(Estatística %in% c("Min", "Median", "Max", "NAs")) %>%
  
  # 3. Transformar para o formato 'wide'
  pivot_wider(
    names_from = Estatística,
    values_from = Valor
  ) %>%
  
  # 4. Reordenar e Renomear as colunas
  select(
    Variável, 
    Mínimo = Min, 
    Mediana = Median, 
    Máximo = Max, 
    NAs
  ) %>%
  # Opcional: Reordenar as linhas por categoria (Tamanho > Morfologia > Dieta)
  arrange(desc(Mínimo)) 

print("--- Tabela 1: Estatísticas Descritivas Consolidadas ---")
knitr::kable(
  tabela_resumo_consolidada, 
  caption = "Estatísticas Descritivas Consolidadas das Variáveis Biométricas, Morfológicas e Tróficas",
  digits = 2
)


### 3.3. Análise de Frequência (Tabelas Categóricas)

# Tabela: Espécies por Ilha
tabela_especie_ilha <- data_pinguins %>%
  drop_na(Species, Island) %>%
  count(Species, Island, name = "Contagem") %>%
  mutate(Percentual = (Contagem / sum(Contagem)) * 100)
print("--- Tabela de Frequência: Espécies por Ilha ---")
print(tabela_especie_ilha)

# Tabela Espécies por Ilha (Com Formatação Kable)

library(dplyr)
library(knitr)

# Tabela de Frequência: Espécies por Ilha
tabela_especie_ilha <- data_pinguins %>%
  drop_na(Species, Island) %>%
  count(Species, Island, name = "Contagem") %>%
  mutate(
    Percentual = (Contagem / sum(Contagem)) * 100
  ) %>%
  arrange(Contagem)
#arrange(desc(Contagem))
#arrange(Island, desc(Contagem))

print("--- Tabela de Frequência: Espécies por Ilha ---")
knitr::kable(
  tabela_especie_ilha,
  caption = "Distribuição de Frequência das Espécies de Pinguins por Ilha",
  digits = 2 # Duas casas decimais para a porcentagem
)


# Tabela: Sexo por Espécie
tabela_sexo_especie <- data_pinguins %>%
  drop_na(Species, Sex) %>%
  count(Species, Sex, name = "Contagem") %>%
  group_by(Species) %>%
  mutate(Percentual_Especie = (Contagem / sum(Contagem)) * 100)
print("--- Tabela Cruzada: Sexo por Espécie ---")
print(tabela_sexo_especie)

library(dplyr)
library(knitr)

# Tabela Tabela Sexo por Espécie (Com Formatação Kable)

tabela_sexo_especie <- data_pinguins %>%
  drop_na(Species, Sex) %>%
  
  # Contar observações para a tabela cruzada
  count(Species, Sex, name = "Contagem") %>%
  
  # Calcular a porcentagem DENTRO de cada espécie
  group_by(Species) %>%
  mutate(Percentual_Especie = (Contagem / sum(Contagem)) * 100) %>%
  ungroup() %>%
  
  # Ordenar por espécie para fácil comparação
  #arrange(Species, desc(Contagem))
  arrange(Species, (Contagem))

print("--- Tabela Cruzada: Sexo por Espécie ---")
knitr::kable(
  tabela_sexo_especie,
  caption = "Tabela Cruzada: Contagem e Percentual de Sexo por Espécie",
  digits = 2 # Duas casas decimais para o percentual
)



# Tabela 4: Resumo Biométrico e Trófico POR ESPÉCIE
tabela_resumo_por_especie <- data_pinguins %>%
  # Remove NAs para garantir médias precisas
  drop_na(`Body Mass (g)`, `Culmen Length (mm)`, `Delta 15 N (o/oo)`) %>% 
  
  # Agrupar pela variável central (Species)
  group_by(Species) %>%
  
  # Calcular estatísticas descritivas para cada grupo
  summarise(
    N = n(), # Contagem de observações no grupo
    Massa_Media_g = mean(`Body Mass (g)`),
    Comprimento_Bico_Media_mm = mean(`Culmen Length (mm)`),
    Delta_15N_Media = mean(`Delta 15 N (o/oo)`),
    Delta_13C_Media = mean(`Delta 13 C (o/oo)`)
  ) %>%
  ungroup()

print("--- Tabela 4: Médias Biométricas e Tróficas por Espécie ---")
print(tabela_resumo_por_especie)



# Tabela 4: Resumo Biométrico e Trófico POR ESPÉCIE (Com Formatação Kable)
# ... (Rodar código de summarise) ...
tabela_resumo_por_especie <- data_pinguins %>%
  drop_na(`Body Mass (g)`, `Culmen Length (mm)`, `Delta 15 N (o/oo)`) %>%
  group_by(Species) %>%
  summarise(
    N = n(),
    Massa_Media_g = mean(`Body Mass (g)`),
    Comprimento_Bico_Media_mm = mean(`Culmen Length (mm)`),
    Delta_15N_Media = mean(`Delta 15 N (o/oo)`),
    Delta_13C_Media = mean(`Delta 13 C (o/oo)`)
  ) %>%
  ungroup()

# Aplica a formatação de relatório (Kable)
print("--- Tabela 4: Médias Biométricas e Tróficas por Espécie ---")
knitr::kable(
  tabela_resumo_por_especie,
  caption = "Médias Biométricas e Tróficas por Espécie",
  digits = 2
)



library(dplyr)
library(knitr)

# Tabela 6: Resumo Detalhado por Espécie e Sexo
tabela_dimorfismo_detalhado <- data_pinguins %>%
  # Remove NAs nas variáveis biométricas e Sex
  drop_na(`Body Mass (g)`, `Culmen Length (mm)`, Sex) %>%
  
  # Agrupar pela variável central (Species) E Sexo
  group_by(Species, Sex) %>%
  
  # Calcular estatísticas descritivas para cada grupo (Média das principais)
  summarise(
    N = n(),
    Massa_Media_g = mean(`Body Mass (g)`),
    Comprimento_Bico_Media_mm = mean(`Culmen Length (mm)`),
    .groups = 'drop'
  )

print("--- Tabela 6: Médias Biométricas Detalhadas (Espécie x Sexo) ---")
knitr::kable(
  tabela_dimorfismo_detalhado,
  caption = "Comparação de Médias Biométricas por Espécie e Sexo (Dimorfismo)",
  digits = 2
)


library(dplyr)
library(knitr)

# Tabela 6 FINAL: Dimorfismo Sexual Detalhado (Biometria, Morfologia e Dieta)
tabela_dimorfismo_final <- data_pinguins %>%
  # Remove NAs nas variáveis chaves (necessário para o cálculo das médias)
  drop_na(
    `Body Mass (g)`, 
    `Culmen Length (mm)`, 
    `Culmen Depth (mm)`,
    `Delta 15 N (o/oo)`,
    `Delta 13 C (o/oo)`,
    Sex
  ) %>%
  
  # Agrupar pela variável central (Species) E Sexo
  group_by(Species, Sex) %>%
  
  # Calcular todas as médias necessárias
  summarise(
    N = n(),
    Massa_Media_g = mean(`Body Mass (g)`),
    Comprimento_Bico_Media_mm = mean(`Culmen Length (mm)`),
    Profundidade_Bico_Media_mm = mean(`Culmen Depth (mm)`), # Variável Adicionada
    Delta_15N_Media = mean(`Delta 15 N (o/oo)`),             # Variável Adicionada
    Delta_13C_Media = mean(`Delta 13 C (o/oo)`),             # Variável Adicionada
    .groups = 'drop'
  )

print("--- Tabela 6: Dimorfismo Sexual Detalhado (FINAL) ---")
knitr::kable(
  tabela_dimorfismo_final,
  caption = "Médias Biométricas e Tróficas por Espécie e Sexo (Dimorfismo Ecológico)",
  digits = 3 # Mantemos alta precisão para os isótopos
)

library(dplyr)
library(knitr)

# Tabela 6 FINAL: Dimorfismo Sexual Detalhado (Biometria e Morfologia)
tabela_dimorfismo_final_morfo <- data_pinguins %>%
  # Remove NAs nas variáveis biométricas e Sex
  drop_na(
    `Body Mass (g)`, 
    `Culmen Length (mm)`, 
    `Culmen Depth (mm)`,
    Sex
  ) %>%
  
  # Agrupar pela variável central (Species) E Sexo
  group_by(Species, Sex) %>%
  
  # Calcular todas as médias necessárias
  summarise(
    N = n(),
    Massa_Media_g = mean(`Body Mass (g)`),
    Comprimento_Bico_Media_mm = mean(`Culmen Length (mm)`),
    Profundidade_Bico_Media_mm = mean(`Culmen Depth (mm)`), 
    .groups = 'drop'
  )

print("--- Tabela 6: Dimorfismo Sexual (Massa e Morfologia) ---")
knitr::kable(
  tabela_dimorfismo_final_morfo,
  caption = "Médias de Tamanho e Morfologia por Espécie e Sexo",
  digits = 2
)

#.............

library(dplyr)
library(knitr)

# Tabela: Resumo de Massa Corporal por Espécie e Sexo
tabela_massa_sexo <- data_pinguins %>%
  # 1. Remover NAs nas variáveis de interesse para garantir médias precisas
  drop_na(`Body Mass (g)`, Sex) %>%
  
  # 2. Agrupar pelos fatores de comparação
  group_by(Species, Sex) %>%
  
  # 3. Calcular as estatísticas focadas apenas na Massa
  summarise(
    N = n(), # Tamanho da amostra
    Massa_Media_g = mean(`Body Mass (g)`),
    Desvio_Padrao_g = sd(`Body Mass (g)`), # Opcional: mostra a variabilidade
    .groups = 'drop'
  )

# 4. Exibir a tabela formatada no console
print("--- Tabela de Massa Corporal por Espécie e Sexo ---")
knitr::kable(
  tabela_massa_sexo,
  digits = 2,
  caption = "Média e Desvio Padrão da Massa Corporal (g) por Espécie e Sexo"
)

#.....

# Carregar as bibliotecas necessárias
library(dplyr)
library(knitr)
library(tidyr) # Para drop_na

# Certifique-se de que o dataset 'data_pinguins' está carregado
# Se não estiver, descomente a linha abaixo:
# data_pinguins <- palmerpenguins::penguins_raw

# Tabela: Resumo de Massa Corporal por Espécie e Sexo (Estendida)
tabela_massa_sexo_estendida <- data_pinguins %>%
  # 1. Remover NAs nas variáveis de interesse para garantir estatísticas precisas
  # Usamos drop_na do pacote tidyr
  drop_na(`Body Mass (g)`, Sex) %>%
  
  # 2. Agrupar pelos fatores de comparação
  group_by(Species, Sex) %>%
  
  # 3. Calcular as estatísticas descritivas completas
  summarise(
    N = n(), 
    Massa_Media_g = mean(`Body Mass (g)`),
    Desvio_Padrao_g = sd(`Body Mass (g)`), 
    Minimo_g = min(`Body Mass (g)`),      # Adicionado: Mínimo
    Mediana_g = median(`Body Mass (g)`),  # Adicionado: Mediana
    Maximo_g = max(`Body Mass (g)`),      # Adicionado: Máximo
    .groups = 'drop'
  )

# 4. Exibir a tabela formatada no console
print("--- Tabela Detalhada de Massa Corporal por Espécie e Sexo ---")
knitr::kable(
  tabela_massa_sexo_estendida,
  digits = 2, # Arredondar para 2 casas decimais
  col.names = c("Espécie", "Sexo", "N", "Média (g)", "Desvio Padrão (g)", 
                "Mínimo (g)", "Mediana (g)", "Máximo (g)"),
  caption = "Estatísticas Descritivas Completas da Massa Corporal"
)

#....................................................


# Gráficos

### 3.4. Visualizações Chave (Gráficos)

library(ggplot2)
library(dplyr)

# Tabela de Frequência: Espécies por Ilha (Reutilizando a lógica da Tabela 2)
# O dataframe 'tabela_especie_ilha' já existe e está ordenado
tabela_especie_ilha_plot <- data_pinguins %>%
  drop_na(Species, Island) %>%
  count(Species, Island, name = "Contagem") %>%
  mutate(Percentual = (Contagem / sum(Contagem)) * 100)

# Gráfico de Barras da Distribuição Geográfica
plot_distribuicao_ilha <- tabela_especie_ilha_plot %>%
  ggplot(aes(x = fct_reorder(Island, Contagem, .fun = sum), 
             y = Contagem, 
             fill = Species)) +
  geom_col(position = position_stack()) + # Barras empilhadas (stack)
  
  labs(
    title = "Distribuição da Amostra de Pinguins por Ilha e Espécie",
    subtitle = "Contexto Espacial e Amostral no Arquipélago de Palmer",
    x = "Ilha de Amostragem",
    y = "Contagem de Indivíduos",
    fill = "Espécie"
  ) +
  # Adicionar rótulos de porcentagem na barra (para facilitar a leitura da Tabela)
  geom_text(aes(label = paste0(round(Percentual, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_distribuicao_ilha)

# Gráfico A: Massa Corporal (Dimorfismo Sexual)
plot_mass <- data_pinguins %>%
  drop_na(`Body Mass (g)`, Species, Sex) %>%
  ggplot(aes(x = Species, y = `Body Mass (g)`, fill = Sex)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Massa Corporal por Espécie e Dimorfismo Sexual")
print(plot_mass)

# Gráfico B: Dispersão Bivariada do Bico (Separação Morfológica)
plot_culmen_bivariado <- data_pinguins %>%
  drop_na(`Culmen Length (mm)`, `Culmen Depth (mm)`, Species) %>%
  ggplot(aes(x = `Culmen Length (mm)`, y = `Culmen Depth (mm)`, color = Species)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(type = "t", linewidth = 1) +
  labs(title = "Relação Comprimento vs. Profundidade do Bico")
print(plot_culmen_bivariado)


#GRÁFICOS  Evidência de Nicho Trófico


# Gráfico C: Comparação do Isótopo Delta 15N (Nível Trófico) por Espécie
plot_delta_15n <- data_pinguins %>%
  drop_na(`Delta 15 N (o/oo)`, Species) %>%
  ggplot(aes(x = Species, y = `Delta 15 N (o/oo)`, fill = Species)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Estratificação Alimentar: Nível Trófico (Delta 15N)",
    subtitle = "Posição na Cadeia Alimentar por Espécie",
    y = "Delta 15 N (‰)",
    x = NULL
  ) +
  theme_minimal()

print(plot_delta_15n)






# Gráfico: Comparação do Isótopo Delta 13 C (Fonte de Energia) por Espécie
plot_delta_13c <- data_pinguins %>%
  drop_na(`Delta 13 C (o/oo)`, Species) %>%
  ggplot(aes(x = Species, y = `Delta 13 C (o/oo)`, fill = Species)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Segregação Espacial: Fonte de Carbono (Delta 13C)",
    subtitle = "Indicador do Nicho de Forrageamento Trófico",
    y = "Delta 13 C (‰)",
    x = "Espécie de Pinguim"
  ) +
  theme_minimal()

print(plot_delta_13c)
