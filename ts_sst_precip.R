library(tidyverse)
library(fable)
library(fabletools)
library(ggthemes)
set_theme(theme_clean())
suppressWarnings(set_theme(theme_clean()))
library(lubridate)
library(feasts)
library(mice)
library(naniar)


# Verificando NAs ---------------------------------------------------------

df <- read.csv("Data/PalmerStation_Daily_Weather.csv", sep = ",", header = T) %>%
  as_tibble()

df %>% glimpse()
# Seguindo o mesmo corte realizado para os dados de temperatura:
palmerweather <- df %>% 
  mutate(
    Date = ymd(Date),
    day = yday(Date)
  ) %>% 
  filter(
    year(Date) >= 1997 & year(Date) <= 2023
  ) %>% 
  rename(
    Temp_avg = Temperature.Average..C.,
    SST_avg = Sea.Surface.Temperature..C.,
    Precip = Precipitation.Melted..mm.
  ) %>% 
  mutate(
    Precip = case_when(
      Precip == "T" ~ "0.05",
      TRUE ~ Precip
    ),
    Precip = as.numeric(Precip)
  ) %>% 
  as_tsibble(index = Date) %>% 
  tsibble::fill_gaps()
# gg_miss_var() permite visualizar graficamente os NAs
palmerweather %>% 
  select(SST_avg, Precip) %>% 
  gg_miss_var()+
  labs(x = "Variáveis", y="Número de NAs")+
  theme_clean()


# Avaliando o tipo de mecanismo de imputação dos dados
palmerweather %>%
  select(-Date) %>% 
  mice(seed = 123) # recomendacao => pmm


# Imputação de dados ------------------------------------------------------

dados_imputar <- palmerweather %>% 
  as_tibble() %>%
  select(day, SST_avg, Precip) %>% 
  as.data.frame()

imp <- dados_imputar %>% 
  mice(method = "pmm", seed = 123)

dados_imputar <- complete(imp)

md.pattern(dados_imputar)
stripplot(imp, Precip~.imp)
stripplot(imp, SST_avg~.imp)

palmer_full <- palmerweather %>% 
  mutate(
    Temp_avg = dados_imputar$Temp_avg,
    SST_avg = dados_imputar$SST_avg,
    Precip = dados_imputar$Precip
  )


# Series Temporais --------------------------------------------------------

## Sea Surface Temperature
stl_sst <- palmer_full %>% 
  model(
    STL(SST_avg~trend()+season(period = 365.25),
        robust = T)
  ) %>% components()
  
stl_sst %>% 
  autoplot()
stl_sst %>% 
  lm(trend~Date, data=.) %>% summary()
stl_sst %>% 
  pull(trend) %>% 
  trend::sens.slope()

palmer_full %>% 
  select(SST_avg) %>% 
  filter(SST_avg <= -8)

## Preciptação 
stl_precip <- palmer_full %>% 
  model(
    STL(Precip)
  ) %>% components() 

stl_precip %>% 
  autoplot()
stl_precip %>% 
  lm(trend~Date, data = .) %>% summary()
stl_precip %>% 
  pull(trend) %>% 
  trend::sens.slope()


