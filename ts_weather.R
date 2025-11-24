library(tidyverse)
library(fable)
library(fabletools)
library(ggthemes)
set_theme(theme_clean())
suppressWarnings(set_theme(theme_clean()))
library(lubridate)
library(feasts)
library(mice)

# descrevendo os dados ----------------------------------------------------

df <- read.csv("Data/PalmerStation_Daily_Weather.csv", sep = ",", header = T) %>%
  as_tibble()

glimpse(df) # nota-se NAs em algumas variaveis

df %>% 
  select(Temperature.Average..C.) %>% 
  summary()

df %>% names()

df %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% 
  summarise(
    total_dias_ano = n(),                             
    dias_com_NA = sum(is.na(Temperature.Average..C.)),
    ano_completo_em_NA = dias_com_NA == total_dias_ano
  ) %>% 
  print(n = 40) # Anos sem NA a partir de 1997

# para uma serie temporal, vamos utilizar apenas dados de temperatura 
palmerweather <- df %>% 
  select(
    Date, Temperature.Average..C., Temperature.High..C., Temperature.Low..C.
  ) %>% 
  mutate(
    Date = ymd(Date),
    day = yday(Date)
  ) %>% 
  filter(
    year(Date) >= 1997 & year(Date) <= 2023
  ) %>% 
  rename(
    Temp_avg = Temperature.Average..C.,
    Temp_min = Temperature.Low..C.,
    Temp_max = Temperature.High..C.
  ) %>% 
  as_tsibble(index = Date)


# investigando NA ---------------------------------------------------------

palmerweather %>% 
  filter(is.na(Temp_avg)) # Há algumas datas que estavam faltando informações
                          # Vamos imputar utilizando o pacote mice


# imputando dados ---------------------------------------------------------

dados_imputar <- palmerweather %>% 
  tsibble::fill_gaps() %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  select(-Date)

imp <- dados_imputar %>% 
  mice(method = "pmm", seed = 123)

dados_imputar <- complete(imp)

md.pattern(dados_imputar)
stripplot(imp, Temp_avg~.imp)

palmerweather_imp <- palmerweather %>% 
  tsibble::fill_gaps() %>% 
  mutate(
    Temp_avg = dados_imputar$Temp_avg,
    Temp_min = dados_imputar$Temp_min,
    Temp_max = dados_imputar$Temp_max
  )

palmerweather_imp %>% filter(is.na(Temp_avg)) # Sem NA
palmerweather_imp %>% filter(is.na(Temp_max)) # Sem NA
palmerweather_imp %>% filter(is.na(Temp_min)) # Sem NA

# investigando tendencia em temp_avg --------------------------------------

stl_dec <- palmerweather_imp %>% 
  model(
    STL(Temp_avg~trend()+season(period = 365.25),
        robust = T)
  ) %>% 
  components()

stl_dec %>% 
  select(Date, trend) %>% 
  lm(trend~Date, data = .) %>% summary()

# Teste de Sen (1968) para verificar significância da inclinicao linear
trend::sens.slope(stl_dec$trend)


# Investigando as demais temperaturas -------------------------------------

analisa_trend <- function(x) {
  
  x <- ensym(x)
  x_exp <- expr((!!x)~trend()+season(period = 365.25))
  
  palmerweather_imp %>%
    model(STL(!!x_exp, robust = TRUE)) %>%
    components() -> comp
  
  lm_res <- lm(trend~Date, data = comp)
  print(summary(lm_res))
  print(trend::sens.slope(comp$trend))
}

stl_min <- palmerweather_imp %>% 
  model(
    STL(Temp_min~trend()+season(period = 365.25),
        robust = T)
  ) %>% 
  components()

stl_min %>% autoplot()+
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank())

analisa_trend("Temp_min")

stl_max <- palmerweather_imp %>% 
  model(
    STL(Temp_max~trend()+season(period = 365.25),
        robust = T)
  ) %>% 
  components()
stl_max %>% autoplot()+
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank())
analisa_trend("Temp_max")


# Retas ajustadas para pontos de tendencia --------------------------------

stl_dec %>% 
  select(Date, trend) %>% 
  ggplot(aes(x = Date, y = trend))+
  geom_line()+
  geom_smooth(formula = y~x, method = lm)
stl_min %>% 
  select(Date, trend) %>% 
  ggplot(aes(x = Date, y = trend))+
  geom_line()+
  geom_smooth(formula = y~x, method = lm)
stl_max %>% 
  select(Date, trend) %>% 
  ggplot(aes(x = Date, y = trend))+
  geom_line()+
  geom_smooth(formula = y~x, method = lm)

# library(leaflet)
# m = leaflet() %>% addTiles()
# m  # a map with the default OSM tile layer
# 
# m = m %>% setView(-93.65, 42.0285, zoom = 17)
# m
# 
# m %>% addPopups(-93.65, 42.0285, 'Here is the <b>Department of Statistics</b>, ISU')