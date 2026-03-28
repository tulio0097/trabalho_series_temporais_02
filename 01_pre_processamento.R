# Pré-Processamento dos dados 


# Bibliotecas necessarias 
library(dplyr)
library(nasapower) # dados do satélite via API da NASA
library(fpp3) # as_tsible
library(rugarch)
library(quantmod)
library(tidyquant)

# Estrutura de dados dos municípios (Coordenadas em Graus Decimais)
municipios_cariri <- data.frame(
  nome = c("Alcantil", "Barra de São Miguel", "Boqueirão", "Cabaceiras", 
           "Queimadas", "Caturité", "Gurjão", "Riacho de Santo Antônio", 
           "Santo André", "São Domingos do Cariri", "São João do Cariri", "São José dos Cordeiros"),
  lat = c(-7.74, -7.75, -7.48, -7.48, -7.35, -7.42, -7.24, -7.62, -7.22, -7.61, -7.39, -7.39),
  lon = c(-36.05, -36.31, -36.13, -36.28, -35.89, -36.02, -36.48, -36.13, -36.63, -36.39, -36.53, -36.80)
)

# Função para coletar e identificar o município
coletar_dados <- function(nome, lat, lon) {
  df <- get_power(
    community = "ag",  # "ag" é comum para climatologia/agricultura
    lonlat = c(lon, lat),
    pars = c("T2M", "PRECTOTCORR", ),
    dates = c("1981-01-01", "2021-01-01"),
    temporal_api = "daily"
  )
  df$municipio <- nome # Adiciona coluna com o nome do município
  return(df)
}

# Executar para todos (isso pode levar alguns minutos devido à API)
lista_resultados <- mapply(coletar_dados, 
                           municipios_cariri$nome, 
                           municipios_cariri$lat, 
                           municipios_cariri$lon, 
                           SIMPLIFY = FALSE)

# Consolidar em um único Data Frame
df_final <- bind_rows(lista_resultados)

df_final <- df_final %>% 
  select(YYYYMMDD,T2M,PRECTOTCORR,municipio)
# Visualizar resultado
unique(df_final$municipio)
summary(df_final)

# 
df_02 <- df_final %>% 
  mutate(
    Data = YYYYMMDD) %>% 
  as_tsibble(index = Data, key = municipio)


plot_serie <- df_02 %>% 
  autoplot(T2M)+
  labs(title = "Temperatura Média - 2 metros")


# ACF e PACF da série 
acf <- df_02 %>%
  ACF(Temperatura_M, lag_max = 20) %>% 
  autoplot() +
  labs(title = "ACF")

pacf <- df_02 %>%
  PACF(Temperatura_M, lag_max = 20) %>% 
  autoplot() +
  labs(title = "PACF")

gridExtra::grid.arrange(
  plot_serie, acf, pacf,
  layout_matrix = rbind(c(1,1),c(2,3))
)

#################################################################################

# Testar se existe efeito ARCH e GARCH
FinTS::ArchTest(df_02$T2M, lags = 12)
FinTS::ArchTest(df_02$PRECTOTCORR, lags = 12)

# Há presença do efeito GARCH nesta série!

#################################################################################
# Estudo da Sazonalidade de um unico municipio

# Análise apenas para a cidade de Queimadas-PB
ss_01 <- df_final %>%
  filter(municipio == "Queimadas") %>% 
  mutate(Data = YYYYMMDD) %>%
  as_tsibble(index = Data)

#### Comportamento da serie T2M
plot_serie_T2M = ss_01 %>%
  autoplot(T2M) +
  labs(title="Temperatura média a 2m (°C)",
       y="")
plot_serie_T2M

plot_serie_PREC = ss_01 %>%
  autoplot(PRECTOTCORR) +
  labs(title="Precipitação Total Corrigida (mm/dia)",
       y="")
plot_serie_PREC


### Plot das duas Séries juntas
ss_01 %>%
  pivot_longer(-c(YYYYMMDD,Data,municipio)) %>%
  ggplot(aes(x = Data, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

### Estudo da Sazonalidade
ss_01 %>%
  gg_season(T2M, labels = "both") +
  labs(y = "Temperatura média a 2m (°C)",
       title = "Sazonalidade: T2M")
