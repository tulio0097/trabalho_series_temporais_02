# Pré-Processamento dos dados 


# Bibliotecas necessarias 
library(dplyr)
library(nasapower) # dados do satélite via API da NASA
library(fpp3) # as_tsible
library(rugarch)
library(quantmod)
library(tidyquant)
library(lubridate)
library(tsibble)

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
    pars = c("T2M", "PRECTOTCORR", "WS2M", "RH2M"),
    dates = c("1981-01-01", "2021-01-01"),
    temporal_api = "daily"
  )
  df$municipio <- nome # Adiciona coluna com o nome do município
  return(df)
}

# Executar para todos (usando a API)
lista_resultados <- mapply(coletar_dados, 
                           municipios_cariri$nome, 
                           municipios_cariri$lat, 
                           municipios_cariri$lon, 
                           SIMPLIFY = FALSE)

# Consolidar em um único Data Frame
df_final <- bind_rows(lista_resultados)

df_final <- df_final %>% 
  select(YYYYMMDD,T2M,PRECTOTCORR,municipio,WS2M,RH2M) %>% 
  rename(
    Data = YYYYMMDD,
    Temperatura_Media = T2M,
    Precipitacao_Total = PRECTOTCORR,
    Velocidade_Vento = WS2M,
    Umidade_Relativa = RH2M
  )
# Visualizar resultado
unique(df_final$municipio)
summary(df_final)

## Transformando em formato longo (tsibble)
df_ts <- df_final%>% 
  as_tsibble(index = Data, key = municipio)


plot_serie <- df_ts %>% 
  autoplot(Temperatura_Media)+
  labs(title = "Temperatura Média - 2 metros")



###########
# Transformando a série em mensal 
dados_mensais <- df_final %>%
  select(Data,Temperatura_Media,Precipitacao_Total,municipio,Velocidade_Vento,Umidade_Relativa) %>% 
  group_by(municipio,Data = floor_date(Data,"month")) %>% 
  summarise(Temperatura_Media = mean(Temperatura_Media,na.rm = TRUE),
            Chuva_total_mes = mean(Precipitacao_Total,na.rm = TRUE),
            Vento_medio = mean(Velocidade_Vento,na.rm = TRUE),
            Umidade_media = sum(Umidade_Relativa,na.rm = TRUE))

dados_ts <- dados_mensais %>%
  mutate(Data = yearmonth(Data)) %>% 
  as_tsibble(index = Data, key = municipio)
###########

# Agrupando os dados por Trimestre e Município
dados_trimestrais <- df_final %>%
  group_by(municipio, Data = floor_date(Data, "quarter")) %>% 
  summarise(
    Temperatura_Media = mean(Temperatura_Media, na.rm = TRUE),
    Chuva_total_trimestre = sum(Precipitacao_Total, na.rm = TRUE),
    Vento_medio = mean(Velocidade_Vento, na.rm = TRUE),
    Umidade_media = mean(Umidade_Relativa, na.rm = TRUE),
    .groups = "drop"
  )

# Convertendo para tsibble (formato de trimestre)
dados_trimestrais_ts <- dados_trimestrais %>%
  mutate(Data = yearquarter(Data)) %>% 
  as_tsibble(index = Data, key = municipio)
#################################################################################
# Estudo da Sazonalidade de um unico municipio

# Análise apenas para a cidade de Queimadas-PB
ss_01 <- df_ts %>%
  filter(municipio == "Queimadas") 

