#### Análise Gráfica 

##serie T2M
plot_serie_01 = ss_01 %>%
  autoplot(Temperatura_Media) +
  labs(title="Temperatura média a 2m (°C)",
       y="")
plot_serie_01

##serie PRECTOTCORR
plot_serie_02 = ss_01 %>%
  autoplot(Precipitacao_Total) +
  labs(title="Precipitação Total Corrigida (mm/dia)",
       y="")
plot_serie_02

##serie WS2M
plot_serie_03 = ss_01 %>%
  autoplot(Velocidade_Vento) +
  labs(title="Velocidade Média do Vento",
       y="")
plot_serie_03

##serie RH2M
plot_serie_04 = ss_01 %>%
  autoplot(Umidade_Relativa) +
  labs(title="Umidade Relativa",
       y="")
plot_serie_04

### Plot das Séries juntas
plot_series <- ss_01 %>%
  pivot_longer(-c(Data,municipio)) %>%
  ggplot(aes(x = Data, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")
plot_series


### Plot das Séries MENSAIS
plot_serie_mes_01 <- dados_ts %>% 
  pivot_longer(-c(Data,municipio)) %>%
  ggplot(aes(x = Data, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")


### Plot das Séries TRIMESTRAIS
plot_serie_trimestre_01 <- dados_trimestrais %>% 
  pivot_longer(-c(Data,municipio)) %>%
  ggplot(aes(x = Data, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

################################################################################
################################################################################
### Estudo da Sazonalidade
sazonalidade_Temperatura <- ss_01 %>%
  gg_season(Temperatura_Media, labels = "both") +
  labs(y = "Temperatura média a 2m (°C)",
       title = "Sazonalidade: Temperatuda Média (Diaria)")

sazonalidade_Precipitacao <- ss_01 %>%
  gg_season(Precipitacao_Total, labels = "both") +
  labs(y = "Temperatura média a 2m (°C)",
       title = "Sazonalidade: Precipitação Total Corrigida")

sazonalidade_Velocidade <- ss_01 %>%
  gg_season(Velocidade_Vento, labels = "both") +
  labs(y = "Temperatura média a 2m (°C)",
       title = "Sazonalidade: Velocidade do Vento")

sazonalidade_Umiade <- ss_01 %>%
  gg_season(Umidade_Relativa, labels = "both") +
  labs(y = "Temperatura média a 2m (°C)",
       title = "Sazonalidade: Umidade Relativa")

Sazonalidade_total <- gridExtra::grid.arrange(
  sazonalidade_Precipitacao, sazonalidade_Temperatura, 
  sazonalidade_Velocidade, sazonalidade_Umiade,
  layout_matrix = rbind(c(1,2),
                        c(3,4))
)



###
### Sazonalidade Mensal
sazonalidade_Temperatura_Mensal_01 <- dados_ts %>%
  filter(municipio == "Queimadas") %>% 
  gg_season(Temperatura_Media, labels = "both") +
  labs(y = "Temperatura média a 2m (°C)",
       title = "Sazonalidade: Temperatuda Média (Mensal)")

### Sazonalidade Trimestral
sazonalidade_Temperatura_Trimestrao_01 <- dados_trimestrais_ts %>%
  filter(municipio == "Queimadas") %>% 
  gg_season(Temperatura_Media, labels = "both") +
  labs(y = "Temperatura média a 2m (°C)",
       title = "Sazonalidade: Temperatuda Média (Trimestral)")





## Teste Estacionáriedade
Estacionaridade <- ss_01 %>%
  pivot_longer(cols = c(Temperatura_Media, Precipitacao_Total, 
                        Velocidade_Vento, Umidade_Relativa), 
               names_to = "Variavel", values_to = "Valor") %>%
  features(Valor, unitroot_kpss)




################################################################################
### ACF e PACF 

## Temperatura
acf1 <- ss_01 %>%
  ACF(Temperatura_Media, lag_max = 20) %>% 
  autoplot() +
  labs(title = "ACF")

pacf1 <- ss_01 %>%
  PACF(Temperatura_Media, lag_max = 20) %>% 
  autoplot() +
  labs(title = "PACF")

ACF_PACF_Temperatura1 <- gridExtra::grid.arrange(
  plot_serie_01, acf1, pacf1,
  layout_matrix = rbind(c(1,1),c(2,3))
)


## Precipitação
acf2 <- ss_01 %>%
  ACF(Precipitacao_Total, lag_max = 20) %>% 
  autoplot() +
  labs(title = "ACF")

pacf2 <- ss_01 %>%
  PACF(Precipitacao_Total, lag_max = 20) %>% 
  autoplot() +
  labs(title = "PACF")

ACF_PACF_Precipitacao2 <- gridExtra::grid.arrange(
  plot_serie_02, acf2, pacf2,
  layout_matrix = rbind(c(1,1),c(2,3))
)


## Velocidade
acf3 <- ss_01 %>%
  ACF(Velocidade_Vento, lag_max = 20) %>% 
  autoplot() +
  labs(title = "ACF")

pacf3 <- ss_01 %>%
  PACF(Velocidade_Vento, lag_max = 20) %>% 
  autoplot() +
  labs(title = "PACF")

ACF_PACF_Velocidade3 <- gridExtra::grid.arrange(
  plot_serie_03, acf3, pacf3,
  layout_matrix = rbind(c(1,1),c(2,3))
)


## Umidade
acf4 <- ss_01 %>%
  ACF(Umidade_Relativa, lag_max = 20) %>% 
  autoplot() +
  labs(title = "ACF")

pacf4 <- ss_01 %>%
  PACF(Umidade_Relativa, lag_max = 20) %>% 
  autoplot() +
  labs(title = "PACF")

ACF_PACF_Umidade4 <- gridExtra::grid.arrange(
  plot_serie_04, acf4, pacf4,
  layout_matrix = rbind(c(1,1),c(2,3))
)




# Testar se existe efeito ARCH e GARCH
FinTS::ArchTest(df_final$Temperatura_Media, lags = 12)
FinTS::ArchTest(df_final$Precipitacao_Total, lags = 12)
FinTS::ArchTest(df_final$Velocidade_Vento, lags = 12)
FinTS::ArchTest(df_final$Umidade_Relativa, lags = 12)
