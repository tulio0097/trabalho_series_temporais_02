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


################################################################################
################################################################################
### Estudo da Sazonalidade
sazonalidade_Temperatura <- ss_01 %>%
  gg_season(Temperatura_Media, labels = "both") +
  labs(y = "Temperatura média a 2m (°C)",
       title = "Sazonalidade: Temperatuda Média")

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


gridExtra::grid.arrange(
  sazonalidade_Precipitacao, sazonalidade_Temperatura, 
  sazonalidade_Velocidade, sazonalidade_Umiade,
  layout_matrix = rbind(c(1,2),
                        c(3,4))
)

## Teste Estacionáriedade
ss_01 %>%
  features(Temperatura_Media, unitroot_kpss)

ss_01 %>%
  features(Precipitacao_Total, unitroot_kpss)

ss_01 %>%
  features(Velocidade_Vento, unitroot_kpss)

ss_01 %>%
  features(Umidade_Relativa, unitroot_kpss)



################################################################################
# ACF e PACF 
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


