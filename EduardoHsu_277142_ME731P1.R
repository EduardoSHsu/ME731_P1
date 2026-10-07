# PROJETO 1 (P1) - ME731: Análise Multivariada: PCA e Distância de Mahalanobis
#Eduardo Hsu RA:277142
#Pacotes e leitura

#desmarque e execute se ainda não tiver instalado
#install.packages("MVN")    
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("e1071")

library(MVN)
library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)
library(readr)

url <- "https://raw.githubusercontent.com/EduardoSHsu/dataset_p1ME731/refs/heads/main/owid-energy-data.csv"
dados <- read_csv(url)
  
# Escolha de variáveis e limpeza

vars_teste <- c(
  "energy_per_capita",
  "per_capita_electricity",
  "carbon_intensity_elec",
  "energy_per_gdp"
)

nomes_pt <- c(
  "Energia per capita",
  "Eletricidade per capita",
  "Intensidade de carbono da eletricidade",
  "Energia por PIB"
)

names(nomes_pt) <- vars_teste

dados_pca <- dados %>%
  filter(year == 2022) %>%
  select(country, iso_code, all_of(vars_teste)) %>%
  na.omit()

#Estatísticas descritivas

estatisticas <- dados_pca %>%
  summarise(
    across(
      all_of(vars_teste),
      list(
        media = ~mean(.x, na.rm = TRUE),
        mediana = ~median(.x, na.rm = TRUE),
        dp = ~sd(.x, na.rm = TRUE),
        minimo = ~min(.x, na.rm = TRUE),
        Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
        Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
        maximo = ~max(.x, na.rm = TRUE)
      )
    )
  )

estatisticas


assimetria <- dados_pca %>%
  summarise(
    across(
      all_of(vars_teste),
      ~skewness(.x, na.rm = TRUE)
    )
  )

assimetria


curtose <- dados_pca %>%
  summarise(
    across(
      all_of(vars_teste),
      ~kurtosis(.x, na.rm = TRUE)
    )
  )

curtose


#Histogramas - dados originais


dados_pca %>%
  pivot_longer(
    cols = all_of(vars_teste),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = setNames(nomes_pt, vars_teste)[variavel]
  ) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30) +
  facet_wrap(
    ~variavel,
    scales = "free"
  ) +
  labs(
    x = NULL,
    y = "Frequência"
  ) +
  theme_minimal()


#QQ-plots - dados originais


dados_pca %>%
  pivot_longer(
    cols = all_of(vars_teste),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = setNames(nomes_pt, vars_teste)[variavel]
  ) %>%
  ggplot(aes(sample = valor)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(
    ~variavel,
    scales = "free"
  ) +
  labs(
    x = "Quantis teóricos",
    y = "Quantis amostrais"
  ) +
  theme_minimal()


#Matriz de dispersão


pairs(
  dados_pca[, vars_teste],
  pch = 19,
  cex = 0.6
)


#Gráficos bivariados com elipses


ggplot(
  dados_pca,
  aes(
    x = energy_per_capita,
    y = per_capita_electricity
  )
) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(
    x = "Energia per capita",
    y = "Eletricidade per capita"
  ) +
  theme_minimal()


ggplot(
  dados_pca,
  aes(
    x = energy_per_capita,
    y = carbon_intensity_elec
  )
) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(
    x = "Energia per capita",
    y = "Intensidade de carbono da eletricidade"
  ) +
  theme_minimal()


ggplot(
  dados_pca,
  aes(
    x = per_capita_electricity,
    y = carbon_intensity_elec
  )
) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(
    x = "Eletricidade per capita",
    y = "Intensidade de carbono da eletricidade"
  ) +
  theme_minimal()


ggplot(
  dados_pca,
  aes(
    x = energy_per_gdp,
    y = carbon_intensity_elec
  )
) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(
    x = "Energia por PIB",
    y = "Intensidade de carbono da eletricidade"
  ) +
  theme_minimal()


#NORMALIDADE MULTIVARIADA - DADOS ORIGINAIS

resultado_mardia <- mvn(
  data = dados_pca[, vars_teste],
  mvn_test = "mardia"
)

resultado_mardia


#QQ-plot multivariado

plot(resultado_mardia, diagnostic = "multivariate", type = "qq")


#Transformação logarítmica


dados_log <- dados_pca %>%
  mutate(
    energy_per_capita = log(energy_per_capita),
    per_capita_electricity = log(per_capita_electricity),
    energy_per_gdp = log(energy_per_gdp)
  )



#Estatísticas após log

estatisticas_log <- dados_log %>%
  summarise(
    across(
      all_of(vars_teste),
      list(
        media = ~mean(.x, na.rm = TRUE),
        mediana = ~median(.x, na.rm = TRUE),
        dp = ~sd(.x, na.rm = TRUE),
        minimo = ~min(.x, na.rm = TRUE),
        Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
        Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
        maximo = ~max(.x, na.rm = TRUE)
      )
    )
  )

estatisticas_log

assimetria_log <- dados_log %>%
  summarise(
    across(
      all_of(vars_teste),
      ~ e1071::skewness(.x, na.rm = TRUE)
    )
  )

assimetria_log

curtose_log <- dados_log %>%
  summarise(
    across(
      all_of(vars_teste),
      ~ e1071::kurtosis(.x, na.rm = TRUE)
    )
  )

curtose_log


#QQ-plots - dados log-transformados


dados_log %>%
  pivot_longer(
    cols = all_of(vars_teste),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = setNames(nomes_pt, vars_teste)[variavel]
  ) %>%
  ggplot(aes(sample = valor)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(
    ~variavel,
    scales = "free"
  ) +
  labs(
    x = "Quantis teóricos",
    y = "Quantis amostrais"
  ) +
  theme_minimal()


#Histogramas - dados log-transformados


dados_log %>%
  pivot_longer(
    cols = all_of(vars_teste),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = setNames(nomes_pt, vars_teste)[variavel]
  ) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30) +
  facet_wrap(
    ~variavel,
    scales = "free"
  ) +
  labs(
    x = "Logaritmo da variável",
    y = "Frequência"
  ) +
  theme_minimal()

#Gráficos bivariados com elipses, dados log


ggplot(
  dados_log,
  aes(
    x = energy_per_capita,
    y = per_capita_electricity
  )
) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(
    x = "Energia per capita",
    y = "Eletricidade per capita"
  ) +
  theme_minimal()


ggplot(
  dados_log,
  aes(
    x = energy_per_capita,
    y = carbon_intensity_elec
  )
) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(
    x = "Energia per capita",
    y = "Intensidade de carbono da eletricidade"
  ) +
  theme_minimal()


ggplot(
  dados_log,
  aes(
    x = per_capita_electricity,
    y = carbon_intensity_elec
  )
) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(
    x = "Eletricidade per capita",
    y = "Intensidade de carbono da eletricidade"
  ) +
  theme_minimal()


ggplot(
  dados_log,
  aes(
    x = energy_per_gdp,
    y = carbon_intensity_elec
  )
) +
  geom_point() +
  stat_ellipse(type = "norm") +
  labs(
    x = "Energia por PIB",
    y = "Intensidade de carbono da eletricidade"
  ) +
  theme_minimal()

#Normalidade multivariada - dados log-transformados


resultado_mardia_log <- mvn(
  data = dados_log[, vars_teste],
  mvn_test = "mardia"
)

resultado_mardia_log

#QQ-plot multivariado dados log

plot(resultado_mardia_log, diagnostic = "multivariate", type = "qq")


#Padronização dos dados log-transformados

X_log <- scale(
  dados_log[, vars_teste]
)


#Análise de Componentes Principais - PCA

pca_log <- prcomp(
  X_log,
  center = FALSE,
  scale. = FALSE
)


#Autovalores e variância explicada


autovalores_log <- pca_log$sdev^2

proporcao_log <- autovalores_log / sum(autovalores_log)

variancia_acumulada_log <- cumsum(proporcao_log)

tabela_pca_log <- data.frame(
  Componente = paste0("PC", 1:length(autovalores_log)),
  Autovalor = autovalores_log,
  Proporcao = proporcao_log,
  Proporcao_acumulada = variancia_acumulada_log
)

tabela_pca_log %>%
  mutate(
    Proporcao = round(Proporcao, 4),
    Proporcao_acumulada = round(Proporcao_acumulada, 4)
  )


#Coeficientes dos autovetores


coeficientes_log <- pca_log$rotation

round(coeficientes_log, 4)

tabela_coeficientes_log <- as.data.frame(coeficientes_log) %>%
  tibble::rownames_to_column("Variavel")

tabela_coeficientes_log


#Scores dos países


scores_log <- as.data.frame(pca_log$x)

scores_log <- dados_log %>%
  select(country, iso_code) %>%
  bind_cols(scores_log)

head(scores_log)

ggplot(
  scores_log,
  aes(x = PC1, y = PC2)
) +
  geom_point() +
  labs(
    x = "PC1",
    y = "PC2",
    title = ""
  ) +
  theme_minimal()


#Distância de Mahalanobis


S_log <- cov(X_log)

d2_log <- mahalanobis(
  X_log,
  center = colMeans(X_log),
  cov = S_log
)

resultado_md_log <- dados_log %>%
  select(country, iso_code) %>%
  mutate(
    D2 = d2_log
  ) %>%
  arrange(desc(D2))

resultado_md_log

head(resultado_md_log, 15)


#Decomposição da distância de Mahalanobis pelos PCs

contrib_log <- sweep(
  pca_log$x^2,
  2,
  pca_log$sdev^2,
  "/"
)

contrib_log <- as.data.frame(contrib_log)

names(contrib_log) <- paste0("PC", 1:4)

contrib_log <- dados_log %>%
  select(country, iso_code) %>%
  bind_cols(contrib_log) %>%
  mutate(
    D2 = rowSums(across(starts_with("PC")))
  ) %>%
  arrange(desc(D2))

head(contrib_log, 15)


#Verificação da equivalência

max(abs(
  d2_log -
    contrib_log$D2[match(dados_log$iso_code, contrib_log$iso_code)]
))


#Estatísticas descritivas da Mahalanobis


summary(resultado_md_log$D2)
sd(resultado_md_log$D2)

ggplot(
  resultado_md_log,
  aes(x = D2)
) +
  geom_histogram(bins = 30) +
  labs(
    x = expression(D[M]^2),
    y = "Frequência",
    title = ""
  ) +
  theme_minimal()


#Principal componente responsável pela distância

resultado_contrib_log <- contrib_log %>%
  mutate(
    PC_principal = c("PC1", "PC2", "PC3", "PC4")[
      max.col(across(PC1:PC4))
    ],
    contribuicao_max = pmax(PC1, PC2, PC3, PC4),
    proporcao_principal = contribuicao_max / D2
  ) %>%
  select(
    country,
    iso_code,
    D2,
    PC1,
    PC2,
    PC3,
    PC4,
    PC_principal,
    proporcao_principal
  )

head(resultado_contrib_log, 15)

# Matriz de correlação - dados log-transformados

matriz_cor_log <- cor(
  dados_log[, vars_teste],
  use = "complete.obs"
)

round(matriz_cor_log, 4)


matriz_cor_log_pt <- matriz_cor_log

rownames(matriz_cor_log_pt) <- nomes_pt[rownames(matriz_cor_log_pt)]
colnames(matriz_cor_log_pt) <- nomes_pt[colnames(matriz_cor_log_pt)]

round(matriz_cor_log_pt, 4)


# Scree plot com critério de Kaiser

dados_scree <- data.frame(
  Componente = 1:length(autovalores_log),
  Autovalor = autovalores_log
)

ggplot(
  dados_scree,
  aes(
    x = Componente,
    y = Autovalor
  )
) +
  geom_line() +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "red"
  ) +
  scale_x_continuous(
    breaks = 1:length(autovalores_log)
  ) +
  labs(
    x = "Componente Principal",
    y = "Autovalor",
    title = "",
    subtitle = ""
  ) +
  theme_minimal()
