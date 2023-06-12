library(dplyr)
library(tidyverse)
library(gtools)
library(stringr)
library(gplots)
library("ggvenn")
library(readxl)
library(cowplot)
library(ggthemes)
library(esquisse)
library("xlsx")
library(ggplot2)
library(tidyr)
library(RColorBrewer)



##INICIANDO FUNÇÕES NECESSÁRIAS
count_na <- function(df) {
  result <- sapply(df, function(col) sum(is.na(col) | col == ""))
  return(result)
}


##Carregando a base dos bolsistas
database<- read.csv("D:/Tecnologia da Informação (TI)/P3 2023.1/Probabilidade e Inferência/Projeto 2.1/Base Bolsistas IC.csv", sep=";",stringsAsFactors = FALSE)

database <- database %>%
  select(discente,titulo,codigo_projeto, ano,orientador,
         categoria, tipo_de_bolsa, linha_pesquisa, grupo_pesquisa, inicio, fim, unidade, status)

database$inicio <- str_sub(database$inicio, 1,4)
database$fim <- str_sub(database$fim, 1,4)

## Identificando informações gerais sobre a base
summary(database) ##BANCO DE DADOS NÃO POSSÚI NA, APENAS COLUNAS VAZIAS


str(database)

glimpse(database)

names(database)


##INICIANDO ANÁLISE DOS DADOS

##DETERMINANDO A QUANTIDADE DE VALORES ÚNICOS EM CADA UMA DAS VARIÁVEIS NOMINAIS E CATEGORICAS

# Removendo a coluna "ano" do dataframe original
df_sem_ano <- database

# Criando um novo dataframe para armazenar a contagem de valores únicos
contagem_valores <- data.frame(
  coluna = character(),
  contagem = numeric()
)

# Obtendo a contagem de valores únicos para cada coluna, exceto "ano"
for (col in names(df_sem_ano)) {
  contagem <- length(unique(df_sem_ano[[col]]))
  contagem_valores <- rbind(contagem_valores, data.frame(coluna = col, contagem = contagem))
}

# Exibindo o novo dataframe com a contagem de valores únicos
print(contagem_valores)


##PREPARANDO OS DADOS PARA PLOTAR

##EVOLUÇÃO DOS PROJETOS EM RELAÇAO AO PASSAR DOS ANOS
PROJETOC_ANO <-  unique(database[,c("ano","codigo_projeto","status")]) %>% 
  group_by(ano,status) %>% 
  summarise(codigo_projeto = n())

##RELAÇÃO ENTRE O TIPO DE BOLSA E A CATEGORIA
CATEGORIA_POR_BOLSA <- (database[,c("categoria","tipo_de_bolsa","status")]) %>% 
  group_by(categoria,status) %>% 
  summarise(tipo_de_bolsa = n())

##RELAÇÃO ENTRE CATEGORIA  O TIPO DE BOLSA
 BOLSA_POR_CATEGORIA<- (database[,c("categoria","tipo_de_bolsa","status")]) %>% 
  group_by(tipo_de_bolsa,status) %>% 
  summarise(categoria = n())
 
 ORDEM_BOLSA_POR_CATEGORIA <- BOLSA_POR_CATEGORIA %>%
   filter(status == "FINALIZADO") %>%
   select(tipo_de_bolsa,categoria) %>%
   arrange(desc(categoria))
 
 ##RELAÇÃO ENTRE UNIDADE E STATUS
 
 UNIDADE_POR_STATUS <- database %>% 
   select(unidade, status) %>% 
   group_by(unidade, status) %>% 
   summarise(contagem = n()) %>%
   arrange(desc(contagem)) %>%
   mutate(unidade = fct_rev(unidade))
 
 ORDEM_UNIDADE_POR_STATUS  <- UNIDADE_POR_STATUS %>% 
   select(unidade, contagem)  %>%
   arrange(desc(contagem))
 
 ##RELAÇÃO ENTRE CATEGORIA, TIPO DE BOLSA E ANO DE INICIO

 BOLSA_POR_ANO <- database %>%
   count(inicio, tipo_de_bolsa, name = "contagem")
   
 BOLSA_POR_ANO <- unique(BOLSA_POR_ANO)
 


##PLOTANDO

##EVOLUÇÃO DOS PROJETOS EM RELAÇAO AO PASSAR DOS ANOS

esquisser(PROJETOC_ANO, viewer = "browser")

ggplot(PROJETOC_ANO) +
  aes(
    x = ano,
    y = codigo_projeto,
    fill = status,
    colour = status
  ) +
  geom_line(size = 3) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_minimal()

##RELAÇÃO ENTRE O TIPO DE BOLSA E A CATEGORIA

esquisser(CATEGORIA_POR_BOLSA, viewer = "browser")

ggplot(CATEGORIA_POR_BOLSA) +
  aes(x = categoria,y = tipo_de_bolsa, fill = status, weight = tipo_de_bolsa) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(status)) +
  geom_text(aes(label= tipo_de_bolsa),vjust=0, size = 7 )
  

##RELAÇÃO ENTRE CATEGORIA  O TIPO DE BOLSA

esquisser(BOLSA_POR_CATEGORIA, viewer = "browser")

ggplot(BOLSA_POR_CATEGORIA) +
  aes(x = categoria , y = tipo_de_bolsa) +
  geom_col(fill = "#112446") +
  theme_minimal() +
  scale_y_discrete(limits = fct_rev(ORDEM_BOLSA_POR_CATEGORIA$tipo_de_bolsa))


##GRÁFICO NA VERSÃO FILL
ggplot(BOLSA_POR_CATEGORIA) +
  aes(x = tipo_de_bolsa, fill = status, weight = categoria) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  ggthemes::theme_base()

##RELAÇÃO ENTRE UNIDADE E STATUS
esquisser(UNIDADE_POR_STATUS, viewer = "browser")

#TOP20 +
ggplot(head(UNIDADE_POR_STATUS, n =20)) +
  aes(x = contagem , y = fct_reorder(unidade, contagem)) +
  geom_col(fill = "#112446") +
  theme_minimal() +
  labs(title = "RELAÇÃO ENTRE UNIDADES E PROJETOS(TOP 20+)")

#TOP 20 - 
ggplot(tail(UNIDADE_POR_STATUS, n = 20)) +
  aes(x = contagem , y = fct_reorder(unidade, contagem)) +
  geom_col(fill = "#112446") +
  theme_minimal() +
  labs(title = "RELAÇÃO ENTRE UNIDADES E PROJETOS(TOP 20-)")

##GRÁFICO NA VERSÃO FILL
ggplot(UNIDADE_POR_STATUS) +
  aes(x = unidade, fill = status, weight = contagem) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 4, face = "bold"))


##RELAÇÃO ENTRE CATEGORIA, TIPO DE BOLSA E ANO DE INICIO
esquisser(BOLSA_POR_ANO, viewer = "browser")


ggplot(BOLSA_POR_ANO) +
  aes(x = inicio, y = tipo_de_bolsa, fill = contagem) +
  geom_tile(size = 1.2) +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  theme_minimal() +
  theme_bw()


##RELAÇÃO ENTRE GRUPO DE PESQUISA E QUANTIDADE DE BOLSAS


BOLSA_POR_GRUPO <- database %>%
  count(grupo_pesquisa, tipo_de_bolsa, name = "contagem") %>%
  arrange(desc(contagem))

BOLSA_POR_GRUPO <- unique(BOLSA_POR_GRUPO)
BOLSA_POR_GRUPO <- head(BOLSA_POR_GRUPO, n = 50)

ggplot(BOLSA_POR_GRUPO) +
  aes(x = tipo_de_bolsa, y = contagem) +
  geom_col(fill = "#112446") +
  theme_minimal() +
  facet_wrap(vars(grupo_pesquisa)) +
  theme(axis.text.x = element_text(angle = 45, size = 10, face="bold", hjust = 1, vjust = 0.4),
        strip.text.x = element_text(size = 7, face = "bold"))


##DISTRIBUIÇÃO DAS BOLSAS POR ORIENTADOR
esquisser(BOLSA_POR_ORIENTADOR, viewer = "browser")


BOLSA_POR_ORIENTADOR <- database %>%
  count(orientador, tipo_de_bolsa, name = "contagem") %>%
  arrange(desc(contagem))

BOLSA_POR_ORIENTADOR <- unique(BOLSA_POR_ORIENTADOR)
BOLSA_POR_ORIENTADOR <- head(BOLSA_POR_ORIENTADOR, n = 50)

ORDEM_BOLSA_POR_ORIENTADOR  <- BOLSA_POR_ORIENTADOR %>% 
  select(orientador, contagem)  %>%
  arrange(desc(contagem))

ggplot(BOLSA_POR_ORIENTADOR) +
  aes(x = contagem , y = orientador) +
  geom_col(fill = "#112446") +
  theme_minimal() +
  scale_y_discrete(limits = fct_rev(ORDEM_BOLSA_POR_ORIENTADOR$orientador))


###DADOS DAS BOLSAS POR ANO
esquisser(BOLSA_POR_ANO, viewer = "browser")


BOLSA_POR_ANO <- database %>%
  count(ano, tipo_de_bolsa, name = "contagem") %>%
  arrange(desc(contagem))

ORDEM_BOLSA_POR_ANO  <- BOLSA_POR_ANO %>% 
  select(tipo_de_bolsa, contagem)  %>%
  arrange(desc(contagem))

ggplot(BOLSA_POR_ANO) +
  aes(x = tipo_de_bolsa, y = contagem) +
  geom_boxplot(fill = "#112446") +
  theme_bw() +
  scale_x_discrete(limits = fct_rev(ORDEM_BOLSA_POR_ANO$tipo_de_bolsa)) +
  theme(axis.text.x = element_text(angle = 45, size = 10, face="bold", hjust = 1, vjust = 0.4))


##AMOSTRAGEM POR ESTRATIFICAÇÃO
install.packages("splitstackshape")
library(splitstackshape)

# Definir a fração desejada para cada estrato
frac <- 0.02  # Nesse exemplo, vamos amostrar 20% de cada estrato

# Realizar amostragem aleatória estratificada com substituição
amostra <- stratified(database, "tipo_de_bolsa", size = round(nrow(database) * frac), replace = TRUE)

# Visualizar a amostra
amostra


##AMOSTRAGEM ALEATÓRIA SISTEMÁTICA
# Definir o tamanho da amostra desejada
tamanho_amostra <- 800  # amostrando 200 observções

# Obter os grupos da coluna "tipo_de_bolsa"
grupos <- unique(database$tipo_de_bolsa)

# Criar um vetor de índices para cada grupo
indices_grupo <- lapply(grupos, function(grupo) {
  which(database$tipo_de_bolsa == grupo)
})

# Calcular o tamanho da amostra para cada grupo
tamanho_grupo <- sapply(indices_grupo, length)
tamanho_amostra_grupo <- round(tamanho_amostra * tamanho_grupo / sum(tamanho_grupo))

# Realizar a amostragem aleatória sistemática
amostra_indices <- unlist(lapply(indices_grupo, function(indices) {
  seq(min(indices), length.out = tamanho_amostra_grupo[1], by = ceiling(length(indices) / tamanho_amostra_grupo[1]))
}))

# Obter a amostra do dataframe
amostra <- database[amostra_indices, ]

# Visualizar a amostra
amostra

#write.csv(amostra, "D:/amostra.csv",row.names = FALSE)



##AMOSTRAGEM POR CONGLOMERADO
# Definir o tamanho da amostra desejada
tamanho_amostra <- 40  # Neste exemplo, vamos amostrar 100 conglomerados

# Realizar a amostragem por conglomerados com substituição
amostra <- database %>%
  group_by(unidade) %>%
  sample_n(size = tamanho_amostra, replace = TRUE)

# Visualizar a amostra
amostra

#write.csv(amostra, "D:/amostra_CONGLOMERADOS.csv",row.names = FALSE)
