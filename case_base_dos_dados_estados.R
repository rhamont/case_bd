library(basedosdados)
library(data.table)
library(tidyverse)
library(udunits2)
library(units)
library(geobr)
library(sf)
library(ggplot2)
library(cowplot)
library(RColorBrewer)


# Defina o seu projeto no Google Cloud

set_billing_id("case-base-dos-dados")


# Para carregar o dado direto no R

query_re_orc <- bdplyr("br_me_siconfi.municipio_receitas_orcamentarias")
df_re_orc <- bd_collect(query_re_orc)

query_dp_orc <- bdplyr("br_me_siconfi.municipio_despesas_orcamentarias")
df_dp_orc <- bd_collect(query_dp_orc)

query_eleicoes_munic <- bdplyr("br_tse_eleicoes.resultados_partido_municipio")
df_eleicoes_munic <- bd_collect(query_eleicoes_munic)


#Apagando variáveis irrelevantes para a análise

df_dp_orc = subset(df_dp_orc, select = -c(estagio, portaria, conta, estagio_bd, id_conta_bd, conta_bd))
df_re_orc = subset(df_re_orc, select = -c(estagio, portaria, conta, estagio_bd, id_conta_bd, conta_bd))


#Renomeando as variáveis de valor

df_dp_orc <- rename(df_dp_orc, valor_dp = valor)
df_re_orc <- rename(df_re_orc, valor_re = valor)


#Transformando a variável de id_municipio em numérica

df_dp_orc$id_municipio <- as.integer(df_dp_orc$id_municipio)
df_re_orc$id_municipio <- as.integer(df_re_orc$id_municipio)


#Simplificando os dados da base de eleições municipais

df_eleicoes_munic <- filter(df_eleicoes_munic, cargo == "presidente", turno == 1, sigla_partido == c("PT", "PSDB"))

df_eleicoes_munic <- filter(df_eleicoes_munic, ano>=1998)

df_eleicoes_munic$num_eleicao <- 0

df_eleicoes_munic$num_eleicao <- ifelse(df_eleicoes_munic$ano==1998, 1, df_eleicoes_munic$num_eleicao)
df_eleicoes_munic$num_eleicao <- ifelse(df_eleicoes_munic$ano==2002, 2, df_eleicoes_munic$num_eleicao)
df_eleicoes_munic$num_eleicao <- ifelse(df_eleicoes_munic$ano==2006, 3, df_eleicoes_munic$num_eleicao)
df_eleicoes_munic$num_eleicao <- ifelse(df_eleicoes_munic$ano==2010, 4, df_eleicoes_munic$num_eleicao)
df_eleicoes_munic$num_eleicao <- ifelse(df_eleicoes_munic$ano==2014, 5, df_eleicoes_munic$num_eleicao)
df_eleicoes_munic$num_eleicao <- ifelse(df_eleicoes_munic$ano==2018, 6, df_eleicoes_munic$num_eleicao)

df_eleicoes_munic <- df_eleicoes_munic %>%
  pivot_wider(names_from = sigla_partido, values_from = votos_nominais)

df_eleicoes_munic <- subset(df_eleicoes_munic, 
                            select = -c(ano, turno, tipo_eleicao, id_municipio_tse, cargo, votos_nao_nominais))

df_eleicoes_munic$id_municipio <- as.integer(df_eleicoes_munic$id_municipio)

df_eleicoes_munic <- df_eleicoes_munic %>% 
  drop_na(id_municipio)

df_eleicoes_munic$PT <- replace(df_eleicoes_munic$PT, is.na(df_eleicoes_munic$PT), 0)
df_eleicoes_munic$PSDB <- replace(df_eleicoes_munic$PSDB, is.na(df_eleicoes_munic$PSDB), 0)

df_eleicoes_munic <- df_eleicoes_munic %>%
  group_by(num_eleicao, sigla_uf) %>%
  summarise(PSDB_uf = sum(PSDB), PT_uf = sum(PT))

df_eleicoes_munic <- subset(df_eleicoes_munic, sigla_uf != "DF")


#Filtrando as bases de receitas e despesas apenas para os anos de 1991 a 2018

df_re_orc <- filter(df_re_orc, 1995 <= ano, ano <= 2018)
df_dp_orc <- filter(df_dp_orc, 1995 <= ano, ano <= 2018)


#Calculando média de receitas e despesas orçamentárias para os quatro anos anteriores a cada uma das eleicões presidenciais

df_dp_orc$num_eleicao <- 0

df_dp_orc$num_eleicao <- ifelse(df_dp_orc$ano>=1995 & df_dp_orc$ano<=1998, 1, df_dp_orc$num_eleicao)
df_dp_orc$num_eleicao <- ifelse(df_dp_orc$ano>=1999 & df_dp_orc$ano<=2002, 2, df_dp_orc$num_eleicao)
df_dp_orc$num_eleicao <- ifelse(df_dp_orc$ano>=2003 & df_dp_orc$ano<=2006, 3, df_dp_orc$num_eleicao)
df_dp_orc$num_eleicao <- ifelse(df_dp_orc$ano>=2007 & df_dp_orc$ano<=2010, 4, df_dp_orc$num_eleicao)
df_dp_orc$num_eleicao <- ifelse(df_dp_orc$ano>=2011 & df_dp_orc$ano<=2014, 5, df_dp_orc$num_eleicao)
df_dp_orc$num_eleicao <- ifelse(df_dp_orc$ano>=2015 & df_dp_orc$ano<=2018, 6, df_dp_orc$num_eleicao)

df_dp_orc <- df_dp_orc %>%
  group_by(num_eleicao, id_municipio, sigla_uf) %>%
  summarise(despesas_orc_media = mean(valor_dp, na.rm = TRUE))

df_dp_orc <- df_dp_orc %>%
  group_by(num_eleicao, sigla_uf) %>%
  summarise(despesas_orc_media_uf = sum(despesas_orc_media, na.rm = TRUE))

df_re_orc$num_eleicao <- 0

df_re_orc$num_eleicao <- ifelse(df_re_orc$ano>=1995 & df_re_orc$ano<=1998, 1, df_re_orc$num_eleicao)
df_re_orc$num_eleicao <- ifelse(df_re_orc$ano>=1999 & df_re_orc$ano<=2002, 2, df_re_orc$num_eleicao)
df_re_orc$num_eleicao <- ifelse(df_re_orc$ano>=2003 & df_re_orc$ano<=2006, 3, df_re_orc$num_eleicao)
df_re_orc$num_eleicao <- ifelse(df_re_orc$ano>=2007 & df_re_orc$ano<=2010, 4, df_re_orc$num_eleicao)
df_re_orc$num_eleicao <- ifelse(df_re_orc$ano>=2011 & df_re_orc$ano<=2014, 5, df_re_orc$num_eleicao)
df_re_orc$num_eleicao <- ifelse(df_re_orc$ano>=2015 & df_re_orc$ano<=2018, 6, df_re_orc$num_eleicao)

df_re_orc <- df_re_orc %>% 
  group_by(num_eleicao, id_municipio, sigla_uf) %>%
  summarise(receitas_orc_media = mean(valor_re, na.rm = TRUE))

df_re_orc <- df_re_orc %>%
  group_by(num_eleicao, sigla_uf) %>%
  summarise(receitas_orc_media_uf = sum(receitas_orc_media, na.rm = TRUE))

orcamento <- merge(df_re_orc, df_dp_orc, by = c('sigla_uf', 'num_eleicao'), sort = TRUE, all=TRUE)
orcamento <- orcamento[complete.cases(orcamento), ]

orcamento$excedente_orc <- orcamento$receitas_orc_media - orcamento$despesas_orc_media


#Fazendo o merge da base de orçamento e de resultados das eleições

final <- merge(orcamento, df_eleicoes_munic, by = c('num_eleicao', 'sigla_uf'), sort = TRUE, all=TRUE)
summary(final)


#Criando a variável de correlação entre o excedente orçamentário e o número de votos nominais em cada partido (PT, PSDB)

final <- final %>% 
  group_by(sigla_uf) %>%
  summarise(corr_pt = cor(excedente_orc, PT_uf), corr_psdb = cor(excedente_orc, PSDB_uf))


#Codificando os Estados através da sigla_uf

final$cod_uf <- 0
final$cod_uf <- ifelse(final$sigla_uf=="RO", 11, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="AC", 12, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="AM", 13, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="RR", 14, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="PA", 15, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="AP", 16, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="TO", 17, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="MA", 21, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="PI", 22, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="CE", 23, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="RN", 24, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="PB", 25, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="PE", 26, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="AL", 27, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="SE", 28, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="BA", 29, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="MG", 31, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="ES", 32, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="RJ", 33, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="SP", 35, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="PR", 41, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="SC", 42, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="RS", 43, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="MS", 50, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="MT", 51, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="GO", 52, final$cod_uf)
final$cod_uf <- ifelse(final$sigla_uf=="DF", 53, final$cod_uf)

final$nome_uf <- "a"
final$nome_uf <- ifelse(final$sigla_uf=="RO", "Rondônia", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="AC", "Acre", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="AM", "Amazônas", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="RR", "Roraima", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="PA", "Pará", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="AP", "Amapá", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="TO", "Tocantins", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="MA", "Maranhão", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="PI", "Piauí", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="CE", "Ceará", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="RN", "Rio Grande do Norte", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="PB", "Paraíba", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="PE", "Pernambuco", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="AL", "Alagoas", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="SE", "Sergipe", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="BA", "Bahia", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="MG", "Minas Gerais", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="ES", "Espírito Santo", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="RJ", "Rio de Janeiro", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="SP", "São Paulo", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="PR", "Paraná", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="SC", "Santa Catarina", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="RS", "Rio Grande do Sul", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="MS", "Mato Grosso do Sul", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="MT", "Mato Grosso", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="GO", "Goiás", final$nome_uf)
final$nome_uf <- ifelse(final$sigla_uf=="DF", "Distrito Federal", final$nome_uf)


#Preparando para fazer os mapas de correlação

states <- read_country(year = 2019)
states$name_state <- tolower(states$name_state)
final$nome_uf <- tolower(final$nome_uf)
states <- dplyr::left_join(states, final, by = c("name_state" = "nome_uf")); states


#Mapa de correlação do número de votos do PT com o excedente orçamentário

png(filename="corr_votos_exc_orc_pt.png", height=20, width=20, unit="cm", res=200)
  states %>% ggplot() + 
    geom_sf(aes(fill = corr_pt), size = .15) +   
    scale_fill_gradient(low = "red", high = "blue", name = "Correlação", limits = c(-1, 1)) + 
    xlab("") +  
    ylab("") +
    geom_sf_label(aes(label = abbrev_state), label.padding = unit(0.5, "mm"),size = 3) + 
    labs(title = "Correlação de votos nominais no PT e excedente orçamentário") +
    theme(plot.caption = element_text(hjust = 0, face= "italic"), 
          plot.title.position = "plot", 
          plot.caption.position =  "plot") + 
    theme(legend.position = "bottom") + theme(legend.title = element_text(size = 10), legend.text=element_text(size=10))
dev.off()


#Mapa de correlação do número de votos do PSDB com o excedente orçamentário

png(filename="corr_votos_exc_orc_psdb.png", height=20, width=20, unit="cm", res=200)
  states %>% ggplot() + 
    geom_sf(aes(fill = corr_psdb), size = .15) +   
    scale_fill_gradient(low = "red", high = "blue", name = "Correlação", limits = c(-1, 1)) + 
    xlab("") +  
    ylab("") +
    geom_sf_label(aes(label = abbrev_state), label.padding = unit(0.5, "mm"), size = 3) + 
    labs(title = "Correlação de votos nominais no PSDB e excedente orçamentário") +
    theme(plot.caption = element_text(hjust = 0, face= "italic"), 
          plot.title.position = "plot", 
          plot.caption.position =  "plot") + 
    theme(legend.position = "bottom") + theme(legend.title = element_text(size = 10), legend.text=element_text(size=10))
dev.off()


  
getwd()
