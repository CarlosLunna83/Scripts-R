library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(readODS)
library(readxl)

plano_de_leitura_2020 <- readxl::read_excel("Plano de leitura 2020.xlsx")
readODS::read_ods()


clean_names(plano_de_leitura_2020) 



stats <- plano_de_leitura_2020 %>%
  clean_names() 

unique(stats$tipo)

unique(stats$pais - !is.na(stats))

readxl::read_excel("")

stats_plano_de_leitura_2020 <- stats %>%
  clean_names()

unique(stats_plano_de_leitura_2020$pais - !is.na(stats_plano_de_leitura_2020))

stats_plano_de_leitura_2020 (-is.na(stats_plano_de_leitura_2020))

(!is.na(stats_plano_de_leitura_2020))

stats_plano_de_leitura_2020 %>%
  select(estado) %>%
  filter(!is.na(stats_plano_de_leitura_2020$estado))


#23 países, diminuindo os pares e tirando o NA

unique

#11 estados diferentes, tirando o NA

?fct_reorder
?summarise

nrow(stats_plano_de_leitura_2020$paginas)

total_paginas_2020 <- stats_plano_de_leitura_2020 %>%
  mutate(paginas = as.numeric(paginas)) %>%
  filter(paginas, status) %>%
  filter(str_detect(status, "Lido")) %>%
  summarise(paginas = sum(paginas))

total_paginas_2020 <- stats_plano_de_leitura_2020 %>%
  mutate(paginas = as.numeric(paginas)) %>%
  summarise(paginas = sum(total_paginas_2020$paginas))

total_paginas_2020
#total de páginas lidas em 2020 14873

media_paginas_mes_2020 <- stats_plano_de_leitura_2020 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  group_by(paginas) %>%
  summarise(media_mes = mean(paginas))
#Aqui foi uma tentativa mal sucedida de pegar a media de paginas por mes
  


paginas_mes_2020 <- stats_plano_de_leitura_2020 %>%
  filter(!is.na(mes_de_conclusao))%>%
  mutate(PÁGINAS = as.numeric(paginas)) %>%
  group_by(mes_de_conclusao) %>%
  summarise(soma_total = sum(paginas)) %>%
  arrange(desc(soma_total))

paginas_mes_2020
#Mês com mais páginas lidas Fevereiro 1615 e com menos Janeiro 72

paginas_por_pais <- stats_plano_de_leitura_2020 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  mutate(paginas = as.numeric(paginas)) %>%
  group_by(pais) %>%
  summarise(paginas_por_pais = sum(paginas)) %>%
  arrange(desc(paginas_por_pais))
#Aqui tentei calcular livros por país e acabei calculando páginas por país

livros_concluidos 



#106 livros concluídos basta rodar o código acima
  
livros_por_pais <- stats_plano_de_leitura_2020 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, pais, qtd_unidade) %>%
  group_by(pais) %>%
  summarise(livros_pais = sum(qtd_unidade)) %>%
  arrange(desc(livros_pais))
#Quantidade de livros por país. Considerando a despadronização de grafia e os livros
#com autores de diferentes países, eis a lista:
#1 Brasil 68
#2 Estados Unidos 7
#3 Hungria 4
#3 Inglaterra 4
#4 Espanha 3
#4 França 3
#5 Argentina 2
#5 Áustria 2
#5 Canadá 2
#5 Colômbia 2
#6 Alemanha 1
#6 Bélgica 1
#6 Camarões 1
#6 Coréia do Sul 1
#6 Índia 1
#6 Itália 1
#6 Peru 1
#6 Rússia 1
#6 Uruguai 1
#6 Venezuela 1

livros_por_estado <- stats_plano_de_leitura_2020 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!is.na(estado)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, estado, qtd_unidade) %>%
  group_by(estado) %>%
  summarise(livros_estado = sum(qtd_unidade)) %>%
  arrange(desc(livros_estado))
#Se for somar o resultado do código não dá os 68 
#pq não colocamos o estado no livro escrito por brasileiros e espanhóis

#1 Rio de Janeiro               19
#2 São Paulo                    17
#3 Pernambuco                    8
#4 Bahia                         7
#5 Minas Gerais                  4
#6 Pará                          3
#7 Ceará                         2
#7 Paraíba                       2
#7 Paraná                        2
#7 Rio Grande do Sul             2
#11 Piauí                        1
  
tipo_de_leitura <- stats_plano_de_leitura_2020 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  select(tipo, qtd_unidade) %>%
  group_by(tipo) %>%
  summarise(quantidade_tipo = sum(qtd_unidade))

tipo_de_leitura
 
#Resusltado tipo de leitura:
# 72 artigos
# 52 livros
#  1 dissertação
#  1 tese


  
  
 