# Plano de Leitura 2023

library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(janitor)
library(readODS)

stats_plano_de_leitura_2023 <- read_ods("Plano de Leitura 2023.ods")
stats_plano_de_leitura_2023

stats_plano_de_leitura_2023 <- clean_names(stats_plano_de_leitura_2023)
stats_plano_de_leitura_2023

# Total de páginas lidas em 2022

paginas_lidas_2023 <- stats_plano_de_leitura_2023 %>%
  select(mes_de_conclusao, paginas) %>%
  filter(!is.na(mes_de_conclusao)) %>%
  summarise(total_paginas = sum(paginas))

paginas_lidas_2023

# Rodando o código acima temos o total de páginas lidas no ano de 2023 = 4261

################################################################################################

# Total de Páginas por mês

paginas_mes_2023 <- stats_plano_de_leitura_2023 %>%
  filter(!is.na(mes_de_conclusao))%>%
  mutate(PÁGINAS = as.numeric(paginas)) %>%
  group_by(mes_de_conclusao) %>%
  summarise(soma_total = sum(paginas)) %>%
  arrange(desc(soma_total))

paginas_mes_2023

# Rodando o código acima temos o número de páginas por mês:

#  1 Setembro                852
#  2 Agosto                  687
#  3 Dezembro                656
#  4 junho                   458
#  5 marco                   422
#  6 abril                   381
#  7 Outubro                 276
#  8 Novembro                240
#  9 maio                    215
# 10 janeiro                  53
# 11 Julho                    21

# Gráfico de Barras

quanti_paginas_2023 <- c(53, 422, 381, 215, 458, 21, 687, 852, 276, 240, 656)
quanti_paginas_2023
mês <- c("Jan", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago",
         "Set", "Out", "Nov", "Dez")

barplot_qtd_paginas_2023 <- barplot(quanti_paginas_2023, names.arg=mês, 
                                    main="Páginas por Mês", 
                                    xlab = "Meses do Ano",
                                    ylab="Quantidade de Páginas")


#################################################################################################

# Total de Livros concluídos 2023

stats_plano_de_leitura_2023

total_livros_concluidos_2023 <- stats_plano_de_leitura_2023 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  filter(!str_detect(status, "Não Lido")) %>%
  summarise(livros_concluidos = sum(qtd_de_unidade))

total_livros_concluidos_2023

#basta rodar o código acima e temos os total de livros lidos no ano 32
#(contando tudo, livros, artigos, etc)  

#################################################################################################
                                    
# Distribuição dos livros por país:

livros_por_pais_2023 <- stats_plano_de_leitura_2023 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, pais, qtd_de_unidade) %>%
  group_by(pais) %>%
  summarise(livros_pais = sum(qtd_de_unidade)) %>%
  arrange(desc(livros_pais))

livros_por_pais_2023

# Rodando o código acima temos a distribuição de livros por país:

#  1 Brasil                  21
#  2 Inglaterra               2
#  3 Canadá                   1
#  4 Colômbia                 1
#  5 Coréia do Sul            1
#  6 Estados Unidos           1
#  7 Finlândia                1
#  8 Grécia                   1
#  9 Itália                   1
# 10 Uruguai                  1
# 11 Venezuela                1

# Gráfico de Setor (Pizza): 

quantidade_de_livros_por_país_2023 = c(21, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1)
names(quantidade_de_livros_por_país_2023) = c("Brasil", "Inglaterra", "Canadá", "Colômbia",
"Coréia do Sul", "Estados Unidos", "Finlândia", "Grécia", "Itália", "Uruguai", "Venezuela")

quantidade_de_livros_por_país_2023

plot(quantidade_de_livros_por_país_2023)  
pie(quantidade_de_livros_por_país_2023, main = "Livros Por País 2023")

################################################################################################

# Distribuição das leituras por Estado

livros_por_estado_2023 <- stats_plano_de_leitura_2023 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!is.na(estado)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, estado, qtd_de_unidade) %>%
  group_by(estado) %>%
  summarise(livros_estado = sum(qtd_de_unidade)) %>%
  arrange(desc(livros_estado))

livros_por_estado_2023


# Rodando o código acima temos a distribuição de livros por Estado em 2022:

# 1 Pernambuco                              9
# 2 São Paulo                               4
# 3 Bahia                                   3
# 4 Rio de Janeiro                          2
# 5 Alagoas                                 1
#   Maranhão                                1
#   Paraíba                                 1


# Os gráficos abaixo serão gerados a partir da distribuição das co-autorias por Estado:

quantidade_de_livros_por_estado_2023 = c(9, 4, 3, 2, 1, 1, 1)
names(quantidade_de_livros_por_estado_2023) = c("PE", "SP", "BA", "RJ", "AL", "MA", "PB")

# Gráfico de setor (pizza)

plot(quantidade_de_livros_por_estado_2023)  
pie(quantidade_de_livros_por_estado_2023, main = "Livros Por Estado 2023")

#Calculo da porcentagem
porc = round(quantidade_de_livros_por_estado_2023*100/sum(quantidade_de_livros_por_estado_2023), 2 )

# Códigos dos rótulos
rótulos = paste("(", porc, "%", sep="")

# Plot do Gráfico
pie(quantidade_de_livros_por_estado_2023, main = "Livros Por Estado 2023", labels = rótulos,
    col = rainbow(7))

# Legenda

legend(1.8, 1, names(quantidade_de_livros_por_estado_2023), col=rainbow(7), pch=rep(20,6),
       cex = 0.8)

# uma opção para exportar figuras em jpeg
#selecionar as duas linhas e rodar
dev.copy(device = jpeg, file = "livros_por_estado_2023", width=600, height=500, res=100)
dev.off()

# uma opção para exportar figuras em PDF
#selecionar as duas linhas e rodar
dev.copy(device = pdf, file="livros_por_estado_2023.pdf", width = 600, paper="USr")
dev.off()

################################################################################################

###############################################################################################

# Distribuição por tipo de leitura


tipo_de_leitura_2023 <- stats_plano_de_leitura_2023 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  select(tipo, qtd_de_unidade) %>%
  group_by(tipo) %>%
  summarise(quantidade_tipo = sum(qtd_de_unidade)) %>%
  arrange(desc(quantidade_tipo))

tipo_de_leitura_2023

# Rodando o código acima o resultado foi:

# 1 Livro                     16
# 2 Artigo                    12
# 3 Dissertação                1
#   Relatório                  1
#   TCC                        1
#   Tese                       1