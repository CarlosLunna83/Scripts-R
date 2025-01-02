# Plano de Leitura 2024

library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(janitor)
library(readODS)
library(scales)

stats_plano_de_leitura_2024 <- read_ods("Plano de Leitura 2024.ods")
stats_plano_de_leitura_2024


stats_plano_de_leitura_2024 <- clean_names(stats_plano_de_leitura_2024)
stats_plano_de_leitura_2024

# Total de páginas lidas em 2024

paginas_lidas_2024 <- stats_plano_de_leitura_2024 %>%
  select(mes_de_conclusao, paginas) %>%
  filter(!is.na(mes_de_conclusao)) %>%
  summarise(total_paginas = sum(paginas))

paginas_lidas_2024

# Rodando o código acima temos o total de páginas lidas no ano de 2024 = 3119

################################################################################################

# Total de Páginas por mês

paginas_mes_2024 <- stats_plano_de_leitura_2024 %>%
  filter(!is.na(mes_de_conclusao))%>%
  mutate(PÁGINAS = as.numeric(paginas)) %>%
  group_by(mes_de_conclusao) %>%
  summarise(soma_total = sum(paginas)) %>%
  arrange(desc(soma_total))

paginas_mes_2024

# Rodando o código acima temos o número de páginas por mês:

#  1 janeiro                1226
#  2 fevereiro               496
#  3 marco                   313
#  4 junho                   271
#  5 abril                   241
#  6 outubro                 225
#  7 agosto                  177
#  8 julho                   117
#  9 maio                     12

view(paginas_mes_2024)

# Gráfico de Barras

quanti_paginas_2024 <- c(1226, 496, 313, 241, 12, 271, 177, 177, 225)
quanti_paginas_2024
mês <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago",
         "Out")

# Gráfico com Barplot:

barplot_qtd_paginas_2024 <- barplot(quanti_paginas_2024, names.arg=mês, 
                                    main="Páginas por Mês", 
                                    xlab = "Meses do Ano",
                                    ylab="Quantidade de Páginas",
                                    labs(quanti_paginas_2024))
                                    
# Gráfico com ggplot2:

paginas_mes_2024 %>%
  ggplot(aes(x= mes_de_conclusao,
            y= soma_total,
            label = soma_total)) +
  geom_bar(stat = "identity") +
  geom_label(size=4)
  


class(stats_plano_de_leitura_2024$mes_de_conclusao)
class(stats_plano_de_leitura_2024$paginas)

pagina_por_mes_conclusao_2024  

# uma opção para exportar figuras em jpeg
#selecionar as duas linhas e rodar
dev.copy(device = jpeg, file = "paginas_por_mes_2023", width=600, height=500, res=100)
dev.off()

# uma opção para exportar figuras em PDF
#selecionar as duas linhas e rodar
dev.copy(device = pdf, file="paginas_por_mes_2023", width = 600, paper="USr")
dev.off()

#################################################################################################

# Total de Livros concluídos 2024

stats_plano_de_leitura_2024

total_livros_concluidos_2024 <- stats_plano_de_leitura_2024 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  filter(!str_detect(status, "Não Lido")) %>%
  summarise(livros_concluidos = sum(qtd_de_unidade))

total_livros_concluidos_2024

#basta rodar o código acima e temos os total de livros lidos no ano 29
#(contando tudo, livros, artigos, etc)  

#################################################################################################
                                    
# Distribuição dos livros por país:

livros_por_pais_2024 <- stats_plano_de_leitura_2024 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, pais, qtd_de_unidade) %>%
  group_by(pais) %>%
  summarise(livros_pais = sum(qtd_de_unidade)) %>%
  arrange(desc(livros_pais))

livros_por_pais_2024

# Rodando o código acima temos a distribuição de livros por país:

#  1 Brasil                  24
#  2 Holanda                  2
#  3 Argentina                1
#  4 Estados Unidos           1
#  5 Suécia                   1


# Gráfico de Setor (Pizza): 

quantidade_de_livros_por_país_2024 = c(24, 2, 1, 1, 1)
names(quantidade_de_livros_por_país_2024) = c("Brasil", "Holanda", "Argentina", "Estados Unidos",
                                              "Suécia")

quantidade_de_livros_por_país_2024

plot(quantidade_de_livros_por_país_2024)  
pie(quantidade_de_livros_por_país_2024, main = "Livros Por País 2024")

################################################################################################

# Distribuição das leituras por Estado

livros_por_estado_2024 <- stats_plano_de_leitura_2024 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!is.na(estado)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, estado, qtd_de_unidade) %>%
  group_by(estado) %>%
  summarise(livros_estado = sum(qtd_de_unidade)) %>%
  arrange(desc(livros_estado))

livros_por_estado_2024


# Rodando o código acima temos a distribuição de livros por Estado em 2024:

# 1 Pernambuco                              5
# 2 Pará                                    3
#   Rio de Janeiro                          3
#   São Paulo                               3
# 5 Bahia                                   2
#   Minas Gerais                            2
# 7 Goiás                                   1
#   Maranhão                                1
#   Paraná                                  1
#   Rio Grande do Norte                     1


# Os gráficos abaixo serão gerados a partir da distribuição das co-autorias por Estado:

quantidade_de_livros_por_estado_2024 = c(5, 3, 3, 3, 2, 2, 1, 1, 1, 1)
names(quantidade_de_livros_por_estado_2024) = c("PE", "PA", "RJ", "SP", "BA", "MG", "GO", "MA", "PR", "RN")

# Gráfico de setor (pizza)

plot(quantidade_de_livros_por_estado_2024)  
pie(quantidade_de_livros_por_estado_2024, main = "Livros Por Estado 2024")

#Calculo da porcentagem
porc = round(quantidade_de_livros_por_estado_2024*100/sum(quantidade_de_livros_por_estado_2024), 2 )

# Códigos dos rótulos
rótulos = paste("(", porc, "%", sep="")

# Plot do Gráfico
pie(quantidade_de_livros_por_estado_2024, main = "Livros Por Estado 2024", labels = rótulos,
    col = rainbow(10))

# Legenda

legend(1.8, 1, names(quantidade_de_livros_por_estado_2024), col=rainbow(10), pch=rep(20,6),
       cex = 0.65)

# uma opção para exportar figuras em jpeg
#selecionar as duas linhas e rodar
dev.copy(device = jpeg, file = "livros_por_estado_2023.jpeg", width=600, height=500, res=100)
dev.off()

# uma opção para exportar figuras em PDF
#selecionar as duas linhas e rodar
dev.copy(device = pdf, file="livros_por_estado_2023.pdf", width = 600, paper="USr")
dev.off()

################################################################################################

###############################################################################################

# Distribuição por tipo de leitura


tipo_de_leitura_2024 <- stats_plano_de_leitura_2024 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  select(tipo, qtd_de_unidade) %>%
  group_by(tipo) %>%
  summarise(quantidade_tipo = sum(qtd_de_unidade)) %>%
  arrange(desc(quantidade_tipo))

tipo_de_leitura_2024

# Rodando o código acima o resultado foi:

# 1 Artigo                     14
# 2 Livro                      10
# 3 Dissertação                 3
#   Tese                        2
