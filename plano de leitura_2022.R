# Plano de Leitura 2022

library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(janitor)
library(readODS)


stats_plano_de_leitura_2022 <- read_ods("Plano de Leitura 2022.ods")
stats_plano_de_leitura_2022

stats_plano_de_leitura_2022 <- clean_names(stats_plano_de_leitura_2022)
stats_plano_de_leitura_2022

# Total de páginas lidas em 2022

paginas_lidas_2022 <- stats_plano_de_leitura_2022 %>%
  select(mes_de_conclusao, paginas) %>%
  filter(!is.na(mes_de_conclusao)) %>%
  summarise(total_paginas = sum(paginas))

paginas_lidas_2022


# Rodando o código acima temos o total de páginas lidas no ano de 2022 = 3119

################################################################################################

# Total de Páginas por mês

paginas_mes_2022 <- stats_plano_de_leitura_2022 %>%
  filter(!is.na(mes_de_conclusao))%>%
  mutate(PÁGINAS = as.numeric(paginas)) %>%
  group_by(mes_de_conclusao) %>%
  summarise(soma_total = sum(paginas)) %>%
  arrange(desc(soma_total))

paginas_mes_2022

# Rodando o código acima temos o número de páginas por mês

# Gráfico de Barras

quanti_paginas_2022 <- c(979, 331, 41, 225, 286, 554, 122, 151, 294, 136)
quanti_paginas_2022
mês <- c("Jan", "Mar", "Abr", "Mai", "Jul", "Ago",
         "Set", "Out", "Nov", "Dez")

barplot_qtd_paginas_2022 <- barplot(quanti_paginas_2022, names.arg=mês, 
                                    main="Páginas por Mês", 
                                    xlab = "Meses do Ano",
                                    ylab="Quantidade de Páginas")


# Para exportar

# uma opção para figuras em jpeg
#selecionar as duas linhas e rodar
dev.copy(device = jpeg, file = "paginas_por_mes_2022.jpeg", width=600, height=500, res=100)
dev.off()

# uma opção para figuras em PDF
#selecionar as duas linhas e rodar
dev.copy(device = pdf, file="paginas_por_mes.pdf", width = 600, paper="USr")
dev.off()

###############################################################################################
# Total de Livros concluídos 2022

stats_plano_de_leitura_2022

total_livros_concluidos_2022 <- stats_plano_de_leitura_2022 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  filter(!str_detect(status, "Não Lido")) %>%
  summarise(livros_concluidos = sum(qtd_de_unidade))

total_livros_concluidos_2022

#basta rodar o código acima e temos os total de livros lidos no ano 18
#(contando tudo, livros, artigos, etc)  

################################################################################################

# Distribuição dos livros por país:

livros_por_pais_2022 <- stats_plano_de_leitura_2022 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, pais, qtd_de_unidade) %>%
  group_by(pais) %>%
  summarise(livros_pais = sum(qtd_de_unidade)) %>%
  arrange(desc(livros_pais))

livros_por_pais_2022

# Rodando o código acima temos a distribuição de livros por país:
# 1 Brasil                  12
# 2 Estados Unidos           3
# 3 Alemanha                 1
#   Argentina                1
#   Portugal                 1


# Gráfico de Setor (Pizza): 

quantidade_de_livros_por_país_2022 = c(12, 3, 1,1,1)
names(quantidade_de_livros_por_país_2022) = c("Brasil", "Estados Unidos", "Alemanha", "Argentina", "Portugal")

quantidade_de_livros_por_país_2022

plot(quantidade_de_livros_por_país_2022)  
pie(quantidade_de_livros_por_país_2022, main = "Livros Por País 2022")

# Só um exemplo de colocar o códido por gráfico de pizza, pois para este exemplo nenhum gráfico 
# vai representar algo muito significativo

################################################################################################

# Distribuição das leituras por Estado

livros_por_estado_2022 <- stats_plano_de_leitura_2022 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!is.na(estado)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, estado, qtd_de_unidade) %>%
  group_by(estado) %>%
  summarise(livros_estado = sum(qtd_de_unidade)) %>%
  arrange(desc(livros_estado))

livros_por_estado_2022


# Rodando o código acima temos a distribuição de livros por Estado em 2022:

# 1 Rio de Janeiro                         3
#   São Paulo                              3
# 3 Rio Grande do Sul                      2
# 4 Bahia                                  1
#   Brasília                               1
#   Pernambuco                             1
#   Rio Grande do Sul/Paraná/São Paulo     1

# Distribuindo a co-autoria para cada Estado, temos:

# 1 São Paulo                              4
# 2 Rio de Janeiro                         3
#   Rio Grande do Sul                      3
# 4 Bahia                                  1
#   Brasília                               1
#   Pernambuco                             1
#   Paraná                                 1

# Os gráficos abaixo serão gerados a partir da distribuição das co-autorias por Estado:

quantidade_de_livros_por_estado_2022 = c(4, 3, 3, 1, 1, 1, 1)
names(quantidade_de_livros_por_estado_2022) = c("SP", "RJ", "RS", "BA", "DF", "PE", "PR")

# Gráfico de setor (pizza)

plot(quantidade_de_livros_por_estado_2022)  
pie(quantidade_de_livros_por_estado_2022, main = "Livros Por Estado 2022")

#Calculo da porcentagem
porc = round(quantidade_de_livros_por_estado_2022*100/sum(quantidade_de_livros_por_estado_2022), 2 )

# Códigos dos rótulos
rótulos = paste("(", porc, "%", sep="")

# Plot do Gráfico
pie(quantidade_de_livros_por_estado_2022, main = "Livros Por Estado 2022", labels = rótulos,
    col = rainbow(7))

# Legenda

legend(1.8, 1, names(quantidade_de_livros_por_estado_2022), col=rainbow(7), pch=rep(20,6),
       cex = 0.8)

# uma opção para exportar figuras em jpeg
#selecionar as duas linhas e rodar
dev.copy(device = jpeg, file = "livros_por_estado_2022", width=600, height=500, res=100)
dev.off()

# uma opção para exportar figuras em PDF
#selecionar as duas linhas e rodar
dev.copy(device = pdf, file="livros_por_estado_2022.pdf", width = 600, paper="USr")
dev.off()

###############################################################################################

# Distribuição por tipo de leitura


tipo_de_leitura_2022 <- stats_plano_de_leitura_2022 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  select(tipo, qtd_de_unidade) %>%
  group_by(tipo) %>%
  summarise(quantidade_tipo = sum(qtd_de_unidade))

tipo_de_leitura_2022

# Rodando o código acima o resultado foi:

# 1 Livro                     12
# 2 Artigo                     4
# 3 Dissertação                1
#   Tese                       1