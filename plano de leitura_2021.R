# Plano de Leitura 2021

library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(janitor)


stats_plano_de_leitura_2021 <- read_excel("Plano de leitura 2021.xlsx")
stats_plano_de_leitura_2021

stats_plano_de_leitura_2021 <- clean_names(stats_plano_de_leitura_2021)
stats_plano_de_leitura_2021

# Total de páginas lidas em 2021

paginas_lidas_2021 <- stats_plano_de_leitura_2021 %>%
  select(mes_de_conclusao, paginas) %>%
  filter(!is.na(mes_de_conclusao)) %>%
  summarise(total_paginas = sum(paginas))

paginas_lidas_5832  
# Rodando o código acima temos o total de páginas lidas no ano de 2021 = 5832

################################################################################################

# Total de Páginas por mês

paginas_mes_2021 <- stats_plano_de_leitura_2021 %>%
  filter(!is.na(mes_de_conclusao))%>%
  mutate(PÁGINAS = as.numeric(paginas)) %>%
  group_by(mes_de_conclusao) %>%
  summarise(soma_total = sum(paginas)) %>%
  arrange(desc(soma_total))

paginas_mes_2021

# Rodando o código acima temos o número de páginas por mês

# Gráfico de Barras

quanti_paginas_2021 <- c(359, 531, 240, 806, 398, 641, 716, 280, 852, 100, 909)
quanti_paginas_2021
mês <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto",
         "Setembro", "Novembro", "Dezembro")

barplot(quanti_paginas_2021, names.arg=mês, main="Páginas por Mês", xlab = "Meses do Ano",
        ylab="Quantidade de Páginas")

# Para exportar

# uma opção para figuras em jpeg
#selecionar as duas linhas e rodar
dev.copy(device = jpeg, file = "paginas_por_mes_2021.jpeg", width=600, height=500, res=100)
dev.off()

# uma opção para figuras em PDF
#selecionar as duas linhas e rodar
dev.copy(device = pdf, file="paginas_por_mes.pdf", width = 600, paper="USr")
dev.off()

###############################################################################################
# Total de Livros concluídos 2021

stats_plano_de_leitura_2021

total_livros_concluidos_2021 <- stats_plano_de_leitura_2021 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  filter(!str_detect(status, "Não Lido")) %>%
  summarise(livros_concluidos = sum(qtd_de_unidade))

  total_livros_concluidos_2021

  #basta rodar o código acima e temos os total de livros lidos no ano 29 
  #(contando tudo, livros, artigos, etc)  
  
  ################################################################################################
  
  # Distribuição dos livros por país:
  
  livros_por_pais_2021 <- stats_plano_de_leitura_2021 %>%
    filter(!is.na(mes_de_conclusao)) %>%
    filter(!str_detect(status, "Em Andamento")) %>%
    select(status, pais, qtd_de_unidade) %>%
    group_by(pais) %>%
    summarise(livros_pais = sum(qtd_de_unidade)) %>%
    arrange(desc(livros_pais))
  
  livros_por_pais_2021
  
  # Rodando o código acima temos a distribuição de livros por país:
  # 1 Brasil                  17
  # 2 Argentina                2
  # 3 Austria                  1
  # 4 Canada                   1
  # 5 Espanha                  1
  # 6 Estados Unidos           1
  # 7 França                   1
  # 8 India                    1
  # 9 Itália                   1
  # 10 Lituania                1
  # 11 Peru                    1
  # 12 Russia                  1
 
  # Gráfico de Setor (Pizza): 
  
  quantidade_de_livros_por_país = c(17, 2, 1,1,1,1,1,1,1,1,1,1)
  names(quantidade_de_livros_por_país) = c("Brasil", "Argentina", "Áustria", "Canadá", "Espanha", 
                                           "Estados Unidos", "França", "Índia", "Itália", 
                                           "Lituania", "Peru", "Rússia")
  quantidade_de_livros_por_país
  
plot(quantidade_de_livros_por_país)  
pie(quantidade_de_livros_por_país, main = "Livros Por País 2021")

# Só um exemplo de colocar o cógido por gráfico de pizza, pois para este exemplo nenhum gráfico 
# vai representar algo muito significativo

################################################################################################

# Distribuição das leituras por Estado

livros_por_estado_2021 <- stats_plano_de_leitura_2021 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  filter(!is.na(estado)) %>%
  filter(!str_detect(status, "Em Andamento")) %>%
  select(status, estado, qtd_de_unidade) %>%
  group_by(estado) %>%
  summarise(livros_estado = sum(qtd_de_unidade)) %>%
  arrange(desc(livros_estado))

livros_por_estado_2021

# Rodando o código acima temos a distribuição de livros por Estado em 2021:

# 1 Pernambuco                     5
# 2 São Paulo                      4
# 3 Paraná                         2
# 4 Rio de Janeiro                 2
# 5 Brasília                       1
# 6 Maranhão                       1
# 7 Mato Grosso do Sul             1
# 8 Rio Grande do Sul              1

quantidade_de_livros_por_estado_2021 = c(5, 4, 2, 2, 1, 1, 1, 1)
names(quantidade_de_livros_por_estado_2021) = c("PE", "SP", "PR", "RJ", "DF", "MA", "MS", "RS")

# Gráfico de setor (pizza)

plot(quantidade_de_livros_por_estado_2021)  
pie(quantidade_de_livros_por_estado_2021, main = "Livros Por Estado 2021")

#Calculo da porcentagem
porc = round(quantidade_de_livros_por_estado_2021*100/sum(quantidade_de_livros_por_estado_2021), 2 )

# Códigos dos rótulos
rótulos = paste("(", porc, "%", sep="")

# Plot do Gráfico
pie(quantidade_de_livros_por_estado_2021, main = "Livros Por Estado 2021", labels = rótulos,
    col = rainbow(7))

# Legenda

legend(1.8, 1, names(quantidade_de_livros_por_estado_2021), col=rainbow(7), pch=rep(20,6),
       cex = 0.8)

# uma opção para exportar figuras em jpeg
#selecionar as duas linhas e rodar
dev.copy(device = jpeg, file = "livros_por_estado_2020", width=600, height=500, res=100)
dev.off()

# uma opção para exportar figuras em PDF
#selecionar as duas linhas e rodar
dev.copy(device = pdf, file="livros_por_estado_2020.pdf", width = 600, paper="USr")
dev.off()

###############################################################################################

# Distribuição por tipo de leitura

# Distribuição por tipo de leitura

tipo_de_leitura_2021 <- stats_plano_de_leitura_2021 %>%
  filter(!is.na(mes_de_conclusao)) %>%
  select(tipo, qtd_de_unidade) %>%
  group_by(tipo) %>%
  summarise(quantidade_tipo = sum(qtd_de_unidade))

tipo_de_leitura_2021

# Rodando o código acima o resultado foi:

# 1 Artigo                    2
# 2 Dissertação               1
# 3 Livro                    25
# 4 Tese                      1