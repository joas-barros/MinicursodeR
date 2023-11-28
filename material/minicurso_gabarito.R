#install.packages("tidyverse")
library(tidyverse)

#Configura o diretorio de trabalho do ambiente

imdb <- read_csv("imdb.csv")

#Operador pipe
x <- c(1, 2, 3, 4)

sqrt(sum(x))

x %>% 
  sum() %>% 
  sqrt()

# Select()

imdb %>% 
  select(titulo)

imdb %>% 
  select(titulo, ano, orcamento)

View(imdb %>% select(titulo:generos))

imdb %>% 
  select(-ano, -direcao)

# 1 - Crie uma tabela com apenas as colunas titulo, direcao, e orcamento. Salve 
# em um objeto chamado imdb_simples.

imdb %>% 
  select(titulo, direcao, orcamento)

imdb %>% 
  select(-num_avaliacoes, -num_criticas_publico, -num_criticas_critica)

imdb %>% 
  select(-contains("num"))

# arrange()

imdb %>% 
  arrange(orcamento)

imdb %>% 
  arrange(desc(orcamento))

imdb %>% 
  arrange(desc(ano), desc(orcamento))


# 3. Ordene os filmes em ordem crescente de ano e decrescente de receita e salve
# em um objeto chamado filmes_ordenados.

imdb %>% 
  arrange(ano, desc(receita))

# 4. Selecione apenas as colunas titulo e orcamento e então ordene de forma 
# decrescente pelo orcamento.

imdb %>% 
  select(titulo, orcamento) %>% 
  arrange(desc(orcamento))

# Filter

imdb %>% 
  filter(nota_imdb > 9)

imdb %>% 
  filter(ano > 2010, nota_imdb > 8.5)

imdb %>% 
  filter(direcao %in% c("Quentin Tarantino", "Steven Spielberg"))

# 5. Crie um objeto chamado filmes_ingles apenas com filmes que sejam apenas no 
# idioma inglês (English).

filmes_ingles <- imdb %>% 
  filter(idioma == "English")

filmes_ingles

# 6. Crie um objeto chamado curtos_legais com filmes de 90 minutos ou menos de 
# duração e nota no imdb maior do que 8.5.

curtos_legais <- imdb %>% 
  filter(duracao <= 90, nota_imdb > 8.5)

# 7. Filmes do “Steven Spielberg” ordenados de forma decrescente por ano, 
# mostrando apenas as colunas titulo e ano;

imdb %>% 
  filter(direcao == "Steven Spielberg") %>% 
  arrange(desc(ano))

# Mutate()

imdb %>% 
  mutate(duracao_horas = duracao / 60)

imdb %>% 
  mutate(houve_lucro = ifelse(receita > orcamento, "Sim", "não"))

# 8 - Crie uma coluna chamada prejuizo (orcamento - receita) e salve a nova 
#tabela em um objeto chamado imdb_prejuizo. Em seguida, filtre apenas os filmes 
#que deram prejuízo e ordene a tabela por ordem crescente de prejuízo.

imdb_prejuizo <- imdb %>% 
  mutate(prejuizo = orcamento - receita) %>% 
  filter(prejuizo > 0) %>% 
  arrange(prejuizo)

# 9 - Crie uma nova coluna que classifique o filme em "recente" (posterior a 
# 2000) e "antigo" (de 2000 para trás).

imdb <- imdb %>% 
  mutate(idade = ifelse(ano > 2000, "Recente", "Antigo"))

# Summarize():

imdb %>% 
  summarise(
    media_orcamento = mean(orcamento, na.rm = TRUE),
    media_receita = mean(receita, na.rm = TRUE),
    media_lucro = mean(receita - orcamento, na.rm = TRUE)
  )

# Group_by()

imdb %>% 
  filter(!is.na(producao), !is.na(receita)) %>% 
  group_by(producao) %>% 
  summarise(receita_media = mean(receita, na.rm = TRUE))

# 10. Retorne tabelas apenas com:
# a. a nota IMDB média dos filmes por ano de lançamento; 
imdb %>% 
  group_by(ano) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE))

# b. a receita média e mediana dos filmes por ano; 
imdb %>% 
  group_by(ano) %>% 
  filter(!is.na(receita)) %>% 
  summarise(receita_media = mean(receita, na.rm = TRUE),
            receita_mediana = median(receita, na.rm = TRUE))

# c. apenas o nome das pessoas que dirigiram mais de 10 filmes.
imdb %>% 
  group_by(direcao) %>% 
  summarise(
    n_de_filmes = n()
  ) %>% 
  filter(n_de_filmes > 10)

# Exportar uma base de dados tratada
# install.packages("writexl")
library(writexl)

write_xlsx(curtos_legais, "curtos_legais.xlsx")

#ggplot2

#Estrutura
imdb %>% 
  mutate(lucro = receita - orcamento) %>% 
  ggplot() +
  geom_point(mapping = aes(x = orcamento, y = receita, color = lucro)) +
  labs(
    x = "Orçamento ($)",
    y = "Receita ($)",
    color = "Lucro ($)",
    title = "Gráfico de dispersão",
    subtitle = "Receita vs Orçamento"
  )

################################# DESAFIO ######################################

#Tratando a base

piores_diretores <- imdb %>% 
  mutate(prejuizo = orcamento - receita) %>% 
  filter(prejuizo > 0) %>% 
  group_by(direcao) %>% 
  summarise(
    n_de_filmes = n()
  ) %>% arrange(desc(n_de_filmes)) %>% 
  head(5)

#Criando o gráfico

piores_diretores %>% 
  mutate(direcao = fct_reorder(direcao, n_de_filmes)) %>% 
  ggplot() +
  geom_col(aes(x = direcao, y = n_de_filmes, fill = direcao), show.legend = FALSE) +
  geom_label(aes(x = direcao, y = n_de_filmes / 2, label = n_de_filmes)) + 
  coord_flip() +
  labs(
    title = "Piores Diretores",
    subtitle = "Diretores com mais filmes com prejuizos"
  )
