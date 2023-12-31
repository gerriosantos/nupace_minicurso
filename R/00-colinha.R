

rm(list = ls())
gc()

library(tidyverse)


# Fazer qrcode da pagina no github


# readr ----



df <- read_delim(file = 'data-raw/municipio.csv', delim = ',')


write_rds(x = df, file = 'path')


##### EXPLICANDO PIPE (|>): ----------------------------------------------

'data-raw/municipio.csv' |>
  read_delim(delim = ',')

#### ---------------------------------------------------------------------


# ----------------------------------------------------------------------

# dplyr ----

#' Função `filter()`


d1 <- df |>
  filter(sigla_uf == 'CE' & nome == 'Coreaú')


d2 <- df |> filter(sigla_uf == 'CE' | sigla_uf == 'PB')


d3 <- df |> filter(!sigla_uf %in% c('CE', 'PB'))




#

library(stringr)

f(x1, x2)

stringr::str_detect()


d4 <- df |>
  filter(str_detect(string = nome, pattern = '^A.*e$'))

d5 <- df |>
  filter(str_detect(string = nome, pattern = 'Coreaú'))



table(d4$sigla_uf)



#' Como funciona o pacote `stringr`

nn <- mtcars |> data.frame() |>
  as_tibble(rownames = NA) |>
  rownames_to_column(var = 'nomes')

str_extract(nn$nomes, pattern = '[0-9]+') # ou usar '[aA-zZ]+' para letras.

str_detect(nn$nomes, pattern = '[0-9]+')



str_sub(string = 'gerrio dos santos', 1, 7)

str_sub(string = 'gerrio barbosa', -7, 14)

# ----------------------------------------------------------------------



#' Função `select()`

# usar apenas o estado do Ceará.
dd <- df |>  filter(sigla_uf == 'CE')


d1 <- dd |>
  select(id_municipio, nome, capital_uf, nome_uf, nome_regiao)

d2 <- dd |>
  select(id_municipio:nome)

d3 <- dd |>
  select(starts_with('id'))

d4 <- dd |>
  select(starts_with('id') & !ends_with('_6'))

# ----------------------------------------------------------------------

#' Função `rename`

dd <- df |>
  filter(sigla_uf == 'CE') |>
  rename(id_mun = id_municipio, id_mun6 = id_municipio_6)


#' Função `mutate`

dd <- df |>  filter(sigla_uf == 'CE') |>
  mutate(nome1 = abjutils::rm_accent(nome),
         ddd_1 = ifelse(ddd == 88, 1, 0))


# Base finbra_uf
dd <- read_rds('data-raw/finbra_uf.rds') |>
  mutate(valor_1 = valor / 1000000000,
         valor_1 = round(valor_1, 3))


# ----------------------------------------------------------------------

#' `group_by()` e `summarise()`


d1 <- df |>
  group_by(sigla_uf) |>
  summarise(
    n = n(),

    across(
      .cols = c(nota_saeb_lingua_portuguesa, nota_saeb_matematica),
      .fns = ~ mean(.x, na.rm = TRUE))

  )






# JOINS - Voltando ao DPLYR ----


d_x <- tibble(

  id = c('Coreaú', 'Moraújo', 'Jijoca', 'Coreaú', 'Coreaú', 'Jijoca',
         'Cariré'),
  value_x = rnorm(n = 7)
)


d_y <- tibble(
  id = c('Coreaú', 'Moraújo', 'Jijoca', 'Sobral'),
  value_y = rnorm(4)
)


# left_join

left_join(x = d_x, y = d_y, by = 'id')

# right_join

right_join(d_x, d_y)


# inner_join

inner_join(d_x, d_y)


# full_join

full_join(d_x, d_y)


# anti_join

anti_join(d_x, d_y) # tem em x, mas nao em y



anti_join(d_y, d_x) # tem em y, mas nao em x



#

df <- d_x |>
  group_by(id) |>
  summarise(value_x = mean(value_x))


# ggplot2 ----

library(ggplot2)


ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point(size = 5, colour = '#215a47')+




  theme_bw()+
  labs(title = 'Grafico de Bolinhas')+
  theme(
    axis.ticks = element_line(colour = 'blue', size = 1)
  )



data=data.frame(value=rnorm(100))

# basic histogram
p <-  ggplot(data = data, aes(x=value)) +
  geom_histogram()


mean(x = c(1:3, NA), na.rm = T)

c(1,2,3, 5, 10)






media_personalizada <- function(x){

  n <- length(x)  # Obtém o tamanho do vetor x
  soma <- sum(x)  # Calcula a soma dos valores do vetor x

  if (n > 0) {

    media <- soma / n  # Calcula a média

    return(media)

  } else {
    return(NA)  # Retorna NA se o vetor estiver vazio
  }


}

media_personalizada(x = c(10, 20, 20, NA))




## --------------------- PROBLEMA DO DIA-A-DIA DOS DADOS -------------------------------------

## -------------------------- FUNCAO ----------------------------------


# Funcao de Padronizacao ----

f_padronizacao <- function(x){

  m = mean(x, na.rm = TRUE)
  dp = sd(x, na.rm = TRUE)

  nota_pad <- (x - m) / dp

  return(nota_pad)
}



library(readr)

df <- read_rds('data-raw/ideb.rds')



d2 <- df |>
  select(ano, id_municipio, contains('nota'), - nota_saeb_media_padronizada) |>
  rename(nota_lp = nota_saeb_lingua_portuguesa,
         nota_mt = nota_saeb_matematica) |>
  filter(ano == 2021) |>
  mutate(nota_mt_padrao = f_padronizacao(x = nota_mt),
         nota_lp_padrao = f_padronizacao(x = nota_lp))



d2 <- df |>
  select(ano, id_municipio, contains('nota'), - nota_saeb_media_padronizada) |>
  rename(nota_lp = nota_saeb_lingua_portuguesa,
         nota_mt = nota_saeb_matematica) |>

  group_by(ano) |>

  mutate(
    across(.cols = c(nota_mt, nota_lp), .fns =  f_padronizacao, .names = '{col}_padrao')
  ) |>
  drop_na() |>
  select(- c(nota_lp, nota_mt))



#
#
# d2 <- df |>
#   select() |>
#
#   rename(
#     nota_mt = nota_saeb_matematica, nota_lp = nota_saeb_lingua_portuguesa
#   ) |>
#   # group_by(ano) |>
#   mutate(
#     across(
#       starts_with('nota'), ~ f_padronizacao(.x), .names = '{col}_pd'
#     )
#   )

# testar a padronizacao

d2 |> group_by(ano) |>
  summarise(
    across(ends_with('padrao'), list(med = mean, dp = sd), na.rm = TRUE),
  )


rm(list = ls())
gc()

# ------------------------------------------------------------------------

#' `*_join()`
#'
#'

# Base despesa educacao 2019 ----
f <- read_rds('data-raw/finbra_mun.rds') |>
  filter(ano == 2019, estagio == 'Despesas Empenhadas',
         str_detect(string = id_conta_bd, pattern = "3.12.000")
  ) |>
  select(ano, id_municipio, valor)

# Base Ideb 2019 ----

d <- read_rds('data-raw/ideb.rds') |>
  filter(ano == 2019, sigla_uf == 'CE', rede == 'municipal') |>
  select(ano, id_municipio, ideb)


# Usando join.

df <- left_join(f, d)


# -----------------------------------------------------------------------------

# tidyr ----


#' `pivot_wider()` e `pivot_longer()`

d <- read_rds('data-raw/ideb.rds') |>
  filter(ano %in% c(2007, 2017), sigla_uf == 'CE', rede == 'municipal') |>
  select(id_municipio, ano, nota_mt = nota_saeb_matematica) |>
  arrange(ano) |>
  pivot_wider(names_from = ano, values_from = nota_mt)


d1 <- d |>
  pivot_longer(-id_municipio, names_to = 'ano', values_to = 'notas_mt')


#' `drop_na()`

d2 <- read_rds('data-raw/ideb.rds') |>
  drop_na()



# Miscelânia ------------------------------------------------------------------

# 1) Despesa Educ vs Ideb


f <- read_rds('data-raw/finbra_mun.rds') |>
  filter(ano == 2019, estagio == 'Despesas Empenhadas',
         str_detect(string = id_conta_bd, pattern = "3.12.000")
  ) |>
  select(ano, id_municipio, valor)

d <- read_rds('data-raw/ideb.rds') |>
  filter(ano == 2019, sigla_uf == 'CE', rede == 'municipal') |>
  select(ano, id_municipio, ideb)


p <- read_rds('data-raw/pop.rds') |>
  filter(ano == 2019) |>
  select(-sigla_uf)

# purrr ----

# df <- left_join(x = d, y = f, by = c('ano', 'id_municipio')) |>
#   left_join(y = p, by = c('ano', 'id_municipio'))


bases <- list(f, d, p)




df <- reduce(bases, left_join) |>
  mutate(desp_per_capita = round(valor / populacao, 2)) |>
  select(-c(valor, populacao))


#' `ggplot()`

# ggplot(data = df, mapping = aes(x = ideb, y = desp_per_capita))+
#   geom_point(color = 'blue', size = 4)+
#   theme_bw()



g1 <- ggplot(data = df, mapping = aes(x = ideb, y = desp_per_capita))+
  geom_point(color = 'blue', size = 3)+
  theme_bw()

# Fazer um mapa ----

library(geobr)

geom <- read_municipality(year = 2019) |>
  filter(abbrev_state == 'CE') |>
  select(id_municipio = code_muni, nome_mun = name_muni, geom) |>
  mutate(id_municipio = as.character(id_municipio))


df_1 <- left_join(df, geom, by = 'id_municipio') |>
  sf::st_as_sf()


g2 <- ggplot(df_1, aes(fill = desp_per_capita))+
  geom_sf(color = NA)+
  scale_fill_viridis_c()+
  theme_minimal()+
  theme(#axis.line = element_blank(),
    axis.text = element_blank())+
  labs(title = 'Mapa do Ceará - Despesa 2019', fill = '')



# Fazer gráfico de linha com a despesa per capita, de 2010 a 2020.


pop <- read_rds('data-raw/pop.rds') |>
  filter(ano %in% c(2013:2020)) |>
  select(-sigla_uf) |>
  arrange(ano)


ff <- read_rds('data-raw/finbra_mun.rds') |>
  filter(ano %in% c(2010:2020), estagio == 'Despesas Empenhadas',
         str_detect(string = id_conta_bd, pattern = "3.12.000")
  ) |>
  select(ano, id_municipio, valor) |>
  left_join(pop, by = c('ano', 'id_municipio')) |>

  group_by(ano) |>
  summarise(
    across(c(valor, populacao), ~ mean(.x, na.rm = TRUE))
  ) |>

  mutate(desp_per_capita = round(valor/populacao, 2))


g3 <- ggplot(ff, aes(x = ano, y = desp_per_capita))+
  geom_line(size = 1, color = 'blue')+
  geom_point(size = 3, color = 'red')+
  scale_x_continuous(breaks = seq(2013, 2020, 1))+
  scale_y_continuous(n.breaks = 10)


# salvar os graficos


lista_graf <- list(g1, g2, g3)

file_name <- c('g1', 'g2', 'g3')



walk2(
  lista_graf,  file_name,
  ~ ggsave(filename = glue::glue('{.y}.pdf'), plot = .x, path = 'figures',
           device = cairo_pdf, width = 10, height = 7, scale = 2))


# PNG
# ggsave(filename = glue::glue('{.y}.png'), plot = .x, path = 'figures',
#        device = 'png',
#        width = 1280, height = 720, units = 'px',
#        scale = 3)



# ----------------------- IMPORTACAO COVID -------------------------------------

# Mortes Covid Ceara ----

# USANDO BRASIL IO ----

# https://www.conass.org.br/painelconasscovid19/

library(data.table)

p <- "https://data.brasil.io/dataset/covid19/caso.csv.gz"



# p <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
# https://github.com/turicas/covid19-br/blob/master/api.md#boletim

dd <- fread(input = p)


d1 <- dd |>
  mutate(ano = lubridate::year(date)) |>
  filter(place_type == 'city', ano == 2020, state == 'CE') |>
  select(date, ano, city_ibge_code, state, confirmed, deaths) |>
  group_by(ano, cod_ibge = city_ibge_code) |>
  filter(date == max(date))




d1 |> group_by(ano) |> summarise(d = sum(deaths))





# USANDO INTEGRA SUS ----

# Pegar microdados covid do integrasus

p_ce <- 'http://download-integrasus.saude.ce.gov.br/download'

dd <- fread(input = p_ce)



