

rm(list = ls())
gc()

library(tidyverse)






# gasto educ vs ideb

gasto <- read_rds('data-raw/finbra_mun.rds') |>
  filter(ano == 2019 & estagio == 'Despesas Empenhadas' &
           str_detect(id_conta_bd, '3.12.000|3.12.367')) |>
  select(ano, id_conta_bd, id_municipio, valor) |>
  group_by(id_municipio) |>
  summarise(ano = first(ano), valor = sum(valor)) |>
  relocate(ano, .before = id_municipio)


ideb <- read_rds("data-raw/ideb.rds") |>
  filter(ano == 2019, sigla_uf == 'CE', rede == 'municipal') |>
  select(id_municipio, ideb_novo = ideb)


populacao <- read_rds('data-raw/pop.rds') |>
  filter(ano == 2019, sigla_uf == 'CE') |>
  select(!c(ano, sigla_uf))



library(geobr)

geom <- read_municipality(year = 2019) |>
  filter(code_state == 23) |>
  select(id_municipio = code_muni, nome = name_muni) |>
  as_tibble() |>
  mutate(id_municipio = as.character(id_municipio))


# gi <- left_join(gasto, ideb, by = join_by(id_municipio)) |>
#   select(!starts_with('ano')) |>
#   left_join(populacao, by = join_by(id_municipio)) |>
#   mutate(valor_pc = valor / populacao)


# pacote purrr substitui o for (loop)


lista <- list(gasto = gasto, ideb = ideb, pop = populacao, geom = geom)

# df <- reduce(lista, left_join)

df <- reduce(lista, ~ left_join(.x, .y, by = 'id_municipio')) |>
  mutate(valor_pc = valor / populacao) |>
  relocate(valor_pc, .before = nome)


rm(b1, gasto, gi, ideb, lista, populacao)


library(extrafont)
library(scales)
fonts()


ggplot(df, aes(valor_pc, ideb_novo)) +
  geom_point(color = 'blue', size = 5)+
  theme_minimal()+

  labs(y = 'Ideb', x = 'Despesa com Educação Municipal')+
  theme(text = element_text(
    family = 'Times New Roman', face = 'bold', size = 20, colour = 'red')) +
  scale_x_continuous(
    breaks = breaks_pretty(n = 10),
    labels = label_comma(accuracy = 0.01, decimal.mark = ',', big.mark = '.')) +
  scale_y_continuous(breaks = breaks_pretty(n = 10)) +
  geom_text(aes(label = nome))




library(sf)

df1 <- df |>
  st_as_sf()


g1 <- df1 |>
  ggplot(aes(fill = ideb_novo))+
  geom_sf()
g1


g2 <- df1 |>
  ggplot(aes(fill = valor_pc))+
  geom_sf(color = 'blue')

g2



library(glue)

l_g <- list(g1, g2)


walk2()


walk2(l_g, c('graf1', 'graf2'),
      ~ ggsave(plot = .x, filename = glue('figures/{.y}.pdf'), device = cairo_pdf))




































