



# TRIMESTRE -----

rm(list = ls())
gc()

library(tidyverse)
#library(tokenizers)
library(tidytext)
source('functions/00-funcao_limpeza_texto.R')
library(readxl)




# Retirando todas as variáveis ----


d <- read_xls('data-raw/dicionario_PNADC_microdados_trimestre4_20231109.xls',
                      skip = 1) |>
  # select(2, 3, nome = `...5`) |>
  janitor::clean_names() |>
  drop_na(codigo_da_variavel)


tam <- d |> dplyr::pull(tamanho)
names <- d |> dplyr::pull(codigo_da_variavel)


# Funcao par pegar as posicoes da pnad

# Funcao par pegar as posicoes da pnad
pos <- fwf_widths(widths = tam, col_names = names)

df <- read_fwf('data-raw/PNADC_2022_trimestre4.txt', col_positions = pos)


df_1 <- df |>
  janitor::clean_names()









