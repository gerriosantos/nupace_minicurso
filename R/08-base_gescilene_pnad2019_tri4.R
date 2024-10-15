
rm(list = ls())
gc()

library(tidyverse)
#library(tokenizers)
library(tidytext)
source('functions/00-funcao_limpeza_texto.R')



# Ocupacoes ----


u <- 'data-raw1/Estrutura_Ocupacao_COD.xls'

dd <- readxl::read_excel(u, skip = 1) |>
  janitor::clean_names() |>
  dplyr::select(1, 4) |>
  tidyr::fill(grande_grupo, .direction = 'down') |>
  tidyr::drop_na(grupo_de_base) |>
  tidyr::pivot_wider(names_from = grande_grupo, values_from = grupo_de_base)


nomes <- readxl::read_excel(u, skip = 1) |>
  janitor::clean_names() |>
  dplyr::select(1, 5) |>
  tidyr::drop_na(grande_grupo)



diretores <- unlist(dd$`1`)

prof_ciencias = unlist(dd$`2`)

prof_nivel_medio <- unlist(dd$`3`)

trab_apoio_adm = unlist(dd$`4`)


trab_servico <- unlist(dd$`5`)

trab_agropecuaria = unlist(dd$`6`)

trab_operario_artesao <- unlist(dd$`7`)

operadores_maq = unlist(dd$`8`)

ocup_elementares = unlist(dd$`9`)

forca_armada <- unlist(dd$`0`)



# 2019 - Trimestral PNADC_012019 ----------------------------------------------

# Link no filezilla
# /Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/Trimestre_4/Documentacao


# Retirar Pnadc Completa ---------

d <- readxl::read_xls('data-raw1/dicionario_PNADC_microdados_trimestre4_20231109.xls',
                      skip = 1) |>
  janitor::clean_names() |>
  dplyr::select(1, 2, 3, nome = `x5`) |>
  dplyr::mutate(nome = coalesce(nome, codigo_da_variavel),
                nome = formata_nome(nome)) |>
  tidyr::drop_na() |>
  tidytext::unnest_tokens(nome_var, nome, token = 'ngrams', n = 4, drop = F) |>
  dplyr::mutate(nome_var = janitor::make_clean_names(coalesce(nome_var, nome)),

                nome_var = ifelse(stringr::str_starts(nome_var, 'peso'), NA, nome_var),
                nome_var = coalesce(nome_var, codigo_da_variavel)

  ) |>
  dplyr::distinct(codigo_da_variavel, .keep_all = TRUE) |>
  dplyr::select(-nome)


writexl::write_xlsx(d, 'variaveis_dic_16_22_tri4.xlsx')


# tam <- d |> pull(tamanho)
# names <- d |> pull(codigo_da_variavel)
# pos <- fwf_widths(widths = tam, col_names = names)



pos <- readxl::read_xlsx('data-raw1/VARIAVEIS.xlsx', skip = 1) |>
  dplyr::select(nome = 1, 2, 3, 4) |>
  janitor::clean_names() |>
  tidyr::drop_na() |>
  dplyr::mutate(nome = janitor::make_clean_names(nome)) |>

  dplyr::select(posicao = posicao_inicial, tamanho, nome, cod_var = codigo_da_variavel) |>

  mutate(begin = posicao - 1, end = (posicao + tamanho) - 1) |>
  select(- c(posicao, tamanho)) |>
  dplyr::relocate(col_names = nome, .after = end)





df <- read_fwf('data-raw1/PNADC_2019_trimestre4.txt', col_positions = pos) |>
  dplyr::mutate(
    local = if_else(area_urbano_x_rural == 1, 'urbano', 'rural'),

    pessoas_dom = as.numeric(numero_de_pessoas_no_domicilio),
    sexo = case_match(genero, c(1) ~ 'masculino', c(2) ~ 'feminino'),
    cor_raca = case_match(raca, c(1) ~ 'Branca', c(2) ~ 'Preta', c(3) ~ 'Amarela',
                          c(4) ~ 'Parda', c(1) ~ 'Indigena'),
    trabalhou = if_else(mercado_de_trabalho_estar_empregado == 1, 'sim', 'nao'),

    ocupacao = case_match(ramo_de_atividade,
      diretores ~ 'diretores', prof_ciencias ~ 'prof_ciencias',

      prof_nivel_medio ~ 'prof_nivel_medio', trab_apoio_adm ~ 'trab_apoio_adm',

      trab_servico ~ 'trab_servico', trab_agropecuaria ~ 'trab_agropecuaria',

      trab_operario_artesao ~ 'trab_operario_artesao', operadores_maq ~ 'operadores_maq',

      ocup_elementares ~ 'ocup_elementares', forca_armada ~ 'forca_armada',

      .default = NA

    ),
    # retira espaçoes entre os numeros.
    idade = str_replace_all(idade, pattern = "\\b\\s+\\b", replacement = ""),
    idade = as.numeric(idade),

    pobreza = as.numeric(pobreza_ser_pobre),

    bpc = case_match(bpc, 1 ~ 'sim', 2 ~ 'nao', .default = NA),

    bolsa_familia = case_match(bolsa_familia, 1 ~ 'sim', 2 ~ 'nao', .default = NA),


    outros_prog_sociais = case_match(outros_programas_sociais, 1 ~ 'sim', 2 ~ 'nao',
                                     .default = NA),

    anos_estudos = case_match(
      anos_de_estudos, c('00') ~ 'sem_instrucao',
      c('01', '02', '03', '04', '05', '06', '07', '08', '09') ~ 'ens_fund',
      c('10', '11', '12', '13') ~ 'ens_medio',
      c('14', '15', '16') ~ 'ens_super', .default = NA)

    ) |>

  dplyr::select(ano, trimestre, uf = estado, local, pessoas_dom,
                sexo, cor_raca, trabalhou, ocupacao, idade, pobreza, bpc,
                bolsa_familia, outros_prog_sociais, anos_estudos)


write_csv(df, 'data/pnadc_2019_trim4.csv')


df1 <- df |> drop_na()

write_csv(df1, 'data/pnadc_2019_trim4_semNAs.csv')






