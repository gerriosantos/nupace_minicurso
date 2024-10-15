


rm(list = ls())
gc()

library(tidyverse)
#library(tokenizers)
library(tidytext)
source('functions/00-funcao_limpeza_texto.R')



# Ocupacoes ----
# Varável foi criada usando o código de ocupacoes, mas na base PNADc trimestre4,
# de 2022, já tem essa variável construída (VD4011)


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






## Fazendo as posicoes e a base da pnad 2022 --------------------------------

## LINHAS DE POBREZA

# Definir linhas de pobreza (de 2019) - critério do Banco Mundial
lep <- 151  # US$ 1,90/dia ~ R$151/mes
lp <- 436    # US$ 5,5/dia ~ R$436/mes


# Definir linhas de pobreza (de 2022) - critério do Banco Mundial
# https://blogs.worldbank.org/opendata/september-2022-global-poverty-update-world-bank-2017-ppps-and-new-data-india
# https://documents.worldbank.org/en/publication/documents-reports/documentdetail/099700509122212929/idu05b43a261041c504a5f0bb3405d0ef310b9e1

lep <- 000  # US$ 2,15/dia ~ R$/mes
lp <-  000  # US$ /dia ~ R$/mes





pos <- readxl::read_xlsx('data-raw1/pnad_2022_tri4/variaveis_dic_16_22_tri4.xlsx') |>
  janitor::clean_names() |>
  tidyr::drop_na() |>
  dplyr::mutate(nome = janitor::make_clean_names(nome),
                posicao_inicial = as.numeric(posicao_inicial)) |>

  dplyr::select(posicao = posicao_inicial, tamanho, nome, cod_var = codigo_da_variavel) |>

  dplyr::mutate(begin = posicao - 1, end = (posicao + tamanho) - 1) |>
  dplyr::select(- c(posicao, tamanho)) |>
  dplyr::relocate(col_names = cod_var, .after = end)





df <- read_fwf('data-raw1/pnad_2022_tri4/PNADC_2022_trimestre4.txt', col_positions = pos) |>



  dplyr::mutate(

    id_dom = as.numeric(paste(upa,V1008,V1014, sep = "")),

    regiao = factor(case_when(
      uf %in% 11:17 ~ "Norte",
      uf %in% 21:29 ~ "Nordeste",
      uf %in% 31:35 ~ "Sudeste",
      uf %in% 41:43 ~ "Sul",
      uf %in% 50:53 ~ "Centro Oeste")),

    situacao_dom = factor(if_else(V1022 == 1, 'urbano', 'rural')),

    membro = ifelse(V2005==15 | V2005==16 | V2005==17, 0, 1),

    chefe = ifelse(V2005== "01", 1, 0),

    pessoas_dom = as.numeric(V2001),

    condicao_dom = as.numeric(V2005),

    idade = as.numeric(V2009),

    crianca = ifelse (idade <=14, 1, 0),

    adulto = ifelse (idade > 14, 1, 0),

    # renda trab e outras rendas
    dplyr::across(c(VD4019, VDI4048), ~ as.numeric(.x)),

    renda_trab = as.numeric(VD4019),

    renda = rowSums(across(c(VD4019, VDI4048)), na.rm = TRUE),

    pob_ext = ifelse(renda < lep & !is.na(renda), 'pob_ex', 'no_pob_ex'),

    pob = ifelse(renda < lp & !is.na(renda), 'pob', 'no_pob'),


    sexo = case_match(V2007, c(1) ~ 'homem', c(2) ~ 'mulher'),

    # retira espaçoes entre os numeros.
    #idade = str_replace_all(idade, pattern = "\\b\\s+\\b", replacement = ""),


    cor_raca = case_match(V2010, c(1) ~ 'branca', c(2) ~ 'preta', c(3) ~ 'amarela',
                          c(4) ~ 'parda', c(5) ~ 'indigena', c(9) ~ NA, .default = NA),
    trabalhou = if_else(V4001 == 1, 'sim', 'nao'),

    ocupacao = case_match(V4010,
                          diretores ~ 'diretores', prof_ciencias ~ 'prof_ciencias',

                          prof_nivel_medio ~ 'prof_nivel_medio', trab_apoio_adm ~ 'trab_apoio_adm',

                          trab_servico ~ 'trab_servico', trab_agropecuaria ~ 'trab_agropecuaria',

                          trab_operario_artesao ~ 'trab_operario_artesao', operadores_maq ~ 'operadores_maq',

                          ocup_elementares ~ 'ocup_elementares', forca_armada ~ 'forca_armada',

                          .default = NA

    ),

    bpc = case_match(VI5001A, 1 ~ 'sim', 2 ~ 'nao', 9 ~ NA, .default = NA),

    bolsa_familia = case_match(VI5002A, 1 ~ 'sim', 2 ~ 'nao', 9 ~ NA, .default = NA),

    outros_prog_sociais = case_match(VI5003A, 1 ~ 'sim', 2 ~ 'nao', 9 ~ NA,
                                     .default = NA),

    celular_dom = as.numeric(S01021),

    celular_dom = ifelse(celular_dom <= 2, 'dois_ou_menos', 'mais_de_tres'),

    tv_assin_dom = case_match(S01026, 1 ~ 'sim', 2 ~ 'nao', .default = NA),

    microcomputador_dom = case_match(S01028, 1 ~ 'sim', 2 ~ 'nao', .default = NA),

    internet_dom = case_match(S01029, 1 ~ 'sim', 2 ~ 'nao', .default = NA),

    disp_intelig_dom = case_match(S01029A, 1 ~ 'sim', 2 ~ 'nao', .default = NA),

    stream_dom = case_match(S01029B, 1 ~ 'sim', 2 ~ 'nao', .default = NA),

    banda_larga_dom = case_match(S01030A3, 1 ~ 'sim', 2 ~ 'nao', 3 ~ 'nao', .default = NA),

    #posto_saude = case_match(S090016, 1 ~ 'sim', 2 ~ 'nao', 9 ~ NA, .default = NA),

    anos_estudos = case_match(
      VD3005, c('00') ~ 'sem_instrucao',
      c('01', '02', '03', '04', '05', '06', '07', '08', '09') ~ 'ens_fund',
      c('10', '11', '12', '13') ~ 'ens_medio',
      c('14', '15', '16') ~ 'ens_super', .default = NA),


    dplyr::across(where(is.character), as.factor)

    ) |>
  dplyr::select(-c(V1008:VDI4048)) |>
  dplyr::group_by(id_dom) |>
  dplyr::mutate(
    n_dom = sum(membro, na.rm = T),
    n_criancas = sum(crianca, na.rm = T),
    n_adultos = sum(adulto, na.rm = T),

    renda_trab_dom = ifelse(membro==1, sum(renda_trab, na.rm = TRUE), 0),

    # Construir variavel de renda domiciliar total e rdpc
    renda_dom = ifelse(membro==1, sum(renda, na.rm = TRUE), 0),

    renda_pc = renda_dom/n_dom



  ) |>
  dplyr::ungroup() |>
  dplyr::filter(uf == 23, chefe == 1)



write_csv(df, 'data/pnadc_2022_trim4_CE.csv')
