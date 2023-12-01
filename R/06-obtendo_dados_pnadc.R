

###############################################################################

## OBTENÇÃO DE DADOS DA PNAD CONTÍNUA

###############################################################################
rm(list=ls(all=TRUE))#Libera memória, apagando os objetos abertos no R.
gc() #Libera a memoria RAM que não esta sendo utilizada pelo computador:
#options(scipen=999) #Remove notação científica.


# setwd("diretório")
# getwd()

# Carregar pacotes
library(PNADcIBGE)
library(survey)
library(srvyr)
library(tidyverse)

###############################################################################

####
# Um vetor com o código de variáveis desejadas
variaveis <- c("Ano", "Trimestre", "UF", "Capital",
               "RM_RIDE", "UPA", "V1008", "V1014",
               "V1022", "V1023", "V1030", "V1032",
               "V2001", "V2005", "V2007", "V2009",
               "V2010", "VD2003", "VD4019", "VD4048",
               "VD5007", "VD5008", "V5004A",
               "V5004A2", "V5005A", "V5005A2",
               "V5006A", "V5006A2", "V5007A",
               "V5007A2", "V5008A", "S01001",
               "S01002", "S01003", "S01004",
               "S01005", "S01006", "S01007", "S01010",
               "S01011A", "S01011C", "S01012A",
               "S01017", "S01018", "S01019",
               "S01021", "S01022", "S01023",
               "S01024", "S01025", "S01028", "S01029",
               "S010311", "S010312", "VD2003", "VD2004", "VD2002",
               "VD3004", "VD3005", "VD3006", "VD4001", "VD4002", "VD4003",
               "VD4004A", "VD4005", "VD4008", "VD4009",
               "VD4010", "VD4011", "VD4012", "VD4013",
               "VD4014", "VD4015", "VD4016", "VD4017",
               "VD4018", "VD4019", "VD4020", "VD4022",
               "VD4030", "VD4031", "VD4035", "VD4036", "VD4037",
               "VD4046", "VD4047", "VD4048", "VD4052",
               "VD5001", "VD5002", "VD5003", "VD5004",
               "VD5005", "VD5006", "VD5007", "VD5008",
               "VD5009", "VD5010", "VD5011")

####
# Download dos dado da PNAD usando o pacote PNADcIBGE

pnadc19 <- get_pnadc(year = 2019,
                    interview = 1,
                    vars = variaveis,
                    design = FALSE,
                    labels = FALSE,
                    deflator = TRUE,
                    defyear = 2019)

# Salvar
saveRDS(pnadc19, file="pnadc19")

# Carregar dados
# pnadc19 <- readRDS(file="pnadc19")

# Verificar tipo de objeto
#class(pnadc)

# Primeira visão das variáveis
#glimpse(pnadc)

# Transformar categóricas 'string' em 'fator'
pnadc19 <- pnadc19 %>% mutate_if(is.character,as.factor)


###--------------------------------------------------------------------------###
# Criar ou recodificar variaveis em nível individual
pnadc19 <- pnadc19 %>%
    mutate(
        id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
        membro = ifelse(VD2002==15 | VD2002==16 | VD2002==17, 0,1),
        chefe = ifelse (VD2002== "01", 1, 0),

        ### Dimensões geograficas

        regiao = factor(case_when(
            UF %in% 11:17 ~ "Norte",
            UF %in% 21:29 ~ "Nordeste",
            UF %in% 31:35 ~ "Sudeste",
            UF %in% 41:43 ~ "Sul",
            UF %in% 50:53 ~ "Centro Oeste")),

        area = factor(case_when(V1022==1 ~ "urbano",
                                V1022==2 ~ "rural")),

        ### Características individuais

        idade = V2009,

        crianca = ifelse (V2009 <=14, 1, 0),

        adulto = ifelse (V2009 >= 14, 1, 0),

        genero = factor(case_when(V2007==1 ~ "homem",
                                  V2007==2 ~ "mulher")),

        raca = factor(case_when(V2010==1 | V2010==3 ~ "branco",
                                V2010==2 | V2010==4 | V2010==5 ~ "preto ou pardo")),

        atividade = factor(case_when(VD4001==1 & VD4002==1 ~ "ativo ocupado",
                                     VD4001==1 & VD4002==2 ~ "ativo desocupado",
                                     VD4001==2 ~ "inativo")),

        escolaridade = factor(case_when(VD3004==1 ~ "sem instrucao",
                                        VD3004==2 ~ "fundam incompleto",
                                        VD3004==3 ~ "fundam completo",
                                        VD3004==4 ~ "medio incompleto",
                                        VD3004==5 ~ "medio completo",
                                        VD3004==6 ~ "superior incompleto",
                                        VD3004==7 ~ "superior completo")),


        ### Contruir variaveis de renda
        # renda trabalho = VD4019
        rend_trab = VD4019*CO2,
        # renda outras fontes = VD4048
        rend_out = VD4048*CO2e,
        renda = rowSums(across(c(rend_trab, rend_out)), na.rm = TRUE)
    )

# Criar ou recodificar variaveis agregadas por domicilios
pnadc19 <- pnadc19 %>%
    group_by(id_dom) %>%
    mutate(n_dom = sum(membro),
           n_criancas = sum(crianca),
           n_adultos = sum(adulto),

           rend_trab_dom = ifelse(membro==1, sum(rend_trab, na.rm = TRUE), 0),

           # Construir variavel de renda domiciliar total e rdpc
           renda_dom = ifelse(membro==1, sum(renda, na.rm = TRUE), 0),
           rdpc = renda_dom/n_dom
    ) %>%
    ungroup()


# Criar variaveis de caracteristicas dos domicilios
pnadc19 <- pnadc19 %>%
    mutate(
        agua_adeq = factor(ifelse((S01007==1 | S01007==2) & S01010==1, "adequada", "inadequada")),

        esgoto_ad = factor(case_when(S01012A==1 & V1022==1 ~ "adequado",
                                 S01012A==2 & V1022==1 ~ "adequado",
                                 S01012A>=3 & V1022==1 ~ "inadequado",
                                 S01012A<=3 & V1022==2 ~ "adequado",
                                 S01012A>=4 & V1022==2 ~ "inadequado")),

        banheiro = factor(ifelse(S01011A>=1, "sim", "nao")),

        num_comodos = S01005,

        densidade = n_dom/ num_comodos,

        geladeira = factor(ifelse(S01023==1, "sim", "nao")),

        maquina_lavar = factor(ifelse(S01024==1, "sim", "nao")),

        computador = factor(ifelse(S01028==1, "sim", "nao")),

        internet = factor(ifelse(S01029==1, "sim", "nao")),

        carro = factor(ifelse(S010311==1, "sim", "nao")),

        moto = factor(ifelse(S010312==1, "sim", "nao"))

        )


##--------------------
## LINHAS DE POBREZA

# Definir linhas de pobreza (de 2019) - critério do Banco Mundial
lep <- 151  # US$ 1,90/dia ~ R$151/mes
lp <- 36    # US$ 5,5/dia ~ R$436/mes

# Criar variáveis dummy para contar pessoas na pobreza
pnadc19 <- pnadc19 %>%
    mutate(
        pobre = factor(ifelse(rdpc< lp & !is.na(rdpc), "pobre", "nao pobre")),
        pobre_ex = factor(ifelse(rdpc< lep & !is.na(rdpc), "pobrex", "nao.pobrex"))
    )


## FILTRO PARA O CE
pov19ce <- pnadc19 %>%
    filter(UF==23 & chefe==1) %>%
    select()


## SALVAR DADOS EM .RDS
#saveRDS(povce, file="pov19ce")

## SALVAR DADOS EM .csv
write.csv(pov19ce,'pov19ce.csv')


###--------------------------------------------------------------------------###
###--------------------------------------------------------------------------###
###--------------------------------------------------------------------------###









