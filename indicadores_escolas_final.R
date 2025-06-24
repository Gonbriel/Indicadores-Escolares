#-----------------IMPORTANDO BASE DE DADOS----------
# ATIVANDO PACOTE
library(readr)
library(dplyr)
library(tidyr)

#DESCOBRINDO O ENCODING
guess_encoding("microdados_ed_basica_2024.csv")

#Importando dados com enconding apropriado
base = read_csv2(file = "microdados_ed_basica_2024.csv",
                 locale=locale(encoding = "ISO-8859-1"))

#Selecionando váriaveis específicas da base para criação dos indicadores
base_ind <- base |>
  dplyr::filter(SG_UF == "RJ") |>
  dplyr::select(NO_MUNICIPIO,TP_DEPENDENCIA, IN_QUADRA_ESPORTES, IN_PARQUE_INFANTIL,IN_ACESSO_INTERNET_COMPUTADOR, IN_BIBLIOTECA_SALA_LEITURA,
                IN_LABORATORIO_CIENCIAS, IN_LABORATORIO_INFORMATICA,IN_MATERIAL_PED_BIL_SURDOS, IN_AGUA_POTAVEL, IN_PROF_SAUDE,
                IN_PROF_NUTRICIONISTA, IN_PROF_PSICOLOGO)

#TRATAMENTO DE VARIAVEIS QUALITATIVAS
base_ind <- base_ind |>
  #recodificando a variavel IN_ACESSO_INTERNET_COMPUTADOR
  mutate(IN_ACESSO_INTERNET_COMPUTADOR = case_when(
    IN_ACESSO_INTERNET_COMPUTADOR == 0 ~ "Não",
    IN_ACESSO_INTERNET_COMPUTADOR == 1 ~ "Sim"
  )) |>
  #recodificando  a variável TP_DEPENDENCIA
  dplyr::mutate(TP_DEPENDENCIA = case_when(
    TP_DEPENDENCIA == 1 ~"Federal",
    TP_DEPENDENCIA == 2 ~"Estadual",
    TP_DEPENDENCIA == 3 ~"Municipal",
    TP_DEPENDENCIA == 4 ~"Privada"
  )) |>
  #recodificando a variavel IN_QUADRA_ESPORTES
  mutate(IN_QUADRA_ESPORTES = case_when(
    IN_QUADRA_ESPORTES == 0 ~ "Não",
    IN_QUADRA_ESPORTES == 1 ~ "Sim"
  ))  |>
  #recodificando a variavel  IN_PARQUE_INFANTIL
  mutate( IN_PARQUE_INFANTIL = case_when(
    IN_PARQUE_INFANTIL == 0 ~ "Não",
    IN_PARQUE_INFANTIL == 1 ~ "Sim"
  ))  |>
  #recodificando a variavel  IN_BIBLIOTECA_SALA_LEITURA
  mutate( IN_BIBLIOTECA_SALA_LEITURA = case_when(
    IN_BIBLIOTECA_SALA_LEITURA == 0 ~ "Não",
    IN_BIBLIOTECA_SALA_LEITURA == 1 ~ "Sim"
  ))  |>
  #recodificando a variavel  IN_LABORATORIO_CIENCIAS
  mutate( IN_LABORATORIO_CIENCIAS= case_when(
    IN_LABORATORIO_CIENCIAS == 0 ~ "Não",
    IN_LABORATORIO_CIENCIAS== 1 ~ "Sim"
  ))  |>
  #recodificando a variavel IN_LABORATORIO_INFORMATICA
  mutate( IN_LABORATORIO_INFORMATICA= case_when(
    IN_LABORATORIO_INFORMATICA == 0 ~ "Não",
    IN_LABORATORIO_INFORMATICA== 1 ~ "Sim"
  ))  |>
  #recodificando a variavel IN_MATERIAL_PED_BIL_SURDOS
  mutate(IN_MATERIAL_PED_BIL_SURDOS= case_when(
    IN_MATERIAL_PED_BIL_SURDOS == 0 ~ "Não",
    IN_MATERIAL_PED_BIL_SURDOS== 1 ~ "Sim"
  ))  |>
  #recodificando a variavel IN_AGUA_POTAVEL
  mutate(IN_AGUA_POTAVEL= case_when(
    IN_AGUA_POTAVEL == 0 ~ "Não",
    IN_AGUA_POTAVEL== 1 ~ "Sim"
  ))  |>
  #recodificando a variavel IN_PROF_SAUDE
  mutate(IN_PROF_SAUDE= case_when(
    IN_PROF_SAUDE == 0 ~ "Não",
    IN_PROF_SAUDE== 1 ~ "Sim"
  ))  |>
  #recodificando a variavel IN_PROF_NUTRICIONISTA
  mutate(IN_PROF_NUTRICIONISTA= case_when(
    IN_PROF_NUTRICIONISTA == 0 ~ "Não",
    IN_PROF_NUTRICIONISTA == 1 ~ "Sim"
  ))  |>
  #recodificando a variavel IN_PROF_PSICOLOGO
  mutate(IN_PROF_PSICOLOGO= case_when(
    IN_PROF_PSICOLOGO == 0 ~ "Não",
    IN_PROF_PSICOLOGO == 1 ~ "Sim"
  ))

base_ind <- base_ind |>
  mutate(
    TIPO_DEPENDENCIA = case_when(
      TP_DEPENDENCIA %in% c("Federal", "Estadual", "Municipal") ~ "Publica",
      TP_DEPENDENCIA == "Privada" ~ "Privada",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(QUADRA_OU_PARQUE = IN_QUADRA_ESPORTES == "Sim" | IN_PARQUE_INFANTIL == "Sim") |>
  mutate(QUADRA_OU_PARQUE = case_when(
    QUADRA_OU_PARQUE == TRUE ~ 'Sim',
    QUADRA_OU_PARQUE == FALSE ~ 'Não'
  )) |>
  mutate(CIENCIA_OU_INFO = IN_LABORATORIO_INFORMATICA == 'Sim' | IN_LABORATORIO_CIENCIAS == 'Sim') |>
  mutate(CIENCIA_OU_INFO = case_when(
    CIENCIA_OU_INFO == TRUE ~ 'Sim',
    CIENCIA_OU_INFO == FALSE ~ 'Não'
  )) |>
  mutate(PSICO_OU_NUTRI = IN_PROF_NUTRICIONISTA == 'Sim' | IN_PROF_PSICOLOGO == 'Sim') |>
  mutate(PSICO_OU_NUTRI = case_when(
    PSICO_OU_NUTRI == TRUE ~ 'Sim',
    PSICO_OU_NUTRI == FALSE ~ 'Não'
  ))

#---------------------Criação dos Indicadores-----------------------------------
## Função que auxilia  a criação de alguns indicadores----
calcular_proporcao <- function(base, var, dependencia, nome_prop){
  base |>
    filter(TIPO_DEPENDENCIA == dependencia) |>
    filter({{var}} == 'Sim') |>
    group_by(NO_MUNICIPIO) |>
    summarise(quant = n(), .groups = 'drop') |>
    left_join(base_escolas, by = 'NO_MUNICIPIO') |>
    mutate(!!nome_prop := round(quant / .data[[dependencia]], 2))
}

## Criando a base group----
base_group <- base_ind |>
  group_by(NO_MUNICIPIO, TIPO_DEPENDENCIA) |>
  summarise(
    n = n(),
    .groups = 'drop'
  )

# Transformando para o formato wide as obserações de escolas por município
base_escolas <- base_group |>
  tidyr::pivot_wider(
    names_from = 'TIPO_DEPENDENCIA',
    values_from = n,
    values_fill = 0
  )

# Criando uma variável pública (Estadual, Federal e Municipal) e total
base_escolas <- base_escolas |>
  mutate(Total = Privada+Publica)

# -------------------------------Convênios--------------------------------------
## CPP----
CPP <- base_escolas |>
  mutate(CPP = round(Publica/Total, 2))

## EMP----
EMP <- base_escolas |>
  mutate(EMP = round(Privada/Total, 2))

#-----------PEQ (Possui quadra de esportes ou parque infantil-------------------
## PEQ_PUB----
peq_pub <- calcular_proporcao(
  base = base_ind,
  var = QUADRA_OU_PARQUE,
  dependencia = 'Publica',
  nome_prop = 'PEQ_PUB'
)

## PEQ_PRIV----
peq_priv <- calcular_proporcao(
  base = base_ind,
  var = QUADRA_OU_PARQUE,
  dependencia = 'Privada',
  nome_prop = 'PEQ_PRIV'
)


#--------------------CAI (Computadores com acesso à internet)-------------------
## CAI_PUB----
cai_pub <- calcular_proporcao(
  base = base_ind,
  var = IN_ACESSO_INTERNET_COMPUTADOR,
  dependencia = 'Publica',
  nome_prop = 'CAI_PUB'
)

## CAI_PRIV----
cai_priv <- calcular_proporcao(
  base = base_ind,
  var = IN_ACESSO_INTERNET_COMPUTADOR,
  dependencia = 'Privada',
  nome_prop = 'CAI_PRIV'
)

#--------------------BS (Bibliotecas ou Salas de Leitura)-----------------------
## BS_PUB----
bs_pub <- calcular_proporcao(
  base = base_ind,
  var = IN_BIBLIOTECA_SALA_LEITURA,
  dependencia = 'Publica',
  nome_prop = 'BS_PUB'
)

## BS_PRIV----
bs_priv <- calcular_proporcao(
  base = base_ind,
  var = IN_BIBLIOTECA_SALA_LEITURA,
  dependencia = 'Privada',
  nome_prop = 'BS_PRIV'
)

#-----------------PEL (Proporção de escolas que possuem Laboratórios)-----------
## PEL_PUB----
pel_pub <- calcular_proporcao(
  base = base_ind,
  var = CIENCIA_OU_INFO,
  dependencia = 'Publica',
  nome_prop = 'PEL_PUB'
)

## PEL_PRIV----
pel_priv <- calcular_proporcao(
  base = base_ind,
  var = CIENCIA_OU_INFO,
  dependencia = 'Privada',
  nome_prop = 'PEL_PRIV'
)

#PMS (Proporção de escolas que possuem materiais de educação para surdos)-------
## PMS_PUB----
pms_pubs <- calcular_proporcao(
  base = base_ind,
  var = IN_MATERIAL_PED_BIL_SURDOS,
  dependencia = 'Publica',
  nome_prop = 'PMS_PUB'
)

## PMS_PRIV----
pms_priv <- calcular_proporcao(
  base = base_ind,
  var = IN_MATERIAL_PED_BIL_SURDOS,
  dependencia = 'Privada',
  nome_prop = 'PMS_PRIV'
)

#-------PAP (Proporção de escolas que possuem água potável)---------------------
## PAP_PUB----
pap_pub <- calcular_proporcao(
  base = base_ind,
  var = IN_AGUA_POTAVEL,
  dependencia = 'Publica',
  nome_prop = 'PAP_PUB'
)

## PAP_PRIV----
pap_priv <- calcular_proporcao(
  base = base_ind,
  var = IN_AGUA_POTAVEL,
  dependencia = 'Privada',
  nome_prop = 'PAP_PRIV'
)

#--------PFS (Proporção de escolas que possuem profissionais da saúde)----------
## PFS_PUB----
pfs_pub <- calcular_proporcao(
  base = base_ind,
  var = IN_PROF_SAUDE,
  dependencia = 'Publica',
  nome_prop = 'PFS_PUB'
)

## PFS_PRIV----
pfs_priv <- calcular_proporcao(
  base = base_ind,
  var = IN_PROF_SAUDE,
  dependencia = 'Privada',
  nome_prop = 'PFS_PRIV'
)

#----PNP (Proporção de escolas que possuem nutricionistas ou psicólogos)--------
## PNP_PUB----
pnp_pub <- calcular_proporcao(
  base = base_ind,
  var = PSICO_OU_NUTRI,
  dependencia = 'Publica',
  nome_prop = 'PNP_PUB'
)

## PNP_PRIV----
pnp_priv <- calcular_proporcao(
  base = base_ind,
  var = PSICO_OU_NUTRI,
  dependencia = 'Privada',
  nome_prop = 'PNP_PRIV'
)

#------------------------------Tabela Final-------------------------------------
## Tratando das bases antes de juntar à uma base final----
base_escolas <- base_escolas |>
  select(NO_MUNICIPIO, Privada, Publica)

CPP <- CPP |>
  select(NO_MUNICIPIO, CPP)

EMP <- EMP |>
  select(NO_MUNICIPIO, EMP)

peq_pub <- peq_pub |>
  select(NO_MUNICIPIO, PEQ_PUB)

peq_priv <- peq_priv |>
  select(NO_MUNICIPIO, PEQ_PRIV)

cai_pub <- cai_pub |>
  select(NO_MUNICIPIO, CAI_PUB)

cai_priv <- cai_priv |>
  select(NO_MUNICIPIO, CAI_PRIV)

bs_pub <- bs_pub |>
  select(NO_MUNICIPIO, BS_PUB)

bs_priv <- bs_priv |>
  select(NO_MUNICIPIO, BS_PRIV)

pel_pub <- pel_pub |>
  select(NO_MUNICIPIO, PEL_PUB)

pel_priv <- pel_priv |>
  select(NO_MUNICIPIO, PEL_PRIV)

pms_pubs <- pms_pubs |>
  select(NO_MUNICIPIO, PMS_PUB)

pms_priv <- pms_priv |>
  select(NO_MUNICIPIO, PMS_PRIV)

pap_pub <- pap_pub |>
  select(NO_MUNICIPIO, PAP_PUB)

pap_priv <- pap_priv |>
  select(NO_MUNICIPIO, PAP_PRIV)

pfs_pub <- pfs_pub |>
  select(NO_MUNICIPIO, PFS_PUB)

pfs_priv <- pfs_priv |>
  select(NO_MUNICIPIO, PFS_PRIV)

pnp_pub <- pnp_pub |>
  select(NO_MUNICIPIO, PNP_PUB)

pnp_priv <- pnp_priv |>
  select(NO_MUNICIPIO, PNP_PRIV)

## Fazendo join e criando a base final----

base_final <- base_escolas |>
  left_join(CPP, by = 'NO_MUNICIPIO') |>
  left_join(EMP, by = 'NO_MUNICIPIO') |>
  left_join(peq_pub, by = 'NO_MUNICIPIO') |>
  left_join(peq_priv, by = 'NO_MUNICIPIO') |>
  left_join(cai_pub, by = 'NO_MUNICIPIO') |>
  left_join(cai_priv, by = 'NO_MUNICIPIO') |>
  left_join(bs_pub, by = 'NO_MUNICIPIO') |>
  left_join(bs_priv, by = 'NO_MUNICIPIO') |>
  left_join(pel_pub, by = 'NO_MUNICIPIO') |>
  left_join(pel_priv, by = 'NO_MUNICIPIO') |>
  left_join(pms_pubs, by = 'NO_MUNICIPIO') |>
  left_join(pms_priv, by = 'NO_MUNICIPIO') |>
  left_join(pap_pub, by = 'NO_MUNICIPIO') |>
  left_join(pap_priv, by = 'NO_MUNICIPIO') |>
  left_join(pfs_pub, by = 'NO_MUNICIPIO') |>
  left_join(pfs_priv, by = 'NO_MUNICIPIO') |>
  left_join(pnp_pub, by = 'NO_MUNICIPIO') |>
  left_join(pnp_priv, by = 'NO_MUNICIPIO')

## Corrigindo o nome de CPP e EMP----
base_final <- base_final |>
  dplyr::rename(EPUB = CPP) |>
  dplyr::rename(EPRIV = EMP)
