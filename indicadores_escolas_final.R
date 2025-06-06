#-----------------IMPORTANDO BASE DE DADOS----------
# ATIVANDO PACOTE
library(readr)
library(dplyr)
library(tidyr)
#IMPORTANDO OS DADOS DO CENSO ESCOLAR DA EDUCAÇÃO BÁSICA 2024
base = read_csv2(file="microdados_ed_basica_2024.csv")

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

#---------------------Criação dos Indicadores-----------------------------------

## Criando a base group----
base_group <- base_ind |>
  group_by(NO_MUNICIPIO, TP_DEPENDENCIA) |>
  summarise(
    n = n(),
    .groups = 'drop'
  )

# Transformando para o formato wide as obserações de escolas por município
base_escolas <- base_group |> 
  tidyr::pivot_wider(
    names_from = 'TP_DEPENDENCIA',
    values_from = n,
    values_fill = 0
  )

# Criando uma variável pública (Estadual, Federal e Municipal) e total
base_escolas <- base_escolas |>
  mutate(Publica = Estadual+Federal+Municipal, 
         Total = Estadual+Federal+Municipal+Privada)

# -------------------------------Convênios--------------------------------------
## CPP----
CPP <- base_escolas |>
  mutate(CPP = round(Publica/Total, 2))

## EMP----
EMP <- base_escolas |>
  mutate(EMP = round(Privada/Total, 2))

#-----------PEQ (Possui quadra de esportes ou parque infantil-------------------
## PEQ_PUB----
peq_pub <- base_ind |>
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_QUADRA_ESPORTES, IN_PARQUE_INFANTIL) |>
  filter(TP_DEPENDENCIA %in% c('Estadual', 'Municipal', 'Federal')) |>
  mutate(QUADRA_OU_PARQUE = IN_QUADRA_ESPORTES == "Sim" | IN_PARQUE_INFANTIL == "Sim") |>
  filter(QUADRA_OU_PARQUE == TRUE) |> 
  group_by(NO_MUNICIPIO, QUADRA_OU_PARQUE) |> 
  summarise(
    quant = n(),
    .groups = 'drop'
  ) |> 
  left_join(base_escolas, by = "NO_MUNICIPIO") |> 
  mutate(PEQ_PUB = round(c(quant / Publica), 2))

## PEQ_PRIV----
peq_priv <- base_ind |>
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_QUADRA_ESPORTES, IN_PARQUE_INFANTIL) |>
  filter(TP_DEPENDENCIA == 'Privada') |>
  mutate(QUADRA_OU_PARQUE = IN_QUADRA_ESPORTES == "Sim" | IN_PARQUE_INFANTIL == "Sim") |>
  filter(QUADRA_OU_PARQUE == TRUE) |> 
  group_by(NO_MUNICIPIO, QUADRA_OU_PARQUE) |> 
  summarise(
    quant = n(),
    .groups = 'drop'
  ) |> 
  left_join(base_escolas, by = "NO_MUNICIPIO") |> 
  mutate(PEQ_PRIV = round(c(quant / Privada), 2))


#--------------------CAI (Computadores com acesso à internet)-------------------
## CAI_PUB----
cai_pub <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_ACESSO_INTERNET_COMPUTADOR) |> 
  filter(TP_DEPENDENCIA %in% c('Estadual', 'Municipal', 'Federal')) |> 
  group_by(NO_MUNICIPIO,IN_ACESSO_INTERNET_COMPUTADOR) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_ACESSO_INTERNET_COMPUTADOR == 'Sim') |> 
  left_join(base_escolas, by = "NO_MUNICIPIO") |> 
  mutate(CAI_PUB = round(c(quant / Publica), 2))

## CAI_PRIV----
cai_priv <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_ACESSO_INTERNET_COMPUTADOR) |> 
  filter(TP_DEPENDENCIA == 'Privada') |> 
  group_by(NO_MUNICIPIO,IN_ACESSO_INTERNET_COMPUTADOR) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_ACESSO_INTERNET_COMPUTADOR == 'Sim') |> 
  left_join(base_escolas, by = "NO_MUNICIPIO") |> 
  mutate(CAI_PRIV = round(c(quant / Privada), 2))

#--------------------BS (Bibliotecas ou Salas de Leitura)-----------------------
## BS_PUB----
bs_pub <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_BIBLIOTECA_SALA_LEITURA) |> 
  filter(TP_DEPENDENCIA %in% c('Estadual', 'Municipal', 'Federal')) |> 
  group_by(NO_MUNICIPIO, IN_BIBLIOTECA_SALA_LEITURA) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_BIBLIOTECA_SALA_LEITURA == 'Sim') |> 
  left_join(base_escolas, by = "NO_MUNICIPIO") |> 
  mutate(BS_PUB = round(c(quant / Publica), 2))

## BS_PRIV----
bs_priv <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_BIBLIOTECA_SALA_LEITURA) |> 
  filter(TP_DEPENDENCIA == 'Privada') |> 
  group_by(NO_MUNICIPIO, IN_BIBLIOTECA_SALA_LEITURA) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_BIBLIOTECA_SALA_LEITURA == 'Sim') |> 
  left_join(base_escolas, by = "NO_MUNICIPIO") |> 
  mutate(BS_PRIV = round(c(quant / Privada), 2))

#-----------------PEL (Proporção de escolas que possuem Laboratórios)-----------
## PEL_PUB----
pel_pub <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_LABORATORIO_CIENCIAS,
         IN_LABORATORIO_INFORMATICA) |> 
  filter(TP_DEPENDENCIA %in% c('Estadual', 'Municipal', 'Federal')) |> 
  mutate(CIENCIA_OU_INFO = IN_LABORATORIO_INFORMATICA == 'Sim' | IN_LABORATORIO_CIENCIAS == 'Sim') |> 
  group_by(NO_MUNICIPIO, CIENCIA_OU_INFO) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(CIENCIA_OU_INFO == 'TRUE') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |> 
  mutate(PEL_PUB = round(c(quant / Publica), 2))

## PEL_PRIV----
pel_priv <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_LABORATORIO_CIENCIAS,
         IN_LABORATORIO_INFORMATICA) |> 
  filter(TP_DEPENDENCIA == 'Privada') |> 
  mutate(CIENCIA_OU_INFO = IN_LABORATORIO_INFORMATICA == 'Sim' | IN_LABORATORIO_CIENCIAS == 'Sim') |> 
  group_by(NO_MUNICIPIO, CIENCIA_OU_INFO) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(CIENCIA_OU_INFO == 'TRUE') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |> 
  mutate(PEL_PRIV = round(c(quant / Privada), 2))

#PMS (Proporção de escolas que possuem materiais de educação para surdos)-------
## PMS_PUB----
pms_pubs <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_MATERIAL_PED_BIL_SURDOS) |> 
  filter(TP_DEPENDENCIA %in% c('Estadual', 'Municipal', 'Federal')) |> 
  group_by(NO_MUNICIPIO, IN_MATERIAL_PED_BIL_SURDOS) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_MATERIAL_PED_BIL_SURDOS == 'Sim') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |> 
  mutate(PMS_PUB = round(c(quant / Publica), 2))

## PMS_PRIV----
pms_priv <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_MATERIAL_PED_BIL_SURDOS) |> 
  filter(TP_DEPENDENCIA == 'Privada') |> 
  group_by(NO_MUNICIPIO, IN_MATERIAL_PED_BIL_SURDOS) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_MATERIAL_PED_BIL_SURDOS == 'Sim') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |> 
  mutate(PMS_PRIV = round(c(quant / Privada), 2))

#-------PAP (Proporção de escolas que possuem água potável)---------------------
## PAP_PUB----
pap_pub <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_AGUA_POTAVEL) |> 
  filter(TP_DEPENDENCIA %in% c('Estadual', 'Municipal', 'Federal')) |> 
  group_by(NO_MUNICIPIO, IN_AGUA_POTAVEL) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_AGUA_POTAVEL == 'Sim') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |>
  mutate(PAP_PUB = round(c(quant / Publica), 2))

## PAP_PRIV----
pap_priv <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_AGUA_POTAVEL) |> 
  filter(TP_DEPENDENCIA == 'Privada') |> 
  group_by(NO_MUNICIPIO, IN_AGUA_POTAVEL) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_AGUA_POTAVEL == 'Sim') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |>
  mutate(PAP_PRIV = round(c(quant / Privada), 2))

#--------PFS (Proporção de escolas que possuem profissionais da saúde)----------
## PFS_PUB----
pfs_pub <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_PROF_SAUDE) |> 
  filter(TP_DEPENDENCIA %in% c('Estadual', 'Municipal', 'Federal')) |> 
  group_by(NO_MUNICIPIO, IN_PROF_SAUDE) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_PROF_SAUDE == 'Sim') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |>
  mutate(PFS_PUB = round(c(quant / Publica), 2))

## PFS_PRIV----
pfs_priv <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_PROF_SAUDE) |> 
  filter(TP_DEPENDENCIA == 'Privada') |> 
  group_by(NO_MUNICIPIO, IN_PROF_SAUDE) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(IN_PROF_SAUDE == 'Sim') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |>
  mutate(PFS_PRIV = round(c(quant / Privada), 2))

#----PNP (Proporção de escolas que possuem nutricionistas ou psicólogos)--------
## PNP_PUB----
pnp_pub <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_PROF_NUTRICIONISTA, IN_PROF_PSICOLOGO) |> 
  mutate(PSICO_OU_NUTRI = IN_PROF_NUTRICIONISTA == 'Sim' | IN_PROF_PSICOLOGO == 'Sim') |>
  filter(TP_DEPENDENCIA %in% c('Estadual', 'Municipal', 'Federal')) |> 
  group_by(NO_MUNICIPIO, PSICO_OU_NUTRI) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(PSICO_OU_NUTRI == 'TRUE') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |>
  mutate(PNP_PUB = round(c(quant / Publica), 2))

## PNP_PRIV----
pnp_priv <- base_ind |> 
  select(NO_MUNICIPIO, TP_DEPENDENCIA, IN_PROF_NUTRICIONISTA, IN_PROF_PSICOLOGO) |> 
  mutate(PSICO_OU_NUTRI = IN_PROF_NUTRICIONISTA == 'Sim' | IN_PROF_PSICOLOGO == 'Sim') |>
  filter(TP_DEPENDENCIA == 'Privada') |> 
  group_by(NO_MUNICIPIO, PSICO_OU_NUTRI) |> 
  summarise(quant = n(), .groups = 'drop') |> 
  filter(PSICO_OU_NUTRI == 'TRUE') |> 
  left_join(base_escolas, by='NO_MUNICIPIO') |>
  mutate(PNP_PRIV = round(c(quant / Privada), 2))

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