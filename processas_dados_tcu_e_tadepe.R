## Gera arquivo base, que será usado para criar tratamento e controle

## carrega bibliotecas relevantes
library(tidyverse)
library(openxlsx)
library(janitor)
library(readr)
library(data.table)
library(readxl)
library(codesBR)
library(textclean)

# seta caminho
setwd("C:/Users/coliv/Documents/R-Projects/avaliacao_de_impacto_012020")

# carrega arquivos de códigos do IBGE, e obras do Tá de Pé que devem ser aleatorizadas
load("sysdata.RDA") # arquivo que vem no pacote codesBR. Fiz um hack para carregar aqui. base codigos
load("obras_visiveis_e_alertas_recebidos.Rdata") # arquivo do Tá de Pé
base_codigos <- fread("base_codigos_v1.txt", encoding="UTF-8") # fonte: http://basedosdados.org/dataset/diretorio-municipios-brasileiros/resource/c1deb363-ffba-4b1e-95dc-c5e08311852e

# carrea planilha de obras do TCU. São três abas

# aba 2, dados da Caixa
obras_gil1 <- read_excel("OBRAS PARALISADAS - Resumo_Versão original atualizada.xlsx", sheet=2) %>%
  clean_names() %>%
  rename(orgao = gestor,
         uf = uf2) %>%
  filter(s_n_t == "S") %>%
  filter(grepl("MUNICIPIO", tomador)) %>% # remove 8771 - 7995 = 776 linhas
  mutate(municipio = tomador,
         municipio = gsub("MUNICIPIO D[A-Z]? ", "", municipio),
         municipio = gsub(" - [A-Z]*" , "" , municipio),
         data_de_inicio = as.character(data_de_inicio),
         data_de_termino_da_obra = as.character(data_de_termino_da_obra)) %>%
  dplyr::select(municipio, uf, objeto, situacao, valor_investimento_r, data_de_inicio,
                data_de_termino_da_obra, percentual_realizado, orgao) %>%
  mutate(dados = "caixa")

# abas 3, dados do PAC
obras_gil2 <- read_excel("OBRAS PARALISADAS - Resumo_Versão original atualizada.xlsx", sheet=3) %>%
  clean_names() %>%
  filter(s_n_t == "S") %>%
  rename(objeto = empreendimento,
         municipio = municipios,
         percentual_realizado = percent_da_execucao_fisica,
         situacao = estagio,
         data_de_inicio = data_de_inicio_prevista,
         data_de_termino_da_obra = data_de_conclusao_revisada,
         valor_investimento_r = total_pac_milhoes_r_valor_do_contrato) %>%
  dplyr::select(municipio, uf, objeto, situacao, valor_investimento_r, data_de_inicio,
                data_de_termino_da_obra, percentual_realizado, orgao) %>%
  mutate(dados = "PAC",
         municipio = gsub("/[A-Z]+", "", municipio),
         municipio = gsub("MUNICIPIO D[A-Z]? ", "", municipio)) %>%
  filter(orgao != "Ministério da Educação")

# aba 5, dados MEC- Educação Superior
obras_gil3 <- read_excel("OBRAS PARALISADAS - Resumo_Versão original atualizada.xlsx", sheet=5) %>%
  clean_names() %>%
  filter(s_n_t == "S") %>%
  rename(objeto = descricao_da_obra,
         percentual_realizado = percent_de_execucao,
         data_de_inicio = inicio_de_execucao_da_obra,
         data_de_termino_da_obra = termino_da_execucao_da_obra_pos_aditivo,
         valor_investimento_r = valor_liquidado) %>%
  dplyr::select(municipio, uf, objeto, situacao, valor_investimento_r, data_de_inicio,
                data_de_termino_da_obra, percentual_realizado ) %>%
  mutate(orgao = NA,
         dados = "MEC ens Tec",
         municipio = gsub("MUNICIPIO D[A-Z]? ", "", municipio))


# junta as bases de dados e um único data.frame
obras_gil <- bind_rows(obras_gil1, obras_gil2, obras_gil3) %>%
  mutate(id = 1:n(),
         id = paste(id, dados, sep="_"))

# remove bases isoladas
rm(obras_gil1, obras_gil2, obras_gil3)

# arruma base do Tá de Pé, para juntar com data.frame com obras do TCU
# renomear colunas e selecionar as relevantes
obras_tdp <- obras_visiveis %>%
  rename(municipio = data_attributes_city_name ,
         uf = data_attributes_state_abbreviation,
         objeto = data_attributes_name,
         situacao = data_attributes_status,
         valor_investimento_r = data_attributes_fnde_value,
         data_de_inicio = data_attributes_execution_start_at,
         data_de_termino_da_obra = data_attributes_execution_end_at,
         percentual_realizado = data_attributes_execution_percentual,
         id = data_id) %>%
  mutate(valor_investimento_r = as.numeric(valor_investimento_r),
         percentual_realizado = as.numeric(percentual_realizado)/100,
         dados = "tadepe",
         orgao = NA) %>%
  dplyr::select(municipio, uf, objeto, situacao, valor_investimento_r, data_de_inicio,
                data_de_termino_da_obra, percentual_realizado, orgao, dados, id)

# junta base do Tá de Pé e TCU
# corrige uns nomes de municípios
obras_amostra <- bind_rows(obras_tdp, obras_gil) %>%
  mutate(municipio = gsub("Poxoréo", "Poxoréu", municipio),
         municipio = gsub("Brasópolis", "Brazópolis", municipio))

# remove obras do Tá de pé e do TCU, pois já temos data.frame com todas as obras
rm(obras_visiveis, obras_tdp)

# total de linhas da base de dados
nrow(obras_amostra) ## 8.407 obras


# arruma base de códigos do ibge, para fazer join com base de obras
base_codigos <- base_codigos %>%
  clean_names() %>%
  dplyr::select(municipality, state_abbrev, id_munic_7, id_munic_6, id_tse ) %>%
  mutate(municipality = toupper(iconv(municipality, from="UTF-8", to = "ASCII//TRANSLIT")),
         municipality = strip(municipality, apostrophe.remove = T, lower.case = F))

# corrige outra base de códigos do ibge
codigos <- codigos %>%
  dplyr::select(sigla_uf, nome_municipio, cod_ibge, cod_tse) %>%
  mutate(municipality = toupper(iconv(nome_municipio, to = "ASCII//TRANSLIT")),
         municipality = strip(municipality, apostrophe.remove = T, lower.case = F))

# vamos adicionar base de códigos do ibge às obras.
#Tenho duas bases de códigos distintas, vou usar as duas
# para maximizar a chance de um dos dois joins dar certo
# assim minimizo o total de NAs

obras_amostra1 <- obras_amostra %>%
  mutate(municipio = toupper(iconv(municipio, from="UTF-8", to = "ASCII//TRANSLIT")),
         municipio = strip(municipio, apostrophe.remove = T, lower.case = F),
         municipio = gsub(" D ", " D", municipio)) %>%
  left_join(base_codigos,  by = c("municipio" = "municipality", "uf" = "state_abbrev")) %>%
  left_join(codigos, by = c("municipio" = "municipality", "uf" = "sigla_uf")) %>%
  mutate(cod_ibge1 = ifelse(!is.na(cod_ibge), cod_ibge, id_munic_7),
         cod_ibge2 = ifelse(!is.na(cod_ibge), cod_ibge, id_munic_6),
         cod_tse_final = ifelse(!is.na(cod_tse), cod_tse, id_tse)) %>%
  dplyr::select(-c(cod_ibge, cod_tse, id_munic_6, id_munic_6, id_tse))



# obras que ficaram de fora do join, pois não deu matche
obras_fora <- obras_amostra1 %>%
  filter(is.na(cod_ibge1))

# obras que ficaram na base, com código do ibge
obras_amostra1 <- obras_amostra1 %>%
  filter(!is.na(cod_ibge1))

# total de obras final
nrow(obras_amostra1) # 8.355

# salva arquivo para aleatorizar
save(obras_amostra1, file="obras_amostra1.RData")
