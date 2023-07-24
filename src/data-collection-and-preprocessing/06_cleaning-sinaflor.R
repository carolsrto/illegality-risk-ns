# Cleaning DOF-SINAFLOR dataset ------------------------------------------

# This script document the basic routine used to clean the "transporte" module
# from the DOF-SINAFLOR dataset.

# Setting up work environment ---------------------------------------------

library(tidyverse)
library(janitor)
library(odbc)
library(DBI)
library(RODBC)
library(dbplyr)
library(devtools)
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)


# Set global options
# disable scientific notation 
options(scipen = 999)  

# ensure decimal places as needed
options(digits=14) 

# increase limit of memory as needed
# memory.size()
# memory.limit()
# memory.limit(size = 5000)


# Data Import -------------------------------------------------------------

# Import data from db (all flows with origin or destination in Para, raw data)
con <- dbConnect(odbc::odbc(), "phdb")
transporte_pa <- tbl(con, in_schema("sinaflor", "transporte_pa")) |> collect()

# Or load from temp data object 
load("data/temp/sinaflor_import.RData")

#Add mun geocode
#obtain geocode from geobr package
mun <- geobr::read_municipality(code_muni = "all", year = 2020)


# Cleaning  --------------------------------------------------------------


# Across dataset: rename columns with EN, remove all special characters, set all
# to upper case.
transp_pa_clean1 <-  transporte_pa |> 
  #rename columns-EN
  dplyr::rename(ENTERPRISE_ORIGIN = nome_razao_social_do_remetente,           
                CPF_CNPJ_ORIGIN = cpf_cnpj_do_remetente, 
                UF_ORIGIN = uf_de_origem, 
                MUNICIPALITY_ORIGIN = municipio_de_origem, 
                CTF_ORIGIN = ctf_do_remetente, 
                TYPE_ORIGIN = tipo_de_origem, 
                NAME_PATIO_ORIGIN = nome_do_patio_de_origem, 
                NUM_AUTEX_SERIE = no_de_serie_da_autex, 
                NUM_ORIGINAL_PERMIT = no_da_autorizacao_original, 
                TYPE_AUTEX = tipo_de_autex, 
                ISSUING_BODY_AUTEX = orgao_emissor_da_autex, 
                VALID_DATE_AUTEX = data_de_validade_da_autex, 
                NUM_IMPORT_DECLARATION = no_da_di, 
                ISSUING_BODY_IMPORT_DECLARATION = orgao_emissor_da_di, 
                VALID_DATE_IMPORT_DECLARATION = data_de_validade_da_di, 
                PORT_OF_ENTRY = nome_porto_de_entrada_no_pais, 
                COUNTRY_ORIGIN = pais_de_origem,
                NUM_AUTESP = no_da_autesp, 
                ISSUING_BODY_AUTESP = orgao_emissor_da_autesp, 
                VALID_DATE_AUTESP = data_de_validade_da_autesp, 
                LATITUDE_ORIGIN = latitude_de_origem, 
                LONGITUDE_ORIGIN = longitude_de_origem,
                UF_DESTINATION = uf_de_destino, 
                MUNICIPALITY_DESTINATION = municipio_de_destino, 
                ENTERPRISE_DESTINATION = nome_razao_social_destinatario, 
                CPF_CNPJ_DESTINATION = cpf_cnpj_do_destinatario, 
                CTF_DESTINATION = ctf_do_destinatario, 
                NAME_PATIO_DESTINATION = nome_patio_de_destino, 
                LATITUDE_DESTINATION = latitude_do_destino, 
                LONGITUDE_DESTINATION = longitude_do_destino, 
                PORT_OF_EXPORT = nome_porto_de_saida_do_pais, 
                MUNICIPALITY_PORT = municipio_do_porto, 
                UF_PORT = uf_do_porto, 
                COUNTRY_DESTINATION = pais_de_destino, 
                ISSUING_DATE = data_de_emissao, 
                YEAR = ano, 
                VALID_DATE_START = validade_inicial, 
                VALID_DATE_END = validade_final, 
                LAST_TRANSACTION = ultima_transacao, 
                DATE_LAST_TRANSACTION = data_da_ultima_transacao, 
                NUM_OFFER = no_da_oferta, 
                DOF_SERIE = no_de_serie_do_dof, 
                DOF_CONTROL_CODE = codigo_de_controle_do_dof, 
                TRANSPORT_ROUTE = rota_do_transporte, 
                PRODUCT = produto, 
                SCIENTIFIC_NAME = nome_cientifico, 
                COMMON_NAME = nome_popular, 
                UNIT = unidade, 
                VOLUME = volume, 
                VALUE_BRL = valor_r, 
                LAST_REPORT_UPDATE = ultima_atualizacao_relatorio) |> 
  # remove special characters
  mutate(across(where(is.character), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  # set all to upper case
  mutate(across(where(is.character), toupper))

# Remove punctuation from CPF_CNPJ (except for ***, which used for anonymization
# of CPFs)
transp_pa_clean2 <- transp_pa_clean1 |> 
  mutate(across(c(CPF_CNPJ_ORIGIN, CPF_CNPJ_DESTINATION), 
                ~ str_remove_all(.,"[\\.\\/\\-]")))


# Adding municipality geocode ---------------------------------------------

# First, adding geocode of municipalities
# Departing from DOF, Unite UF_MUN_ORIGIN and UF_MUN_DEST into a single UF_MUN list
all_org_dest_mun <- transp_pa_clean2 |>  
  select(UF_ORIGIN, MUNICIPALITY_ORIGIN, UF_DESTINATION, MUNICIPALITY_DESTINATION) |> 
  unite(UF_MUN_OR, UF_ORIGIN, MUNICIPALITY_ORIGIN, sep = "_") |>  
  unite(UF_MUN_DEST, UF_DESTINATION, MUNICIPALITY_DESTINATION, sep = "_") |>  
  pivot_longer(c(`UF_MUN_OR`, `UF_MUN_DEST`), names_to = "DETAIL", values_to = "UF_MUN")

# Number of municipalities
distict_org_dest_mun <- all_org_dest_mun |>  distinct(UF_MUN) 

# Departing from mun, build UF_MUN and retain geocode
geocmun <- mun |>  
  select(code_muni, name_muni, abbrev_state) |>   
  as_tibble() |>  
  select(-geom) |>  
  unite(UF_MUN, abbrev_state, name_muni, sep = "_") |>  
  mutate(across(where(is.character), toupper)) |> 
  #remove special characters to match 
  mutate(across(c(UF_MUN), ~stringi::stri_trans_general(., "Latin-ASCII")))

# Checking existing patterns discrepancy between datasets on the "´" vs. "'" 
str_detect(distict_org_dest_mun$UF_MUN, "`") |> view(n=length(distict_org_dest_mun$UF_MUN))|> count(x) 
str_detect(distict_org_dest_mun$UF_MUN, "´") |> view(n=length(distict_org_dest_mun$UF_MUN))|> count(x)
str_detect(distict_org_dest_mun$UF_MUN, "'") |> view(n=length(distict_org_dest_mun$UF_MUN))|> count(x) 

str_detect(geocmun$UF_MUN, "`") |> view(n=length(geocmun$UF_MUN))|> count(x) 
str_detect(geocmun$UF_MUN, "´") |> view(n=length(geocmun$UF_MUN))|> count(x) 
str_detect(geocmun$UF_MUN, "'") |> view(n=length(geocmun$UF_MUN))|> count(x) 

#distict_org_dest_mun$UF_MUN <-  gsub( "´", "'", distict_org_dest_mun$UF_MUN)


#Combining unique municipalities per state with respective geocode
join_mun_geoc <- distict_org_dest_mun |>  
  full_join(geocmun) 

# Check whether geocode is missing
join_mun_geoc |> filter(is.na(UF_MUN)) |> view()
join_mun_geoc |> filter(is.na(code_muni)) |> view()

# Manual assignment of municipality geocode for those missing Name of
# municipality will not be changed. Only respective geocode will be added. For
# example, both RJ_PARATI and RJ_PARATY will receive geocode 3303807 we will
# have both still listed on the dataset.

geocmun_key <- join_mun_geoc |>  
  rename(GEOCODE = code_muni) |>  
  mutate(GEOCODE = case_when(UF_MUN == "RS_SANTANA DO LIVRAMENTO" ~ 4317103, TRUE ~ GEOCODE)) |>   
  mutate(GEOCODE = case_when(UF_MUN == "RJ_PARATI" ~ 3303807, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "RN_AUGUSTO SEVERO" ~ 2401305, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "PB_CAMPO DE SANTANA" ~ 2516409, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "SP_EMBU" ~ 3515004, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "MG_DONA EUSEBIA" ~ 3122900, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "RN_ASSU" ~ 2400208, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "SP_MOJI MIRIM" ~ 3530805, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "TO_COUTO DE MAGALHAES" ~ 1706001, TRUE ~ GEOCODE)) |>
  mutate(GEOCODE = case_when(UF_MUN == "SP_MOJI MIRIM" ~ 3530805, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "PA_PIACAS" ~ 1505650, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "RN_PRESIDENTE JUSCELINO" ~ 2410306, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "PB_SERIDO" ~ 2515401, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "PE_BELEM DE SAO FRANCISCO" ~ 2601607, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "SP_EMBU" ~ 3515004, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "MG_DONA EUSEBIA" ~ 3122900, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "PA_ELDORADO DOS CARAJAS" ~ 1502954, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "PA_SANTA ISABEL DO PARA" ~ 1506500, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "CE_ITAPAGE" ~ 2306306, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "PE_IGUARACI" ~ 2606903, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "SP_SAO LUIS DO PARAITINGA" ~ 3550001, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "MG_BRASOPOLIS" ~ 3108909, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "RJ_TRAJANO DE MORAIS" ~ 3305901, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "BA_SANTA TERESINHA" ~ 2928505, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "RS_SANTANA DO LIVRAMENTO" ~ 4317103, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "TO_SAO VALERIO DA NATIVIDADE" ~ 1720499, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "BA_MUQUEM DE SAO FRANCISCO" ~ 2922250, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "RN_ASSU" ~ 2400208, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "SP_MOJI MIRIM" ~ 3530805, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "TO_SAO VALERIO DA NATIVIDADE" ~ 1720499, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "BA_SANTA TERESINHA" ~ 2928505, TRUE ~ GEOCODE)) |> 
  mutate(GEOCODE = case_when(UF_MUN == "SC_GRAO PARA" ~ 4206108, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "PE_NOVA PETROLANDIA" ~ 2611002, TRUE ~ GEOCODE)) |>  
  mutate(GEOCODE = case_when(UF_MUN == "DF_MUNICIPIO PROVISORIO" ~ 5300108, TRUE ~ GEOCODE)) 

#Check whether any municipality remains with no corresponding geocode  
geocmun_key |> filter(is.na(UF_MUN)) |> view() 
geocmun_key |> filter(is.na(GEOCODE)) |> view() 

#Check out duplicated GEOCODES
geocmun_key |>  
  group_by(GEOCODE) |>  
  mutate(DUPLICATES = n()>1) |>  
  filter(DUPLICATES == TRUE) |>  
  view()


# Add GEOCMUN_ORIGIN and GEOCMUN_DESTTINATION to dataset
transp_pa_clean3 <- transp_pa_clean2 |> 
  unite(UF_MUN, UF_ORIGIN, MUNICIPALITY_ORIGIN, sep = "_", remove = FALSE) |>  
  left_join(geocmun_key, by = c("UF_MUN")) |>  
  rename(GEOCMUN_ORIGIN = GEOCODE) |>  
  relocate(GEOCMUN_ORIGIN, .after = MUNICIPALITY_ORIGIN) |>  
  select(-UF_MUN) |>  
  unite(UF_MUN, UF_DESTINATION, MUNICIPALITY_DESTINATION, sep = "_", remove = FALSE) |>  
  left_join(geocmun_key, by = c("UF_MUN")) |>  
  rename(GEOCMUN_DESTINATION = GEOCODE) |>  
  relocate(GEOCMUN_DESTINATION, .after = MUNICIPALITY_DESTINATION) |>  
  select(-c(UF_MUN, MUNICIPALITY_ORIGIN, MUNICIPALITY_DESTINATION))


rm(mun, geocmun_key, join_mun_geoc, all_org_dest_mun, distict_org_dest_mun, geocmun)


transporte_pa <- transp_pa_clean3

#Save .RData copy
save(transporte_pa, autorizacao_pa, file="./data/temp/transporte_sinaflor_clean.RData")


# Saving to db. 

# N.B. After removals its becoming clear the dataset for ipe-only will be
# manageable outside a database env. If expanding to all states, species and
# covering more years, a db env becomes a better solution. 

# #establishing database connection 
# con <- dbConnect(odbc::odbc(), "phdb", encoding = "UTF-8") 
# #N.B. Change encoding as needed. Check encoding with readr::guess_encoding(). An
# #encoding list can be also found with iconvlist().
# 
# #save transporte and autorizacao modules to database
# #saving transporte_pa to schema sinaflor
# dbWriteTable(con, SQL("sinaflor.transporte_pa"), transporte_pa, overwrite = TRUE)
# 
# #saving autorizacao_pa to schema sinaflor
# dbWriteTable(con, SQL("sinaflor.autorizacao_pa"), autorizacao_pa, overwrite = TRUE)

