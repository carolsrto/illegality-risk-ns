# Import DOF-SINAFLOR dataset (transport and authorization modules) -------

# Imports data accessed via the IBAMA-MMA Open data Portal available at:
# https://dados.gov.br/dados/conjuntos-dados/dof-autorizacoes-de-exploracao-florestal.
# See "download-sinaflor.R" script. 



# Setting up work environment ---------------------------------------------

library(tidyverse)
library(janitor)
library(odbc)
library(DBI)
library(RODBC)
library(dbplyr)


# Set global options
# disable scientific notation 
options(scipen = 999)  

# ensure decimal places as needed
options(digits=14) 

# increase limit of memory as needed
# memory.size()
# memory.limit()
memory.limit(size = 5000)


# Data Import -------------------------------------------------------------

# Import by module, given each module is essentially a different dataset and for
# better control of column type. 

#list all files
dof_sinaflor <- list.files(path = "./data/raw/sinaflor", 
                           #set pattern as convenient: "csv", "PA-transporte", "transporte"
                           pattern="csv", 
                           full.names = TRUE)

# import transport module
transporte <- dof_sinaflor %>% 
  str_subset(., pattern = "transporte") %>% 
  map_df(., read_delim, ";", escape_double = FALSE, 
         col_types = cols(`Nome/Razão Social do Remetente` = col_character(), 
                          `CPF/CNPJ do Remetente` = col_character(), 
                          `UF de Origem` = col_character(), 
                          `Município de Origem` = col_character(), 
                          `CTF do Remetente` = col_character(), 
                          `Tipo de Origem` = col_character(), 
                          `Nome do Pátio de Origem` = col_character(), 
                          `Nº de Série da Autex` = col_character(), 
                          `Nº da Autorização Original` = col_character(), 
                          `Tipo de Autex` = col_character(), 
                          `Órgão Emissor da Autex` = col_character(), 
                          `Data de Validade da Autex` = col_date(format = "%d/%m/%Y"), 
                          `Nº da DI` = col_character(), 
                          `Órgão Emissor da DI` =  col_character(), 
                          `Data de Validade da DI` = col_date(format = "%d/%m/%Y"), 
                          `Nome Porto de Entrada no País` =  col_character(),
                          `País de Origem` =  col_character(),
                          `Nº da Autesp` =  col_character(), 
                          `Órgão Emissor da Autesp` =  col_character(), 
                          `Data de Validade da Autesp` = col_date(format = "%d/%m/%Y"), 
                          `Latitude de Origem` = col_double(),
                          `Longitude de Origem` = col_double(),
                          `UF de Destino` =  col_character(),
                          `Município de Destino` =  col_character(),
                          `Nome/Razão Social Destinatário` =  col_character(),
                          `CPF/CNPJ do Destinatário` =  col_character(),
                          `CTF do Destinatário` =  col_character(),
                          `Nome Pátio de Destino` =  col_character(),
                          `Latitude do Destino` = col_double(),
                          `Longitude do Destino` = col_double(),
                          `Nome Porto de Saída do País` =  col_character(),
                          `Município do Porto` =  col_character(),
                          `UF do Porto` =  col_character(),
                          `País de Destino` =  col_character(),
                          `Data de Emissão` =  col_date(format = "%d/%m/%Y"), 
                          `Ano` = col_character(), 
                          `Validade Inicial` =  col_date(format = "%d/%m/%Y"), 
                          `Validade Final` =  col_date(format = "%d/%m/%Y"), 
                          `Última Transação` = col_character(),
                          `Data da Última Transação` =  col_date(format = "%d/%m/%Y"),
                          `Nº da Oferta` = col_character(),
                          `Nº de Série do DOF` = col_character(),
                          `Código de Controle do DOF` = col_character(),
                          `Rota do Transporte` = col_character(),
                          `Produto` = col_character(),
                          `Nome Científico` = col_character(),
                          `Nome Popular` = col_character(),
                          `Unidade` = col_character(),
                          `Volume` = col_double(),
                          `Valor (R$)` = col_double(),
                          `Última Atualização Relatório` = col_datetime(format = "%d/%m/%Y %H:%M"),
             ),
         na = c("", "NA"),
         locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8"),
         trim_ws = TRUE)




# import authorization module
autorizacao <- dof_sinaflor %>% 
  str_subset(., pattern = "autorizacao") %>% 
  map_df(., read_delim, ";", escape_double = FALSE, 
         col_types = cols(`Nome/Razão Social do Detentor` = col_character(), 
                          `CPF/CNPJ do Detentor` = col_character(), 
                          `CTF do Detentor` = col_character(), 
                          `Nome da Origem` = col_character(), 
                          UF = col_character(), 
                          `Município` = col_character(), 
                          Latitude = col_double(), 
                          Longitude = col_double(), 
                          `Área(ha)` = col_double(), 
                          `Tipo de Autex` = col_character(), 
                          `Número de Série da Autex` = col_character(), 
                          `Nº da Autorização Original` = col_character(), 
                          `Data de Cadastro da Autex` = col_date(format = "%d/%m/%Y"), 
                          `Data de Liberação da Autex` = col_date(format = "%d/%m/%Y"), 
                          `Data de Validade da Autex` = col_date(format = "%d/%m/%Y"), 
                          ANO = col_integer(), 
                          `Situação Atual` = col_character(), 
                          `Tipo de Produto` = col_character(), 
                          `Nome Científico` = col_character(), 
                          `Nome Popular` = col_character(), 
                          `Última Atualização Relatório` = col_datetime(format = "%d/%m/%Y %H:%M"), 
                          ), 
        na = c("", "NA"),
        locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8"),
        trim_ws = TRUE) #Warnings relate to encoding issue: 
                        #https://www.i18nqa.com/debug/bug-double-conversion.html


# The same approach can be used for the conversion and transportation route
# details dataset as needed.


# Data storage ------------------------------------------------------------


## To.Rdata file ----------------------------------------------------------

# Here we limit analysis to data with origin or destination in Para state for
# this study so we drop all other data at this stage.

# clean column name and removal of non-relevant flows
transporte_pa <- clean_names(transporte) |> 
  filter(uf_de_origem == "PA" | uf_de_destino == "PA")
autorizacao_pa <- clean_names(autorizacao) |> 
  filter(uf == "PA")

# first save .RData copy
save(transporte_pa, autorizacao_pa, file="./data/temp/sinaflor_import.RData")

## Database ----------------------------------------------------------------

# Given the overall data size, one may find useful to proceed with cleaning and
# exploration in a database setting. All SINAFLOR data from 2007-2020 comes to
# about 35 M observations and 17.5 GB (file explorer info). We use PostgreSQL
# and the odbc package for interfacing with R. For more on the use one db
# interface with R see: https://db.rstudio.com/best-practices/schema/

# establishing database connection 
con <- dbConnect(odbc::odbc(), "phdb", encoding = "latin1") 
# N.B. Change encoding as needed. Check encoding with readr::guess_encoding(). An
# encoding list can be also found with iconvlist().

# save transporte and autorizacao modules to database
# saving transporte_pa to schema sinaflor
dbWriteTable(con, SQL("sinaflor.transporte_pa"), transporte_pa, overwrite = TRUE)

# saving autorizacao_pa to schema sinaflor
dbWriteTable(con, SQL("sinaflor.autorizacao_pa"), autorizacao_pa, overwrite = TRUE)

# Clean env.
rm(transporte, autorizacao,  dof_sinaflor)

