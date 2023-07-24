# Auto-download DOF-SINAFLOR Dataset --------------------------------------

# Accessed via the IBAMA-MMA Open data Portal available at:
# https://dados.gov.br/dados/conjuntos-dados/dof-autorizacoes-de-exploracao-florestal.

# The dataset is split into the following modules which can be downloaded systematically:
# 1. DOF - Transportes de Produtos Florestais
# 2. DOF - Autorizacao de Exploracao Florestal
# 3. DOF - Conversao de Produtos Florestais
# 4. DOF - Trechos e Veiculos de Transportes Florestais (Detalhes)

# Data download is prompted by changing URL parameters state, dataset, year. 
# For instance, below you find data for Acre ("AC"), "autorizacao", "2007"
# http://dadosabertos.ibama.gov.br/dados/DOF/AC/autorizacao/2007.csv

# Last download: 20230724


# Setting up work environment ---------------------------------------------
library(tidyverse)
library(curl)

# Setting URL Parameters --------------------------------------------------

states <- as_tibble(c("AC", "AL","AP","AM","BA","CE","DF","ES","GO","MA","MT",
                      "MS", "MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO",
                      "RR","SC","SP","SE","TO"))

dof_dsets <- as_tibble(c("autorizacao", "transporte", "conversao", "detalhe"))

years <- as_tibble(c(2007:2020))


# Auto-download -----------------------------------------------------------

#specify folder for data dump
setwd("./data/raw/sinaflor")

#i indexes through the states, j  indexes through dataset type, k indexes though year
for (i in (1:27)){  #set this to length states
  for( j in (1:4)){   #set this to length dof_dsets
    for( k in (1:14)){  #set this to length years
        
      #download the url and save it as a temporary file
      downloadurl <- paste("http://dadosabertos.ibama.gov.br/dados/DOF/",
                           as.character(states[i,1]),"/",
                           as.character(dof_dsets[j,1]),"/",
                           as.character(years[k,1]),".csv", sep = "")
      
      downloadurl
      temp <- paste("DOF-", as.character(states[i,1]), "-",
                            as.character(dof_dsets[j,1]),"-",
                            as.character(years[k,1]),".csv",sep = "")
      
      
      #skip "HTTP error 404" as not all years have data
      skip_to_next <- FALSE
      
      tryCatch(curl::curl_download(downloadurl, temp), error = function(e) { skip_to_next <<- TRUE})
      
      if(skip_to_next) { next }  
      
    }}}



