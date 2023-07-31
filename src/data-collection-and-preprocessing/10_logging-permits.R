# Consolidating and cross-validating logging permit data  -----------------

# Data on logging permits are compiled here using different supporting sources
# of information, nonetheless items 2 and 4 in "loading data" section below are the
# primary data for state and federal logging authorization respectively. The
# outcome of this script focuses on the variables used for the study (i.e. permit
# number, permit year, municipality, coordinates/geometry, area authorized,
# status of permits, and type of permit), but others can also be added from data
# imported below or leveraging the autef pdf scraping code.



# Setting up the work environment -----------------------------------------

# Loading libraries 
library(tidyverse)
library(janitor)
library(Hmisc)
library(sf)
library(geobr)
library(parzer)
library(tmap)

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits=14) 



# Loading data ------------------------------------------------------------

# 1. Polygons of Annual Production Units (UPA) authorized for harvest via the
# AUTEF logging permits. Downloaded from SIMLAM webpage Jan-2021. Found in
# shapefile format via SIMLAM (PMFS.rar)
# http://monitoramento.semas.pa.gov.br/simlam/bases_simlam_publico/

# Load available upa polygons 
autef_upa <- sf::st_read("data/raw/autef-upa-shp-simlam/UPA.shp") |> #, options = "ENCODING=ISO-8859-1"
  rowid_to_column() |> 
  mutate(rowid = as.character(rowid))



# 2. AUTEF data from pdf scrapping accessed via SIMLAM webpage 
# https://monitoramento.semas.pa.gov.br/simlam/index.htm (licensing tab,
# LAR and AUTEF search engine)
autef_pdf <- read_csv("data/raw/autef-pdfs-simlam/autef_pdfs.csv")

# First round download/scrapping. Used here to collect stats for
# methods/documentation.
autef_pdf_v0 <- read_delim("data/raw/autef-pdfs-simlam/pa_autef_info.csv", 
                        delim = ";", escape_double = FALSE, 
                        col_types = cols(imovel_area = col_character(),
                                         reserva_legal_area = col_character(), 
                                         mfs_area = col_character(), 
                                         antropizada_area = col_character(), 
                                         app_upa = col_character(), 
                                         area_autorizada = col_character(), 
                                         quantidade_m3 = col_character()), 
                        locale = locale(decimal_mark = ",", grouping_mark = "."), 
                        trim_ws = TRUE)



# 3. Brancalion et al 2018 autef data made available in supplementary material. 
autef_brancalion <- read_delim("data/raw/autef-brancalion/AUTEFS_tabeladas_TUDO_final.csv",
                                          delim = ";", escape_double = FALSE, col_types = cols(arquivo = col_character(), 
                                          autef = col_character(), area_t = col_character(), 
                                          area_l = col_character(), nomes_ci = col_character(), 
                                          nomes_pop = col_character(), ind = col_integer(), 
                                          vol_ha = col_double(), vol_tot = col_double()), 
                                          locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                          encoding = "ISO-8859-1"), trim_ws = TRUE)



# 4. AUTEX data from "autorizacao" module of DOF-SINAFLOR made available through
# Brazil's open data portal. Loaded from previous import of relevant DOF-SINAFLOR
# modules (available via "import-sinaflor.R" script):
load("./data/raw/autex-sinaflor/sinaflor_import.RData.RData")
rm(transporte_pa)


# 5. Type of origin data from Ibama 2019 study. 
autef_type_credit <- read_csv("data/raw/autef-credits-ibama/saldoActAcp 10-01-2008 (simplificado).csv")


# 6. State and municipality information

# obtain geocode from geobr package
mun <- geobr::read_municipality(code_muni = "all", year = 2020)
# load data on states
states <- geobr::read_state(year = "2020")



# Cleaning  ---------------------------------------------------------------

## autef-upa ---------------------------------------------------------------

# Cleaning round performed on shapefile data prior to PDF scrapping, thus
# performs corrections superseeded by latter (e.g. area authorized, year of
# permit issuance etc.). Still, kept here as documentation and evidence the data
# associated with polygons was likely hand-typed, showcasing data integration
# and management challenges within state-level gov. institutions.

# General check 
  # Simple feature collection with 1269 features and 11 fields (with 3 geometries empty)
  # Geometry type: MULTIPOLYGON
  # Dimension:     XY
  # Bounding box:  xmin: -57.034247375525 ymin: -9.3650160475871 xmax: -46.227746240996 ymax: -0.18875065993848
  # Geodetic CRS:  SIRGAS 2000

# Check out missing polygons
autef_upa |> filter(is.na(geometry)) 
  # Missing geometries (3) have been removed at import

# Remove geometry and clean content as df
autef_upa_df1 <- autef_upa |> 
  st_set_geometry(NULL) |> 
  janitor::clean_names() |> 
  rename(volume_m3 = volume_m)



### Addressing hand-typed errors and outliers  ------------------------------

# Checking and addressing the most common hand-typed errors based on visual check

# Checking extra letters and spaces in the double columns
str_which(autef_upa_df1$imovel_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$upa_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$arealiq_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$volume_m3, "[:alpha:]|[:space:]")

# checking what these look like
autef_upa_df1[403, c(9:12)]
autef_upa_df1[842, c(9:12)]
autef_upa_df1[582, c(9:12)]

# > autef_upa_df1[403, c(9:12)]
# imovel_ha   upa_ha arealiq_ha volume_m3
# 403  303, 874 197,3479   130,1692 3790,0730
# > autef_upa_df1[842, c(9:12)]
# imovel_ha   upa_ha arealiq_ha  volume_m3
# 842   2178 ha 857,4286   834,6749 22864,4609
# > autef_upa_df1[582, c(9:12)]
# imovel_ha upa_ha arealiq_ha volume_m3
# 582       100  99,95   97,94 ha 1348,1600

# Remove letters and extra spaces from to-be-double columns
autef_upa_df1 <- autef_upa_df1 |> mutate(across(imovel_ha:volume_m3, ~ str_replace_all(., "[:alpha:]|[:space:]", "")))

# Checking removal
str_which(autef_upa_df1$imovel_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$upa_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$arealiq_ha, "[:alpha:]|[:space:]")
str_which(autef_upa_df1$volume_m3, "[:alpha:]|[:space:]")



# Checking on extra dots
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_detect(., "\\.")))

# rowid    imovel_ha    upa_ha arealiq_ha   volume_m3
# 1    182    1752,5950   597,598   538,4073  16.39,7594
# 2    217   83437,1287  4.006,89 3.886,3943 106757,3027
# 3    272     774.6370   774,637   727,1690  21617,5940
# 4    361    2881,6897  2445,357 2.367,0609  71003,7588
# 5    366   2.991,3932  1574,237  1369,7753  41087,9728
# 6    371     486,3509     262,9   233,4610  5.287,3562
# 7    391    55.602,22 2497,5436  2469,4241  71057,1985
# 8    526  22.147,5739 2084,1346  1948,3045  41993,4056
# 9    614   153.113,94   4146,54  3458,7764 100453,8990
# 10   759 153.115,0258  3748,721  3304,2352  94167,5636
# 11   898     923,7482   515,349   491,1928 12.769,7359
# 12   902    1653,9836 5.863.690   564,4386  13659,9617
# 13   986      96,7361   77,7158    66,0011  1.978,3404


# Check on two or more commas or dots
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_count(., "[:punct:]") >= 2))

# rowid    imovel_ha    upa_ha arealiq_ha   volume_m3
# 1    182    1752,5950   597,598   538,4073  16.39,7594
# 2    217   83437,1287  4.006,89 3.886,3943 106757,3027
# 3    361    2881,6897  2445,357 2.367,0609  71003,7588
# 4    366   2.991,3932  1574,237  1369,7753  41087,9728
# 5    371     486,3509     262,9   233,4610  5.287,3562
# 6    391    55.602,22 2497,5436  2469,4241  71057,1985
# 7    526  22.147,5739 2084,1346  1948,3045  41993,4056
# 8    614   153.113,94   4146,54  3458,7764 100453,8990
# 9    759 153.115,0258  3748,721  3304,2352  94167,5636
# 10   898     923,7482   515,349   491,1928 12.769,7359
# 11   902    1653,9836 5.863.690   564,4386  13659,9617
# 12   986      96,7361   77,7158    66,0011  1.978,3404


# Correcting only occurrence of a single dot 
str_which(autef_upa_df1$imovel_ha, "774.6370") 
autef_upa_df1[272, 9:12]
autef_upa_df1[272, 9] <- "774,6370" #Typing error
autef_upa_df1[272, 9] 


# Check double dot
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_count(., "\\.") >= 2))

# Correcting error
str_which(autef_upa_df1$upa_ha, "5.863.690") #Data check https://monitoramento.semas.pa.gov.br/simlam/index.htm
autef_upa_df1[902, ]
autef_upa_df1[902, 10]
autef_upa_df1[902, 10] <- "586,3690" #Typing error
autef_upa_df1[902, 10]


# Check double commas
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_count(., "\\,") >= 2))


# Checking manually more significant/problematic values: 
str_which(autef_upa_df1$volume_m3, "16.39,7594")

autef_upa_df1[182, ]
autef_upa_df1[182, 12]
autef_upa_df1[182, 12] <- "16139,7594" #Typing error: 1 replaced by dot
autef_upa_df1[182, 12]

str_which(autef_upa_df1$upa_ha, "4.006,89") # Correct value, the only issue is the dot 
# separator for the thousand values but change 
# it altogether after. 

str_which(autef_upa_df1$imovel_ha, "55.602,22")

autef_upa_df1[391, ]
autef_upa_df1[391, 9]
autef_upa_df1[391, 9] <- "69251,9667" #Typing error: previous number was the RL (Legal Reserve area)
autef_upa_df1[391, 9]

str_which(autef_upa_df1$imovel_ha, "153.113,94")

autef_upa_df1[614, ]
autef_upa_df1[614, 9]
autef_upa_df1[614, 9] <- "153113,0258" #Error on the decimals only (checked against LAR too)
autef_upa_df1[614, 9]

str_which(autef_upa_df1$volume_m3, "12.769,7359") # Correct value, the only issue is the dot 
# separator for the thousand values but change 
# it altogether after. 

str_which(autef_upa_df1$volume_m3, "107600145") # Outliers in EDA: Volume should be 10760.0145  
# processo 2009/31858; autef 1704

autef_upa_df1[330, ]
autef_upa_df1[330, 12]
autef_upa_df1[330, 12] <- 10760.0145
autef_upa_df1[330, 12]



# Remove all dots as separator 

# Last check on remaining dots
autef_upa_df1 |> 
  select(rowid, imovel_ha:volume_m3) |> 
  filter(if_any(imovel_ha:volume_m3, ~ str_detect(., "\\.")))
# rowid    imovel_ha    upa_ha arealiq_ha   volume_m3
# 1   217   83437,1287  4.006,89 3.886,3943 106757,3027
# 2   361    2881,6897  2445,357 2.367,0609  71003,7588
# 3   366   2.991,3932  1574,237  1369,7753  41087,9728
# 4   371     486,3509     262,9   233,4610  5.287,3562
# 5   526  22.147,5739 2084,1346  1948,3045  41993,4056
# 6   759 153.115,0258  3748,721  3304,2352  94167,5636
# 7   898     923,7482   515,349   491,1928 12.769,7359
# 8   986      96,7361   77,7158    66,0011  1.978,3404

# Remove all dots
autef_upa_df1 <- autef_upa_df1 |> mutate(across(imovel_ha:volume_m3, ~ str_replace_all(., "\\.", "")))

# Replace all commas with dots
autef_upa_df1 <- autef_upa_df1 |> mutate(across(imovel_ha:volume_m3, ~ str_replace_all(., ",", ".")))

#Converting chr to numeric 
autef_upa_df1 <- autef_upa_df1 |> 
  mutate(across(imovel_ha:volume_m3, ~as.numeric(.))) 




### Addressing dates --------------------------------------------------------

# Change data type from char to date 
autef_upa_df2 <- autef_upa_df1 |> 
  mutate(validautef =  as.Date(validautef, format = "%d/%m/%Y")) 
# N.B.: This coerces certain hand-typed errors into NA so identify/fix dates
# that do not follow pattern.

# Check NAs on dates
#check_is_na <- autef_upa_df2 |> filter_all(any_vars(is.na(.))) 
check_is_na_date <- autef_upa_df2 |> filter(is.na(validautef)) 

autef_upa_df1[3,]
autef_upa_df2[3,]
autef_upa_df2[3, 7] <- as.Date("2018-04-23")
autef_upa_df2[3,]

autef_upa_df1[73, ]
autef_upa_df2[73, ]
autef_upa_df2[73, 7] <- as.Date("2014-11-27")
autef_upa_df2[73, ]

autef_upa_df1[333, ]
autef_upa_df2[333, ]
autef_upa_df2[333, 7] <- as.Date("2011-11-23")
autef_upa_df2[333, ]

autef_upa_df1[1173, ]
autef_upa_df2[1173, ]
autef_upa_df2[1173, 7] <- as.Date("2019-09-27")
autef_upa_df2[1173, ]

#Aside from NAs, typos:
autef_upa_df1[871, ]
autef_upa_df2[871, ]
autef_upa_df2[871, 7] <- as.Date("2019-05-16")
autef_upa_df2[871, ]

#Type on year column
autef_upa_df1[1030, ]
autef_upa_df2[1030, ]
autef_upa_df2[1030, 6] <- "2013"
autef_upa_df2[1030, ]

#For summarised issues: date errors spotted at the initial type_convert() trial. 
# 1: [1039, 7]: expected no trailing characters, but got '2013
# 2013
# ' 
# 2: [2, 8]: expected date like %d/%m/%Y, but got '23/042018' 
# 3: [78, 8]: expected date like %d/%m/%Y, but got '27/112014' 
# 4: [340, 8]: expected date like %d/%m/%Y, but got '23/11//2011' 
# 5: [877, 8]: expected date like %d/%m/%Y, but got '16/05/219' 
# 6: [1178, 8]: expected date like %d/%m/%Y, but got '27/092019' 

### Rename/translate cols and last cosmetic changes --------------------------
autef_upa_df3 <- autef_upa_df2 |> 
  dplyr::rename(holder = detentor,#translate while cleaning dataset
                property = nomeimovel, 
                process = processo, 
                year_autef = anoautef, 
                valid_autef = validautef, 
                municipality = municipio, 
                property_ha = imovel_ha, 
                volume_m3 = volume_m3) |> 
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(AREALIQ_HA = as.double(AREALIQ_HA))



### Remove duplicates -------------------------------------------------------

# Check on n of duplicates
dupe_check <- autef_upa_df3  |>  group_by(AUTEF) |> mutate(n = n())  |>  filter(n > 1)

# Keep rows with distinct combinations for all variables (except the id and the
# removed geometry
autef_upa_df4 <- autef_upa_df3 |> 
  distinct(HOLDER, PROPERTY, PROCESS, AUTEF, YEAR_AUTEF, VALID_AUTEF, MUNICIPALITY, 
      PROPERTY_HA, UPA_HA, AREALIQ_HA, VOLUME_M3, .keep_all = TRUE)
#1247 Observations


# Check on observations that remain a duplicate
dupe_check <- autef_upa_df4  |>  group_by(AUTEF) |> mutate(n = n())  |>  filter(n > 1)

# Duplicates only due to style of process number reporting/trailing zeros

autef_upa_df5 <- autef_upa_df4 |> distinct(AUTEF, .keep_all = TRUE)
#1243 Observations as well as unique AUTEFs. 



### Addressing characters and encoding ---------------------------------------

#Remove all special characters
autef_upa_df6 <- autef_upa_df5 |> 
  mutate(across(where(is.character), ~stringi::stri_trans_general(., "Latin-ASCII")))



### Addressing missing area -------------------------------------------------

# Obsolete after complete pdf auto-read, but leaving here for documentation

Hmisc::describe(autef_upa_df6)
# 21 permits do not display area liq., need to be double checked
# 4 have missing volume

# Check permits with missing area
autef_upa_df6 |> 
  filter(is.na(AREALIQ_HA)) |> view()

# Manual correction
autef_upa_df6 <- autef_upa_df6 |> 
  mutate(AREALIQ_HA = case_when(AUTEF == "1151" ~ 68.3790, 
                                AUTEF == "1276" ~ 70.8603,
                                AUTEF == "291" ~ 59.3750,
                                AUTEF == "347" ~ 70.0000,
                                AUTEF == "411" ~ 78.3178,
                                AUTEF == "416" ~ 71.6642,
                                AUTEF == "445" ~ 62.2440,
                                AUTEF == "647" ~ 57.3352,
                                AUTEF == "698" ~ 53.2498,
                                AUTEF == "145" ~ 73.4351,
                                AUTEF == "263" ~ 63.3654,
                                AUTEF == "594" ~ 73.8935,
                                AUTEF == "596" ~ 66.0428,
                                AUTEF == "846" ~ 44.5524,
                                AUTEF == "522" ~ 56.3010,
                                AUTEF == "573" ~ 76.9756,
                                AUTEF == "389" ~ 46.4617,
                                AUTEF == "108" ~ 2584.6380,
                                AUTEF == "125" ~ 447.4880,
                                AUTEF == "126" ~ 1524.8690, 
                                AUTEF == "137" ~ 692.9690, 
                                AUTEF == "128" ~ 300.0000, 
                                TRUE ~ AREALIQ_HA
                                ))



### Addressing municipalities ---------------------------------------------

# N.B. Municipality issues and geocode addition resolved later in the code by
# using data from the PDF scrapping. The next few steps are kept for the purpose
# of documentation.

# Manual fix prior to geocode addition
  # 11 entries display multiple municipalities:

# MUNICIPALITY                          code_muni
# 1                      RODON DO PARA        NA
# 2                   GOANESIA DO PARA        NA  
# 3                      NOV PROGRESSO        NA
# 4                  MOJUI- DOS CAMPOS        NA
# 5             SENADOR JOSE PORFI-RIO        NA
# 6                  AVEIRO/ RUROPOLIS        NA
# 7                           TUCURUI-        NA
# 8                         PAU D ARCO        NA
# 9             SEANDOR JOSE PORFI-RIO        NA
# 10                 ALTAMIRA/ITAITUBA        NA
# 11                           ALMERIM        NA
# 12                           JURUTI-        NA
# 13           GOIANESIA DO PARA\nESIA        NA
# 14                  PLACAS/RUROPOLIS        NA
# 15                     PLACAS/URUARA        NA
# 16          PLACAS/MOJUI- DOS CAMPOS        NA
# 17                  RUROPOLIS/AVEIRO        NA
# 18                           IAITUBA        NA
# 19                  ITAITUBA/TRAIRIO        NA
# 20                           ITAIUBA        NA
# 21 PACAJA/TUCURUI-/NOVO REPARTIMENTO        NA
# 22       PORTEL/BAGRE/OEIRAS DO PARA        NA
# 23                     CHAVES/ANAJAS        NA
# 24                        MOJU/BAIIO        NA

# Reassigning municipality names
autef_upa_df6 <- autef_upa_df6 |> 
  mutate(MUNICIPALITY = case_when(MUNICIPALITY == "RODON DO PARA" ~ "RONDON DO PARA",
                                  MUNICIPALITY == "GOANESIA DO PARA" ~ "GOIANESIA DO PARA",
                                  MUNICIPALITY == "GOIANESIA DO PARA\nESIA" ~ "GOIANESIA DO PARA",
                                  MUNICIPALITY == "NOV PROGRESSO" ~ "NOVO PROGRESSO",
                                  MUNICIPALITY == "MOJUI- DOS CAMPOS" ~ "MOJUI DOS CAMPOS",
                                  MUNICIPALITY == "SENADOR JOSE PORFI-RIO" ~ "SENADOR JOSE PORFIRIO",
                                  MUNICIPALITY == "TUCURUI-" ~ "TUCURUI",
                                  MUNICIPALITY == "PAU D ARCO" ~ "PAU D'ARCO",
                                  MUNICIPALITY == "ALMERIM" ~ "ALMEIRIM",
                                  MUNICIPALITY == "JURITI-" ~ "JURITI",
                                  MUNICIPALITY == "IAITUBA" ~ "ITAITUBA",
                                  MUNICIPALITY == "ITAIUBA" ~ "ITAITUBA",
                                  TRUE ~ MUNICIPALITY))



# Distinct municipalities in the autef_upa data
distinct_mun <- autef_upa_df6 |> 
  distinct(MUNICIPALITY) 

# Departing from mun, build UF_MUN and retain geocode
geocmun <- mun |>  
  select(code_muni, name_muni) |>   
  as_tibble() |>  
  select(-geom) |>  
  filter(grepl(c("^15"), code_muni)) |> 
  mutate(across(where(is.character), toupper)) |> 
  #remove special characters to match 
  mutate(across(c(name_muni), ~stringi::stri_trans_general(., "Latin-ASCII"))) 

join_mun_geoc <- distinct_mun |>  
  left_join(geocmun, by = c("MUNICIPALITY" = "name_muni")) |> 
  rename(GEOCODE = code_muni)

join_mun_geoc |> filter(is.na(GEOCODE)) 

# MUNICIPALITY code_muni
# 1                  AVEIRO/ RUROPOLIS        NA
# 2             SEANDOR JOSE PORFI-RIO        NA
# 3                  ALTAMIRA/ITAITUBA        NA
# 4                            JURUTI-        NA
# 5                   PLACAS/RUROPOLIS        NA
# 6                      PLACAS/URUARA        NA
# 7           PLACAS/MOJUI- DOS CAMPOS        NA
# 8                   RUROPOLIS/AVEIRO        NA
# 9                   ITAITUBA/TRAIRIO        NA
# 10 PACAJA/TUCURUI-/NOVO REPARTIMENTO        NA
# 11       PORTEL/BAGRE/OEIRAS DO PARA        NA
# 12                     CHAVES/ANAJAS        NA
# 13                        MOJU/BAIIO        NA

# Add geocode to upa data
autef_upa_df7 <- autef_upa_df6 |> 
  left_join(join_mun_geoc, by = "MUNICIPALITY")



### Add back geometry -------------------------------------------------------
#add back geom: 
autef_upa_df7 <- left_join(autef_upa_df7, autef_upa |> select(rowid, geometry), 
                           by = c("ROWID" = "rowid")) |> 
  #remove previous id and save resulting df with a new ID
  rowid_to_column() |> 
  select(-ROWID) |> 
  rename(GEOM = geometry, ID = rowid)



### Adding centroid lat-long -------------------------------------------------

#st_centroid requires object of class sfg, sfc or sf, thus obtain centroid in a
#different step and join back

# generating polygon centroid 
upa_geom <- autef_upa_df7 |>  
  select(AUTEF, GEOM) |> 
  st_as_sf() |> 
  st_make_valid() |> 
  st_centroid() |> 
  mutate(X = map_dbl(GEOM, 1),
         Y = map_dbl(GEOM, 2)) |> 
  as_tibble()|>  
  select(-GEOM)

# add centroid to upa data
autef_upa_df8 <- autef_upa_df7 |> 
  left_join(upa_geom, by = "AUTEF")


### Addressing type of origin ------------------------------------------------

# All permits listed in this shapefile are tagged as AUTEF and connected to
# annual production units (UPAs) from Sustainable Forest Management Plans (PMFS)
autef_upa_df9 <- autef_upa_df8 |> 
  mutate(TYPE = "AUTEF")



### Saving output ------------------------------------------------------------

autef_upa <- autef_upa_df9 |> 
  #remove "susp" from AUTEF logging permit number (status will come from pdfs)
  mutate(AUTEF = str_remove(AUTEF, "SUSP"))

# Visual check 
plot(autef_upa_df9$GEOM)

#Clean env.
rm(autef_upa_df1, autef_upa_df2, autef_upa_df3, autef_upa_df4, autef_upa_df5,
   autef_upa_df6, autef_upa_df7, autef_upa_df8, autef_upa_df9, check_is_na_date,
   dupe_check, join_mun_geoc, distinct_mun, geocmun, upa_geom)



## autef_pdf_v0 (for statistics on auto_download/methods) -------------------------------

# General cleaning
autef_pdf_v0_df1 <- autef_pdf_v0 |> 
  janitor::clean_names() |>  
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII"))) 

# Create new column with autef number only
autef_pdf_v0_df2 <- autef_pdf_v0_df1 |>
  separate(AUTEF_NUM, into = c("PERMIT_NUMBER", "PERMIT_YEAR")) |>
  mutate(PERMIT_NUMBER = str_remove(PERMIT_NUMBER, "^0+")) 

# Check missing data as this was the first try for pdf scrapping

Hmisc::describe(autef_pdf_v0)
# Missing area liquida 
# Missing status (calcelada, suspensa etc.) 
# Missing coordinates: 1315
# Missing municipality: 48
# Missing valid date: 90

# Most permits are indeed unique, thus remove 4 duplicates
autef_pdf_v0_df2 |> distinct(PERMIT_NUMBER, PERMIT_YEAR) |> count()



### Remove duplicates -------------------------------------------------------

# Are there duplicates?
autef_pdf_v0_df2   |>  
  group_by(PERMIT_NUMBER) |> 
  mutate(n = n())  |>  
  filter(n > 1) #|> view()

# remove duplicates
autef_pdf_v0_df3 <- autef_pdf_v0_df2 |> 
  select(PERMIT_NUMBER, PERMIT_YEAR, MUNICIPIO, DATUM, COORD, AREA_AUTORIZADA) |> 
  distinct(.keep_all = TRUE)
#2238  

### Get stats on auto-download data ----------------------------------------

# How many permits in PDF do not have a polygon to back-up at this point?
stats <- autef_pdf_v0_df3 |> 
  semi_join(autef_upa, by = c('PERMIT_NUMBER' = 'AUTEF')) 

# 875 UPA-Polygon can be found in this wider pool of permits, meaning we were
# missing a large number of permits
  
# 1243 is the original number of observations/polygons from the .shp, Thus, 368
# are the number of polygons which the first auto-download did not produce a
# permit to cross-check information.

# How many permits do we find listed as entering the supply chain? 

# N.B. This stats requires an object from the mapping-illegality-risk code
# "vol_by_lp" but can be skipped here.
stats <- vol_by_lp |> 
  select(LP_REF) |> 
  semi_join(autef_pdf_v0_df3, by = c('LP_REF' = 'PERMIT_NUMBER'))
# 888 out of 1292 used to substantiate volumes entering the supply chain. Thus
# 404 permits are as of now not located. We first make sure we map those that
# come from national-level jurisdiction authorizations then we proceed to
# manually search/download any permits that may be missing.

# Clean env. 
rm(autef_pdf_v0, autef_pdf_v0_df1, autef_pdf_v0_df2, autef_pdf_v0_df3, 
   stats)


## autef_pdf -------------------------------------------------------------

# Primary source of data for logging permit

# General cleaning
autef_pdf_df1 <- autef_pdf |> 
  janitor::clean_names() |>  
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  mutate(across(where(is.character), str_squish))

# General parsing and tidying of pdf data
autef_pdf_df2 <- autef_pdf_df1  |>
  mutate(AUTEF_NUM = str_replace(AUTEF_NUM, pattern = "AUTEF Nº:", replacement = "")) |> 
  mutate(AUTEF_NUM = str_squish(AUTEF_NUM)) |> 
  separate(AUTEF_NUM, into = c("PERMIT_NUMBER", "PERMIT_YEAR")) |> 
  mutate(AREA_LIQ = str_replace(AREA_LIQ, pattern = "HA", replacement = "")) |> 
  mutate(VALID_DATE = str_replace(VALID_DATE, pattern = "VALIDADE ATE:", replacement = "")) |> 
  mutate(VALID_DATE = as.Date(VALID_DATE, "%d/%m/%Y")) |> 
  mutate(AREA_LIQ = str_replace(AREA_LIQ, pattern = "\\.", replacement = "")) |> 
  mutate(AREA_LIQ = str_replace(AREA_LIQ, pattern = "\\,", replacement = "\\.")) |> 
  mutate(AREA_LIQ = as.double(AREA_LIQ))
  

### Remove duplicates -------------------------------------------------------

# Are there duplicates?
autef_pdf_df2   |>  
  group_by(PERMIT_NUMBER) |> 
  mutate(n = n())  |>  
  filter(n > 1) #|> view()

# remove duplicates
autef_pdf_df3 <- autef_pdf_df2   |> 
  distinct(.keep_all = TRUE)

# Check again whether duplicates where removed
autef_pdf_df3   |>  
  group_by(PERMIT_NUMBER) |> 
  mutate(n = n())  |>  
  filter(n > 1) |> view()
# Remaining duplicates are associated to scrapping issues. Individual check follows.



### Addressing scrapping issues ----------------------------------------------

# A few files have a different structure (e.g. either miss the lateral bar due
# to missing activation date or are LAR, "Licensa Atividade Rural" or display
# many missing fields). At this point we discard entries where issues cannot be
# determined. More details under "Addressing lp status".

# Two entries show municipality instead of permit number
autef_pdf_df3 |> filter(grepl(c("[a-zA-Z]+"), PERMIT_NUMBER))

autef_pdf_df3 <- autef_pdf_df3 |> filter(!grepl(c("[a-zA-Z]+"), PERMIT_NUMBER)) 



### Addressing coordinates ----------------------------------------------------

# Coordinates are reported in all WGS84, SAD69 annd SIRGAS200, also in DD, DMS
# and UTM. We transform all these to DD in SIRGAS200.

# Parsing and cleaning coordinates
autef_pdf_df4 <-  autef_pdf_df3 |> 
  mutate(COORD_GEO = sub("-", "_", COORD_GEO, fixed = TRUE)) |> 
  separate(COORD_GEO, into = c("DATUM", "COORD"), sep = "_") |> 
  mutate(DATUM = str_replace(DATUM, pattern = "DATUM:", replacement = "")) |> 
  mutate(DATUM = str_squish(DATUM)) |> 
  mutate(HEMISPHERE = case_when(grepl(c("HEMISFERIO: S"), COORD) ~ "SOUTH", 
                                grepl(c("HEMISFERIO: N"), COORD) ~ "NORTH",
                                TRUE ~ as.character(NA))) |> 
  mutate(COORD = str_replace(COORD, pattern = "HEMISFERIO: SUL -", replacement = "")) |> 
  mutate(ZONE = case_when(grepl(c("FUSO: 21"), COORD) ~ "21",
                          grepl(c("FUSO: 22"), COORD) ~ "22", 
                          grepl(c("FUSO: 23"), COORD) ~ "23",
                          TRUE ~ as.character(NA))) |> 
  mutate(COORD = str_replace(COORD, pattern = "FUSO: 21 -|FUSO: 22 -|FUSO: 23 -", replacement = ""))

# From coord, separate northing and easting
autef_pdf_df5 <- autef_pdf_df4 |> 
  #Longitude, X , W-E
  mutate(X = str_extract(COORD, pattern = "(E:\\s{0,2})-?,?\\d+:?,?\\d+:?,?\\d+:?,?\\d+")) |>
  mutate(X = case_when(is.na(X) ~ str_extract(COORD, pattern = "(W:\\s{0,2})-?,?\\d+:?,?\\d+:?,?\\d+:?,?\\d+"), TRUE ~ X)) |> 
  #Latitude, Y , N-S
  mutate(Y = str_extract(COORD, pattern = "(N:\\s{0,2})-?,?\\d+:?,?\\d+:?,?\\d+:?,?\\d+")) |> 
  mutate(Y = case_when(is.na(Y) ~ str_extract(COORD, pattern = "(S:\\s{0,2})-?,?\\d+:?,?\\d+:?,?\\d+:?,?\\d+"), TRUE ~ Y))


# Pass Northing and Easting to own column
autef_pdf_df6 <- autef_pdf_df5 |> 
  #address longitude, X , W-E
  mutate(WE = str_sub(X, 1, 1)) |> 
  mutate(X = str_replace(X, "E\\:|W\\:", "")) |> 
  #mutate(X = str_replace_all(X, "-", ""))
  # Temporary solution, which will not affect results: most coord fall in the S-W 
  # quadrant, meaning most have negative signs. We can remove this to address 
  # difference in reporting and rather use the NS WE indication for conversion. 
  # Also because there is a series of other errors such as inversion between S 
  # and W.
  # address latitude, Y, N-S
  # string start with N or S start 
  mutate(NS = str_sub(Y, 1, 1)) |> 
  mutate(Y = str_replace(Y, "N\\:|S\\:", ""))
  #mutate(Y = str_replace_all(Y, "-", "")) 


# N.B: Up until here we were supposed to parse the Geographic Coordinates string
# as extracted from PDF saving all information (different Datum, whether N-S or
# E-W, Hemisphere and Zone when provided). For now, I went a step ahead and
# removed the "-" sign because of the broadly all coords will be in the S-W
# quadrant. We should check more careful on all cases though. There are coords
# in SAD69, SIRGAS2000, and WGS84. There are also a mix of DD and UTM.

# Look cases in which lat (N-S, Y) could be negative before removing the "-".
# N.B.: Leaving commented out EDA below so others may be able to follow logic,
# but likewise welcome suggestions on how to address coordinate conversion with
# this number of special cases.

# # South hemisphere and no zone, any negative value?
# autef_pdf_df6 |> filter(HEMISPHERE == "SOUTH", is.na(ZONE)) |> view()
# # No negative values for those that are reported in UTM, for the remaining all
# # have negative values. As seen below, all also display N for the NS and E for
# # WE and this probably supersedes the indication of Southern Hemisphere. 
# 
# # Here one can see coordinates are invested for a large number of entries:
# # Info displayed at NS/WE 
# autef_pdf_df6 |> filter(HEMISPHERE == "SOUTH", is.na(ZONE)) |> distinct(NS)
# autef_pdf_df6 |> filter(HEMISPHERE == "SOUTH", is.na(ZONE)) |> distinct(WE)
# # All that have been reported with southern hemisphere 
# 
# # South. Hemis. UTM Coord
# autef_pdf_df6 |> filter(HEMISPHERE == "SOUTH", !is.na(ZONE)) |> view()
# # All that display southern hemisphere and zone do not have a negative signal
# # and all UTM as expected.
# 
# # Any other cases for Southern Hemisphere?
# autef_pdf_df6 |> filter(HEMISPHERE == "SOUTH") |> view()
# autef_pdf_df6 |> filter(HEMISPHERE == "SOUTH", is.na(ZONE)) |> distinct(NS)
# autef_pdf_df6 |> filter(HEMISPHERE == "SOUTH", is.na(ZONE)) |> distinct(WE)
# # All cases that spell out southern hemisphere have N and E as their NS/WE
# # reporting. Cases a bit more problematic are when NS/WE is already set as S and
# # W and yet give negative values.
# 
# # I think the correct way to deal with this is by DATUM. 
# 
# autef_pdf_df6 |> distinct(DATUM) 
# 
# autef_pdf_df6 |> group_by(DATUM) |> count()
# 
# # # A tibble: 6 × 2
# # # Groups:   DATUM [6]
# # DATUM                         n
# # <chr>                     <int>
# # 1 PORTE:                      118 # Check what does this mean in the long run, but likely limited for ipe
# # 2 SAD69                      1960
# # 3 SEM COORDENADA GEOGRAFICA     2
# # 4 SIRGAS2000                  434
# # 5 WGS84                        57
# # 6 NA                            3
# 
# # No opposite signals for any coord so they likely fall in the same SW quadrant
# autef_pdf_df6 |> filter(grepl(c("-"), Y), !grepl(c("-"), X)) |> view() 
# autef_pdf_df6 |> filter(grepl(c("-"), X), !grepl(c("-"), Y)) |> view()
# # Check details when both positive
# autef_pdf_df6 |> filter(!grepl(c("-"), X), !grepl(c("-"), Y)) |> view() 
# # Check details when both negative
# autef_pdf_df6 |> filter(grepl(c("-"), X), grepl(c("-"), Y)) |> view()



#### Obtain zone by municipality centroid ------------------------------------

# Extract municipal centroids
mun_centroid <- 
  st_centroid(mun)|>  
  mutate(X = map_dbl(geom, 1),
         Y = map_dbl(geom, 2))|>  
  as_tibble()|>  
  select(-geom)

# Obtain zone of municipal centroid
mun_zone <- mun_centroid |> 
  filter(code_state == "15") |> 
  mutate(UTM_ZONE = floor((X + 180) / 6) + 1)

# Tidy up/Normalize data for join
mun_zone <- mun_zone |>
  janitor::clean_names() |>
  rename_with(str_to_upper) |>
  mutate(across(where(is.character), toupper)) |>
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII")))

# Key for join
mun_zone_join <- mun_zone |> select(NAME_MUNI, UTM_ZONE)

# Complement missing zones
autef_pdf_df7 <- autef_pdf_df6 |> 
  left_join(mun_zone_join, by = c("MUN" = "NAME_MUNI")) |> 
  mutate(ZONE = case_when(is.na(ZONE) ~ UTM_ZONE, TRUE ~ ZONE)) |> 
  select(-UTM_ZONE)

rm(mun_zone, mun_zone_join)


#### SAD69 -------------------------------------------------------------------

# Step 1: Convert most SAD69 Coord (dms, dd)
sad69 <- autef_pdf_df7 |> 
  filter(DATUM == "SAD69") |> 
  # all in SW quadrant so remove negative values and pass SW to parzer
  mutate(X = str_replace_all(X, "-", "")) |>
  mutate(Y = str_replace_all(Y, "-", "")) |>
  #address longitude, X, W-E
  mutate(X = str_replace(X, "\\:", "°")) |> 
  mutate(X = str_replace(X, "\\:", "\\’")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |>
  mutate(WE = case_when(WE == "E" ~ "W", TRUE ~ WE)) |> 
  unite(X, X, WE, sep = " ",  na.rm = T) |> 
  # address latitude, Y, N-S
  mutate(Y = str_replace(Y, "\\:", "°")) |> 
  mutate(Y = str_replace(Y, "\\:", "\\’")) |> 
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(NS = case_when(NS == "N" ~ "S", TRUE ~ NS)) |> 
  unite(Y, Y, NS, sep = " ", na.rm = T) |> 
  # transform dms in dd, correct sign of dd
  mutate(Y = parzer::parse_lat(Y), 
         X = parzer::parse_lon(X))

sad69 |> distinct(PERMIT_NUMBER) |> count()

# NB: Could try first setting coord. to sad69 than parsing vs. parsing than
# setting coord. but this works for now so leaving suggestion here.

# Errors noticed in the way N/E are reported. Rule can be set, if X (Lon) beyond
# Para bounding box of limits, then invert X and Y because it is likely a
# reporting mistake.

# Grab bounding box for Para
pa <- states |> 
  filter(name_state == "Pará")
bbox <- st_bbox(pa)


# If higher or smaller than boundaries of Para bounding box, swap x and y
sad69_swap_lonlat <- sad69 |> 
  mutate(NEW_X = case_when((X > bbox[1] & X < bbox[3]) == TRUE ~ X, TRUE ~ Y)) |> 
  mutate(NEW_Y = case_when((X > bbox[1] & X < bbox[3]) == FALSE ~ X, TRUE ~ Y)) |> 
  select(-c(X,Y)) |> 
  rename(X = NEW_X, Y = NEW_Y)


# Set coords to sad69 CRS and transform to sirgas2000
sad69_dms_dd_to_sirgas2000 <- sad69_swap_lonlat |> 
  filter(!is.na(X)) |> # why this is needed for the moment https://github.com/r-spatial/sf/issues/1034
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs") |>  # we can transform later, but too many special cases. Best to tackle these fully and check later whether some can be generalized. 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble() |> 
  select(-geometry)

sad69_dms_dd_to_sirgas2000 |> distinct(PERMIT_NUMBER) |> count()

sad69_dms_dd_to_sirgas2000_join <- sad69_dms_dd_to_sirgas2000 |> 
  select(X, Y, PERMIT_NUMBER)


#Step 2: Convert UTM coord

# Checking the values not converted. 

# Get a vector of those permits where conversion was not possible
sad69_key <- sad69 |> filter(is.na(X) | is.na(Y)) |>  distinct(PERMIT_NUMBER) |> pull()

sad69_utm <- autef_pdf_df7 |> 
  filter(PERMIT_NUMBER %in% sad69_key) |> 
  filter(!is.na(X)) |> 
  #filter(!grepl("-", X), str_detect(X, "(?<!,)\\d\\d\\d\\d\\d\\d")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(X = as.double(X))|> 
  #filter(X > 100) |> # arbitrary 100 to separate any last dd from UTM  
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(Y = as.double(Y)) 
  

sad69_utm |> group_by(ZONE) |> count()
# Below numbers, prior to mun centroid zone add
# # A tibble: 4 × 2
# # Groups:   ZONE [4]
# ZONE      n
# <chr> <int>
# 1 21       53
# 2 22       26
# 3 23        5
# 4 NA      137


# Convert UTM SAD69

# Step 1: Convert UTM coord by zone and identify wrongly inverted lat-lon

# Information on proj4 here: 
# https://wiki.osgeo.org/wiki/Brazilian_Coordinate_Reference_Systems

sad69_utm_21 <- sad69_utm  |> 
  filter(ZONE == "21") |> 
  # Set coords to SAD69 UTM
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=21 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  # Convert to SIRGAS 2000
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_22 <- sad69_utm  |> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=22 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_23 <- sad69_utm |> 
  filter(ZONE == "23") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=23 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_trans <- bind_rows(sad69_utm_21, sad69_utm_22, sad69_utm_23) |> 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

# Correct inversion of lat-long
swap_latlon_key <- sad69_utm_trans |>  filter(X > 0) |>  pull(PERMIT_NUMBER) #TODO: use bbox in filter



# Step 2: Convert again, now with inverted coord corrected

# Swap lon-lat that are listed in the key
swap_latlon <- sad69_utm |> 
  mutate(NEW_X = case_when(PERMIT_NUMBER %in% swap_latlon_key ~ Y, TRUE ~ X)) |>
  mutate(NEW_Y = case_when(PERMIT_NUMBER %in% swap_latlon_key ~ X, TRUE ~ Y)) |> 
  select(-c(X,Y)) |> 
  rename(X = NEW_X, Y = NEW_Y)

# Convert by zone
sad69_utm_21 <- swap_latlon  |> 
  filter(ZONE == "21") |> 
  # Set coords to SAD69 UTM
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=21 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  # Convert to SIRGAS 2000
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_22 <- swap_latlon  |> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=22 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sad69_utm_23 <- swap_latlon|> 
  filter(ZONE == "23") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=23 +south +units=m +ellps=aust_SA +towgs84=-67.35,3.88,-38.22") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")


sad69_utm_to_sirgas2000 <- bind_rows(sad69_utm_21, sad69_utm_22, sad69_utm_23) |> 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

sad69_utm_to_sirgas2000_join <- sad69_utm_to_sirgas2000 |> 
  select(X, Y, PERMIT_NUMBER)

sad69_to_sirgas2000_dd <- sad69 |> 
  left_join(sad69_dms_dd_to_sirgas2000_join, by = 'PERMIT_NUMBER') |> 
  mutate(X = X.y) |> 
  mutate(Y = Y.y) |> 
  select(-c(X.x, X.y, Y.x, Y.y)) |> 
  left_join(sad69_utm_to_sirgas2000_join, by = 'PERMIT_NUMBER') |>
  mutate(X = case_when(is.na(X.x) ~ X.y, TRUE ~ X.x)) |> 
  mutate(Y = case_when(is.na(Y.x) ~ Y.y, TRUE ~ Y.x)) |> 
  select(-c(X.x, X.y, Y.x, Y.y))

sad69_to_sirgas2000_dd_plot <- sad69_to_sirgas2000_dd |> 
  filter(!is.na(X)) |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")


# Check out mapping features
tmap_mode("view")

mun_pa <- mun |> 
  filter(code_state == "15")

tm_shape(pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Black") +
  tm_shape(mun_pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Grey") +
  tm_shape(sad69_to_sirgas2000_dd_plot) +
  tm_dots()

rm(sad69, sad69_dms_dd_to_sirgas2000, sad69_dms_dd_to_sirgas2000_join, 
   sad69_key, sad69_swap_lonlat, sad69_utm, sad69_utm_21, sad69_utm_22, 
   sad69_utm_23, sad69_utm_trans, sad69_utm_to_sirgas2000_join, 
   sad69_utm_to_sirgas2000, swap_latlon_key, swap_latlon, sad69_to_sirgas2000_plot)


#### SIRGAS2000 ----------------------------------------------------------

sirgas2000 <- autef_pdf_df7 |> 
  filter(DATUM == "SIRGAS2000") |> 
  # all in SW quadrant so remove negative values and pass SW to parzer
  mutate(Y = str_replace_all(Y, "-", "")) |>
  mutate(X = str_replace_all(X, "-", "")) |> 
  #address longitude, X, W-E
  mutate(X = str_replace(X, "\\:", "°")) |> 
  mutate(X = str_replace(X, "\\:", "\\’")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(WE = case_when(WE == "E" ~ "W", TRUE ~ WE)) |> 
  unite(X, X, WE, sep = " ",  na.rm = T) |> 
  #address latitude, Y, N-S
  mutate(Y = str_replace(Y, "\\:", "°")) |> 
  mutate(Y = str_replace(Y, "\\:", "\\’")) |> 
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(NS = case_when(NS == "N" ~ "S", TRUE ~ NS)) |> 
  unite(Y, Y, NS, sep = " ", na.rm = T) |>
  mutate(Y = parzer::parse_lat(Y), 
         X = parzer::parse_lon(X)) 

sirgas2000_utm_key <- sirgas2000 |> filter(is.na(X)) |> pull(PERMIT_NUMBER)

sirgas2000_utm <- autef_pdf_df7 |> 
  filter(PERMIT_NUMBER %in% sirgas2000_utm_key) |> 
  #filter(!grepl("-", X), str_detect(X, "(?<!,)\\d\\d\\d\\d\\d\\d")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(X = as.double(X))|> 
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(Y = as.double(Y)) 

sirgas2000_utm  |> group_by(ZONE) |> count()
# # A tibble: 3 × 2
# # Groups:   ZONE [3]
# ZONE      n
# <chr> <int>
# 1 21       12
# 2 22        8
# 3 23        1

sirgas2000_utm_21 <- sirgas2000_utm |> 
  filter(ZONE == "21") |> 
  # Assign SIRGAS2000 as utm crs
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=21 +south +units=m +ellps=GRS80 +towgs84=0,0,0 +no_defs") |> 
  # Transform to SIRGAS2000 "longlat" (dd)
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sirgas2000_utm_22 <- sirgas2000_utm|> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=22 +south +units=m +ellps=GRS80 +towgs84=0,0,0 +no_defs") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sirgas2000_utm_23 <- sirgas2000_utm |> 
  filter(ZONE == "23") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=23 +south +units=m +ellps=GRS80 +towgs84=0,0,0 +no_defs") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

sirgas2000_utm_to_dd <- bind_rows(sirgas2000_utm_21, sirgas2000_utm_22, sirgas2000_utm_23) |> 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

sirgas2000_utm_to_dd_join <-  sirgas2000_utm_to_dd |> 
  select(PERMIT_NUMBER, X, Y)

sirgas2000_dd <- sirgas2000 |> 
  left_join(sirgas2000_utm_to_dd_join, by = 'PERMIT_NUMBER') |> 
  mutate(X = case_when(is.na(X.x) ~ X.y, TRUE ~ X.x)) |> 
  mutate(Y = case_when(is.na(Y.x) ~ Y.y, TRUE ~ Y.x)) |> 
  select(-c(X.x, X.y, Y.x, Y.y))
# No inverted longlat with SIRGAS2000

# Create sf obj
sirgas2000_dd_plot <- sirgas2000_dd |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")
  
# Check out mapping features
tmap_mode("view")
tm_shape(pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Black") +
  tm_shape(mun_pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Grey") +
  tm_shape(sirgas2000_dd_plot ) +
  tm_dots()

rm(sirgas2000, sirgas2000_utm_21, sirgas2000_utm_22, sirgas2000_utm_23, 
   sirgas2000_utm_key, sirgas2000_utm, sirgas2000_utm_to_dd, sirgas2000_utm_to_dd_join)


#### WGS84 ----------------------------------------------------------

# Step 1: Convert most WGS84 coord 
wgs84 <- autef_pdf_df7 |> 
  filter(DATUM == "WGS84") |> 
  # all in SW quadrant so remove negative values and pass SW to parzer
  mutate(Y = str_replace_all(Y, "-", "")) |>
  mutate(X = str_replace_all(X, "-", "")) |> 
  #address longitude, X, W-E
  mutate(X = str_replace(X, "\\:", "°")) |> 
  mutate(X = str_replace(X, "\\:", "\\’")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(WE = case_when(WE == "E" ~ "W", TRUE ~ WE)) |> 
  unite(X, X, WE, sep = " ",  na.rm = T) |> 
  #address latitude, Y, N-S
  mutate(Y = str_replace(Y, "\\:", "°")) |> 
  mutate(Y = str_replace(Y, "\\:", "\\’")) |> 
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(NS = case_when(NS == "N" ~ "S", TRUE ~ NS)) |> 
  unite(Y, Y, NS, sep = " ", na.rm = T) |> 
  mutate(Y = parzer::parse_lat(Y), 
         X = parzer::parse_lon(X)) 

# Correct for inverted coord
wgs84_swap_lonlat <- wgs84 |> 
  mutate(NEW_X = case_when((X > bbox[1] & X < bbox[3]) == TRUE ~ X, TRUE ~ Y)) |> 
  mutate(NEW_Y = case_when((X > bbox[1] & X < bbox[3]) == FALSE ~ X, TRUE ~ Y)) |> 
  select(-c(X,Y)) |> 
  rename(X = NEW_X, Y = NEW_Y)

# Set coords to wgs84 and transform to sirgas2000
wgs84_dms_to_sirgas2000 <- wgs84_swap_lonlat |> 
  filter(!is.na(X)) |> # why this is needed for the moment https://github.com/r-spatial/sf/issues/1034
  # Set dd coord to WGS84
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +datum=WGS84") |> 
  # Transform from WGS*$ to SIRGAS200
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs") |>  
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble() |> 
  select(-geometry)


wgs84_dms_to_sirgas2000_join <- wgs84_dms_to_sirgas2000 |> 
  select(X, Y, PERMIT_NUMBER)


# Step 2: Convert utm WGS84 coord 

# Get a vector of those permits where conversion was not possible
wgs84_key <- wgs84 |> filter(is.na(X)) |>  distinct(PERMIT_NUMBER) |> pull()


wgs84_utm <- autef_pdf_df7 |> 
  filter(PERMIT_NUMBER %in% wgs84_key) |> 
  filter(!is.na(X)) |> 
  #filter(!grepl("-", X), str_detect(X, "(?<!,)\\d\\d\\d\\d\\d\\d")) |> 
  mutate(X = str_replace(X, "\\,", "\\.")) |> 
  mutate(X = as.double(X))|> 
  #filter(X > 100) |> # arbitrary 100 to separate any last dd from UTM  
  mutate(Y = str_replace(Y, "\\,", "\\.")) |> 
  mutate(Y = as.double(Y)) 


# Convert UTM WGS84

# Step 1: Convert UTM coord by zone and identify wrongly inverted lat-lon

wgs84_utm_21 <- wgs84_utm |> 
  filter(ZONE == "21") |> 
  # Set coords to SAD69 UTM
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=21 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  # Convert to SIRGAS 2000
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

wgs84_utm_22 <- wgs84_utm  |> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=22 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

# wgs84_utm_23 <- wgs84_utm|> 
#   filter(ZONE == "23") |> 
#   st_as_sf(coords = c('X', 'Y'), crs="+proj=utm +zone=23 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
#   st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

wgs84_utm_trans <- bind_rows(wgs84_utm_21, wgs84_utm_22) |> # add if additional permits display this wgs84_utm_23 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

# Correct inversion of lat-long
swap_latlon_key <- wgs84_utm_trans |>  filter(X > 0) |>  pull(PERMIT_NUMBER) #TODO: use bbox in filter


# Step 2: Convert again, now with inverted coord corrected

# Swap lon-lat that are listed in the key
swap_latlon <- wgs84_utm |> 
  mutate(NEW_X = case_when(PERMIT_NUMBER %in% swap_latlon_key ~ Y, TRUE ~ X)) |>
  mutate(NEW_Y = case_when(PERMIT_NUMBER %in% swap_latlon_key ~ X, TRUE ~ Y)) |> 
  select(-c(X,Y)) |> 
  rename(X = NEW_X, Y = NEW_Y)

# Convert by zone
wgs84_utm_21 <- swap_latlon  |> 
  filter(ZONE == "21") |> 
  # Set coords to SAD69 UTM
  st_as_sf(coords = c('X', 'Y'),  crs="+proj=utm +zone=21 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  # Convert to SIRGAS 2000
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")

wgs84_utm_22 <- swap_latlon  |> 
  filter(ZONE == "22") |> 
  st_as_sf(coords = c('X', 'Y'),  crs="+proj=utm +zone=22 +south +units=m +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  st_transform(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")


wgs84_utm_to_sirgas2000 <- bind_rows(wgs84_utm_21, wgs84_utm_22) |> 
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))|>  
  as_tibble()|>  
  select(-geometry)

wgs84_utm_to_sirgas2000_join <- wgs84_utm_to_sirgas2000 |> 
  select(X, Y, PERMIT_NUMBER)


wgs84_to_sirgas2000_dd <- wgs84 |> 
  left_join(wgs84_dms_to_sirgas2000_join, by = 'PERMIT_NUMBER') |> 
  mutate(X = X.y) |> 
  mutate(Y = Y.y) |> 
  select(-c(X.x, X.y, Y.x, Y.y)) |> 
  left_join(wgs84_utm_to_sirgas2000_join, by = 'PERMIT_NUMBER') |>
  mutate(X = case_when(is.na(X.x) ~ X.y, TRUE ~ X.x)) |> 
  mutate(Y = case_when(is.na(Y.x) ~ Y.y, TRUE ~ Y.x)) |> 
  select(-c(X.x, X.y, Y.x, Y.y))

wgs84_to_sirgas2000_dd_plot <- wgs84_to_sirgas2000_dd |> 
  filter(!is.na(X)) |> 
  st_as_sf(coords = c('X', 'Y'), crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs")


# Check out mapping features
tmap_mode("view")
tm_shape(pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Black") +
  tm_shape(mun_pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Grey") +
  tm_shape(wgs84_to_sirgas2000_dd_plot) +
  tm_dots()

rm(wgs84, wgs84_dms_to_sirgas2000, wgs84_dms_to_sirgas2000_join, wgs84_key, wgs84_swap_lonlat, 
   wgs84_utm, wgs84_utm_21, wgs84_utm_22, wgs84_utm_to_sirgas2000, wgs84_utm_trans, 
   wgs84_utm_to_sirgas2000_join, bbox, swap_latlon_key, swap_latlon)


#### Binding and Plotting  ---------------------------------------------------------------

# Bring all coord together
lp_coord <- bind_rows(sad69_to_sirgas2000_dd, wgs84_to_sirgas2000_dd, sirgas2000_dd)

lp_coord_key <- lp_coord |> distinct(PERMIT_NUMBER) |> pull()

lp_unplotable <- autef_pdf_df7 |> 
  mutate(X = as.double(X)) |> 
  mutate(Y = as.double(Y)) |> 
  filter(!PERMIT_NUMBER %in% lp_coord_key)

# Add back data lost from missing coordinates
autef_pdf_df8 <- bind_rows(lp_coord, lp_unplotable) |> 
  select(-c(COORD, HEMISPHERE, ZONE, WE, NS, DATUM))


# Bind all plotable lp's
autef_pdf_plotable <- bind_rows(sad69_to_sirgas2000_dd_plot, wgs84_to_sirgas2000_dd_plot, sirgas2000_dd_plot)

# Check out mapping features
tmap_mode("view")
tm_shape(pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Black") +
  tm_shape(mun_pa) +
  tm_basemap("Stamen.TonerLite") +
  tm_polygons(alpha = 0,  border.col = "Grey") +
  tm_shape(autef_pdf_plotable) +
  tm_dots()

rm(sad69_to_sirgas2000_dd, wgs84_to_sirgas2000_dd, sirgas2000_dd, 
   sad69_to_sirgas2000_dd_plot, wgs84_to_sirgas2000_dd_plot, sirgas2000_dd_plot, 
   lp_coord, lp_unplotable, lp_coord_key)



### Addressing lp status  ----------------------------------------------------

# Parsing permit status 
autef_pdf_df9 <- autef_pdf_df8 |> 
  mutate(EXPIRATION_DATE = str_sub(AUTEF_STATUS, -10, -1)) |> 
  mutate(EXPIRATION_DATE = as.Date(EXPIRATION_DATE, "%d/%m/%Y")) |>  #Thus, those without expiration date/missing lateral bar may be still active 
  mutate(LP_STATUS = case_when(grepl(c("TITULO VENCIDO EM:"), AUTEF_STATUS) ~ "EXPIRED", 
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CONCLUIDO POR CUMPRIMENTO" ~ "EXPIRED", 
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CONCLUIDO POR VENCIMENTO" ~ "EXPIRED",
                               grepl(c("ATIVO : VENCIMENTO ESTENDIDO ATE"), AUTEF_STATUS) ~ "EXTENDED_CANCELLED SUSPENSION",
                               grepl(c("PRORROGADO"), AUTEF_STATUS) ~ "EXTENDED",
                               AUTEF_STATUS == "SUSPENSO" ~ "SUSPENDED",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO" ~ "CANCELLED",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR OUTROS" ~ "CANCELLED_OTHERS",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR ACAO CIVEL PUBLICA" ~ "CANCELLED_CIVIL LAWSUIT",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR FALHA NA ELABORACAO" ~ "CANCELLED_ELABORATION FAILURE",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR SUBSTITUICAO" ~ "CANCELLED_PERMIT SUBSTITUTION",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CONSTATACAO DE ILEGALIDADE" ~ "CANCELLED_ILLEGALITY",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: CANCELADO POR NAO CUMPRIMENTO DAS CONDICIONANTES" ~ "CANCELLED_NONCOMPLIENCE WITH CONDITIONS",
                               AUTEF_STATUS == "MOTIVO DE CANCELAMENTO: EM EXECUCAO JUDICIAL" ~ "CANCELLED_JUDICIAL PROCESS",
                               PERMIT_NUMBER %in% c("1922", "1974", "20134", "20138",  "2084",   "2263",   "2304",   "2333",   "2364",
                                                   "2375",   "2388",   "2391",   "2435",   "2480",   "2504",   "27825",  "27826",  "27838",
                                                   "2944", "899",    "27493",  "11",     "1760",   "2069",   "2090",   "2093",   "2332",
                                                   "27822",  "27927",  "31") ~ "MISSING ACTIVATION DATE", 
                               TRUE ~ "UNDETERMINED")) |> 
  select(-AUTEF_STATUS)

# Data check
autef_pdf_df9 |> distinct(LP_STATUS) |> arrange(LP_STATUS)

# Investigate why some pdfs have other info instead of status. 
# autef_pdf_df9 |> distinct(AUTEF_STATUS) |> arrange(AUTEF_STATUS)
# 
# autef_pdf_df9 |> filter(AUTEF_STATUS %in% c("HISTORICO DA TRAMITACAO DE PROCESSO",   
#                                          "RECIBO DE PROTOCOLO",                 
#                                          "RESPONSAVEL TECNICO:")) |> view()
# 
# autef_pdf_df9 |> filter(AUTEF_STATUS %in% c("HISTORICO DA TRAMITACAO DE PROCESSO",   
#                                             "RECIBO DE PROTOCOLO",                 
#                                             "RESPONSAVEL TECNICO:")) |> pull(PERMIT_NUMBER)
# 
# autef_pdf_df9 |> filter(AUTEF_STATUS %in% c("HISTORICO DA TRAMITACAO DE PROCESSO",
#                                            "RECIBO DE PROTOCOLO",
#                                            "RESPONSAVEL TECNICO:")) |>
#   distinct(PERMIT_NUMBER) |>
#   semi_join(vol_by_lp, by = c('PERMIT_NUMBER' = 'LP_REF')) # Only number 31 was used for ipe.


# Visual check for those we have the permit number.

# 1) All of the following have a missing activation date, thus a missing valid
# date, thus missing status:

# "1922"   "1974"   "20134"  "20138"  "2084"   "2263"   "2304"   "2333"   "2364"
# "2375"   "2388"   "2391"   "2435"   "2480"   "2504"   "27825"  "27826"  "27838"
# "2944" "899"    "27493"  "11"     "1760"   "2069"   "2090"   "2093"   "2332"
# "27822"  "27927"  "31"

# 2) Case of missing status because still valid/not expired.
#273613: Issues in 2020, valid until 2022
#273807: Issued data not specied but valid until 2023



# Removing entries where permit is.na(), expanding the analysis to other species
# and years will need a more careful look at these. In practice this means
# permits that have not match in the DOF/GF will be classified as "UNDETERMINED"
# or broadly missing. From seeing this data it could be the LAR only.

autef_pdf_df10 <- autef_pdf_df9 |> 
  filter(!is.na(PERMIT_NUMBER)) |> 
  mutate(LP_STATUS = case_when(LP_STATUS %in% c("HISTORICO DA TRAMITACAO DE PROCESSO",   
                                                "RECIBO DE PROTOCOLO",                 
                                                "RESPONSAVEL TECNICO:") ~ as.character(NA), 
                               TRUE ~ LP_STATUS))

autef_pdf_df10 |> distinct(LP_STATUS) |> arrange(LP_STATUS)



### Saving  ---------------------------------------------------------------

#Recycling object
autef_pdf <- autef_pdf_df10 |> 
  # add type here as we rbind with correct data
  mutate(TYPE = "AUTEF") 

# Clean env.
rm(autef_pdf_df1, autef_pdf_df2, autef_pdf_df3, autef_pdf_df4, autef_pdf_df5,
   autef_pdf_df6, autef_pdf_df7, autef_pdf_df8, autef_pdf_df9, autef_pdf_df10, 
   autef_pdf_plotable)


### Stats  -----------------------------------------------------------------

stats <- vol_by_lp |> 
  select(LP_REF) |> 
  semi_join(autef_pdf, by = c('LP_REF' = 'PERMIT_NUMBER'))
# 1215 out of 1292 permits (which are listed as having volume entering the supply
# chain) can now be used to validate such entries. The remaining are AUTEX and
# missing permits.

# N.B. This stats requires an object from the mapping-illegality-risk code
# "vol_by_lp" but can be skipped here.

rm(stats)

## autex -------------------------------------------------------------------

# General cleaning 
# Rename columns with EN, remove all special characters, set all to upper case.
autex_df1 <-  autorizacao_pa |> 
  dplyr::rename(PERMIT_HOLDER = nome_razao_social_do_detentor, 
                CPF_CNPJ_PERMIT_HOLDER = cpf_cnpj_do_detentor, 
                CTF_PERMIT_HOLDER = ctf_do_detentor, 
                NAME_ORIGIN = nome_da_origem, 
                UF = uf, 
                MUNICIPALITY = municipio, 
                LATITUDE = latitude, 
                LONGITUDE = longitude, 
                AREA_HA = area_ha,
                AUTEX_TYPE = tipo_de_autex,
                NUM_AUTEX_SERIE = numero_de_serie_da_autex, 
                NUM_ORIGINAL_PERMIT = no_da_autorizacao_original, 
                ISSUING_BODY_AUTEX = orgao_emissor_da_autorizacao, 
                REGISTRATION_DATE_AUTEX = data_de_cadastro_da_autex, 
                RELEASE_DATE_AUTEX = data_de_liberacao_da_autex,
                VALID_DATE_AUTEX = data_de_validade_da_autex, 
                YEAR = ano, 
                CURRENT_STATUS = situacao_atual, 
                PRODUCT_TYPE = tipo_de_produto, 
                SCIENTIFIC_NAME = nome_cientifico, 
                COMMON_NAME = nome_popular, 
                UNIT = unidade_de_medida, 
                VOLUME_AUTHORIZED = volume_original_autorizado,
                VOLUME_REMAINING = volume_remanescente,
                LAST_REPORT_UPDATE = ultima_atualizacao_relatorio) |> 
  mutate(across(where(is.character), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  mutate(across(where(is.character), toupper))

# Remove punctuation from CPF_CNPJ (except for ***, which used for anonymization
# of CPFs)
autex_df2 <- autex_df1 |> 
  mutate(across(CPF_CNPJ_PERMIT_HOLDER, ~ str_remove_all(.,"[\\.\\/\\-]")))

# All autex have the same area, latitude(Y) and longitude (X)
autex_df2 |> distinct(NUM_AUTEX_SERIE) |> count()
autex_df2 |> distinct(NUM_AUTEX_SERIE, AREA_HA) |> count() 
autex_df2 |> distinct(NUM_AUTEX_SERIE, AREA_HA, LATITUDE, LONGITUDE) |> count() 

# Extent of missing coord
autex_df2 |> filter(is.na(LATITUDE)) 
autex_df2 |> filter(is.na(LONGITUDE)) 

### Remove duplicates -------------------------------------------------------

# We need unique permits from here
autex_df3 <- autex_df2 |> 
  distinct(NUM_AUTEX_SERIE, NUM_ORIGINAL_PERMIT, AREA_HA, LATITUDE, LONGITUDE,
           PERMIT_HOLDER, CPF_CNPJ_PERMIT_HOLDER, CTF_PERMIT_HOLDER, NAME_ORIGIN, MUNICIPALITY, 
           AUTEX_TYPE, CURRENT_STATUS)

#NB. One can add more variables here, e.g.: 
# [1] "PERMIT_HOLDER"           "CPF_CNPJ_PERMIT_HOLDER"  "CTF_PERMIT_HOLDER"       "NAME_ORIGIN"             "UF"                     
# [6] "MUNICIPALITY"            "LATITUDE"                "LONGITUDE"               "AREA_HA"                 "AUTEX_TYPE"             
# [11] "NUM_AUTEX_SERIE"         "NUM_ORIGINAL_PERMIT"     "ISSUING_BODY_AUTEX"      "REGISTRATION_DATE_AUTEX" "RELEASE_DATE_AUTEX"     
# [16] "VALID_DATE_AUTEX"        "YEAR"                    "CURRENT_STATUS"          "PRODUCT_TYPE"            "SCIENTIFIC_NAME"        
# [21] "COMMON_NAME"             "UNIT"                    "VOLUME_AUTHORIZED"       "VOLUME_REMAINING"        "LAST_REPORT_UPDATE"   

### Fix missing coordinates --------------------------------------------------
# Add approximate coordinate for this specific case
autex_df3  <- autex_df3 |> 
  mutate(LONGITUDE = case_when(is.na(LONGITUDE) ~ -56.483056, TRUE ~ LONGITUDE))

### Addressing missing area --------------------------------------------------
# # Extent of missing area or area set to zero
# autex_df3 |> filter(is.na(AREA_HA))  |> view()
# autex_df3 |> filter(AREA_HA == 0) |> view()

# Patterns of missing area are related to type of AUTEX so we will not discard
# these.
autex_df3 |> filter(is.na(AREA_HA)) |> 
  group_by(PERMIT_HOLDER, CURRENT_STATUS) |> 
  count()
autex_df3 |> filter(AREA_HA == 0) |> 
  group_by(PERMIT_HOLDER, CURRENT_STATUS) |> 
  count()



### Addressing type permit ---------------------------------------------------

# Types of AUTEX listed
autex_df3 |> distinct(AUTEX_TYPE)

# # A tibble: 5 × 1
# AUTEX_TYPE                                                           
# <chr>                                                                
# 1 AUTEX-SUPRESSAO DE VEGETACAO EM LICENCIAMENTO AMBIENTAL-MATERIA PRIMA
# 2 AUTEX-SUPRESSAO DE VEGETACAO EM LICENCIAMENTO AMBIENTAL              
# 3 AUTEX-EXPLORACAO EM PLANOS DE MANEJO                                 
# 4 AUTEX-EXPLORACAO EM PLANOS DE MANEJO-MATERIA PRIMA                   
# 5 AUTEX-EXPLORACAO DE FLORESTA PLANTADA-MATERIA PRIMA 

autex_df4 <- autex_df3 |> 
  mutate(TYPE = case_when(AUTEX_TYPE == "AUTEX-SUPRESSAO DE VEGETACAO EM LICENCIAMENTO AMBIENTAL-MATERIA PRIMA" ~ "AUTEX_SV",
                          AUTEX_TYPE == "AUTEX-SUPRESSAO DE VEGETACAO EM LICENCIAMENTO AMBIENTAL" ~ "AUTEX_SV", 
                          AUTEX_TYPE == "AUTEX-EXPLORACAO EM PLANOS DE MANEJO" ~ "AUTEX_PMFS",
                          AUTEX_TYPE == "AUTEX-EXPLORACAO EM PLANOS DE MANEJO-MATERIA PRIMA" ~ "AUTEX_PMFS",
                          AUTEX_TYPE == "AUTEX-EXPLORACAO DE FLORESTA PLANTADA-MATERIA PRIMA" ~ "AUTEX_EFP",
                          TRUE ~ AUTEX_TYPE)) 


### Saving output --------------------------------------------------

# Reassign object
autex_pa <- autex_df4 |> 
  mutate(AREA_HA = as.numeric(AREA_HA))

# Clean env. 
rm(autorizacao_pa, autex_df1, autex_df2, autex_df3, autex_df4, join_autex_pa)



## autef_brancalion --------------------------------------------------------

# General cleaning
autef_brancalion_df1 <- autef_brancalion |> 
  janitor::clean_names() |>   
  rename_with(str_to_upper) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(everything(), ~stringi::stri_trans_general(., "Latin-ASCII"))) |> 
  separate(AUTEF, into = c("PERMIT_NUMBER", "PERMIT_YEAR"))|>  
  mutate(PERMIT_NUMBER = str_remove(PERMIT_NUMBER, "^0+"))
  
### Remove duplicates --------------------------------------------------------

# Ensure these are unique
autef_brancalion_df2 <- autef_brancalion_df1 |> distinct(PERMIT_NUMBER, AREA_L) |> 
  #discarding error 
  filter(!grepl('JAN|FEV|JUN|JUL|SET|OUT|AGO', PERMIT_NUMBER)) |> 
  mutate(AREA_L = str_replace_all(AREA_L, pattern = "\\,", replacement = "\\.")) |> 
  mutate(AREA_L = as.double(AREA_L))


### Addressing type of permit ------------------------------------------------

# As in with the upa and pdf, we know data from Brancalion et al 2018 refers to
# AUTEFs, thus we simply set to this type.
autef_brancalion_df3 <- autef_brancalion_df2 |> 
  mutate(TYPE = "AUTEF")

### Saving output ------------------------------------------------------------
autef_brancalion <- autef_brancalion_df3

# Clean env. 
rm(autef_brancalion_df1, autef_brancalion_df2, autef_brancalion_df3)



## credit (origin) type -------------------------------------------------------------

# This is dataset provided through IBAMA (2019) study for 2012-2017. It provides
# type of permit per permit number and can be used to corroborate/complement
# understanding of other datasets used here. It is worth noting this data is not
# systematically incorporated into the logging permit compilation because it
# does not cover the entire period of time.

# General tidying
autef_type_credit_df1 <- autef_type_credit |>  
  rename(PERMIT = N_AUTORIZAÇÃO, PROCESS = N_PROCESSO, TYPE_CREDIT = TIPO_CRÉDITO)|>  
  separate(PROCESS, into = c("PROCESS_YEAR", "PROCESS_NUMBER"))|>  
  separate(PERMIT, into = c("PERMIT_NUMBER", "PERMIT_YEAR"))|>  
  mutate(PROCESS_NUMBER = str_remove(PROCESS_NUMBER, "^0+"))%>% 
  mutate(PERMIT_NUMBER = str_remove(PERMIT_NUMBER, "^0+")) |> 
  select(-"...1") |> 
  distinct()

# Check for permits that have been classified in more than one type
autef_type_credit_df1 |> group_by(PERMIT_NUMBER) |> mutate(n = n()) |> filter(n > 1) |> view()

# N.B. Most duplicates are between AUTEF and EFPP, which are both issued under
# PMFS. EFPP, nonetheless, refers to "Exploração Florestal em Pequenas
# Propriedades" (Forest Exploration in Small Properties). So it is still a PMF,
# but a special case. AUAS on the other hand stands for "Áreas de Uso
# Alternativo do Solo" or understood as legal deforestation so permits under
# this type would be state-authorized deforestation. 

# Entries to exclude (in case permits more than one type AUTEF-EFPP, just use AUTEF for
# simplicity)
autef_type_credit_df2 <- autef_type_credit_df1 |> 
  group_by(PERMIT_NUMBER) |> 
  mutate(n = n()) |> 
  filter(n > 1, !grepl("AUTEF", TYPE_CREDIT)) 

# Remove permits that have a duplicate number 
autef_type_credit_df3 <- autef_type_credit_df1 |> anti_join(autef_type_credit_df2)

# N.B. Keep in mind permit 99 is a AUAS as well as AUTEF. 

# Reassigning object
autef_type_credit  <- autef_type_credit_df3

autef_type_credit |> distinct() 

# Clean env. 
rm(autef_type_credit_df1, autef_type_credit_df2, autef_type_credit_df3)
  

# Cross-validate/triangulate sources (all permits) ---------------------------

# Get all unique autef numbers from the pdf scrapping file
permit_num_pdf <- autef_pdf |> 
  distinct(PERMIT_NUMBER) |> 
  rename(LP_NUM = PERMIT_NUMBER) |> 
  mutate(LP_SOURCE = "AUTEF_PDF")

# Get all unique autef numbers form the upa polygon file
permit_num_upa <- autef_upa |> distinct(AUTEF) |> 
  rename(LP_NUM = AUTEF) |> 
  mutate(LP_SOURCE = "AUTEF_UPA")

# Get all unique autex numbers from the dof authorization module
permit_num_autex <- autex_pa |> distinct(NUM_AUTEX_SERIE) |> 
  rename(LP_NUM = NUM_AUTEX_SERIE) |> 
  mutate(LP_SOURCE = "AUTEX")

# Get all unique original permits listed alongside autex numbers from the dof
# authorization module
permit_num_autex_original_lp <- autex_pa |> distinct(NUM_ORIGINAL_PERMIT) |> 
  separate(NUM_ORIGINAL_PERMIT, into = c("LP_NUM", "YEAR"), sep = "/")|>  
  select(-YEAR) |> 
  mutate(LP_NUM = str_remove(LP_NUM, "^0+")) |> 
  mutate(LP_SOURCE = "AUTEX_ORIGINAL_LP") |> 
  filter(!is.na(LP_NUM)) |> 
  distinct(LP_NUM, LP_SOURCE)

# Get all unique permits listed in Brancalion's file
permit_num_autef_brancalion <- autef_brancalion |> 
  distinct(PERMIT_NUMBER) |> 
  rename(LP_NUM = PERMIT_NUMBER) |> 
  mutate(LP_SOURCE = "AUTEF_BRANCALION")

# Get all unique permits listed in permit credit type file
permit_num_type_credit <- autef_type_credit |> 
  distinct(PERMIT_NUMBER) |> 
  rename(LP_NUM = PERMIT_NUMBER) |> 
  mutate(LP_SOURCE = "AUTEF_TYPE_CREDIT")


# Bring all unique logging permits together
lp_df1 <- bind_rows(permit_num_upa, 
                    permit_num_pdf, 
                    permit_num_autex, 
                    permit_num_autex_original_lp, 
                    permit_num_autef_brancalion,
                    permit_num_type_credit)

# Create a string variable that stores across which data sources unique permits show up
lp_df2 <- pivot_wider(lp_df1, 
                      names_from = LP_SOURCE, 
                      values_from = LP_SOURCE, 
                      id_cols = LP_NUM)

lp_df2 |> distinct(LP_NUM) |> count() 


# All unique logging permits with information on where these appears, whether in
# autef polygons, the pdfs, in the autex, displayed as origin permit of the
# autex or in the credit type data. 
key_lp <- lp_df2 |> 
  unite(LP_SOURCE, AUTEF_UPA, AUTEF_PDF, AUTEX, AUTEX_ORIGINAL_LP, AUTEF_BRANCALION, 
        AUTEF_TYPE_CREDIT, sep = "_")

# Summary on number of permits from different data sources. Note those permits
# without NA have not been discarded.
key_lp |> distinct(LP_NUM) |> count() 

# visual check
key_lp |> group_by(LP_SOURCE) |> count()  |> view()

# Clean env.
rm(lp_df1, lp_df2, permit_num_upa, permit_num_pdf, permit_num_autex, 
   permit_num_autex_original_lp, permit_num_autef_brancalion, 
   permit_num_type_credit)



## Stats on logging permits sources for the case of ipe -------------------

# At last, we mapped 3337 unique logging permit that can be used to potentially
# validate timber entering the supply chain. Comprehensive information (e.g.
# original authorization/pdf) was not, however, located for all of these. We
# focus on ipe so the final logging permit compilation/dataset it is still not
# comprehensive for all species. This compilation, however, provides
# comprehensive information for 1) 2571 state-level AUTEF PDFs and 1) 193
# federal-level AUTEX. A total of 2764 logging permits.

#More specifically, permits that are used to substantiate timber transport used
#are as per below stats: 

#1) State-level/AUTEF PDFs 
stats_autef <- vol_by_lp |> 
  select(LP_REF) |> 
  semi_join(autef_pdf, by = c('LP_REF' = 'PERMIT_NUMBER'))
# 1215 out of 1292 permits (which are listed as having volume entering the supply
# chain) can be used to validate volume credits.

#2) Federal-level/AUTEX database (module "autorizacao")
stats_autex <- vol_by_lp |> 
  select(LP_REF) |> 
  semi_join(autex_pa, by = c('LP_REF' = 'NUM_AUTEX_SERIE'))
# 47 out of 1292 permits at federal jurisdiction level can be used to substantiate
# logging permits.

# Thus, 1262 is the pool of data we draw from for ipe. The remaining 30 are the
# undetermined.



# Build the logging permits dataset --------------------------------------------

# Here we select what variables are of relevance for the study, but others can
# be added. We start with all unique logging permits found and the string
# providing evidence/mapping the source of information. The minimum data we need
# is permit number, permit year, municipality, coordinates/geometry, area
# authorized, status of permits, and type of permit so the resulting dataset
# logging permits dataset focus on these. 


# Starting from unique logging permit, add information from AUTEF-PDFs
key_lp_df1 <- key_lp |> 
  # add info from autef_pdf
  left_join(autef_pdf, by = c("LP_NUM" = "PERMIT_NUMBER")) |> 
  # add info from autef_upa
  left_join(autef_upa, by = c("LP_NUM" = "AUTEF")) |>
  #remove those variables not relevant for the analysis
  select(-c(ID, HOLDER, PROPERTY, PROCESS, PROPERTY_HA, UPA_HA, VOLUME_M3)) 
  
# Step-wise integration of overlapping variables. N.B.: The AUTEF-PDFs are
# considered the baseline data but autef_upa are used to capture perimeter but
# can be also useful to identifying potential differences between these sources
# of data.
key_lp_df2 <- key_lp_df1 |> 
  # if missing coord, add from geom centroid. N.B. there were instances where
  # scrapping did not correctly grab the coordinate or that "missing coord" is
  # indicated.
  mutate(X = case_when(is.na(X.x) ~ X.y, TRUE ~ X.x)) |> 
  mutate(Y = case_when(is.na(Y.x) ~ Y.y, TRUE ~ Y.x)) |> 
  # Correcting coord error (to be corrected at PDF-level cleaning in a future opportunity)
  mutate(Y = case_when(LP_NUM == "1069" ~ Y.y, TRUE ~ Y.x)) |> 
  # if municipality is missing, add from autef_upa
  mutate(MUN = case_when(MUN != MUNICIPALITY ~ MUN, #Just a reminder some are not the same 
                         is.na(MUN) ~ MUNICIPALITY, 
                         TRUE ~ MUN)) |> 
  # if area is missing, add from autef_upa
  mutate(AREA_LIQ = case_when(AREA_LIQ != AREALIQ_HA ~ AREA_LIQ, #Permits checked, when differing mostly hand-typing and rounding discrepancies
                              is.na(AREA_LIQ) ~ AREALIQ_HA, 
                              TRUE ~ AREA_LIQ)) |>
  # add missing type indication. Here mostly AUTEFs, but if patterns change for
  # other species one will want more detail here.
  mutate(TYPE = case_when(is.na(TYPE.x) ~ TYPE.y, TRUE ~ TYPE.x)) |> 
  # if valid_date missing, add from autef_upa
  mutate(VALID_DATE = case_when(VALID_DATE != VALID_AUTEF ~ VALID_DATE, #Just a reminder some are not the same 
                                is.na(VALID_DATE) ~ VALID_AUTEF, 
                                TRUE ~ VALID_DATE)) |> 
  #if year is missing, add from autef_upa
  mutate(PERMIT_YEAR = case_when(PERMIT_YEAR != YEAR_AUTEF ~ PERMIT_YEAR, #Just a reminder some are not the same 
                                is.na(PERMIT_YEAR) ~ YEAR_AUTEF, 
                                TRUE ~ PERMIT_YEAR)) |> 
  # tidy up 
  select(-c(X.x, X.y, Y.x, Y.y, MUNICIPALITY, TYPE.x, TYPE.y, AREALIQ_HA, 
            VALID_AUTEF, YEAR_AUTEF)) |> 
  rename(MUNICIPALITY = MUN, AREA_HA = AREA_LIQ)


  
# Add autex data 
# Wrangle to add autex data
join_autex_pa <- autex_pa |> select(NUM_AUTEX_SERIE, LATITUDE, LONGITUDE, 
                                    MUNICIPALITY, AREA_HA, TYPE, CURRENT_STATUS)
# Add autex data 
key_lp_df3 <- key_lp_df2 |> 
  # add info from autex 
  left_join(join_autex_pa, by = c("LP_NUM" = "NUM_AUTEX_SERIE")) |> 
  mutate(X = case_when(is.na(X) ~ LONGITUDE, TRUE ~ X)) |> 
  mutate(Y = case_when(is.na(Y) ~ LATITUDE, TRUE ~ Y)) |> 
  mutate(MUNICIPALITY = case_when(!is.na(MUNICIPALITY.y) ~ MUNICIPALITY.y, TRUE ~ MUNICIPALITY.x)) |> 
  mutate(AREA_HA = case_when(is.na(AREA_HA.x) ~ AREA_HA.y, TRUE ~ AREA_HA.x)) |>  # N.B. It is not clear whether this area can be equated to the area liq. from AUTEF, but we are using it for the analysis.
  mutate(LP_STATUS = case_when(is.na(LP_STATUS) ~ CURRENT_STATUS, TRUE ~ LP_STATUS)) |>
  mutate(TYPE = case_when(is.na(TYPE.x) ~ TYPE.y, TRUE ~ TYPE.x)) |>
  select(-c(LATITUDE, LONGITUDE, MUNICIPALITY.x, MUNICIPALITY.y, TYPE.x, 
            TYPE.y, AREA_HA.x, AREA_HA.y, CURRENT_STATUS, GEOCODE))

# N.B. At this moment we treat AUTEX data as a standalone category, not
# distinguishing between status reported in this dataset (i.e. EMITIDA OFERTA,
# ESTORNADO ITEM, etc.). We found most timber entering the supply chain from
# AUTEX would be connected to "ESTORNADO ITEM", which is a category when credits
# are issued then credited back e.g. because a shipment was not sent, returned
# etc. This is at odds with the "RECEIVED" cargo from which we derive flows.
# Thus, given ambiguity and lack of metadata to clarify standing of this share
# we do not evaluate status. 



# Add GEOCMUN variable

# Distinct municipalities in the autef_upa data
distinct_mun <- key_lp_df3 |> 
  distinct(MUNICIPALITY) 

# Departing from mun, build UF_MUN and retain geocode
geocmun <- mun |>  
  select(code_muni, name_muni) |>   
  as_tibble() |>  
  select(-geom) |>  
  filter(grepl(c("^15"), code_muni)) |> 
  mutate(across(where(is.character), toupper)) |> 
  #remove special characters to match 
  mutate(across(c(name_muni), ~stringi::stri_trans_general(., "Latin-ASCII"))) 

join_mun_geoc <- distinct_mun |>  
  left_join(geocmun, by = c("MUNICIPALITY" = "name_muni")) |> 
  rename(GEOCODE = code_muni)

join_mun_geoc |> filter(is.na(GEOCODE)) 

# # A tibble: 2 × 2
# MUNICIPALITY GEOCODE
# <chr>          <dbl>
# 1 PAU D ARCO        NA
# 2 NA                NA

# Add geocode to lp data 
key_lp_df4 <- key_lp_df3 |> 
  left_join(join_mun_geoc, by = "MUNICIPALITY") |> 
  mutate(GEOCODE = case_when(MUNICIPALITY == "PAU D ARCO" ~ 1505551, 
                             TRUE ~ GEOCODE)) 



# Logging permit dataset 

# N.B. Refinement of scraping and further cleaning iterations will increase the
# robustness of the dataset as inconsistencies likely remain. Still, we have
# minimized most, including several special cases. The "lp" below is what can be
# found in illegality-risk-v1.1.RData and illegality-risk-vyc44-v1.1.Rdata. 
lp <- key_lp_df4

lp |> distinct(LP_NUM) |> count()



# Saving output ---------------------------------------------------------------

save(lp, file="./data/processed/logging-permits.RData")

# Clean env.
rm(key_lp_df1, key_lp_df2, key_lp_df3, key_lp_df4, autef_pdf, autef_pdf_v0, autef_pdf, 
   autef_brancalion, autef_type_credit, autef_upa, autex_pa, geocmun)

labelled::look_for(lp, details = "full")
labelled::look_for(lp)



# Annex: Summary of sources and stats on type of origin for ipe ---------------

# vol_by_lp not available on data import: the following kept here for
# documentation but can be skipped.
stats <- vol_by_lp |> 
  left_join(lp, by = c("LP_REF" = "LP_NUM"))

stats |> group_by(LP_SOURCE) |> count()

# # A tibble: 17 × 2
# # Groups:   LP_SOURCE [17]
# LP_SOURCE                                                         n
# <chr>                                                         <int>
# 1 AUTEF_UPA_AUTEF_PDF_NA_AUTEX_ORIGINAL_LP_NA_AUTEF_TYPE_CREDIT     1
# 2 AUTEF_UPA_AUTEF_PDF_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT     66
# 3 AUTEF_UPA_AUTEF_PDF_NA_NA_AUTEF_BRANCALION_NA                    82
# 4 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_AUTEF_TYPE_CREDIT                  114
# 5 AUTEF_UPA_AUTEF_PDF_NA_NA_NA_NA                                 427
# 6 AUTEF_UPA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT                           1 -- effectively missing
# 7 AUTEF_UPA_NA_NA_NA_NA_NA                                          4 -- effectively missing
# 8 NA_AUTEF_PDF_NA_AUTEX_ORIGINAL_LP_NA_AUTEF_TYPE_CREDIT            3
# 9 NA_AUTEF_PDF_NA_AUTEX_ORIGINAL_LP_NA_NA                           4
# 10 NA_AUTEF_PDF_NA_NA_AUTEF_BRANCALION_AUTEF_TYPE_CREDIT            62
# 11 NA_AUTEF_PDF_NA_NA_AUTEF_BRANCALION_NA                          186
# 12 NA_AUTEF_PDF_NA_NA_NA_AUTEF_TYPE_CREDIT                          26
# 13 NA_AUTEF_PDF_NA_NA_NA_NA                                        244
# 14 NA_NA_AUTEX_NA_NA_NA                                             47
# 15 NA_NA_NA_AUTEX_ORIGINAL_LP_NA_NA                                  1
# 16 NA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT                                 11 -- effectively missing 
# 17 NA                                                               13 -- effectively missing

# Missing permits/Undetermined permits:
stats |> filter(LP_SOURCE %in% c("AUTEF_UPA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT", 
                                 "AUTEF_UPA_NA_NA_NA_NA_NA", 
                                 "NA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT", 
                                 "NA_NA_NA_AUTEX_ORIGINAL_LP_NA_NA")) |> view()

stats |> filter(is.na(LP_SOURCE)) 

stats |> group_by(TYPE) |> count()

stats |> filter(is.na(TYPE)) |>

stats |> 
  group_by(TYPE) |> 
  summarise(sum(VOLUME)) |> 
  adorn_totals()

stats |> 
  group_by(TYPE) |> 
  summarise(sum(VOLUME)) |> 
  adorn_totals() |>
  adorn_percentages("col")

# Type of logging permit of origin 
# 92% AUTEF, 7% AUTEX-PMFS, 0.08% AUTEX-Legal Def, 1.1% Undetermined.

stats |> filter(AREA_HA < 1) 
# All areas set to zero are AUTEX_SV (legal deforestation) Undetermined do not
# have area so these are not included in the yield analysis (again, this is
# about 1.1% of volume)

stats |> arrange(desc(AREA_HA)) 

# Check permits where area > 10000
stats |> filter(AREA_HA>10000) |> pull(LP_REF)
#[1] "10152201801739" "10152201904874" "10152201912573" "20152201909413"
# "20532201800131" "20532201800132" "20532201803167"
# N.B. all appear to be simply large areas. No errors found to be associated 
# with these.



