# Product Selection and Conversion Factor set up --------------------------

# This script documents the product selection process as well as the
# decision-making regarding what conversion factors (Volumetric Yield
# Coefficients, VYC) will be applied to products in order to transform these
# into roundwood equivalent (RWE) later. 

# The output is a set of products key that can be used to select and convert
# relevant products to RWE, including for the conversion factor sensitivity
# analysis.

# Suggestions for improvement or feedback are always most welcome. Reach out
# through https://github.com/carolsrto/illegality-risk-ns or e-mail directly at
# caroline.franca@chalmers.se



# Setting up work environment ---------------------------------------------

# Loading needed libraries
library(tidyverse)

# Global options 
# disable scientific notation 
options(scipen = 999)  

# control decimal places
options(digits=14)



# Loading data  -----------------------------------------------------------

# Loading from .RData
load("./data/temp/consolidated.RData")

# - transport: a consolidated file containing all flows from state- and 
# federal-level DOFs and so all products contained in the data.



# 1 Ipe product selection & Decision-making on inclusion --------------------

#Notes on products considerations: 

# 1) Product excluded due to relevance: All categories that include biomass for
# energy production (including firewood and charcoal) and residues of the
# processing industry in general. Non-timber forest products as well as minerals
# (e.g. ferro gusa) were also discarded. 

# 2) Products excluded due to need: "PRODUTO ACABADO" or finished product is a
# broad category, with arguably no conversion factor given we are unaware of
# what type of product is truly being considered finished. It is also a
# relatively smaller share of consumed products as reported by IBAMA 2019 when
# compared to the final consumption of sawnwood categories: They show it makes
# up 2.45% of the total final consumption for all species produced in Brazil
# between 2012-2017, but it does not make the top 10 products (97.55% of final
# consumption) for the case of ipe.

# 3) Products we decided to exclude. TORETE (short log) was excluded because it
# is mostly reported under GF2 in SISFLORA, a category for biomass, residues and
# the products in general. Moreover, due to the ambiguity of whether this is
# processed into other timber products or rather used as biomass/charcoal made
# us decide against the including of this category, despite IBAMA 2019 including
# this in their analysis. Additionally TORETES were about 0.2 % (SISFLORA PA 1
# and 2) and 0.6% (SINAFLOR) of the volume when summing only toretes and
# roundwood (with no removal of re-transport). 

# We also decided to exclude certain categories that represented marginal flows
# such as roundwood of produced timber used for commercial plantations (42 m3
# have been reported over the 10 years for ipe) as well as mouroes/posts (47.2
# st over 10 years). Plywood was also removed 4.58 m3 for plywood for the 10
# years.

# Altogether we find this removals have minimal impact on the overall findings,
# for other species this removals should be revisited.


# Decision-making on product inclusion starts from narrowing down flows per
# species and broader params under analysis. This also allows us to discard
# certain products that are never transacted/relevant for said species.
products <- transport |>
  filter(ID_YEAR %in% c(2009:2019)) |> 
  filter(STATUS_GF_DOF %in% c("RECEBIDO", "RECEBIDO_VENCIDO", "EXPORTADO")) |> 
  filter(grepl(c("HANDROANTHUS|TABEBUIA"), SCIENTIFIC_NAME)) |> 
  distinct(PRODUCT) 


# Build "products to keep" data frame starting by setting: 
# products to be excluded  == 0 
# products to be included  == 1
products_temp <- products|> 
  mutate(PRODUCT_GROUP = case_when(
    # To exclude: 
    # biomass and residues
    grepl(c("RESIDUO|CARVAO|BRIQUETE|CAVACO|CASCA|LASCA|RACHA|LENHA|SERRAGEM|APROVEITAMENTO|SOBRAS|ROLETE"), products$PRODUCT) == TRUE ~ "0",
    # Special cases such as products with no established conversion factor, marginal volume 
    grepl(c("ACABADO|ESTIPE|LAPIDADOS|MOUROES|TORETES|COMPENSADO"), products$PRODUCT) == TRUE ~ "0",
    # From comercial plantations
    PRODUCT == "TORAS DE MADEIRA PRODUZIDA" ~ "0",
    # To keep: 
    # Roundwoods
    PRODUCT == "TORA"  ~ "1",
    PRODUCT == "TORAS DE MADEIRA NATIVA" ~ "1",
    # Sawnwoods 
    grepl(c("SERRADA|SARRAF|CAIBR|BLOCO|RIPA|VIG|TABUA|FILE|PRANCH|DORMENTE"), products$PRODUCT) == TRUE ~ "1",
    # Planed and further processed/industrialized
    grepl(c("APLAINADA|DECKING|LAMINA|ASSOALHO|LAMBRIL|PISO|TACOS|BENEFICIADA|PORTAL|ALISAR"), products$PRODUCT) == TRUE ~ "1"))

# Select products to keep
products_keep <- products_temp |> filter(!PRODUCT_GROUP == 0) |> 
  distinct(PRODUCT) 


# 2 Building the conversion factor data frame ----------------------------------

# N.B. The conversion factor is derived from a combination of legislation and
# scientific literature. Based on relevant products, we build a baseline
# conversion factor dataframe. In this df, the VYC_TEMP (Volumetric Yield
# Coef.)can then be multiplied by different values that capture assumptions of
# low (35% VYC) to high (53.9%) efficiency in primary breakdown. Refer to
# Supplementary Table 1 for references. 

# Add a grouping variable so that conversion factor can be applied at the grouping level
conversion_baseline <- products_keep |> 
  mutate(PRODUCT_GROUP = case_when(
    PRODUCT == "TORA"  ~ "ROUNDWOOD",
    PRODUCT == "TORAS DE MADEIRA NATIVA" ~ "ROUNDWOOD", 
    grepl(c("SERRADA|SARRAF|CAIBR|BLOCO|RIPA|VIG|FILE|PRANCH|DORMENTE|CURT"), products_keep$PRODUCT) == TRUE ~ "SAWNWOOD", 
    PRODUCT == "TABUA" ~ "SAWNWOOD", 
    grepl(c("APLAINADA 4"), products_keep$PRODUCT) == TRUE ~ "PLANED 4 FACES (S4S)",  
    grepl(c("APLAINADA 2"), products_keep$PRODUCT) == TRUE ~ "PLANED 2 FACES (S2S)",  
    grepl(c("DECKING|PISO|TACOS"), products_keep$PRODUCT) == TRUE ~ "DECKING AND FLOORING",  
    grepl(c("FAQUEADA"), products_keep$PRODUCT) == TRUE ~ "VENEER SLICED",  
    grepl(c("TORNEAD|DESENROLADA"), products_keep$PRODUCT) == TRUE ~ "VENEER PEELED",  
    PRODUCT == "COMPENSADO" ~ "PLYWOOD", 
    PRODUCT == "TORETES" ~ "SHORT ROUNDWOOD", 
    PRODUCT == "MADEIRA BENEFICIADA" ~ "INDUSTRIALIZED TIMBER", 
    PRODUCT == "PRODUTO ACABADO" ~ "FINISHED PRODUCT", 
    grepl(c("MOUR"), products_keep$PRODUCT) == TRUE ~ "POSTS"
  ))


# Set up baseline VYC
conversion_baseline <- conversion_baseline |> 
  mutate(VYC_TEMP = case_when(
    # to be used as is
    PRODUCT_GROUP == "ROUNDWOOD" ~ 100, 
    PRODUCT_GROUP == "VENEER SLICED" ~ 45, #same across all species, except for when species-specif study on conversion is available. Set by legislation.
    PRODUCT_GROUP == "VENEER PEELED" ~ 55, #same across all species, except for when species-specif study on conversion is available. Set by legislation.
    # to be multiplied by given VYC
    PRODUCT_GROUP == "SAWNWOOD" ~ 1, 
    PRODUCT_GROUP == "PLANED 2 FACES (S2S)" ~ 1*85/100, 
    PRODUCT_GROUP == "PLANED 4 FACES (S4S)" ~ 1*82/100, 
    PRODUCT_GROUP == "INDUSTRIALIZED TIMBER" ~ 1*82/100,
    PRODUCT_GROUP == "DECKING AND FLOORING" ~ 1*82/100,
  ))



# Alternatively one could build a more specific key for conversion e.g.: 

# Add a grouping variable so that conversion factor can be applied at the grouping level
products_conversion_key53 <- products_keep |> 
  mutate(PRODUCT_GROUP = case_when(
    PRODUCT == "TORA"  ~ "ROUNDWOOD",
    PRODUCT == "TORAS DE MADEIRA NATIVA" ~ "ROUNDWOOD", 
    grepl(c("SERRADA|SARRAF|CAIBR|BLOCO|RIPA|VIG|FILE|PRANCH|DORMENTE|CURT"), products_keep$PRODUCT) == TRUE ~ "SAWNWOOD", 
    PRODUCT == "TABUA" ~ "SAWNWOOD", 
    grepl(c("APLAINADA 4"), products_keep$PRODUCT) == TRUE ~ "PLANED 4 FACES (S4S)",  
    grepl(c("APLAINADA 2"), products_keep$PRODUCT) == TRUE ~ "PLANED 2 FACES (S2S)",  
    grepl(c("DECKING|PISO|TACOS"), products_keep$PRODUCT) == TRUE ~ "DECKING AND FLOORING",  
    grepl(c("FAQUEADA"), products_keep$PRODUCT) == TRUE ~ "VENEER SLICED",  
    grepl(c("TORNEAD|DESENROLADA"), products_keep$PRODUCT) == TRUE ~ "VENEER PEELED",  
    PRODUCT == "COMPENSADO" ~ "PLYWOOD", 
    PRODUCT == "TORETES" ~ "SHORT ROUNDWOOD", 
    PRODUCT == "MADEIRA BENEFICIADA" ~ "INDUSTRIALIZED TIMBER", 
    PRODUCT == "PRODUTO ACABADO" ~ "FINISHED PRODUCT", 
    grepl(c("MOUR"), products_keep$PRODUCT) == TRUE ~ "POSTS"
    ))


# Add conversion factor by grouping 
products_conversion_key53 <- products_conversion_key53  |> 
  mutate(VYC = case_when(
    PRODUCT_GROUP == "ROUNDWOOD" ~ 100, 
    PRODUCT_GROUP == "SAWNWOOD" ~ 53.9, 
    PRODUCT_GROUP == "PLANED 2 FACES (S2S)" ~ 53.9*85/100, 
    PRODUCT_GROUP == "PLANED 4 FACES (S4S)" ~ 53.9*82/100, 
    PRODUCT_GROUP == "INDUSTRIALIZED TIMBER" ~ 53.9*82/100,
    PRODUCT_GROUP == "DECKING AND FLOORING" ~ 53.9*82/100,
    PRODUCT_GROUP == "VENEER SLICED" ~ 45,
    PRODUCT_GROUP == "VENEER PEELED" ~ 55
  ))

# Add equivalence factor
products_conversion_key53 <- products_conversion_key53  |> 
  mutate(EQUIVALENCE_FACTOR = 1/VYC*100) |>  
  relocate(EQUIVALENCE_FACTOR, .after = VYC)

# Add reference 
products_conversion_key53 <- products_conversion_key53  |> 
  mutate(REFERENCE = case_when(
    PRODUCT_GROUP == "ROUNDWOOD" ~ "CONAMA 497 2020", 
    PRODUCT_GROUP == "SAWNWOOD" ~ "Romero et al 2020", 
    PRODUCT_GROUP == "PLANED 2 FACES (S2S)" ~ "Romero et al 2020/IN IBAMA 9 2016/CONAMA 497 2020", 
    PRODUCT_GROUP == "PLANED 4 FACES (S4S)" ~ "Romero et al 2020/IN IBAMA 9 2016", 
    PRODUCT_GROUP == "INDUSTRIALIZED TIMBER" ~ "Romero et al 2020/CONAMA 497 2020",
    PRODUCT_GROUP == "DECKING AND FLOORING" ~ "Romero et al 2020/IN IBAMA 9 2016/CONAMA 497 2020",
    PRODUCT_GROUP == "VENEER SLICED" ~ "CONAMA 497 2020",
    PRODUCT_GROUP == "VENEER PEELED" ~ "CONAMA 497 2020"
  ))


# 3 Translate product names -------------------------------------------------

#TODO: (low-priority) can apply previous translation to all products 

# 4 Save outputs to ./data/processed ----------------------------------------

save(conversion_baseline,
     file = "./data/temp/products.Rdata")

rm(products, products_temp, products_keep, products_conversion_key53)



#Annex: Basic EDA for product inclusion (ipe-specific) -----------------------------------------------

# Check products and units
transport |> 
  filter( grepl(c("HANDROANTHUS|TABEBUIA"), SCIENTIFIC_NAME)) |> 
  distinct(PRODUCT, UNIT) |> 
  arrange(PRODUCT) |>  
  print(n = 80)

# # A tibble: 78 x 2
# PRODUCT                                                                UNIT 
# <chr>                                                                  <chr>
# 1 APROVEITAMENTO DE LAMINA FAQUEADA                                      M3   
# 2 APROVEITAMENTO DE LAMINA TORNEADA                                      M3   
# 3 APROVEITAMENTOS DE MADEIRA                                             M3   
# 4 BLOCO                                                                  M3   
# 5 BLOCO, QUADRADO OU FILE                                                M3   
# 6 BRIQUETE                                                               M3   
# 7 CAIBRINHOS                                                             M3   
# 8 CAIBRO                                                                 M3   
# 9 CAIBRO CURTO                                                           M3   
# 10 CARVAO                                                                 MDC  
# 11 CASCAS                                                                 KG   
# 12 CAVACO                                                                 M3   
# 13 CAVACOS                                                                M3   
# 14 COMPENSADO                                                             M3   
# 15 DECKING                                                                M3   
# 16 DORMENTE                                                               M3   
# 17 DORMENTE SERRADA                                                       M3   
# 18 FILE                                                                   M3   
# 19 LAMINA DESENROLADA                                                     M3   
# 20 LAMINA FAQUEADA                                                        M3   
# 21 LAPIDADOS                                                              M3   
# 22 MADEIRA APLAINADA 2 FACES (S2S)                                        M3   
# 23 MADEIRA APLAINADA 4 FACES (S4S)                                        M3   
# 24 MADEIRA BENEFICIADA                                                    M3   
# 25 MADEIRA INDUSTRIALIZADA DE RESIDUO                                     M3   
# 26 MADEIRA INDUSTRILIZADA DE RESIDUO (CAIBRINHO)                          M3   
# 27 MADEIRA INDUSTRILIZADA DE RESIDUO (RIPA)                               M3   
# 28 MADEIRA INDUSTRILIZADA DE RESIDUO (SHORT OU SARRAFO)                   M3   
# 29 MADEIRA LAMINADA FAQUEADA                                              M3   
# 30 MADEIRA LAMINADA TORNEADA                                              M3   
# 31 MADEIRA SERRADA                                                        M3   
# 32 MADEIRA SERRADA (CAIBRO)                                               M3   
# 33 MADEIRA SERRADA (PRANCHA)                                              M3   
# 34 MADEIRA SERRADA (PRANCHAO DESDOBRADO)                                  M3   
# 35 MADEIRA SERRADA (TABUA)                                                M3   
# 36 MADEIRA SERRADA (VARETA)                                               M3   
# 37 MADEIRA SERRADA (VIGA)                                                 M3   
# 38 MADEIRA SERRADA (VIGOTA)                                               M3   
# 39 MADEIRA SERRADA APLAINADA 2 FACES                                      M3   
# 40 MADEIRA SERRADA APLAINADA 4 FACES                                      M3   
# 41 MOUROES                                                                ST   
# 42 MOUROES                                                                M3   
# 43 MOUROES (ST)                                                           ST   
# 44 MOUROES OU MOIROES                                                     ST   
# 45 PISO ENGENHEIRADO                                                      M3   
# 46 PISOS E ASSOALHOS                                                      M3
# 47 PRANCHA                                                                M3   
# 48 PRANCHAO                                                               M3   
# 49 PRODUTO ACABADO                                                        M3   
# 50 RESIDUO                                                                M3   
# 51 RESIDUO - MIOLO DE COMPENSADO                                          M3   
# 52 RESIDUO DA INDUSTRIA MADEIREIRA PARA FINS DE APROVEITAMENTO INDUSTRIAL M3   
# 53 RESIDUO DE SERRARIA                                                    M3   
# 54 RESIDUO FONTE DE ENERGIA                                               M3   
# 55 RESIDUO PARA APROVEITAMENTO INDUSTRIAL                                 M3   
# 56 RESIDUO PARA FINS ENERGETICOS                                          M3   
# 57 RESIDUOS PARA FINS ENERGETICOS                                         M3   
# 58 RIPA                                                                   M3   
# 59 RIPA CURTA                                                             M3   
# 60 RIPAS                                                                  M3   
# 61 ROLETE                                                                 M3   
# 62 SARRAFO                                                                M3   
# 63 SARRAFO CURTO                                                          M3   
# 64 SARRAFOS                                                               M3   
# 65 SERRAGEM                                                               M3   
# 66 SOBRAS E APARAS DE MADEIRA                                             M3   
# 67 TABUA                                                                  M3   
# 68 TABUA APLAINADA 2 FACES (S2S)                                          M3   
# 69 TABUA APLAINADA 4 FACES (S4S)                                          M3   
# 70 TABUA CURTA                                                            M3   
# 71 TACOS                                                                  M3   
# 72 TORA                                                                   M3   
# 73 TORAS DE MADEIRA NATIVA                                                M3   
# 74 TORAS DE MADEIRA PRODUZIDA                                             M3   
# 75 TORETES                                                                M3   
# 76 VARA                                                                   M3   
# 77 VIGA                                                                   M3   
# 78 VIGOTA                                                                 M3   

transport |>
  filter(ID_YEAR %in% c(2009:2019), 
         STATUS_GF_DOF %in% c("RECEBIDO", "RECEBIDO_VENCIDO", "EXPORTADO"), 
         grepl(c("HANDROANTHUS|TABEBUIA"), SCIENTIFIC_NAME), 
         grepl(c("MOUR"), PRODUCT)) |>
  group_by(PRODUCT, UNIT, SYSTEM) |> 
  summarise(sum(VOLUME))
# `summarise()` has grouped output by 'PRODUCT', 'UNIT'. You can override using the `.groups` argument.
# # A tibble: 3 x 4
# # Groups:   PRODUCT, UNIT [3]
# PRODUCT            UNIT  SYSTEM        `sum(VOLUME)`
# <chr>              <chr> <chr>                 <dbl>
# 1 MOUROES            ST    SISFLORA_PA_2          13.7
# 2 MOUROES (ST)       ST    SINAFLOR               13.7
# 3 MOUROES OU MOIROES ST    SISFLORA_PA_1          33.5
#Very likely duplicate between SISFLORA and SINAFLOR (13.7)


potential_production_vector <- transport |>
  filter(ID_YEAR %in% c(2009:2019), 
         STATUS_GF_DOF %in% c("RECEBIDO", "RECEBIDO_VENCIDO", "EXPORTADO"), 
         grepl(c("HANDROANTHUS|TABEBUIA"), SCIENTIFIC_NAME), 
         grepl(c("TORETE|TORA|MOUR"), PRODUCT)) |>
  group_by(PRODUCT, UNIT, SYSTEM) |> 
  summarise(sum(VOLUME))
# `summarise()` has grouped output by 'PRODUCT', 'UNIT'. You can override using the `.groups` argument.
# # A tibble: 7 x 4
# # Groups:   PRODUCT, UNIT [4]
# PRODUCT                    UNIT  SYSTEM        `sum(VOLUME)`
# <chr>                      <chr> <chr>                 <dbl>
# 1 TORA                       M3    SINAFLOR           231276. 
# 2 TORA                       M3    SISFLORA_PA_2      389876. 
# 3 TORAS DE MADEIRA NATIVA    M3    SISFLORA_PA_1      891608. 
# 4 TORAS DE MADEIRA PRODUZIDA M3    SISFLORA_PA_1          28.3
# 5 TORETES                    M3    SINAFLOR              463. 
# 6 TORETES                    M3    SISFLORA_PA_1        5250. 
# 7 TORETES                    M3    SISFLORA_PA_2         704. 

5250*100/(891608+5250)
463*100/(231276+463)
704*100/(389876+704)
# > 5250*100/(891608+5250)
# [1] 0.58537694930524 -- 0.6%
# > 463*100/(231276+463)
# [1] 0.19979373346739 -- 0.2%
# > 704*100/(389876+704)
# [1] 0.18024476419684 -- 0.2%


transport |>
  filter(ID_YEAR %in% c(2009:2019), 
         STATUS_GF_DOF %in% c("RECEBIDO", "RECEBIDO_VENCIDO", "EXPORTADO"), 
         grepl(c("HANDROANTHUS|TABEBUIA"), SCIENTIFIC_NAME), 
         grepl(c("COMPENSADO"), PRODUCT)) |>
  group_by(PRODUCT, UNIT, SYSTEM) |> 
  summarise(sum(VOLUME))
# `summarise()` has grouped output by 'PRODUCT', 'UNIT'. You can override using the `.groups` argument.
# # A tibble: 2 x 4
# # Groups:   PRODUCT, UNIT [2]
# PRODUCT                       UNIT  SYSTEM        `sum(VOLUME)`
# <chr>                         <chr> <chr>                 <dbl>
# 1 COMPENSADO                    M3    SISFLORA_PA_2          4.58 #Plywood
# 2 RESIDUO - MIOLO DE COMPENSADO M3    SISFLORA_PA_1        748.


