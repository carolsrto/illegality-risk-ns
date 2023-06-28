# Input-Output model application ------------------------------------------

# This script is currently under development/refinement. This script contains
# the implementation of the environmentally extended input-output analysis for
# the ipê timber supply chain for the case of Pará, Brazil between 2009-2019. It
# also provides more details on rationale behind flow inclusion for this
# analysis, following descriptions under methods and supplementary text. 

# Reach out if you have suggestions for improvement, feedback or ideas for
# collaboration. See https://github.com/carolsrto/illegality-risk-ns or e-mail
# directly at caroline.franca@chalmers.se



# Setting up the work environment -----------------------------------------

# Loading libraries 
library(tidyverse)
library(scales)
library(janitor)
library(sf)
library(patchwork)
library(reshape2) 
library(expm)

# Set global options
# disable scientific notation 
options(scipen = 999)  

# decimal places 
options(digits=14) 



# Importing data ----------------------------------------------------------

# loading objects from .RData
load("./data/processed/illegality-risk-v1.1.RData")

# illegality-risk-v1.1.RData loads the following objects: 

# - transport:
# Includes a compilation of:
#     - Forest Guides (GF), Para state-level DOF from SISFLORA 2.0 (2016-2020)
#     - Forest Guides (GF), Para state-level DOF from SISFLORA 1.0 (2007-2016)
#     - Document of Forest Origin (DOF), national-level SINAFLOR with Para as
# UF_ORIGIN and/or UF_DESTINATION (2009-2020) 
# N.B.:
#     - CPF/CNPJ of origin and destination as well as cadasters of producers and
# consumers have been anonymized to address any immediate concern regarding LGPD
# (General Data Protection Law).
#     - Data has undergone cleaning and pre-processing steps and draws on a subset
# of variables relevant for the study.

# - conversion_baseline: list of selected products for the analysis with
# respective conversion factors and product translation.

# - lp: compilation of logging permits with details such as such as area authorized,
# permit status and geolocation.


# Parameter selection & Pre-processing ------------------------------------



## Parameter set-up --------------------------------------------------------

# Years to be analysed
analysis_period <- 2009:2019

# String for status inclusion
transaction_status <- c("RECEBIDO", "RECEBIDO_VENCIDO", "EXPORTADO")

# String for species selection
species <- "HANDROANTHUS|TABEBUIA"

# Base conversion factor
base_vyc <- 44.5
# N.B. In this version, change VYC here for numbers on upper-lower boundaries
# (i.e. high efficiency: 53.9, low efficiency: 35). Study's figures are mostly
# based in this 44.5 mid-point and thus it is advisable you start here.

# Products conversion key baseline
conversion_key <- conversion_baseline |> 
  mutate(VYC = case_when(PRODUCT_GROUP %in% c("SAWNWOOD", 
                                              "PLANED 2 FACES (S2S)", 
                                              "PLANED 4 FACES (S4S)", 
                                              "INDUSTRIALIZED TIMBER", 
                                              "DECKING AND FLOORING") 
                         ~ VYC_TEMP*base_vyc, 
                         TRUE ~ VYC_TEMP))


# Clean env. 
rm(conversion_baseline)
# N.B. Run for different parameters at the moment is still manual. Keep in mind
# this script removes intermediary objects, including the initially loaded data.



## Transport transactions subset ---------------------------------------------

# Subset data by time-frame, transaction status, species and relevant products 
transport_df1 <- transport |> 
  filter(ID_YEAR %in% analysis_period, 
         # Keep flows that have been received, received late or exported. 
         # NB. Received at port (RECEBIDO PORTO) has not been added. More details 
         # on this decision provided later. 
         STATUS_GF_DOF %in% transaction_status, 
         # Select species of interest for analysis 
         grepl(species, SCIENTIFIC_NAME),
         # Product conversion key from pre-selected pool 
         PRODUCT %in% conversion_key$PRODUCT) 



## Actor anonymization --------------------------------------------------------

# Developed under "08_actors.R" in order to account for LGPD.
transport_df2 <- transport_df1
rm(transport, transport_df1)

# P vector, Part 1: Data Inclusion & Pre-processing  --------------------------

## Overview included/discarded flows ------------------------------------------

# After initial choice of parameters (status of transaction, period of analysis,
# species and broader product selection), inclusions for the p vector are as
# follows:

# - Products: only roundwood is included. (Toretes were not included:low share
# 0.2% and 0.6% for sisflora and sinaflor respectively; mouroes/posts were
# excluded; production vector also does not include any biomass of residues from
# the harvesting process. See "09_products.R" for more details).

# - Permit reporting for roundwood: roundwood flows with no permit data have
# been discarded. In SISFLORA, all GF1-level flow displays a permit-process. As
# explained in the study, this one of the basic legal requirements in reporting.
# In SINAFLOR data, however, 66% of roundwood flows do not have a listed permit.
# A closer analysis in the patterns of data reporting (lack thereof), however,
# showed such flows are likely duplicates between SISFLORA-PA and SINAFLOR
# systems. The bulk of SINAFLOR removals based on missing permits (148 236 m3,
# 64% of SINAFLOR roundwood flows) shared four key features in common i. did not
# have a listed permit, ii. had actor of origin that equaled that of destination
# destination, iii. had data on type of origin missing and iv. did not display
# coordinates of origin or destination. Thus the production vector only includes
# roundwood flows with permits reported. 

# - Re-transport: Re-transport from re-sale flows can lead to overestimation/
# inflating of production figures. In the SISFLORA-PA, GF-types variable can be
# used. GF1 is the module used for roundwood transport. All roundwood that is
# transported under other GF types (GF3, GF3i, GF4) can be classified as
# re-transport based on legislation and previous IBAMA 2019 study. For SINAFLOR,
# only about 35% of all roundwood flows with origin in the state had a type of
# origin listed, making it challenging to remove re-transport on a similar
# basis. Still, if we take a step-wise approach and remove first flows with no
# permit listed, then we can remove re-transport on the basis of type of origin
# (about 3%) and this is used to minimize the risk of misrepresenting production
# figures.

# - Destination equal to origin: We remove all instances where CPF/CNPJs of
# Origin == Destination. So from 1.08 M m3 reported for the entire decade we
# discard around 9% (97 966) where origin was equal to destination. This is
# about 3% for SINAFLOR (after permit removal), 11% (75 671) for SISFLORA 1.0
# and 6% (19 843) for SISFLORA 2.0. See supplementary material for details on
# rationale of removal. 

# - Production from outer state: we do not include any volume from outer-state
# at this moment. Outer-state production is added later in the production
# vector, after conversion to rowndwood equivalent and before we add dummies for
# missing flows. Outer-satte flows are also used later in the transactions
# matrix (Z)



## Applying decision on production flows inclusion -----------------------------

# Roundwood-only dataframe 
rw_sp <- transport_df2 |> 
  filter(grepl(c("TORA"), PRODUCT), # Toretes have been opted out (see "09_products.R") 
         # Remove SISFLORA RW re-transport by dropping all but GF1 level. RW
         # export for most native species is banned, including ipe thus no
         # "DOFEXP" type added here. 
         TYPE_GF_DOF %in% c("GF1", "DOF"),
         # Remove RW coming from other states
         UF_ORIGIN == "PA") |> 
  # Remove SINAFLOR flows that do not have permit
  # create column to filter out cases
  unite(PERMIT, PERMIT_NUM_AUTEX_SERIE, col = "RM", sep = "_", remove = FALSE) |> 
  filter(RM != "NA_NA") |> 
  select(-RM) |> 
  # Remove SINAFLOR RW re-transport by discarding flows that start w/ string "PATIO" (yard)
  filter(!grepl(c("^PATIO"), TYPE_ORIGIN)) |> 
  # Removed 50 m3 related to a municipality of origin missing
  filter(!is.na(GEOCMUN_ORIGIN))

# Unite all production (N.B. No other products here, but to keep in mind for
# other species to be added in the future)
p_all_prods_sp <- rw_sp

# Remove producers that send RW to themselves (cpf/cnpj of origin equal to destination) 
p_sp_df1 <- p_all_prods_sp |>  
  select(CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN, 
         CPF_CNPJ_DESTINATION, GEOCMUN_DESTINATION, VOLUME, SYSTEM, ID_YEAR) |>   
  subset(!(CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION)) 

rm(p_all_prods_sp)



## Duplicate removal -------------------------------------------------------

# N.B. This is applied to SINAFLOR data only and no duplicates from SISFLORA
# were found in SINAFLOR after the removal of roundwood flows without permit.
# Still, one can test clear actor-mun duplicates in the below. 

# Checking for volume duplicates by actor-mun
# Build summarized df by unique actor-municipality in order to remove duplicates
# dedupe_key <-  rw_sp |>
#   select(CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN, VOLUME, SYSTEM) |>
#   group_by(CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN, SYSTEM) |> #.add = TRUE
#   summarise(VOLUME = round(sum(VOLUME))) |>
#   #build unique keys, complete for CNPJs and only with 3 digits for CPF
#   mutate(DIGITS = case_when(nchar(CPF_CNPJ_ORIGIN) > 11 ~ CPF_CNPJ_ORIGIN,
#                             nchar(CPF_CNPJ_ORIGIN) == 11 ~ str_sub(CPF_CNPJ_ORIGIN, 1, 3),
#                             TRUE ~ as.character(NA))) |>
#   unite(DIGITS, GEOCMUN_ORIGIN, col = "KEY_DUP", sep = "_", remove = FALSE) |>
#   select(-DIGITS)
# 
# #Remove values with both an actor-mun key duplicate and a total volume
# #duplicate
# dedupe <-  dedupe_key  |>
#   group_by(KEY_DUP, VOLUME) |>
#   mutate(RM = n()) |>
#   unite(SYSTEM, RM, col = "TEMP", sep = "-", remove = FALSE) |>
#   filter(TEMP != "SINAFLOR-2") |>
#   select(-c(RM)) |>
#   ungroup()

# Compare overall change in volume
# rw_sp |> group_by(SYSTEM) |> summarise(sum(VOLUME))
# rw_sp |>  summarise(sum(VOLUME))
# 
# dedupe |> group_by(SYSTEM) |> summarise(sum(VOLUME))
# dedupe |>  summarise(sum(VOLUME))
# N.B. Changes is volume are minimal; rounding used to compare volume.

# Clean Env.
#rm(dedupe, dedupe_key)


## Summarise volume by producer/actor-municipality -------------------------

# Summarise volume by producer/actor-municipality
p_sp <- p_sp_df1 |> 
  group_by(CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN) |>   
  summarise(VOLUME_SUM = sum(VOLUME)) |>   
  ungroup() |> 
  unite("CPF_CNPJ_GEOCMUN_ORIGIN", CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN) 

# Quick check
p_sp |> arrange(VOLUME_SUM) 


## Contextual stats --------------------------------------------------------

# TL;DR: Skip section to "Z matrix, Part 1 Data Inclusion & Roundwood Equivalent
# Conversion" for direct reproduction of results. Here you find further details
# intended to clarify magnitude of (and decisions on) data inclusion/removal for
# the analysis.

### Stats on vol./records discarded (cpf/cnpj of origin == destination) ----

# Stats on removal of equal origin and destination. 
vol_before <- rw_sp  |>
  group_by(SYSTEM) |>
  summarise(VOLUME_SUM = sum(VOLUME), n = n()) |>
  mutate(TEMP = "BEFORE")
vol_after <- p_sp_df1 |>
  group_by(SYSTEM) |>
  summarise(VOLUME_SUM = sum(VOLUME), n = n()) |>
  mutate(TEMP = "AFTER")
stats_self_transport <- bind_cols(vol_before, vol_after) |>
  janitor::adorn_totals("row") |>
  mutate(PERC_VOL = 100 - (VOLUME_SUM...6 * 100 / VOLUME_SUM...2)) |>
  mutate(RM_VOL = VOLUME_SUM...2 - VOLUME_SUM...6) |> 
  mutate(PERC_n = 100 - (n...7 * 100 / n...3)) |>
  mutate(RM_n = n...3 - n...7) |> as_tibble()
# # A tibble: 4 × 12
# SYSTEM...1    VOLUME_SUM...2  n...3 TEMP...4 SYSTEM...5    VOLUME_SUM...6  n...7 TEMP...8 PERC_VOL RM_VOL PERC_n  RM_n
# <chr>                  <dbl>  <int> <chr>    <chr>                  <dbl>  <int> <chr>       <dbl>  <dbl>  <dbl> <int>
# 1 SINAFLOR              76318.   5153 BEFORE   SINAFLOR              73867.   4890 AFTER        3.21  2451.   5.10   263
# 2 SISFLORA_PA_1        691989.  27419 BEFORE   SISFLORA_PA_1        616317.  21449 AFTER       10.9  75672.  21.8   5970
# 3 SISFLORA_PA_2        315878. 120233 BEFORE   SISFLORA_PA_2        296034. 112702 AFTER        6.28 19843.   6.26  7531
# 4 Total               1084184. 152805 -        -                    986218. 139041 -            9.04 97966.   9.01 13764

# Clean Env. 
rm(vol_after, vol_before, stats_self_transport)



### Stats on actors discarded (cpf/cnpj of origin equal to destination) ------

# Numbers on actors discarded 
# Producers
cpf_cnpj_origin <- rw_sp |> distinct(CPF_CNPJ_ORIGIN) |> 
  rename(CPF_CNPJ = CPF_CNPJ_ORIGIN)
cpf_cnpj_destination <- rw_sp |> distinct(CPF_CNPJ_DESTINATION) |> 
  rename(CPF_CNPJ = CPF_CNPJ_DESTINATION)
cpf_cnpj_all <- bind_rows(cpf_cnpj_origin, cpf_cnpj_destination) |> distinct()
count(cpf_cnpj_all)
# 1740 producers

# Producers with discarded CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION
cpf_cnpj_origin_rm <- p_sp_df1  |> distinct(CPF_CNPJ_ORIGIN) |> 
  rename(CPF_CNPJ = CPF_CNPJ_ORIGIN)
cpf_cnpj_destination_rm <- p_sp_df1  |> distinct(CPF_CNPJ_DESTINATION) |> 
  rename(CPF_CNPJ = CPF_CNPJ_DESTINATION)
cpf_cnpj_all_rm <- bind_rows(cpf_cnpj_origin_rm, cpf_cnpj_destination_rm) |> distinct()
count(cpf_cnpj_all_rm)
# 1730 producers, thus we discard 10 cpfs/cnpjs

# Listing actors discarded
cpf_cnpj_discarded_p <- anti_join(cpf_cnpj_all, cpf_cnpj_all_rm) |> pull()

# What is the relevance of these discarded flows?

# Select only such discarded flows for a closer look
actor_discard <- rw_sp |> filter(CPF_CNPJ_ORIGIN %in% cpf_cnpj_discarded_p)

# RW volume removed
actor_discard  |> summarise(sum(VOLUME))
# # A tibble: 1 x 1
# `sum(VOLUME)`
# <dbl>
#   1        19733.

# RW volume for entire ipe subset
rw_sp |> summarise(sum(VOLUME))
# # A tibble: 1 x 1
# `sum(VOLUME)`
# <dbl>
#   1      1084184.

19733*100/1084184 
# 1.82% of RW 


actor_discard  |> group_by(SYSTEM) |> summarise(sum(VOLUME))
# # A tibble: 3 x 2
# SYSTEM        `sum(VOLUME)`
# <chr>                 <dbl>
# 1 SINAFLOR              2318.
# 2 SISFLORA_PA_1         8766.
# 3 SISFLORA_PA_2         8649.

actor_discard  |> group_by(SYSTEM, UF_ORIGIN, CPF_CNPJ_ORIGIN, CPF_CNPJ_DESTINATION) |> 
  summarise(sum(VOLUME))

# Clean Env. 
rm(cpf_cnpj_origin, cpf_cnpj_destination, cpf_cnpj_origin_rm, 
   cpf_cnpj_destination_rm, cpf_cnpj_all, cpf_cnpj_all_rm, cpf_cnpj_discarded_p, 
   actor_discard)


## Visualizing the production vector  --------------------------------------

# Visualizing the production vector, after discarding flows:
p_sp_df1 |>
  ggplot()+
  geom_col(aes(ID_YEAR, VOLUME, fill = SYSTEM)) +
  scale_y_continuous(expression(paste("Volume (", m^{3}, ")" )),
                     labels = label_number(scale_cut = cut_short_scale()),
                     limits = c(0, 130000)) +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = c("#9b6433", "#67869b", "#2d4a58"),
                    name = "System", labels = c("SINAFLOR", "SISFLORA (PA) 1.0", 
                                                "SISFLORA (PA) 2.0")) +
  ggtitle("Yearly roundwood production, Handroanthus spp./Tabebuia spp.")

# Save figures
# ggsave("./results/suppl_production_rm_retransport.jpg")

# Clean env.
rm(p_sp_df1)



# Z matrix, Part 1: Data Inclusion & Roundwood Equivalent Conversion ---------

# As in Kastner et al. 2011: Matrix of tradeflow in equivalents; obtained from
# "trade" (here interpreted as transactions) matrices for the products derived
# from the respective "crop" (i.e. timber).

## Overview included/discarded flows: ----------------------------------------

# Starting from initial choice of parameters (status of transaction, period of
# analysis, species and broader product selection), inclusions of flows in the Z
# matrix are as follows:

# - Products: Includes all timber products (roundwood and processed) except from
# the already removed, such as residues harvesting and industrialization
# process, biomass for energy, NTFP and minerals, *produto acabado/finished
# product*, toretes/short logs, mouroes/posts and compensado/plywood. See
# products script for more detail.

# - Origin equal to destination: we removed all flows where origin equals
# destination. A few reasons can be noted here. This is done due to nature of
# such flows not having an impact on the physical share attributed the an actor
# in the input-output model, which is our main object of study. Additionally,
# setting CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION appears to be a consistent
# feature of duplication reporting found in SINAFLOR and thus by removing this
# flows in the Z matrix we minimize the inclusion of duplicates between
# state-level and national-level systems.

# Two exceptions apply, for SISFLORA 1.0 all export flows also had the feature
# of CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION. Thus for all export flows
# (relevant only for SISFLORA 1.0 and 2.0 since we don't use SINAFLOR data on
# exports) we set CPF_CNPJ_DESTINATION as EXPORT prior to CPF_CNPJ_ORIGIN ==
# CPF_CNPJ_DESTINATION removal. Second, a marginal share (~700m3) of what is
# understood as final consumption for SISFLORA 1.0 (cadasters of destination are
# missing/zero) also displays the feature CPF_CNPJ_ORIGIN ==
# CPF_CNPJ_DESTINATION. For these cases, CPF_CNPJ_DESTINATION is set to
# FINALCONSUMPTION so these flows are not excluded.

# N.B. With this removal keep in mind the diagonal of Z will have zero values
# and matrix inversion can be used *as well as* polynomial approx, nonetheless
# the former is faster and thus preferable. 



## Applying decision on transaction flows inclusion ----------------------------

# All origin-destination flows, selecting relevant variables (or those necessary
# to discard non-relevant flows)
sp_matrix <- transport_df2 |> 
  select(CPF_CNPJ_ORIGIN,  GEOCMUN_ORIGIN, UF_ORIGIN, 
         CPF_CNPJ_DESTINATION, GEOCMUN_DESTINATION, UF_DESTINATION,
         TYPE_GF_DOF, CADASTER_DESTINATION, 
         SCIENTIFIC_NAME, PRODUCT, VOLUME,  
         TYPE_ORIGIN, TYPE_DESTINATION,
         PERMIT, PROCESS, PERMIT_NUM_AUTEX_SERIE, ID_YEAR, SYSTEM)


# Before we remove any flows with cpf/cnpj of origin that equal cpf/cnpj of
# destination we want to make sure this is not final consumption, which are: 
# 1) Export flows; 
# 2) in SISFLORA 1.0, flows with missing cadaster of destination; 
# 3) in SISFLORA 2.0, flows with missing/set to zero cadaster of destination; 


# Set CPF_CNPJ_DESTINATION to EXPORT to all known flows that are exports
sp_matrix_df1 <-  sp_matrix |>
  mutate(CPF_CNPJ_DESTINATION = case_when(grepl(c("EXPORT"), TYPE_DESTINATION) 
                                          ~ "EXPORT",
                                          TRUE ~ CPF_CNPJ_DESTINATION))

# Rationale: exactly all flows that are exports also have origin == destination
sp_matrix |>
  filter(grepl(c("EXPORT"), TYPE_DESTINATION)) |>
  group_by(SYSTEM) |>
  summarise(n = n(), VOLUME = sum(VOLUME)) |>
  distinct()
# # A tibble: 3 × 3
# SYSTEM            n  VOLUME
# <chr>         <int>   <dbl>
# 1 SINAFLOR      11428 192611.
# 2 SISFLORA_PA_1 21870 367894.
# 3 SISFLORA_PA_2 11003 165524.

sp_matrix |>
  filter(grepl(c("EXPORT"), TYPE_DESTINATION)) |>
  group_by(SYSTEM) |>
  summarise(CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION, n = n(), VOLUME = sum(VOLUME)) |>
  distinct()
# # A tibble: 3 × 4
# # Groups:   SYSTEM [3]
# SYSTEM        `CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION`     n  VOLUME
# <chr>         <lgl>                                     <int>   <dbl>
# 1 SINAFLOR      TRUE                                      11428 192611.
# 2 SISFLORA_PA_1 TRUE                                      21870 367894.
# 3 SISFLORA_PA_2 NA                                        11003 165524. 

#All flows that had missing CPF_CNPJ_DESTINATION were exports, a feature of SISFLORA 2.0
sp_matrix |>
  filter(grepl(c("EXPORT"), TYPE_DESTINATION), SYSTEM == "SISFLORA_PA_2") |>
  distinct(CPF_CNPJ_DESTINATION)


# Set CPF_CNPJ_DESTINATION to FINALCONSUMPTION to all flows that have a
# missing/set to zero cadaster of destination and also have CPF_CNPJ_ORIGIN ==
# CPF_CNPJ_DESTINATION
sp_matrix_df2 <- sp_matrix_df1 |> 
  mutate(CPF_CNPJ_DESTINATION = case_when((is.na(CADASTER_DESTINATION)|CADASTER_DESTINATION == "0") 
                                          & CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION ~ "FINALCONSUMPTION", 
                                          TRUE ~ CPF_CNPJ_DESTINATION))

#Rationale: 
sp_matrix_df1 |>
  filter(is.na(CADASTER_DESTINATION), CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION)|>
  group_by(SYSTEM) |>
  summarise(n = n(), VOLUME = sum(VOLUME))

sp_matrix_df1 |>
  filter(is.na(CADASTER_DESTINATION)|CADASTER_DESTINATION=="0", 
         CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION)|>
  group_by(SYSTEM) |>
  summarise(n = n(), VOLUME = sum(VOLUME))

#But it should be kept in mind that a large share of supposedly final
#consumption, which not export also has cpf/cnpj origin diff from dest. 
sp_matrix_df1 |>
  filter(is.na(CADASTER_DESTINATION)|CADASTER_DESTINATION=="0")|>
  group_by(SYSTEM) |>
  summarise(n = n(), VOLUME = sum(VOLUME))

sp_matrix_df1 |>
  filter(is.na(CADASTER_DESTINATION)|CADASTER_DESTINATION=="0", 
         grepl(c("EXPORT"), TYPE_DESTINATION))|>
  group_by(SYSTEM) |>
  summarise(n = n(), VOLUME = sum(VOLUME))

sp_matrix_df1 |>
  filter(is.na(CADASTER_DESTINATION)|CADASTER_DESTINATION=="0", 
         !grepl(c("EXPORT"), TYPE_DESTINATION))|>
  group_by(SYSTEM) |>
  summarise(n = n(), VOLUME = sum(VOLUME))

sp_matrix_df1 |>
  filter(grepl(c("EXPORT"), TYPE_DESTINATION))|>
  group_by(SYSTEM) |>
  summarise(n = n(), VOLUME = sum(VOLUME))

sp_matrix_df1 |>
  filter(is.na(CADASTER_DESTINATION)|CADASTER_DESTINATION == "0")|>
  group_by(SYSTEM, TYPE_DESTINATION) |>
  summarise(n = n(), VOLUME = sum(VOLUME)) |>
  adorn_totals() |>
  adorn_percentages("col") |> 
  as_tibble()

# So 77.5% of reported final consumption (SISFLORA 1.0 and 2.0) is export (prior
# to RWE conversion so we should be careful but keep in mind these are
# interesting figures). The remaining is mostly allegedly going to other states
# (if we only take SISFLORA).

sp_matrix_df1 |>
  filter(is.na(CADASTER_DESTINATION)|CADASTER_DESTINATION == "0", 
         CPF_CNPJ_DESTINATION != "EXPORT")|>
  group_by(UF_DESTINATION) |>
  summarise(VOLUME = sum(VOLUME)) |>
  adorn_totals() |>
  adorn_percentages("col") |> 
  as_tibble() |> 
  arrange(desc(VOLUME))

# # # A tibble: 25 × 3
# # UF_DESTINATION      n VOLUME
# # <chr>           <dbl>  <dbl>
# #   1 Total          1      1     
# # 2 SC             0.108  0.248 
# # 3 PR             0.120  0.214 
# # 4 SP             0.120  0.120 
# # 5 BA             0.123  0.0612
# # 6 CE             0.0770 0.0513
# # 7 RJ             0.0471 0.0322
# # 8 DF             0.0521 0.0317
# # 9 RN             0.0462 0.0307
# # 10 MG             0.0480 0.0291
# # # … with 15 more rows

sp_matrix_df1 |>
  filter(CPF_CNPJ_DESTINATION != "EXPORT", !grepl("TORA", PRODUCT))|>
  group_by(UF_DESTINATION) |>
  summarise(VOLUME = sum(VOLUME)) |>
  adorn_totals() |>
  adorn_percentages("col") |> 
  as_tibble() |> 
  arrange(desc(VOLUME)) # |> view()


# Remove all transactions where actors send products to themselves
sp_matrix_df3 <- sp_matrix_df2 |> 
  filter(CPF_CNPJ_ORIGIN != CPF_CNPJ_DESTINATION)


# Removing exports from SINAFLOR and working with Exports from SISFLORA-PA only
sp_matrix_df4 <- sp_matrix_df3 |> filter(!(!is.na(TYPE_DESTINATION) & 
                                             TYPE_DESTINATION == "EXPORT" & SYSTEM == "SINAFLOR"))

# N.B. For now all exports from SINAFLOR are removed. SINAFLOR data is missing
# from 2017-2019 (also all period since) and deduplication at export level
# raises questions on missing data yet not clarified. I have been in
# communication with Ibama staff and can derive from exchanges that data is
# incomplete. The status RECEBIDO_PORTO also adds to complications; gaps between
# these and exported should be assessed in an more in-depth analysis focused on
# export only.

# In summary: 

# All volume, pre conversion and IO
sp_matrix |> 
  group_by(SYSTEM) |> 
  summarise(n = n(), VOLUME = sum(VOLUME))
sp_matrix_df1 |> 
  group_by(SYSTEM) |> 
  summarise(n = n(), VOLUME = sum(VOLUME))
sp_matrix_df2 |> 
  group_by(SYSTEM) |> 
  summarise(n = n(), VOLUME = sum(VOLUME)) 

# After removing origin == destination
sp_matrix_df3 |> 
  group_by(SYSTEM) |> 
  summarise(n = n(), VOLUME = sum(VOLUME)) 

# After removing SINAFLOR exports
sp_matrix_df4 |> 
  group_by(SYSTEM) |> 
  summarise(n = n(), VOLUME = sum(VOLUME)) 



## Removing duplicates by total sum of unique actor-mun relation -----------

# Year-to-year

# Set parameters
# data to deduplicate
data <- sp_matrix_df4

#analysis_period: already at the beginning of the analysis

#start empty data frame to store results
dedupe_results <- tibble()
#start empty data frame to store result stats
dedupe_stats <- tibble()

# Start deduplication loop 
for (i in analysis_period) {
  
  # testing for one year
  # i = 2009
  
  # Build unique actor-municipality grouping
  dedupe_key <-  data |>
    filter(ID_YEAR == i) |> 
    group_by(CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN, ID_YEAR, SYSTEM) 
  
  # Save original indices prior to summarizing
  indices <- group_rows(dedupe_key) 
  
  # Build a key to remove equal actor-mun values between SINAFLOR and SISFLORA
  dedupe_key_df1 <- dedupe_key |> 
    summarise(VOLUME = round(sum(VOLUME)), .groups = "keep") |>
    #build unique keys, complete for CNPJs and only with 3 digits for CPF
    mutate(DIGITS = case_when(nchar(CPF_CNPJ_ORIGIN) > 11 ~ CPF_CNPJ_ORIGIN,
                              nchar(CPF_CNPJ_ORIGIN) == 11 ~ str_sub(CPF_CNPJ_ORIGIN, 1, 3),
                              TRUE ~ as.character(NA))) |>
    unite(DIGITS, GEOCMUN_ORIGIN, col = "KEY_DUP", sep = "_", remove = FALSE) |>
    select(-DIGITS) |> 
    rowid_to_column() |> 
    ungroup()
  
  # Find duplicate rows to remove from SINAFLOR
  dedupe_key_df2 <- dedupe_key_df1 |>
    group_by(KEY_DUP, VOLUME) |> 
    mutate(RM = n()) |>
    unite(SYSTEM, RM, col = "TEMP", sep = "-", remove = FALSE) |>
    filter(TEMP != "SINAFLOR-2") |> # What if is two similar from sinaflor? No because it first summarizes necessarily by actor/mun/year/system... two matches from the same system would not be possible
    select(-c(RM)) |>
    ungroup()
  
  pull_rm <- anti_join(dedupe_key_df1, dedupe_key_df2, by = "rowid") |> pull(rowid)
  
  rows_to_rm <- indices[pull_rm] |> unlist()
  
  deduped <- dedupe_key |> 
    ungroup() |> 
    rowid_to_column() |> #to check whether correct rows have been dropped
    slice(-rows_to_rm)
  
  
  # Collect stats
  n_rm <- length(rows_to_rm)
  vol_rm <- (data |> filter(ID_YEAR == i) |> summarise(sum(VOLUME))) - 
    (deduped |>  summarise(sum(VOLUME)))
  
  stats <- tibble(N_RM = n_rm, VOL_RM = vol_rm, ID_YEAR = i) #not true volume because of rounding
  
  dedupe_stats <- bind_rows(dedupe_stats, stats)
  
  # Save deduped dataframe
  dedupe_results <- bind_rows(dedupe_results, deduped)
  
}

rm(dedupe_key, dedupe_key_df1, dedupe_key_df2, indices, stats, deduped, i, n_rm, 
   pull_rm, rows_to_rm)

sp_matrix_df5 <- dedupe_results

rm(dedupe_results, dedupe_stats)



# How much deduplication this logic affords? 
sp_matrix_df4 |> 
  group_by(SYSTEM) |> 
  summarise(n = n(), VOLUME = sum(VOLUME)) 

sp_matrix_df5 |> 
  group_by(SYSTEM) |> 
  summarise(n = n(), VOLUME = sum(VOLUME)) 

243893-231613 
# N.B. Deduplication only discards and extra 12280. The deduplication done here
# is more important (i.e. removes much more volume) when SINAFLOR origin ==
# destination were not removed/addressed. But broadly just keep in mind it would be
# excellent to have a robust and systematic way for removal of duplication such
# as the data shown in Ibama 2019 where "Type of Guide" variable has a
# state-level flow observation.



## Roundwood equivalent conversion ------------------------------------------

rwe_sp_pre <- sp_matrix_df5 |> 
  # bring in data on conversion factor
  left_join(conversion_key, by = "PRODUCT") |> 
  # perform conversion 
  mutate(RWE = VOLUME*(1/VYC*100)) |> 
  select(-c(VYC, VYC_TEMP)) |> 
  # summarize similar flows
  group_by(CPF_CNPJ_ORIGIN,  GEOCMUN_ORIGIN, UF_ORIGIN, 
           CPF_CNPJ_DESTINATION, GEOCMUN_DESTINATION, UF_DESTINATION,
           PRODUCT, PERMIT, PROCESS, PERMIT_NUM_AUTEX_SERIE, 
           ID_YEAR, SYSTEM, PRODUCT_GROUP) |> 
  summarise(RWE = sum(RWE)) |> 
  arrange(desc(PRODUCT)) |> 
  unite("CPF_CNPJ_GEOCMUN_ORIGIN", CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN) |> 
  unite("CPF_CNPJ_GEOCMUN_DESTINATION", CPF_CNPJ_DESTINATION, GEOCMUN_DESTINATION) |> 
  ungroup()



## Contextual stats  -----------------------------------------------------------

#TODO: under development; add any contextual stats needed to support rationale
#for removals and assessment of magnitude of changes pre/post RWE conversion.

## Visualizing product groups by conversion -------------------------------------

rwe_sp_pre |>
  ggplot() +
  geom_col(aes(y = RWE, x = ID_YEAR, fill = PRODUCT_GROUP)) +
  scale_fill_manual(values = c("#DCB795", "#B1A68B", "#8CA298", "#6598A4",
                               "#4582A5", "#31679F", "#274C90", "#202E7C")) + # collors4all scico lapaz 11 : c("#FFF2F2", "#FAD6C2", "#DCB795", "#B1A68B", "#8CA298", "#6598A4", "#4582A5", "#31679F", "#274C90", "#202E7C", "#190C65")
  labs(y = expression(paste("Roundwood Equivalent (  ",  m^{3}, ")" )),
       x = "") +
  ggtitle("Product group shares in roundwood equivalent")


# Clean env. 
rm(cpf_cnpj_all, cpf_cnpj_all_rm, cpf_cnpj_destination, cpf_cnpj_destination_rm, 
   cpf_cnpj_origin, cpf_cnpj_origin_rm,
   data, vol_after, vol_before, vol_by_lp, vol_rm, 
   add_final_cons, add_final_cons_df1, add_own_consumption)


# Z matrix, Part 2: Wrangling for square matrix set up ---------------------

# The Z matrix requires all actors involved in the transactions as well as
# producers.

# Summarise RWE for all unique origin-destination of actor-municipality involved
# in timber transactions
rwe_sp <- rwe_sp_pre  |>  
  select(CPF_CNPJ_GEOCMUN_ORIGIN, CPF_CNPJ_GEOCMUN_DESTINATION, RWE) |>   
  group_by(CPF_CNPJ_GEOCMUN_ORIGIN, CPF_CNPJ_GEOCMUN_DESTINATION) |>   
  summarise(RWE = sum(RWE)) |>   
  ungroup()

# list of all CPF_CNPJ of origin 
all_CPF_CNPJ_GEOCMUN_origin <- rwe_sp |>  
  distinct(CPF_CNPJ_GEOCMUN_ORIGIN) |>  
  rename(CPF_CNPJ_GEOCMUN = CPF_CNPJ_GEOCMUN_ORIGIN)

# list of all CPF_CNPJ of destination 
all_CPF_CNPJ_GEOCMUN_destination <- rwe_sp |>  
  distinct(CPF_CNPJ_GEOCMUN_DESTINATION) |>  
  rename(CPF_CNPJ_GEOCMUN = CPF_CNPJ_GEOCMUN_DESTINATION)

# combine all unique actors involved in sp transactions
all_CPF_CNPJ_GEOCMUN <- bind_rows(all_CPF_CNPJ_GEOCMUN_origin, #includes producer given this is a subset of origin
                                  all_CPF_CNPJ_GEOCMUN_destination) |>   
  distinct()

# Ensure correct set up of square matrix by adding all unique actors to the
# summarized data frame rwe_sp. 
rwe_sp_all_actors <- rwe_sp |>   
  #Departing from summarized RWE df, add 
  full_join(all_CPF_CNPJ_GEOCMUN, by = c("CPF_CNPJ_GEOCMUN_ORIGIN" = "CPF_CNPJ_GEOCMUN")) |>   
  full_join(all_CPF_CNPJ_GEOCMUN, by = c("CPF_CNPJ_GEOCMUN_DESTINATION" = "CPF_CNPJ_GEOCMUN")) |>   
  mutate(CPF_CNPJ_GEOCMUN_DESTINATION = case_when(
    is.na(CPF_CNPJ_GEOCMUN_DESTINATION) ~ CPF_CNPJ_GEOCMUN_ORIGIN, TRUE ~ CPF_CNPJ_GEOCMUN_DESTINATION)) |>   
  mutate(CPF_CNPJ_GEOCMUN_ORIGIN = case_when(
    is.na(CPF_CNPJ_GEOCMUN_ORIGIN) ~ CPF_CNPJ_GEOCMUN_DESTINATION, TRUE ~ CPF_CNPJ_GEOCMUN_ORIGIN)) |>   
  mutate(RWE = case_when(is.na(RWE) ~ 0, TRUE ~ RWE)) |>   
  ungroup() 


# Transforming the tibble to square-matrix format with Reshape2::acast
Z <- rwe_sp_all_actors |>   
  reshape2::acast(CPF_CNPJ_GEOCMUN_DESTINATION ~ CPF_CNPJ_GEOCMUN_ORIGIN, 
                  fun.aggregate = sum, value.var = "RWE") 

#TODO: script improvement; use new functions by tidyr given the phase out of
#reshape2. Look closer at expand and complete to build the square matrix.

# Check whether there are any values for the diagonal 
sum(diag(Z))

# rm(sp_matrix, sp_matrix_df1, sp_matrix_df2, sp_matrix_df3, sp_matrix_df4, sp_matrix_df5,
#    all_CPF_CNPJ_GEOCMUN_origin, all_CPF_CNPJ_GEOCMUN_destination, rwe_sp_all_actors, rwe_sp_pre)



# P vector, Part 2: Wrangling for additions of all actors in the p vector ------

# The p vector must be of same length that of Z, thus we want to add all unique
# actors to the producers vector as well (setting production to zero)

# Add all unique actors to producers vectors
# additional actors are assigned a production value of 0
p_all_CPF_CNPJ_GEOCMUN <- p_sp |>   
  full_join(all_CPF_CNPJ_GEOCMUN, by = c("CPF_CNPJ_GEOCMUN_ORIGIN" = "CPF_CNPJ_GEOCMUN")) |>
  rename(CPF_CNPJ_GEOCMUN = CPF_CNPJ_GEOCMUN_ORIGIN) |>
  mutate(VOLUME_SUM = case_when(is.na(VOLUME_SUM) ~ 0, TRUE ~ VOLUME_SUM)) |> 
  group_by(CPF_CNPJ_GEOCMUN) |> 
  summarise(VOLUME_SUM = sum(VOLUME_SUM)) |> 
  ungroup()


# Transform a data frames/tibble into matrices while keeping rownames as indices
# It is not good practice to mic functions/analyses, but this is still a prototype script
make_matrix <- function(df, rownames = NULL){
  my_matrix <-  as.matrix(df)
  if(!is.null(rownames))
    rownames(my_matrix) = rownames
  my_matrix
} 


# P vector
p <- make_matrix(select(p_all_CPF_CNPJ_GEOCMUN, -CPF_CNPJ_GEOCMUN),
                 pull(p_all_CPF_CNPJ_GEOCMUN, CPF_CNPJ_GEOCMUN))  

# Sort p vector in ascending order 
p <- as.matrix(p[ order(rownames(p)), ])

# acast used in Z orders indices by default. One can test broadly if these match: 
Z[nrow(Z), , drop = FALSE] #|> View()
p[nrow(p), , drop = FALSE]



# P vector, Part 3: Wrangling for outer state flows -------------------------

# All timber inflows coming from outer state are summed up and added in the
# production vector. Since it is out of the scope of the study to analyse place
# of origin of outer state flows we consider these accounted for (volume not
# added as a discrepancy/status valid)

# Create RWE_OUTER_STATE vector that captures volume inflows from other states
p_outer_state <- colSums(Z) |> 
  enframe(name = "CPF_CNPJ_GEOCMUN", value = "RWE") |> 
  mutate(RWE_OUTER_STATE = case_when(!str_detect(CPF_CNPJ_GEOCMUN, "_15") ~ 
                                       RWE, TRUE ~ 0)) |> 
  select(-RWE)

# Saving intermediary objects
# save(p_outer_state, file="./data/processed/p_outer_state.Rdata")

sum(p_outer_state$RWE_OUTER_STATE) 
# At VYC 44.45, 173031.08
# At VYC 35, 219219.11
# At VYC 53.9, 143038.85

# p from matrix to tibble
p_para <- p |> enframe(name = "CPF_CNPJ_GEOCMUN", value = "RWE_TEMP") 

p <- left_join(p_para, p_outer_state) |> 
  mutate(RWE = RWE_TEMP + RWE_OUTER_STATE) |> 
  select(-c(RWE_TEMP, RWE_OUTER_STATE)) |> 
  deframe()



# x, DMI, the "Domestic Material Input" -------------------------------------

# x or DMI vector is obtained by x <-  p + Z*i 

# Set summation vector i (vector of ones)
i <- matrix(1, length(p), ncol = 1) 

# Last checks to objects: 

# p vector
# view(p)
# Here includes roundwood and inflows from outer state. 
# sum(p) 

# Z matrix
# view(Z)


# Vector of domestic material input (DMI, equals production plus inflows)
dmi <- p + Z%*%i 

sum(dmi==0) 
# N.B. This is the number of actors that do not produce, do not receive but send 
# timber forward. 

# There are actors that do not produce nor receive any inflows, but that
# send/sell products forward. This is where the risk of introducing illegal
# timber in the supply chain is the highest. Other reasons, nonetheless, must be
# kept in mind. Sender could have received timber in previous year; sender could
# have not reported correctly its CPF_CNPJ, which is used to link senders and
# receivers thus breaking the chain between production and final consumption,
# issues with estimates given conversion factors used. 

# Additionally, implementation-wise these also represent a zero on the dmi
# vector. In the next calculation step, the dmi is diagonalized and inverted in
# order to obtain the coefficient matrix. The issue with the zeros is that they
# yield a determinant that is zero, not allowing for the matrix inversion. We
# add a dummy variable so we can keep track of missing outflows connected with
# such cases and in parallel mitigate this issue, moving forward with the
# calculations in the most efficient way (i.e when compared to polynomial aprox.
# tested; other possibilities to test are; Moore-penrose pseudoinverse; Gauss
# Jordan elimination)



# Z matrix, Part 3: Addressing negative relative consumption (dummy addition) ----

# Creates a vector where if apparent consumption is positive or zero, the value
# is set to zero, but if negative than we get the absolute value.
cons_discrep <- ifelse((dmi - colSums(Z)) < 0, abs((dmi - colSums(Z))) ,0) |>
  as.vector() |>
  setNames(rownames(Z))

# Saving intermediate objects
# save(cons_discrep, file = "./data/temp/discrepancy.RData")

# Apparent consumption totals:
sum(dmi - colSums(Z))
# Overall discrepancy
sum(cons_discrep)
# Share of missing flows
sum(cons_discrep)*100/(sum(cons_discrep) + sum(dmi - colSums(Z)))



# Create a sequence of dummies for each row 
suffix <- seq(1:length(cons_discrep))
dummy_names <- paste(names(cons_discrep), suffix, sep = "D")
dummy <- diag(cons_discrep) |>
  magrittr::set_rownames(names(cons_discrep)) |>
  magrittr::set_colnames(dummy_names) 

# From dummy, remove all that zero or nearing zero (because we do not want to
# reintroduce zeros in the diagonal)
cols_to_remove <- which(colSums(dummy) < 1)
dummy <- dummy[,-cols_to_remove]

# Can check dimension before and after <1
dim(dummy)

# Create a sequence of reciprocal empty rows, which have the number of dummy rows
# and the number of Z + dummy columns 
dummy_reciprocal <- matrix(0, nrow = dim(dummy)[2], ncol = dim(dummy)[1]+dim(dummy)[2]) |>
  magrittr::set_rownames(colnames(dummy))

# Just checking dimensions
dim(dummy)
dim(dummy_reciprocal)
which(colSums(dummy) == 0) #indeed, no zero being reintroduced.

# Addition of dummy variable to the Z matrix
Zd <- Z |>
  cbind(dummy) |>
  rbind(dummy_reciprocal)

# Sum of the new diagonal is still zero
sum(diag(Zd))

#N.B.: This step adds discrepant values as a column in Z and is now just being
#called Zd (Z that includes discrepancies). This should be thought of as
#production + inflows - outflows (thus dmi - outflows, thus like apparent
#consumption), with the exception that if p + i - o is positive or zero we leave
#this value unchanged. If this value is negative, we add this a "dummy inflow".
#This allows us to still calculate the relative env. impact associated with all
#the other transactions.



# P vector, Part 3: Wrangling for final p vector ------------------------------

# Adding the sum of the inflows from the dummy
dummy_prod <- colSums(dummy) |> # sum of cols because dummy is initially diagonalized
  as.vector() |>
  setNames(colnames(dummy)) |>
  make_matrix()

# New production vector with added discrepancies 
pd <- rbind(p, dummy_prod)

# Fixing i vector
id <- matrix(1, length(pd), ncol = 1) 

# Calculate a new dmi 
dmid <- pd + Zd%*%id   

# Check remaining zeros 
length(which(dmid == 0)) 



#x, DMI, Part 2: Addressing zeros in the dmi  --------------------------------------------

# The new DMI with added discrepancies reintroduced zeros so we will check on
# these. Most of such can be seen as small residual balances/values very close
# to zero which removal is understood as negligible to the analysis.

#TODO: low priority improvement; can write a function before the dummy where if
#no inflow, no production and marginal outflow, remove record.

# # Which are these actors?
# which(dmid == 0)
# zero <- which(dmid == 0)
# dmid[zero, ]
# # > dmid[zero, ]
# # 02592197000120_1508001 04625345000191_1501402 
# # 0                      0 
# # 05418344000139_1504703 07465844000157_1504950 
# # 0                      0 
# # 10710621000112_1507953 34910679000178_1508001 
# # 0                      0 
# 
# # Let's check these: 
# 
# #1)
# # Does not produce anything
# p["02592197000120_1508001", ]
# # Does not receive anything 
# inflows <- rowSums(Z)
# inflows["02592197000120_1508001"]
# # Hence dmi == 0
# dmi["02592197000120_1508001", ]
# 
# # But its has an outflow (despite residual)
# outflows <- colSums(Z)
# outflows["02592197000120_1508001"]
# 
# 
# 
# #2)
# #Same actors but at the Zd level vs. Z
# pd["02592197000120_1508001", ] # this is the same as p["02592197000120_1508001", ] 
# sum(Zd["02592197000120_1508001", ]) # no inflows, again we already new
# dmid["02592197000120_1508001", ] # thus no surprises
# 
# outflowsZd <- colSums(Zd)
# outflowsZd["02592197000120_1508001"] # again, marginal flow
# 
# 
# # Testing the others:
# p["04625345000191_1501402", ]
# inflows <- rowSums(Z)
# inflows["04625345000191_1501402"]
# dmi["04625345000191_1501402", ]
# outflows <- colSums(Z)
# outflows["04625345000191_1501402"]
# 
# p["05418344000139_1504703", ]
# inflows <- rowSums(Z)
# inflows["05418344000139_1504703"]
# dmi["05418344000139_1504703", ]
# outflows <- colSums(Z)
# outflows["05418344000139_1504703"]
# 
# #...

#So you can see that the removal of the dummy < 1 caused this actor with a
#marginal flow not to have a dummy in the new production vector. With a dmi of
#nothing, it is understandable that this is what reintroduces the zero in the
#diag. 

#So what we do is that we remove those flows with  no prod, no inflow and
#marginal outflow.

# Remove rows from dmid
rows_to_remove <- which(dmid == 0)
dmid <- as.matrix(dmid[-rows_to_remove, ])

# Remove rows from Zd
Zd <- as.matrix(Zd[-rows_to_remove, ])
Zd <- as.matrix(Zd[, -rows_to_remove])

#Remove rows from pd
pd <- as.matrix(pd[-rows_to_remove, ])


#Clean env.
rm(dummy, dummy_prod, dummy_reciprocal, dmi, p, p_para, zero, suffix, rows_to_rm, 
   rows_to_remove, outflows, outflowsZd, dummy_names)



# A, Matrix of shares of "exports" in DMI, according to "country" of destination ----

# A = Z * (x_hat^-1)

# Set up x_hat (diagonalize vector x)
x_hat <- as.vector(dmid) |>diag(nrow = length(dmid))

det(x_hat)  # If there are zeros on the dmi diagonal, the determinant will be zero.

# Coefficient matrix of "outflows" or outflows share. This process is also
# referred to as the "normalization" of [transaction] flows "Z"
A <- Zd %*% solve(x_hat) |>
  magrittr::set_colnames(rownames(Zd))



# R, Matrix of DMI, according to country of origin ----------------------------

# R = (I - A)^-1 * p_hat

# Establish p_hat, the diagonalized production vector with the discrepancy dummy
pd_hat <- as.vector(pd) |> diag(nrow = length(pd))


R <- (solve(diag(dim(A)[1]) - A)) %*% pd_hat |> 
  magrittr::set_colnames(rownames(A))



# c, Vector of the share of domestic/internal consumption in total DMI -----------------

c <- as.vector((dmid - colSums(Zd))/dmid) |>  setNames(rownames(dmid)) 

# Clean env.
rm(pd, pd_hat, x_hat, Z, Zd, A, i, id)



# Rc, Matrix of apparent "internal" consumption, according to "actor" of origin --------

# Matrix of consumption in RWE according to origin (take a 10 min break)
start_time <- Sys.time()
Rc <- diag(c) %*% R |> 
  magrittr::set_rownames(colnames(R))
end_time <- Sys.time()
end_time - start_time



# Clean env. 
# rm(R, c, start_time, end_time)



# Transform Rc back to dataframe format for easier mapping and visualization
io_CPF_CNPJ_GEOCMUN <- Rc |>
  reshape2::melt(varnames = c("DESTINATION", "ORIGIN"), value.name = "RWE") 

# save(io_CPF_CNPJ_GEOCMUN, 
#      file="./data/temp/io_output.Rdata")


# Environmental Extensions ----------------------------------------------------

# The objective with the environmental extension is that we can determine the
# share of: i) valid, ii) valid with yield >99 percentile, iii) invalid &
# missing and iv) autex from a consumer perspective.

# The first step is to get to vector of same length of Rc with share of these. 

# Start from the production vector (make sure removals are the same) 
p_sp_lp <- transport_df2 |> # If not starting from transport_df2, than apply params (period, species, status)
  filter(grepl(c("TORA"), PRODUCT), 
         TYPE_GF_DOF %in% c("GF1", "DOF"),
         UF_ORIGIN == "PA") |> 
  unite(PERMIT, PERMIT_NUM_AUTEX_SERIE, col = "RM", sep = "_", remove = FALSE) |> 
  filter(RM != "NA_NA") |> 
  select(-RM) |> 
  filter(!grepl(c("^PATIO"), TYPE_ORIGIN)) |> 
  filter(!is.na(GEOCMUN_ORIGIN)) |> 
  subset(!(CPF_CNPJ_ORIGIN == CPF_CNPJ_DESTINATION))

# Pre-process permit data from the p vector for match with lp information 
# Address state-level logging permits (AUTEF). Separate PROCESS and PERMIT into
# four columns (tidying) for joining data set
p_sp_lp <- p_sp_lp |> 
  separate(PROCESS, into = c("PROCESS_YEAR", "PROCESS_NUMBER"), sep = "/")|>  
  separate(PERMIT, into = c("PERMIT_NUMBER", "PERMIT_YEAR"), sep = "/")|>  
  mutate(PROCESS_NUMBER = str_remove(PROCESS_NUMBER, "^0+"))|>  
  mutate(PERMIT_NUMBER = str_remove(PERMIT_NUMBER, "^0+")) |> 
  mutate(LP_REF = case_when(!is.na(PERMIT_NUM_AUTEX_SERIE) ~ PERMIT_NUM_AUTEX_SERIE,
                            TRUE ~ as.character(NA))) |>
  mutate(LP_REF = if_else(is.na(LP_REF), PERMIT_NUMBER, LP_REF)) |> 
  select(-c(TYPE_GF_DOF, STATUS_GF_DOF,  SCIENTIFIC_NAME, TYPE_ORIGIN, 
            TYPE_DESTINATION, LATITUDE_ORIGIN, LATITUDE_DESTINATION, LONGITUDE_ORIGIN,
            LONGITUDE_DESTINATION, PRODUCT))

# No missing LPs as expected
p_sp_lp |> filter(is.na(LP_REF)) |> count()

# joining volume and permit data
join_p_lp_data <- p_sp_lp  |>  
  left_join(lp, by = c( "LP_REF" = "LP_NUM")) 

# How do we treat the share of "lp not found/irregular/incomplete"?
data_env_ext <- join_p_lp_data |> 
  mutate(LP_SOURCE = case_when(is.na(LP_SOURCE) ~ "UNDETERMINED", TRUE ~ LP_SOURCE)) |>
  mutate(LP_STATUS = case_when(is.na(LP_STATUS) ~ "UNDETERMINED", TRUE ~ LP_STATUS))

data_env_ext <- data_env_ext |> 
  mutate(STATUS_GROUP1 = case_when(LP_STATUS %in% c("EXPIRED",
                                                    #"EMITIDA OFERTA", #Or AUTEX, decide as I have all info
                                                    "EXTENDED", 
                                                    "EXTENDED_CANCELLED SUSPENSION") ~ "VALID", 
                                   LP_STATUS %in% c(#"ESTORNADO ITEM", #Or AUTEX, decide as I have all info
                                     "SUSPENDED", 
                                     "CANCELLED", 
                                     "CANCELLED_OTHERS", 
                                     "CANCELLED_PERMIT SUBSTITUTION", 
                                     "CANCELLED_ELABORATION FAILURE", 
                                     "CANCELLED_NONCOMPLIENCE WITH CONDITIONS", 
                                     "CANCELLED_ILLEGALITY", 
                                     "MISSING ACTIVATION DATE", 
                                     "UNDETERMINED") ~ "INVALID_MISSING", 
                                   LP_STATUS %in% c("EMITIDA OFERTA", 
                                                    "ESTORNADO ITEM") ~ "AUTEX", 
                                   TRUE ~ "INVALID_MISSING")) 


# Calculate the share of volume status per CPF_CNPJ_ORIGIN (volume origin)
share_status <- data_env_ext |> 
  group_by(CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN, LP_REF, STATUS_GROUP1) |> # ID_YEAR
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup() 

# Check total of lp in tranp data
p_sp_lp |> distinct(LP_REF) |> count() #1292

# permit data from the transport side
vol_by_lp <- p_sp_lp |>
  group_by(LP_REF) |>
  summarise(VOLUME = sum(VOLUME))

# joining volume and permit data
join_vol_lp_data <- vol_by_lp |>  
  left_join(lp, by = c( "LP_REF" = "LP_NUM")) 

# How do we treat the share of "lp not found/irregular/incomplete"?
valid_overst_yield <- join_vol_lp_data |> 
  filter(LP_STATUS %in% c("EXPIRED","EXTENDED", "EXTENDED_CANCELLED SUSPENSION")) |> 
  mutate(YIELD = VOLUME/AREA_HA) |> 
  mutate(OVERSTATED_Q99 = if_else(YIELD > 2.5, 1, 0)) |> 
  filter(OVERSTATED_Q99 == 1) |> 
  select(LP_REF, OVERSTATED_Q99) 

share_status <- share_status |> 
  left_join(valid_overst_yield, by = 'LP_REF') |>
  mutate(STATUS_GROUP1 = case_when(OVERSTATED_Q99 == 1 ~ "VALID_OVERSTATED", 
                                   TRUE ~ STATUS_GROUP1)) |> 
  select(-c(OVERSTATED_Q99, LP_REF))

# Create a data frame with shares of valid, invalid & missing and autex
share_status <- share_status |>
  unite("CPF_CNPJ_GEOCMUN", CPF_CNPJ_ORIGIN:GEOCMUN_ORIGIN) |> 
  group_by(CPF_CNPJ_GEOCMUN, STATUS_GROUP1) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup() |> 
  pivot_wider(names_from = STATUS_GROUP1, values_from = VOLUME, values_fill = 0) |>
  mutate(TOTAL = VALID + VALID_OVERSTATED + INVALID_MISSING + AUTEX) |> 
  mutate(SHARE_VALID = VALID/TOTAL) |> 
  mutate(SHARE_VALID_OVERSTATED = VALID_OVERSTATED/TOTAL) |> 
  mutate(SHARE_INVALID_MISSING = INVALID_MISSING/TOTAL) |> 
  mutate(SHARE_AUTEX = AUTEX/TOTAL)

# Checking total volume still the same
sum(share_status$TOTAL)



# Addressing the fact Rc has discrepancy columns: 
# Extract Rc matrix index to a vector 
E_irisk_df1 <- dimnames(Rc)[[1]]  |>
  tibble() |>
  rename(E = "dimnames(Rc)[[1]]") |>
  separate(E, into = c("CPF_CNPJ_GEOCMUN", "INDEX"), sep = "D") |>
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", #Index 0 means no discrepancy for certain flow
                           INDEX != "0" ~ "1", #Index 1 means discrepancy
                           TRUE ~ INDEX)) 

# Check count
E_irisk_df1 |> count(INDEX)

# Join share of valid, invalid & missing and autex to index
E_irisk_df2 <- E_irisk_df1 |> 
  # remove discrepant flows for the join
  filter(INDEX != "1") |>
  # add the share of volume known by CPF_CNPJ_GEOCMUN
  left_join(share_status, by = 'CPF_CNPJ_GEOCMUN') |> 
  select(-c(VALID, INVALID_MISSING, AUTEX, TOTAL)) |>
  # set all missing values to zero   
  mutate(SHARE_VALID = case_when(is.na(SHARE_VALID) ~ 0, TRUE ~ SHARE_VALID)) |> 
  mutate(SHARE_VALID_OVERSTATED = case_when(is.na(SHARE_VALID_OVERSTATED) ~ 0, 
                                            TRUE ~ SHARE_VALID_OVERSTATED)) |>
  mutate(SHARE_INVALID_MISSING = case_when(is.na(SHARE_INVALID_MISSING) ~ 0, 
                                           TRUE ~ SHARE_INVALID_MISSING)) |> 
  mutate(SHARE_AUTEX = case_when(is.na(SHARE_AUTEX) ~ 0, TRUE ~ SHARE_AUTEX))

# Add the discrepancy flows (not zero) back to the df (able to do this because
# discrepancy values were added last, so no need to arrange order)
E_irisk_df3 <-  E_irisk_df1 |>
  filter(INDEX != "0") |>
  mutate(SHARE_VALID = 0) |> 
  mutate(SHARE_VALID_OVERSTATED = 0) |>  
  mutate(SHARE_INVALID_MISSING = 0) |> 
  mutate(SHARE_AUTEX = 0) 

#TODO: script improvement; move from excessive breakdown/repetition in object
#assignment to function. 
E_irisk_valid <- bind_rows(E_irisk_df2, E_irisk_df3) |>
  select(CPF_CNPJ_GEOCMUN, SHARE_VALID) |>
  deframe()

E_irisk_valid_overst <- bind_rows(E_irisk_df2, E_irisk_df3) |>
  select(CPF_CNPJ_GEOCMUN, SHARE_VALID_OVERSTATED) |>
  deframe()

E_irisk_invalid_missing <- bind_rows(E_irisk_df2, E_irisk_df3) |>
  select(CPF_CNPJ_GEOCMUN, SHARE_INVALID_MISSING) |>
  deframe()

E_irisk_autex <- bind_rows(E_irisk_df2, E_irisk_df3) |>
  select(CPF_CNPJ_GEOCMUN, SHARE_AUTEX) |>
  deframe()

# estimation of consumption from known and unknown sources 
valid_cons <- Rc %*% diag(E_irisk_valid) |> 
  magrittr::set_colnames(rownames(Rc))

valid_overst_cons <- Rc %*% diag(E_irisk_valid_overst) |> 
  magrittr::set_colnames(rownames(Rc))

invalid_missing_cons <- Rc %*% diag(E_irisk_invalid_missing) |> 
  magrittr::set_colnames(rownames(Rc))

autex_cons <- Rc %*% diag(E_irisk_autex) |> 
  magrittr::set_colnames(rownames(Rc))

#TODO: script improvement; apply enframe/ deframe; use of tidyr in replacement
#of reshape2, melt and dcast. 

# Data frame with values for the environmental extension
ee_valid_cons <- valid_cons |>
  reshape2::melt(varnames = c("DESTINATION", "ORIGIN"), value.name = "RWE") |>
  mutate(DESTINATION = as.character(DESTINATION)) |>
  mutate(ORIGIN = as.character(ORIGIN))

ee_valid_overst_cons <- valid_overst_cons |>
  reshape2::melt(varnames = c("DESTINATION", "ORIGIN"), value.name = "RWE") |>
  mutate(DESTINATION = as.character(DESTINATION)) |>
  mutate(ORIGIN = as.character(ORIGIN))

ee_invalid_missing_cons <- invalid_missing_cons |>
  reshape2::melt(varnames = c("DESTINATION", "ORIGIN"), value.name = "RWE") |>
  mutate(DESTINATION = as.character(DESTINATION)) |>
  mutate(ORIGIN = as.character(ORIGIN))

ee_autex_cons <- autex_cons |>
  reshape2::melt(varnames = c("DESTINATION", "ORIGIN"), value.name = "RWE") |>
  mutate(DESTINATION = as.character(DESTINATION)) |>
  mutate(ORIGIN = as.character(ORIGIN))

# Renaming object in support of reproduction of "12_mapping-illegality-risks.R"
rw <- p_sp_lp

# Clean env. 
rm(Rc, p_sp, p_sp_lp, join_p_lp_data, join_vol_lp_data, share_status, data_env_ext,
   E_irisk_df1, E_irisk_df2, E_irisk_df3,
   E_irisk_valid, E_irisk_valid_overst, E_irisk_invalid_missing, E_irisk_autex,
   valid_cons, invalid_missing_cons, autex_cons, analysis_period, base_vyc, species, 
   transaction_status)

rm(all_CPF_CNPJ_GEOCMUN_destination, all_CPF_CNPJ_GEOCMUN_origin, dmid, all_CPF_CNPJ_GEOCMUN, 
   conversion_key, p_all_CPF_CNPJ_GEOCMUN, rwe_sp, rwe_sp_all_actors, rwe_sp_pre, sp_matrix, 
   sp_matrix_df1, sp_matrix_df2, sp_matrix_df3, sp_matrix_df4, sp_matrix_df5, rw_sp, 
   valid_overst_yield, valid_overst_cons, cols_to_remove)

# Saving outputs 
# Env. extensions 
# save(ee_valid_cons, ee_valid_overst_cons, ee_invalid_missing_cons, ee_autex_cons, 
#    file="./data/temp/env_extensions.Rdata")

# Objects used in "12_mapping-illegality-risks.R"

# save(ee_valid_cons, ee_valid_overst_cons, ee_invalid_missing_cons, ee_autex_cons,
#       io_CPF_CNPJ_GEOCMUN, p_outer_state, cons_discrep, rw, lp, transport_df2,
#       file="./data/processed/illegality-risk-vyc44-1.X.Rdata")




# References --------------------------------------------------------------

options(citation.bibtex.max=999)

citation()
citation("tidyverse")
citation("scales")
citation("janitor")
citation("sf")
citation("patchwork")
citation("reshape2") 
citation("expm")

