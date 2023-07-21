# Mapping illegality risks and outputs of the EEIO analysis -------------------

# This script is currently under development/refinement. It contains all code
# needed for the reproduction of the figures and statistics from "Putting
# numbers on timber illegality risk: the case of ipê in Pará", currently under
# review.

# This version includes material for both main and supplementary figures and
# texts. Main figures and associated data wrangling and analyses are presented
# first, followed by supplementary material and it has been organized -- as far
# as possible -- in the same sequence as the main text.

# Reach out if you have suggestions for improvement, feedback or ideas for
# collaboration. See https://github.com/carolsrto/illegality-risk-ns or e-mail
# directly at caroline.franca@chalmers.se



# Setting up the work environment ---------------------------------------------

# Loading libraries 
library(tidyverse)
library(ggpmisc)
library(janitor)
library(geobr)
library(sf)
library(tmap)
library(scatterpie)
library(ggalluvial)
library(scales)
library(patchwork)
library(paletteer)
library(scico)
library(reshape2) 

# Set global options
# disable scientific notation 
options(scipen = 999)  

# ensure decimal places as needed
options(digits=14) 



# Loading data ------------------------------------------------------------

# loading objects from .RData
load("./data/processed/illegality-risk-vyc44-v1.1.Rdata")

# "illegality-risk-vyc44-v1.1.Rdata" loads the following data objects: 

# Base logging permits and subset of transport data:
# - lp: compilation of logging permits data
# - rw: subset of roundwood data transported out of forest of origin (GF/DOF
# compilation for ipê, 2009-2019, all batches received/received with delay). This
# is equivalent to the production vector.
# - transport_df2: subset of all relevant transport transactions.

# Outputs of EEIO model: 
# N.B. Objects reflect the mid-point conversion efficiency of 44.5. To obtain statistics 
# and figures for upper-lower boundaries (i.e. high efficiency: 53.9, low efficiency: 35)
# Go back to the "input_output_model.R" and change VYC to the desired value. 

# - io_CPF_CNPJ_GEOCMUN: main output from the EEIO model
# - ee_valid_cons: env. extension share for valid lp
# - ee_valid_ovest_cons: env. extension share for valid and overstated lp
# - ee_invalid_missing_cons: env. extension share for missing invalid lp
# - ee_autex_cons: env. extension share for issued under federal jurisdiction
# - cons_discrep: vector of discrepancy between outflows and inflows
# - p_outer_state: vector of volume inflows from other states

# Admin boundaries:
# - mun: municipality data, available from:
mun <- geobr::read_municipality(code_muni = "all", year = "2020")
# - states: states data, available from:
states <- geobr::read_state(year = "2020")

# RADAM 
# N.B. Instructions for loading data for the replication of Brancalion et al.
# 2018 code used as a basis for analysis of overstated yield can be found directly 
# in the section "Fig. 2 Species yield estimation". The following loaded data objects 
# can be used to speed up reproduction of Suppl. Fig. S4 and further instructions 
# are provided in the code for independent reproduction of objects.
# - dados_19ha, dados_66ha, dados_370ha: statistics summary of simulation 
# - radam_hist_19ha, radam_hist_66ha, radam_hist_370ha: simulation histograms at 
# different minimum area thresholds
# - means_19ha, means_66ha, means_370ha: simulation means at different minimum 
# area thresholds



# Pre-processing -------------------------------------------------------------

# Pre-processing admin boundaries
# for join, geocode as character in both datasets 
mun$code_muni <- as.character(mun$code_muni)

# extract municipalies centroids
mun_centroid <- 
  st_centroid(mun)|>  
  mutate(X = map_dbl( geom, 1),
         Y = map_dbl(geom, 2))|>  
  as_tibble()|>  
  select(-geom)

#selecting Pará state for better view
states_pa <- states|>  filter(name_state == "Pará")

#selecting Pará state for better view
mun_pa <- mun|>  filter(abbrev_state == "PA")

# # Check out mapping features
# tmap_mode("view")
# 
# #municipalities
# tm_shape(mun) +
#   tm_basemap("Stamen.TonerLite") +
#   tm_polygons(alpha = 0,  border.col = "Red")
# 
# #states
# tm_shape(states) +
#   tm_basemap("Stamen.TonerLite") +
#   tm_polygons(alpha = 0,  border.col = "Red")


# Fig 1 Mapping forest of origin -----------------------------------------

# Summarising volume by permit number (volume entering the supply chain)
vol_by_lp <- rw |>
  group_by(LP_REF) |>
  summarise(VOLUME = sum(VOLUME))

# Joining volume and logging permit data
join_vol_lp_data <- vol_by_lp |>  
  left_join(lp, by = c( "LP_REF" = "LP_NUM")) 

# Capturing missing permit info
data_fig1 <- join_vol_lp_data |> 
  mutate(LP_SOURCE = case_when(is.na(LP_SOURCE) ~ "UNDETERMINED", 
                               TRUE ~ LP_SOURCE)) |>
  mutate(LP_STATUS = case_when(is.na(LP_STATUS) ~ "UNDETERMINED", 
                               TRUE ~ LP_STATUS))
# N.B. Missing permits are those we have simply no record across all potential
# sources of information (e.g.  AUTEF polygons from SIMLAM, AUTEF PDFs from
# SIMLAM, AUTEX from SINAFLOR, supporting datasets such as found in Brancalion
# et al 2018 and the Ibama 2019 studies). If source is not found for such
# permits, then status is also missing, but we may certain permits which have
# source but miss status of logging permits. 

# Grouping status for different level of details (from most aggregate to most
# detailed) 
# Status grouping 1:
data_fig1 <- data_fig1 |> 
  mutate(STATUS_GROUP1 = case_when(LP_STATUS %in% c("EXPIRED",
                                                    "EXTENDED", 
                                                    "EXTENDED_CANCELLED SUSPENSION") ~ "VALID", 
                                   LP_STATUS %in% c("SUSPENDED", 
                                                    "CANCELLED", 
                                                    "CANCELLED_OTHERS", 
                                                    "CANCELLED_PERMIT SUBSTITUTION", 
                                                    "CANCELLED_ELABORATION FAILURE", 
                                                    "CANCELLED_NONCOMPLIENCE WITH CONDITIONS", 
                                                    "CANCELLED_ILLEGALITY", 
                                                    "MISSING ACTIVATION DATE") ~ "INVALID", 
                                   LP_STATUS %in% c("EMITIDA OFERTA", 
                                                    "ESTORNADO ITEM") ~ "AUTEX", 
                                   TRUE ~ as.character(NA)))

# N.B. At this moment we treat AUTEX data as a standalone category, not
# distinguishing between status reported in this dataset (i.e. EMITIDA OFERTA,
# ESTORNADO ITEM, etc.). We found most timber entering the supply chain from
# AUTEX would be connected to "ESTORNADO ITEM", which is a category when credits
# are issued then credited back e.g. because a shipment was not sent, etc. This
# is at odds with the "RECEIVED" cargo from which we derive flows. Thus, given
# ambiguity and lack of metadata to clarify standing of this share we do not
# evaluate status. 

# Data check
data_fig1 |> group_by(LP_STATUS, STATUS_GROUP1) |> count()

# Status grouping 2
data_fig1 <- data_fig1 |> 
  mutate(STATUS_GROUP2 = case_when(LP_STATUS %in% c("EXPIRED", 
                                                    "EXTENDED", 
                                                    "EXTENDED_CANCELLED SUSPENSION") ~ "VALID",
                                   LP_STATUS %in% c("SUSPENDED") ~"SUSPENDED", 
                                   LP_STATUS %in% c("CANCELLED", 
                                                    "CANCELLED_OTHERS", 
                                                    "CANCELLED_PERMIT SUBSTITUTION", 
                                                    "CANCELLED_ELABORATION FAILURE", 
                                                    "CANCELLED_NONCOMPLIENCE WITH CONDITIONS") ~ "CANCELLED", 
                                   LP_STATUS %in% c("CANCELLED_ILLEGALITY") ~ "CANCELLED_ILLEGALITY", 
                                   LP_STATUS %in% c("MISSING ACTIVATION DATE") ~ "MISSING ACTIVATION DATE", 
                                   LP_STATUS %in% c("EMITIDA OFERTA", 
                                                    "ESTORNADO ITEM") ~ "AUTEX", 
                                   TRUE ~ as.character(NA))) 
# Data check
data_fig1 |> group_by(LP_STATUS, STATUS_GROUP2) |> count()


# Status grouping 3
data_fig1 <- data_fig1 |> 
  mutate(STATUS_GROUP3 = case_when(LP_STATUS %in% c("EXPIRED") ~ LP_STATUS, 
                                   LP_STATUS %in% c("EMITIDA OFERTA") ~ "AUTEX", 
                                   LP_STATUS %in% c("EXTENDED", "EXTENDED_CANCELLED SUSPENSION") ~ "EXTENDED",
                                   LP_STATUS %in% c("SUSPENDED") ~ "SUSPENDED", 
                                   LP_STATUS %in% c("ESTORNADO ITEM") ~ "AUTEX",
                                   LP_STATUS %in% c("CANCELLED", "CANCELLED_OTHERS") ~ "CANCELLED",
                                   LP_STATUS %in% c("CANCELLED_PERMIT SUBSTITUTION") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_ELABORATION FAILURE") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_NONCOMPLIENCE WITH CONDITIONS") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_ILLEGALITY") ~ LP_STATUS, 
                                   LP_STATUS %in% c("MISSING ACTIVATION DATE") ~ "MISSING ACTIVATION DATE",
                                   TRUE ~ as.character(NA))) 

# Data check
data_fig1 |> group_by(LP_STATUS, STATUS_GROUP3) |> count()


# Re-leveling factors for better plots 
data_fig1 <- data_fig1 |> 
  mutate(STATUS_GROUP1 = fct_relevel(STATUS_GROUP1, c("VALID", 
                                                      "INVALID", 
                                                      "AUTEX"))) |> 
  mutate(STATUS_GROUP2 = fct_relevel(STATUS_GROUP2, c("VALID", 
                                                      "MISSING ACTIVATION DATE", 
                                                      "SUSPENDED", 
                                                      "CANCELLED", 
                                                      "CANCELLED_ILLEGALITY", 
                                                      "AUTEX"))) |> 
  mutate(STATUS_GROUP3 = fct_relevel(STATUS_GROUP3, c("EXPIRED", 
                                                      "EXTENDED",
                                                      "MISSING ACTIVATION DATE", 
                                                      "SUSPENDED",
                                                      "CANCELLED",
                                                      "CANCELLED_PERMIT SUBSTITUTION", 
                                                      "CANCELLED_ELABORATION FAILURE", 
                                                      "CANCELLED_NONCOMPLIENCE WITH CONDITIONS", 
                                                      "CANCELLED_ILLEGALITY",
                                                      "AUTEX")))



## Fig 1a Map -----------------------------------------------------------------

# Here we map the volume entering the supply chain by municipality and permit of
# origin.

# Municipality layer (All volume entering the supply chain as reported by timber
# transport data; GFs/DOFs)

# Transform geobr municipality data to match transport data for mapping
mun_pa_vol <- mun_pa |> 
  janitor::clean_names() |>
  rename_with(str_to_upper) |>
  mutate(across(where(is.character), toupper)) |>
  mutate(across(c(NAME_MUNI, NAME_STATE), ~stringi::stri_trans_general(., "Latin-ASCII")))

# Summarize roundwood volume (m^3) by mun
vol_by_mun <- rw|>
  group_by(GEOCMUN_ORIGIN) |>
  summarise(VOLUME = sum(VOLUME)) |> 
  mutate(GEOCMUN_ORIGIN = as.character(GEOCMUN_ORIGIN))

# Add multipolygons info 
map_mun_vol <- vol_by_mun |> 
  full_join(mun_pa_vol, by = c('GEOCMUN_ORIGIN' = 'CODE_MUNI')) |> 
  st_as_sf()

# Map base municipality layer
fig1a_base <- ggplot() +
  geom_sf(data = map_mun_vol, aes(fill = VOLUME)) +
  scale_fill_gradientn(name = "Roundwood production\n by municipality", 
                       colours = c("#E4D7BD", "#CFB889", "#B99956", "#A37824"), na.value = "#EAEDE9", 
                       labels = label_number(scale = 1/1000000, suffix = " Mm\u00B3")) + 
  theme_void() +
  theme(legend.title = element_blank())



# Map logging permit layer 

# Check out centroid of municipalities for annotation
mun_centroid |> filter(name_muni == "Santarém")
mun_centroid |> filter(name_muni == "Juruti")
mun_centroid |> filter(name_muni == "Prainha")

# Mapping volume entering the supply chain by Valid, Invalid, AUTEX
fig1a <- fig1a_base +
  geom_point(data = filter(data_fig1, !is.na(STATUS_GROUP1)), 
             aes(x = X, y = Y, size = VOLUME, colour = factor(STATUS_GROUP1)), 
             alpha = 0.5, stroke = FALSE) +
  scale_size(name = "Roundwood production \n by Logging Permit", 
             limits = c(min(data_fig1$VOLUME), 
                        max(data_fig1$VOLUME)), 
             range = c(0, 10), 
             #labels = label_comma(suffix = " m\u00B3"))+ 
             labels = label_number(scale = 1/100000, suffix = " Mm\u00B3"))+ 
  annotate("segment", x = -55.2, xend = -55.2, y = -2.68, yend =  -0.9, 
           colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -56.2, xend = -57.7, y = -2.62, yend =  -2.1, 
           colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -53.7, xend = -52.8, y = -2.12, yend =  -1, 
           colour = "black", size=0.5, alpha=0.8) +
  annotate("text", x = c(-55.2, -57.7, -52.8), y = c(-0.7, -1.9, -0.8), 
           label = c("Santarém", "Juruti", "Prainha") , 
           color="black", size=4 , angle=0, fontface="bold", parse = TRUE) +
  
  scale_colour_manual(name = "Status of \n Logging Permit", 
                      values = c("#2D609B", "#843837", "grey50"), 
                      labels = c("Valid: Expired, Extended", 
                                 "Invalid: Suspended, Cancelled, \n Missing activation date", 
                                 "National jurisdiction (AUTEX)")) +
  guides(colour = guide_legend(override.aes = list(linewidth=3)))+
  labs(title = "a") + 
  theme_void() +
  theme(plot.title.position = "plot", 
        plot.title = element_text(face="bold"))

# N.B. Warnings are for rows with no coordinates



## Fig 1b Bar plot ----------------------------------------------------------

# Status of issued logging permits used to transport roundwood out of forest of
# origin.

key_rw_lp <- data_fig1 |> 
  ungroup() |> 
  select(-VOLUME)

lp_barplot <- rw |> 
  select(PERMIT_NUMBER, ID_YEAR, VOLUME, LP_REF) |> 
  left_join(key_rw_lp, by = "LP_REF") 

data_col <- lp_barplot |> 
  mutate(ID_YEAR = factor(ID_YEAR, levels = 2009:2019)) 

data_line <- lp_barplot |> 
  mutate(ID_YEAR = factor(ID_YEAR, levels = 2009:2019)) |> 
  group_by(ID_YEAR) |> 
  mutate(VOLUME = sum(VOLUME))

# Annotation details for the production line
annotate_vol_year <- data_line |>
  distinct(VOLUME) |> 
  mutate(VOLUME = paste0(round(VOLUME/1000000, digits = 3), " Mm\u00B3")) |> 
  arrange(ID_YEAR) |>  pull()
x <- c(1, 2.5, 3, 4, 5.5, 6, 6.7, 8, 9, 10, 10.7) 
y <- c(0.2, 0.45, 0.8, 0.03, 0.22, 0.97, 0.46, 0.29, 0.61, 0.51, 0.82)

# Percentage by status and production, all permits
fig1b <- ggplot() +
  geom_col(data = data_col, mapping = aes(x = ID_YEAR, y = VOLUME, 
                                          fill = STATUS_GROUP3), position = "fill") +
  scale_fill_manual(name = "Status of \n Logging Permit", 
                    values = c("#2D609B", "#3C79A5", "#A67A60", "#A16959",
                               "#9C5651", "#8F4342", "#792C2C", "#631717", 
                               "#4D0001", "grey50"), 
                    #Color for NA values      
                    na.value="black", 
                    label = c("Expired", 
                              "Extended",
                              "Missing Activation Date",
                              "Suspended", 
                              "Cancelled", 
                              "Cancelled (permit substitution)", 
                              "Cancelled (elaboration failure)", 
                              "Cancelled (non-complience with conditions)", 
                              "Cancelled (illegality)", 
                              "National jurisdiction (AUTEX)", 
                              "Undetermined")) +
  geom_line(data = data_line, group=1, mapping = aes(x = ID_YEAR, y = scales::rescale(VOLUME, to = c(0.05, 0.95))), size = .7) +
  annotate("text", 
           x = x, 
           y = y, 
           label = annotate_vol_year, 
           size = 3) + 
  ylab("") +
  xlab("") +
  labs(title = "b") + 
  theme_minimal() +
  theme(plot.title.position = "plot", 
        plot.title = element_text(face="bold"))

# Check figure
fig1ab <- fig1a/fig1b

# Saving fig1ab, jpeg 
jpeg("./results/fig1ab.jpeg",
     width = 10, height = 12, units = "in",
     bg = "white", res = 700)
print(fig1ab)
dev.off()

# Saving fig1ab, pdf 
# For aliasing effects in Windows OS, open with Acrobat Reader.
ggsave(filename = "./results/fig1ab.pdf",
       width = 10,
       height = 12, 
       bg = "white")


# Clean env.
rm(mun_pa_vol, vol_by_mun, data_col, lp_barplot, key_rw_lp, data_line, 
   fig1a_base, fig1a, fig1b, fig1ab, x, y, annotate_vol_year)



# N.B. Color pallete selection: 
# - used the  cols4all library for exploring different pallettes and later the 
# paletteer and scico libraries, which are loaded with in an used in latter figures.
# - all figures are based on a combination of scico-lapaz and scico-bilbao
# - both are colorblind friendly under sequential setting, but when looking at 
# diverging scales, e.g hcl-blue_red3 one can confirm these scales of red to blue 
# with varying chroma and luminance are often classified as colorfriendly. 

# Color blindness can be double checked with
# https://www.color-blindness.com/coblis-color-blindness-simulator/ The only
# problematic would be Achromatopsia. This can be addressed via the
# supplementary facet wrap on individual view of valid, invalid and AUTEX.


# Supporting Stats (Part 1) --------------------------------------------------

### Volume by mun and lp status ----------------------------------------------

# Wrangling for stats:
# Permit data from the transport side, now including year and municipality
vol_by_lp_stats_mun <- rw |>
  group_by(LP_REF, ID_YEAR, GEOCMUN_ORIGIN) |>
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()

# Joining volume and permit data
join_vol_lp_stats_mun <- vol_by_lp_stats_mun |>  
  left_join(lp, by = c( "LP_REF" = "LP_NUM")) |> 
  mutate(GEOCMUN_ORIGIN = as.character(GEOCMUN_ORIGIN)) |> 
  left_join(mun_pa, by = c("GEOCMUN_ORIGIN" = "code_muni")) |> 
  select(LP_REF, ID_YEAR, VOLUME, GEOCMUN_ORIGIN, MUNICIPALITY, 
         name_muni, LP_SOURCE, LP_STATUS, TYPE, AREA_HA)

# Removing the share of missing permits/lp status undetermined 
data_stats_mun <- join_vol_lp_stats_mun |> 
  mutate(LP_SOURCE = case_when(is.na(LP_SOURCE) ~ "UNDETERMINED", 
                               TRUE ~ LP_SOURCE)) |>
  mutate(LP_STATUS = case_when(is.na(LP_STATUS) ~ "UNDETERMINED", 
                               TRUE ~ LP_STATUS))

data_stats_mun |> 
  filter(LP_STATUS == "UNDETERMINED") |> 
  group_by(LP_STATUS) |> 
  summarise(sum(VOLUME))

data_stats_mun <- data_stats_mun |> filter(LP_STATUS != "UNDETERMINED") 



# Logging permit per municipality for all years: 
data_stats_mun |> 
  group_by(MUNICIPALITY) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  arrange(desc(VOLUME)) |> 
  adorn_totals() |> as_tibble() |> 
  print(n = 100)

top_mun <-   
  data_stats_mun|> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 3)|>  
  pull(MUNICIPALITY)

top_mun
#[1] "SANTAREM" "JURUTI"   "PRAINHA" 

sum((data_stats_mun |> filter(MUNICIPALITY %in% top_mun))$VOLUME)/sum(rw$VOLUME)
#[1] 0.38252185770132

# N.B. "Just three municipalities---Santarém, Juruti and Prainha---in
# the mid-west of the state together account for nearly 38% of volume logged
# over the period 2009-2019."



# Mun make up in 2009 vs. 2019:
# Logging permit 2009
data_stats_mun |> 
  filter(ID_YEAR == "2009") |> 
  group_by(MUNICIPALITY) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  arrange(desc(VOLUME)) |> 
  adorn_totals() |> as_tibble() 

top_mun <-   
  data_stats_mun|>   
  filter(ID_YEAR == "2009") |> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 2)|>  
  pull(MUNICIPALITY)

top_mun
#[1] "ANAPU"  "PACAJA"

sum((data_stats_mun |> filter(ID_YEAR == "2009", MUNICIPALITY %in% top_mun))$VOLUME)/
  sum((rw|> filter(ID_YEAR == "2009"))$VOLUME)
#[1] 0.3817975082756


# Imaflora's "Oeste and Calha Norte" municipalities (West) check:
top_mun <-   
  data_stats_mun|>   
  filter(ID_YEAR == "2009") |> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 10)|>  
  pull(MUNICIPALITY)

top_mun
# [1] "ANAPU"             "PACAJA"            "JURUTI"            "NOVO REPARTIMENTO" "PRAINHA"           "PLACAS"           
# [7] "SANTAREM"          "URUARA"            "VITORIA DO XINGU"  "ACARA"

# Imaflora's "Oeste and Calha Norte" municipalities (West): 
# Juruti,
# Prainha, 
# Santarém

# N.B."in 2009 the mid-state municipalities of Anapu and Pacaja alone accounted
# for 38% of total production, but by 2019 these municipalities did not even
# make to top 10."



# Logging permit 2019
data_stats_mun |> 
  filter(ID_YEAR == "2019") |> 
  group_by(MUNICIPALITY) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  arrange(desc(VOLUME)) |> 
  adorn_totals() |> as_tibble() 

top_mun <-   
  data_stats_mun|>   
  filter(ID_YEAR == "2019") |> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 2)|>  
  pull(MUNICIPALITY)

top_mun
#[1] "ALTAMIRA" "JURUTI" 

sum((data_stats_mun |> filter(ID_YEAR == "2019", MUNICIPALITY %in% top_mun))$VOLUME)/
  sum((rw|> filter(ID_YEAR == "2019"))$VOLUME)
#[1] 0.48624997470144

# Imaflora's "Oeste and Calha Norte" municipalities (West) check:
top_mun <-   
  data_stats_mun|>   
  filter(ID_YEAR == "2019") |> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 10)|>  
  pull(MUNICIPALITY)

top_mun
# [1] "ALTAMIRA"       "JURUTI"         "PRAINHA"        "SANTAREM"       "MONTE ALEGRE"   "AVEIRO"         "URUARA"        
# [8] "ITAITUBA"       "NOVO PROGRESSO" "OBIDOS" 

# Imaflora, "Oeste and Calha Norte" municipalities (West):
# Aveiro, 
# Itaituba, 
# Juruti, 
# Monte Alegre, 
# Novo Progresso, 
# Prainha, 
# Santarém


# N.B. "...with nearly 49% of timber extracted reportedly coming from western flank
# of Altamira (the largest Brazilian municipality, covering significant portion
# of mid-state Para) and Juruti (bordering the Amazonas state and harboring
# the first high capacity port entering Par ́ a from the west."



## Suppl. Fig. S2 ------------------------------------------------------------------

# Wrangling for Fig. S2
data_stats_mun_inv <- join_vol_lp_stats_mun |> 
  mutate(LP_SOURCE = case_when(is.na(LP_SOURCE) ~ "UNDETERMINED", 
                               TRUE ~ LP_SOURCE)) |>
  mutate(LP_STATUS = case_when(is.na(LP_STATUS) ~ "UNDETERMINED", 
                               TRUE ~ LP_STATUS)) |> 
  mutate(STATUS_GROUP1 = case_when(LP_STATUS %in% c("EXPIRED",
                                                    "EXTENDED", 
                                                    "EXTENDED_CANCELLED SUSPENSION") ~ "VALID", 
                                   LP_STATUS %in% c("SUSPENDED", 
                                                    "CANCELLED", 
                                                    "CANCELLED_OTHERS", 
                                                    "CANCELLED_PERMIT SUBSTITUTION", 
                                                    "CANCELLED_ELABORATION FAILURE", 
                                                    "CANCELLED_NONCOMPLIENCE WITH CONDITIONS", 
                                                    "CANCELLED_ILLEGALITY", 
                                                    "MISSING ACTIVATION DATE") ~ "INVALID", 
                                   LP_STATUS %in% c("EMITIDA OFERTA", 
                                                    "ESTORNADO ITEM") ~ "AUTEX", 
                                   TRUE ~ LP_STATUS)) |> 
  mutate(STATUS_GROUP2 = case_when(LP_STATUS %in% c("EXPIRED") ~ LP_STATUS, 
                                   LP_STATUS %in% c("EMITIDA OFERTA") ~ "AUTEX", #"AUTEX (issued offer)", #Or AUTEX, decide as I have all info
                                   LP_STATUS %in% c("EXTENDED", "EXTENDED_CANCELLED SUSPENSION") ~ "EXTENDED",
                                   LP_STATUS %in% c("SUSPENDED") ~ "SUSPENDED", 
                                   LP_STATUS %in% c("ESTORNADO ITEM") ~ "AUTEX", #"AUTEX (credits given back)",  #Or AUTEX, decide as I have all info
                                   LP_STATUS %in% c("CANCELLED", "CANCELLED_OTHERS") ~ "CANCELLED",
                                   LP_STATUS %in% c("CANCELLED_PERMIT SUBSTITUTION") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_ELABORATION FAILURE") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_NONCOMPLIENCE WITH CONDITIONS") ~ LP_STATUS, 
                                   LP_STATUS %in% c("CANCELLED_ILLEGALITY") ~ LP_STATUS, 
                                   LP_STATUS %in% c("MISSING ACTIVATION DATE") ~ "MISSING ACTIVATION DATE",
                                   TRUE ~ LP_STATUS)) 

# Check on volume
data_stats_mun_inv |> 
  group_by(STATUS_GROUP1) |> 
  summarise(VOLUME = sum(VOLUME))

data_stats_mun_inv |> 
  group_by(STATUS_GROUP2) |> 
  summarise(VOLUME = sum(VOLUME))

# See that municipality data exists from both joined datasets
data_stats_mun_inv |> 
  filter(STATUS_GROUP1 == "INVALID") |> 
  group_by(MUNICIPALITY) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  arrange(desc(VOLUME)) |> 
  adorn_totals() |> as_tibble() 
# # A tibble: 33 × 2
# MUNICIPALITY    VOLUME
# <chr>            <dbl>
# 1 SANTAREM        32594.
# 2 JURUTI          26520.
# 3 PRAINHA         21433.
# 4 URUARA          16191.
# 5 PLACAS          11234.
# 6 AVEIRO           9810.
# 7 ANAPU            7089.
# 8 ANAJAS           5892.
# 9 IPIXUNA DO PARA  5121.
# 10 PACAJA           4474.

data_stats_mun_inv |> 
  filter(STATUS_GROUP1 == "INVALID") |> 
  group_by(name_muni) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  arrange(desc(VOLUME)) |> 
  adorn_totals() |> as_tibble() 

# # A tibble: 34 × 2
# name_muni       VOLUME
# <chr>            <dbl>
# 1 Santarém        32107.
# 2 Juruti          26520.
# 3 Prainha         24303.
# 4 Uruará          15075.
# 5 Placas          11350.
# 6 Aveiro           9810.
# 7 Anapu            7077.
# 8 Anajás           5892.
# 9 Pacajá           4474.
# 10 Ipixuna Do Pará  3367.

data_stats_mun_inv |> 
  filter(STATUS_GROUP1 == "INVALID") |> 
  group_by(name_muni) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  arrange(desc(VOLUME)) |> 
  adorn_totals() |> as_tibble() 

top_mun <-   
  data_stats_mun_inv|> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 3)|>  
  pull(MUNICIPALITY)

sum((data_stats_mun_inv |> filter(MUNICIPALITY %in% top_mun))$VOLUME)/sum(rw$VOLUME)
#[1] 0.38811192266398

# N.B. ..."Santarém, Juruti and Prainha are municipalities where most volume from
# invalid permits originated, together accounting for 39% of invalid permits
# (Suppl. Fig. S1c-d)."

# Clean env. 
rm(top_mun)



# Visualizing patterns of production by mun over time

# Supplementary Fig S2a-b
figS2a <- data_stats_mun |> 
  group_by(MUNICIPALITY, ID_YEAR) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup() |> 
  mutate(MUNICIPALITY = str_to_title(MUNICIPALITY)) |> 
  mutate(MUNICIPALITY = fct_lump(MUNICIPALITY, 17)) |> #view()
  ggplot() +
  geom_col(aes(x = ID_YEAR, y = VOLUME, fill = reorder(MUNICIPALITY, VOLUME, FUN = sum, decreasing = T)), position = "fill") +
  scale_fill_manual(name = "Municipality of \n Logging Permit",
                    values = paletteer::paletteer_c("scico::lapaz", n=19, direction = -1)) +
  ylab("Volume") +
  xlab("") +
  theme_minimal() +
  labs(title = "Propotion of volume from all logging permits by municipality", 
       tag = "a")+
  theme(plot.tag = element_text(face="bold"))


# Top 10 municipality look
top_mun_figS2b <-   
  data_stats_mun|> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 10)|>  #change 10 to 5 to test it
  pull(MUNICIPALITY)

figS2b <- data_stats_mun |> 
  group_by(MUNICIPALITY, ID_YEAR) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup() |> 
  filter(MUNICIPALITY %in% top_mun_figS2b) |> 
  mutate(MUNICIPALITY = str_to_title(MUNICIPALITY)) |> 
  ggplot() +
  geom_area(aes(x = ID_YEAR, y = VOLUME, group = MUNICIPALITY, 
                fill = reorder(MUNICIPALITY, VOLUME, FUN = sum, decreasing = T)))+
  scale_fill_manual(name = "Municipality of \n Logging Permit",
                    values = paletteer_c(`"scico::lapaz"`, n=10, direction = -1)) +
  ylab(expression(paste("Volume (", m^3, ")", sep=""))) +
  xlab("") +
  labs(title = "Total volume share from largest producing municipalities", 
       tag = "b") +
  theme_minimal() +
  theme(plot.tag = element_text(face="bold"))


# Supplementary Fig. S1c-d

# Where is the volume with invalid/missing illegality risk coming from?
n <- 16
figS2c <- data_stats_mun_inv |> 
  filter(STATUS_GROUP1 == "INVALID") |> 
  group_by(MUNICIPALITY, ID_YEAR) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup() |> 
  mutate(MUNICIPALITY = str_to_title(MUNICIPALITY)) |> 
  mutate(MUNICIPALITY = fct_lump(MUNICIPALITY, n)) |> #view()
  ggplot() +
  geom_col(aes(x = ID_YEAR, y = VOLUME, 
               fill = reorder(MUNICIPALITY, VOLUME, FUN = sum, decreasing = T)), 
           position = "fill") +
  scale_fill_manual(name = "Municipality of \n Logging Permit",
                    values = paletteer_c(`"scico::lapaz"`, n=n+1, direction = -1)) +
  ylab("Volume") +
  xlab("") +
  labs(title = "Propotion of volume from invalid logging permits by municipality", 
       tag = "c") + 
  theme_minimal() +
  theme(plot.tag = element_text(face="bold"))


top_mun_figS2d <-   
  data_stats_mun|> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 10)|>  #change 10 to 5 to test it
  pull(MUNICIPALITY)


figS2d_data_inv <- data_stats_mun_inv |> 
  filter(STATUS_GROUP1 == "INVALID") |> 
  group_by(MUNICIPALITY, ID_YEAR) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup() |> 
  filter(MUNICIPALITY %in% top_mun_figS2d)


# Expand the data.frame
figS2d_data <- merge(figS2d_data_inv, 
                     expand.grid(MUNICIPALITY = unique(figS2d_data_inv$MUNICIPALITY),
                                 ID_YEAR = unique(figS2d_data_inv$ID_YEAR),
                                 stringsAsFactors = F),
                     all.y = T)

# Fill NA values with zeros
figS2d_data$VOLUME[is.na(figS2d_data$VOLUME)] <- 0

figS2d <- figS2d_data |> 
  mutate(MUNICIPALITY = str_to_title(MUNICIPALITY)) |> 
  ggplot() +
  geom_area(aes(x = ID_YEAR, y = VOLUME, 
                group = MUNICIPALITY, 
                fill = reorder(MUNICIPALITY, VOLUME, FUN = sum, decreasing = T)), 
            position = "stack")+
  scale_fill_manual(name = "Municipality of \n Logging Permit",
                    values = paletteer_c(`"scico::lapaz"`, n=10, direction = -1)) +
  ylab(expression(paste("Volume (", m^3, ")", sep=""))) +
  xlab("") +
  labs(title = "Total volume share from invalid logging permits by municipality", 
       tag = "d")+
  theme_minimal() +
  theme(plot.tag = element_text(face="bold"))

# Suppl. FigS1
(figS2a+figS2b)/(figS2c+figS2d)

# Saving fig
ggsave(filename = "./results/figS2.jpeg",
       width = 16,
       height = 14,
       dpi=700,
       units = "in")

# Saving fig
ggsave(filename = "./results/figS2.pdf",
       width = 16,
       height = 14)



##  Type of lp  --------------------------------------------------

data_type_lp <- vol_by_lp |> 
  left_join(lp, by = c("LP_REF" = "LP_NUM"))

# Volume by type of permit 
data_type_lp  |> 
  group_by(TYPE) |> 
  summarise(sum(VOLUME)) |> 
  adorn_totals()

# Share of permit type
data_type_lp  |> 
  group_by(TYPE) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  adorn_totals() |> 
  adorn_percentages("col") |> as_tibble()



# 92% AUTEF (state-level), 7% AUTEX-PMFS (national-level), 0.08% AUTEX-Legal Def, 
# 1.1% Undetermined 

#N.B. "Nearly all ipê production comes from Sustainable Forest Management Plans
# (PMFS), most of which (92% of volume) have logging permits authorized at
# state-level and 7% originated from enterprises licensed under national
# jurisdiction..."


# Figures for share of permit type by year
join_vol_lp_stats_mun |> 
  filter(ID_YEAR %in% 2009:2015) |> 
  group_by(TYPE) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  adorn_totals() |> 
  adorn_percentages("col") |> as_tibble() 

join_vol_lp_stats_mun |> 
  filter(ID_YEAR == "2019") |> 
  group_by(TYPE, ID_YEAR) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  adorn_totals() |> 
  adorn_percentages("col") |> as_tibble() 


# N.B. "Between 2009-2015, only 2% of logging permits for ipe were issued under
# it, but with the start of extraction from the National Forest concessions ...
# by 2019 this share was up to 25% of logging"



## Details on missing/inv. permits -----------------------------------------------

data_fig1|> group_by(LP_SOURCE) |> count()

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
# 17 UDETERMINED                                                      13 -- effectively missing


data_fig1 |> 
  filter(LP_STATUS == "UNDETERMINED") |> 
  group_by(LP_SOURCE, LP_STATUS) |> 
  count() |> 
  adorn_totals() |> as_tibble()

# # A tibble: 6 × 3
# LP_SOURCE                               LP_STATUS        n
# <chr>                                   <chr>        <int>
# 1 AUTEF_UPA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT UNDETERMINED     1 - effectively missing, pdf not found to back up
# 2 AUTEF_UPA_NA_NA_NA_NA_NA                UNDETERMINED     4 - effectively missing, pdf not found to back up
# 3 NA_NA_NA_AUTEX_ORIGINAL_LP_NA_NA        UNDETERMINED     1 - autex
# 4 NA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT        UNDETERMINED    11 - effectively missing, IBAMA 2019 type credit
# 5 UNDETERMINED                            UNDETERMINED    13 - effectively missing
# 6 Total                                   -               30

#29 missing permits as AUTEX ORIGINAL LP is an additional info to main AUTEX number

# N.B. Slight changes may apply to details here as the intention is to minimize
# missing/understand any data quality issue associated with it.

# Missing permits/Undetermined permits:
# data_fig1 |> filter(LP_SOURCE %in%
#                           c("AUTEF_UPA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT",
#                              "AUTEF_UPA_NA_NA_NA_NA_NA",
#                              "NA_NA_NA_NA_NA_AUTEF_TYPE_CREDIT",
#                              "NA_NA_NA_AUTEX_ORIGINAL_LP_NA_NA")) |> view()


# Share coming from invalid
data_stats_mun_inv |>
  group_by(STATUS_GROUP1) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  adorn_totals() |> 
  adorn_percentages("col") |> as_tibble()

# # A tibble: 5 × 2
# STATUS_GROUP1 VOLUME
# <chr>          <dbl>
# 1 AUTEX         0.0694
# 2 INVALID       0.162 
# 3 UNDETERMINED  0.0184 (missing share)
# 4 VALID         0.751 
# 5 Total         1 

# Same as the following: 
data_fig1 |> 
  group_by(STATUS_GROUP1) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  adorn_totals() |> 
  adorn_percentages("col") |> as_tibble()


# Total volume coming from invalid
data_stats_mun_inv |>
  group_by(STATUS_GROUP1) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  adorn_totals() |> as_tibble()

# A tibble: 5 × 2
# STATUS_GROUP1  VOLUME
# <chr>           <dbl>
# 1 AUTEX          68422.
# 2 INVALID       159451. ~0.16 Mm3
# 3 UNDETERMINED   18139. (missing share)
# 4 VALID         740206.
# 5 Total         986218.


# N.B."Nearly all (98%) of total ipe volumes entering the first stage of the
# supply-chain could be linked to an existing logging permit, rendering the
# illegality risk from missing permits negligible. When looking at the status of
# matched permits (Fig. 1b), however, 16% (0.16 Mm3) of ipe volume entered the
# crediting system on the basis of outright invalid permits; i.e., permits that
# had been suspended, cancelled, or that were missing an activation date."

# N.B. "...While our results indicate that on average 76% of ipe volumes that
# enters the the supply-chain can be matched to a valid logging permit
# (excluding flows from other states), this does not necessarily imply that the
# timber is free from illegality risks."



### Methods/Supplementary text -------------------------------------------

# Checks on missing information across permits. 
join_vol_lp_data |> filter(is.na(LP_SOURCE)) |> count() #13
join_vol_lp_data |> distinct(LP_SOURCE)
join_vol_lp_data |> filter(is.na(X)) |> count() #36
join_vol_lp_data |> filter(is.na(X)) |> view()
join_vol_lp_data |> filter(is.na(AREA_HA)) |> count() #32
join_vol_lp_data |> filter(is.na(AREA_HA)) |> view()

# N.B. Missing information across permits have been minimized by further
# research in prior process of data cleaning and at this stage are mostly part
# of the missing/undetermined share of permits. Still we keep these in mind
# through the analysis (e.g. some plotting warnings related to the removal of
# these obs.)

# Sum by source
join_vol_lp_data |>  group_by(LP_SOURCE) |> summarise(sum(VOLUME)) |> adorn_totals()

rw_tot <- sum(join_vol_lp_data$VOLUME)

# How much volume UPA polygons cover? 
join_vol_lp_data |> 
  filter(grepl("UPA", LP_SOURCE)) |> 
  summarise(sum(VOLUME)) |>
  pull()/rw_tot*100
# 54% only: motivation to obtain full pdfs. 

# How much volume PDF data cover?
join_vol_lp_data |> 
  filter(grepl("PDF", LP_SOURCE)) |> 
  summarise(sum(VOLUME)) |>
  pull()/rw_tot*100
#[1] 90.754683370231

# Update from 91.2% (after auto+manual download/search). Figure starts
# corroborating IBAMA 2019 that overwhelming majority ipe comes from AUTEF-PMFS.

# How much volume AUTEX data cover?
join_vol_lp_data |> 
  filter(grepl("AUTEX", LP_SOURCE)) |> 
  summarise(sum(VOLUME)) |>
  pull()/rw_tot*100
#[1] 8.0392921135894
# Update from 7.7%

# Between what we can account for with PDFs and data from AUTEX, what share are we missing?
100-(90.75+8.04) # Only 1.21% cannot be accounted for. 
# Update from 1.08%. 

# Clean env.
rm(data_stats_mun, data_stats_mun_inv, top_mun_figS2b, top_mun_figS2d, figS2d_data, 
   join_vol_lp_stats_mun, vol_by_lp_stats_mun, data_type_lp, figS2d_data_inv, 
   figS2a, figS2b, figS2c, figS2d, n)



# Fig. 2 Species yield estimation ----------------------------------------------

# Selecting main data for Fig.2
data_fig2_all <- data_fig1 |> 
  filter(!is.na(STATUS_GROUP3), 
         AREA_HA > 0) |> # Areas from AUTEX permits/supressao vegetal are set to zero in dataset  
  mutate(YIELD = VOLUME/AREA_HA)

# Applying data subsets for figures
data_fig2_valid <- data_fig1 |> 
  filter(STATUS_GROUP3 %in% c("EXPIRED", "EXTENDED")) |> 
  mutate(YIELD = VOLUME/AREA_HA)

data_fig2_invalid <- data_fig1 |> 
  filter(!STATUS_GROUP3 %in% c("EXTENDED","EXPIRED","AUTEX")) |> 
  mutate(YIELD = VOLUME/AREA_HA) 

data_fig2_autex  <- data_fig1 |> 
  filter(STATUS_GROUP3 == "AUTEX", 
         AREA_HA > 0) |> # Areas from AUTEX permits/supressao vegetal are set to zero in dataset  
  mutate(YIELD = VOLUME/AREA_HA)

data_fig2_valid_and_autex <- data_fig1 |> 
  filter(STATUS_GROUP3 %in% c("EXPIRED", "EXTENDED", "AUTEX")) |> 
  mutate(YIELD = VOLUME/AREA_HA)


## Checking on outliers (yield, area) ----------------------------------------

#[Data check not immediately reproducible, mostly for documentation purposes]

# What is the range in area for logging permits?
# data_fig1 |> arrange(AREA_HA) |> view()

# A number of permits have area set to zero, but all are AUTEX from legal
# deforestation.

# data_fig1 |> filter(AREA_HA > 0) |> summary()

# Smallest permit has 18.75 ha and the largest 1.3M ha. 

# Here taking a look at enterprises under national jurisdiction vs. state
# licenses makes a difference. 
# 
# data_fig1 |> arrange(desc(AREA_HA)) |> view()
# #8 of the top 10 largest areas (which range in sharp drop from 1.3 Mha to
# #5165ha) are listed in the AUTEX. 
# 
# autorizacao_pa |> filter(numero_de_serie_da_autex %in% c("10152201801739" ,"10152201904874", "10152201912573",
#                                  "20152201909413" ,"20532201800131" ,"20532201800132" ,
#                                  "20532201803167")) |> 
#   distinct(nome_razao_social_do_detentor, nome_da_origem, area_ha) |> arrange(desc(area_ha))
# # # A tibble: 7 × 3
# # nome_razao_social_do_detentor                    nome_da_origem                                area_ha
# # <chr>                                            <chr>                                           <dbl>
# # 1 ASSOCIAÇÃO COMUNITÁRIA DEUS PROVERÁ              RESEX  VERDE  PARA  SEMPRE                   1289338.
# # 2 ASSOC.DE DESEN AGROEXTRATIVISTA DO BAIXO ACARAI  RESEX VERDE PARA SEMPRE                      1288601.
# # 3 COOPERATIVA MISTA AGROEXTRATIVISTA DO RIO INAMBÚ COOPRUNÃ                                      674326.
# # 4 PATAUÁ FLORESTAL LTDA. - SPE                     UMF IV - FLONA ALTAMIRA                       111373.
# # 5 PATAUÁ FLORESTAL LTDA. - SPE                     UMF III - FLONA ALTAMIRA                       98384.
# # 6 RRX MINERAÇÃO E SERVIÇOS LTDA - EPP              UMF I - FLONA DE ALTAMIRA                      39047.
# # 7 COOPERATIVA MISTA DA FLONA DO TAPAJÓS            FLORESTA NACIONAL DO TAPAJÓS (FLONA TAPAJÓS)   37912.
# 
# # Summary across subsets
# Hmisc::describe(data_fig2_valid$AREA_HA)
# Hmisc::describe(data_fig2_invalid$AREA_HA)
# Hmisc::describe(data_fig2_autex$AREA_HA)
# Hmisc::describe(data_fig2_all$AREA_HA)
# 
# # 1292 Total permits substantiating volume entering the supply chain, we can use
# # 1243 to obtain yield as these have areas. 
# 
# # The 49 remaining are: 
# data_fig1 |> filter(AREA_HA<1) |> view()
# #16 permits are AUTEX legal def and display 0 as area. 
# 
# data_fig1 |> filter(is.na(AREA_HA)) |> view()
# #32 Permits do not display area so cannot be used. Most of these are
# #"Undetermined", permits could not be downloaded although might have been cited
# #in different datasets.
# 
# # In summary, what would have been area outliers have been checked. - Areas set
# # to zero related to AUTEX legal deforestation(16); - Largest areas under AUTEX
# # nat. jurisdiction permits; - Any missing areas related to permits that could
# # not be located.(32); missing permits.
# 
# # What is the range in yield for logging permits?
# Hmisc::describe(data_fig2_all$YIELD)
# Hmisc::describe(data_fig2_valid$YIELD)
# Hmisc::describe(data_fig2_invalid$YIELD)
# Hmisc::describe(data_fig2_autex$YIELD)
# 
# # Looking at outliers for yield
# top_10_yield <- data_fig2_all |> arrange(desc(YIELD)) |> head(n=10) |> select(LP_REF, AREA_HA, YIELD, VOLUME)
# mean(top_10_yield$AREA_HA)
# 
# top_5_yield <- data_fig2_all |> arrange(desc(YIELD)) |> head(n=5) |> select(LP_REF, AREA_HA, YIELD, VOLUME)
# mean(top_5_yield$AREA_HA)
# 
# mean(data_fig2_all$AREA_HA)
# mean(data_fig2_valid$AREA_HA)
# mean(data_fig2_invalid$AREA_HA, na.rm = TRUE)
# mean(data_fig2_autex$AREA_HA)
# # The average of the top 10 yields licensed area is ~97ha, whereas the average
# # of valid, invalid, autex and all are respectively: 654ha, 1157ha, 123422ha.
# 
# # Where do these high yields come from?
# data_fig2_all |> arrange(desc(YIELD)) |> head(n=10) |> select(LP_REF, AREA_HA, YIELD, VOLUME)
# data_fig2_valid |> arrange(desc(YIELD)) |> head(n=10) |> select(LP_REF, AREA_HA, YIELD, VOLUME)

# Most of them found in valid permits. 

# For supplementary material, boxplot (currently S4)



## Fig 2a and Suppl. FigS3a and FigS3b --------------------------------------


### Replicating Brancalion's RADAM estimates --------------------------------

# This section is based on a replication of Brancalion et al 2018
# (https://www.science.org/doi/10.1126/sciadv.aat1192) yield estimates. Code is
# followed original as far as possible, which can be accessed via Zenodo here: 
# (https://zenodo.org/record/1244107#.WvLg9oiUvIU)


# Load files needed as per availability in Zenodo Data Repository 

# Check available files: 
list.files("./data/raw/radambrasil-brancalion/REPOSITORY_Brancalion et al 2018_SA/")

# [1] "AUTEFS_tabeladas_TUDO_final.csv"       "DADOS_RADAM_ALL.dbf"                  
# [3] "dados_RADAM_parcelas_Para.csv"         "LAST_ 1 SCRIPT ULTIMAS ANALISES.R"    
# [5] "nomes_engenheiros_FINAL.csv"           "O SCRIPT_TODAS AS ANALISES.R"         
# [7] "valores_especies_FINAL.xlsx"           "valores_especies_NOVO_agosto2017.xlsx"

# Basic information on RADAM plots within Para state
Para_plots <- readr::read_csv("./data/raw/radambrasil-brancalion/REPOSITORY_Brancalion et al 2018_SA/dados_RADAM_parcelas_Para.csv", 
                              locale = locale())
# All RADAM data available through the study
vol_radam <-  foreign::read.dbf("./data/raw/radambrasil-brancalion/REPOSITORY_Brancalion et al 2018_SA/DADOS_RADAM_ALL.dbf")

# Loading only to check species used in Brancalion analysis, but no analysis on monetary value is performed here. 
valor.sps <- readxl::read_excel("./data/raw/radambrasil-brancalion/REPOSITORY_Brancalion et al 2018_SA/valores_especies_NOVO_agosto2017.xlsx")


# visual check on species used by Brancalion. Here we use only Tabebuia and
# Handroanthus, but look to expand the analysis to all species.
valor.sps$nome_pop
valor.sps$nome_ci1
valor.sps$nome_ci2
valor.sps$nome_ci3
valor.sps$nome_ci4

# replacing Brancalion's permit data 
licensas <- data_fig2_all |> # Numbers are based on all permits 
  mutate(nomes_ci = "Handroanthus serratifolius") |>
  mutate(nomes_pop = "ipe") |>
  mutate(AREA_HA = as.numeric(AREA_HA)) |>
  filter(!is.na(AREA_HA), AREA_HA > 1)


# Objects for RADAM Plots
Para_vol_radam <- vol_radam[vol_radam$ID_COMP %in% Para_plots$id_parcela,]
n.radam <-  length(unique(Para_vol_radam$ID_COMP))

# N.B. Our replication is based on 394 plots, which is the total we found
# although the article mentions 426. This count was also displayed in the code
# available in the repository.

# Para_plots |> distinct(id_parcela) |> count() #402
# Para_vol_radam |> distinct(ID_COMP) |> count() #394

# How many Radam plots and logging permits?
unique.Radam = Para_vol_radam[!duplicated(Para_vol_radam$ID_COMP),]
nrow(unique.Radam)

unique.licensas = licensas[!duplicated(licensas$LP_REF),]
nrow(unique.licensas)

# Check on the permit areas
summary(unique.licensas$AREA_HA)

# Wrangling for start of analysis 
# criando a coluna genero e especie junto (binomial)              
Para_vol_radam$spp = paste(Para_vol_radam$GENERO, Para_vol_radam$ESPECIE, sep = " ")
Para_vol_radam$spp = factor(Para_vol_radam$spp)  

# Checking species
Para_vol_radam |> filter(grepl("Handroanthus|Tabebuia", spp)) |> distinct(spp)
# N.B. Only two species of ipê can be found in RADAM, Tabebuia impetiginosa and
# Tabebuia serratifolia. See methods for more info on species/genus consideration.

# retirando colunas inuteis dos dados de volume do Radam
names(Para_vol_radam)
# Para_vol_radam = Para_vol_radam[,c("ID_COMP", "FAMILIA", "GENERO", "ESPECIE", "NM_REGIONA", "ALT_FUSTE", "spp", "DIAM_CAP", "VOL_MADEIR", "coords.x1",  "coords.x2")]
Para_vol_radam = Para_vol_radam[,c("ID_COMP", "FAMILIA", "GENERO", "ESPECIE", "NM_REGIONA", "ALT_FUSTE", "spp", "DIAM_CAP", "VOL_MADEIR")]

# NB. The original code mentions volume is calculated:
#obs: o volume eh calculado com a ALT_FUSTE e fator de forma de 0,7: 
#t = Para_vol_radam[1,]; t
#v = round(t$ALT_FUSTE*(t$DIAM_CAP/pi)^2*pi/40000*0.7, 2); v
#rm(t, v)

#selecionando apenas individuos com DAP>50
cap.min = 50*pi
Para_vol_radam = Para_vol_radam[Para_vol_radam$DIAM_CAP > cap.min, ]
#OBS: ao retirar os DAPS menores que 50 cm, duas parcelas foram eliminadas.
#Contudo, elas podem ser consideradas as estimativas de media de volume posterioemente.
#Portanto, eh importante, se quiser, o uso do objeto "n.radam"

# Checking removal
Para_vol_radam |> distinct(ID_COMP) |> count() # 392


# Start of means estimation               

# Setting up vectors and commenting out those not used 
#VALOR = c()
#NOME.POP = c()
NOME.CI_1 = c()
#P95 = c()
#LIMIAR_Q90 = c()
MIN.RADAM_raw = c()
MAX.RADAM_raw = c()
MEAN.RADAM = c()
MEDIAN.RADAM = c()
Q90.RADAM = c()
Q95.RADAM = c()
Q99.RADAM = c()
N.RADAM = c()
MEAN.LICENSAS = c()
MEDIAN.LICENSAS = c()
Q90.LICENSAS = c()
Q95.LICENSAS = c()
Q99.LICENSAS = c()
N.LICENSAS = c()
MEAN.RATIO = c()
SD.RATIO = c()

NOMES.CI = c()
VOL.TOTAL.LICENSAS = c()
#TOTAL.ARVORES.LICENSAS = c()
TOTAL.AREA.LICENSAS = c()
TOTAL.ARVORES.RADAM = c()
TOTAL.VOL.RADAM = c()


#LISTA COM DAS FDP (t distribution) de cada especie com os dados do Radam
LISTA_FDP.RADAM = list()

#LISTA COM DAS FDP (t distribution) de cada especie com os dados das autefs
#LISTA_LICENSAS = list()

# os parametros para o RADAM. Serao retirados a partir de medias de amostras com n.plots
n.rando = 10000
# Change here to produce simulations with minimum areas higher than 19ha
n.plots = ceiling(min(licensas$AREA_HA)) # n.plots vai ser a area da menor autef 
# n.plots = 66
# n.plots = 370
n.plots


# only species that will be used:
only.sps.used = which(valor.sps$nome_ci1 %in% c("Handroanthus serratifolius"))

valor.sps[only.sps.used,]

#gerar nomes das especies para colocar no grafico de histograma do radam
sps.names.plot = paste(
  unlist(
    lapply(
      strsplit(as.character(valor.sps$nome_ci1), split = " "), 
      function(x) (x[1]))), 
  "sp")



for(i in only.sps.used){
  
  #i = 1
  #nomes.ci
  nomes.ci = as.character(valor.sps[i, c("nome_ci1", "nome_ci2", "nome_ci3", "nome_ci4")])
  nomes.ci = as.character(na.omit(as.character(nomes.ci)))
  nomes.ci
  
  #### Radam data -----------------------------------------------------------
  t.radam = data.frame(matrix(NA, ncol = 2, nrow = 0))
  t.radam
  names(t.radam) = c("plot", "vol")
  
  vol = c()
  dens.arv = c()
  plot = as.character(c())
  parcelas = unique(Para_vol_radam$ID_COMP)
  length(unique(Para_vol_radam$ID_COMP))
  
  
  for(j in 1:length(unique(Para_vol_radam$ID_COMP))){
    
    #j = 37 # this would be grabbing info parcel by parcel
    t = Para_vol_radam[Para_vol_radam$ID_COMP == parcelas[j], ] #selec. parcela
    t = t[grep(paste(nomes.ci, collapse = "|"), t$spp),] #selec. especie
    v = sum(t$VOL_MADEIR, na.rm = T) #soma do volume
    
    #abastecendo os vetores
    plot = c(plot, as.character(parcelas[j]))
    vol = c(vol, v)
    dens.arv = c(dens.arv, length(t$VOL_MADEIR)) #number of trees
    
    rm(t)
  } #end looping das parcelas
  
  t.radam = data.frame(plot, vol, dens.arv)
  rm(plot, vol, dens.arv)
  
  #### os parametros do radam serao retirados a partir de medias de amostras com n.plots
  #n.rando = 10000
  #n.plots = ceiling(min(licensas$area_l)) # #o n.plots vai ser a area da menor autef
  means = c()
  
  for(j in 1:n.rando){means = c(means, mean(sample(t.radam$vol, n.plots)))}
  
  mean.radam = mean(means, na.rm = T)
  sd.radam = sd(means, na.rm = T) # sd:0.55, double check I can use sd with mean of means
  median.radam = median(means, na.rm = T)
  q90.radam = quantile(means, 0.9)
  q95.radam = quantile(means, 0.95)
  q99.radam = quantile(means, 0.99)
  
  # FDP - distribuicao t das amostras
  if(length(which(t.radam$vol > 0)) > 1){ #se tiver pelo menos mais de uma observacao no RADAM
    #density
    density = dt(seq(min(t.radam$vol), max(t.radam$vol), by = 0.01),
                 n.radam, mean(t.radam$vol, na.rm = T))
    
    #probability
    probability = pt(seq(min(t.radam$vol), max(t.radam$vol), by = 0.01),
                     n.radam, mean(t.radam$vol, na.rm = T))
    
    #tabela com os valores, a densidade e a probabilidade acumulada
    table = data.frame(vol = seq(min(t.radam$vol), max(t.radam$vol), by = 0.01), density, probability)
    table = table[order(table$probability, decreasing = F),]
    table$probability.inv = 1-table$probability
    
    #abastecendo a LISTA
    LISTA_FDP.RADAM[[valor.sps$nome_pop[i]]] = table
    rm(table)
    
  }else{
    
    LISTA_FDP.RADAM[[valor.sps$nome_pop[i]]] = NA
  }# End ifelse FDP RADAM
  # End RADAM data
  
  #### Autef/Autex data -----------------------------------------------------------
  
  #selecionando a especie
  t.licensas = licensas[grep(paste(nomes.ci, collapse = "|"), licensas$nomes_ci, perl=TRUE, value=FALSE),]
  
  #Grafico da distribuicao dos dados do radam e os limiares do radam
  jpeg(paste0("./results/HIST_autefs_", sps.names.plot[i], ".jpeg"), unit = "in", width = 6, height = 6, res = 300)
  
  #parametros do grafico das autefs 
  range.autef = max(t.licensas$YIELD) - min(t.licensas$YIELD)
  t.hist.autef = hist(t.licensas$YIELD, plot = F, breaks = range.autef/0.1)
  t.hist.autef$counts = t.hist.autef$counts/sum(t.hist.autef$counts)*100 #transformando em densidade
  
  col.autef = rep("white", length(t.hist.autef$breaks))
  col.autef[t.hist.autef$breaks < q90.radam] = "blue"
  col.autef[t.hist.autef$breaks > q90.radam & t.hist.autef$breaks < q95.radam] = "yellow"
  col.autef[t.hist.autef$breaks > q95.radam & t.hist.autef$breaks < q99.radam] = "orange"
  col.autef[t.hist.autef$breaks > q99.radam] = "red"
  
  plot(t.hist.autef, xlab = "Volume (ha)", ylab = "Density (%)", main = sps.names.plot[i], 
       col = col.autef, las = 1, cex.lab = 1.2)
  
  dev.off()
  # End histogram. 
  
  mean.licensas = mean(t.licensas$YIELD, na.rm = T)
  # Double check sd can be reported in this way
  sd.licensas = sd(t.licensas$YIELD, na.rm = T) # sd 2.2, check that I can use sd to compare with radam
  median.licensas = median(t.licensas$YIELD, na.rm = T)
  q90.licensas = quantile(t.licensas$YIELD, 0.9)
  q95.licensas = quantile(t.licensas$YIELD, 0.95)
  q99.licensas = quantile(t.licensas$YIELD, 0.99)
  
  
  # N.B. Not used in our analysis. 
  # #calculando Q95
  # seq = seq(min(t.radam$vol), max(t.radam$vol), by = 0.1)
  # limiar = seq[pt(seq, n.radam, mean(t.radam$vol)) > 0.95][1]
  # #limiar = quantile(t.radam$vol, 0.95)
  # 
  # P95 = c(P95, length(which(t.licensas$YIELD > limiar))/nrow(t.licensas))
  # rm(seq)
  #LISTA_LICENSAS[[valor.sps$nome_pop[i]]] = t.licensas
  
  
  #### Plotting figures --------------------------------------------------------
  
  # Brancalion et al. 2018 original graphic for comparison 
  #Grafico da distribuicao dos dados do radam e os limiares dos quantis do radam
  jpeg(paste0("./results/HIST_radam_", sps.names.plot[i], ".jpeg"), unit = "in", width = 6, height = 6, res = 300)
  
  #parametros do grafico do radam 
  range.radam = max(means) - min(means)
  t.hist.radam = hist(means, plot = F, breaks = range.radam/0.1)
  t.hist.radam$counts = t.hist.radam$counts/sum(t.hist.radam$counts)*100 #transformando em densidade
  
  col.radam = rep("white", length(t.hist.radam$breaks))
  col.radam[t.hist.radam$breaks < q90.radam] = "blue"
  col.radam[t.hist.radam$breaks > q90.radam & t.hist.radam$breaks < q95.radam] = "yellow"
  col.radam[t.hist.radam$breaks > q95.radam & t.hist.radam$breaks < q99.radam] = "orange"
  col.radam[t.hist.radam$breaks > q99.radam] = "red"
  
  #x.max = ceiling(max(c(max(means), max(t.licensas$YIELD))))
  y.max = ceiling(max(c(max(t.hist.radam$counts), max(t.hist.autef$counts))))
  
  #radam graph
  plot(t.hist.radam, xlab = "Yield (m³/ha)", ylab = "Density (%)", main = "Radam", 
       col = col.radam, las = 1, cex.lab = 1.2, xlim = c(0, 10), ylim = c(0, y.max))
  
  
  dev.off()
  
  
  
  #Grafico da distribuicao dos dados do radam e os limiares dos quantis do radam
  jpeg(paste0("./results/HIST_lp_", sps.names.plot[i], ".jpeg"), unit = "in", width = 6, height = 6, res = 300)
  
  #parametros do grafico das autefs 
  range.autef = max(t.licensas$YIELD) - min(t.licensas$YIELD)
  t.hist.autef = hist(t.licensas$YIELD, plot = F, breaks = range.autef/0.1)
  t.hist.autef$counts = t.hist.autef$counts/sum(t.hist.autef$counts)*100 #transformando em densidade
  
  col.autef = rep("white", length(t.hist.autef$breaks))
  col.autef[t.hist.autef$breaks < q90.radam] = "blue"
  col.autef[t.hist.autef$breaks > q90.radam & t.hist.autef$breaks < q95.radam] = "yellow"
  col.autef[t.hist.autef$breaks > q95.radam & t.hist.autef$breaks < q99.radam] = "orange"
  col.autef[t.hist.autef$breaks > q99.radam] = "red"
  
  #x.max = ceiling(max(c(max(means), max(t.licensas$YIELD))))
  y.max = ceiling(max(c(max(t.hist.radam$counts), max(t.hist.autef$counts))))
  
  #autef graph
  plot(t.hist.autef, xlab = "Yield (m³/ha)", ylab = "Density (%)", main = "Logging permits", 
       col = col.autef, las = 1, cex.lab = 1.2, xlim = c(0, 10), ylim = c(0, y.max))
  
  
  dev.off()
  
  
  # New plots:
  
  # RADAM Plot baseline for supplementary
  dens_radam <- t.hist.radam$counts |> 
    as_tibble(value = "counts") |> 
    rowid_to_column() |> 
    rename(counts = "value")
  
  mids_radam <- t.hist.radam$mids |> 
    as_tibble(name = "mids") |> 
    rename(mids = "value")
  
  radam_hist <- bind_cols(dens_radam, mids_radam) |>
    mutate(group = case_when((mids+0.05) < q90.radam ~ "group1", 
                             (mids+0.05) > q90.radam & (mids+0.05) < q95.radam ~ "group2", 
                             (mids+0.05) > q95.radam & (mids+0.05) < q99.radam ~ "group3",
                             (mids+0.05) > q99.radam ~ "group4",
                             TRUE ~ as.character(NA))) 
  
  
  figS3b <- ggplot(radam_hist, aes(x = mids, y = counts, fill = group)) +
    geom_bar(stat="identity") +
    coord_cartesian(xlim = c(0, 8), expand = FALSE) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8))+
    ylim(0,10) + 
    labs(y = "Probability density", 
         x = expression(paste("Yield (", m^3, ha^-1, ")", sep="")), 
         title = "RADAM Plots", 
         tag = "b") +
    scale_fill_manual(values = c("#2D609B", 
                                 "#CBB94AFF", 
                                 "#A66B20FF", 
                                 "#843837"))+
    theme_minimal()+
    theme(legend.position = "none", 
          plot.tag = element_text(face="bold"))
  
  
  # Logging permit for main fig and supplementary
  dens_lp <- t.hist.autef$counts|> 
    as_tibble(value = "counts") |> 
    rowid_to_column() |> 
    rename(counts = "value")
  
  mids_lp <- t.hist.autef$mids |> 
    as_tibble(name = "mids") |> 
    rename(mids = "value")
  
  lp_hist <- bind_cols(dens_lp, mids_lp)  |>
    mutate(group = case_when((mids+0.05) < q90.radam ~ "group1", 
                             (mids+0.05) > q90.radam & (mids+0.05) < q95.radam ~ "group2", 
                             (mids+0.05) > q95.radam & (mids+0.05) < q99.radam ~ "group3",
                             (mids+0.05) > q99.radam ~ "group4",
                             TRUE ~ as.character(NA)))
  
  # Main figure
  fig2a <- ggplot(lp_hist, aes(x = mids, y = counts, fill = group)) +
    geom_bar(stat="identity") +
    geom_text(aes(x = (0+q90.radam)/2 , y = 6.5, label = "<90%"), angle = 90, color = "#2D609B", size=6) +
    geom_text(aes(x = (q90.radam+q95.radam)/2 , y = 4.5, label = "90-95%"), angle = 90, color = "#CBB94AFF", size=6) +
    geom_text(aes(x = (q95.radam+q99.radam)/2, y = 4, label = "95-99%"), angle = 90, color = "#A66B20FF", size=6) +
    geom_text(aes(x = q99.radam+1 , y = 3.5, label = ">99%"), angle = 90, color = "#843837", size=6) +
    coord_cartesian(xlim = c(0, 8), expand = FALSE) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8))+
    ylim(0,10) + 
    labs(y = "Probability density", 
         x = "", 
         #title = "Logging permits", 
         tag = "a") +
    scale_fill_manual(values = c("#2D609B", 
                                 "#CBB94AFF", 
                                 "#A66B20FF", 
                                 "#843837"))+
    theme_minimal()+
    theme(legend.position = "none", 
          plot.tag = element_text(face="bold"))
  
  #Supplementary figure 
  figS3a <- fig2a + 
    labs(title = "Logging permits", 
         x = expression(paste("Yield (", m^3, ha^-1, ")", sep="")), 
         tag = "a")
  
  
  # Consulting palettes
  # paletteer::paletteer_c(`"scico::lapaz"`, n=11, direction = 1)
  # paletteer::paletteer_c(`"scico::bilbao"`, n=11, direction = 1)
  # paletteer::paletteer_c(`"scico::roma"`, n=15, direction = 1)
  
  # End new plots code (fig2a and figS3a)
  
  
  #ABASTECENDO OS OBJETOS:
  #VALOR = c(VALOR, valor.sps$valor[i])
  #NOME.POP = c(NOME.POP, valor.sps$nome_pop[i])
  NOME.CI_1 = c(NOME.CI_1, valor.sps$nome_ci1[i])
  
  #LIMIAR_Q90 = c(LIMIAR_Q90, limiar)
  MIN.RADAM_raw = c(MIN.RADAM_raw, min(t.radam$vol, na.rm = T))
  MAX.RADAM_raw = c(MAX.RADAM_raw, max(t.radam$vol, na.rm = T))
  MEAN.RADAM = c(MEAN.RADAM, mean.radam)
  MEDIAN.RADAM = c(MEDIAN.RADAM, median.radam)
  Q90.RADAM = c(Q90.RADAM, q90.radam)
  Q95.RADAM = c(Q95.RADAM, q95.radam)
  Q99.RADAM = c(Q99.RADAM, q99.radam)
  N.RADAM = c(N.RADAM, length(which(t.radam$vol > 0)))
  
  MEAN.LICENSAS = c(MEAN.LICENSAS, mean.licensas)
  MEDIAN.LICENSAS = c(MEDIAN.LICENSAS, median.licensas)
  Q90.LICENSAS = c(Q90.LICENSAS, q90.licensas)
  Q95.LICENSAS = c(Q95.LICENSAS, q95.licensas)
  Q99.LICENSAS = c(Q99.LICENSAS, q99.licensas)
  N.LICENSAS = c(N.LICENSAS, nrow(t.licensas))
  
  MEAN.RATIO = c(MEAN.RATIO, mean(t.licensas$YIELD/mean(t.radam$vol)))
  SD.RATIO = c(SD.RATIO, sd(t.licensas$YIELD/mean(t.radam$vol)))
  
  
  
  NOMES.CI = c(NOMES.CI, paste(nomes.ci, collapse = "; "))
  #VOL.TOTAL.LICENSAS = c(VOL.TOTAL.LICENSAS, sum(t.licensas$VOLUM * t.licensas$AREA_HA))
  #TOTAL.ARVORES.LICENSAS = c(TOTAL.ARVORES.LICENSAS, sum(t.licensas$ind, na.rm = T))
  TOTAL.AREA.LICENSAS = c(TOTAL.AREA.LICENSAS, sum(t.licensas$AREA_HA, na.rm = T))
  TOTAL.ARVORES.RADAM = c(TOTAL.ARVORES.RADAM, sum(t.radam$dens.arv))
  TOTAL.VOL.RADAM = c(TOTAL.VOL.RADAM, sum(t.radam$vol))
  
  # rm(t.radam, t.licensas,
  #    mean.radam, median.radam, q90.radam, q95.radam, q99.radam,
  #    mean.licensas, median.licensas, q90.licensas, q95.licensas, q99.licensas,
  #    limiar, Para_plots, Para_vol_radam, unique.licensas, unique.Radam,
  #    )
  # 
  print(round(i/nrow(valor.sps)*100, 2))
  
} # End 

# End of species loop. N.B. We used only one species, but the loop is set up to
# use with others as well.


#### Saving estimates -------------------------------------------------------

# Storing generated figures
dados = data.frame(#NOME.POP, 
  NOME.CI_1, 
  #VALOR, 
  #P95, 
  MIN.RADAM_raw, 
  MAX.RADAM_raw, 
  #LIMIAR_Q90,
  MEAN.RADAM, 
  MEDIAN.RADAM, 
  Q90.RADAM, 
  Q95.RADAM, 
  Q99.RADAM,  
  N.RADAM, 
  TOTAL.ARVORES.RADAM, 
  TOTAL.VOL.RADAM,
  MEAN.LICENSAS,
  MEDIAN.LICENSAS, 
  Q90.LICENSAS, 
  Q95.LICENSAS, 
  Q99.LICENSAS, 
  N.LICENSAS,
  TOTAL.AREA.LICENSAS,
  MEAN.RATIO, 
  SD.RATIO)



# Simulation for Supplementary Fig S4 still manual at this stage. Go through the
# replication of Brancalion section and change the "n.plots" variable to 66 and
# 370 for full reproduction. In that scenario also recall certain figures may be
# overridden. We provide the data objects so you can skip the simulations steps.

# radam_hist_19ha <- radam_hist
# means_19ha <- means
# means_19ha |> quantile()
# dados_19ha <- dados

# radam_hist_66ha <- radam_hist
# dados_66ha <- dados
# means_66ha <- means
# means_66ha |> quantile()

# radam_hist_370ha <- radam_hist
# dados_370ha <- dados
# means_370ha <- means
# means_370ha |> quantile()

rm(NOME.CI_1, MIN.RADAM_raw, MAX.RADAM_raw, MEAN.RADAM, MEDIAN.RADAM,
   Q90.RADAM, Q95.RADAM, Q99.RADAM, N.RADAM, MEAN.LICENSAS, MEDIAN.LICENSAS,
   Q90.LICENSAS, Q95.LICENSAS, Q99.LICENSAS, N.LICENSAS, MEAN.RATIO, SD.RATIO,
   TOTAL.AREA.LICENSAS, TOTAL.ARVORES.RADAM, TOTAL.VOL.RADAM,
   VOL.TOTAL.LICENSAS)

# #filtrando qndo N no radam ou nas licensas menor que o "minimo"
# dados.full = dados
# head(dados.full)
# tail(dados.full)
# 
# #write.csv2(dados.full, "TABELA_dadosfull.csv", row.names = F)
# minimo = 50
# dados = dados.full[which(dados.full$N.LICENSAS >= minimo & 
#                            dados.full$N.RADAM >= minimo), ]

# Storing RADAM prob distribution to be used in fig 2C
radam_fdp <- LISTA_FDP.RADAM |> as.data.frame() |> 
  rename(vol = "Ipê.vol", dens = "Ipê.density", prob = "Ipê.probability" , 
         prob_inv = "Ipê.probability.inv")



## Fig 2b and Suppl. FigS3c and FigS3d  --------------------------------------

# Cumulative sum volume for valid permits
ncsv_valid <- select(data_fig2_valid, VOLUME, YIELD) |> 
  arrange(YIELD) |> 
  mutate(csv = 1-cumsum(VOLUME)/sum(VOLUME))

# Cumulative sum volume for invalid permits
ncsv_invalid <- select(data_fig2_invalid, VOLUME, YIELD) |> 
  arrange(YIELD) |> 
  mutate(csv = 1-cumsum(VOLUME)/sum(VOLUME))

# Cumulative sum volume for AUTEX permits
ncsv_autex <- select(data_fig2_autex , VOLUME, YIELD) |> 
  arrange(YIELD) |> 
  mutate(csv = 1-cumsum(VOLUME)/sum(VOLUME))



# Fig2b  
colors <- c( "Valid: Expired, Extended" = "#2D609B", 
             "Invalid: Suspended, Cancelled, \n Missing activation date" = "#681C1C", 
             "National Jurisdiction \n (AUTEX)" = "gray50")

fig2b <- ggplot()+
  annotate("rect",xmin=dados$Q99.RADAM, xmax=8, ymin=0, ymax=1, 
           alpha=0.4, fill="#843837")+
  annotate("rect",xmin=dados$Q95.RADAM, xmax=dados$Q99.RADAM,ymin=0,ymax=1, 
           alpha=0.4, fill="#A66B20FF")+
  annotate("rect",xmin=dados$Q90.RADAM, xmax=dados$Q95.RADAM,ymin=0,ymax=1, 
           alpha=0.4, fill="#CBB94AFF")+
  annotate("rect",xmin=0,xmax=dados$Q90.RADAM,ymin=0,ymax=1, 
           alpha=0.4, fill="#2D609B")+
  geom_step(data = ncsv_valid, aes(x = YIELD, y = csv, 
                                   color = "Valid: Expired, Extended"), size = 0.6) +
  geom_step(data = ncsv_invalid, aes(x = YIELD, y = csv, 
                                     color = "Invalid: Suspended, Cancelled, \n Missing activation date"), size = 0.6) +
  geom_step(data = ncsv_autex, aes(x = YIELD, y = csv, 
                                   color = "National Jurisdiction \n (AUTEX)"), size = 0.6) +
  labs(tag = "b", 
       x = expression(paste("Yield (", m^3, ha^-1, ")", sep="")), 
       y = "Cumulative share of volume (%)") +
  scale_color_manual(name = "Status of \n Logging Permits", 
                     values = colors) +
  coord_cartesian(xlim = c(0, 8), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8))+
  scale_y_continuous(labels = c("0", "25", "50", "75", "100")) +
  guides(col = guide_legend(reverse = TRUE))+
  theme_minimal() +
  theme(plot.title.position = "plot", 
        plot.tag = element_text(face="bold"),
        legend.position = c(0.8, 0.75), 
        legend.background = element_rect(fill="white",
                                         linewidth=0.5, 
                                         linetype="solid",
                                         colour ="white"))


fig2a/fig2b


#Saving figs
ggsave(filename = "./results/fig2ab.jpeg",
       width = 8,
       height = 10,
       dpi=700,
       units = "in")

ggsave(filename = "./results/fig2ab.pdf",
       width = 8,
       height = 10,
       dpi=700,
       units = "in")



# Supplementary figS3c is nearly the same as fig2b
figS3c <- ggplot()+
  annotate("rect",xmin=dados$Q99.RADAM, xmax=8, ymin=0, ymax=1, alpha=0.4, fill="#843837")+
  annotate("rect",xmin=dados$Q95.RADAM, xmax=dados$Q99.RADAM,ymin=0,ymax=1, alpha=0.4, fill="#A66B20FF")+
  annotate("rect",xmin=dados$Q90.RADAM, xmax=dados$Q95.RADAM,ymin=0,ymax=1, alpha=0.4, fill="#CBB94AFF")+
  annotate("rect",xmin=0,xmax=dados$Q90.RADAM,ymin=0,ymax=1, alpha=0.4, fill="#2D609B")+
  geom_step(data = ncsv_valid, aes(x = YIELD, y = csv, color = "Valid: Expired, Extended"), size = 0.6) +
  geom_step(data = ncsv_invalid, aes(x = YIELD, y = csv, color = "Invalid: Suspended, Cancelled, \n Missing activation date"), size = 0.6) +
  geom_step(data = ncsv_autex, aes(x = YIELD, y = csv, color = "National Jurisdiction \n (AUTEX)"), size = 0.6) +
  labs(tag = "c", 
       x = expression(paste("Yield (", m^3, ha^-1, ")", sep="")), 
       y = "Cumulative share of volume (%)",
       title = "Cumulative Sum, Volume",
       color = "Status of \n Logging Permits") +
  scale_color_manual(values = colors) +
  coord_cartesian(xlim = c(0, 8), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8))+
  scale_y_continuous(labels = c("0", "25", "50", "75", "100")) +
  #xlim(0, 8) +
  guides(col = guide_legend(reverse = TRUE))+
  theme_minimal() +
  theme(plot.tag = element_text(face="bold"),
        legend.position = c(0.8, 0.75), 
        legend.background = element_rect(fill="white",
                                         size=0.5, 
                                         linetype="solid",
                                         colour ="white"))




# Cumulative sum area for valid permits
ncsa_valid <- select(data_fig2_valid, AREA_HA, YIELD) |> 
  arrange(YIELD) |> 
  mutate(csv = 1-cumsum(AREA_HA)/sum(AREA_HA))

# Cumulative sum AREA_HA for invalid permits
ncsa_invalid <- select(data_fig2_invalid, AREA_HA, YIELD) |> 
  filter(!is.na(AREA_HA)) |> 
  arrange(YIELD) |> 
  mutate(csv = 1-cumsum(AREA_HA)/sum(AREA_HA))

# Cumulative sum AREA_HA for AUTEX permits
ncsa_autex <- select(data_fig2_autex , AREA_HA, YIELD) |> 
  arrange(YIELD) |> 
  mutate(csv = 1-cumsum(AREA_HA)/sum(AREA_HA))



figS3d <- ggplot()+
  annotate("rect",xmin=dados$Q99.RADAM, xmax=8, ymin=0, ymax=1, alpha=0.4, fill="#843837")+
  annotate("rect",xmin=dados$Q95.RADAM, xmax=dados$Q99.RADAM,ymin=0,ymax=1, alpha=0.4, fill="#A66B20FF")+
  annotate("rect",xmin=dados$Q90.RADAM, xmax=dados$Q95.RADAM,ymin=0,ymax=1, alpha=0.4, fill="#CBB94AFF")+
  annotate("rect",xmin=0,xmax=dados$Q90.RADAM,ymin=0,ymax=1, alpha=0.4, fill="#2D609B")+
  geom_step(data = ncsa_valid, aes(x = YIELD, y = csv, color = "Valid: Expired, Extended"), size = 0.6) +
  geom_step(data = ncsa_invalid, aes(x = YIELD, y = csv, color = "Invalid: Suspended, Cancelled, \n Missing activation date"), size = 0.6) +
  geom_step(data = ncsa_autex, aes(x = YIELD, y = csv, color = "National Jurisdiction \n (AUTEX)"), size = 0.6) +
  labs(tag = "d", 
       x = expression(paste("Yield (", m^3, ha^-1, ")", sep="")), 
       y = "Cumulative share of area (%)",
       title = "Cumulative Sum, Area",
       color = "Status of \n Logging Permits") +
  scale_color_manual(values = colors) +
  coord_cartesian(xlim = c(0, 8), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8))+
  scale_y_continuous(labels = c("0", "25", "50", "75", "100")) +
  guides(col = guide_legend(reverse = TRUE))+
  # xlim(0, 8) +
  theme_minimal() +
  theme(plot.tag = element_text(face="bold"),
        legend.position = c(0.8, 0.75), 
        legend.background = element_rect(fill="white",
                                         #alpha = 0.8,
                                         size=0.5, 
                                         linetype="solid",
                                         colour ="white"))


(figS3a+figS3b)/(figS3c+figS3d)

#Saving figs
ggsave(filename = "./results/figS3.jpeg",
       width = 14,
       height = 10,
       dpi=700,
       units = "in")

ggsave(filename = "./results/figS3.pdf",
       width = 14,
       height = 10,
       dpi=700,
       units = "in")


rm(fig2a, fig2b, figS3a, figS3b, figS3c, figS3d, colors, mids_lp, mids_radam, 
   ncsv_autex, ncsv_valid, ncsv_invalid,ncsa_autex, ncsa_valid, ncsa_invalid)



# Supporting Stats (Part 2) -----------------------------------------------

## Overstated ipe yields ---------------------------------------------------

# Previous analysis include all permits valid, invalid and autex. Some of the
# figures below are based only on valid permits and Autex (federal-issued
# logging permits)

# Average yield for logging permits
dados$MEAN.LICENSAS
# average only for valid permits
# data_fig2_valid |> 
#   summarise(mean(YIELD))
#mean.licensas
sd.licensas 
# SD stats only for valid permits
data_fig2_valid |> 
  summarise(sd(YIELD))
# N.B...."we found an average ipe yield of 1.8 m3.ha−1 (sd: 2.2 m3.ha−1)" This
# sentence refers only to valid logging permits, if we were to include all
# possible observations (from licensas since the analysis above is done for all
# yields we are able to calculate, including invalid ones) the ipe yield is
# still 1.8, but the SD is about 2.9.


# Average yield for radam
dados$MEAN.RADAM
#mean.radam
# N.B. ...."This can be contrasted to field observations from Para, where yields
# average 0.7 m3.ha−1". 



# What is the share of permits (valid) displaying overstated yields? (Noting
# these values may change slightly given randomization and therefore why certain
# values are left as comments in text)
data_fig2_valid |> 
  filter(YIELD > dados$Q99.RADAM) |>  #q99 Radam = 2.4442 for all permits. 
  count()
data_fig2_valid |> 
  count()
311/1103*100

# N.B. ..."28% of valid permits display yields that exceed the RADAM plots 99%
# percentile class (2.4 m3.ha−1)."


data_fig2_valid |> 
  filter(YIELD > dados$Q99.RADAM) |>  #q99 Radam = 2.4442 for all permits
  summarise(sum(VOLUME))
data_fig2_valid |> 
  summarise(sum(VOLUME))
356283/740206*100

#N.B. ..."nearly half (48%, 0.36 Mm3) of the volume entering the supply chain
#originates from these permits"

data_fig2_valid |> 
  filter(YIELD > dados$Q99.RADAM) |>  #q99 Radam = 2.4442 for all permits
  summarise(sum(AREA_HA))
data_fig2_valid |> 
  summarise(sum(AREA_HA))
100209/721079*100

#N.B. ..." but this volume is extracted from only 14% (0.1Mha) of the total
#licensed area under valid permits (see also Suppl. Fig. S2)."

# Extra figures for different thresholds 
data_fig2_valid |> 
  filter(YIELD > dados$Q99.RADAM) |> 
  summarise(mean(YIELD))
data_fig2_valid |> 
  filter(YIELD > dados$Q99.RADAM) |> 
  summarise(sd(YIELD))


data_fig2_valid |> 
  filter(YIELD > 4.09) |>  #q99 Radam = 2.4442 for all permits
  count()
data_fig2_valid |> 
  count()
96/1103*100


data_fig2_valid |> 
  filter(YIELD > 4.09) |>  #q99 Radam = 2.4442 for all permits
  summarise(sum(VOLUME))
data_fig2_valid |> 
  summarise(sum(VOLUME))
109674/740206*100

# Figures for AUTEX (federally-issued permits)
data_fig2_autex |> 
  summarise(mean(YIELD))
data_fig2_autex |> 
  summarise(sd(YIELD))

#N.B. ..." areas from federal-licensed enterprises display yields much more
#aligned with stocking volumes derived from RADAM plots, with an average 0.68
#m3ha−1 (sd: 0.87 m3ha−1)."

data_fig2_autex |> 
  count() #29
data_fig2_valid |> 
  count() #1103
29/1103*100


# Clean env.
rm(dados, t.radam, t.licensas, sd.radam, sd.licensas,
   mean.radam, median.radam, q90.radam, q95.radam, q99.radam,
   mean.licensas, median.licensas, q90.licensas, q95.licensas, q99.licensas,
   limiar, Para_plots, Para_vol_radam, unique.licensas, unique.Radam)

rm(valor.sps, vol_radam, licensas, n.radam, n.rando, nomes.ci, NOMES.CI,
   n.plots, only.sps.used, parcelas, probability, density, species,
   sps.names.plot, means, i, j, cap.min, radam_fdp, radam_hist, lp_hist, 
   dens_radam, dens_lp, v, LISTA_FDP.RADAM)

rm(t.hist.autef, t.hist.radam, col.autef, col.radam, range.autef, range.radam,
   x, y, n, y.max)


## Suppl. Fig. S4 Sensitivity RADAM yield distribution simulation ------------

### Suppl. Fig. S4a -----------------------------------------------------------------
# Quartile-based simulation check 

# What areas to consider? 
# All areas
summary(data_fig2_all$AREA_HA)
# Min.         1st Qu.          Median            Mean         3rd Qu.            Max. 
# 18.7537000      66.4294250     370.0337000    3561.0098098    1003.4181000 1289338.1614000 

# Splitting the dataset into quartiles to discuss area bias
summary(data_fig2_all$AREA_HA) |> tibble() |> pull("summary(data_fig2_all$AREA_HA)")


# Defining categories according to area quartiles
data_fig2_all$cat_var <- cut(data_fig2_all$AREA_HA,
                             breaks = c(18,  66, 370, 1003, 13000000),
                             labels = c('Q1(19-]', 'Q2(66-]', 'Q3(370-]', 'Q4(1003-1.3M)'))

# Applying lower boundaries to simulations 19, 66, 370ha
data_suppl_radam1 <- data_fig2_all |> 
  mutate(class = case_when(cat_var =='Q1(19-]' & YIELD<dados_19ha$Q90.RADAM ~ "<90%", 
                           cat_var =='Q1(19-]' & YIELD<dados_19ha$Q95.RADAM ~ "90-95%",
                           cat_var =='Q1(19-]' & YIELD<dados_19ha$Q99.RADAM ~ "95-99%", 
                           cat_var =='Q1(19-]' & YIELD>dados_19ha$Q99.RADAM ~ ">99%",
                           cat_var =='Q2(66-]' & YIELD<dados_66ha$Q90.RADAM ~ "<90%", 
                           cat_var =='Q2(66-]' & YIELD<dados_66ha$Q95.RADAM ~ "90-95%",
                           cat_var =='Q2(66-]' & YIELD<dados_66ha$Q99.RADAM ~ "95-99%", 
                           cat_var =='Q2(66-]' & YIELD>dados_66ha$Q99.RADAM ~ ">99%", 
                           cat_var =='Q3(370-]' & YIELD<dados_370ha$Q90.RADAM ~ "<90%", 
                           cat_var =='Q3(370-]' & YIELD<dados_370ha$Q95.RADAM ~ "90-95%",
                           cat_var =='Q3(370-]' & YIELD<dados_370ha$Q99.RADAM ~ "95-99%", 
                           cat_var =='Q3(370-]' & YIELD>dados_370ha$Q99.RADAM ~ ">99%",
                           cat_var =='Q4(1003-1.3M)' & YIELD<dados_19ha$MEAN.RADAM ~ "<90%",
                           cat_var =='Q4(1003-1.3M)' & YIELD>dados_19ha$MEAN.RADAM ~ ">99%",
                           TRUE~as.character(NA)))



# Inset volume table
# All permits 
vol_inset1 <- data_fig2_all |> 
  group_by("Quantile (ha)" = cat_var) |> 
  summarise("Volume (m³)" = sum(VOLUME)) |> 
  adorn_rounding()
#Valid only
vol_inset2 <- data_fig2_all |> 
  filter(STATUS_GROUP1=="VALID") |> 
  group_by("Quantile (ha)" = cat_var) |> 
  summarise("Volume (m³)" = sum(VOLUME)) |> 
  adorn_rounding() 
# Inset for valid only since risk applies to this share
vol_inset <- vol_inset2 

# Remove top 10 outliers for yield
n <- data_suppl_radam1 |> 
  select(YIELD) |> 
  slice_max(YIELD, n = 10) |> 
  slice_min(YIELD, n = 1) |> pull()

p1 <- data_suppl_radam1 |> 
  filter(YIELD < n, 
         #STATUS_GROUP1 == "VALID", 
         !is.na(cat_var)) |> 
  ggplot(aes(x = cat_var, y=YIELD))+
  geom_jitter(aes(colour=class), size=0.9, alpha=0.6, position = position_jitter(width = .2)) +
  scale_colour_manual(breaks =  c("<90%",  "90-95%",  "95-99%", ">99%"),
                      values = c("#2D609B",  "#CBB94AFF",  "#A66B20FF", "#843837"))+
  annotate("rect", xmin=c(0.9,0.9), xmax=c(1.1, 1.1),
           ymin=c(quantile(means_19ha, 0.25), quantile(means_19ha, 0.25)) ,
           ymax=c(quantile(means_19ha, 0.75), quantile(means_19ha, 0.75)),
           linewidth = 1, alpha=0.2, color="black")+
  annotate("segment", x = 0.9, xend = 1.1, 
           y = median(means_19ha), yend = median(means_19ha), linewidth = 1)+
  annotate("segment", x = 1.9, xend = 2.1, 
           y = median(means_66ha), yend = median(means_66ha), linewidth = 1)+
  annotate("segment", x = 2.9, xend = 3.1, 
           y = median(means_370ha), yend = median(means_370ha), linewidth = 1)+
  annotate("rect", xmin=c(1.9,1.9), xmax=c(2.1, 2.1),
           ymin=c(quantile(means_66ha, 0.25), quantile(means_66ha, 0.25)) ,
           ymax=c(quantile(means_66ha, 0.75), quantile(means_66ha, 0.75)),
           linewidth = 1, alpha=0.2, color="black")+
  annotate("rect", xmin=c(2.9,2.9), xmax=c(3.1, 3.1),
           ymin=c(quantile(means_370ha, 0.25), quantile(means_370ha, 0.25)) ,
           ymax=c(quantile(means_370ha, 0.75), quantile(means_370ha, 0.75)),
           linewidth = 1, alpha=0.2, color="black")+
  annotate("pointrange", 
           x = 1, # Positioning on first category 
           y = median(means_19ha), 
           ymin = min(means_19ha), 
           ymax = max(means_19ha), 
           colour = "black", size = 0.1, linewidth = 1, alpha=0.9, shape= ".") + # 17, 19, 25
  annotate("pointrange", 
           x = 2, # Positioning on first category 
           y = median(means_66ha), 
           ymin = min(means_66ha), 
           ymax = max(means_66ha), 
           colour = "black", size = 0.1, linewidth = 1, alpha=0.9, shape=".") +
  annotate("pointrange", 
           x = 3, # Positioning on first category 
           y = median(means_370ha), 
           ymin = min(means_370ha), 
           ymax = max(means_370ha), 
           colour = "black", size = 0.1, linewidth = 1, alpha=0.9, shape=".") +
  ggpp::annotate(geom = "table", x = 4.5, y = 8.5, label = list(vol_inset)) +
  geom_hline(yintercept = dados_19ha$MEAN.RADAM, linetype= "dashed", color = "black") +
  geom_hline(yintercept = dados_19ha$Q99.RADAM, linetype= "dashed", color = "black") +
  scale_y_continuous(breaks = c(0, 0.7, 2, 2.5, 4, 6, 8))+
  labs(y = expression(paste("Yield (", m^3, ha^-1, ")", sep="")), x = "", 
       colour = "RADAM yield \n baseline",
       tag = "a")+
  theme(plot.tag = element_text(face="bold"))

#theme(legend.position="bottom")


# Check figure
figS4a <- p1



### Suppl. Fig. S4b/S4c -------------------------------------------------------

# Trend yield over area 

# What is the share of valid? Baseline for area and volume  
data_fig2_all |> 
  filter(STATUS_GROUP1 == "VALID") |> 
  summarise(AREA_SUM = sum(AREA_HA), VOL_SUM= sum(VOLUME))

cnt_valid <- data_fig2_all |> 
  filter(STATUS_GROUP1 == "VALID") |> count()


figS4b <- data_fig2_all |> 
  filter(AREA_HA < 100, YIELD < n, STATUS_GROUP1 == "VALID") |> 
  ggplot(aes(x = AREA_HA, y = YIELD)) +
  geom_point() +
  stat_poly_eq() +
  geom_smooth(method="lm", se=FALSE)+
  labs(y = expression(paste("Yield (", m^3, ha^-1, ")", sep="")), x = "Area (ha)", 
       tag = "b") +
  coord_cartesian(ylim = c(0, 8)) +
  theme(plot.tag = element_text(face="bold"))

data_fig2_all |> 
  filter(AREA_HA < 100, YIELD < n, STATUS_GROUP1 == "VALID") |> 
  count()



figS4c <- data_fig2_all |> 
  filter(AREA_HA < 1000, AREA_HA > 100, YIELD < n, STATUS_GROUP1 == "VALID") |> 
  ggplot(aes(x = AREA_HA, y = YIELD)) +
  geom_point() +
  stat_poly_eq() +
  geom_smooth(method="lm", se=FALSE)+ 
  labs(y = "", x = "Area (ha)", tag = "c")+
  coord_cartesian(ylim = c(0, 8), 
                  xlim = c(100, 1000))+
  scale_x_continuous(breaks = c(100, 250, 500, 750, 1000))+
  theme(plot.tag = element_text(face="bold"))


data_fig2_all |> 
  filter(AREA_HA < 1000, AREA_HA > 100, YIELD < n, STATUS_GROUP1 == "VALID") |> 
  count()


figS4 <- figS4a/(figS4b|figS4c)

# Saving figs
#jpeg
jpeg("./results/figS4.jpeg",
     width = 10, height = 12, units = "in",
     bg = "white", res = 700)
print(figS4)
dev.off()

#pdf
ggsave(filename = "./results/figS4.pdf",
       width = 10,
       height = 12,
       dpi=700,
       units = "in")

# Clean env. 
rm(vol_inset, vol_inset1, vol_inset2, vol_inset3, radam_hist_19ha, radam_hist_66ha, 
   radam_hist_370ha, means_19ha, means_66ha, means_370ha, dados_19ha, dados_66ha, 
   dados_370ha)

rm(figS4, figS4a, figS4b, figS4c)


## Suppl. Fig. S5 ------------------------------------------------------------

# Location of overestimated volume 
loc_overst <- data_fig2_valid_and_autex |> 
  filter(YIELD > 2.5, TYPE != "AUTEX_SV") # AUTEX_SV; Autex for supressão veg. (legal deforestation)

# By mun centroid
mun_overst <- mun_centroid |> 
  filter(code_state == "15") |> 
  select(code_muni, name_muni, X, Y) |> 
  mutate(across(where(is.character), toupper)) |> 
  mutate(across(where(is.character), ~stringi::stri_trans_general(., "Latin-ASCII")))

loc_overst_mun <- loc_overst |>
  group_by(MUNICIPALITY) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  left_join(mun_overst, by = c("MUNICIPALITY"="name_muni"))



# Without basemap
p1 <- ggplot() +
  geom_sf(data = mun_pa, color = gray(.7)) +
  geom_point(data = loc_overst, aes(x = X, y = Y, size = VOLUME), color = "#843837",
             alpha = 0.5, stroke = FALSE) +
  scale_size(name = "Roundwood production \n by Logging Permit \n (Yield >99%)", 
             limits = c(min(data_fig1$VOLUME), 
                        max(data_fig1$VOLUME)), 
             range = c(0, 10), 
             labels = label_number(scale_cut = cut_short_scale(), suffix = " m\u00B3"))+
  #labs(tag = "a") + 
  theme_void() +
  theme(plot.title.position = "plot", 
        plot.tag = element_text(face="bold"))


# Plot with mun centroid
#draw scatterpie 
p2 <- ggplot()  +
  geom_sf(data = mun_pa, color = gray(.7)) +
  geom_point(data = loc_overst_mun, aes(x = X, y = Y, size = VOLUME, 
                                        color = reorder(MUNICIPALITY, VOLUME, FUN = sum, decreasing = T)),
             alpha = 0.8, stroke = FALSE) +
  scale_size(name = "Summary \n by Municipality", 
             range = c(0, 15), 
             labels = label_number(scale_cut = cut_short_scale(), suffix = " m\u00B3"))+ 
  scale_color_manual(values=paletteer_c(`"scico::lapaz"`, n=29, direction = -1))+
  #labs(tag = "b") + 
  guides(color = "none")+     
  theme_void() +
  theme(plot.tag = element_text(face="bold"))


p3 <- loc_overst |> 
  filter(STATUS_GROUP1 == "VALID") |> 
  mutate(MUNICIPALITY = str_to_title(MUNICIPALITY)) |> 
  # mutate(MUNICIPALITY = fct_lump(MUNICIPALITY, n)) |> 
  ggplot() +
  geom_col(mapping = aes(x=STATUS_GROUP1, y=VOLUME, fill = reorder(MUNICIPALITY, VOLUME, FUN = sum, decreasing = T))) +
  scale_fill_manual(name = "Municipality of \n Logging Permit",
                    values = paletteer_c(`"scico::lapaz"`, n=29, direction = -1)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale(), suffix = " m\u00B3")) +
  ylab("")+
  xlab("")+
  theme_minimal() +
  theme(axis.text.x = element_blank())

figS5a <- (p3+p2) + 
  plot_layout(widths = c(1, 3), 
              guides = 'collect')

figS5 <- (p1|figS5a) +
  plot_annotation(tag_levels = list(c('a', 'b', ''))) &
  theme(plot.tag = element_text(face = 'bold'))


# Saving figs
#jpeg
jpeg("./results/figS5.jpeg",
     width = 20, height = 10, units = "in",
     bg = "white", res = 700)
print(figS5)
dev.off()

#pdf
ggsave(filename = "./results/figS5.pdf",
       width = 20,
       height = 10,
       dpi=700,
       units = "in")

# Clean env. 
rm(figS5, figS5a, p1, p2, p3)


# Municipality by overstated yields 
loc_overst |> 
  group_by(MUNICIPALITY) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  arrange(desc(VOLUME)) |> 
  adorn_totals() |> as_tibble() 

top_mun <-   
  loc_overst_mun |> 
  group_by(MUNICIPALITY)|>  
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup()|> 
  arrange(desc(VOLUME)) |> 
  top_n(wt = VOLUME, n = 2)|>  
  pull(MUNICIPALITY)

top_mun 
#[1] "JURUTI"   "SANTAREM"

sum((loc_overst_mun |> filter(MUNICIPALITY %in% top_mun))$VOLUME)/sum(rw$VOLUME)

sum((loc_overst_mun |> filter(MUNICIPALITY %in% top_mun))$VOLUME)/sum(loc_overst_mun$VOLUME)

sum((loc_overst_mun |> filter(MUNICIPALITY %in% top_mun))$VOLUME)

#N.B. ..."Juruti and Santarém alone accounts for 31% of the overstated share
#(0.11 Mm3; 11% of the entire roundwood production)". 

# Clean env.
rm(loc_overst, loc_overst_mun, mun_overst, p1,p2,p3, top_mun)


## Suppl. Fig. S7 --------------------------------------------------------

# Setting up display order for all status categories
levels(data_fig2_all$STATUS_GROUP3) <- c("Expired",
                                         "Extended",
                                         "Missing Activation Date",
                                         "Suspended",
                                         "Cancelled",
                                         "Cancelled (permit substitution)",
                                         "Cancelled (elaboration failure)",
                                         "Cancelled (non-complience with conditions)",
                                         "Cancelled (illegality)",
                                         "National jurisdiction (AUTEX)")


#Yield
count_labels_a <- data_fig2_all |> 
  group_by(STATUS_GROUP3) |> 
  mutate(STATUS_GROUP3 = fct_rev(STATUS_GROUP3)) |> 
  summarise(textlabel = paste("n:", format(n(), big.mark = " "))) 

figS7a <- data_fig2_all |> 
  ggplot(aes(x = STATUS_GROUP3, y = YIELD))+
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6, aes(color=STATUS_GROUP3))+
  scale_color_manual(values = c("#2D609B","#3C79A5","#A67A60","#A16959","#9C5651", 
                                "#8F4342", "#792C2C","#631717","#4D0001","grey50")) +
  xlab("")+
  ylab(expression(paste(
    "Handroanthus spp. Yield (",
    m^3, ha^-1,
    ")", sep="")))+
  labs(title = "Yield distribution by status of logging permit ", 
       tag = "a")+
  scale_x_discrete(labels = paste(count_labels_a$STATUS_GROUP3," (", count_labels_a$textlabel,")",sep=""), 
                   limits=rev) +
  coord_flip() +
  theme(legend.position="none",
        #plot.title.position = "plot",
        plot.tag = element_text(face="bold"))





# Volume 
count_labels_b <- data_fig2_all |> 
  group_by(STATUS_GROUP3) |> 
  mutate(STATUS_GROUP3 = fct_rev(STATUS_GROUP3)) |> 
  summarise(textlabel = paste("n:", format(n(), big.mark = " "))) 


figS7b <- data_fig2_all |> 
  ggplot(aes(STATUS_GROUP3, VOLUME))+
  geom_boxplot(outlier.alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.6, aes(color=STATUS_GROUP3))+
  scale_color_manual(values = c("#2D609B","#3C79A5","#A67A60","#A16959","#9C5651", 
                                "#8F4342", "#792C2C","#631717","#4D0001","grey50")) +
  ylab(expression(paste(
    "Handroanthus spp. Volume (",
    m^3,
    ")", sep="")))+
  xlab("")+
  labs(title = "Volume distribution by status of logging permit ", 
       tag = "b")+
  scale_x_discrete(labels = paste(count_labels_b$STATUS_GROUP3," (", count_labels_b$textlabel,")",sep=""), 
                   limits=rev) +
  coord_flip() +
  theme(legend.position="none",
        #plot.title.position = "plot",
        plot.tag = element_text(face="bold"))

# Area 
count_labels_c <- data_fig2_all |> 
  filter(AREA_HA < 10000) |> 
  group_by(STATUS_GROUP3) |> 
  mutate(STATUS_GROUP3 = fct_rev(STATUS_GROUP3)) |> 
  summarise(textlabel = paste("n:", format(n(), big.mark = " "))) 

figS7c <- data_fig2_all |> 
  filter(AREA_HA < 10000) |> 
  ggplot(aes(STATUS_GROUP3, AREA_HA))+
  geom_boxplot(outlier.alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.6, aes(color=STATUS_GROUP3))+
  scale_color_manual(values = c("#2D609B","#3C79A5","#A67A60","#A16959","#9C5651", 
                                "#8F4342", "#792C2C","#631717","#4D0001","grey50")) +
  ylab("Logging permit area (ha)")+
  xlab("")+
  labs(title = "Area distribution by status of logging permit", 
       tag = "c")+
  scale_x_discrete(labels = paste(count_labels_c$STATUS_GROUP3," (", count_labels_c$textlabel,")",sep=""), 
                   limits=rev) +
  coord_flip() +
  theme(legend.position="none",
        #plot.title.position = "plot",
        plot.tag = element_text(face="bold"))


figS7 <- figS7a/figS7b/figS7c

# Saving figs
# jpeg
jpeg("./results/figS7.jpeg", 
     width = 12, height = 15, units = "in", 
     bg = "white", res = 700)
print(figS7)
dev.off()

#pdf
ggsave(filename = "./results/figS7.pdf",
       width = 12,
       height = 15,
       dpi=700,
       units = "in")

# Clean env. 
rm(figS7, figS7a, figS7b, figS7c)

rm(data_fig2_valid, data_fig2_invalid, data_fig2_autex, data_fig2_all, 
   data_fig2_valid_and_autex)



## Methods/Supplementary text ----------------------------------------------

#[Not immediately reproducible]
#TODO: re-add scientific name for stats 

# What species do we include and what is the volume breakdown?
rw |> group_by(SCIENTIFIC_NAME) |> 
  summarise(sum(VOLUME)) |> 
  arrange(desc(`sum(VOLUME)`)) |> 
  adorn_totals() 

rw |> group_by(SCIENTIFIC_NAME) |> 
  summarise(sum(VOLUME)) |> 
  arrange(desc(`sum(VOLUME)`)) |> 
  adorn_totals() |> 
  adorn_percentages("col") 

# How is reporting done more recently?
rw|> filter(ID_YEAR == 2019)  |> 
  group_by(SCIENTIFIC_NAME) |> 
  summarise(sum(VOLUME)) |> 
  arrange(desc(`sum(VOLUME)`)) |> 
  adorn_totals() |> 
  adorn_percentages("col") 


rw |> mutate(SP_GROUP = case_when(SCIENTIFIC_NAME %in% c("TABEBUIA SERRATIFOLIA (VAHL) NICHOLS.", 
                                                         "HANDROANTHUSSERRATIFOLIUS", 
                                                         "HANDROANTHUS SERRATIFOLIUS", 
                                                         "TABEBUIASERRATIFOLIA",
                                                         "TABEBUIA SERRATIFOLIA") ~ "HANDROANTHUS SERRATIFOLIUS",
                                  SCIENTIFIC_NAME %in% c("TABEBUIA IMPETIGINOSA (MART. EX DC.) STANDL.",
                                                         "TABEBUIAIMPETIGINOSA", 
                                                         "HANDROANTHUS IMPETIGINOSUM", 
                                                         "HANDROANTHUSIMPETIGINOSUM") ~ "HANDROANTHUS IMPETIGINOSUM", 
                                  SCIENTIFIC_NAME %in% c("TABEBUIA SP.", 
                                                         "TABEBUIA", 
                                                         "TABEBUIA SP") ~ "TABEBUIA SP", 
                                  TRUE ~ "OTHERS")) |> 
  group_by(SP_GROUP) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  adorn_totals() |> 
  adorn_percentages("col") 

# SP_GROUP                   VOLUME
# HANDROANTHUS IMPETIGINOSUM 0.055215489803281
# HANDROANTHUS SERRATIFOLIUS 0.817384605893647
# OTHERS                     0.047811687780342
# TABEBUIA SP                0.079588216522730
# Total                      1.000000000000000


rw |> filter(ID_YEAR == 2019)  |> 
  mutate(SP_GROUP = case_when(SCIENTIFIC_NAME %in% c("TABEBUIA SERRATIFOLIA (VAHL) NICHOLS.", 
                                                     "HANDROANTHUSSERRATIFOLIUS", 
                                                     "HANDROANTHUS SERRATIFOLIUS", 
                                                     "TABEBUIASERRATIFOLIA",
                                                     "TABEBUIA SERRATIFOLIA") ~ "HANDROANTHUS SERRATIFOLIUS",
                              SCIENTIFIC_NAME %in% c("TABEBUIA IMPETIGINOSA (MART. EX DC.) STANDL.",
                                                     "TABEBUIAIMPETIGINOSA", 
                                                     "HANDROANTHUS IMPETIGINOSUM", 
                                                     "HANDROANTHUSIMPETIGINOSUM") ~ "HANDROANTHUS IMPETIGINOSUM", 
                              SCIENTIFIC_NAME %in% c("TABEBUIA SP.", 
                                                     "TABEBUIA", 
                                                     "TABEBUIA SP") ~ "TABEBUIA SP", 
                              TRUE ~ "OTHERS")) |> 
  group_by(SP_GROUP) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  adorn_totals() |> 
  adorn_percentages("col") 

# N.B. Figures here only relate to production/roundwood. More species details on
# products code.

#Clean env.
rm(data_fig2_all, data_fig2_autex, data_fig2_invalid, data_fig2_valid, 
   data_fig2_valid_and_autex)



# Fig 3 Ipe origins and destination ---------------------------------------

# Pre-process transport data. 
# The output of the next two chunks of code is the "rw" loaded initially, but
# logic provided here for reference. This (and "rw" object) are equivalent to
# the production vector.

# Production by species
p_sp_lp <- transport_df2 |>
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
  mutate(LP_REF = if_else(is.na(LP_REF), PERMIT_NUMBER, LP_REF))

p_sp_lp <- rw


## Pre-processing share of illegality risk at origin  ----------------------

# Joining volume and permit data
join_p_lp_data <- p_sp_lp |>  
  left_join(lp, by = c( "LP_REF" = "LP_NUM")) 

# Get production of valid, invalid & missing, autex
data_production_status <- join_p_lp_data |> 
  mutate(LP_SOURCE = case_when(is.na(LP_SOURCE) ~ "UNDETERMINED", TRUE ~ LP_SOURCE)) |>
  mutate(LP_STATUS = case_when(is.na(LP_STATUS) ~ "UNDETERMINED", TRUE ~ LP_STATUS)) |> 
  mutate(STATUS_GROUP1 = case_when(LP_STATUS %in% c("EXPIRED",
                                                    "EXTENDED", 
                                                    "EXTENDED_CANCELLED SUSPENSION") ~ "VALID", 
                                   LP_STATUS %in% c("SUSPENDED", 
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
share_status_production <- data_production_status |> 
  group_by(CPF_CNPJ_ORIGIN, GEOCMUN_ORIGIN, LP_REF, STATUS_GROUP1) |> # ID_YEAR
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup() 

# Add share of valid overstated
# permit data from the transport side
vol_by_lp <- p_sp_lp |>
  group_by(LP_REF) |>
  summarise(VOLUME = sum(VOLUME))

# joining volume and permit data
join_vol_lp_data <- vol_by_lp |>  
  left_join(lp, by = c( "LP_REF" = "LP_NUM")) 

# obtaining yield
valid_overst_yield <- join_vol_lp_data |> 
  filter(LP_STATUS %in% c("EXPIRED", "EXTENDED", "EXTENDED_CANCELLED SUSPENSION")) |> 
  mutate(YIELD = VOLUME/AREA_HA) |> 
  mutate(OVERSTATED_Q99 = if_else(YIELD > 2.5, 1, 0)) |> 
  filter(OVERSTATED_Q99 == 1) |> 
  select(LP_REF, OVERSTATED_Q99) 

# Adding overstated yield share
share_status_production <- share_status_production |> 
  left_join(valid_overst_yield, by = 'LP_REF') |>
  mutate(STATUS_GROUP1 = case_when(OVERSTATED_Q99 == 1 ~ "VALID_OVERSTATED", 
                                   TRUE ~ STATUS_GROUP1)) |> 
  select(-c(OVERSTATED_Q99, LP_REF)) |> 
  unite("CPF_CNPJ_GEOCMUN", CPF_CNPJ_ORIGIN:GEOCMUN_ORIGIN)

#Adding volume from outer state (loaded object from input-output model analysis)
p_outer_state 
# p_outer_state <- colSums(Z) |> 
#   enframe(name = "CPF_CNPJ_GEOCMUN", value = "RWE") |> 
#   mutate(RWE_OUTER_STATE = case_when(!str_detect(CPF_CNPJ_GEOCMUN, "_15") ~ RWE, TRUE ~ 0)) |> 
#   select(-RWE)
share_outer_state <-  p_outer_state |> 
  filter(RWE_OUTER_STATE > 0) |> 
  rename(VOLUME = RWE_OUTER_STATE) |> 
  mutate(STATUS_GROUP1 = "OUTER_STATE_AUTEX")

#Adding discrepancies by municipality (loaded object from input-output model analysis)
cons_discrep 
# cons_discrep <- ifelse((dmi - colSums(Z)) < 0, abs((dmi - colSums(Z))) ,0) |>
#   as.vector() |>
#   setNames(rownames(Z))
share_cons_discrep <- cons_discrep |> enframe(name = "CPF_CNPJ_GEOCMUN", value = "VOLUME") |> 
  filter(VOLUME > 0) |> 
  mutate(STATUS_GROUP1 = "DISCREPANCY")

share_status <- bind_rows(share_status_production, share_outer_state, share_cons_discrep)


# Create a data frame with shares of valid, valid overstated, invalid & missing and autex
share_status <- share_status |>
  group_by(CPF_CNPJ_GEOCMUN, STATUS_GROUP1) |> 
  summarise(VOLUME = sum(VOLUME)) |> 
  ungroup() |> 
  pivot_wider(names_from = STATUS_GROUP1, values_from = VOLUME, values_fill = 0) 


# Wrangling for plotting by municipality 
share_mun <- share_status |> 
  separate(CPF_CNPJ_GEOCMUN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_") |> 
  group_by(GEOCMUN_ORIGIN) |> 
  summarise(across(DISCREPANCY:AUTEX, ~ sum(.x))) |> 
  mutate(TOTAL = rowSums(across(where(is.numeric)))) |> 
  left_join(mun_centroid, by = c('GEOCMUN_ORIGIN' = 'code_muni'))



## Fig 3a Scatterpie -------------------------------------------------------

# Check out centroid of municipalities for annotation
mun_centroid |> filter(name_muni == "Santarém")
mun_centroid |> filter(name_muni == "Juruti")
mun_centroid |> filter(name_muni == "Prainha")

mun_centroid |> filter(name_muni == "Itaituba")
mun_centroid |> filter(name_muni == "Altamira")
mun_centroid |> filter(name_muni == "Novo Progresso")

mun_centroid |> filter(name_muni == "Uruará")
mun_centroid |> filter(name_muni == "Belém")
mun_centroid |> filter(name_muni == "Barcarena")
mun_centroid |> filter(name_muni == "Breu Branco")
mun_centroid |> filter(name_muni == "Dom Eliseu")


# Zooming in
states_crop <- st_crop(states, xmin = -60 , xmax = -45, ymin = -11, ymax = 2.5)


# remove bubbles that do not add to the picture (more manual but data available
# for more detailed check)
share_mun_omitted <- share_mun |> 
  filter(between(X,-60,-45), between(Y, -11, 2.5))


# Draw scatterpie 
fig3a <- ggplot()  +
  geom_sf(data = states_crop, color = gray(.7)) +
  geom_sf(data = mun_pa, color = gray(.5)) +
  geom_scatterpie(data = share_mun_omitted, 
                  aes(x = X, y = Y, r = sqrt(TOTAL)/700), # refine scaling for plot display (also, why to use sqrt https://www.r-bloggers.com/2018/03/the-illusion-of-linearity-trap-when-making-bubble-charts-in-ggplot2/)
                  cols = c("VALID", 
                           "VALID_OVERSTATED", 
                           "INVALID_MISSING",  
                           "AUTEX",
                           "OUTER_STATE_AUTEX",
                           "DISCREPANCY"),
                  alpha = 0.6) +
  scale_fill_manual(
    breaks = c("VALID", 
               "VALID_OVERSTATED", 
               "INVALID_MISSING",  
               "AUTEX",
               "OUTER_STATE_AUTEX",
               "DISCREPANCY"),
    labels = c( "Valid: Expired, Extended",
                "Valid: Expired, Extended (>99%)", 
                "Invalid: Cancelled, Suspended, \n Missing activation date", 
                "Enterprises under National Jurisdiction", 
                "Other states", 
                "Missing flows"),
    values = c("VALID" = "#518DA6", 
               "VALID_OVERSTATED" = "#A37824", 
               "INVALID_MISSING" = "#6F2222",  
               "AUTEX" = "#D3B18E",
               "OUTER_STATE_AUTEX" = "grey70",
               "DISCREPANCY" = "black")) +
  annotate("segment", x = -55.2, xend = -55.3, y = -2.68, yend =  -1.3, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -56.2, xend = -57.7, y = -2.62, yend =  -2.1, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -53.7, xend = -52.8, y = -2.12, yend =  -1, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -56.5, xend = -57.5, y = -5.87, yend =  -6.5, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -55.6, xend = -55.9, y = -8.06, yend =  -9.0, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -53.9, xend = -53.6, y = -6.48, yend =  -7.8, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -53.8, xend = -53.6, y = -3.58, yend =  -4.7, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -48.5, xend = -48.31, y = -1.24, yend =  0.3, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -48.6, xend = -47.3, y = -1.50, yend =  -0.1, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -49.4, xend = -49.3, y = -3.73, yend =  -4.6, colour = "black", size=0.5, alpha=0.8) +
  annotate("segment", x = -47.9, xend = -46.7, y = -4.19, yend =  -4.9, colour = "black", size=0.5, alpha=0.8) +
  annotate("text", 
           x = c(-55.3, -57.7, -52.8, 
                 -57.5, -55.9, -53.6, 
                 -53.6, -48.31, -47.3, 
                 -49.3, -46.8), 
           y = c(-1.1, -1.9, -0.8, 
                 -6.7, -9.2, -8, 
                 -4.9, 0.5, 0.1, 
                 -4.8, -5.1), 
           
           label = c("Santarém", "Juruti", "Prainha", 
                     "Itaituba", "NovoProgresso", "Altamira", 
                     "Uruará",  "Belém", "Barcarena", "BreuBranco", "DomEliseu") , #"italic(Santarém)" 
           color="black", size=4 , angle=0, fontface="bold", parse = TRUE) +
  labs(tag = "a",
       x = "", 
       y = "",
       fill = NULL) +
  theme_minimal()  +
  theme(legend.position = "none", 
        #legend.position = c(0.85, 0.85)
        plot.tag = element_text(face="bold")) 



## Pre-processing io and env. ext. strings -----------------------------------

# Pre-process strings for full io
start_time <- Sys.time()
io_mun_all <- io_CPF_CNPJ_GEOCMUN|> 
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>  
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  #actually worry if there is no warnings, because the next steps are based on the NA assigment
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>  
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy
end_time <- Sys.time()
end_time - start_time

# Check totals to see consequences of filtering small numbers
sum(io_CPF_CNPJ_GEOCMUN$RWE)  
sum(io_mun_all$RWE)

# N.B. io_CPF_CNPJ_GEOCMUN (as well as the env. ext.) contains the long-form
# data for the output square matrix. Since the original is sparce, several
# origin-destination values are zero (or very small values) the root of
# inefficiencies here. Filtering zeros and such small values supports processing
# for reproducing the work fast, nonetheless for full reproduction one should
# comment out " #filter(RWE > 0)"

# Pre-process strings for the share of valid consumption
io_mun_valid <- ee_valid_cons|>  
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>  
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>  
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy

# Pre-process strings for the share of valid but overestimated consumption
io_mun_valid_overst <- ee_valid_overst_cons |>
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|> 
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy

# Pre-process strings for the share of valid but invalid and missing 
io_mun_invalid_missing <- ee_invalid_missing_cons |>
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy

# Pre-process strings for the share of autex
io_mun_autex <- ee_autex_cons |>
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy



## Fig 3b Alluvium -------------------------------------------------------

# Wrangling for the visualization: 

# Bring all status shares together and obtain remaining two (discrepancies and outer-state flows)
io_alluv_df1 <- io_mun_all |> 
  left_join(io_mun_valid, by = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION", 
                                 "CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN", "INDEX")) |> 
  left_join(io_mun_valid_overst, by = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION", 
                                        "CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN", "INDEX")) |> 
  left_join(io_mun_invalid_missing, by = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION", 
                                           "CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN", "INDEX")) |> 
  left_join(io_mun_autex, by = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION", 
                                 "CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN", "INDEX")) |> 
  rename(RWE_IO = RWE.x, 
         RWE_E_valid = RWE.y, 
         RWE_E_valid_overst = RWE.x.x,
         RWE_E_invalid_missing = RWE.y.y, 
         RWE_E_autex = RWE) |> 
  mutate(across(RWE_IO:RWE_E_autex, ~ replace_na(.x, 0))) |> 
  # Approximate share that enters from outer-state 
  mutate(RWE_outer_state = case_when(INDEX != 1 ~ RWE_IO - RWE_E_valid - RWE_E_valid_overst - RWE_E_invalid_missing - RWE_E_autex, 
                                     TRUE ~ 0)) |> 
  mutate(RWE_discrep = case_when(INDEX == 1 ~ RWE_IO, TRUE ~ 0))

# All shares in tidy format
io_alluv_df2 <- io_alluv_df1 |> 
  pivot_longer(cols = c(RWE_IO, 
                        RWE_E_valid, 
                        RWE_E_valid_overst, 
                        RWE_E_invalid_missing, 
                        RWE_E_autex, 
                        RWE_outer_state, 
                        RWE_discrep), names_to = "STATUS", values_to = "RWE") |> 
  filter(STATUS != "RWE_IO")

# Double-check figures
io_alluv_df2 |> filter(STATUS == "RWE_IO") |> summarise(sum(RWE))
io_alluv_df2 |> filter(STATUS == "RWE_E_valid") |> summarise(sum(RWE))
io_alluv_df2 |> filter(STATUS == "RWE_E_valid_overst") |> summarise(sum(RWE))
io_alluv_df2 |> filter(STATUS == "RWE_E_invalid_missing") |> summarise(sum(RWE))
io_alluv_df2 |> filter(STATUS == "RWE_E_autex") |> summarise(sum(RWE))
io_alluv_df2 |> filter(STATUS == "RWE_outer_state") |> summarise(sum(RWE))
io_alluv_df2 |> filter(STATUS == "RWE_discrep") |> summarise(sum(RWE))


# Subset data
io_alluv_df3 <- io_alluv_df2 |>  
  filter(RWE > 200)|>  
  select(GEOCMUN_ORIGIN, GEOCMUN_DESTINATION, STATUS, RWE)

top_mun_org <-   
  io_alluv_df3|> 
  group_by(GEOCMUN_ORIGIN)|>  
  summarise(RWE = sum(RWE))|> 
  ungroup()|>  
  arrange(desc(RWE))|>  
  top_n(wt = RWE, n = 10)|>  #change 10 to 5 to test it
  pull(GEOCMUN_ORIGIN)

top_mun_dest <-   
  io_alluv_df3|> 
  group_by(GEOCMUN_DESTINATION)|>  
  summarise(RWE = sum(RWE))|> 
  ungroup()|>  
  arrange(desc(RWE))|>  
  top_n(wt = RWE, n = 10)|>  #change 10 to 5 to test it
  pull(GEOCMUN_DESTINATION)

io_alluv_df4 <- io_alluv_df3|> 
  mutate(GEOCMUN_ORIGIN = ifelse(GEOCMUN_ORIGIN %in% top_mun_org, GEOCMUN_ORIGIN , "Other origins"),
         GEOCMUN_DESTINATION = ifelse(GEOCMUN_DESTINATION %in% top_mun_dest, GEOCMUN_DESTINATION, "Other destinations"))|> 
  group_by(GEOCMUN_ORIGIN, GEOCMUN_DESTINATION, STATUS)|> 
  summarise(RWE = sum(RWE))|> 
  ungroup()

io_alluv_df5 <- io_alluv_df4 |>  
  left_join(mun, by = c('GEOCMUN_ORIGIN'= "code_muni"))|>  
  select(c(GEOCMUN_ORIGIN, GEOCMUN_DESTINATION, STATUS, RWE, name_muni))|>  
  mutate(GEOCMUN_ORIGIN = case_when(!is.na(name_muni) ~ name_muni, TRUE ~ GEOCMUN_ORIGIN))|> 
  select(-name_muni)|>  
  left_join(mun, by = c('GEOCMUN_DESTINATION'= "code_muni"))|>  
  select(c(GEOCMUN_ORIGIN, GEOCMUN_DESTINATION, STATUS, RWE, name_muni))|>  
  mutate(GEOCMUN_DESTINATION = case_when(!is.na(name_muni) ~ name_muni, TRUE ~ GEOCMUN_DESTINATION))|>  
  select(-name_muni)|>  
  arrange(desc(RWE))

fig3b <- ggplot(io_alluv_df5,
                aes(y = RWE, axis1 = GEOCMUN_ORIGIN, axis2 = GEOCMUN_DESTINATION)) +
  geom_alluvium(
    aes(fill = STATUS), 
    width = 1/6, 
    aes.bind = 'alluvia', #or 'flows', depending of prioritization
    alpha = .6, #transparency of flows
    knot.pos = 0.3, #somewhat defines angles of flows for certain curve_types
    #decreasing = FALSE,
    curve_type = "quintic"#"linear", "cubic", "quintic", "sine", "arctangent", and "sigmoid". "xspline"
    #segments = 2000 
  ) + 
  #creates boxes
  geom_stratum(alpha = 0.7, #transparency of lodes (municipalities)
               width = 1/6, 
               #decreasing = FALSE,
               color = "black") + 
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3.5)+
  # geom_label(stat = "stratum", aes(label = after_stat(stratum)), min.y = 200) +
  scale_fill_manual(name = "", 
                    labels = c("Missing physical flows", 
                               "Enterprises under national jurisdiction (AUTEX)",  
                               "Invalid and Missing Logging Permits", 
                               "Valid Logging Permits", 
                               "Valid Logging Permits, Yield > 99%", 
                               "Other states"), 
                    values = c("black",
                               "#D3B18E",
                               "#6F2222", #red
                               "#518DA6", #blue "#2D609B", "#843837"
                               "#A37824", # yellow/brown #F2CD3B "#E4D7BD", "#CFB889", "#B99956", "#A37824"
                               "grey70"))+
  
  labs(tag = "b") +
  theme_void() + 
  theme(legend.position = "bottom",
        legend.text = element_text(face = "italic", size = 10), 
        plot.tag = element_text(face = "bold"))


fig3 <- fig3a/fig3b

# Saving figs
#jpeg
ggsave(filename = "./results/fig3ab.jpeg",
       width = 10,
       height = 18,
       dpi=700,
       units = "in",
       bg = "white")

#pdf
ggsave(filename = "./results/fig3ab.pdf",
       width = 10,
       height = 18,
       dpi=700,
       units = "in",
       bg = "white")


rm(fig3a, fig3b, fig3, io_alluv_df1, io_alluv_df2, io_alluv_df3, io_alluv_df4, 
   io_alluv_df5)


# Supporting Stats (Part 3) -----------------------------------------------

## Discrepancies in prod. to cons. ----------------------------------------

# TODO: N.B. Code efficiency improvement: At this moment this is a quite
# cumbersome way to put flows together, particularly for upper/lower bounds.
# This section is currently being improved, nonetheless values (and particularly
# their magnitude) are not expected to change.

# Pre-process strings for full io
start_time <- Sys.time()
io_mun_all <- io_CPF_CNPJ_GEOCMUN|> 
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>  
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  #actually worry if there is no warnings, because the next steps are based on the NA assigment
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>  
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy
end_time <- Sys.time()
end_time - start_time
rm(start_time, end_time)

# Check totals to see consequences of filtering small numbers. 
sum(io_CPF_CNPJ_GEOCMUN$RWE)  
sum(io_mun_all$RWE)

# N.B. io_CPF_CNPJ_GEOCMUN contains the long-form data for the output square
# matrix. Since the original matrix is sparce, several origin-destination values
# are zero (or very small values) the root of inefficiencies here. Filtering
# zeros and such small values supports processing for reproducing the work at
# greater speed, nonetheless for full reproduction one can comment out "
# #filter(RWE > 0)"


# VYC Baseline: 44.45
# 2,069,222.7481046

# VYC High efficiency: 53.9
# 1,781,557 

# VYC Low efficiency: 35
# 2,594,111.80


# N.B. In order to reproduce the upper/lower boundary figures, run the
# "input-output-model.R" changing the base conversion factor "base_vyc".


# Pre-process strings for the share of valid consumption
io_mun_valid <- ee_valid_cons|>  
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>  
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>  
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy


sum(io_mun_valid$RWE)
sum(ee_valid_cons$RWE)  

# Pre-process strings for the share of valid but overestimated consumption
io_mun_valid_overst <- ee_valid_overst_cons |>
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  # why isn't discrepancy zeroed?
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy

sum(io_mun_valid_overst$RWE)
sum(ee_valid_overst_cons$RWE)  

# Pre-process strings for the share of valid but invalid and missing 
io_mun_invalid_missing <- ee_invalid_missing_cons |>
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  # why isn't discrepancy zeroed?
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy

sum(io_mun_invalid_missing$RWE)
sum(ee_invalid_missing_cons$RWE)


# Pre-process strings for the share of autex
io_mun_autex <- ee_autex_cons |>
  filter(RWE > 0.01) |> 
  separate(ORIGIN, into = c("CPF_CNPJ_ORIGIN", "GEOCMUN_ORIGIN"), sep = "_" )|>
  separate(GEOCMUN_ORIGIN, into = c("GEOCMUN_ORIGIN", "INDEX"), sep = "D" )|>  # why isn't discrepancy zeroed?
  #warnings due to "discrepancy" data (not to worry, NAs are used next)
  mutate(INDEX = case_when(is.na(INDEX) ~ "0", TRUE ~ INDEX))|>  #Index 0 means no discrepancy for certain flow
  separate(DESTINATION, into = c("CPF_CNPJ_DESTINATION", "GEOCMUN_DESTINATION"), sep = "_" )|>
  mutate(INDEX = case_when(INDEX != "0" ~ "1", TRUE ~ INDEX)) #Index 1 means discrepancy

sum(io_mun_autex$RWE)
sum(ee_autex_cons$RWE)



# Total volume leaving forests across Pará between 2009-2019
sum(rw$VOLUME)
#N.B. ..."nearly 1.0 Mm3 of ipe roundwood"

# Total volume entering from other states 
sum(p_outer_state$RWE_OUTER_STATE)

# Total volumes
io_mun_all |> # io_mun_all had no removals from io_CPF_CPF_GEOCMUN
  group_by(INDEX) |> 
  summarise(sum(RWE)) |> 
  adorn_totals() |> as_tibble()

# VYC Baseline: 44.45
# # A tibble: 3 × 2
# INDEX `sum(RWE)`
# <chr>      <dbl>
# 1 0       1159249. State production 986218. + outer states flow 173031
# 2 1        909974. Missing flows
# 3 Total   2069223.


# VYC High efficiency: 53.9
# # A tibble: 3 × 2
# INDEX `sum(RWE)`
# <chr>      <dbl>
# 1 0       1129257. State production 986218. + outer states flow 143039
# 2 1        652300. Missing flows
# 3 Total   1781557.

# VYC Low efficiency: 35
# # A tibble: 3 × 2
# INDEX `sum(RWE)`
# <chr>      <dbl>
# 1 0       1205437. State production 986218. + outer states flow 219219
# 2 1       1388675. Missing flows
# 3 Total   2594112.

# N.B. ..."According to transport records (GF and DOF) analysed, we find nearly
# 1.0 Mm3 of ipê roundwood were reported leaving forests across Pará between
# 2009-2019, with an additional 0.17 Mm3 (roundwood equivalent, RWE) entering the
# supply-chain from other federal units."

#N.B. ..."2.1 Mm3 RWE (1.8-2.6 Mm3 RWE using a high (53.9%) and low (35%)
#overall sawmill efficiency in timber processing, respectively; see Methods)."


# Total percentages
io_mun_all |> # io_mun_all had no removals from io_CPF_CPF_GEOCMUN
  group_by(INDEX) |> 
  summarise(sum(RWE)) |> 
  adorn_totals() |> 
  adorn_percentages("col") |> 
  as_tibble()

# VYC Baseline: 44.45
# # A tibble: 3 × 2
# INDEX `sum(RWE)`
# <chr>      <dbl>
# 1 0          0.560
# 2 1          0.440
# 3 Total      1 

# VYC High efficiency: 53.9
# # A tibble: 3 × 2
# INDEX `sum(RWE)`
# <chr>      <dbl>
# 1 0          0.634
# 2 1          0.366
# 3 Total      1 

#35 
# # A tibble: 3 × 2
# INDEX `sum(RWE)`
# <chr>      <dbl>
# 1 0          0.465
# 2 1          0.535
# 3 Total      1 

#N.B. ..."This implies that, 44% (37-54%) of the total ipe volume entering
#supply chain in Para cannot be traced to a forest of origin (or to an inflow
#from other states)"


# Stats on discrepancy, 44.5 (Numbers may change slightly)
# Top mun discrepancy
top_share_mun <- share_mun|> 
  arrange(desc(DISCREPANCY)) |> 
  top_n(wt = DISCREPANCY, n = 3)|> 
  pull(name_muni)
#[1] "Belém"       "Breu Branco" "Itaituba" 

# Volume
sum((share_mun |> filter(name_muni %in% top_share_mun))$DISCREPANCY)
# 44.5
# 341372.68884173

# 35
# 529159.74607905

# 53.9
# 232674.39592022

# Percentages
sum((share_mun |> filter(name_muni %in% top_share_mun))$DISCREPANCY)/sum(share_mun$DISCREPANCY)
# 44.5
# 0.37592232432186

# 35
# 0.381051467468

# 53.9
# 0.35669516098193

# N.B. ..."Beĺem, Breu Branco, and Itaituba—all key port cities and commercial
# hubs—together account for 38% (38-36%; 0.34 Mm3, 0.53-0.23Mm3) of total
# discrepancies (0.91 Mm3, 1.39-0.65 Mm3).



# Metropolitan and adjacent cities

# Volume of missing flows
sum((share_mun |> filter(name_muni %in% c("Belém", "Barcarena", "Benevides", "Breu Branco", "Itaituba", 
                                          "Ananindeua", "Castanhal", "Santa Bárbara Do Pará", 
                                          "Tucuruí")))$DISCREPANCY)
#44.5
#440538.76051143

#35 
#693236.43419791

#53
#299893.5282643

# Percentage of missing flows 
sum((share_mun |> filter(name_muni %in% c("Belém", "Barcarena", "Benevides", "Breu Branco", "Itaituba", 
                                          "Ananindeua", "Castanhal", "Santa Bárbara Do Pará", 
                                          "Tucuruí")))$DISCREPANCY)/sum(share_mun$DISCREPANCY)

#44.5
#0.47168077951168

#35
#0.49920418646874

#53
#0.4597436255012

# N.B. ..." When accounting for metropolitan and adjacent cities, these geographical
# hotspots are even more prominent, making up nearly half of all discrepancies
# (48%, 50-46%; 0.44 Mm3, 0.690.30 Mm3) and a fifth of all ipe being transported
# in the Para supply-chain.

# Municipalities of interest
share_mun |> 
  filter(name_muni %in% c("Belém", "Barcarena", "Benevides", "Breu Branco", "Itaituba", 
                          "Ananindeua", "Castanhal", "Santa Bárbara Do Pará", 
                          "Tucuruí")) 

share_mun |> 
  filter(name_muni %in% c("Belém", 
                          "Barcarena", 
                          "Benevides", 
                          "Ananindeua", 
                          "Castanhal", 
                          "Santa Bárbara Do Pará"))

# Volume by mun of interest 
sum((share_mun |> filter(name_muni %in% c("Belém", 
                                          "Barcarena", 
                                          "Benevides", 
                                          "Ananindeua", 
                                          "Castanhal", 
                                          "Santa Bárbara Do Pará")))$DISCREPANCY)

#45.5
#211560.81241296

#35
#374001.61387596

#53
#125762.05227643

# Percentage mun of interest 
sum((share_mun |> filter(name_muni %in% c("Belém", 
                                          "Barcarena", 
                                          "Benevides", 
                                          "Ananindeua", 
                                          "Castanhal", 
                                          "Santa Bárbara Do Pará")))$DISCREPANCY)/sum(share_mun$DISCREPANCY)

#44.5
#0.23297245191918

#35
#0.26932106015022

#53
#0.19279609733054


# ..."The Belem metropolitan region (which includes origin municipalities of
# Belém, Ananindeua, Benevides, Castanhal, Santa ́Barbara do Para  ),
# together with neighboring port city of Barcarena—none of which are producing
# any ipe, but from where most timber exports leave the state—alone account for
# nearly a fourth (23%, 27-19%; 0.21 Mm3, 0.37-0.13 Mm3) of all discrepancies."


# Volume Breu Branco and Tucurui
sum((share_mun |> filter(name_muni %in% c("Breu Branco",
                                          "Tucuruí")))$DISCREPANCY)
#44.5
#[1] 139707.74615293

#35
#181469.05407861

#53
# 113225.47654138

# Percentage Breu Branco and Tucurui
sum((share_mun |> filter(name_muni %in% c("Breu Branco",
                                          "Tucuruí")))$DISCREPANCY)/sum(share_mun$DISCREPANCY)

#44.5
#[1] 0.15384728297326

#35
# 0.13067707789389

#53
#0.17357724051438

# N.B. ..."Branco and Tucuruı —south of Belém along the Tocantins river and
# adjacent municipalities in this key commercial hub—make up 15% (13-17%, 0.14
# Mm3, 0.18-0.11 Mm3) of discrepancies.


# Export share 
sum((io_mun_all|> filter(CPF_CNPJ_DESTINATION %in% c("EXPORT")))$RWE)
#44.5
#1411796.1443163

#35
#2591964.8237454 

#53
#1779646.0540712

sum((io_mun_all|> filter(CPF_CNPJ_DESTINATION %in% c("EXPORT")))$RWE)/sum(io_mun_all$RWE)
#44.5 
#0.68293980426056

#35
#0.69174868908828

#53
#0.65421479263048




# References --------------------------------------------------------------

options(citation.bibtex.max=999)

citation()
citation("ggplot2")
citation("scatterpie")
citation("ggalluvial")
citation("patchwork")
citation("tidyverse")
citation("ggpmisc")
citation("janitor")
citation("geobr")
citation("sf")
citation("tmap")
citation("scales")
citation("paletteer")
citation("scico")
citation("reshape2")



