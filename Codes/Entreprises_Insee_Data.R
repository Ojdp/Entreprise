# ======================== Données entreprises INSEE ===================== #
#
# Récupération des données de l'Insee sur les entreprises
# Obj calcul taux de marge et taux d'investissement
# 2 approches : (1) par branches, (2) par secteurs institutionnels
#
# 5 grands secteurs institutionnels : Sociétés non financières (SNF) (S.11), 
# Sociétés financières (SF) (S.12), APU (S.13), Ménages (S.14), Institutions sans 
# but lucratif au service des ménages (ISBLSM) (S.15)
# ======================================================================== #


# install.packages(c("stargazer","ggthemes", "eurostat","ofce","parallel","lubridate",
#                    "ggplot2","plotrix","Hmisc","readxl","httr","data.table","insee",
#                    "writexl","tidyverse","zoo","devtools","writexl","Rtools","inseeLocalData",
#                    "ggsci","systemfonts"))
# devtools::install_github("OFCE/ofce")
# devtools::install_github("oddworldng/INEbaseR")

library(tidyverse)  # inclut dplyr, ggplot2, readr, etc.
library(data.table)  # gestion rapide des données
library(lubridate)   # manipulation des dates
library(insee)       # données INSEE
library(readxl)      # lecture des fichiers Excel
library(writexl)     # export en Excel
library(scales)      # échelles ggplot2
library(ggthemes)    # thèmes ggplot2
library(zoo)                                                                                                                                                                                                                                                                                       
library(magrittr)
library(openxlsx)
library(systemfonts)
library(ofce)

# Date base pour la construction de l'index
DateIndex <- "2019-10-01"

# Liste des bases de l'Insee
bases <- get_dataset_list()



# 1. CNT - Comptes des branches (CB) -------------------------------------------
# Paramètres séries à garder : VA par branches trimestrielle et Excédent Brut d'Exploitation

## 1.1. VA par branches -----------------------------------------------------

# Opération B1 = VA par branches
# CNA produit = tout sauf "DSN-CNT" (services principalement non marchands) et "DS-CNT" (services)
# Valorisation : L = volume aux prix de l'année précédente chainés, V = valeur aux prix courants

dataVA <- get_insee_dataset("CNT-2020-CB")%>%
  filter(CORRECTION=="CVS-CJO")%>%
  filter(CNA_PRODUIT %in% c("A17-AZ", "A17-C1", "A17-C2", "A17-C3", "A17-C4", "A17-C5", 
                            "A17-DE", "A17-FZ", "A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", 
                            "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU", "D-CNT", 
                            "DI-CNT", "DIM-CNT", "DSM-CNT", "SMNA-CNT"))%>%
  filter(OPERATION %in% c("B1")) %>%
  filter(VALORISATION == "L")%>%
  split_title()%>%
  select(DATE,OBS_VALUE,TITLE_FR1,TITLE_FR2)%>%
  arrange(DATE, TITLE_FR2) %>% 
  rename(value = OBS_VALUE,
         name = TITLE_FR2)%>%
  group_by(name)%>%
  mutate(Index = value / value[DATE == DateIndex] * 100, # comparaison avec 2019 = 100
         Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100) # taux de croissance annuel


## 1.2. Excédent Brut d'Exploitation -----------------------------------------------------

# Opération B2 = Excédent Brut d'Exploitation
# CNA produit = tout sauf "DSN-CNT" (services principalement non marchands) et "DS-CNT" (services)

dataEBE_wide <- get_insee_dataset("CNT-2020-CB")%>%
  filter(CORRECTION=="CVS-CJO")%>%
  filter(CNA_PRODUIT %in% c("A17-AZ", "A17-C1", "A17-C2", "A17-C3", "A17-C4", "A17-C5", 
                            "A17-DE", "A17-FZ", "A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", 
                            "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU", "D-CNT", 
                            "DI-CNT", "DIM-CNT", "DSM-CNT", "SMNA-CNT"))%>%
  filter(OPERATION %in% c("B2")) %>%
  split_title()%>%
  select(DATE,OBS_VALUE,TITLE_FR2)%>%
  arrange(DATE,TITLE_FR2)%>%
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_")

dataEBE <- dataEBE_wide %>%
  pivot_longer(cols = -c(DATE)) %>%
  mutate(Index = value / value[DATE == DateIndex] * 100)%>%
  group_by(name)%>%
  mutate(Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100)


## 1.3. Marges -----------------------------------------------------

# Questions :
# à quoi ressemble le fichier excel ?
# pourquoi y a total = total branches - services non marchands - agri, ça représente quoi ?

# # Contribution à l'EBE par branche (partie à recoder)/ Marge
# 
# datamarge <- bind_rows(dataVA, dataEBE_wide) %>%
#   mutate(Total = `Total branches` - `Services non marchands` - `Agriculture`)%>%
#   pivot_longer(cols = -c(DATE, name)) %>%
#   group_by(DATE, name) %>%
#   arrange(DATE) 
# 
# marge_rows <- datamarge %>%
#   group_by(DATE, name) %>%
#   summarise(value = (value[OPERATION_label_fr == "B2 - Excédent d'exploitation"] / value[OPERATION_label_fr == "B1 - Valeur ajoutée"]) * 100, .groups = 'drop') %>%
#   mutate(OPERATION_label_fr = "Marge") %>%
#   mutate(Index = value / value[DATE == "2019-10-01"] * 100)
# 
# 
# file_path <- "C:/Users/153003/Documents/Entreprise/Marge.xlsx" 
# 
# Marge <- read_excel(file_path) %>%
#   pivot_longer(cols = -c(DATE)) %>%
#   mutate(DATE = as.Date(DATE) )





# 2. CNT - Opérations sur biens et services -------------------------------------------
# Paramètres séries à garder : FCBF par produit pour entreprises non financières trimestrielle

## 2.1. FCBF par branches -----------------------------------------------------

# Opération P51S = FCBF par branches
# CNA produit = tout sauf : "DB-CNT" (biens), "DI-CNT" (biens industriels), "DIM-CNT" 
# (biens manufacturés), "DS-CNT" (services), "DSM-CNT" (services marchands)

dataFBCF_wide <- get_insee_dataset("CNT-2020-OPERATIONS")%>%
  filter(CORRECTION=="CVS-CJO")%>%
  filter(CNA_PRODUIT %in% c("A17-AZ", "A17-C1", "A17-C2", "A17-C3", "A17-C4", "A17-C5",
                            "A17-DE", "A17-FZ", "A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ",
                            "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU", "D-CNT"))%>%
  filter(OPERATION %in% c("P51S")) %>%
  filter(VALORISATION %in% c("L","V"))%>%
  split_title() %>%
  # add_insee_metadata() %>% #ajoute des infos sur la série
  select(DATE, OBS_VALUE, TITLE_FR2, TITLE_FR3) %>%
  arrange(DATE, TITLE_FR2, TITLE_FR3) %>% 
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_") %>%
  mutate(
    Autres = `Services aux ménages` + `Produits agricoles` + `Autres produits industriels` + `Services immobiliers`,
    Total = `Services aux ménages` + `Produits agricoles` + `Autres produits industriels` + `Services immobiliers` + `Biens d'équipement` + `Construction` + `Matériels de transport` + `Information-communication` + `Services aux entreprises`
  )

dataFBCF <- dataFBCF_wide %>%
  pivot_longer(cols = -c(DATE, TITLE_FR3)) %>%
  group_by(TITLE_FR3, name) %>%
  arrange(DATE) %>%
  mutate(
    Index = value / value[DATE==DateIndex]*100,
    CroissanceAnnuelle = (value - lag(value, n = 4)) / lag(value, n = 4) * 100,
    CroissanceTrimestrielle = (value - lag(value, n = 1)) / lag(value, n = 1) * 100,
    CroissanceDateBase = (value - value[DATE== DateIndex]) /  value[DATE== DateIndex] * 100) %>%
  ungroup()%>%
  group_by(DATE,TITLE_FR3)%>%
  mutate(Part= (value/ value[name == "Total"]*100))%>%
  ungroup()%>%
  group_by(TITLE_FR3,name)%>%
  mutate(ContributionAnnuelle = CroissanceAnnuelle * lag(Part, n = 4) /100,
         ContributionTrimestrielle = CroissanceTrimestrielle * lag(Part, n = 1) /100,
         ContributionDateBase = CroissanceDateBase * (Part[DATE==DateIndex])/100) %>%
  rename(Produit=name)%>%
  ungroup()



# 3. CNT - Comptes de secteurs institutionnels (CSI) -------------------------------------------

# Regarder le taux d'investissement pour les SNF totales 

## 3.1. SNF : Taux d'investissement, marges, autofin -----------------------------------------------------

# SECT_INST "S11" = SNF
# Opération "B1" = VA, "P51" = FBCF, "B2" = EBE, "B9NF" = Capacité/besoin de financement,
# "B8G" = épargne

dataTotal <- get_insee_dataset("CNT-2020-CSI") %>%
  filter(CORRECTION == "CVS-CJO")%>%
  filter(OPERATION %in% c("B1", "P51", "B2", "B9NF", "B8G"))%>%
  filter(SECT_INST == "S11")%>%
  split_title() %>%
  select(DATE,OBS_VALUE,TITLE_FR1)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, names_sep = "_")%>%
  mutate(Tx_Invest = (`Formation brute de capital fixe des sociétés non financières`/ `Valeur ajoutée des sociétés non financières`)*100,
         Tx_Marge = (`Excédent brut d'exploitation des sociétés non financières`/ `Valeur ajoutée des sociétés non financières`)*100,
         Autofinancement = (`Épargne des sociétés non financières`/`Formation brute de capital fixe des sociétés non financières`)*100)%>%
  pivot_longer(-c(DATE))%>%
  group_by(name)%>%
  mutate(Index = value / value[DATE == DateIndex] * 100,
         Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100)




# 4. Ajout prévisions -------------------------------------------

dataPrev <- data.frame(
  DATE = as.Date(c("2024-10-01", "2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01")),
  Tx_Marge = c(30.6,30.8,31.1,31.3, 31.3),
  Tx_Invest = c(22.78,22.78, 22.78,22.8, 22.82))

dataTotal_wide <- dataTotal %>%
  filter(name %in% c("Tx_Invest", "Tx_Marge"))%>%
  pivot_wider(
    names_from = name,
    values_from = c(value, Index, Croissance),
    names_glue = "{name}.{.value}"
  ) %>% 
  rename(Tx_Invest = Tx_Invest.value,
         Tx_Marge = Tx_Marge.value)

dataPrev <- bind_rows(dataTotal_wide %>% select(DATE, Tx_Marge, Tx_Invest), 
                      dataPrev) %>%
  pivot_longer(cols = -c(DATE)) %>%
  mutate(DATE = as.Date(DATE))



# 5. Enquete de conjoncture INSEE -------------------------------------------

dataEnquete <- get_insee_dataset("CLIMAT-AFFAIRES")%>%
  split_title() %>%
  filter(INDICATEUR == "CLIMAT_AFFAIRES")%>%
  select(DATE,OBS_VALUE,TITLE_FR1)%>%
  arrange(DATE)