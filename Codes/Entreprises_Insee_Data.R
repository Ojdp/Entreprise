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

library(tidyverse) 
library(lubridate)   
library(insee)       
library(readxl)      
library(writexl)   
library(zoo)                                                                                                                                                                                                                                                                                       
library(systemfonts)
library(ofce)

# Dates base pour la construction de l'index
if (!exists("DateIndex")){DateIndex <- "2019-10-01"} 
if (!exists("AnneeRef")){AnneeRef <- "2018"} 
if (!exists("DateRef")){DateRef <- "2018-10-01"} 

# Liste des bases de l'Insee
# bases <- get_dataset_list()



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
  split_title() %>%
  select(DATE, value=OBS_VALUE, TITLE_FR1, name=TITLE_FR2)%>%
  arrange(DATE, name) %>% 
  group_by(name)%>%
  mutate(Index = value / value[DATE == DateIndex] * 100, # comparaison avec 2019 = 100
         Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100) # taux de croissance annuel


## 1.2. Excédent Brut d'Exploitation -----------------------------------------------------

# Opération B2 = Excédent Brut d'Exploitation
# CNA produit = tout sauf "DSN-CNT" (services principalement non marchands) et "DS-CNT" (services)

dataEBE <- get_insee_dataset("CNT-2020-CB")%>%
  filter(CORRECTION=="CVS-CJO")%>%
  filter(CNA_PRODUIT %in% c("A17-AZ", "A17-C1", "A17-C2", "A17-C3", "A17-C4", "A17-C5", 
                            "A17-DE", "A17-FZ", "A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", 
                            "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU", "D-CNT", 
                            "DI-CNT", "DIM-CNT", "DSM-CNT", "SMNA-CNT"))%>%
  filter(OPERATION %in% c("B2")) %>%
  split_title()%>%
  select(DATE, value=OBS_VALUE, name=TITLE_FR2)%>%
  arrange(DATE, name) %>% 
  mutate(Index = value / value[DATE == DateIndex] * 100)%>%
  group_by(name)%>%
  mutate(Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100)


## 1.3. Marges -----------------------------------------------------

# Taux de marge = EBE / VA brute en valeur

# On s'intéresse aux ENF donc le Total c'est "Marchand non agricole"
# Ou alors calcul du Total en enlevant aussi les services financiers, pour avoir les ENF
# --> que choisir ?

dataMarge <- get_insee_dataset("CNT-2020-CB") %>%
  filter(CORRECTION=="CVS-CJO") %>%
  filter(CNA_PRODUIT %in% c("A17-AZ", "A17-C1", "A17-C2", "A17-C3", "A17-C4", "A17-C5", 
                            "A17-DE", "A17-FZ", "A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", 
                            "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU", "D-CNT", 
                            "DI-CNT", "DIM-CNT", "DSM-CNT", "SMNA-CNT")) %>%
  filter(OPERATION %in% c("B1","B2")) %>%
  filter(VALORISATION != "L") %>%   #VA en valeur et pas volume
  split_title() %>%
  select(DATE, OBS_VALUE, operation=TITLE_FR1, name=TITLE_FR2) %>%
  arrange(DATE, name) %>% 
  pivot_wider(names_from = name, values_from = OBS_VALUE, names_sep = "_") %>%
  mutate(`Marchand non agricole non financier` = `Marchand non agricole` - `Services financiers`) %>%
  pivot_longer(cols=-c(DATE,operation)) %>% 
  pivot_wider(names_from = operation, values_from = value, names_sep = "_")

# Calcul du taux de marge, croissance et contributions des branches
dataMarge <- dataMarge %>%  
  arrange(DATE, name) %>% 
  group_by(DATE) %>% 
  mutate(
    TxMarge = `Excédent brut d'exploitation`/`Valeur ajoutée des branches` *100,
    partVA = (`Valeur ajoutée des branches`/`Valeur ajoutée des branches`[name == "Marchand non agricole"] *100),
    # partVA_NF = (`Valeur ajoutée des branches`/`Valeur ajoutée des branches`[name == "Marchand non agricole non financier"] *100)
  ) %>% 
  group_by(name) %>% 
  mutate(
    Index = TxMarge / TxMarge[DATE==DateIndex] *100
  ) %>% 
  mutate(
    CroissanceAnnuelle = (TxMarge - lag(TxMarge, n = 4)) / lag(TxMarge, n = 4) * 100,
    CroissanceTrim = (TxMarge - lag(TxMarge, n = 1)) / lag(TxMarge, n = 1) * 100,
    CroissanceDateBase = (TxMarge - TxMarge[DATE== DateRef]) /  TxMarge[DATE== DateRef] * 100,
    CroissanceAnneeMoyBase = (TxMarge - mean(TxMarge[format(DATE, "%Y") == AnneeRef], na.rm = TRUE)) / mean(TxMarge[format(DATE, "%Y") == AnneeRef], na.rm = TRUE) * 100
  ) %>% 
  mutate(
    VarTxMargeAnnuelle = TxMarge - lag(TxMarge, n = 4),
    VarTxMargeTrim = TxMarge - lag(TxMarge, n = 1),
    VarTxMargeDateBase = TxMarge - TxMarge[DATE== DateRef],
    VarTxMargeAnneeMoyBase = TxMarge - mean(TxMarge[format(DATE, "%Y") == AnneeRef], na.rm = TRUE)
  ) %>%
  mutate(
    ContributionAnnuelle = VarTxMargeAnnuelle * lag(partVA, n = 4) /100,
    ContributionTrim = VarTxMargeTrim * lag(partVA, n = 1) /100,
    ContributionDateBase = VarTxMargeDateBase * (partVA[DATE==DateRef])/100,
    ContributionAnneeMoyBase = VarTxMargeAnneeMoyBase * mean(partVA[format(DATE, "%Y") == AnneeRef], na.rm = TRUE) /100
  )



# Tests pour comprendre pourquoi contrib Industrie != contrib Energie + contrib Manuf
# Test pour comprendre décomposition contributions
contribMarge <- dataMarge %>% 
  select(DATE, name, ContributionAnneeMoyBase) %>% 
  pivot_wider(names_from = name, values_from = ContributionAnneeMoyBase) %>% 
  mutate(
    `Total` = `Autres branches industrielles` + `Commerce` + `Construction` + `Services aux entreprises` +
      `Hébergement-restauration` + `Industries agro-alimentaires` + `Énergie, eau, déchets` + `Services immobiliers` + 
      `Information-communication` + `Services financiers` + `Transport` + `Biens d'équipement` + `Matériels de transport` + 
      `Services aux ménages`,
    `Total_ag` = `Construction` + `Énergie, eau, déchets` + `Biens manufacturés` + `Services principalement marchands`,
    `Total_agag` = `Construction` + `Industrie` + `Services principalement marchands`
  ) %>% 
  select(DATE, `Marchand non agricole`, `Total`, `Total_ag`, `Total_agag`, everything())
# Verif part VA
test <- dataMarge %>%
  filter(name %in% c("Industrie", "Énergie, eau, déchets", "Biens manufacturés")) %>%
  group_by(DATE) %>%
  summarise(
    partVA_Industrie = sum(partVA[name == "Industrie"]),
    partVA_Somme = sum(partVA[name %in% c("Énergie, eau, déchets", "Biens manufacturés")]),
    Difference = partVA_Industrie - partVA_Somme
  ) # OK
# Verif egalité taux marge
test <- dataMarge %>%
  filter(name %in% c("Industrie", "Énergie, eau, déchets", "Biens manufacturés")) %>%
  group_by(DATE) %>%
  summarise(
    TxMarge_Industrie = mean(TxMarge[name == "Industrie"], na.rm = TRUE),
    TxMarge_Calculée = sum(partVA[name == "Énergie, eau, déchets"] * TxMarge[name == "Énergie, eau, déchets"] + 
                             partVA[name == "Biens manufacturés"] * TxMarge[name == "Biens manufacturés"]) /
      sum(partVA[name %in% c("Énergie, eau, déchets", "Biens manufacturés")]),
    Difference = TxMarge_Industrie - TxMarge_Calculée
  ) # OK
# Verif egalité variation taux marge
test <- dataMarge %>%
  filter(name %in% c("Industrie", "Énergie, eau, déchets", "Biens manufacturés")) %>%
  group_by(DATE) %>%
  summarise(
    VarTxMarge_Industrie = sum(VarTxMargeAnneeMoyBase[name == "Industrie"]),
    VarTxMarge_Calculée = sum((partVA[name == "Énergie, eau, déchets"] * VarTxMargeAnneeMoyBase[name == "Énergie, eau, déchets"] +
                                 partVA[name == "Biens manufacturés"] * VarTxMargeAnneeMoyBase[name == "Biens manufacturés"])) /
      sum(partVA[name %in% c("Énergie, eau, déchets", "Biens manufacturés")]),
    Difference = VarTxMarge_Industrie - VarTxMarge_Calculée
  ) # Non







# 2. CNT - Opérations sur biens et services -------------------------------------------
# Paramètres séries à garder : FCBF par produit pour entreprises non financières trimestrielle

## 2.1. FCBF par branches -----------------------------------------------------

# Opération P51S = FCBF par branches
# CNA produit = tout sauf : "DB-CNT" (biens), "DI-CNT" (biens industriels), "DIM-CNT" 
# (biens manufacturés), "DS-CNT" (services), "DSM-CNT" (services marchands)

dataFBCF <- get_insee_dataset("CNT-2020-OPERATIONS")%>%
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
  ) %>%
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

# Regarder les taux d'investissement et de marge pour les SNF totales 

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
  DATE = as.Date(c("2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01", "2026-01-01", "2026-04-01", "2026-07-01", "2026-10-01")),
  Tx_Marge = c(32.1, 32.1, 32.1, 32.1, 32.1, 32.1, 32.1, 32.1),
  Tx_Invest = c(22.1, 22.1, 22.1, 22.1, 22.1, 22.1, 22.1, 22.1))

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