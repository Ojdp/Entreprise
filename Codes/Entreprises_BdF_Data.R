# ======================== Données entreprises BdF ===================== #
# Données sur l'endettement des entreprises et les faillites
# ======================================================================== #

#API Banque de France
#clé : 7MyyvJc-4YAD-cr
#valeur confidentielle : a2ff8f02132cd59c97bc2d082dfa5806

library(tidyverse)
library(rsdmx)
library(ofce)
library(xml2)
library(gridExtra)
library(rwebstat)
library(svglite)
library(ggsci)
library(openxlsx)
library(jsonlite)
library(httr)

# Dates base pour la construction de l'index
if (!exists("DateIndex")){DateIndex <- "2019-10-01"} 
if (!exists("AnneeIndex")){AnneeIndex <- "2019"} 


#MIR1: indentifiant du jeu de données, nom du dataset et ce qui suit c la clé 
#Crédit = Mensuelle, France, Etablissements de crédit et autres institutions financières, crédit, Toutes maturités, flux mensuels cumulés sur un an , Tous montants, SNF résidentes, euro, contrats nouveaux
#Endettement = Mensuel, France, Brut, Endettement, Total, Indices notionnels des stocks, Résidents, Sociétés non financières (S11), Toutes monnaies confondues, Taux de croissance annuel


# 1. Récupération des données -------------------------------------------

fetch_and_process_data <- function(url, headers = headers) {
  
  headers <- add_headers(
    Authorization = "Apikey 82ce51865859143598789064e4b7bc60e0ba0307b5ee69ad79c7ba44",  
    accept = "application/json"
  )
  
  # Make the HTTP request with SSL verification disabled
  response <- GET(url, headers, httr::config(ssl_verifypeer = FALSE))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Extract JSON content
    content_json <- content(response, "text", encoding = "UTF-8")
    
    # Convert JSON to dataframe, select required columns, and sort by time_period in descending order
    data <- fromJSON(content_json, flatten = TRUE) %>%
      select(c(title_fr, time_period, obs_value)) %>%
      mutate(title_fr = sub(",.*", "", title_fr)) %>%
      
      # Convert time_period to Date (assuming monthly data)
      mutate(time_period = as.Date(paste0(time_period, "-01"), format = "%Y-%m-%d")) %>%
      
      # Sort time_period in descending order
      arrange(time_period)
    
    # Rename the columns based on the 'title_fr' value
    new_column_name <- unique(data$title_fr)
    data <- data %>%
      rename(!!new_column_name := obs_value) %>%
      select(-title_fr) %>%
      rename(date = time_period) # Drop 'title_fr' column after renaming
    
    
    return(data)
  } else {
    # Handle errors
    cat("Request failed with status:", status_code(response), "\n")
    return(NULL)
  }
}

generate_url <- function(dataset_id, series_key = NULL) {
  base_url <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json/"
  
  # Construct the query parameters
  if (!is.null(series_key)) {
    where_clause <- paste0("series_key+IN+%28%22", series_key, "%22%29")
  } else {
    stop("Either series_key or series_name must be provided.")
  }
  
  url <- paste0(base_url, "?where=", where_clause, "&order_by=-time_period_start")
  return(url)
}


## Crédit bancaire - Encours --------------

url_EncoursTotal <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A26.A.1.U6.2240.Z01.E")
url_EncoursTrésorie <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N2Z.A.1.U6.2240.Z01.E")
url_EncoursInvestissement <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N1Z.A.1.U6.2240.Z01.E")
url_EncoursInvestissementImmo <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N1ZIM.A.1.U6.2240.Z01.E")
url_EncoursInvestissementEqui <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N1ZQ.A.1.U6.2240.Z01.E")

EncoursTotal <- fetch_and_process_data(url_EncoursTotal)
EncoursTrésorie <- fetch_and_process_data(url_EncoursTrésorie)
EncoursInvestissement <- fetch_and_process_data(url_EncoursInvestissement)
EncoursInvestissementImmo <- fetch_and_process_data(url_EncoursInvestissementImmo)
EncoursInvestissementEqui <- fetch_and_process_data(url_EncoursInvestissementEqui)


## Crédit bancaire - Flux brut sur 12 mois -----------------

url_CreditTotal <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.Y.A.2240U6.EUR.N")
url_CreditPlus1M <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.Y.1.2240U6.EUR.N")
url_CreditMoins1M <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.Y.0.2240U6.EUR.N")

CreditTotal <- fetch_and_process_data(url_CreditTotal)
CreditPlus1M <- fetch_and_process_data(url_CreditPlus1M)
CreditMoins1M <- fetch_and_process_data(url_CreditMoins1M)


## Crédits accordés aux sociétés non financières résidentes, flux mensuels, CVS ------

url_VariationCreditTotal <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.Y.R.A26.A.4.U6.2240.Z01.E")
url_VariationCreditTreso <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.Y.R.A2N2Z.A.4.U6.2240.Z01.E")
url_VariationCreditInvImmo <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.Y.R.A2N1ZIM.A.4.U6.2240.Z01.E")
url_VariationCreditInvEqui <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.Y.R.A2N1ZQ.A.4.U6.2240.Z01.E")
url_VariationCreditAutre <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.Y.R.A2N3Z.A.4.U6.2240.Z01.E")

VariationCreditTotal <- fetch_and_process_data(url_VariationCreditTotal)
VariationCreditTreso <- fetch_and_process_data(url_VariationCreditTreso)
VariationCreditInvImmo <- fetch_and_process_data(url_VariationCreditInvImmo)
VariationCreditInvEqui <- fetch_and_process_data(url_VariationCreditInvEqui)
VariationCreditAutre <- fetch_and_process_data(url_VariationCreditAutre)


## Encours de titres de dette émis par les sociétés non financières URLs ---------

url_Titres <- generate_url(dataset_id = "SC1", series_key = "SC1.M.FR.1100.F33000.N.1.Z01.E.Z")
url_TitresLT <- generate_url(dataset_id = "SC1", series_key = "SC1.M.FR.1100.F33200.N.1.EUR.E.Z")
url_TitresCT <- generate_url(dataset_id = "SC1", series_key = "SC1.M.FR.1100.F33100.N.1.EUR.E.Z")

Titres <- fetch_and_process_data(url_Titres)
TitresLT <- fetch_and_process_data(url_TitresLT)
TitresCT <- fetch_and_process_data(url_TitresCT)


## Taux --------------

# Coût de financement des SNF, taux en %
url_CoutFinancement <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.AT0.A.R.A.2240U6.EUR.N")
# Taux crédit bancaire
url_TauxCredit <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.R.A.2240U6.EUR.N")
# Taux d'intérêt annuel, titres de dettes, produits financiers dérivés exclus, Sociétés non financières, Euro
url_TauxTitreDette <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A30.K.R.1.2240.EUR.N")

CoutFinancement <- fetch_and_process_data(url_CoutFinancement)
TauxCredit <- fetch_and_process_data(url_TauxCredit)
TauxTitreDette <- fetch_and_process_data(url_TauxTitreDette)


## Taux de croissance brut (sur un an) ------------------
url_TxCroissanceCredit <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A26.A.I.U6.2240.Z01.A")
TxCroissanceCredit <- fetch_and_process_data(url_TxCroissanceCredit)




# Suppression des variables url inutiles mtn
rm(list = ls(pattern = "^url"))




# 2. Encours de crédit par objet : investissement, équipement, immobilier, trésorerie -------------------------------------------

dfEncours <- list(EncoursTotal, EncoursTrésorie, EncoursInvestissement, EncoursInvestissementImmo, EncoursInvestissementEqui)
EncoursCredit <- reduce(dfEncours, inner_join, by = "date") %>% 
  rename(
    Total = `Crédits accordés aux sociétés non financières résidentes`,
    Tresorerie = `Crédits de trésorerie accordés aux sociétés non financières résidentes`,
    Investissement = `Crédits à l'investissement accordés aux sociétés non financières résidentes`,
    Immobilier = `Crédits à l'investissement - immobilier accordés aux sociétés non financières résidentes`,
    Equipement = `Crédits à l'investissement - équipement accordés aux sociétés non financières résidentes`
  ) %>%
  filter(date >= "1994-01-01") %>%
  mutate(across(c(Total, Immobilier, Equipement, Tresorerie), as.numeric),
         Autre = Total - Immobilier - Equipement - Tresorerie) %>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur") %>%
  group_by(date) %>%
  mutate(Part = (Valeur / Valeur[Variable=="Total"])*100) %>%
  ungroup() %>%
  group_by(Variable) %>%
  mutate(
    Index = Valeur / mean(Valeur[format(date, "%Y") == AnneeIndex]) *100,
    CroissanceAnnuelle = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100,
    CroissanceMensuelle = (Valeur - lag(Valeur, n = 1)) / lag(Valeur, n = 1) * 100,
    ContribAnnuelle = CroissanceAnnuelle * lag(Part, n = 12) /100,
    ContribMensuelle = CroissanceMensuelle * lag(Part, n = 1) /100
  )
  
# Endettement lors des crises
EncoursCredit <- EncoursCredit %>% 
  group_by(Variable) %>%
  mutate(GFC = ((Valeur[date == "2009-12-01"] - Valeur[date == "2008-09-01"])/ Valeur[date == "2008-09-01"])*100,
         Covid = ((Valeur[date == "2021-03-01"] - Valeur[date == "2020-02-01"])/ Valeur[date == "2020-02-01"])*100,
         PostCovid = ((Valeur[date == "2023-12-01"] - Valeur[date == "2021-04-01"])/ Valeur[date == "2021-04-01"])*100,
         Normal = ((Valeur[date == "2025-01-01"] - Valeur[date == "2024-01-01"])/ Valeur[date == "2024-01-01"])*100,
         ContributionGFC = GFC * Part[date == "2008-09-01"]/100,
         ContributionCovid = Covid * Part[date == "2020-02-01"]/100,
         ContributionPC = PostCovid * Part[date == "2021-04-01"]/100,
         ContributionNormal = Normal * Part[date == "2024-01-01"]/100)     



# 3. Variations mensuelles d'encours de crédits par objet (données cvs-cjo)	 -------------

dfVariationEncours <- list(VariationCreditTotal, VariationCreditTreso, VariationCreditAutre, 
                           VariationCreditInvImmo, VariationCreditInvEqui)
VariationEncours <- reduce(dfVariationEncours, inner_join, by = "date") %>% 
  rename(
    Total = `Crédits accordés aux sociétés non financières résidentes`,
    Tresorerie = `Crédits de trésorerie accordés aux sociétés non financières résidentes`,
    Autre = `Autres crédits accordés aux sociétés non financières résidentes`,
    Immobilier = `Flux CVS & CJO/ crédits aux sociétés non financières ; investissement - immobilier`,
    Equipement = `Crédits à l'investissement - équipement accordés aux sociétés non financières résidentes`
  ) %>% 
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")



# 3. Titres de dette à CT ou LT -----------------------------------------------------

EndettementSource <- EncoursTotal %>% 
  inner_join(TitresCT, by = "date") %>% 
  inner_join(TitresLT, by = "date") %>%
  rename(Credit = `Crédits accordés aux sociétés non financières résidentes`,
         TitresCT = `Encours de titres de dette à court terme en euros`,
         TitresLT = `Encours de titres de dette à long terme`)%>%
  filter(date >= "1999-01-01") %>%
  mutate(Total = TitresCT + TitresLT + Credit) %>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur") %>%
  group_by(date) %>%
  mutate(Part = (Valeur / Valeur[Variable=="Total"])*100) %>%
  ungroup() %>%
  group_by(Variable) %>%
  mutate(
    CroissanceAnnuelle = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100,
    ContribAnnuelle = CroissanceAnnuelle * lag(Part, n = 12) /100,
    Covid = ((Valeur[date == "2021-03-01"] - Valeur[date == "2020-02-01"])/ Valeur[date == "2020-02-01"])*100,
    PostCovid = ((Valeur[date == "2024-01-01"] - Valeur[date == "2021-04-01"])/ Valeur[date == "2021-04-01"])*100,
    ContributionCovid = Covid * Part[date == "2020-02-01"]/100,
    ContributionPC = PostCovid * Part[date == "2021-04-01"]/100
  )



# 4. Global : Financement crédits vs titres	 -------------

dfFinancement <- list(Titres, TxCroissanceCredit, CoutFinancement, TauxCredit, TauxTitreDette)
FinancementCreditMarche <- reduce(dfFinancement, inner_join, by = "date") %>% 
  rename(
    EncoursTitreDette = "Encours de titres de dette émis par les sociétés non financières",
    CroissanceCredit = "Crédits accordés aux sociétés non financières résidentes",
    CoutFinancement = "Coût de financement des SNF",
    CoutCredit = "Crédits nouveaux aux SNF",
    CoutTitreDette = "Taux d'intérêt annuel"
    ) %>% 
  mutate(
    CroissanceTitres = (EncoursTitreDette - lag(EncoursTitreDette, n = 12)) / lag(EncoursTitreDette, n = 12) * 100,
    SpreadTaux = CoutCredit - CoutTitreDette,
    SpreadCroissance = CroissanceCredit - CroissanceTitres
    )
  







# 5. Comparaison encours et VA -----------------------------------------------------

# # Données INSEE pour VA par secteur
# library(insee)
# 
# source<-"CNT-2014-CB" # à réactualiser
# bases<-get_dataset_list()
# idbank_list <- get_idbank_list(source)
# corr<-"CVS-CJO"
# flux<-c("A17-FZ","A17-AZ","D-CNT","A17-GZ", "A17-HZ", "A17-JZ", "A17-LZ", "A17-MN")
# variable<-"B1" # VA par branches
# naf<-"Volumes aux prix de l'année précédente chaînés"
# 
# ListeID <-idbank_list%>%
#   filter(CORRECTION==corr,
#          CNA_PRODUIT %in% flux,
#          OPERATION %in% variable,
#          VALORISATION_label_fr==naf)%>%
#   select(idbank)%>%
#   pull(idbank)
# 
# VABranches <- get_insee_idbank(ListeID) %>%
#   split_title()%>%
#   add_insee_metadata()%>%
#   select(DATE,OBS_VALUE,TITLE_FR2,OPERATION_label_fr)%>%
#   arrange(DATE)%>%
#   pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_")%>%
#   rename(`Info` = `Information-communication`,
#          Total = `Total branches`,
#          Conseil = `Services aux entreprises`,
#          Immo = `Services immobiliers`,
#          date = DATE)%>%
#   select(!"OPERATION_label_fr")%>%
#   pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
#   rename(VA = Valeur)
# 
# VATotal <- VABranches%>% 
#   filter(Variable == "Total" & date >= "1993-04-01")%>%
#   inner_join(EncoursCredit, by ="date")%>%
#   mutate(Rapport = Valeur /VA)




# 4. Enquête d'accès au crédit ----------------------------

# ça ne marche pas (séries pas réactualisées)
# PME<- w_data(dataset_name = "CONJ", series_name = "CONJ.Q.N01.N.TS.000TA.DMTIA001.PM")
# EI<- w_data(dataset_name = "CONJ", series_name = "CONJ.Q.N01.N.TS.000TA.DMTIA001.EI")
# 
# Conjoncture <- inner_join (PME, EI, by = "date") %>%
#   rename( PME = "CONJ.Q.N01.N.TS.000TA.DMTIA001.PM",
#           EI = "CONJ.Q.N01.N.TS.000TA.DMTIA001.EI")


# 5. Flux de Crédit ----------------------------

# ça ne marche pas (séries pas réactualisées)
# FluxTotal <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.Y.R.A26.A.4.U6.2240.Z01.E")
# FluxTreso <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.Y.R.A2N2Z.A.4.U6.2240.Z01.E")
# FluxInv<- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.Y.R.A2N1Z.A.4.U6.2240.Z01.E")
# 
# Flux <- inner_join(inner_join(FluxTotal, FluxTreso, by = "date"), FluxInv, by = "date")%>%
#   rename(
#     Total = "BSI1.M.FR.Y.R.A26.A.4.U6.2240.Z01.E",
#     Treso = "BSI1.M.FR.Y.R.A2N2Z.A.4.U6.2240.Z01.E",
#     Inv = "BSI1.M.FR.Y.R.A2N1Z.A.4.U6.2240.Z01.E"
#   )

# 6. Encours par Secteur ----------------------------

# Agriculture <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.AZ.TT")
# InduManuf <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.C.TT")
# Indu <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.BE.TT")
# Construction <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.FZ.TT")
# Commerce <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.G.TT")
# Transport <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.H.TT")
# Info <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.JZ.TT")
# Immo <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.LZ.TT")
# Conseil <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.MN.TT")
# Enseignement <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.PS.TT") 
# 
# Secteur1 <- inner_join(inner_join(Agriculture, InduManuf, by ="date"), Indu, by = "date")%>%
#   rename( Agriculture = "DIREN.M.FR.CR.LME.ME.01.N.AZ.TT",
#           InduManuf = "DIREN.M.FR.CR.LME.ME.01.N.C.TT",
#           Indu = "DIREN.M.FR.CR.LME.ME.01.N.BE.TT")
# 
# Secteur2 <- inner_join(inner_join(Construction, Commerce, by ="date"), Transport, by = "date")%>%
#   rename( Construction = "DIREN.M.FR.CR.LME.ME.01.N.FZ.TT",
#           Commerce = "DIREN.M.FR.CR.LME.ME.01.N.G.TT",
#           Transport = "DIREN.M.FR.CR.LME.ME.01.N.H.TT")
# 
# Secteur3 <- inner_join(inner_join(Info, Immo, by ="date"), Conseil, by = "date")%>%
#   rename( Info = "DIREN.M.FR.CR.LME.ME.01.N.JZ.TT",
#           Immo = "DIREN.M.FR.CR.LME.ME.01.N.LZ.TT",
#           Conseil = "DIREN.M.FR.CR.LME.ME.01.N.MN.TT")
# 
# Secteur <- inner_join(inner_join(Secteur1, Secteur2, by ="date"), Secteur3, by = "date")%>%
#   select(date,Agriculture, InduManuf, Indu, Construction, Commerce, Transport, Info, Immo, Conseil)%>%
#   filter( date >= "2012-03-01")%>%
#   mutate(Total = Agriculture + Indu + Construction+ Commerce+Transport+Info+Immo+Conseil,
#          TotalSansImmo = Agriculture + Indu + Construction+ Commerce+Transport+Info+Conseil)%>%
#   pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
#   group_by(date)%>%
#   mutate(Part= Valeur/Valeur[Variable=="Total"]*100)%>%
#   group_by(Variable)%>%
#   mutate(Croissance = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100,
#          Contribution = Croissance * lag(Part, n = 12) /100)
# 
# 
# #On prend la donnée du dernier mois comme celle du trimestre et on change la date pour avoir la même que VA
# 
# SecteurTrim <-  inner_join(inner_join(Secteur1, Secteur2, by ="date"), Secteur3, by = "date")%>%
#   select(date,Agriculture, InduManuf, Indu, Construction, Commerce, Transport, Info, Immo, Conseil)%>%
#   filter( date >= "2012-03-01")%>%
#   mutate(Total = Agriculture + Indu + Construction+ Commerce+Transport+Info+Immo+Conseil)%>%
#   filter(month(date) %in% c(3, 6, 9, 12))%>%
#   mutate(date = if_else(month(date) == 3, as.Date(paste0(year(date), "-01-01")), date),
#          date = if_else(month(date) == 6, as.Date(paste0(year(date), "-04-01")), date),
#          date = if_else(month(date) == 9, as.Date(paste0(year(date), "-07-01")), date),
#          date = if_else(month(date) == 12, as.Date(paste0(year(date), "-10-01")), date))%>%
#   pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
#   rename(Encours = Valeur)
# 
# SecteurTrim <- inner_join(SecteurTrim, VABranches %>% filter(date >= "2012-01-01"), by = c("date", "Variable"))%>%
#   mutate( Rapport = Encours /VA,
#           Index = Rapport /  Rapport [date == "2019-10-01"] * 100)
# 


# 7. Analyse de la correlation Credit/taux d'intéret ----------------------------

# 1. Corrélation entre 2004 et 2020 

# DataJusqu2020 <- subset(DataCredit, date <= as.Date("2019-12-01"))
# correlation_jusqu2020 <- cor(DataJusqu2020$VariationCreditTotal, DataJusqu2020$TauxCreditTotal, use="complete.obs")
# cat("Corrélation jusqu'à fin 2019 : ", correlation_jusqu2020, "\n")
# 
# # 2. Corrélation entre Mars 2020 et Décembre 2022
# DataMars2020Dec2022 <- subset(DataCredit, date >= as.Date("2020-03-01") & date <= as.Date("2022-11-01"))
# correlation_mars2020_dec2022 <- cor(DataMars2020Dec2022$VariationCreditTotal, DataMars2020Dec2022$TauxCreditTotal, use = "complete.obs")
# cat("Corrélation entre Mars 2020 et Décembre 2022 : ", correlation_mars2020_dec2022, "\n")
# 
# # 3. Corrélation entre Janvier 2023 et Août 2024
# DataJan2023Aug2024 <- subset(DataCredit, date >= as.Date("2023-01-01") & date <= as.Date("2024-08-01"))
# correlation_jan2023_aout2024 <- cor(DataJan2023Aug2024$VariationCreditTotal, DataJan2023Aug2024$TauxCreditTotal, use = "complete.obs")
# cat("Corrélation entre Janvier 2023 et Août 2024 : ", correlation_jan2023_aout2024, "\n")
# 
# rm(DataJusqu2020, correlation_jusqu2020, DataMars2020Dec2022, correlation_mars2020_dec2022, DataJan2023Aug2024, correlation_jan2023_aout2024)




#Au cas où l'API Bdf ne fonctionne pas
# EncoursCredit <- read_excel("Webstat_Export_20231004.xlsx")%>%
#   arrange(DATE)%>%
#  filter(!(DATE>= "1990-01-01" & DATE <= "1994-03-01"))%>%
#   mutate(across(c(Total, Investissement, Trésorie), as.numeric),
#                 Autres = Total - Investissement - Trésorie,
#          DATE = as.Date(DATE)) %>%
# pivot_longer(cols = -DATE, names_to = "Variable", values_to = "Valeur")%>%
# # mutate(Part = (Valeur/ Valeur[Variable == "Total"] *100 )) %>%
# filter(!(Variable %in% c("Total"))) 