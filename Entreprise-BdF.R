#API Banque de France
#clé : 7MyyvJc-4YAD-cr
#valeur confidentielle : a2ff8f02132cd59c97bc2d082dfa5806

library(tidyverse)
library(rsdmx)
library(ggplot2)
library(ofce)
library(magrittr)
library(xml2)
library(gridExtra)
library(rwebstat)
library(svglite)
library(ggsci)
library(openxlsx)
library(jsonlite)
library(httr)

#MIR1: indentifiant du jeu de données, nom du dataset et ce qui suit c la clé 
#Crédit = Mensuelle, France, Etablissements de crédit et autres institutions financières, crédit, Toutes maturités, flux mensuels cumulés sur un an , Tous montants, SNF résidentes, euro, contrats nouveaux
#Endettement = Mensuel, France, Brut, Endettement, Total, Indices notionnels des stocks, Résidents, Sociétés non financières (S11), Toutes monnaies confondues, Taux de croissance annuel


fetch_and_process_data <- function(url, headers = headers) {
  
  headers <- add_headers(
    Authorization = "Apikey 82ce51865859143598789064e4b7bc60e0ba0307b5ee69ad79c7ba44",  
    accept = "application/json"
  )
  
  # Make the HTTP request with SSL verification disabled
  response <- GET(url, headers, config(ssl_verifypeer = FALSE))
  
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

# Generate URLs

# Credit data URLs
url_CreditTotal <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.Y.A.2240U6.EUR.N")
url_CreditPlus1M <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.Y.1.2240U6.EUR.N")
url_CreditMoins1M <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.Y.0.2240U6.EUR.N")
url_CreditsTresorie <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.Y.R.A2N2ZZ.A.4.U6.2240.Z01.E")
url_CreditsFBCF <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.Y.R.A2N1ZZ.A.4.U6.2240.Z01.E")
url_Taux <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.R.A.2240U6.EUR.N")




# Encours Total URLs
url_Endettement <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.DEB.A.I.U6.2240.Z01.A")
url_EncoursTotal <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A26.A.1.U6.2240.Z01.E")
url_EncoursTrésorie <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N2Z.A.1.U6.2240.Z01.E")
url_EncoursInv <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N1Z.A.1.U6.2240.Z01.E")
url_EncoursInvestissementImmo <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N1ZIM.A.1.U6.2240.Z01.E")
url_EncoursInvestissementEqui <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N1ZQ.A.1.U6.2240.Z01.E")

# Encours de titres de dette émis par les sociétés non financières URLs
url_Titres <- generate_url(dataset_id = "SC1", series_key = "SC1.M.FR.1100.F33000.N.1.Z01.E.Z")
url_TitresLT <- generate_url(dataset_id = "SC1", series_key = "SC1.M.FR.1100.F33200.N.1.EUR.E.Z")
url_TitresCT <- generate_url(dataset_id = "SC1", series_key = "SC1.M.FR.1100.F33100.N.1.EUR.E.Z")


# Using the generated URLs
CreditTotal <- fetch_and_process_data(url_CreditTotal)
CreditPlus1M <- fetch_and_process_data(url_CreditPlus1M)
CreditMoins1M <- fetch_and_process_data(url_CreditMoins1M)
CreditsTresorie <- fetch_and_process_data(url_CreditsTresorie)
CreditsFBCF <- fetch_and_process_data(url_CreditsFBCF)
Taux <- fetch_and_process_data(url_Taux)
Endettement <- fetch_and_process_data(url_Endettement)
EncoursTotal <- fetch_and_process_data(url_EncoursTotal)
EncoursTrésorie <- fetch_and_process_data(url_EncoursTrésorie)
EncoursInv <- fetch_and_process_data(url_EncoursInv)
EncoursInvestissementImmo <- fetch_and_process_data(url_EncoursInvestissementImmo)
EncoursInvestissementEqui <- fetch_and_process_data(url_EncoursInvestissementEqui)
Titres <- fetch_and_process_data(url_Titres)
TitresLT <- fetch_and_process_data(url_TitresLT)
TitresCT <- fetch_and_process_data(url_TitresCT)

EncoursInvestissement <- inner_join(inner_join(EncoursInvestissementImmo, EncoursInvestissementEqui, by ="date"), EncoursInv, by = "date")%>%
  rename( EncoursInvestissement = `Crédits à l'investissement accordés aux sociétés non financières résidentes`,
          Immobilier = `Crédits à l'investissement - immobilier accordés aux sociétés non financières résidentes`,
          Equipement = `Crédits à l'investissement - équipement accordés aux sociétés non financières résidentes`)
  

EncoursCredit <- inner_join(inner_join(EncoursTotal, EncoursTrésorie, by = "date"), EncoursInvestissement, by = "date")%>%
  rename(Total = `Crédits accordés aux sociétés non financières résidentes`,
         Tresorie = `Crédits de trésorerie accordés aux sociétés non financières résidentes`)%>%
  filter(!(date >= "1993-01-01" & date <= "1993-03-01"))%>%
  mutate(across(c(Total, Immobilier, Equipement, Tresorie), as.numeric),
         Autre = Total - Immobilier - Equipement - Tresorie) %>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
  group_by(Variable)%>%
  mutate( Croissance = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100,
          Index = Valeur / mean(Valeur[date >= "2019-01-01" & date <= "2019-12-31"]) * 100) %>%
  group_by(date)%>%
  mutate(Part = (Valeur / Valeur[Variable=="Total"])*100)%>%
  ungroup()%>%
  group_by(Variable)%>%
  mutate(GFC = ((Valeur[date == "2009-12-01"] - Valeur[date == "2008-09-01"])/ Valeur[date == "2008-09-01"])*100,
         Covid = ((Valeur[date == "2021-03-01"] - Valeur[date == "2020-02-01"])/ Valeur[date == "2020-02-01"])*100,
         PostCovid = ((Valeur[date == "2023-12-01"] - Valeur[date == "2021-04-01"])/ Valeur[date == "2021-04-01"])*100,
         Normal = ((Valeur[date == "2024-08-01"] - Valeur[date == "2024-01-01"])/ Valeur[date == "2024-01-01"])*100,
         Contribution = Croissance * lag(Part, n = 12) /100,
         ContributionGFC = GFC * Part[date == "2008-09-01"]/100,
         ContributionCovid = Covid * Part[date == "2020-02-01"]/100,
         ContributionPC = PostCovid * Part[date == "2021-04-01"]/100,
         ContributionNormal = Normal * Part[date == "2024-01-01"]/100)     


EndettementSource <- inner_join(inner_join(EncoursTotal, TitresCT, by = "date"), TitresLT, by = "date")%>%
  rename(Credit = `Crédits accordés aux sociétés non financières résidentes`,
         TitresCT = `Encours de titres de dette à court terme en euros`,
         TitresLT = `Encours de titres de dette à long terme`)%>%
  filter(date >= "1999-01-01")%>%
  mutate(Total = TitresCT + TitresLT + Credit)%>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
  group_by(Variable)%>%
  mutate( Croissance = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100)%>%
  group_by(date)%>%
  mutate(Part = (Valeur / Valeur[Variable=="Total"])*100)%>%
  group_by(Variable)%>%
  mutate(Contribution = Croissance * lag(Part, n = 12) /100,
         Covid = ((Valeur[date == "2021-03-01"] - Valeur[date == "2020-02-01"])/ Valeur[date == "2020-02-01"])*100,
         PostCovid = ((Valeur[date == "2024-01-01"] - Valeur[date == "2021-04-01"])/ Valeur[date == "2021-04-01"])*100,
         ContributionCovid = Covid * Part[date == "2020-02-01"]/100,
         ContributionPC = PostCovid * Part[date == "2021-04-01"]/100)

EncoursInvestissement2<- EncoursInvestissement%>%
pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
  filter(!(date >= "1993-01-01" & date <= "1993-04-01"))%>%
  group_by(Variable)%>%
  mutate( Croissance = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100)%>%
  group_by(date)%>%
  mutate(Part = (Valeur / Valeur[Variable=="EncoursInvestissement"])*100)%>%
  group_by(Variable)%>%
  mutate(Contribution = Croissance * lag(Part, n = 12) /100,
         Covid = ((Valeur[date == "2021-03-01"] - Valeur[date == "2020-02-01"])/ Valeur[date == "2020-02-01"])*100,
         PostCovid = ((Valeur[date == "2023-12-01"] - Valeur[date == "2021-04-01"])/ Valeur[date == "2021-04-01"])*100,
         Normal = ((Valeur[date == "2024-07-01"] - Valeur[date == "2024-01-01"])/ Valeur[date == "2024-01-01"])*100,
         ContributionPC = PostCovid * Part[date == "2021-04-01"]/100)



# Endettement$date <- as.Date(Endettement$date)
ordre_personnalise <- c("GCF", "Covid", "PostCovid", "Normal")

EndettementGraph <- EncoursCredit %>%
  filter(date == "2024-07-01")  %>%
  filter(! Variable == "EncoursInvestissement")%>%
  select(ContributionGFC, ContributionCovid, ContributionPC, ContributionNormal) %>%
  pivot_longer(cols = c(ContributionGFC, ContributionCovid, ContributionPC, ContributionNormal),
               names_to = "Variable1",
               values_to = "Contribution")%>%
  mutate(Variable1 = recode(Variable1, 
                            ContributionGFC = "GCF", 
                            ContributionCovid = "Covid", 
                            ContributionPC = "PostCovid",
                            ContributionNormal = "Normal"), 
    Variable1 = factor(Variable1, levels = ordre_personnalise))


moyenne_croissance <- EncoursCredit %>%
  filter(date >= "2013-01-01" & date <= "2019-01-01" & Variable == "Total") %>%
  summarise(moyenne_croissance = mean(Croissance, na.rm = TRUE))

total_treso <- EncoursCredit %>%
  filter(date >= "2020-06-01" & date <= "2021-12-01" & Variable == "EncoursTresorie") %>%
  summarise(total_treso = sum(Valeur, na.rm = TRUE))


# La variable moyenne_croissance contiendra la moyenne des valeurs de Croissance où Variable est "Total"
Graph9<- ggplot(aes(x = as.Date(date), y = Contribution), data = EncoursCredit) +
  geom_bar(data = EncoursCredit %>% 
              filter(!Variable %in% c("Total", "EncoursInvestissement")),
           aes(fill = Variable), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(EncoursCredit, Variable == "Total"),
             color = "black", size = 0.8) + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-5, 15), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2010-04-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))

date_debut <- as.Date("2020-05-01")
date_fin <- as.Date("2021-02-01")

# Filtrer les données et calculer la moyenne de la colonne "Croissance"
moyenne_croissance <- mean(
  subset(EncoursCredit, 
         Variable == "Total" & date >= date_debut & date <= date_fin)$Croissance,
  na.rm = TRUE
)

# Afficher le résultat
cat("La moyenne de la croissance entre", date_debut, "et", date_fin, "est :", moyenne_croissance, "\n")

Graph9

Croissance<- ggplot(aes(x = as.Date(date), y = Croissance, color = Variable), data = EncoursCredit) +
  geom_line(size = 0.8) + 
  labs(
    caption = "Source: Banque de France",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-25, 40), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2010-04-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))


Croissance


ggplot() +
  geom_line(data = EncoursCredit %>% filter( Variable == "Total"),
            aes(x = as.Date(date), y = Valeur),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(400000, 1466851), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("1993-04-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))


ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph9.png", plot = GraphEndettement, width = 16, height = 8, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph9.svg", plot = GraphEndettement, width = 8, height = 8, units = "in")

ggplot() +
  geom_line(data = EncoursCredit %>% filter( Variable == "Total"),
            aes(x = as.Date(date), y = Index),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(80, 130), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))

ggplot() +
  geom_line(data = EncoursCredit %>% filter( Variable == "Tresorie"),
            aes(x = as.Date(date), y = Index),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = ""
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(60, 150), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))


ggplot() +
  geom_line(data = EncoursCredit %>% filter( Variable == "Tresorie"),
            aes(x = as.Date(date), y = Croissance),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = ""
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-10, 40), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))

CroissanceCrises <- ggplot(data = EndettementGraph %>% filter(Variable1 != "GCF")) +
    geom_bar(aes(x = Variable1, y = Contribution, fill = Variable), 
             data = EndettementGraph %>% filter(Variable1 != "GCF", !Variable %in% c("Total")), 
             position = "stack", stat = "identity") +  
    geom_point(aes(x = Variable1, y = Contribution), 
               data = EndettementGraph %>% filter(Variable1 != "GCF", Variable == "Total"), 
               color = "black", size = 4.5) + 
    geom_text(aes(x = Variable1, y = Contribution, label = signif(Contribution, 2)), 
              data = EndettementGraph %>% filter(Variable1 != "GCF", Variable == "Total"), 
              size = 2, color = "white") +
    labs(
      caption = "Source: Banque de France\nNote: la période Covid est celle comprise entre février 2020 et mars 2021.\nEnfin la période post-Covid correspond aux mois entre celui d'avril 2021 et la dernière donnée disponible en janvier 2024.",
      title = "",
      x = "",
      y = "Contribution en pourcentage à la croissance totale"
    ) +
    theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
    theme(legend.position = "bottom") +  
    scale_y_continuous(limits = c(-2, 13.5), labels = scales::label_number(decimal.mark = ",")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_fill_manual(values = c("#DADAEB", "#6A51A3", "#9E9AC8", "#E7298A")) +
    guides(fill = guide_legend(title = NULL))
  
CroissanceCrises

EndettementGraph2 <- EncoursCredit %>%
  filter(date %in% c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")) %>%
  select(Contribution, date) 

CroissanceAn<- ggplot(data = EndettementGraph2) +
  geom_bar(data = EndettementGraph2 %>% 
             filter(!Variable %in% c("Total", "EncoursInvestissement")), 
           aes(x = date, y = Contribution, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_point(data = EndettementGraph2 %>% filter(Variable == "Total"), 
             aes(x = date, y = Contribution), 
             color = "black", size = 4.5) + 
  geom_text(data = EndettementGraph2 %>% filter(Variable == "Total"), 
            aes(x = date, y = Contribution, label = signif(Contribution, 2)), 
            size = 2, color = "white") +
  labs(
    caption = "Source: Banque de France
               Note: Variation de janvier en janvier",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-3, 14), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("#DADAEB", "#6A51A3","#9E9AC8", "#E7298A")) +
  guides(fill = guide_legend(title = NULL))

#Graph sur les sources d'endettement 
SourceEndettementContribution <- ggplot(data = EndettementSource) +
  geom_bar(data = EndettementSource %>% 
             filter(Variable %in% c("TitresCT", "TitresLT", "Credit")), 
           aes(x = as.Date(date), y = Contribution, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_line(data = EndettementSource %>% 
                           filter(Variable =="Total"),
                       aes(x = as.Date(date), y = Contribution),
                       color = "black", size = 0.6) +
  labs(
    caption = "Source: Banque de France
            Note: Variation de janvier en janvier",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-3, 14), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(limits = as.Date(c("2015-01-01", max(EndettementSource$date))), expand = c(0, 0)) +
  guides(fill = guide_legend(title = NULL))

SourceEndettementContribution2 <- ggplot(data = EndettementSource%>% 
         filter()) +
  geom_bar(data = EndettementSource %>% 
             filter(Variable %in% c("TitresCT", "TitresLT", "Credit")), 
           aes(x = as.Date(date), y = Contribution, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_line(data = EndettementSource %>% 
              filter(Variable =="Total"),
            aes(x = as.Date(date), y = Contribution),
            color = "black", size = 0.6) +
  labs(
    caption = "Source: Banque de France
            Note: Variation de janvier en janvier",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-3, 14), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(limits = as.Date(c("2018-01-01", max(EndettementSource$date))), expand = c(0, 0)) +
  guides(fill = guide_legend(title = NULL))



SourceEndettementContribution <- ggplot(data = EndettementSource %>% 
                                          filter(Variable %in% c("TitresCT", "TitresLT", "Credit"),
                                                 date == "2023-10-01")) +
  geom_bar(aes(x = 1, y = ContributionCovid, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_point(data = EndettementSource %>% filter(Variable == "Total"), 
             aes(x = 1, y = ContributionCovid), 
             color = "black", size = 2.5) +
  geom_text(data = EndettementSource %>% filter(Variable == "Total"), 
            aes(x = 1, y = ContributionCovid, label = round(ContributionCovid)), 
            color = "black", size = 3.5,vjust = -1) +  # Added label aesthetic
  geom_bar(aes(x = 2, y = ContributionPC, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_point(data = EndettementSource %>% filter(Variable == "Total"), 
             aes(x = 2, y = ContributionPC), 
             color = "black", size = 2.5) +
  geom_text(data = EndettementSource %>% filter(Variable == "Total"), 
            aes(x = 2, y = ContributionPC, label = round(ContributionPC)), 
            color = "black", size = 3.5,vjust = -1) +  # Added label aesthetic
  labs(
    caption = "Source: Banque de France\nNote: Variation de janvier en janvier",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-1, 13), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_blank(),   # Remove x-axis labels
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  guides(fill = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = NULL)

SourceEndettementContribution

EndettementSourceGraph <- EncoursSource %>%
  filter(date == "2024-01-01")  %>%
  filter(! Variable == "EncoursInvestissement")%>%
  select(ContributionGFC, ContributionCovid, ContributionPC) %>%
  pivot_longer(cols = c(ContributionGFC, ContributionCovid, ContributionPC),
               names_to = "Variable1",
               values_to = "Contribution")%>%
  mutate(Variable1 = recode(Variable1, 
                            ContributionGFC = "GCF", 
                            ContributionCovid = "Covid", 
                            ContributionPC = "PostCovid"), 
         Variable1 = factor(Variable1, levels = ordre_personnalise))


SourceEndettementMillion <- ggplot(data = EndettementSource) +
  geom_bar(data = EndettementSource %>% 
             filter(!Variable =="Total"), 
           aes(x = as.Date(date), y = Valeur, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_line(data = EndettementSource %>% 
              filter(Variable =="Total"),
            aes(x = as.Date(date), y = Valeur),
            color = "black", size = 0.6) + 
  labs(
    caption = "Source: Banque de France
            Note: Variation de janvier en janvier",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(0, 2000000), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(limits = as.Date(c("2015-01-01", max(EndettementSource$date))), expand = c(0, 0)) +
  guides(fill = guide_legend(title = NULL))

PartTitre <- ggplot(data = EndettementSource %>%
         mutate(date = as.Date(date)) %>%
         filter(Variable %in% c("TitresCT","TitresLT"))) +
  geom_line(aes(x = date, y = Part, color = Variable), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Part Titres CT/LT")

GraphCredit1<- ggplot(data = EncoursCredit %>%
         mutate(date = as.Date(date)) %>%
         filter(!Variable %in% c("Equipement","Immobilier"))) +
  geom_line(aes(x = date, y = Croissance, color = Variable), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 30), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Encours de Crédit, % de GA")


 
 
 GraphCredit2<- ggplot(data = EncoursCredit %>%
                         mutate(date = as.Date(date)), 
                       aes(x = date, y = Croissance, color = Variable)) +
   geom_line(show.legend = TRUE) +
   labs(
     caption = "Source: Banque de France",
     y = NULL,
     x = NULL,
     color = NULL
   ) +
   theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
   theme(legend.position = "bottom") +
   scale_x_date(limits = as.Date(c("2011-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
   scale_y_continuous(limits = c(-20, 40), labels = scales::label_number(decimal.mark = ",")) +
   ggtitle("Encours de Crédit, % de GA")



GraphCredit3<- ggplot(data = EncoursCredit %>%
                        mutate(date = as.Date(date))) +
  geom_line(aes(x = date, y = Valeur, color = Variable), show.legend = TRUE)+
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(10000, 1000000), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("Encours de Crédit, Millions Euros")

max_tresorerie <- max(EncoursCredit$Valeur[EncoursCredit$Variable == "Tresorerie"])

ggplot(data = EncoursCredit %>%
         filter(Variable =="Tresorie"),
         mutate(date = as.Date(date))) +
  geom_line(aes(x = date, y = Valeur, color = Variable), show.legend = TRUE)+
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(10000, 1000000), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("Encours de Crédit, Millions Euros")


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

#Données INSEE pour VA par secteur 
library(insee)

source<-"CNT-2014-CB"

bases<-get_dataset_list()

idbank_list <- get_idbank_list(source)

corr<-"CVS-CJO"
flux<-c("A17-FZ","A17-AZ","D-CNT","A17-GZ", "A17-HZ", "A17-JZ", "A17-LZ", "A17-MN")
variable<-"B1"
naf<-"Volumes aux prix de l'année précédente chaînés"


testCommerce<-idbank_list%>%
  filter(CORRECTION==corr,
         CNA_PRODUIT %in% flux,
         OPERATION %in% variable,
         VALORISATION_label_fr==naf)%>%
  select(idbank)%>%
  pull(idbank)


dataVA =
  get_insee_idbank(testCommerce) %>%
  split_title()%>%
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,TITLE_FR2,OPERATION_label_fr)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_")%>%
  rename(`Info` = `Information-communication`,
         Total = `Total branches`,
         Conseil = `Services aux entreprises`,
         Immo = `Services immobiliers`,
         date = DATE)%>%
  select(!"OPERATION_label_fr")%>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
  rename(VA = Valeur)
 
  
dataVATotal <- dataVA%>% 
  filter(Variable == "Total" & date >= "1993-04-01")%>%
  inner_join(EncoursCredit, by ="date")%>%
  mutate(Rapport = Valeur /VA)
  
EncoursMillions <- ggplot(data = EncoursCredit %>%
                        mutate(date = as.Date(date)) %>%
                        filter(!Variable %in% c("EncoursInvestissement", "Total")), 
                      aes(x = date, y = Valeur, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") +  
  labs(
    title = "",
    x = "",
    y = "En Millions euros"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2013-02-01", "2024-01-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, 1400000), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

EncoursVA <- ggplot(data = dataVATotal %>%
                            mutate(date = as.Date(date)) %>%
                            filter(!Variable.y %in% c("EncoursInvestissement", "Total")), 
                          aes(x = date, y = Rapport, fill = Variable.y)) +
  geom_bar(position = "stack", stat = "identity") +  
  labs(
    title = "",
    x = "",
    y = "en points de VA"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2019-02-01", "2024-01-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, 2.75), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


ggsave(filename = "C:/Users/153003/Documents/Entreprise/EncoursMillions.png", plot = EncoursMillions, width = 8, height = 6, units = "in")


#Taux de Croissance des crédits accordés aux entreprises

url_CroissanceCreditTotal <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A26.A.I.U6.2240.Z01.A")
url_CroissanceCreditTreso <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N2Z.A.I.U6.2240.Z01.A")
url_CroissanceCreditInvestissement <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N1Z.A.I.U6.2240.Z01.A")
url_CroissanceCreditAutre <- generate_url(dataset_id = "BSI1", series_key = "BSI1.M.FR.N.R.A2N3Z.A.I.U6.2240.Z01.A")
url_TauxCreditTotal <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A20.A.R.A.2240U6.EUR.N")
url_TauxTitreDette <- generate_url(dataset_id = "MIR1", series_key = "MIR1.M.FR.B.A30.K.R.1.2240.EUR.N")

# Fetch and process each dataset
CroissanceCreditTotal <- fetch_and_process_data(url_CroissanceCreditTotal)
CroissanceCreditTreso <- fetch_and_process_data(url_CroissanceCreditTreso)
CroissanceCreditInvestissement <- fetch_and_process_data(url_CroissanceCreditInvestissement)
CroissanceCreditAutre <- fetch_and_process_data(url_CroissanceCreditAutre)
TauxCreditTotal <- fetch_and_process_data(url_TauxCreditTotal)
TauxTitreDette <- fetch_and_process_data(url_TauxTitreDette)

DataCredit <- inner_join(CroissanceCreditTotal, TauxCreditTotal, by = "date")%>%
  rename(CroissanceCreditTotal = "Crédits accordés aux sociétés non financières résidentes",
          TauxCreditTotal = "Crédits nouveaux aux SNF")

DataCredit <- inner_join(DataCredit, TauxTitreDette, by ="date")%>%
  rename(TauxTitreDette = "Taux d'intérêt annuel")

CroissanceTitres <- Titres%>%
  rename (CroissanceTitres= "Encours de titres de dette émis par les sociétés non financières")%>%
mutate(CroissanceTitres = (CroissanceTitres - lag(CroissanceTitres, n = 12)) / lag(CroissanceTitres, n = 12) * 100)

DataCredit <- inner_join(DataCredit, CroissanceTitres, by ="date")%>%
  mutate(SpreadTaux = TauxCreditTotal - TauxTitreDette,
         SpreadCroissance = CroissanceCreditTotal - CroissanceTitres)
         

ggplot(data = DataCredit) +
  geom_line(aes(x = as.Date(date), y = CroissanceCreditTotal, color = "Taux de croissance annuelle des crédits"), size = 1) +
  geom_line(aes(x = as.Date(date), y = TauxCreditTotal, color = "Taux d'intérêt annuel des nouveaux crédits"), size = 1) +
  labs(
    caption = "",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2017-01-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.5, 13.5), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(
    values = c("Taux de croissance annuelle des crédits" = "blue", 
               "Taux d'intérêt annuel des nouveaux crédits" = "red"))

CroissanceTaux

CroissanceTauxTitreDette<- ggplot(data = DataCredit) +
  geom_line(aes(x = as.Date(date), y = TauxTitreDette, color = "Titres de dette émis par les SNF, taux d'intérêt annuel"), show.legend = TRUE) +
  geom_line(aes(x = as.Date(date), y = CroissanceTitres, color = "Titres de dette émis par les SNF, croissance annuelle des encours"), show.legend = TRUE) +
  labs(
    caption = "",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2018-01-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 20), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL,
                     labels = c("Croissance annuelle des titres",
                                "Taux d'intérêt annuel"),
                     values = c("blue", "red")) +  # Specify color values for the two lines
  ggtitle("Titres de dette émis par les SNF")

CroissanceTauxTitreDette

#Comparaison des 2

CroissanceEncoursTitre<- ggplot(data = DataCredit) +
  geom_line(aes(x = as.Date(date), y = CroissanceCreditTotal , color = "Crédits accordés aux sociétés non financières résidentes, croissance annuelle"), show.legend = TRUE) +
  geom_line(aes(x = as.Date(date), y = CroissanceTitres, color = "Titres de dette émis par les SNF, croissance annuelle des encours"), show.legend = TRUE) +
  labs(
    caption = "",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 17), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL,
                     labels = c("Croissance encours de crédit",
                                "Croissance des titres de dette"),
                     values = c("blue", "red")) +  # Specify color values for the two lines
  ggtitle("Financement des SNF")

SpreadTaux<- ggplot(data = DataCredit) +
  geom_line(aes(x = as.Date(date), y = TauxTitreDette, color = " taux d'intérêt annuel des titres de dettes"), show.legend = TRUE) +
  geom_line(aes(x = as.Date(date), y = TauxCreditTotal, color = "taux d'intérêt des nouveaux crédits"), show.legend = TRUE) +
  labs(
    caption = "",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2007-01-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 6), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL,
                     labels = c("taux d'intérêt annuel des titres de dettes",
                                "taux d'intérêt des nouveaux crédits"),
                     values = c("blue", "red")) +  # Specify color values for the two lines
  ggtitle("spread de taux financement SNF")

CorrelationVariation<- ggplot(data = DataCredit) +
  geom_line(aes(x = as.Date(date), y = SpreadTaux, color = " différence des taux"), show.legend = TRUE) +
  geom_line(aes(x = as.Date(date), y = SpreadCroissance, color = "différence de variation"), show.legend = TRUE) +
  labs(
    caption = "",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-16, 8), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL,
                     labels = c("différence de taux",
                                "différence de variation"),
                     values = c("blue", "red")) +  
  ggtitle("Elasticité Credit/Titre")


# ggplot(data = EncoursCredit %>%
#          filter(Variable == c("EncoursInvestissement", "Immobilier", "Investissement"))) +
#   geom_line(aes(x = as.Date(date), y = Valeur, color = "Crédits accordés aux sociétés non financières résidentes"), show.legend = TRUE) +
#   #geom_line(aes(x = as.Date(date), y = TauxCreditTotal, color = "Crédits nouveaux aux SNF, taux d'intérêt annuel"), show.legend = TRUE) +
#   labs(
#     caption = "",
#     y = NULL,
#     x = NULL,
#     color = NULL
#   ) +
#   theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
#   theme(legend.position = "bottom") +
#   scale_x_date(limits = as.Date(c("2011-01-01", max(DataCredit$date))), expand = c(0, 0)) +
#   scale_y_continuous(limits = c(0,1000000), labels = scales::label_number(decimal.mark = ",")) +
#   scale_color_manual(name = NULL)


# Plot
library(lubridate)

Graph8 <-ggplot(data = DataCredit%>%
         filter(date >= "2019-01-01")) +
  geom_point(aes(x = CroissanceCreditTotal, y = TauxCreditTotal, color = as.factor(lubridate::year(date))),
             size = 3) +
  labs(
    caption = "",
    x = "Croissance annuelle  des nouveaux crédits",
    y = "Taux d'intérêt annuel des nouveaux crédits",
    color = ""
  ) +
  scale_color_manual(values = c("2019" = "#98F5FF", "2020" = "#53868B", 
                                "2021" = "#8EE5EE", "2022" = "#7AC5CD", "2023" ="#00868B" , "2024"= "#2F4F4F")) + 
  theme_minimal() +
  geom_text(data = DataCredit %>% filter(year(date) == 2023),
            aes(label = format(date, "%m"), x = CroissanceCreditTotal, y = TauxCreditTotal),
            vjust = -1, size = 2.5) +
  geom_path(aes(x = CroissanceCreditTotal, y = TauxCreditTotal),
            color = "gray") 

ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph8.png", plot = CorrelationCredit, width = 16, height = 8, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph8.svg", plot = CorrelationCredit, width = 8, height = 6, units = "in")



Correlationdette <-ggplot(data = DataCredit%>%
                             filter(date >= "2019-01-01")) +
  geom_point(aes(x = CroissanceTitres, y = TauxTitreDette, color = as.factor(lubridate::year(date))),
             size = 3) +
  labs(
    caption = "",
    x = "Croissance annuelle des titres de dette",
    y = "Taux d'intérêt annuel des titres de dette",
    color = ""
  ) +
  scale_color_manual(values = c("2019" = "#98F5FF", "2020" = "#53868B", 
                                "2021" = "#8EE5EE", "2022" = "#7AC5CD", "2023" ="#00868B" , "2024"= "#2F4F4F")) + 
  theme_minimal() +
  geom_text(data = DataCredit %>% filter(year(date) == 2023),
            aes(label = format(date, "%m"), x = CroissanceTitres, y = TauxTitreDette),
            vjust = -1, size = 2.5) +
  geom_path(aes(x = CroissanceTitres, y = TauxTitreDette),
            color = "gray") 

CorrelationPastCredit <- ggplot(data = DataCredit %>%
                           filter(year(date) %in% c(2005, 2006, 2007, 2008, 2009, 2023))) +
  geom_point(aes(x = CroissanceCreditTotal, y = TauxCreditTotal, color = as.factor(year(date))),
             size = 3) +
  labs(
    caption = "",
    x = "Croissance annuelle des crédits",
    y = "Taux d'intérêt annuel des nouveaux crédits",
    color = ""
  ) +
  scale_color_manual(values = c("2005" = "#1C86EE", "2006" = "#9AC0CD", 
                                "2007" = "#607B8B", "2008" = "#00BFFF", "2009" = "#87CEFA", "2023" ="#7A378B")) + 
  theme_minimal() +
  geom_path(aes(x = CroissanceCreditTotal, y = TauxCreditTotal),
            color = "gray") 

 

CorrelationAllCredit <- ggplot(data = DataCredit, aes(x = CroissanceCreditTotal, y = TauxCreditTotal, color = as.factor(year(date)))) +
  geom_point(size = 2) +
  labs(
    caption = "",
    x = "Croissance annuelle des crédits",
    y = "Taux d'intérêt annuel des nouveaux crédits",
    color = ""
  ) +  
  theme_minimal() +
  geom_path(aes(x = CroissanceCreditTotal, y = TauxCreditTotal), color = "gray")

CorrelationAllDette <- ggplot(data = DataCredit, aes(x = CroissanceTitres, y = TauxTitreDette, color = as.factor(year(date)))) +
  geom_point(size = 2) +
  labs(
    caption = "",
    x = "Croissance annuelle des titres de dette",
    y = "Taux d'intérêt annuel des titres de dette",
    color = ""
  ) +  
  theme_minimal() +
  geom_path(aes(x = CroissanceTitres, y = TauxTitreDette), color = "gray")




# Enquête d'accès au crédit
PME<- w_data(dataset_name = "CONJ", series_name = "CONJ.Q.N01.N.TS.000TA.DMTIA001.PM")
EI<- w_data(dataset_name = "CONJ", series_name = "CONJ.Q.N01.N.TS.000TA.DMTIA001.EI")

Conjoncture <- inner_join (PME, EI, by = "date") %>%
      rename( PME = "CONJ.Q.N01.N.TS.000TA.DMTIA001.PM",
              EI = "CONJ.Q.N01.N.TS.000TA.DMTIA001.EI")

Conjoncture <- ggplot(data = Conjoncture) +
  geom_line(aes(x = as.Date(date), y = PME, color = "PME"), show.legend = TRUE) +
  geom_line(aes(x = as.Date(date), y = EI, color = "EI"), show.legend = TRUE) +
  labs(
    caption = "Proportion",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2012-04-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(78, 98), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL, values = c("PME" = "blue", "EI" = "red"))

# Flux de Crédit

FluxTotal <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.Y.R.A26.A.4.U6.2240.Z01.E")
FluxTreso <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.Y.R.A2N2Z.A.4.U6.2240.Z01.E")
FluxInv<- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.Y.R.A2N1Z.A.4.U6.2240.Z01.E")

Flux <- inner_join(inner_join(FluxTotal, FluxTreso, by = "date"), FluxInv, by = "date")%>%
  rename(
    Total = "BSI1.M.FR.Y.R.A26.A.4.U6.2240.Z01.E",
    Treso = "BSI1.M.FR.Y.R.A2N2Z.A.4.U6.2240.Z01.E",
    Inv = "BSI1.M.FR.Y.R.A2N1Z.A.4.U6.2240.Z01.E"
    )


Flux <- ggplot(data = Flux) +
  geom_line(aes(x = as.Date(date), y = Total, color = "Total"), show.legend = TRUE) +
  geom_line(aes(x = as.Date(date), y = Treso, color = "Treso"), show.legend = TRUE) +
  geom_line(aes(x = as.Date(date), y = Inv, color = "Inv"), show.legend = TRUE) +
  labs(
    caption = "",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2018-01-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-8000, 18000), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL, values = c("Total" = "blue", "Treso" = "red", "Inv" = "black"))

#Encours par Secteur

Agriculture <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.AZ.TT")
InduManuf <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.C.TT")
Indu <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.BE.TT")
Construction <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.FZ.TT")
Commerce <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.G.TT")
Transport <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.H.TT")
Info <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.JZ.TT")
Immo <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.LZ.TT")
Conseil <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.MN.TT")
Enseignement <- w_data(dataset_name = "DIREN", series_name = "DIREN.M.FR.CR.LME.ME.01.N.PS.TT") 


Secteur1 <- inner_join(inner_join(Agriculture, InduManuf, by ="date"), Indu, by = "date")%>%
  rename( Agriculture = "DIREN.M.FR.CR.LME.ME.01.N.AZ.TT",
          InduManuf = "DIREN.M.FR.CR.LME.ME.01.N.C.TT",
          Indu = "DIREN.M.FR.CR.LME.ME.01.N.BE.TT")

Secteur2 <- inner_join(inner_join(Construction, Commerce, by ="date"), Transport, by = "date")%>%
  rename( Construction = "DIREN.M.FR.CR.LME.ME.01.N.FZ.TT",
          Commerce = "DIREN.M.FR.CR.LME.ME.01.N.G.TT",
          Transport = "DIREN.M.FR.CR.LME.ME.01.N.H.TT")

Secteur3 <- inner_join(inner_join(Info, Immo, by ="date"), Conseil, by = "date")%>%
  rename( Info = "DIREN.M.FR.CR.LME.ME.01.N.JZ.TT",
         Immo = "DIREN.M.FR.CR.LME.ME.01.N.LZ.TT",
          Conseil = "DIREN.M.FR.CR.LME.ME.01.N.MN.TT")

Secteur <- inner_join(inner_join(Secteur1, Secteur2, by ="date"), Secteur3, by = "date")%>%
  select(date,Agriculture, InduManuf, Indu, Construction, Commerce, Transport, Info, Immo, Conseil)%>%
  filter( date >= "2012-03-01")%>%
  mutate(Total = Agriculture + Indu + Construction+ Commerce+Transport+Info+Immo+Conseil,
         TotalSansImmo = Agriculture + Indu + Construction+ Commerce+Transport+Info+Conseil)%>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
  group_by(date)%>%
  mutate(Part= Valeur/Valeur[Variable=="Total"]*100)%>%
  group_by(Variable)%>%
  mutate(Croissance = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100,
         Contribution = Croissance * lag(Part, n = 12) /100)

SecteurGraph <- ggplot(aes(x = as.Date(date), y = Contribution), data = Secteur) +
  geom_bar(data = Secteur%>% 
             filter(Variable %in% c("Agriculture", "Construction", "Immo", "Indu", "Info", "Commerce", "Conseil", "Transport")),
           aes(fill = Variable), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(Secteur, Variable == "Total"),
            color = "black", size = 0.7) + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-2, 13), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(Secteur$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))+
  ggtitle("Encours des SNF par secteur")

SecteurGraph2 <- ggplot(aes(x = as.Date(date), y = Contribution), data = Secteur) +
  geom_bar(data = Secteur%>% 
             filter(Variable %in% c("Agriculture", "Construction", "Indu", "Info", "Commerce", "Conseil", "Transport")),
           aes(fill = Variable), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(Secteur, Variable == "TotalSansImmo"),
            color = "black", size = 0.7) + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-2, 11), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(Secteur$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))+
  ggtitle("Encours des SNF par secteur sans Immo")
  

#On prend la donnée du dernier mois comme celle du trimestre et on change la date pour avoir la même que VA


SecteurTr<-  inner_join(inner_join(Secteur1, Secteur2, by ="date"), Secteur3, by = "date")%>%
  select(date,Agriculture, InduManuf, Indu, Construction, Commerce, Transport, Info, Immo, Conseil)%>%
  filter( date >= "2012-03-01")%>%
  mutate(Total = Agriculture + Indu + Construction+ Commerce+Transport+Info+Immo+Conseil)%>%
  filter(month(date) %in% c(3, 6, 9, 12))%>%
  mutate(date = if_else(month(date) == 3, as.Date(paste0(year(date), "-01-01")), date),
         date = if_else(month(date) == 6, as.Date(paste0(year(date), "-04-01")), date),
         date = if_else(month(date) == 9, as.Date(paste0(year(date), "-07-01")), date),
         date = if_else(month(date) == 12, as.Date(paste0(year(date), "-10-01")), date))%>%
pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
  rename(Encours = Valeur)

dataVA <- dataVA%>%
filter( date >= "2012-01-01")

SecteurTrim <- inner_join(SecteurTr, dataVA, by = c("date", "Variable"))%>%
  mutate( Rapport = Encours /VA,
          Index = Rapport /  Rapport [date == "2019-10-01"] * 100)

EncoursVASecteur <- ggplot(data = SecteurTrim) +
  geom_line(aes(x = as.Date(date), y = Rapport, color = Variable), show.legend = TRUE) +
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2012-04-01", max(SecteurTrim$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 9), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Rapport des Encours des SNF et de la VA par secteur")

EncoursVASecteurIndex <- ggplot(data = SecteurTrim) +
  geom_line(aes(x = as.Date(date), y = Index, color = Variable), show.legend = TRUE) +
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2012-04-01", max(SecteurTrim$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(60, 155), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Rapport des Encours des SNF et de la VA par secteur, T4:2019 = 100")


save(GraphEndettement, CroissanceCrises, CroissanceAn, EncoursVA, Flux, CroissanceTaux, Correlation, CorrelationPast, Conjoncture, SecteurGraph,
     file = "graphiques_Credit.rda")



#Analyse de la correlation Credit/taux d'intéret

# 1. Corrélation entre 2004 et 2020 

DataJusqu2020 <- subset(DataCredit, date <= as.Date("2019-12-01"))
correlation_jusqu2020 <- cor(DataJusqu2020$CroissanceCreditTotal, DataJusqu2020$TauxCreditTotal, use="complete.obs")
cat("Corrélation jusqu'à fin 2019 : ", correlation_jusqu2020, "\n")

# 2. Corrélation entre Mars 2020 et Décembre 2022
DataMars2020Dec2022 <- subset(DataCredit, date >= as.Date("2020-03-01") & date <= as.Date("2022-11-01"))
correlation_mars2020_dec2022 <- cor(DataMars2020Dec2022$CroissanceCreditTotal, DataMars2020Dec2022$TauxCreditTotal, use = "complete.obs")
cat("Corrélation entre Mars 2020 et Décembre 2022 : ", correlation_mars2020_dec2022, "\n")

# 3. Corrélation entre Janvier 2023 et Août 2024
DataJan2023Aug2024 <- subset(DataCredit, date >= as.Date("2023-01-01") & date <= as.Date("2024-08-01"))
correlation_jan2023_aout2024 <- cor(DataJan2023Aug2024$CroissanceCreditTotal, DataJan2023Aug2024$TauxCreditTotal, use = "complete.obs")
cat("Corrélation entre Janvier 2023 et Août 2024 : ", correlation_jan2023_aout2024, "\n")



