install.packages(c("stargazer","ggthemes", "eurostat","ofce","parallel","lubridate","ggplot2","plotrix","Hmisc","readxl","httr","data.table","insee", "writexl","tidyverse","zoo"))
install.packages("devtools", "writexl", "Rtools","inseeLocalData","ggsci")
install.packages("systemfonts")
devtools::install_github("OFCE/ofce")
devtools::install_github("oddworldng/INEbaseR")

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
library(readxl)


#Paramètres séries à garder : Excédent Brut d'Exploitation et VA par produit trimestrielle

bases<-get_dataset_list()

dataVA <-get_insee_dataset("CNT-2020-CB")%>%
  filter(CORRECTION=="CVS-CJO")%>%
  filter(CNA_PRODUIT %in% c("SMNA-CNT", "DIM-CNT","DSM-CNT", "DI-CNT","A17-FZ","A17-AZ","D-CNT","A17-C1", "A17-C2", "A17-C3", "A17-C4","A17-C5", "A17-DE","A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU"))%>%
  filter(OPERATION %in% c("B1")) %>%
  filter(VALORISATION == "L")%>%
  split_title()%>%
  select(DATE,OBS_VALUE,TITLE_FR1,TITLE_FR2)%>%
  arrange(DATE)%>%
  #pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_")%>%
  rename(value = OBS_VALUE,
         name = TITLE_FR2)%>%
  group_by(name)%>%
  mutate(Index = value / value[DATE == "2019-10-01"] * 100,
         Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100)

  
  #group_by(DATE,name)%>%
  #mutate(Part = value/value[name == "Total Branches"] * 100) 
  #ungroup()


GraphVA  <-ggplot(data = dataVA) +
  geom_line(data = . %>% filter(
    name %in% c("Industries agro-alimentaires", "Commerce", "Construction", 
                "Information-communication", 
                "Services principalement marchands", "Industrie")),
    aes(x = DATE, y = Index, color = name), show.legend = TRUE) +
  labs(
    caption = "Source: Insee",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataVA$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(60, 125), oob = scales::squish, labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("France: Valeur ajoutée des SNF par branches, (Index T4:2019 = 100")

GraphVA

dataEBE <- get_insee_dataset("CNT-2020-CB")%>%
  filter(CORRECTION=="CVS-CJO")%>%
  filter(CNA_PRODUIT %in% c("SMNA-CNT", "DSM-CNT", "DIM-CNT", "DI-CNT","A17-FZ","A17-AZ","D-CNT","A17-C1", "A17-C2", "A17-C3", "A17-C4","A17-C5", "A17-DE","A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU"))%>%
  filter(OPERATION %in% c("B2")) %>%
  split_title()%>%
  select(DATE,OBS_VALUE,TITLE_FR2)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_")

dataEBE2 <- dataEBE %>%
  pivot_longer(cols = -c(DATE)) %>%
  mutate(Index = value / value[DATE == "2019-10-01"] * 100)%>%
  group_by(name)%>%
  mutate(Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100)

# Contribution à l'EBE par branche (partie à recoder)/ Marge

datamarge <- bind_rows(dataVA,dataEBE) %>%
  mutate(Total = `Total branches` - `Services non marchands` - `Agriculture`)%>%
  pivot_longer(cols = -c(DATE, name)) %>%
  group_by(DATE, name) %>%
  arrange(DATE) 

marge_rows <- datamarge %>%
  group_by(DATE, name) %>%
  summarise(value = (value[OPERATION_label_fr == "B2 - Excédent d'exploitation"] / value[OPERATION_label_fr == "B1 - Valeur ajoutée"]) * 100, .groups = 'drop') %>%
  mutate(OPERATION_label_fr = "Marge") %>%
  mutate(Index = value / value[DATE == "2019-10-01"] * 100)


file_path <- "C:/Users/153003/Documents/Entreprise/Marge.xlsx" 

Marge <- read_excel(file_path) %>%
  pivot_longer(cols = -c(DATE)) %>%
  mutate(DATE = as.Date(DATE) )

Graph7 <- ggplot(data=Marge %>% filter(name %in% c("Autres branches industrielles", "Commerce", "Construction", 
                                                                       "Services aux entreprises", "Hébergement-restauration","Industries agro-alimentaires","Énergie, eau, déchets",
"Service Immobiliers","Information-communication", "Services financiers", "Transport", "Biens d'équipement", "Matériels de transport", "Services aux ménages")),
                               aes(x = DATE, y = value, fill = name)) +
  geom_bar(position = "stack", stat = "identity") + 
  geom_line(data = Marge %>% 
              filter(name == "Marchand non agricole"),
            aes(x = DATE, y = value), show.legend = FALSE)  +
  labs(
    title = "Contributions to Growth",
    x = "",
    y = "Contribution (en pp)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 9),  # Adjust legend title size
        legend.text = element_text(size = 6)) +  # Adjust legend text size # 
  scale_x_date(limits = as.Date(c("2018-11-01", max(Marge$DATE))), date_labels = "%Y-%m-%d", breaks = "1 years") +
  scale_y_continuous(limits = c(min(Marge$value), 5), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(fill = guide_legend(title = NULL)) +
  ggtitle(" Contribution à la croissance du taux de marge par branche relativement à la moyenne de 2018")

Graph7

ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph7.png", plot = Graph7, width = 8, height = 6, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph7.svg", plot = Graph7, width = 16, height = 8, units = "in")


GraphRepere <- ggplot(data=Marge %>% filter(name %in% c( "Construction","Énergie, eau, déchets",
"Manufacturier", "Services principalement marchands")),
                 aes(x = DATE, y = value, fill = name)) +
  geom_bar(position = "stack", stat = "identity") + 
  geom_line(data = Marge %>% 
              filter(name == "Marchand non agricole"),
            aes(x = DATE, y = value), show.legend = FALSE)  +
  labs(
    title = "Contributions to Growth",
    x = "",
    y = "Contribution (en pp)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 9),  # Adjust legend title size
        legend.text = element_text(size = 6)) +  # Adjust legend text size # 
  scale_x_date(limits = as.Date(c("2018-11-01", max(Marge$DATE))), date_labels = "%Y-%m-%d", breaks = "1 years") +
  scale_y_continuous(limits = c(min(Marge$value), 5), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  guides(fill = guide_legend(title = NULL)) +
  ggtitle(" Contribution à la croissance du taux de marge par branche relativement à la moyenne de 2018")

GraphRepere

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphRepere.png", plot = GraphRepere, width = 8, height = 6, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphRepere.svg", plot = GraphRepere, width = 16, height = 8, units = "in")


GraphMargeIndex <- ggplot(data = datamarge) +
  geom_line(data = . %>% filter(OPERATION_label_fr == "Marge" & 
                                  name %in% c("Industries agro-alimentaires", "Commerce", "Construction", 
                                              "Information-communication", 
                                              "Services principalement marchands", "Industrie")),
            aes(x = DATE, y = Index, color = name), show.legend = TRUE) +
  labs(
    caption = "Source: Insee",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(datamarge$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(60, 170), oob = scales::squish, labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("France: Marge Index par rapport au T4:2019")

GraphMargeVA <- ggplot(data = marge_rows) +
  geom_line(data = . %>% 
              filter(name %in% c("Industries agro-alimentaires", "Commerce", "Construction", "Information-communication", "Hébergement-restauration", "Services principalement marchands", "Industrie"),
                     OPERATION_label_fr == "Marge"),
            aes(x = DATE, y = value, color = name), show.legend = TRUE) +
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(datamarge$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(15, 80), oob = scales::squish, labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("France: Marge en % de la VA")


ggplot(data=datamarge %>% filter(OPERATION_label_fr == "B2 - Excédent d'exploitation" & 
                                    name %in% c("Service Immobiliers", "Commerce", "Construction", 
                                                "Services aux entreprises", "Industrie", "Information-communication")),
       aes(x = DATE, y = Contribution, fill = name)) +
  geom_bar(position = "stack", stat = "identity") +  # Create a stacked bar chart
  labs(
    title = "Contributions to Growth",
    x = "",
    y = "Contribution (%)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2016-01-01", "2023-10-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(-10, 12), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("France: Contribution à la croissance de l'EBE par branche, en %")



print(GraphFBCFmarge1)
ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFmargeIndex.png", plot = GraphFBCFmargeIndex, width = 8, height = 6, units = "in")

GraphFBCFmarge1 <- ggplot(data=marge_rows %>% filter(name %in% c("Total","Industries agro-alimentaires", "Commerce", "Construction", "Information-communication", "Hébergement-restauration", "Services principalement marchands", "Industrie"))) +
  geom_line(aes(x = DATE, y = Index, color = name), show.legend = TRUE)+
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit.raul$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(60, 140), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: Marge des SNF en Indice T42019=100")

print(GraphFBCFmarge1)

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFmarge1.png", plot = GraphFBCFmarge1, width = 8, height = 6, units = "in")

#Paramètres séries à garder : FCBF par produit pour entreprises non financières trimestrielle

dataFBCFproduit <- get_insee_dataset("CNT-2020-OPERATIONS")%>%
  filter(CORRECTION=="CVS-CJO")%>%
  filter(CNA_PRODUIT %in% c("A17-FZ","A17-AZ","D-CNT","A17-C1", "A17-C2", "A17-C3", "A17-C4","A17-C5", "A17-DE","A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU"))%>%
  filter(OPERATION %in% c("P51S")) %>%
  filter(VALORISATION %in% c("L","V"))%>%
  split_title() %>%
  add_insee_metadata() %>%
  select(DATE, OBS_VALUE, TITLE_FR2, TITLE_FR3) %>%
  arrange(DATE) %>%
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_") %>%
  mutate(
    Autres = `Services aux ménages` + `Produits agricoles` + `Autres produits industriels` + `Services immobiliers`,
    Total = `Services aux ménages` + `Produits agricoles` + `Autres produits industriels` + `Services immobiliers` + `Biens d'équipement` + `Construction` + `Matériels de transport` + `Information-communication` + `Services aux entreprises`)%>%
  pivot_longer(cols = -c(DATE, TITLE_FR3)) %>%
  group_by(TITLE_FR3, name) %>%
  arrange(DATE) %>%
  mutate(
    Index = value / value[DATE=="2019-10-01"]*100,
    CroissanceAnnuelle = (value - lag(value, n = 4)) / lag(value, n = 4) * 100,
    CroissanceTrimestrielle = (value - lag(value, n = 1)) / lag(value, n = 1) * 100,
    Croissance2019 = (value - value[DATE== "2019-10-01"]) /  value[DATE== "2019-10-01"] * 100) %>%
  ungroup()%>%
  group_by(DATE,TITLE_FR3)%>%
  mutate(Part= (value/ value[name == "Total"]*100))%>%
  ungroup()%>%
  group_by(TITLE_FR3,name)%>%
  mutate(ContributionAnnuelle = CroissanceAnnuelle * lag(Part, n = 4) /100,
         ContributionTrimestrielle = CroissanceTrimestrielle * lag(Part, n = 1) /100,
         Contribution2019 = Croissance2019 * (Part[DATE=="2019-10-01"])/100)%>%
  rename(Produit=name)%>%
  ungroup()
     


GraphFBCFparproduit<- ggplot(data=  dataFBCFproduit %>%
                               filter((Produit %in% c( "Autres", "Biens d'équipement","Construction","Matériels de transport","Information-communication","Services aux entreprises")) &
                                        (TITLE_FR3 %in% "Volume aux prix de l'année précédente chaînés"))) +
  geom_line(aes(x = DATE, y = Index, color = Produit), show.legend = TRUE)+
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(50, 130), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par produits en volume en Indice T42019=100")

GraphFBCFparproduit
ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduit.png", plot = GraphFBCFparproduit, width = 8, height = 6, units = "in")
 
GraphFBCFparproduitGA <- ggplot(data=dataFBCFproduit %>%
                                  filter((Produit %in% c( "Autres", "Biens d'équipement","Construction","Matériels de transport","Information-communication","Services aux entreprises")) &
                                           (TITLE_FR3 %in% "Volume aux prix de l'année précédente chaînés"))) +
  geom_line(aes(x = DATE, y = CroissanceAnnuelle, color = Produit), show.legend = TRUE)+
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-30, 60), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par produits en glissement annuel en volume, en %")

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduitGA.png", plot = GraphFBCFparproduitGA, width = 8, height = 6, units = "in")

GraphFBCFparproduitGA

GraphFBCFTotalGA <- ggplot(data=dataFBCFproduit %>%
                                  filter((Produit == "Tx_Investissement") &
                                           (TITLE_FR3 %in% "Volume aux prix de l'année précédente chaînés"))) +
  geom_line(aes(x = DATE, y = value, color = Produit), show.legend = TRUE)+
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(11, 17), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par produits en glissement annuel en volume, en %")

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduitGA.png", plot = GraphFBCFparproduitGA, width = 8, height = 6, units = "in")

GraphFBCFTotalGA

GraphFBCFparproduitContribution <- ggplot(data = dataFBCFproduit %>%
                                            filter(Produit %in% c("Autres", "Biens d'équipement", "Construction", "Matériels de transport", "Information-communication", "Services aux entreprises") & 
                                                     TITLE_FR3 == "Volume aux prix de l'année précédente chaînés")) +
  geom_bar(aes(x = DATE, y = Contribution2019, fill = Produit), position = "stack", stat = "identity") +  
  geom_point(data = dataFBCFproduit %>%
               filter(Produit %in% c("Total") & TITLE_FR3 == "Volume aux prix de l'année précédente chaînés"), aes(x = DATE, y = Contribution2019), color = "black", size = 3) +# Create a stacked bar chart
  labs(
    title = "Contributions to Growth",
    
    x = "",
    y = "Contribution (%)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2017-01-01", max(dataFBCFproduit$DATE))), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(-10, 12), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("France: Contribution à la croissance des FBCF des SNF par produits en volume, en %")

GraphFBCFparproduitContribution
ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduitContribution.png", plot = GraphFBCFparproduitContribution, width = 8, height = 6, units = "in")

GraphFBCFparproduitContributionBis <- ggplot(data = dataFBCFproduit %>%
                                            filter(Produit %in% c("Autres", "Biens d'équipement", "Construction", "Matériels de transport", "Information-communication", "Services aux entreprises") & 
                                                     TITLE_FR3 == "Volume aux prix de l'année précédente chaînés")) +
  geom_bar(aes(x = DATE, y = ContributionAnnuelle, fill = Produit), position = "stack", stat = "identity") +  
  geom_point(data = dataFBCFproduit %>%
               filter(Produit %in% c("Total") & TITLE_FR3 == "Volume aux prix de l'année précédente chaînés"), aes(x = DATE, y = ContributionAnnuelle), color = "black", size = 3) +# Create a stacked bar chart
  labs(
    title = "Contributions to Growth",
    
    x = "",
    y = "Contribution (%)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2017-01-01", max(dataFBCFproduit$DATE))), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(-8, 10), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("France: Contribution à la croissance des FBCF des SNF par produits en volume, en %")

#si jamais ce graph est utilisé faut revoir l'échelle 

GraphFBCFparproduitContributionBis

Graph3FR <- ggplot() +
    geom_bar(data = dataFBCFproduit %>%
               filter(Produit %in% c("Autres", "Biens d'équipement", "Construction", "Matériels de transport", "Information-communication", "Services aux entreprises") &
                        TITLE_FR3 == "Volume aux prix de l'année précédente chaînés" &
                        DATE >= "2020-01-01"),
             aes(x = DATE, y = Contribution2019, fill = Produit),
             position = "stack", stat = "identity") +
    
    geom_point(data = dataFBCFproduit %>%
                 filter(Produit == "Total" &
                          TITLE_FR3 == "Volume aux prix de l'année précédente chaînés" &
                          DATE >= "2020-01-01"),
               aes(x = DATE, y = Contribution2019),
               color = "black", size = 3) + # Add black points for "Total"
    
    labs(
      title = "",
      x = "",
      y = "Contribution en pp"
    ) +
    
    theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
    theme(legend.position = "bottom") +  
    scale_x_date(date_labels = "%Y-%m-%d") +  # You can specify date limits if needed
    scale_y_continuous(limits = c(-20, 15), labels = scales::label_number(decimal.mark = ",")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.4)  # Add horizontal dashed line
  
Graph3FR
ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph3FR.svg", plot = Graph3FR, width = 8, height = 6, units = "in")


ggplot(dataFBCFproduit %>%
         filter(Produit == "Total" &
                  TITLE_FR3 == "Volume aux prix de l'année précédente chaînés"),
       aes(x = DATE, y = CroissanceAnnuelle)) +
  geom_line() +
  labs(
    caption = NULL,
    y = "% de la VA",  # Added label for the y-axis
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(-20, 30), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  scale_color_discrete()

#Regarder le taux d'investissement pour les SNF totales 

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
  mutate(Index = value / value[DATE == "2019-10-01"] * 100,
         Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100)

moyenne_tx_Invest <- dataTotal %>%
  filter(DATE >= as.Date("2012-01-01") & DATE <= as.Date("2019-10-01")) %>%
  filter(name == "Tx_Invest") %>%
  summarise(moyenne_tx_marge = mean(value, na.rm = TRUE))

moyenne_tx_marge <- dataTotal %>%
  filter(DATE >= as.Date("2012-01-01") & DATE <= as.Date("2019-10-01")) %>%
  filter(name == "Tx_Marge") %>%
  summarise(moyenne_tx_marge = mean(value, na.rm = TRUE))

moyenne_tx_marge2 <- dataTotal %>%
  filter(DATE >= as.Date("2021-01-01") & DATE <= as.Date("2024-04-01")) %>%
  filter(name == "Tx_Marge") %>%
  summarise(moyenne_tx_marge = mean(value, na.rm = TRUE))

moyenne_Autofinancement <- dataTotal %>%
  filter(DATE >= as.Date("2012-01-01") & DATE <= as.Date("2019-10-01")) %>%
  filter(name == "Autofinancement") %>%
  summarise(moyenne_Auto = mean(value, na.rm = TRUE))

ggplot(data=dataTotal %>%
         filter(name == "Autofinancement"),
       aes(x=DATE,y=value)) +
  geom_line(aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataTotal$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(70, 105), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  scale_color_discrete() 

ggplot(data=dataTotal %>%
         filter(name == "Tx_Invest"),
       aes(x=DATE,y=value)) +
  geom_line(aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataTotal$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(20, 24), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("France: Taux d'investissement des SNF (FCBF/VA)")+
  scale_color_discrete() 

ggplot(data=dataTotal %>%
         filter(name == "Tx_Marge"),
       aes(x=DATE,y=value)) +
  geom_line(aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataTotal$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(28, 37), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("France: Taux de marge des SNF (FCBF/VA)")+
  scale_color_discrete() 

ggplot(data=dataTotal %>%
        filter(name == "Excédent brut d'exploitation des sociétés non financières"),
      aes(x=DATE,y=value)) +
  geom_line(aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataTotal$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(75000, 130000), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("France: EBE des SNF")+
  scale_color_discrete() 

ggplot(data=dataVA %>%
         filter(TITLE_FR1 == "Valeur ajoutée des branches")%>%
       filter(name == "Marchand non agricole"),
       aes(x=DATE,y=value)) +
  geom_line(aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataVA$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(320000, 441939), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("France: Valeur ajoutée des SNF Marchand non agri")+
  scale_color_discrete() 

ggplot(data=dataVA %>%
         filter(TITLE_FR1 == "Valeur ajoutée des branches")%>%
         filter(name == "Marchand non agricole"),
       aes(x=DATE,y=Croissance)) +
  geom_line(aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataVA$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(-10, 15), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("France: Valeur ajoutée Marchand non agri")+
  scale_color_discrete() 


ggplot(data=dataTotal %>%
         filter(name == "Excédent brut d'exploitation des sociétés non financières"),
       aes(x=DATE,y=value)) +
  geom_line(aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataTotal$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(75000, 113826), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("Excédent brut d'exploitation des sociétés non financières")+
  scale_color_discrete() 


ggplot(data=dataEBE2 %>%
         filter(name == "Marchand non agricole"),
       aes(x=DATE,y=Index)) +
  geom_line(aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataTotal$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(85, 130), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("Excédent brut d'exploitation des sociétés non financières")+
  scale_color_discrete() 

# rajouter les prévisions pour le graphique

dataPrev <- data.frame(
  DATE = as.Date(c("2024-07-01", "2024-10-01", "2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01")),
  Tx_Marge = c(30.8,30.6,30.8,31.1,31.3, 31.3),
  Tx_Invest = c(22.75, 22.78,22.78, 22.78,22.8, 22.82))


dataTotal2<- dataTotal %>%
  filter(name %in% c("Tx_Invest", "Tx_Marge"))%>%
  pivot_wider(names_from = name, values_from = value)
  
  
dataPrev <- bind_rows(dataTotal2,dataPrev)%>%
  pivot_longer(cols = -c(DATE))%>%
  mutate(DATE = as.Date(DATE))

dataAlterEco <- dataPrev %>%
  pivot_wider(names_from = name, values_from = value)

file_path <- "C:/Users/153003/Documents/Entreprise/dataAlterEco.xlsx"

# Write the dataframe to an Excel file
write.xlsx(dataAlterEco, file_path)

  
library(ggplot2)

graph7<- ggplot(data = dataPrev) +
  geom_line(aes(x = DATE, y = value, color = name, linetype = ifelse(DATE < as.Date("2024-03-01"), "dotted", "solid")), show.legend = TRUE) +
  geom_vline(xintercept = as.numeric(as.Date("2024-03-01")), color = "grey", linetype = "solid", alpha = 0.25) +
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataTotal$DATE))), expand = c(0, 0)) +
  scale_y_continuous(
    name = "% de la VA",
    limits = c(20, 37),
    labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')
  ) +
  scale_color_discrete(labels = c("Tx_Invest" = "Taux d'investissement", "Tx_Marge" = "Taux de Marge")) +
  guides(linetype = FALSE) +
  annotate("rect", xmin = as.Date("2024-07-01"), xmax = as.Date("2026-01-01"), 
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray")+
ggtitle("Taux d'investissement et de marge des entreprises (en % de la VA)")

graph7

ggsave(filename = "C:/Users/153003/Documents/Entreprise/graph7.svg", plot = graph7, width = 8, height = 6, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Entreprise/graph7.png", plot = graph7, width = 8, height = 6, units = "in")
#Export


# ggtitle("Taux de marge et d'investissement des Sociétés non Financières")

print(Graph4FR)

ggsave(filename = "C:/Users/153003/Documents/Entreprise/graph1FR.svg", plot = graph1FR, width = 8, height = 6, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Entreprise/graph1FR.png", plot = graph1FR, width = 8, height = 6, units = "in")
#Export

wb <- createWorkbook()
addWorksheet(wb, "EBE")
addWorksheet(wb, "VA")
addWorksheet(wb, "Marge")
addWorksheet(wb, "FBCFagentvol")
addWorksheet(wb, "FBCFagentvaleur")
addWorksheet(wb, "FBCFproduitvaleur")
addWorksheet(wb, "FBCFproduitvolume")


writeData(wb, sheet = "EBE", dataEBE, startCol = 1, startRow = 1)
writeData(wb, sheet = "VA", dataVA, startCol = 1, startRow = 1)
writeData(wb, sheet = "Marge", dataRapports, startCol = 1, startRow = 1)
writeData(wb, sheet = "FBCFagentvol", dataFBCFagentvolume, startCol = 1, startRow = 1)
writeData(wb, sheet = "FBCFagentvaleur", dataFBCFagentvaleur, startCol = 1, startRow = 1)
writeData(wb, sheet = "FBCFproduitvaleur", dataFBCFproduit, startCol = 1, startRow = 1)
writeData(wb, sheet = "FBCFproduitvolume", dataFBCFproduit, startCol = 1, startRow = 1)

# Sauvegardez le fichier Excel
saveWorkbook(wb, "C:/Users/153003/Documents/Entreprise/DataEntreprise.xlsx", overwrite = TRUE)

#Enquete de conjoncture INSEE


Enquete <- get_insee_dataset("CLIMAT-AFFAIRES")%>%
  split_title() %>%
  filter(INDICATEUR == "CLIMAT_AFFAIRES")%>%
  select(DATE,OBS_VALUE,TITLE_FR1)%>%
  arrange(DATE)
 
ggplot(data = Enquete, aes(x = DATE, y = OBS_VALUE)) +
  geom_line() +
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(Enquete$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(60, 120), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("France: Indicateur du climat des affaires") +
  scale_color_discrete()









  
  


   
   