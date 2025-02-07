install.packages(c("stargazer","ggthemes", "eurostat","ofce","parallel","lubridate","ggplot2","plotrix","Hmisc","readxl","httr","data.table","insee", "writexl","tidyverse","zoo"))
install.packages("devtools", "writexl", "Rtools","inseeLocalData","ggsci")
devtools::install_github("OFCE/ofce")
devtools::install_github("oddworldng/INEbaseR")

library(dplyr)
library(eurostat)
library(ggthemes)
library(scales)
library(ofce)
library(tidyverse)
library(stargazer)
library(ofce)
library(parallel)
library(lubridate)
library(ggplot2)
library(plotrix)
library(Hmisc)
library(readxl)
library(writexl)
library(httr)
library(data.table)
library(insee)
library(tidyverse)
library(zoo)                                                                                                                                                                                                                                                                                       
library(magrittr)
library(openxlsx)


#Paramètres séries à garder : Excédent Brut d'Exploitation et VA par produit trimestrielle

source<-"CNT-2014-CB"


bases<-get_dataset_list()

idbank_list <- get_idbank_list(source)




corr<-"CVS-CJO"
flux<-c("SMNA-CNT","DSM-CNT","DSN-CNT", "DI-CNT","A17-FZ","A17-AZ","D-CNT","A17-C1", "A17-C2", "A17-C3", "A17-C4","A17-C5", "A17-DE","A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU")
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
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_")

corr<-"CVS-CJO"
flux<-c("SMNA-CNT","DSM-CNT","DSN-CNT", "DI-CNT","A17-FZ","A17-AZ","D-CNT","A17-C1", "A17-C2", "A17-C3", "A17-C4","A17-C5", "A17-DE","A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU")
variable<-"B2"

testCommerce<-idbank_list%>%
  filter(CORRECTION==corr,
         CNA_PRODUIT %in% flux,
         OPERATION %in% variable)%>%
  select(idbank)%>%
  pull(idbank)


dataEBE =
  get_insee_idbank(testCommerce) %>%
  split_title()%>%
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,TITLE_FR2,OPERATION_label_fr)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_")


datamarge <- bind_rows(dataVA,dataEBE) %>%
  pivot_longer(cols = -c(DATE, OPERATION_label_fr)) %>%
  group_by(DATE, name) %>%
  arrange(DATE) 

marge_rows <- datamarge %>%
  group_by(DATE, name) %>%
  summarise(value = value[OPERATION_label_fr == "B2 - Excédent d'exploitation"] / 
              value[OPERATION_label_fr == "B1 - Valeur ajoutée" ]) %>%
  mutate(OPERATION_label_fr= "Marge")%>%
ungroup() %>%
  filter(!is.na(value))

datamarge <- bind_rows(datamarge, marge_rows)%>%
  group_by(OPERATION_label_fr, name) %>%
  mutate(Index = value / value[DATE == "2019-10-01"] * 100,
         Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100)



GraphMarge <-ggplot(data = datamarge) +
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
  scale_y_continuous(limits = c(60, 160), oob = scales::squish, labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("France: Marge des SNF en % de la VA")


dataVA <- dataVA%>%
pivot_longer(cols = -c(DATE, OPERATION_label_fr))
  
dataVA <- dataVA%>%
mutate(Index = value / value[DATE == "2019-10-01"] * 100,
       Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100)%>%
group_by(DATE)%>%
mutate(Part = ifelse(any(name == "Total Branches"), value / value[name == "Total Branches"] * 100, NA))

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
  scale_x_date(limits = as.Date(c("2011-01-01", max(datamarge$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(60, 125), oob = scales::squish, labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("France: Valeur ajoutée des SNF par branches, (Index T4:2019 = 100")

  


GraphFBCFmargeIndex<- ggplot(data = dataFBCFmarge) +
  geom_line(data = . %>% filter(name %in% "Industries agro-alimentaires","Commerce", "Construction", "Information-communication","Hébergement-restauration","Services principalement marchands","Industrie"),
  aes(x = DATE, y = Index, color = name), show.legend = TRUE)+
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit.raul$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(60, 160),oob = scales::squish, labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: Marge des SNF en Indice T42019=100")

print(GraphFBCFmarge1)
ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFmargeIndex.png", plot = GraphFBCFmargeIndex, width = 8, height = 6, units = "in")

GraphFBCFmarge1 <- ggplot(data=data.marg2) +
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
  scale_y_continuous(limits = c(-30, 1700), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: Marge des SNF en Indice T42019=100")

print(GraphFBCFmarge1)

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFmarge1.png", plot = GraphFBCFmarge1, width = 8, height = 6, units = "in")



#Paramètres séries à garder : FBCF par agent trimestrielle volume/ valeur

source<-"CNT-2014-PIB-EQB-RF"

bases<-get_dataset_list()

idbank_list <- get_idbank_list(source)


corr<-"CVS-CJO"
flux<-"Valeur absolue"
variable<- c("P51","P51G","P51M","P51P","P51S")
valo <- c("Volumes aux prix de l'année précédente chaînés", "Valeurs aux prix courants")


testCommerce<-idbank_list%>%
  filter(CORRECTION==corr,
         VALORISATION_label_fr %in%valo,
         NATURE_label_fr==flux,
         OPERATION %in% variable,
  )%>%
  select(idbank)%>%
  pull(idbank)


dataFBCFagent =
  get_insee_idbank(testCommerce) %>%
  split_title()%>%
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,TITLE_FR1,TITLE_FR3)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, names_sep = "_")


dataFBCFagent <- dataFBCFagent%>%
  pivot_longer(cols=-c(DATE,TITLE_FR3))%>%
  group_by(TITLE_FR3,name)%>%
  arrange(DATE)%>%
  mutate(Index=value/value[DATE=="2019-10-01"]*100,
         Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100,
         Cumule= Index[DATE=="2023-04-01"]- 100) %>%
  ungroup()%>%
  group_by(DATE,TITLE_FR3)%>%
  mutate(Part= value/value[name=="FBCF de l'ensemble des secteurs institutionnels"]*100)%>%
  ungroup()%>%
  group_by(TITLE_FR3,name)%>%
  mutate(Contribution = Croissance * lag(Part, n = 4) /100)%>%
  ungroup()

filtre.agent <-"FBCF de l'ensemble des secteurs institutionnels"
filtre.prix2<-"Volume aux prix de l'année précédente chaînés"

data.ag1<-dataFBCFagent%>%
  filter(TITLE_FR3 %in% filtre.prix1)

data.ag3<-dataFBCFagent%>%
  filter(TITLE_FR3 %in% filtre.prix2)%>%
  filter(!name %in% filtre.agent)


GraphFBCFparagent<- ggplot(data = dataFBCFagent %>% filter(TITLE_FR3 == "Valeur aux prix courants")) +
  geom_line(aes(x = DATE, y = Index, color = name), show.legend = TRUE)+
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFagent$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(70, 110), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par agent en volume en Indice T42019=100")+
  guides(color = guide_legend(nrow=2))

view(GraphFBCFparagent)

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparagent.png", plot = GraphFBCFparagent, width = 8, height = 6, units = "in")

GraphFBCFparagentGA <- ggplot(data=data.ag2) +
  geom_line(aes(x = DATE, y = Croissance, color = name), show.legend = TRUE)+
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit.raul$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-25, 50), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par produits en glissement annuel en volume, en %")


ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparagentGA.png", plot = GraphFBCFparagentGA, width = 8, height = 6, units = "in")


GraphFBCFparagentContribution <- ggplot(data=data.ag3, aes(x = DATE, y = Contribution, fill = name)) +
  geom_bar(position = "stack", stat = "identity") +  # Create a stacked bar chart
  labs(
    title = "Contributions to Growth",
    x = "Date",
    y = "Contribution (%)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2019-04-01", "2023-04-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(-10, 15), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("France: Contribution à la croissance des FBCF par agent en volume, en %")

print(GraphFBCFparagentContribution)

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparagentContribution.png", plot = GraphFBCFparagentContribution, width = 8, height = 6, units = "in")


#Paramètres séries à garder : FCBF par produit pour entreprises non financières trimestrielle

source<-"CNT-2014-OPERATIONS"

bases<-get_dataset_list()

idbank_list <- get_idbank_list(source)


corr<-"CVS-CJO"
flux<-c("A17-FZ","A17-AZ","D-CNT","A17-C1", "A17-C2", "A17-C3", "A17-C4","A17-C5", "A17-DE","A17-GZ", "A17-HZ", "A17-IZ", "A17-JZ", "A17-KZ", "A17-LZ", "A17-MN", "A17-OQ", "A17-RU")
variable<-c("P51S")
valo <- c("Volumes aux prix de l'année précédente chaînés", "Valeurs aux prix courants")


testCommerce<-idbank_list%>%
  filter(CORRECTION==corr,
         CNA_PRODUIT %in% flux,
         VALORISATION_label_fr %in% valo,
         OPERATION %in% variable,
  )%>%
  select(idbank)%>%
  pull(idbank)


 dataFBCFproduit =
  get_insee_idbank(testCommerce) %>%
  split_title()%>%
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,TITLE_FR2,TITLE_FR3)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, names_sep = "_")%>%
  mutate(Autres = `Services aux ménages` + `Produits agricoles`+ `Autres produits industriels`+`Services immobiliers`,
         Total = `Services aux ménages` + `Produits agricoles`+ `Autres produits industriels`+`Services immobiliers`+ `Biens d'équipement`+`Construction`+`Matériels de transport`+`Information-communication`+`Services aux entreprises`,
         Tx_Investissement = (Total/ dataVA$`Total branches`)*100)


dataFBCFproduit.raul <- dataFBCFproduit%>%
  pivot_longer(cols=-c(DATE,TITLE_FR3))%>%
  group_by(TITLE_FR3,name)%>%
  arrange(DATE)%>%
  mutate(Index=value/value[DATE=="2019-10-01"]*100,
         Croissance = (value - lag(value, n = 4)) / lag(value, n = 4) * 100,
         Croissance2019 = (value - value[DATE== "2019-10-01"]) /  value[DATE== "2019-10-01"] * 100) %>%
  ungroup()%>%
  group_by(DATE,TITLE_FR3)%>%
  mutate(Part= (value/ value[name == "Total"]*100))%>%
ungroup()%>%
group_by(TITLE_FR3,name)%>%
  mutate(Contribution = Croissance * lag(Part, n = 4) /100,
         Contribution2019 = Croissance2019 * (Part[DATE=="2019-10-01"])/100)%>%
  ungroup()

filtre.produit<-c( "Autres", "Biens d'équipement","Construction","Matériels de transport","Information-communication","Services aux entreprises")
filtre.prix1<-"Valeur aux prix courants"
filtre.prix2<-"Volume aux prix de l'année précédente chaînés"
filtre.total <- "Total"

data.gr1<-dataFBCFproduit.raul%>%
  filter(name %in% filtre.produit)%>%
  filter(TITLE_FR3 %in% filtre.prix1)%>%
rename(Produit=name)

data.gr2<-dataFBCFproduit.raul%>%
  filter(name %in% filtre.produit)%>%
  filter(TITLE_FR3 %in% filtre.prix2)%>%
rename(Produit=name)

data.gr3<-dataFBCFproduit.raul%>%
  filter(name %in% filtre.total)%>%
  filter(TITLE_FR3 %in% filtre.prix2)%>%
  rename(Produit=name)


GraphFBCFparproduit<- ggplot(data=data.gr2) +
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
  scale_y_continuous(limits = c(50, 130), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par produits en volume en Indice T42019=100")

print(GraphFBCFparproduit)
ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduit.png", plot = GraphFBCFparproduit, width = 8, height = 6, units = "in")
 
GraphFBCFparproduitGA <- ggplot(data=data.gr2) +
  geom_line(aes(x = DATE, y = Croissance, color = name), show.legend = TRUE)+
  labs(
    caption = "Source: Insee.",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit.raul$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-30, 60), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par produits en glissement annuel en volume, en %")

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduitGA.png", plot = GraphFBCFparproduitGA, width = 8, height = 6, units = "in")


GraphFBCFparproduitContribution <- ggplot(data=data.gr2, aes(x = DATE, y = Contribution, fill = Produit)) +
  geom_bar(position = "stack", stat = "identity") +  # Create a stacked bar chart
  labs(
    title = "Contributions to Growth",
    x = "Date",
    y = "Contribution (%)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2019-01-01", "2023-04-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(-10, 20), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("France: Contribution à la croissance des FBCF des SNF par produits en volume, en %")

ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduitContribution.png", plot = GraphFBCFparproduitContribution, width = 8, height = 6, units = "in")


Graph3FR <- ggplot() +
  geom_bar(data=data.gr2 |> filter(DATE>="2020-01-01"), aes(x = DATE, y = Contribution2019, fill = Produit),position = "stack", stat = "identity") +  
  geom_point(data = data.gr3 |> filter(DATE>="2020-01-01"), aes(x = DATE, y = Contribution2019), color = "black", size = 3) + # Create a stacked bar chart
  labs(
    title = "",
    x = "",
    y = "Contribution en pp"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_x_date( date_labels = "%Y-%m-%d", limits = ) +
  scale_y_continuous(limits = c(-20, 15), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  # ggtitle("Contribution à la croissance totale des FBCF des SNF (par rapport au T4:2019)")+
geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.4)  # Ajoutez la ligne horizontale ici


print(Graph3FR)
ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph3FR.svg", plot = Graph3FR, width = 8, height = 6, units = "in")

#Reagrder le taux d'investissement pour les SNF totales 

source <- "CNT-2014-CSI"
bases <- get_dataset_list()
idbank_list <- get_idbank_list(source)
  
corr<-"CVS-CJO"
flux<-"S11"
variable<-c("B1","P51","B2","B9NF","B8G")


testCommerce<-idbank_list%>%
  filter(CORRECTION==corr,
         SECT_INST == flux,
         OPERATION %in% variable,
  )%>%
  select(idbank)%>%
  pull(idbank)


dataTotal =
  get_insee_idbank(testCommerce) %>%
  split_title()%>%
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,TITLE_FR1)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, names_sep = "_")%>%
  mutate(Tx_Invest = (`Formation brute de capital fixe des sociétés non financières`/ `Valeur ajoutée des sociétés non financières`)*100,
         Tx_Marge = (`Excédent brut d'exploitation des sociétés non financières`/ `Valeur ajoutée des sociétés non financières`)*100,
         Autofinancement = (`Épargne des sociétés non financières`/`Formation brute de capital fixe des sociétés non financières`)*100)%>%
  pivot_longer(-c(DATE))

moyenne_tx_marge <- dataTotal %>%
  filter(DATE >= as.Date("2012-01-01") & DATE <= as.Date("2019-12-31")) %>%
  summarise(moyenne_tx_marge = mean(Tx_Marge, na.rm = TRUE))


filtre<-c("Tx_Invest","Tx_Marge","Autofinancement")


data.T1<-dataTotal%>%
  filter(name %in% filtre)

# rajouter les prévisions pour le graphique

dates_existantes <- dataTotal$DATE
nouvelles_dates <- as.Date(c("2023-07-01", "2023-10-01", "2024-01-01", "2024-04-01", "2024-07-01", "2024-10-01"))
toutes_dates <- c(dates_existantes, nouvelles_dates)
dataPrev <- data.frame(
  DATE = toutes_dates,
  Tx_Marge = c(dataTotal$Tx_Marge, 32.6, 32.7, 32.8, 32.8, 32.9, 32.8),
  Tx_Invest = c( dataTotal$Tx_Invest, 25.57800873,25.45037396, 25.27257694,25.09602201, 24.89625268, 24.79686644))

dataPrev <- dataPrev%>%
  pivot_longer(-c(DATE))

Graph4FR <- ggplot(data=dataPrev, aes(x=DATE,y=value)) +
  # geom_rect(aes(xmin=as.Date("2023-07-01"),
            #     xmax=as.Date("2024-12-01"),
            #     ymin=-Inf,
            #     ymax=Inf),
            # alpha=0.1,
            # fill="grey") +
  geom_line(aes(color= name),linetype=2)+
  geom_line(data=subset(dataPrev,DATE<="2023-07-01"),aes(color= name))+
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCFproduit.raul$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(22, 37), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  scale_color_discrete() 
# ggtitle("Taux de marge et d'investissement des Sociétés non Financières")

print(Graph4FR)

ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph4FR.svg", plot = Graph4FR, width = 8, height = 6, units = "in")

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

#parties graphique pour la prévision








  
  


   
   