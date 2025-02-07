#API Banque de France
#clé : 7MyyvJc-4YAD-cr
#valeur confidentielle : a2ff8f02132cd59c97bc2d082dfa5806

install.packages("svglite")
                 
install.packages("ggsci")


library(rwebstat)
library(svglite)
library(ggsci)
library(openxlsx)

#MIR1: indentifiant du jeu de données, nom du dataset et ce qui suit c la clé 
#Crédit = Mensuelle, France, Etablissements de crédit et autres institutions financières, crédit, Toutes maturités, flux mensuels cumulés sur un an , Tous montants, SNF résidentes, euro, contrats nouveaux
#Endettement = Mensuel, France, Brut, Endettement, Total, Indices notionnels des stocks, Résidents, Sociétés non financières (S11), Toutes monnaies confondues, Taux de croissance annuel

webstat_client_ID <- "523de456-638c-4ac1-a31e-7c466d1329d0"

CreditTotal <- w_data(dataset_name = "MIR1", series_name = "MIR1.M.FR.B.A20.A.Y.A.2240U6.EUR.N")
CreditPlus1M <- w_data(dataset_name = "MIR1", series_name = "MIR1.M.FR.B.A20.A.Y.1.2240U6.EUR.N")
CreditMoins1M<- w_data(dataset_name = "MIR1", series_name = "MIR1.M.FR.B.A20.A.Y.0.2240U6.EUR.N")
CreditsTresorie <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.Y.R.A2N2ZZ.A.4.U6.2240.Z01.E")
CreditsFBCF <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.Y.R.A2N1ZZ.A.4.U6.2240.Z01.E")
Taux <- w_data(dataset_name = "MIR1", series_name = "MIR1.M.FR.B.A20.A.R.A.2240U6.EUR.N")


# Encours Total
Endettement <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.DEB.A.I.U6.2240.Z01.A")
EncoursTotal <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.A26.A.1.U6.2240.Z01.E")
EncoursTrésorie <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.A2N2Z.A.1.U6.2240.Z01.E")
EncoursInvestissement <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.A2N1Z.A.1.U6.2240.Z01.E")
EncoursInvestissementImmo <- w_data(dataset_name ="BSI1", series_name = "BSI1.M.FR.N.R.A2N1ZIM.A.1.U6.2240.Z01.E")
EncoursInvestissementEqui <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.A2N1ZQ.A.1.U6.2240.Z01.E")

#Encours de titres de dette émis par les sociétés non financières 
Titres <- w_data(dataset_name = "SC1", series_name =  "SC1.M.FR.1100.F33000.N.1.Z01.E.Z")
TitresLT <- w_data(dataset_name = "SC1", series_name = "SC1.M.FR.1100.F33200.N.1.EUR.E.Z")
TitresCT <- w_data(dataset_name = "SC1", series_name = "SC1.M.FR.1100.F33100.N.1.EUR.E.Z")

EncoursInvestissement <- inner_join(inner_join(EncoursInvestissementImmo, EncoursInvestissementEqui, by ="date"), EncoursInvestissement, by = "date")%>%
  rename( EncoursInvestissement = "BSI1.M.FR.N.R.A2N1Z.A.1.U6.2240.Z01.E",
          Immobilier = "BSI1.M.FR.N.R.A2N1ZIM.A.1.U6.2240.Z01.E",
          Equipement = "BSI1.M.FR.N.R.A2N1ZQ.A.1.U6.2240.Z01.E")

EncoursCredit <- inner_join(inner_join(EncoursTotal, EncoursTrésorie, by = "date"), EncoursInvestissement, by = "date")%>%
  rename(Total = "BSI1.M.FR.N.R.A26.A.1.U6.2240.Z01.E",
         Tresorie = "BSI1.M.FR.N.R.A2N2Z.A.1.U6.2240.Z01.E")%>%
filter(!(date >= "1993-01-01" & date <= "1993-03-01"))%>%
  mutate(across(c(Total, Immobilier, Equipement, Tresorie), as.numeric),
         Autre = Total - Immobilier - Equipement - Tresorie)%>%
pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
  group_by(Variable)%>%
mutate( Croissance = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100)%>%
group_by(date)%>%
mutate(Part = (Valeur / Valeur[Variable=="Total"])*100)%>%
ungroup()%>%
group_by(Variable)%>%
  mutate(Croissance = (Valeur- lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100,
         GFC = ((Valeur[date == "2009-12-01"] - Valeur[date == "2008-09-01"])/ Valeur[date == "2008-09-01"])*100,
         Covid = ((Valeur[date == "2021-03-01"] - Valeur[date == "2020-02-01"])/ Valeur[date == "2020-02-01"])*100,
         PostCovid = ((Valeur[date == "2023-12-01"] - Valeur[date == "2021-04-01"])/ Valeur[date == "2021-04-01"])*100,
         Contribution = Croissance * lag(Part, n = 12) /100,
         ContributionGFC = GFC * Part[date == "2008-09-01"]/100,
         ContributionCovid = Covid * Part[date == "2020-02-01"]/100,
         ContributionPC = PostCovid * Part[date == "2021-04-01"]/100)


EncoursInvestissement<- EncoursInvestissement%>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Valeur")%>%
  filter(!(date >= "1993-01-01" & date <= "1993-04-01"))%>%
  group_by(Variable)%>%
  mutate( Croissance = (Valeur - lag(Valeur, n = 12)) / lag(Valeur, n = 12) * 100)%>%
  group_by(date)%>%
  mutate(Part = (Valeur / Valeur[Variable=="EncoursInvestissement"])*100)


Endettement1 <- inner_join(inner_join(EncoursInvestissement, TitresCT, by = "date"), TitresLT, by = "date")%>%
  rename(
         TitresCT = "SC1.M.FR.1100.F33100.N.1.EUR.E.Z",
         TitresLT = "SC1.M.FR.1100.F33200.N.1.EUR.E.Z")


# Endettement$date <- as.Date(Endettement$date)
ordre_personnalise <- c("GCF", "Covid", "PostCovid")

EndettementGraph <- EncoursCredit %>%
  filter(date == "2023-12-01")  %>%
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


moyenne_croissance <- EncoursCredit %>%
  filter(date >= "2013-01-01" & date <= "2019-01-01" & Variable == "Total") %>%
  summarise(moyenne_croissance = mean(Croissance, na.rm = TRUE))

total_treso <- EncoursCredit %>%
  filter(date >= "2020-06-01" & date <= "2021-12-01" & Variable == "EncoursTresorie") %>%
  summarise(total_treso = sum(Valeur, na.rm = TRUE))

EncoursCredit$date <- as.Date(EncoursCredit$date)
# La variable moyenne_croissance contiendra la moyenne des valeurs de Croissance où Variable est "Total"
GraphEndettement<- ggplot(aes(x = date, y = Contribution), data = EncoursCredit) +
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
  scale_y_continuous(limits = c(-3, 13), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  #scale_fill_manual(values = c("#6A51A3","#66A61E", "#E7298A")) +
  ggtitle("Variation de l'endettement des Sociétés Non Financières") +
  guides(fill = guide_legend(title = NULL))


ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphEndettement.png", plot = GraphEndettement, width = 16, height = 8, units = "in")


EndettementGraph1 <- EndettementGraph%>% 
  filter(Variable1 != "GCF")

GraphEndettementbis<- ggplot(data = EndettementGraph1) +
  geom_bar(data = EndettementGraph1 %>% 
             filter(!Variable %in% c("Immobilier", "Equipement", "Total")), 
           aes(x = Variable1, y = Contribution, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_point(data = EndettementGraph1 %>% filter(Variable == "Total"), 
             aes(x = Variable1, y = Contribution), 
             color = "black", size = 4.5) + 
  geom_text(data = EndettementGraph1 %>% filter(Variable == "Total"), 
            aes(x = Variable1, y = Contribution, label = signif(Contribution, 2)), 
            size = 2, color = "white") +
  labs(
    caption = "Source: Banque de France
              Note:  la période Covid est celle comprise entre février 2020 et mars 2021
              Enfin la période post Covid correspond aux mois entre celui d'avril 2021 
    et la dernière donnée disponible en décembre 2023.",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-4, 13), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("#DADAEB", "#6A51A3","#9E9AC8", "#E7298A", "#66A61E")) +
  ggtitle("Variation de l'endettement des Sociétés Non Financières") +
  guides(fill = guide_legend(title = NULL))


EndettementGraph2 <- Endettement %>%
  filter(date %in% c("2020-01-01", "2021-01-01", "2022-01-01", "2023-12-01")) %>%
  select(Contribution, date) %>%
  mutate(date = replace(date, date == "2023-12-01", "2023-01-01"))

GraphEndettementTer<- ggplot(data = EndettementGraph2) +
  geom_bar(data = EndettementGraph2 %>% 
             filter(!Variable %in% c("Immobilier", "Equipement", "Total")), 
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
              Note: Variation de janvier en janvier sauf pour 2023 janvier - décembre.",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-2, 14.5), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("#DADAEB", "#6A51A3","#9E9AC8", "#E7298A", "#66A61E")) +
  ggtitle("Variation de l'endettement des SNF") +
  guides(fill = guide_legend(title = NULL))

GraphEndettementTer<- ggplot(data = EndettementGraph2) +
  geom_bar(data = EndettementGraph2 %>% 
             filter(!Variable %in% c("Immobilier", "Equipement", "Total")), 
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
              Note: Variation de janvier en janvier sauf pour 2023 janvier - décembre.",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-2, 14.5), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("#DADAEB", "#6A51A3","#9E9AC8", "#E7298A", "#66A61E")) +
  ggtitle("Variation de l'endettement des SNF") +
  guides(fill = guide_legend(title = NULL))

 GraphCredit1<- ggplot(data = EncoursCredit %>%
                         mutate(date = as.Date(date)) %>%
                         filter(!Variable %in% c("Tresorie", "Autres"))) +
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
   scale_y_continuous(limits = c(-1, 14), labels = scales::label_number(decimal.mark = ",")) +
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



GraphCredit3<- ggplot(data=EncoursCredit2) +
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

  
GraphCredit <- ggplot(data = EncoursCredit %>%
                        mutate(date = as.Date(date)) %>%
                        filter(!Variable %in% c("EncoursInvestissement", "Total")), 
                      aes(x = date, y = Valeur, fill = Variable)) +
  geom_bar(position = "stack", stat = "identity") +  
  labs(
    title = "Contributions to Growth",
    x = "Date",
    y = "En Millions euros"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2013-02-01", "2023-12-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, 1400000), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  ggtitle("Encours de crédit des Sociétés Non Financières")


ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphCredit.png", plot = GraphCredit, width = 8, height = 6, units = "in")


#Taux de Croissance des crédits accordés aux entreprises

CroissanceCreditTotal <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.A26.A.I.U6.2240.Z01.A")
CroissanceCreditTreso <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.A2N2Z.A.I.U6.2240.Z01.A")
CroissanceCreditInvestissement<- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.A2N1Z.A.I.U6.2240.Z01.A")
CroissanceCreditAutre <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.R.A2N3Z.A.I.U6.2240.Z01.A")

TauxCreditTotal <- w_data(dataset_name = "MIR1", series_name = "MIR1.M.FR.B.A20.A.R.A.2240U6.EUR.N")

DataCredit <- inner_join(CroissanceCreditTotal, TauxCreditTotal, by = "date")%>%
  rename(CroissanceCreditTotal = "BSI1.M.FR.N.R.A26.A.I.U6.2240.Z01.A",
          TauxCreditTotal = "MIR1.M.FR.B.A20.A.R.A.2240U6.EUR.N")


CroissanceTaux <- ggplot(data = DataCredit) +
  geom_line(aes(x = as.Date(date), y = CroissanceCreditTotal, color = "Crédits accordés aux sociétés non financières résidentes"), show.legend = TRUE) +
  geom_line(aes(x = as.Date(date), y = TauxCreditTotal, color = "Crédits nouveaux aux SNF, taux d'intérêt annuel"), show.legend = TRUE) +
  labs(
    caption = "",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.5, 13.5), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL,
                     values = c("Crédits accordés aux sociétés non financières résidentes" = "red",
                                "Crédits nouveaux aux SNF, taux d'intérêt annuel" = "blue"),
                     labels = c("Taux de croissance annuelle des crédits",
                                "Taux d'intérêt annuel des nouveaux crédits"))

ggplot(data = EncoursCredit %>%
         filter(Variable == c("EncoursInvestissement", "Immobilier", "Investissement"))) +
  geom_line(aes(x = as.Date(date), y = Valeur, color = "Crédits accordés aux sociétés non financières résidentes"), show.legend = TRUE) +
  #geom_line(aes(x = as.Date(date), y = TauxCreditTotal, color = "Crédits nouveaux aux SNF, taux d'intérêt annuel"), show.legend = TRUE) +
  labs(
    caption = "",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(DataCredit$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1000000), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL)

CroissanceTaux2<- ggplot(data = DataCredit %>%
         filter(date >= "2019-01-01")) +
  geom_point(aes(x = CroissanceCreditTotal, y = TauxCreditTotal, color = as.factor(year(date))),
                                                 size = 3) +
  labs(
    caption = "",
    x = "Croissance annuelle des crédits",
    y = "Taux d'intérêt annuel des nouveaux crédits",
    color = ""
  ) +
  scale_color_manual(values = c("2019" = "#98F5FF", "2020" = "#53868B", 
                                "2021" = "#8EE5EE", "2022" = "#7AC5CD", "2023" = "#2F4F4F")) + 
  theme_minimal() +
  geom_text(data = DataCredit %>% filter(year(date) == 2023),
            aes(label = format(date, "%m"), x = CroissanceCreditTotal, y = TauxCreditTotal),
            vjust = -1, size = 2.5) +
  geom_path(aes(x = CroissanceCreditTotal, y = TauxCreditTotal),
            color = "gray") +
  ggtitle("Relation entre la croissance crédits et les taux d'intérêt")


ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph6FR.svg", plot = Graph6FR, width = 8, height = 6, units = "in")
