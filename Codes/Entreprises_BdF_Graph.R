# ================= Graphiques entreprises - données BdF ===================== #
# Encours, crédit, faillites
# ======================================================================== #

source("Entreprises_BdF_Data.R")


# 1. Croissance moyenne de l'encours -------------------------------------------

moyenne_croissance <- EncoursCredit %>%
  filter(date >= "2013-01-01" & date <= "2019-01-01" & Variable == "Total") %>%
  summarise(moyenne_croissance = mean(Croissance, na.rm = TRUE))

total_treso <- EncoursCredit %>%
  filter(date >= "2020-06-01" & date <= "2021-12-01" & Variable == "EncoursTresorerie") %>%
  summarise(total_treso = sum(Valeur, na.rm = TRUE))

date_debut <- as.Date("2020-05-01")
date_fin <- as.Date("2021-02-01")
# Filtrer les données et calculer la moyenne de la colonne "Croissance"
moyenne_croissance <- mean(
  subset(EncoursCredit, 
         Variable == "Total" & date >= date_debut & date <= date_fin)$Croissance,
  na.rm = TRUE
)
cat("La moyenne de la croissance entre", date_debut, "et", date_fin, "est :", moyenne_croissance, "\n")


# 2. Série temporelle de l'encours -------------------------------------------

## valeur -----------------------------------------------------
ggplot() +
  geom_line(data = EncoursCredit %>% filter(Variable == "Total"),
            aes(x = as.Date(date), y = Valeur),
            color = "black") + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = "en millions d'euros",
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  scale_y_continuous(limits = c(600000, 1500000), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2011-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Encours total des crédits bancaires des SNF")
  
  

## indice base 2019=100 -----------------------------------------------------
ggplot() +
  geom_line(data = EncoursCredit %>% filter( Variable == "Total"),
            aes(x = as.Date(date), y = Index),
            color = "black") + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = "indice base 2019=100",
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  scale_y_continuous(limits = c(80, 140), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Encours total des crédits bancaires des SNF")


# 3. Série temporelle de l'encours des crédits de trésorerie -------------------------------------------

## indice base 2019=100 -----------------------------------------------------
ggplot() +
  geom_line(data = EncoursCredit %>% filter( Variable == "Tresorerie"),
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

## croissance -----------------------------------------------------
ggplot() +
  geom_line(data = EncoursCredit %>% filter( Variable == "Tresorerie"),
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




# 4. Contribution à la croissance de l'encours par objet -------------------------------------------

## 4.1. Contribution à la croissance mensuelle -----------------------------------------------------

# en barres
GraphContribEncours <- ggplot(data = EncoursCredit, aes(x = as.Date(date), y = Contribution)) +
  geom_bar(data = EncoursCredit %>% filter(!Variable %in% c("Total", "EncoursInvestissement")),
           aes(fill = Variable), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(EncoursCredit, Variable == "Total"),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: Banque de France",
    title = "",
    x = "",
    y = "Contribution à la croissance de l'encours total (en pp)",
    fill = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-5, 15), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2011-01-01", max(EncoursCredit$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Contribution à la croissance mensuelle de l'encours total")
  
GraphContribEncours

# avec des lignes (- lisible)
GraphCroissanceEncours <- ggplot(aes(x = as.Date(date), y = Croissance, color = Variable), data = EncoursCredit) +
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

GraphCroissanceEncours

## 4.2. Contribution à la croissance annuelle -----------------------------------------------------

EndettementGraphData2 <- EncoursCredit %>%
  filter(date %in% c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")) %>%
  select(Contribution, date) 

CroissanceAn<- ggplot(data = EndettementGraphData2) +
  geom_bar(data = EndettementGraphData2 %>% 
             filter(!Variable %in% c("Total", "EncoursInvestissement")), 
           aes(x = date, y = Contribution, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_point(data = EndettementGraphData2 %>% filter(Variable == "Total"), 
             aes(x = date, y = Contribution), 
             color = "black", size = 4.5) + 
  geom_text(data = EndettementGraphData2 %>% filter(Variable == "Total"), 
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

CroissanceAn


# 5. Endettement lors des crises -------------------------------------------

ordre_personnalise <- c("GCF", "Covid", "PostCovid", "Normal")
EndettementGraphData <- EncoursCredit %>%
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

CroissanceCrises <- ggplot(data = EndettementGraphData %>% filter(Variable1 != "GCF")) +
  geom_bar(aes(x = Variable1, y = Contribution, fill = Variable), 
           data = EndettementGraphData %>% filter(Variable1 != "GCF", !Variable %in% c("Total")), 
           position = "stack", stat = "identity") +  
  geom_point(aes(x = Variable1, y = Contribution), 
             data = EndettementGraphData %>% filter(Variable1 != "GCF", Variable == "Total"), 
             color = "black", size = 4.5) + 
  geom_text(aes(x = Variable1, y = Contribution, label = signif(Contribution, 2)), 
            data = EndettementGraphData %>% filter(Variable1 != "GCF", Variable == "Total"), 
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


# 6. Encours de crédit -------------------------------------------

## % croissance -----------------------------------------------------
GraphCredit1<- ggplot(data = EncoursCredit %>%
                        mutate(date = as.Date(date)) %>%
                        filter(!Variable %in% c("Equipement","Immobilier"))) +
  geom_line(aes(x = date, y = CroissanceAnnuelle, color = Variable), show.legend = TRUE) +
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

GraphCredit1

GraphCredit2<- ggplot(data = EncoursCredit %>%
                        mutate(date = as.Date(date)), 
                      aes(x = date, y = CroissanceAnnuelle, color = Variable)) +
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

GraphCredit2

## millions d'€ -----------------------------------------------------
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

GraphCredit3

## trésorie, millions d'€ -----------------------------------------------------
ggplot(data = EncoursCredit %>%
         filter(Variable =="Tresorerie") %>% 
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




# 7. Sources d'endettement CT/LT -------------------------------------------

## 7.1. Contributions à la croissance ---------------------------

# à partir de 2015
SourceEndettementContribution <- ggplot(data = EndettementSource) +
  geom_bar(data = EndettementSource %>% 
             filter(Variable %in% c("TitresCT", "TitresLT", "Credit")), 
           aes(x = as.Date(date), y = ContribAnnuelle, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_line(data = EndettementSource %>% 
              filter(Variable =="Total"),
            aes(x = as.Date(date), y = ContribAnnuelle),
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

SourceEndettementContribution

# à partir de 2018
SourceEndettementContribution2 <- ggplot(data = EndettementSource) +
  geom_bar(data = EndettementSource %>% 
             filter(Variable %in% c("TitresCT", "TitresLT", "Credit")), 
           aes(x = as.Date(date), y = ContribAnnuelle, fill = Variable), 
           position = "stack", stat = "identity") +  
  geom_line(data = EndettementSource %>% 
              filter(Variable =="Total"),
            aes(x = as.Date(date), y = ContribAnnuelle),
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

SourceEndettementContribution2

# au T3 2023
SourceEndettementContribution2023 <- ggplot(data = EndettementSource %>% 
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

SourceEndettementContribution2023


## 7.2. Contribution en millions ------------------------------

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

SourceEndettementMillion

## 7.3. Part des titres ---------------------------

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

PartTitre


# 8. Croissance des taux -------------------------------------------

## 8.1. Croissance des crédits ---------------------------

ggplot(data = FinancementCreditMarche) +
  geom_line(aes(x = as.Date(date), y = CroissanceCredit, color = "Taux de croissance annuelle des crédits")) +
  geom_line(aes(x = as.Date(date), y = CoutCredit, color = "Taux d'intérêt annuel des nouveaux crédits")) +
  labs(
    caption = "Source: Banque de France",
    y = "en %",
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2019-01-01", max(FinancementCreditMarche$date))), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(
    values = c("Taux de croissance annuelle des crédits" = "blue", 
               "Taux d'intérêt annuel des nouveaux crédits" = "red")) +
  ggtitle("Croissance annuelle et coût des crédits bancaires")


## 8.2. Croissance des titres de dette ---------------------------

ggplot(data = FinancementCreditMarche) +
  geom_line(aes(x = as.Date(date), y = CroissanceTitres, color = "Taux de croissance annuelle des titres de dette émis par les SNF")) +
  geom_line(aes(x = as.Date(date), y = CoutTitreDette, color = "Taux d'intérêt annuel")) +
  labs(
    caption = "Source: Banque de France",
    y = "en %",
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2019-01-01", max(FinancementCreditMarche$date))), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(
    values = c("Taux de croissance annuelle des titres de dette émis par les SNF" = "blue", 
               "Taux d'intérêt annuel" = "red")) +
  ggtitle("Croissance annuelle et coût des titres de dette émis par les SNF")



## 8.3. Comparaison des 2 ---------------------------

CroissanceEncoursTitre<- ggplot(data = FinancementCreditMarche) +
  geom_line(aes(x = as.Date(date), y = CroissanceCredit , color = "Crédits accordés aux SNF résidentes, croissance annuelle")) +
  geom_line(aes(x = as.Date(date), y = CroissanceTitres, color = "Titres de dette émis par les SNF, croissance annuelle des encours")) +
  labs(
    caption = "Source: Banque de France",
    y = "en %",
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2019-01-01", max(FinancementCreditMarche$date))), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Financement des SNF")

CroissanceEncoursTitre

## 8.4. Spread ---------------------------

SpreadTaux <- ggplot(data = FinancementCreditMarche) +
  geom_line(aes(x = as.Date(date), y = CoutCredit, color = "Taux d'intérêt des nouveaux crédits")) +
  geom_line(aes(x = as.Date(date), y = CoutTitreDette, color = "Taux d'intérêt annuel des titres de dettes")) +
  labs(
    caption = "Source: Banque de France",
    y = "en %",
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2019-01-01", max(FinancementCreditMarche$date))), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Spread de taux de financement des SNF")

SpreadTaux

## 8.4. Différences de taux et de croissance ---------------------------

CorrelationVariation<- ggplot(data = FinancementCreditMarche) +
  geom_line(aes(x = as.Date(date), y = SpreadTaux, color = " différence des taux (crédit - titre)")) +
  geom_line(aes(x = as.Date(date), y = SpreadCroissance, color = "différence de variation (crédit - titre)")) +
  labs(
    caption = "Source: Banque de France",
    y = "en pp",
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2019-01-01", max(FinancementCreditMarche$date))), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL,
                     labels = c("différence de taux",
                                "différence de variation"),
                     values = c("blue", "red")) +  
  ggtitle("Différences de croissance et de taux entre crédits bancaires et titres de dette")

CorrelationVariation



# 9. Recherche de corrélations -------------------------------------------

## 9.1. Crédits ---------------------------

CorrelationCredit <-ggplot(data = FinancementCreditMarche%>%
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
  geom_text(data = FinancementCreditMarche %>% filter(year(date) == 2023),
            aes(label = format(date, "%m"), x = CroissanceCreditTotal, y = TauxCreditTotal),
            vjust = -1, size = 2.5) +
  geom_path(aes(x = CroissanceCreditTotal, y = TauxCreditTotal),
            color = "gray") 

CorrelationCredit
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/CorrelationCredit.png", plot = CorrelationCredit, width = 16, height = 8, units = "in")
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/CorrelationCredit.svg", plot = CorrelationCredit, width = 8, height = 6, units = "in")

## 9.2. Dette ---------------------------

CorrelationDette <-ggplot(data = FinancementCreditMarche%>%
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
  geom_text(data = FinancementCreditMarche %>% filter(year(date) == 2023),
            aes(label = format(date, "%m"), x = CroissanceTitres, y = TauxTitreDette),
            vjust = -1, size = 2.5) +
  geom_path(aes(x = CroissanceTitres, y = TauxTitreDette),
            color = "gray") 

CorrelationDette

## 9.3. Crédit - années passées ---------------------------

CorrelationPastCredit <- ggplot(data = FinancementCreditMarche %>%
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

CorrelationPastCredit

## 9.4. Crédit - toutes années ---------------------------

CorrelationAllCredit <- ggplot(data = FinancementCreditMarche, aes(x = CroissanceCreditTotal, y = TauxCreditTotal, color = as.factor(year(date)))) +
  geom_point(size = 2) +
  labs(
    caption = "",
    x = "Croissance annuelle des crédits",
    y = "Taux d'intérêt annuel des nouveaux crédits",
    color = ""
  ) +  
  theme_minimal() +
  geom_path(aes(x = CroissanceCreditTotal, y = TauxCreditTotal), color = "gray")

## 9.5. Dette - toutes années ---------------------------

CorrelationAllDette <- ggplot(data = FinancementCreditMarche, aes(x = CroissanceTitres, y = TauxTitreDette, color = as.factor(year(date)))) +
  geom_point(size = 2) +
  labs(
    caption = "",
    x = "Croissance annuelle des titres de dette",
    y = "Taux d'intérêt annuel des titres de dette",
    color = ""
  ) +  
  theme_minimal() +
  geom_path(aes(x = CroissanceTitres, y = TauxTitreDette), color = "gray")



# 10. Encours et VA -------------------------------------------

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

EncoursVA <- ggplot(data = VATotal %>%
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

# ggsave(filename = "C:/Users/153003/Documents/Entreprise/EncoursMillions.png", plot = EncoursMillions, width = 8, height = 6, units = "in")


# 11. Conjoncture -------------------------------------------

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
  scale_x_date(limits = as.Date(c("2012-04-01", max(FinancementCreditMarche$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(78, 98), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL, values = c("PME" = "blue", "EI" = "red"))


# 12. Flux de Crédit ----------------------------

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
  scale_x_date(limits = as.Date(c("2018-01-01", max(FinancementCreditMarche$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-8000, 18000), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = NULL, values = c("Total" = "blue", "Treso" = "red", "Inv" = "black"))

# 13. Encours par Secteur ----------------------------

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








# save(GraphEndettement, CroissanceCrises, CroissanceAn, EncoursVA, Flux, CroissanceTaux, Correlation, CorrelationPast, Conjoncture, SecteurGraph,
#      file = "graphiques_Credit.rda")