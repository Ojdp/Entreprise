# ================= Graphiques entreprises - données INSEE ===================== #
# Taux de marge et taux d'investissement
# Approches : (1) par branches, (2) par secteurs institutionnels
# ======================================================================== #

source("Entreprises_Insee_Data.R")



# 1. VA par branches -------------------------------------------

## 1.1. VA des SNF par branches -----------------------------------------------------

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


## 1.2. VA des SNF marchandes non agricoles  -----------------------------------------------------

### Valeur -----------------------------------
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

### Croissance ---------------------------------
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


# 2. EBE -------------------------------------------

ggplot(data=dataEBE %>%
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
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataEBE$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(85, 130), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("Excédent brut d'exploitation des sociétés non financières")+
  scale_color_discrete() 


# 3. Marges -------------------------------------------

GraphPrev <- ggplot(data=Marge %>% filter(name %in% c("Autres branches industrielles", "Commerce", "Construction", 
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

GraphPrev

# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphPrev.png", plot = GraphPrev, width = 8, height = 6, units = "in")
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphPrev.svg", plot = GraphPrev, width = 16, height = 8, units = "in")



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

# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphRepere.png", plot = GraphRepere, width = 8, height = 6, units = "in")
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphRepere.svg", plot = GraphRepere, width = 16, height = 8, units = "in")


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
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCF.raul$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(60, 140), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: Marge des SNF en Indice T42019=100")

print(GraphFBCFmarge1)
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFmarge1.png", plot = GraphFBCFmarge1, width = 8, height = 6, units = "in")







# 4. FBCF -------------------------------------------

## 4.1. Croissance de la FBCF totale des SNF -----------------------------------------------------

ggplot(dataFBCF %>%
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
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCF$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(-20, 30), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  scale_color_discrete()


## 4.2. FBCF par produit (indice) -----------------------------------------------------

GraphFBCFparproduit <- ggplot(data=dataFBCF %>% 
                        filter((Produit %in% c( "Autres", "Biens d'équipement","Construction",
                                                "Matériels de transport","Information-communication",
                                                "Services aux entreprises")) &
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
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCF$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(50, 130), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par produits en volume en Indice T42019=100")

GraphFBCFparproduit
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduit.png", plot = GraphFBCFparproduit, width = 8, height = 6, units = "in")


## 4.3. FBCF par produit (glissement annuel) -----------------------------------------------------

GraphFBCFparproduitGA <- ggplot(data=dataFBCF %>%
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
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataFBCF$DATE))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-30, 60), labels = scales::label_number(decimal.mark = ",")) +
  # scale_color_manual(name = "Légende des séries",  # Nom de la légende pour les séries de données
  # values = c("SNF" = "green", "Ménages" = "red", "APU" = "blue", "Total" = "black")) +
  ggtitle("France: FBCF par produits en glissement annuel en volume, en %")

GraphFBCFparproduitGA
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduitGA.png", plot = GraphFBCFparproduitGA, width = 8, height = 6, units = "in")


## 4.4. Contribution à la croissance de la FBCF -----------------------------------------------------

### Base 2019 = 0 ----------------------------------
GraphFBCFparproduitContribution <- ggplot(data = dataFBCF %>%
                                            filter(Produit %in% c("Autres", "Biens d'équipement", "Construction", "Matériels de transport", "Information-communication", "Services aux entreprises") & 
                                                     TITLE_FR3 == "Volume aux prix de l'année précédente chaînés")) +
  geom_bar(aes(x = DATE, y = ContributionDateBase, fill = Produit), position = "stack", stat = "identity") +  
  geom_point(data = dataFBCF %>%
               filter(Produit %in% c("Total") & TITLE_FR3 == "Volume aux prix de l'année précédente chaînés"), aes(x = DATE, y = ContributionDateBase), color = "black", size = 3) +# Create a stacked bar chart
  labs(
    title = "Contributions to Growth",
    
    x = "",
    y = "Contribution (%)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2017-01-01", max(dataFBCF$DATE))), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(-10, 12), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("France: Contribution à la croissance des FBCF des SNF par produits en volume, en %")

GraphFBCFparproduitContribution
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphFBCFparproduitContribution.png", plot = GraphFBCFparproduitContribution, width = 8, height = 6, units = "in")


### Même graphique Base 2019 = 0, mais qui commence en 2020 ------------
Graph3FR <- ggplot() +
  geom_bar(data = dataFBCF %>%
             filter(Produit %in% c("Autres", "Biens d'équipement", "Construction", "Matériels de transport", "Information-communication", "Services aux entreprises") &
                      TITLE_FR3 == "Volume aux prix de l'année précédente chaînés" &
                      DATE >= "2020-01-01"),
           aes(x = DATE, y = ContributionDateBase, fill = Produit),
           position = "stack", stat = "identity") +
  geom_point(data = dataFBCF %>%
               filter(Produit == "Total" &
                        TITLE_FR3 == "Volume aux prix de l'année précédente chaînés" &
                        DATE >= "2020-01-01"),
             aes(x = DATE, y = ContributionDateBase),
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
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/Graph3FR.svg", plot = Graph3FR, width = 8, height = 6, units = "in")


### Contribution à la croissance annuelle -------------------
#si jamais ce graph est utilisé faut revoir l'échelle 
GraphFBCFparproduitContributionBis <- ggplot(data = dataFBCF %>%
                                               filter(Produit %in% c("Autres", "Biens d'équipement", "Construction", "Matériels de transport", "Information-communication", "Services aux entreprises") & 
                                                        TITLE_FR3 == "Volume aux prix de l'année précédente chaînés")) +
  geom_bar(aes(x = DATE, y = ContributionAnnuelle, fill = Produit), position = "stack", stat = "identity") +  
  geom_point(data = dataFBCF %>%
               filter(Produit %in% c("Total") & TITLE_FR3 == "Volume aux prix de l'année précédente chaînés"), aes(x = DATE, y = ContributionAnnuelle), color = "black", size = 3) +# Create a stacked bar chart
  labs(
    title = "Contributions to Growth",
    
    x = "",
    y = "Contribution (%)"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2017-01-01", max(dataFBCF$DATE))), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(-8, 10), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("France: Contribution à la croissance des FBCF des SNF par produits en volume, en %")

GraphFBCFparproduitContributionBis






# 5. Avec l'approche par secteurs institutionnels - SNF -------------------------------------------

## 5.1. Autofinancement -----------------------------------------------------

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
  ggtitle("France: Taux d'autofinancement des SNF (épargne/FBCF)")+
  scale_color_discrete() 


## 5.2. Taux d'investissement -----------------------------------------------------

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


## 5.3. Taux de marge -----------------------------------------------------

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


## 5.4. EBE -----------------------------------------------------

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



## 5.5. Calcul moyenne sur la période -----------------------------------------------------

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



# 6. Avec prévisions -------------------------------------------

GraphPrev<- ggplot(data = dataPrev) +
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

GraphPrev
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphPrev.svg", plot = GraphPrev, width = 8, height = 6, units = "in")
# ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphPrev.png", plot = GraphPrev, width = 8, height = 6, units = "in")



# 7. Enquete de conjoncture INSEE -------------------------------------------

ggplot(data = dataEnquete, aes(x = DATE, y = OBS_VALUE)) +
  geom_line() +
  labs(
    caption = NULL,
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(dataEnquete$DATE))), expand = c(0, 0)) +
  scale_y_continuous(name = "% de la VA", 
                     limits = c(60, 120), 
                     labels = scales::label_number(accuracy = 1, scale = 1, decimal.mark = ',')) +
  ggtitle("France: Indicateur du climat des affaires") +
  scale_color_discrete()







# #Export
# 
# wb <- createWorkbook()
# addWorksheet(wb, "EBE")
# addWorksheet(wb, "VA")
# addWorksheet(wb, "Marge")
# addWorksheet(wb, "FBCFagentvol")
# addWorksheet(wb, "FBCFagentvaleur")
# addWorksheet(wb, "FBCFproduitvaleur")
# addWorksheet(wb, "FBCFproduitvolume")
# 
# 
# writeData(wb, sheet = "EBE", dataEBE, startCol = 1, startRow = 1)
# writeData(wb, sheet = "VA", dataVA, startCol = 1, startRow = 1)
# writeData(wb, sheet = "Marge", dataRapports, startCol = 1, startRow = 1)
# writeData(wb, sheet = "FBCFagentvol", dataFBCFagentvolume, startCol = 1, startRow = 1)
# writeData(wb, sheet = "FBCFagentvaleur", dataFBCFagentvaleur, startCol = 1, startRow = 1)
# writeData(wb, sheet = "FBCFproduitvaleur", dataFBCF, startCol = 1, startRow = 1)
# writeData(wb, sheet = "FBCFproduitvolume", dataFBCF, startCol = 1, startRow = 1)
# 
# # Sauvegardez le fichier Excel
# saveWorkbook(wb, "C:/Users/153003/Documents/Entreprise/DataEntreprise.xlsx", overwrite = TRUE)







