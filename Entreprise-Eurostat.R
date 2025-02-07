install.packages("rsdmx")

library(rsdmx)
library(tidyverse)
library(rsdmx)
library(zoo)
library(lubridate)
library(ggplot2)
library(ofce)

struc <- data.frame(readSDMX('https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/datastructure/ESTAT/NASQ_10_F_BS')@datastructures[[1]]@Components)

FinancialBS <- data.frame(      (readSDMX(providerId = 'ESTAT', resource = 'data', flowRef = 'nasq_10_f_bs',
                                      key = 'Q.MIO_EUR.S11.LIAB.F+F3+F31+F32+F4+F41+F42+F5.DE+ES+FR+IT+EA20')       )     )%>%
  select(geo, obsTime, obsValue, na_item)%>% 
  pivot_wider(names_from = "na_item", values_from = "obsValue")%>%
  mutate(obsTime = as.yearqtr(obsTime, format = "%Y-Q%q"))

# F: Total Liabilities
# F3: Debt Securities (équivalent CNF.Q.N.FR.W0.S11.S1.N.L.LE.F3.T._Z.XDC._T.S.V.N._T  sur Bdf)
# F31:Short Term debt Securities 
# F32:Long Term debt securities 
# F4: Loans
# F41:Short Term Loans
# F42:Long Term Loans
# F5: Equity and investement fund shares


filtre.FR<- "FR"

FinancialBSFR <- FinancialBS %>%
  filter( geo %in% filtre.FR) %>%
  select(-geo)%>%
  mutate(Verif = F - (F3+F5+F4),
         Verif2 = (Verif/ F)*100)
  pivot_longer(cols = -obsTime, names_to = "Variable", values_to = "Valeur")%>%
  mutate(Verif = Valeur[Variable == "F"] - (Valeur[Variable == "F3"]+ Valeur[Variable == "F4"]+ Valeur[Variable == "F5"]))
  

