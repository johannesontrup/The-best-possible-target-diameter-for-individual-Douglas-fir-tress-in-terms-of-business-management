library(RPostgreSQL)
library(tidyverse)
library(writexl)
library(dplyr)
write_db <- TRUE 

channel2 <- dbConnect(RPostgres::Postgres(),
                     dbname = "2025_Masterarbeit_Ontrub_Douglasienzieldurchmesser",
                     host = "134.76.17.104",
                     user = "khusman1",
                     port = "5432",
                     password = "456")

#1 Tabellen der BWI (csv-Dateien) in R laden
#2 Spalte des Bundeslandes und Bestockungstyps an die Haupttabelle anfügen
#3 Filtern der nötigen Daten: Bundesland = RLP, Bestandstyp = Douglasien-Reinbestände,
# Altersklasse = 41-60 jährige Douglasien (3.Akl)
#____________________________________________________________________________________________#
#1 Tabellen der BWI (csv-Dateien) in R laden

dat_b4_baeume <- dbReadTable(channel2, "dat_b4_baeume")
dat_b4_bestock <- dbReadTable(channel2, "dat_b4_bestock")
dat_b4_ecke_raum <- dbReadTable(channel2, "dat_b4_ecke_raum")

#______________________________________________________________________________________________#
#2 Spalte des Bundeslandes und Bestockungstyps an die Haupttabelle anfügen

dat_b4_baeume <- left_join(dat_b4_baeume,
                           dat_b4_bestock %>% select(Tnr, Enr, BestockTypFein),
                           by = c("Tnr", "Enr"))

dat_b4_baeume <- left_join(dat_b4_baeume, dat_b4_ecke_raum %>% select(Tnr, Enr, Bl), by = c("Tnr", "Enr"))

#___________________________________________________________________________________________#
 #3 Filtern der nötigen Daten: Bundesland = RLP, Bestandstyp = Douglasien-Reinbestände,
# Altersklasse = 41-60 jährige Douglasien (3.Akl)
Dgl_Baeume <- dat_b4_baeume %>% filter(Bl %in% 7)

Dgl_Baeume<-dat_b4_baeume %>% filter (BestockTypFein %in%2300)

Dgl_Baeume<-dat_b4_baeume %>% filter (Al_ba >=41, Al_ba <=60)























