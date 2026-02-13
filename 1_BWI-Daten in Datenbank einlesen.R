library(RPostgreSQL)
library(tidyverse)
library(writexl)
library(readr)
write_db<-TRUE

channel2 <- dbConnect(RPostgres::Postgres(),
                      dbname = "2025_Masterarbeit_Ontrub_Douglasienzieldurchmesser",
                      host = "134.76.17.104",
                      user = "khusman1",
                      port = "5432",
                      password = "456")

# Laden der csv-Datein in R und überschreiben auf die Datenbank

dat_b4_baeume<-read_csv("C:/Users/Johan/OneDrive/Dokumente/Forststudium/Master-Studium/Masterarbeit/BWI2022_abgel_Daten/dat_b4_baeume.csv")
dbWriteTable(channel2, "dat_b4_baeume", dat_b4_baeume)
dbExistsTable(channel2, "dat_b4_baeume")

dat_b4_bestock<-read_csv("C:/Users/Johan/OneDrive/Dokumente/Forststudium/Master-Studium/Masterarbeit/BWI2022_abgel_Daten/dat_b4_bestock.csv")
dbWriteTable(channel2, "dat_b4_bestock", dat_b4_bestock)
dbExistsTable(channel2, "dat_b4_bestock")
dbListTables(channel2)

dat_b4_bestock_baanteile<-read_csv("C:/Users/Johan/OneDrive/Dokumente/Forststudium/Master-Studium/Masterarbeit/BWI2022_abgel_Daten/dat_b4_bestock_baanteile.csv")
dbWriteTable(channel2, "dat_b4_bestock_baanteile", dat_b4_bestock_baanteile)

dat_b4_bestock_gt4m<-read_csv("C:/Users/Johan/OneDrive/Dokumente/Forststudium/Master-Studium/Masterarbeit/BWI2022_abgel_Daten/dat_b4_bestock_gt4m.csv")
dbWriteTable(channel2, "dat_b4_bestock_gt4m", dat_b4_bestock_gt4m)

dat_b0_ecke<-read_csv("C:/Users/Johan/OneDrive/Dokumente/Forststudium/Master-Studium/Masterarbeit/BWI2022_abgel_Daten/dat_b0_ecke.csv")
dbWriteTable(channel2, "dat_b0_ecke", dat_b0_ecke)

dat_b4_ecke_raum<-read_csv("C:/Users/Johan/OneDrive/Dokumente/Forststudium/Master-Studium/Masterarbeit/BWI2022_abgel_Daten/dat_b4_ecke_raum.csv")
dbWriteTable(channel2, "dat_b4_ecke_raum", dat_b4_ecke_raum)

dat_b4_ecke_w<-read_csv("C:/Users/Johan/OneDrive/Dokumente/Forststudium/Master-Studium/Masterarbeit/BWI2022_abgel_Daten/dat_b4_ecke_w.csv")
dbWriteTable(channel2, "dat_b4_ecke_w", dat_b4_ecke_w)
