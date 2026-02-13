library(RPostgreSQL)
library(tidyverse)
library(writexl)
library(readxl)
library(dplyr)
write_db <- TRUE

channel2 <- dbConnect(RPostgres::Postgres(),
                      dbname = "2025_Masterarbeit_Ontrub_Douglasienzieldurchmesser",
                      host = "134.76.17.104",
                      user = "khusman1",
                      port = "5432",
                      password = "456")

#1 Laden der Tabellen für die Ausgangsvariante
#2 Berechnung des Bestockungsgrades (BG)
#3 Zusammenfassen aller Tabellen zu einer Zeitreihe
#4 Einteilen nach Bestockungsgrad-Klassen
#5 Überschreiben in Datenbank
# gleicher Umgang mit allen anderen Szenarien
#_____________________________________________________________________________________________#
#1 Laden der Tabellen für die Ausgangsvariante
#1.1 Laden der tr-Tabellen des WaldPlaners (Einzelbaumdaten)

Df5_I.Ekl_z80_int1_00<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_00")
Df5_I.Ekl_z80_int1_05<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_05")
Df5_I.Ekl_z80_int1_10<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_10")
Df5_I.Ekl_z80_int1_15<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_15")
Df5_I.Ekl_z80_int1_20<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_20")
Df5_I.Ekl_z80_int1_25<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_25")
Df5_I.Ekl_z80_int1_30<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_30")
Df5_I.Ekl_z80_int1_35<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_35")
Df5_I.Ekl_z80_int1_40<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_40")
Df5_I.Ekl_z80_int1_45<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_45")
Df5_I.Ekl_z80_int1_50<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_50")
Df5_I.Ekl_z80_int1_55<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_55")
Df5_I.Ekl_z80_int1_60<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_60")
Df5_I.Ekl_z80_int1_65<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_65")
Df5_I.Ekl_z80_int1_70<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_70")
Df5_I.Ekl_z80_int1_75<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_75")
Df5_I.Ekl_z80_int1_80<-dbReadTable(channel2, "tr_dgl_1ekl_3akl_df_z80_5jahre_int1_80")

#1.2 Laden der Bestandesgrundflächen je Ertragsklasse nach Bergel (1985) (bereits in Datenbank)
G_ET_I<-dbReadTable(channel2, "G_ET_I")
G_ET_I <- G_ET_I %>% mutate(g_et = g * 100) %>% select(-g) # Umbenennung und Umrechnung in dm^2 
#(wie WaldPlaner)

#1.3 Laden der st-Tabelle des WaldPlaners (Bestandesdaten)
Df5_I.Ekl_z80_int1_00_Bestand<-dbReadTable(channel2, "st_dgl_1ekl_3akl_df_z80_5jahre_int1_00")

#_______________________________________________________________________________________________#
#2 Berechnung des Bestockungsgrades (BG)
#2.1 Anfügen der Bestandesgrundfläche und Grundfläche der Ertragstafel an die tr-Tabelle der Ausgangs-
#Einzelbaumdaten
Df5_I.Ekl_z80_int1_00 <- Df5_I.Ekl_z80_int1_00 %>%left_join(Df5_I.Ekl_z80_int1_00_Bestand %>% select(id, g),by = "id")
Df5_I.Ekl_z80_int1_00 <- Df5_I.Ekl_z80_int1_00 %>%left_join(G_ET_I %>% select(age, g_et),by = "age")

#2.2 Berechnung des Bestockungsgrades
Df5_I.Ekl_z80_int1_00 <- Df5_I.Ekl_z80_int1_00 %>% mutate(BG = round (g / g_et,2))


#________________________________________________________________________________________#
#3 Zusammenfassen aller tr-Tabellen zu einer Zeitreihe


#3.1 Alle Tabellen im Workspace auswählen, die mit "Df5_I.Ekl_z80_int1_" beginnen

tab_names <- ls(pattern = "^Df5_I\\.Ekl_z80_int1_")


#3.2 Tabelle Df5_I.Ekl_z80_int1_00_Bestand ausschließen

tab_names <- tab_names[tab_names != "Df5_I.Ekl_z80_int1_00_Bestand"]


#3.3 Tabellen aus dem Workspace holen

tab_list <- mget(tab_names)


#3.4 Alle Tabellen zu einer großen Tabelle zusammenfügen
# ---------------------------
Df5_I.Ekl_z80_int1 <- bind_rows(tab_list, .id = "quelle")

#________________________________________________________________________________________________#
#4 Einteilen nach Bestockungsgrad-Klassen


#4.1 Tabelle für BG 0,70 - 0,89

Df5_I.Ekl_z80_int1_ubestock <- Df5_I.Ekl_z80_int1 %>%
  filter(BG >= 0.70 & BG <= 0.89)


#4.2 Tabelle für BG 0,90 - 1,09

Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1 %>%
  filter(BG >= 0.90 & BG <= 1.09)


#4.3 Tabelle für BG 1,10 - 1,29

Df5_I.Ekl_z80_int1_uebestock <- Df5_I.Ekl_z80_int1 %>%
  filter(BG >= 1.10 & BG <= 1.29)

#________________________________________________________________________________________________#
#5 Überschreiben in Datenbank

dbWriteTable(channel2, "Df5_I.Ekl_z80_int1_ubestock", Df5_I.Ekl_z80_int1_ubestock, overwrite = TRUE)
dbWriteTable(channel2, "Df5_I.Ekl_z80_int1_nbestock", Df5_I.Ekl_z80_int1_nbestock, overwrite = TRUE)
dbWriteTable(channel2, "Df5_I.Ekl_z80_int1_uebestock", Df5_I.Ekl_z80_int1_uebestock, overwrite = TRUE)
