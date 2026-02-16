library(RPostgreSQL)
library(tidyverse)
library(writexl)
library(readxl)
library(dplyr)
write_db <- TRUE

channel2 <- dbConnect(RPostgres::Postgres(),
                      dbname = "2025_Masterarbeit_Ontrub_Douglasienzieldurchmesser",
                      host = "",
                      user = "",
                      port = "",
                      password = "")


#____________________________________________________________________________________________________#
#1 Laden aller Tabellen



Df5_I.Ekl_z80_int1_nbestock_ZBaum <-dbReadTable(channel2, "Df5_I.Ekl_z80_int1_nbestock_ZBaum")
Df10_I.Ekl_z80_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df10_I.Ekl_z80_int1_nbestock_ZBaum")
Df5_I.Ekl_z120_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z120_int1_nbestock_ZBaum")
Df5_I.Ekl_z80_int12_nbestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z80_int12_nbestock_ZBaum")
Df5_I.Ekl_z80_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z80_int1_ubestock_ZBaum")
Df10_I.Ekl_z80_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df10_I.Ekl_z80_int1_ubestock_ZBaum")
Df5_I.Ekl_z120_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z120_int1_ubestock_ZBaum")
Df5_I.Ekl_z80_int12_ubestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z80_int12_ubestock_ZBaum")
Df5_I.Ekl_z80_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z80_int1_uebestock_ZBaum")
Df10_I.Ekl_z80_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df10_I.Ekl_z80_int1_uebestock_ZBaum")
Df5_I.Ekl_z120_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z120_int1_uebestock_ZBaum")
Df5_I.Ekl_z80_int12_uebestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z80_int12_uebestock_ZBaum")
Df5_0.Ekl_z80_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z80_int1_uebestock_ZBaum")
Df10_0.Ekl_z80_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df10_0.Ekl_z80_int1_uebestock_ZBaum")
Df5_0.Ekl_z120_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z120_int1_uebestock_ZBaum")
Df5_0.Ekl_z80_int12_uebestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z80_int12_uebestock_ZBaum")
Df5_0.Ekl_z80_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z80_int1_ubestock_ZBaum")
Df10_0.Ekl_z80_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df10_0.Ekl_z80_int1_ubestock_ZBaum")
Df5_0.Ekl_z120_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z120_int1_ubestock_ZBaum")
Df5_0.Ekl_z80_int12_ubestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z80_int12_ubestock_ZBaum")
Df5_0.Ekl_z80_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z80_int1_nbestock_ZBaum")
Df10_0.Ekl_z80_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df10_0.Ekl_z80_int1_nbestock_ZBaum")
Df5_0.Ekl_z120_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z120_int1_nbestock_ZBaum")
Df5_0.Ekl_z80_int12_nbestock_ZBaum<-dbReadTable(channel2, "Df5_0.Ekl_z80_int12_nbestock_ZBaum")
Df5_II.Ekl_z80_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z80_int1_nbestock_ZBaum")
Df10_II.Ekl_z80_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df10_II.Ekl_z80_int1_nbestock_ZBaum")
Df5_II.Ekl_z120_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z120_int1_nbestock_ZBaum")
Df5_II.Ekl_z80_int12_nbestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z80_int12_nbestock_ZBaum")
Df5_II.Ekl_z80_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z80_int1_ubestock_ZBaum")
Df10_II.Ekl_z80_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df10_II.Ekl_z80_int1_ubestock_ZBaum")
Df5_II.Ekl_z120_int1_ubestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z120_int1_ubestock_ZBaum")
Df5_II.Ekl_z80_int12_ubestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z80_int12_ubestock_ZBaum")
Df5_II.Ekl_z80_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z80_int1_uebestock_ZBaum")
Df10_II.Ekl_z80_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df10_II.Ekl_z80_int1_uebestock_ZBaum")
Df5_II.Ekl_z120_int1_uebestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z120_int1_uebestock_ZBaum")
Df5_II.Ekl_z80_int12_uebestock_ZBaum<-dbReadTable(channel2, "Df5_II.Ekl_z80_int12_uebestock_ZBaum")
kDf_II.Ekl_uebestock_ZBaum<-dbReadTable(channel2, "kDf_II.Ekl_uebestock_ZBaum")
kDf_II.Ekl_ubestock_ZBaum<-dbReadTable(channel2, "kDf_II.Ekl_ubestock_ZBaum")
kDf_II.Ekl_nbestock_ZBaum<-dbReadTable(channel2, "kDf_II.Ekl_nbestock_ZBaum")
kDf_I.Ekl_nbestock_ZBaum<-dbReadTable(channel2, "kDf_I.Ekl_nbestock_ZBaum")
kDf_I.Ekl_ubestock_ZBaum<-dbReadTable(channel2, "kDf_I.Ekl_ubestock_ZBaum")
kDf_I.Ekl_uebestock_ZBaum<-dbReadTable(channel2, "kDf_I.Ekl_uebestock_ZBaum")
kDf_0.Ekl_uebestock_ZBaum<-dbReadTable(channel2, "kDf_0.Ekl_uebestock_ZBaum")
kDf_0.Ekl_ubestock_ZBaum<-dbReadTable(channel2, "kDf_0.Ekl_ubestock_ZBaum")
kDf_0.Ekl_nbestock_ZBaum<-dbReadTable(channel2, "kDf_0.Ekl_nbestock_ZBaum")

Hekfr_Erloes_Durchschnitt<-dbReadTable(channel2, "Hekfr_Erloese_Einzelbaum_Durchschnitt")


#_____________________________________________________________________________________________________#
#2 Wertermittlung des Einzelbaumes
#2.1 Mittlere Qualität
#2.1.1 Qualität Mittel: Abschnitt 19 m Länge
# Festlegen der Stärkeklassen
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    Staerkeklasse = case_when(
      between(diam_10m, 10, 14) ~ "1a",
      between(diam_10m, 15, 19) ~ "1b",
      between(diam_10m, 20, 24) ~ "2a",
      between(diam_10m, 25, 29) ~ "2b",
      between(diam_10m, 30, 34) ~ "3a",
      between(diam_10m, 35, 39) ~ "3b",
      between(diam_10m, 40, 49) ~ "4",
      between(diam_10m, 50, 59) ~ "5",
      between(diam_10m, 60, 69) ~ "6",
      between(diam_10m, 70, 79) ~ "7",
      diam_10m >= 80            ~ "8",
      TRUE ~ NA_character_   
    )
  )
# Anfügen des hekfr. Erlöses für berechnete Stärkeklasse
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  left_join(
    Hekfr_Erloes_Durchschnitt %>% select(Staerkeklasse, B.C, D),
    by = "Staerkeklasse"
  )

#2.1.2 Qualität mittel: Abschnitt 10m Länge
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  # 1. Stärkeklasse für zweiten Abschnitt bestimmen
  mutate(
    Staerkeklasse_24.5m = case_when(
      between(diam_24.5m, 10, 14) ~ "1a",
      between(diam_24.5m, 15, 19) ~ "1b",
      between(diam_24.5m, 20, 24) ~ "2a",
      between(diam_24.5m, 25, 29) ~ "2b",
      between(diam_24.5m, 30, 34) ~ "3a",
      between(diam_24.5m, 35, 39) ~ "3b",
      between(diam_24.5m, 40, 49) ~ "4",
      between(diam_24.5m, 50, 59) ~ "5",
      between(diam_24.5m, 60, 69) ~ "6",
      between(diam_24.5m, 70, 79) ~ "7",
      diam_24.5m >= 80            ~ "8",
      TRUE ~ NA_character_
    )
  ) %>%
# Qualitätswerte D aus Referenztabelle anhängen
  left_join(
    Hekfr_Erloes_Durchschnitt %>%
      select(Staerkeklasse, D) %>%
      rename(Staerkeklasse_24.5m = Staerkeklasse,
             D_24.5m   = D),
    by = "Staerkeklasse_24.5m"
  )

#2.1.3 Qualität mittel: Reststück --> IH anhängen
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(IH = Hekfr_Erloes_Durchschnitt$IH[1])


#2.1.4 Abtriebswert [Ax] berechnen für Qualität mittel
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    across(
      c(vol_19, vol_10, rest, B.C, D_24.5m, IH, diam_24.5m, vol_efm),
      as.numeric
    ),
    # Schritt 1: Summe
    vol_sum = vol_19 + vol_10 + rest,
    
    # Schritt 2: Anteile
    share_19 = vol_19 / vol_sum,
    share_10 = vol_10 / vol_sum,
    share_rest = rest / vol_sum,
    
    # Schritt 3: gewichtete Volumina
    vol_19_eff = share_19 * vol_efm,
    vol_10_eff = share_10 * vol_efm,
    rest_eff   = share_rest * vol_efm,
    
    # Schritt 4: Ax berechnen
    vol_10_component = case_when(
      diam_24.5m %in% c(0, -1)               ~ vol_10_eff * IH,
      diam_10m < 10                          ~ vol_10_eff * IH,
      diam_24.5m < 10                        ~ vol_10_eff * IH,
      Staerkeklasse_24.5m %in% c("1a", "1b") ~ vol_10_eff * IH,
      TRUE                                   ~ vol_10_eff * D_24.5m
    ),
    Ax_mittel = if_else(
      Staerkeklasse == "1a",
      vol_19_eff * IH + vol_10_component + rest_eff * IH,  # Spezialfall
      vol_19_eff * B.C + vol_10_component + rest_eff * IH  # Standard
    )
  ) %>%
  select(-c(vol_sum, share_19, share_10, share_rest,
            vol_19_eff, vol_10_eff, rest_eff, vol_10_component))


#2.2 Abtriebswert schlecht (Ax_schlecht) berechnen
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    across(
      c(vol_19, vol_10, rest, vol_efm, D, D_24.5m, IH,
        diam_10m, diam_24.5m),
      as.numeric
    ),
    
    # Summe der Volumina
    vol_sum = vol_19 + vol_10 + rest,
    
    # Anteile
    share_1 = if_else(vol_sum == 0, 0, vol_19 / vol_sum),
    share_2 = if_else(vol_sum == 0, 0, vol_10 / vol_sum),
    share_r = if_else(vol_sum == 0, 0, rest / vol_sum),
    
    # Effektive Volumina
    vol_19_eff = share_1 * vol_efm,
    vol_10_eff = share_2 * vol_efm,
    rest_eff = share_r * vol_efm,
    
    # vol_1 Regeln
    vol_19_comp = case_when(
      diam_10m %in% c(0, -1) ~ vol_19_eff * IH,
      Staerkeklasse == "1a" ~ vol_19_eff * IH,
      TRUE ~ vol_19_eff * D
    ),
    
    # vol_2 Regeln (korrigiert: 1a und 1b → IH)
    vol_10_comp = case_when(
      diam_24.5m %in% c(0, -1) ~ vol_10_eff * IH,
      diam_24.5m  < 10 ~ vol_10_eff * IH,
      Staerkeklasse2_gut %in% c("1a", "1b") ~ vol_10_eff * IH,
      TRUE ~ vol_10_eff * D_24.5m
    ),
    # rest2 Regeln
    rest_comp = rest_eff * IH,
    
    # finale Ax-Berechnung
    Ax_schlecht = vol_19_comp + vol_10_comp + rest_comp
  ) %>%
  select(-c(vol_sum, share_1, share_2, share_r,
            vol_19_eff, vol_10_eff, rest_eff,
            vol_19_comp, vol_10_comp, rest_comp))


dbWriteTable(channel2, "Df5_I.Ekl_z80_int1_nbestock",Df5_I.Ekl_z80_int1_nbestock, overwrite=TRUE)



#2.3 Abtriebswert bei Wertästung 6,5 m

#2.3.1 Abschnitt 1: Stärkeklasse definieren und Hekfr. Erlös A, B und C anhängen

Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    Staerkeklasse1_WAs6.5 = case_when(
      between(diam_3.5m, 10, 14) ~ "1a",
      between(diam_3.5m, 15, 19) ~ "1b",
      between(diam_3.5m, 20, 24) ~ "2a",
      between(diam_3.5m, 25, 29) ~ "2b",
      between(diam_3.5m, 30, 34) ~ "3a",
      between(diam_3.5m, 35, 39) ~ "3b",
      between(diam_3.5m, 40, 49) ~ "4",
      between(diam_3.5m, 50, 59) ~ "5",
      between(diam_3.5m, 60, 69) ~ "6",
      between(diam_3.5m, 70, 79) ~ "7",
      diam_3.5m >= 80            ~ "8",
      TRUE ~ NA_character_   
    )
  ) %>%
  # Qualitätswerte A und B und C aus Referenztabelle anhängen
  left_join(
    Hekfr_Erloes_Durchschnitt %>%
      select(Staerkeklasse, A, B.C) %>%
      rename(Staerkeklasse1_WAs6.5 = Staerkeklasse,
             B.C_3.5m=B.C,
             A_3.5m=A),
    by = "Staerkeklasse1_WAs6.5"
  )

#2.3.2 Abschnitt 2
#Stärkeklassen definieren
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    Staerkeklasse2_WAs6.5 = case_when(
      between(diam_14.3m, 10, 14) ~ "1a",
      between(diam_14.3m, 15, 19) ~ "1b",
      between(diam_14.3m, 20, 24) ~ "2a",
      between(diam_14.3m, 25, 29) ~ "2b",
      between(diam_14.3m, 30, 34) ~ "3a",
      between(diam_14.3m, 35, 39) ~ "3b",
      between(diam_14.3m, 40, 49) ~ "4",
      between(diam_14.3m, 50, 59) ~ "5",
      between(diam_14.3m, 60, 69) ~ "6",
      between(diam_14.3m, 70, 79) ~ "7",
      diam_14.3m >= 80            ~ "8",
      TRUE ~ NA_character_   # Fallback, falls kleiner als 10
    )
  ) %>%
# Qualitätswerte B und D aus Referenztabelle anhängen
  left_join(
    Hekfr_Erloes_Durchschnitt %>%
      select(Staerkeklasse, B.C) %>%
      rename(Staerkeklasse2_WAs6.5 = Staerkeklasse,
             B.C_14.3m=B.C),
    by = "Staerkeklasse2_WAs6.5"
  )


#2.3.3 Abschnitt 3
# Stärkeklassen definieren
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    Staerkeklasse3_WAs6.5 = case_when(
      between(diam_27m, 10, 14) ~ "1a",
      between(diam_27m, 15, 19) ~ "1b",
      between(diam_27m, 20, 24) ~ "2a",
      between(diam_27m, 25, 29) ~ "2b",
      between(diam_27m, 30, 34) ~ "3a",
      between(diam_27m, 35, 39) ~ "3b",
      between(diam_27m, 40, 49) ~ "4",
      between(diam_27m, 50, 59) ~ "5",
      between(diam_27m, 60, 69) ~ "6",
      between(diam_27m, 70, 79) ~ "7",
      diam_27m >= 80            ~ "8",
      TRUE ~ NA_character_   # Fallback, falls kleiner als 10
    )
  ) %>%
# Qualitätswerte B und D aus Referenztabelle anhängen
  left_join(
    Hekfr_Erloes_Durchschnitt %>%
      select(Staerkeklasse,D) %>%
      rename(Staerkeklasse3_WAs6.5 = Staerkeklasse,
             D_27m=D),
    by = "Staerkeklasse3_WAs6.5"
  )

#2.3.4 Berechnung des Abtriebswertes
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    across(
      c(vol_1_WAs6.5, vol_2_WAs6.5, vol_3_WAs6.5, rest_WAs6.5, vol_efm, A_3.5m, B.C_3.5m, B.C_14.3m, D_27m, IH,
        diam_3.5m, diam_14.3m, diam_27m),
      as.numeric
    ),
    # Summe der Volumina
    vol_sum = vol_1_WAs6.5 + vol_2_WAs6.5 + vol_3_WAs6.5 + rest_WAs6.5,
    
    # Anteile (sicher gegen Division durch 0)
    share_1 = if_else(vol_sum == 0, 0, vol_1_WAs6.5 / vol_sum),
    share_2 = if_else(vol_sum == 0, 0, vol_2_WAs6.5 / vol_sum),
    share_3 = if_else(vol_sum == 0, 0, vol_3_WAs6.5 / vol_sum),
    share_r = if_else(vol_sum == 0, 0, rest_WAs6.5 / vol_sum),
    
    # effektive (gewichtete) Volumina
    vol_1_eff = share_1 * vol_efm,
    vol_2_eff = share_2 * vol_efm,
    vol_3_eff = share_3 * vol_efm,
    rest2_eff = share_r * vol_efm,
    
    # vol_1 Regeln
    vol_1_comp = case_when(
      diam_3.5m %in% c(0, -1) ~ vol_1_eff * IH,
      Staerkeklasse1_WAs6.5 %in% c("1a","1b","2a","2b","3a") ~ vol_1_eff * B.C_3.5m,
      TRUE ~ vol_1_eff * (A_3.5m * 0.85)
    ),
    
    # vol_2 Regeln
    vol_2_comp = case_when(
      diam_14.3m %in% c(0, -1) ~ vol_2_eff * IH,
      diam_14.3m < 10 ~ vol_2_eff * IH,
      Staerkeklasse2_WAs6.5 == "1a" ~ vol_2_eff * IH,
      TRUE ~ vol_2_eff * B.C_14.3m
    ),
    
    # vol_3 Regeln (korrigiert: 1a/1b -> mit IH multiplizieren)
    vol_3_comp = case_when(
      diam_27m %in% c(0, -1) ~ vol_3_eff * IH,
      diam_27m <10 ~ vol_3_eff * IH,
      Staerkeklasse3_WAs6.5 %in% c("1a","1b") ~ vol_3_eff * IH,  # <-- Änderung
      TRUE ~ vol_3_eff * D_27m
    ),
    
    # rest2 mit IH
    rest2_comp = rest2_eff * IH,
    
    # finale Ax-Berechnung
    Ax_WAs6.5_schlecht = vol_1_comp + vol_2_comp + vol_3_comp + rest2_comp
  ) %>%
  select(-c(vol_sum, share_1, share_2, share_3, share_r,
            vol_1_eff, vol_2_eff, vol_3_eff, rest2_eff,
            vol_1_comp, vol_2_comp, vol_3_comp, rest2_comp))




#2.4 Abtriebswert bei Wertästung 12 m

#2.4.1 Abschnitt 1: Stärkeklasse definieren und Hekfr. Erlös A, B und C anhängen

Df5_I.Ekl_z80_int1_nbestock<- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    Staerkeklasse1_WAs12 = case_when(
      between(diam_3.5m, 10, 14) ~ "1a",
      between(diam_3.5m, 15, 19) ~ "1b",
      between(diam_3.5m, 20, 24) ~ "2a",
      between(diam_3.5m, 25, 29) ~ "2b",
      between(diam_3.5m, 30, 34) ~ "3a",
      between(diam_3.5m, 35, 39) ~ "3b",
      between(diam_3.5m, 40, 49) ~ "4",
      between(diam_3.5m, 50, 59) ~ "5",
      between(diam_3.5m, 60, 69) ~ "6",
      between(diam_3.5m, 70, 79) ~ "7",
      diam_3.5m >= 80            ~ "8",
      TRUE ~ NA_character_   
    )
  ) %>%
  left_join(
    Hekfr_Erloes_Durchschnitt %>%
      select(Staerkeklasse, A, B) %>%
      rename(Staerkeklasse1_WAs12 = Staerkeklasse,
             B_6.3m=B,
             A_6.3m=A),
    by = "Staerkeklasse1_WAs12"
  )

#2.4.2 Abschnitt 2: Stärkeklassen definieren und hekfr. Erlös aus Referenztabelle anhängen
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    Staerkeklasse2_WAs12 = case_when(
      between(diam_14.3m, 10, 14) ~ "1a",
      between(diam_14.3m, 15, 19) ~ "1b",
      between(diam_14.3m, 20, 24) ~ "2a",
      between(diam_14.3m, 25, 29) ~ "2b",
      between(diam_14.3m, 30, 34) ~ "3a",
      between(diam_14.3m, 35, 39) ~ "3b",
      between(diam_14.3m, 40, 49) ~ "4",
      between(diam_14.3m, 50, 59) ~ "5",
      between(diam_14.3m, 60, 69) ~ "6",
      between(diam_14.3m, 70, 79) ~ "7",
      diam_14.3m >= 80            ~ "8",
      TRUE ~ NA_character_   
    )
  ) %>%
  left_join(
    Hekfr_Erloes_Durchschnitt %>%
      select(Staerkeklasse, B.C) %>%
      rename(Staerkeklasse2_WAs12 = Staerkeklasse,
             B.C_17m=B.C),
    by = "Staerkeklasse2_WAs12"
  )

# Abtriebswert berechnen
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    across(
      c(vol_1_WAs12, vol_2_WAs12, vol_3_WAs6.5, rest_WAs6.5, vol_efm, A_6.3m, B_6.3m, B.C_17m.x, D_27m, IH,
        diam_6.3m, diam_17m, diam_27m),
      as.numeric
    ),
    # Summe der Volumina
    vol_sum = vol_1_WAs12 + vol_2_WAs12 + vol_3_WAs6.5 + rest_WAs6.5,
    
    # Anteile (sicher gegen Division durch 0)
    share_1 = if_else(vol_sum == 0, 0, vol_1_WAs12 / vol_sum),
    share_2 = if_else(vol_sum == 0, 0, vol_2_WAs12 / vol_sum),
    share_3 = if_else(vol_sum == 0, 0, vol_3_WAs6.5 / vol_sum),
    share_r = if_else(vol_sum == 0, 0, rest_WAs6.5 / vol_sum),
    
    # effektive (gewichtete) Volumina
    vol_1_eff = share_1 * vol_efm,
    vol_2_eff = share_2 * vol_efm,
    vol_3_eff = share_3 * vol_efm,
    rest2_eff = share_r * vol_efm,
    
    # vol_1 Regeln
    vol_1_comp = case_when(
      diam_6.3m %in% c(0, -1) ~ vol_1_eff * IH,
      Staerkeklasse1_WAs12 %in% c("1a","1b","2a","2b","3a") ~ vol_1_eff * B_6.3m,
      TRUE ~ vol_1_eff * (A_6.3m * 0.85)
    ),
    
    # vol_2 Regeln
    vol_2_comp = case_when(
      diam_17m %in% c(0, -1) ~ vol_2_eff * IH,
      diam_17m < 10 ~ vol_2_eff * IH,
      Staerkeklasse2_WAs12 == "1a" ~ vol_2_eff * IH,
      TRUE ~ vol_2_eff * B.C_17m.x
    ),
    
    # vol_3 Regeln (korrigiert: 1a/1b -> mit IH multiplizieren)
    vol_3_comp = case_when(
      diam_27m %in% c(0, -1) ~ vol_3_eff * IH,
      diam_27m <10 ~ vol_3_eff * IH,
      Staerkeklasse3_WAs6.5 %in% c("1a","1b") ~ vol_3_eff * IH,  # <-- Änderung
      TRUE ~ vol_3_eff * D_27m
    ),
    
    # rest2 mit IH
    rest2_comp = rest2_eff * IH,
    
    # finale Ax-Berechnung
    Ax_WAs12_schlecht = vol_1_comp + vol_2_comp + vol_3_comp + rest2_comp
  ) %>%
  select(-c(vol_sum, share_1, share_2, share_3, share_r,
            vol_1_eff, vol_2_eff, vol_3_eff, rest2_eff,
            vol_1_comp, vol_2_comp, vol_3_comp, rest2_comp))




#____________________________________________________________________________________________________#
#3 Kalkulation der Bodenbruttorente (Opportunitätskosten des Bodens)


#3.1 Kronenprojektionsflaeche berechnen


Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(kpf = (pi/4) * (crownwidth^2) *0.0001)

#3.2Kulturkosten (c) pro Einzelbaum berechnen

Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(
    c = (810 / 10000) * kpf
  )

#3.3 Bodenbruttorente berechnen

library(dplyr)

# Diskontierungszinssatz
r <- 0.03

# Funktion zur Berechnung der Bodenbruttorente 
calc_bb <- function(df_row, r = 0.03) {
  
  # Kulturkosten (Alter 0) ---
  c_disk <- df_row$c
  
  # Diskontierter Abtriebswert (Alter = age) ---
  age <- df_row$age
  Ax_disk <- df_row$Ax_mittel / (1 + r)^age
  
  #Durchforstungserträge berechnen nach Regeln ---
  df_ages <- seq(25, 130, by = 5)  # mögliche Durchforstungsalter
  df_values <- sapply(df_ages, function(df_age) {
    
    if (df_age >= age) return(0)  # keine Durchforstung nach Umtriebsalter
    
    # Regeln anwenden
    if (df_age == 25) {
      df_val <- (1000 / 10000) * df_row$kpf
    } else if (df_age == 30) {
      df_val <- (1500 / 10000) * df_row$kpf
    } else if (df_age >= 35 && df_age <= 130) {
      df_val <- (2000 / 10000) * df_row$kpf
    } else {
      df_val <- 0
    }
    
    # Diskontierung auf Alter des Baumes
    df_val / (1 + r)^df_age
  })
  
  # Kapitalwert ---
  Kapitalwert <- Ax_disk + sum(df_values, na.rm = TRUE) - c_disk
  
  # Bodenbruttorente (Annuität über Umtriebszeit) ---
  bb <- Kapitalwert * ((1 + r)^age * r) / ((1 + r)^age - 1)
  
  return(bb)
}

# Anwendung auf alle Zeilen ---
  Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  rowwise() %>%
  mutate(bb = calc_bb(cur_data(), r = r)) %>%
  ungroup()


dbWriteTable(channel2, "Df5_I.Ekl_z80_int1_nbestock", Df5_I.Ekl_z80_int1_nbestock, overwrite=TRUE)

#__________________________________________________________________________________________________#
#4 Filtern der Z-Bäume

tabellen_namen <- c(
  "Df5_I.Ekl_z80_int1_nbestock", "Df10_I.Ekl_z80_int1_nbestock",
  "Df5_I.Ekl_z120_int1_nbestock", "Df5_I.Ekl_z80_int12_nbestock",
  "Df5_I.Ekl_z80_int1_ubestock", "Df10_I.Ekl_z80_int1_ubestock",
  "Df5_I.Ekl_z120_int1_ubestock", "Df5_I.Ekl_z80_int12_ubestock",
  "Df5_I.Ekl_z80_int1_uebestock", "Df10_I.Ekl_z80_int1_uebestock",
  "Df5_I.Ekl_z120_int1_uebestock", "Df5_I.Ekl_z80_int12_uebestock",
  "Df5_0.Ekl_z80_int1_uebestock", "Df10_0.Ekl_z80_int1_uebestock",
  "Df5_0.Ekl_z120_int1_uebestock", "Df5_0.Ekl_z80_int12_uebestock",
  "Df5_0.Ekl_z80_int1_ubestock", "Df10_0.Ekl_z80_int1_ubestock",
  "Df5_0.Ekl_z120_int1_ubestock", "Df5_0.Ekl_z80_int12_ubestock",
  "Df5_0.Ekl_z80_int1_nbestock", "Df10_0.Ekl_z80_int1_nbestock",
  "Df5_0.Ekl_z120_int1_nbestock", "Df5_0.Ekl_z80_int12_nbestock",
  "Df5_II.Ekl_z80_int1_nbestock", "Df10_II.Ekl_z80_int1_nbestock",
  "Df5_II.Ekl_z120_int1_nbestock", "Df5_II.Ekl_z80_int12_nbestock",
  "Df5_II.Ekl_z80_int1_ubestock", "Df10_II.Ekl_z80_int1_ubestock",
  "Df5_II.Ekl_z120_int1_ubestock", "Df5_II.Ekl_z80_int12_ubestock",
  "Df5_II.Ekl_z80_int1_uebestock", "Df10_II.Ekl_z80_int1_uebestock",
  "Df5_II.Ekl_z120_int1_uebestock", "Df5_II.Ekl_z80_int12_uebestock"
)

for (tabelle in tabellen_namen) {
  df <- get(tabelle)
  
  # Nur filtern, wenn croptree existiert
  if ("croptree" %in% names(df)) {
    df_ZBaum <- df %>% filter(croptree == 1)
    assign(paste0(tabelle, "_ZBaum"), df_ZBaum, envir = .GlobalEnv)
  }
}

dbWriteTable(channel2, "Df5_I.Ekl_z80_int1_nbestock_ZBaum", Df5_I.Ekl_z80_int1_nbestock_ZBaum, overwrite=TRUE)
