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

#1 Höhe, BHD und Volumen für Berechnung mittels r-Paket rBDAT umformen
#2 Aushaltung (Berechnung des Volumens und Mittendurchmessers der Stammholzabschnitte) für mittlere
# schlechte Qualität 
#3 Aushaltung (Berechnung des Volumens und Mittendurchmessers der Stammholzabschnitte) bei einer
#  Wertästung, 6,5 und 12 m Höhe 
#___________________________________________________________________________________________#
#1 Höhe, BHD, und Volumen für Berechnung mittels r-Paket rBDAT umformen

Df5_I.Ekl_z80_int1_nbestock$height<-Df5_I.Ekl_z80_int1_nbestock$height/100
Df5_I.Ekl_z80_int1_nbestock$dbh<-Df5_I.Ekl_z80_int1_nbestock$dbh/1000
Df5_I.Ekl_z80_int1_nbestock$vol_1000<-Df5_I.Ekl_z80_int1_nbestock$vol_1000/1000
Df5_I.Ekl_z80_int1_nbestock$vol_efm<-Df5_I.Ekl_z80_int1_nbestock$vol_1000*0.78

#_____________________________________________________________________________________________#
#2 Aushaltung (Berechnung des Volumens und Mittendurchmessers der Stammholzabschnitte) für mittlere
# schlechte Qualität

install.packages("rBDAT")
library(rBDAT)
library(dplyr)
#2.1 Douglasien-Schaftkurvenfunktion auswählen und Spaltennamen für BHD und Baumhöhe zu "D1" und "H" ändern

Df5_I.Ekl_z80_int1_nbestock$BDATArtNr<-getSpeciesCode("Douglasie")
names(Df5_I.Ekl_z80_int1_nbestock)[names(Df5_I.Ekl_z80_int1_nbestock) == "dbh"] <- "D1"
names(Df5_I.Ekl_z80_int1_nbestock)[names(Df5_I.Ekl_z80_int1_nbestock) == "height"] <- "H"

#2.2 Stammholzvolumen und Mittendurchmesser, Abschnitt 0,5-19,5 m
#2.2.1 Stammholzvolumen
get_section_volume <- function(BDATArtNr, D1, H, start = 0.5, end = 19.5) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}

get_section_volume <- function(BDATArtNr, D1, H, start = 0.5, end = 19.5) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}


Df5_I.Ekl_z80_int1_nbestock$vol_19 <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A = 0.5, B = 19.5)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#2.2.2 Durchmesser in 10 m Höhe

Df5_I.Ekl_z80_int1_nbestock$diam_10m <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  getDiameter(tree, Hx = 10)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#2.2.3 Mittendurchmesser in Stärkeklasse umformen
adjust_d <- function(d) {
  d_floor <- floor(d)
  case_when(
    d_floor <= 20 ~ d_floor - 1,
    d_floor >= 21 & d_floor <= 37 ~ d_floor - 2,
    d_floor >= 38 & d_floor <= 53 ~ d_floor - 3,
    d_floor >= 54 & d_floor <= 70 ~ d_floor - 4,
    d_floor >= 71 ~ d_floor - 5,
    TRUE ~ NA_real_
  )
}
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(across(
    .cols = starts_with("abschnitt") & ends_with("_d"),
    .fns  = adjust_d,
    .names = "{.col}"  
  ))
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(diam_10m = adjust_d(diam_10m))


#2.3 Stammholzvolumen und Mittendurchmesser, Abschnitt 19,5-29,5 m

#2.3.1 Stammholzvolumen
get_section_volume <- function(BDATArtNr, D1, H, start = 19.5, end = 29.5) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}


get_section_volume <- function(BDATArtNr, D1, H, start = 19.5, end = 29.5) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}

Df5_I.Ekl_z80_int1_nbestock$vol_10 <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A = 19.5, B = 29.5)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)


#2.3.2 Mittendurchmesser, 24,5 m Höhe
Df5_I.Ekl_z80_int1_nbestock$diam_24.5m <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  getDiameter(tree, Hx = 24.5)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#2.3.3 Stärkeklasse für Mittendurchmesser ermitteln
adjust_d <- function(d) {
  d_floor <- floor(d)
  case_when(
    d_floor <= 20 ~ d_floor - 1,
    d_floor >= 21 & d_floor <= 37 ~ d_floor - 2,
    d_floor >= 38 & d_floor <= 53 ~ d_floor - 3,
    d_floor >= 54 & d_floor <= 70 ~ d_floor - 4,
    d_floor >= 71 ~ d_floor - 5,
    TRUE ~ NA_real_
  )
}
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(across(
    .cols = starts_with("abschnitt") & ends_with("_d"),
    .fns  = adjust_d,
    .names = "{.col}"   
  ))
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(diam_24.5m = adjust_d(diam_24.5m))



# Volumen IH, Abschnitt 29,5m - Endhöhe
#2.4.1 Volumen
get_section_volume <- function(BDATArtNr, D1, H, start = 29.5, end = H) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}

get_section_volume <- function(BDATArtNr, D1, H, start = 29.5, end = H) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}

Df5_I.Ekl_z80_int1_nbestock$rest <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A = 29.5, B = H)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

dbWriteTable(channel2, "Df5_I.Ekl_z80_int1_nbestock", Df5_I.Ekl_z80_int1_nbestock, overwrite=TRUE)

#3 Aushaltung (Berechnung des Volumens und Mittendurchmessers der Stammholzabschnitte) bei einer
#  Wertästung, 6,5 und 12 m Höhe 

#3.1 Stammholzvolumen, Abschnitt 0,5-12 m Höhe
#3.1.1 Stammholzvolumen
get_section_volume <- function(BDATArtNr, D1, H, start = 0.5, end = 12) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}


get_section_volume <- function(BDATArtNr, D1, H, start = 0.5, end = 12) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}


Df5_I.Ekl_z80_int1_nbestock$vol_1_gut <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A = 0.5, B = 12)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.1.2 Mittendurchmesser, 6,25 m Höhe (abgerundet auf 6 m) 

Df5_I.Ekl_z80_int1_nbestock$diam_6m <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  getDiameter(tree, Hx = 6)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.1.3 Stärkeklasse für Mittendurchmesser ermitteln
adjust_d <- function(d) {
  d_floor <- floor(d)
  case_when(
    d_floor <= 20 ~ d_floor - 1,
    d_floor >= 21 & d_floor <= 37 ~ d_floor - 2,
    d_floor >= 38 & d_floor <= 53 ~ d_floor - 3,
    d_floor >= 54 & d_floor <= 70 ~ d_floor - 4,
    d_floor >= 71 ~ d_floor - 5,
    TRUE ~ NA_real_
  )
}
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(across(
    .cols = starts_with("abschnitt") & ends_with("_d"),
    .fns  = adjust_d,
    .names = "{.col}"   
  ))
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(diam_6m = adjust_d(diam_6m))

#3.2 Stammholzvolumen, Abschnitt 0,5-6,5 m Höhe
#3.2.1 Stammholzvolumen
get_section_volume <- function(BDATArtNr, D1, H, start = 0.5, end = 6.5) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}


get_section_volume <- function(BDATArtNr, D1, H, start = 0.5, end = 6.5) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}


Df5_I.Ekl_z80_int1_nbestock$vol_1_gut <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A = 0.5, B = 6.5)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.2.2 Mittendurchmesser, 3,5 m Höhe

Df5_I.Ekl_z80_int1_nbestock$diam_6m <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  getDiameter(tree, Hx = 3.5)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.2.3 Stärkeklasse für Mittendurchmesser ermitteln
adjust_d <- function(d) {
  d_floor <- floor(d)
  case_when(
    d_floor <= 20 ~ d_floor - 1,
    d_floor >= 21 & d_floor <= 37 ~ d_floor - 2,
    d_floor >= 38 & d_floor <= 53 ~ d_floor - 3,
    d_floor >= 54 & d_floor <= 70 ~ d_floor - 4,
    d_floor >= 71 ~ d_floor - 5,
    TRUE ~ NA_real_
  )
}
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(across(
    .cols = starts_with("abschnitt") & ends_with("_d"),
    .fns  = adjust_d,
    .names = "{.col}"   
  ))
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(diam_3.5m = adjust_d(diam_3.5m))


#3.3 Stammholzvolumen, Abschnitt 12-22 m Höhe
#3.3.1 Stammholzvolumen
get_section_volume <- function(BDATArtNr, D1, H, start = 12, end = 22) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}


get_section_volume <- function(BDATArtNr, D1, H, start = 12, end = 22) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}


Df5_I.Ekl_z80_int1_nbestock$vol_1_gut <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A = 12, B = 22)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.3.2 Mittendurchmesser, 17 m Höhe 

Df5_I.Ekl_z80_int1_nbestock$diam_17m <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  getDiameter(tree, Hx = 17)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.3.3 Stärkeklasse für Mittendurchmesser ermitteln
adjust_d <- function(d) {
  d_floor <- floor(d)
  case_when(
    d_floor <= 20 ~ d_floor - 1,
    d_floor >= 21 & d_floor <= 37 ~ d_floor - 2,
    d_floor >= 38 & d_floor <= 53 ~ d_floor - 3,
    d_floor >= 54 & d_floor <= 70 ~ d_floor - 4,
    d_floor >= 71 ~ d_floor - 5,
    TRUE ~ NA_real_
  )
}
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(across(
    .cols = starts_with("abschnitt") & ends_with("_d"),
    .fns  = adjust_d,
    .names = "{.col}"   
  ))
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(diam_17m = adjust_d(diam_17m))

#3.4 Stammholzvolumen, Abschnitt 6,5-22 m Höhe
#3.4.1 Stammholzvolumen
get_section_volume <- function(BDATArtNr, D1, H, start = 6.5, end = 22) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}


get_section_volume <- function(BDATArtNr, D1, H, start =6.5, end = 22) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}


Df5_I.Ekl_z80_int1_nbestock$vol_1_gut <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A =6.5, B = 22)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.3.2 Mittendurchmesser, 14,25 m Höhe (abgerundet auf 14 m) 

Df5_I.Ekl_z80_int1_nbestock$diam_17m <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  getDiameter(tree, Hx = 14)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.3.3 Stärkeklasse für Mittendurchmesser ermitteln
adjust_d <- function(d) {
  d_floor <- floor(d)
  case_when(
    d_floor <= 20 ~ d_floor - 1,
    d_floor >= 21 & d_floor <= 37 ~ d_floor - 2,
    d_floor >= 38 & d_floor <= 53 ~ d_floor - 3,
    d_floor >= 54 & d_floor <= 70 ~ d_floor - 4,
    d_floor >= 71 ~ d_floor - 5,
    TRUE ~ NA_real_
  )
}
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(across(
    .cols = starts_with("abschnitt") & ends_with("_d"),
    .fns  = adjust_d,
    .names = "{.col}"   
  ))
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(diam_14m = adjust_d(diam_14m))


#3.5 Stammholzvolumen, Abschnitt 22-32 m Höhe
#3.5.1 Stammholzvolumen
get_section_volume <- function(BDATArtNr, D1, H, start = 22, end = 32) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}


get_section_volume <- function(BDATArtNr, D1, H, start =22, end = 32) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}


Df5_I.Ekl_z80_int1_nbestock$vol_1_gut <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A =22, B = 32)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.5.2 Mittendurchmesser, 27 m Höhe (abgerundet auf 27 m) 

Df5_I.Ekl_z80_int1_nbestock$diam_17m <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  getDiameter(tree, Hx = 27)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

#3.5.3 Stärkeklasse für Mittendurchmesser ermitteln
adjust_d <- function(d) {
  d_floor <- floor(d)
  case_when(
    d_floor <= 20 ~ d_floor - 1,
    d_floor >= 21 & d_floor <= 37 ~ d_floor - 2,
    d_floor >= 38 & d_floor <= 53 ~ d_floor - 3,
    d_floor >= 54 & d_floor <= 70 ~ d_floor - 4,
    d_floor >= 71 ~ d_floor - 5,
    TRUE ~ NA_real_
  )
}
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(across(
    .cols = starts_with("abschnitt") & ends_with("_d"),
    .fns  = adjust_d,
    .names = "{.col}"   
  ))
Df5_I.Ekl_z80_int1_nbestock <- Df5_I.Ekl_z80_int1_nbestock %>%
  mutate(diam_27m = adjust_d(diam_27m))


#3.6 Volumen IH-Abschnitt
#3.6.1 Volumen IH-Abschnitt
get_section_volume <- function(BDATArtNr, D1, H, start = 32, end = H) {
  getVolume(
    tree  = list(BDATArtNr = BDATArtNr, D1 = D1, H = H),
    start = start,
    end   = end
  )
}


get_section_volume <- function(BDATArtNr, D1, H, start =32, end = H) {
  tree <- datBDAT(data.frame(
    BDATArtNr = BDATArtNr,
    D1 = D1,
    H  = H
  ))
  
  getVolume(tree = tree, start = start, end = end)
}


Df5_I.Ekl_z80_int1_nbestock$vol_1_gut <- mapply(function(D1, H, BDATArtNr) {
  tree <- list(spp = BDATArtNr, D1 = D1, H = H)
  AB <- list(A =32, B = H)
  getVolume(tree, AB = AB, iAB = "H", bark = FALSE)
}, Df5_I.Ekl_z80_int1_nbestock$D1, Df5_I.Ekl_z80_int1_nbestock$H, Df5_I.Ekl_z80_int1_nbestock$BDATArtNr)

dbWriteTable(channel2, "Df5_I.Ekl_z80_int1_nbestock", Df5_I.Ekl_z80_int1_nbestock, overwrite=TRUE)


