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

#1 Laden der Tabelle und Bildung der Traktecken
#2 Bonitierung nach Weise
#3 Filtern der jeweiligen Ertragsklasse und Erstellen der drei Tabellen
#4 Umformen der Anfangsbuchstaben der Spaltenüberschriften zu Kleinbuchstaben
#5 Überschreiben der Tabellen in die Datenbank

#_________________________________________________________________________________________#
#1 Laden der Tabelle und Bildung der Traktecken
Dgl_Baeume_I.Ekl<-dbReadTable(channel2, "Dgl_Baeume_I.Ekl")
Dgl_Baeume <- Dgl_Baeume %>%mutate(Tnrenr = paste0(Tnr, Enr))

#____________________________________________________________________________________________#
#2 Bonitierung nach Weise
# Auswahl der 20 % stärksten Einzelbäume je Traktecke (hochgerechnet auf 1 ha)
Dgl_Baeume_Ekl <- Dgl_Baeume %>%
  group_by(tnrenr) %>%
  arrange(desc(Bhd), .by_group = TRUE) %>%
  mutate(
    N_ha_total = sum(N_ha),
    grenze = 0.2 * N_ha_total,
    N_ha_kum = cumsum(N_ha),
    N_ha_kum_vor = lag(N_ha_kum, default = 0),
    
    N_ha_top20 = case_when(
      N_ha_kum <= grenze ~ N_ha,
      N_ha_kum_vor < grenze ~ grenze - N_ha_kum_vor,
      TRUE ~ 0
    ),
    
    top20 = N_ha_top20 > 0,
# Zuweisung der Ertragsklasse nach der Ertragstafel von Bergel (1985), wobei zwischen 
#den Altern interpoliert wird. 0.Ekl = -0,5-0,49 Ekl, I.Ekl= 0,5-1,49. Ekl und II.Ekl = 1,5-2,49. Ekl
    Ekl = case_when(top20 & Al_ba == 41 & Hoehe >= 280 & Hoehe <= 325 ~ "0",
                    top20 & Al_ba == 42 & Hoehe >= 285 & Hoehe <= 331 ~ "0",
                    top20 & Al_ba == 43 & Hoehe >= 291 & Hoehe <= 337 ~ "0",
                    top20 & Al_ba == 44 & Hoehe >= 297 & Hoehe <= 343 ~ "0",
                    top20 & Al_ba == 45 & Hoehe >= 302 & Hoehe <= 349 ~ "0",
                    top20 & Al_ba == 46 & Hoehe >= 307 & Hoehe <= 355 ~ "0",
                    top20 & Al_ba == 47 & Hoehe >= 313 & Hoehe <= 360 ~ "0",
                    top20 & Al_ba == 48 & Hoehe >= 318 & Hoehe <= 366 ~ "0",
                    top20 & Al_ba == 49 & Hoehe >= 323 & Hoehe <= 372 ~ "0",
                    top20 & Al_ba == 50 & Hoehe >= 328 & Hoehe <= 377 ~ "0",
                    top20 & Al_ba == 51 & Hoehe >= 333 & Hoehe <= 382 ~ "0",
                    top20 & Al_ba == 52 & Hoehe >= 337 & Hoehe <= 386 ~ "0",
                    top20 & Al_ba == 53 & Hoehe >= 342 & Hoehe <= 391 ~ "0",
                    top20 & Al_ba == 54 & Hoehe >= 346 & Hoehe <= 396 ~ "0",
                    top20 & Al_ba == 55 & Hoehe >= 351 & Hoehe <= 400 ~ "0",
                    top20 & Al_ba == 56 & Hoehe >= 355 & Hoehe <= 405 ~ "0",
                    top20 & Al_ba == 57 & Hoehe >= 359 & Hoehe <= 409 ~ "0",
                    top20 & Al_ba == 58 & Hoehe >= 363 & Hoehe <= 414 ~ "0",
                    top20 & Al_ba == 59 & Hoehe >= 368 & Hoehe <= 418 ~ "0",
                    top20 & Al_ba == 60 & Hoehe >= 372 & Hoehe <= 422 ~ "0",
                    top20 & Al_ba == 41 & Hoehe >= 237 & Hoehe <= 279 ~ "I",
                    top20 & Al_ba == 42 & Hoehe >= 242 & Hoehe <= 284 ~ "I",
                    top20 & Al_ba == 43 & Hoehe >= 247 & Hoehe <= 290 ~ "I",
                    top20 & Al_ba == 44 & Hoehe >= 252 & Hoehe <= 296 ~ "I",
                    top20 & Al_ba == 45 & Hoehe >= 257 & Hoehe <= 301 ~ "I",
                    top20 & Al_ba == 46 & Hoehe >= 262 & Hoehe <= 306 ~ "I",
                    top20 & Al_ba == 47 & Hoehe >= 267 & Hoehe <= 312 ~ "I",
                    top20 & Al_ba == 48 & Hoehe >= 272 & Hoehe <= 317 ~ "I",
                    top20 & Al_ba == 49 & Hoehe >= 277 & Hoehe <= 322 ~ "I",
                    top20 & Al_ba == 50 & Hoehe >= 282 & Hoehe <= 327 ~ "I",
                    top20 & Al_ba == 51 & Hoehe >= 286 & Hoehe <= 332 ~ "I",
                    top20 & Al_ba == 52 & Hoehe >= 290 & Hoehe <= 336 ~ "I",
                    top20 & Al_ba == 53 & Hoehe >= 295 & Hoehe <= 341 ~ "I",
                    top20 & Al_ba == 54 & Hoehe >= 299 & Hoehe <= 345 ~ "I",
                    top20 & Al_ba == 55 & Hoehe >= 303 & Hoehe <= 350 ~ "I",
                    top20 & Al_ba == 56 & Hoehe >= 307 & Hoehe <= 354 ~ "I",
                    top20 & Al_ba == 57 & Hoehe >= 311 & Hoehe <= 358 ~ "I",
                    top20 & Al_ba == 58 & Hoehe >= 315 & Hoehe <= 362 ~ "I",
                    top20 & Al_ba == 59 & Hoehe >= 319 & Hoehe <= 367 ~ "I",
                    top20 & Al_ba == 60 & Hoehe >= 323 & Hoehe <= 371 ~ "I",
                    top20 & Al_ba == 41 & Hoehe >= 198 & Hoehe <= 236 ~ "II",
                    top20 & Al_ba == 42 & Hoehe >= 203 & Hoehe <= 241 ~ "II",
                    top20 & Al_ba == 43 & Hoehe >= 207 & Hoehe <= 246 ~ "II",
                    top20 & Al_ba == 44 & Hoehe >= 212 & Hoehe <= 251 ~ "II",
                    top20 & Al_ba == 45 & Hoehe >= 217 & Hoehe <= 256 ~ "II",
                    top20 & Al_ba == 46 & Hoehe >= 221 & Hoehe <= 261 ~ "II",
                    top20 & Al_ba == 47 & Hoehe >= 226 & Hoehe <= 266 ~ "II",
                    top20 & Al_ba == 48 & Hoehe >= 230 & Hoehe <= 271 ~ "II",
                    top20 & Al_ba == 49 & Hoehe >= 235 & Hoehe <= 276 ~ "II",
                    top20 & Al_ba == 50 & Hoehe >= 239 & Hoehe <= 281 ~ "II",
                    top20 & Al_ba == 51 & Hoehe >= 243 & Hoehe <= 285 ~ "II",
                    top20 & Al_ba == 52 & Hoehe >= 247 & Hoehe <= 289 ~ "II",
                    top20 & Al_ba == 53 & Hoehe >= 252 & Hoehe <= 294 ~ "II",
                    top20 & Al_ba == 54 & Hoehe >= 256 & Hoehe <= 298 ~ "II",
                    top20 & Al_ba == 55 & Hoehe >= 260 & Hoehe <= 302 ~ "II",
                    top20 & Al_ba == 56 & Hoehe >= 264 & Hoehe <= 306 ~ "II",
                    top20 & Al_ba == 57 & Hoehe >= 268 & Hoehe <= 310 ~ "II",
                    top20 & Al_ba == 58 & Hoehe >= 271 & Hoehe <= 314 ~ "II",
                    top20 & Al_ba == 59 & Hoehe >= 275 & Hoehe <= 318 ~ "II",
                    top20 & Al_ba == 60 & Hoehe >= 279 & Hoehe <= 322 ~ "II",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

#_________________________________________________________________________________________#
#3 Filtern der jeweiligen Ertragsklasse und Erstellen der drei Tabellen
Dgl_Baeume_0.Ekl  <- Dgl_Baeume %>% filter(Ekl == "0")
Dgl_Baeume_I.Ekl  <- Dgl_Baeume %>% filter(Ekl == "I")
Dgl_Baeume_II.Ekl <- Dgl_Baeume %>% filter(Ekl == "II")


#__________________________________________________________________________________________#
#4 Umformen der Anfangsbuchstaben der Spaltenüberschriften zu Kleinbuchstaben
names(Dgl_Baeume_0.Ekl) <- paste0(tolower(substr(names(Dgl_Baeume_0.Ekl), 1, 1)),substr(names(Dgl_Baeume_0.Ekl), 2, nchar(names(Dgl_Baeume_0.Ekl))))
names(Dgl_Baeume_I.Ekl) <- paste0(tolower(substr(names(Dgl_Baeume_I.Ekl), 1, 1)),substr(names(Dgl_Baeume_I.Ekl), 2, nchar(names(Dgl_Baeume_I.Ekl))))
names(Dgl_Baeume_II.Ekl) <- paste0(tolower(substr(names(Dgl_Baeume_II.Ekl), 1, 1)),substr(names(Dgl_Baeume_II.Ekl), 2, nchar(names(Dgl_Baeume_II.Ekl))))

#__________________________________________________________________________________________#
#5 Überschreiben der Tabellen in die Datenbank
dbWriteTable(channel2, "Dgl_Baeume_0.Ekl", Dgl_Baeume_0.Ekl, overwrite = TRUE)
dbWriteTable(channel2, "Dgl_Baeume_I.Ekl", Dgl_Baeume_I.Ekl, overwrite = TRUE)
dbWriteTable(channel2, "Dgl_Baeume_II.Ekl", Dgl_Baeume_II.Ekl, overwrite = TRUE)

