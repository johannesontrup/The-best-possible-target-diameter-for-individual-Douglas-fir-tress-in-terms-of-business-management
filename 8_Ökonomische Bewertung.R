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

#1 Laden der Tabellen aus der Datenbank
#2 Bestimmung des Alters beim Zieldurchmesser
#3 Bestimmung des Abtriebswertes beim Zieldurchmesser
#4 Berechnung des Einzelbaum-Erwartungswertes
#5 Integralberechnung für ökonomischen Nutzenentgang
#6 Berechnung der Annuität des Einzelbaum-Erwartungswertes
#__________________________________________________________________________________________________#

# 1 Laden der Tabellen aus der Datenbank

Df5_I.Ekl_z80_int1_nbestock_ZBaum<-dbReadTable(channel2, "Df5_I.Ekl_z80_int1_nbestock_ZBaum")

#_____________________________________________________________________________________________________#

#2 Bestimmung des Alters beim Zieldurchmesser

library(dplyr)
library(minpack.lm)


#2.1 Chapman-Richards-Funktion und Inverse

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

age_from_D_chapman <- function(D_target, a, b, c) {
  if (is.na(a) || is.na(b) || is.na(c) || D_target <= 0 || D_target >= a) return(NA_real_)
  -log(1 - (D_target / a)^(1/c)) / b
}


#2.2 Fit-Funktion für die Tabelle

fit_chapman <- function(df) {
  df_model <- df %>% filter(!is.na(age), !is.na(D1), age > 0, D1 > 0)
  if (nrow(df_model) < 6) return(NULL)
  
  start_a <- max(df_model$D1, na.rm = TRUE)
  start_b <- 0.005
  start_c <- 2
  
  fit_try <- tryCatch(
    nlsLM(D1 ~ a * (1 - exp(-b * age))^c,
          data = df_model,
          start = list(a = start_a, b = start_b, c = start_c),
          control = nls.lm.control(maxiter = 1000)),
    error = function(e) e
  )
  
  if (inherits(fit_try, "error")) return(NULL)
  
  coef(fit_try)  
}


#2.3 Berechnung des Alters für gegebenen Zieldurchmesser

compute_age <- function(df, D_target) {
  pars <- fit_chapman(df)
  
  if (is.null(pars)) return(data.frame(D_target = D_target, age = NA_real_))
  
  age <- age_from_D_chapman(D_target, pars["a"], pars["b"], pars["c"])
  
  data.frame(
    D_target = D_target,
    age = round(age, 2)
  )
}


#2.4 Tabelle und Zieldurchmesser eintragen

df<-Df5_I.Ekl_z80_int1_nbestock_ZBaum

D_target <-66.8  


#2.5 Alter berechnen lassen
result <- compute_age(df, D_target)
result

#__________________________________________________________________________________________________#

#3 Bestimmung des Abtriebswertes beim Zieldurchmesser

library(dplyr)
library(minpack.lm)


#3.1 Definition der Funktion zur Bestimmmung des Abtriebswertes

get_Ax_for_D <- function(df, D_target, response, scale_factor = 1000) {
  
  df_model <- df %>%
    select(D1, !!sym(response)) %>%
    rename(Ax = !!sym(response)) %>%
    filter(
      !is.na(D1), !is.na(Ax),
      D1 > 0, Ax > 0, D1 <= 80
    ) %>%
    mutate(Ax_scaled = Ax / scale_factor)
  
  # Chapman-Richards-Fit
  fit <- nlsLM(
    Ax_scaled ~ a * (1 - exp(-b * D1))^c,
    data = df_model,
    start = list(
      a = max(df_model$Ax_scaled),
      b = 0.01,
      c = 2
    ),
    control = nls.lm.control(maxiter = 500)
  )
  
  pars <- coef(fit)
  
  # Ax am Ziel-Durchmesser
  Ax_val <- pars["a"] *
    (1 - exp(-pars["b"] * D_target))^pars["c"] *
    scale_factor
  
  return(as.numeric(Ax_val))
}


#3.2 Auswahl der Qualitäten


varianten <- tibble(
  Variante = c(
    "Wertastung 12 m +15 %",
    "Wertastung 12 m",
    "Wertastung 12 m -15 %",
    "Wertastung 6.5 m +15 %",
    "Wertastung 6.5 m",
    "Wertastung 6.5 m -15 %"
  ),
  
  Ax_col = c(
    "Ax_WAs12_gut",
    "Ax_WAs12",
    "Ax_WAs12_schlecht",
    "Ax_WAs6.5_gut",
    "Ax_WAs6.5",
    "Ax_WAs6.5_schlecht"
  ),
  
  D_Ziel = c(
    70.8, 
    70.7, 
    70.4,  
    67.8,
    67.4,
    66.8
  )
)


#3.3 Auswahl der Z-Bäume

df <- Df5_I.Ekl_z80_int1_nbestock_ZBaum

#3.4 Anwendung der in 3.1 definierten Funktion
Ax_results <- varianten %>%
  rowwise() %>%
  mutate(
    Ax_Ziel = get_Ax_for_D(
      df = df,
      D_target = D_Ziel,
      response = Ax_col
    )
  ) %>%
  ungroup()


#3.5 Ergebnisausgabe

Ax_results %>%
  mutate(Ax_Ziel = round(Ax_Ziel, 2)) %>%
  print(n = Inf)




#____________________________________________________________________________________________________#

#4 Berechnung des Einzelbaum-Erwartungswertes

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)


#4.1 Chapman-Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}


#4.2 Fit-Funktion für Wertzuwachs und Opportunitätskosten

fit_wzw_Opp.Kosten_pair <- function(df, name, Ax_col, bb_col, i = 0.03) {
  
  df_model <- df %>%
    select(D1, !!sym(Ax_col), !!sym(bb_col)) %>%
    rename(Ax_mittel = !!sym(Ax_col),
           bb = !!sym(bb_col)) %>%
    filter(!is.na(D1), !is.na(Ax_mittel), !is.na(bb),
           D1 > 0, D1 <= 80, Ax_mittel > 0, bb > 0)
  
  # Fit Ax (für WZW)
  fit_Ax <- nlsLM(
    Ax_mittel ~ a * (1 - exp(-b * D1))^c,
    data = df_model,
    start = list(a = max(df_model$Ax_mittel),
                 b = 0.01,
                 c = 2),
    control = nls.lm.control(maxiter = 1000)
  )
  ca <- coef(fit_Ax)
  WZW_func <- function(D) chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  
  # Fit Opp-Kosten (Ax*i + bb)
  fit_Opp <- nlsLM(
    I(Ax_mittel * i + bb) ~ a * (1 - exp(-b * D1))^c,
    data = df_model,
    start = list(a = max(df_model$Ax_mittel * i + df_model$bb),
                 b = 0.01,
                 c = 2),
    control = nls.lm.control(maxiter = 1000)
  )
  cb <- coef(fit_Opp)
  Opp_func <- function(D) chapman(D, cb["a"], cb["b"], cb["c"])
  
  list(
    name = name,
    WZW_func = WZW_func,
    Opp_func = Opp_func
  )
}


#4.3 Ermittlung des Zieldurchmessers und Berechnung des Integrals von BHD = 45cm bis Zieldurchmesser
integral_analysis <- function(fit_result, D1_min = 45, D1_max = 80) {
  
  f1 <- fit_result$WZW_func
  f2 <- fit_result$Opp_func
  f_diff <- function(D) f1(D) - f2(D)
  
  # Schnittpunkt suchen
  root <- tryCatch(
    uniroot(f_diff, interval = c(D1_min, D1_max))$root,
    error = function(e) {
      
      D_seq <- seq(D1_min, D1_max, by = 0.01)
      D_seq[which.min(abs(f_diff(D_seq)))]
    }
  )
  
  # Integral berechnen
  integral_val <- integrate(f_diff, lower = D1_min, upper = root)$value
  
  data.frame(
    Variante = fit_result$name,
    Schnittpunkt = root,
    Integral = integral_val
  )
}


#4.4 Auswahl der Z-Bäume

df <- Df5_I.Ekl_z80_int1_nbestock_ZBaum

varianten <- list(
  "Mittel"     = list(Ax = "Ax_mittel",    bb = "bb")
  
)


#4.5 Fits durchführen

fits <- lapply(names(varianten), function(nm) {
  v <- varianten[[nm]]
  fit_wzw_Opp.Kosten_pair(df, nm, v$Ax, v$bb, i = 0.03)
})


#4.6 Integralberechnung

results <- bind_rows(lapply(fits, integral_analysis, D1_min = 45, D1_max = 80))

#4.7 Ergebnisausgabe
print(results)

#_____________________________________________________________________________________________________#

#5 Integralberechnung für ökonomischen Nutzenentgang (>1cm, >5cm und >10 cm Zieldurchmesser) 


library(dplyr)
library(minpack.lm)
library(tibble)


#5.1 Chapman-Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}


#5.2 Fit für Wertzuwachs und Opportunitätskosten

fit_wzw_oppkosten_pair <- function(df, name, Ax_col, bb_col, i = 0.03) {
  
  df_model <- df %>%
    select(D1, !!sym(Ax_col), !!sym(bb_col)) %>%
    rename(Ax_mittel = !!sym(Ax_col), bb = !!sym(bb_col)) %>%
    filter(!is.na(D1), !is.na(Ax_mittel), !is.na(bb),
           D1 > 0, D1 <= 80, Ax_mittel > 0, bb > 0) %>%
    mutate(Opp.Kosten_values = Ax_mittel * i + bb)
  
  # ---- Fit Abtriebswert (Ax)
  fit_Ax <- nlsLM(
    Ax_mittel ~ a * (1 - exp(-b * D1))^c,
    data = df_model,
    start = list(a = max(df_model$Ax_mittel), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 200)
  )
  ca <- coef(fit_Ax)
  
  WZW_func <- function(D) {
    chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  }
  
  # ---- Fit Opportunitätskosten (NIVEAU)
  fit_Opp <- nlsLM(
    Opp.Kosten_values ~ a * (1 - exp(-b * D1))^c,
    data = df_model,
    start = list(a = max(df_model$Opp.Kosten_values), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 200)
  )
  cb <- coef(fit_Opp)
  
  Opp_func <- function(D) {
    chapman(D, cb["a"], cb["b"], cb["c"])
  }
  
  list(
    name = name,
    WZW_func = WZW_func,
    Opp_func = Opp_func,
    D_range = range(df_model$D1)
  )
}


#5.3 Zieldurchmesser bestimmen

find_target_diameter <- function(fit_result,
                                 D_min = 45,
                                 D_max = 80) {
  
  f_diff <- function(D) {
    fit_result$WZW_func(D) - fit_result$Opp_func(D)
  }
  
  if (f_diff(D_min) * f_diff(D_max) < 0) {
    uniroot(f_diff, interval = c(D_min, D_max))$root
  } else {
    NA
  }
}


#5.4 Integralberechnung ab Zieldurchmesser

integral_from_target <- function(fit_result,
                                 D_min = 45,
                                 D_max = 80,
                                 deltas = c(1, 5, 10)) {
  
  D_target <- find_target_diameter(fit_result, D_min, D_max)
  
  if (is.na(D_target)) {
    return(tibble(
      Variante = fit_result$name,
      `Zieldurchmesser [cm]` = NA,
      `Δ [cm]` = deltas,
      `Obergrenze [cm]` = NA,
      `Integralflaeche (WZW - Opp)` = NA
    ))
  }
  
  f1 <- fit_result$WZW_func
  f2 <- fit_result$Opp_func
  
  results <- lapply(deltas, function(d) {
    
    upper <- D_target + d
    
    integral_val <- tryCatch(
      integrate(function(D) f1(D) - f2(D),
                lower = D_target,
                upper = upper)$value,
      error = function(e) NA
    )
    
    tibble(
      Variante = fit_result$name,
      `Zieldurchmesser [cm]` = round(D_target, 2),
      `Δ [cm]` = d,
      `Obergrenze [cm]` = round(upper, 2),
      `Integralflaeche (WZW - Opp)` = round(integral_val, 4)
    )
  })
  
  bind_rows(results)
}


#5.5 Auswahl der Z-Bäume und Qualitäten

df <- Df5_I.Ekl_z80_int1_nbestock_ZBaum

varianten <- list(
  "Wertastung 12 m +15 %"     = list(Ax = "Ax_WAs12_gut",    bb = "bb"),
  "Wertastung 12 m"    = list(Ax = "Ax_WAs12",   bb = "bb"),
  "Wertastung 12 m -15 %"   = list(Ax = "Ax_WAs12_schlecht",   bb = "bb"),
  "Wertastung 6,5 m +15 %"  = list(Ax = "Ax_WAs6.5_gut", bb = "bb"),
  "Wertastung 6,5 m" = list(Ax="Ax_WAs6.5", bb= "bb"),
  "Wertastung 6,5 m -15 %"  = list(Ax= "Ax_WAs6.5_schlecht", bb= "bb")
)

#5.6 Berechnung der Fits und Integrale
fits <- lapply(names(varianten), function(nm) {
  v <- varianten[[nm]]
  fit_wzw_oppkosten_pair(df, nm, v$Ax, v$bb, i = 0.03)
})


results <- bind_rows(
  lapply(fits, integral_from_target,
         D_min = 45,
         D_max = 80,
         deltas = c(1, 5, 10))
)


#5.7 Ergebnisausgabe

results %>%
  arrange(Variante, `Δ [cm]`) %>%
  print(n = Inf)

#______________________________________________________________________________________________________#
#6 Berechnung der Annuität des Einzelbaum-Erwartungswertes

library(dplyr)
library(minpack.lm)
library(nlme)


#6.1 Chapman-Richards-Hilfsfunktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}

# Inverse Chapman-Richards: Alter aus Durchmesser
age_from_D_chapman <- function(D_target, a, b, c) {
  if (is.na(a) || is.na(b) || is.na(c) || D_target <= 0 || D_target >= a) return(NA_real_)
  age <- -log(1 - (D_target / a)^(1/c)) / b
  return(age)
}


#6.2 Fit-Funktion für eine Variante

fit_chapman_variant <- function(df, Ax_col) {
  df_model <- df %>%
    select(age, D1, !!sym(Ax_col)) %>%
    filter(!is.na(age), !is.na(D1), age > 0, D1 > 0)
  
  if (nrow(df_model) < 6) return(NULL)
  
  # Startwerte
  start_a <- max(df_model$D1, na.rm = TRUE)
  start_b <- 0.005
  start_c <- 2
  
  # Fit Chapman-Richards (nlsLM)
  fit_try <- tryCatch(
    nlsLM(D1 ~ a * (1 - exp(-b * age))^c,
          data = df_model,
          start = list(a = start_a, b = start_b, c = start_c),
          control = nls.lm.control(maxiter = 1000)),
    error = function(e) e
  )
  
  if (inherits(fit_try, "error")) return(NULL)
  
  pars <- coef(fit_try)
  list(a = pars["a"], b = pars["b"], c = pars["c"])
}


#6.3 Z-Bäume und deren Qualitäten sowie Zieldurchmesser und Kapitalwerte eintragen

df <- Df5_I.Ekl_z80_int1_nbestock_ZBaum

varianten <- list(
  "Wertastung 12 m +15 %"     = list(Ax = "Ax_WAs12_gut",    Zield= 70.8, KW = 121.05),
  "Wertastung 12 m"    = list(Ax = "Ax_WAs12", Zield= 70.7, KW = 99.70),
  "Wertastung 12 m -15 %"   = list(Ax = "Ax_WAs12_schlecht", Zield= 70.4, KW= 78.30),
  "Wertastung 6,5 m +15 %"  = list(Ax = "Ax_WAs6.5_gut", Zield= 67.8, KW= 78.05),
  "Wertastung 6,5 m" = list(Ax="Ax_WAs6.5", Zield= 67.4, KW= 65.92),
  "Wertastung 6,5 m -15 %"  = list(Ax= "Ax_WAs6.5_schlecht", Zield= 66.8, KW= 53.86)
)

# Zinssatz
i <- 0.03


#6.4 Zeitraumberechnung und Kalkulation der Annuität

results <- lapply(names(varianten), function(var) {
  pars <- fit_chapman_variant(df, Ax_col = varianten[[var]]$Ax)
  if (is.null(pars)) return(data.frame(Variante = var, age_45 = NA, age_Ziel = NA,
                                       Altersdifferenz = NA, Annuitaet = NA))
  
  # Alter bei BHD = 45 cm
  age_45 <- age_from_D_chapman(45, pars$a, pars$b, pars$c)
  
  # Alter beim Zieldurchmesser
  D_ziel <- varianten[[var]]$Zield
  age_ziel <- age_from_D_chapman(D_ziel, pars$a, pars$b, pars$c)
  
  # Altersdifferenz
  Altersdifferenz <- age_ziel - age_45
  
  # Kapitalwert hier selbst eintragen
  KW <- varianten[[var]]$KW
  if (is.na(KW)) Annuitaet <- NA_real_ else {
    # Annuität berechnen
    WGF <- (i * (1 + i)^Altersdifferenz) / ((1 + i)^Altersdifferenz - 1)
    Annuitaet <- KW * WGF
  }
  
  data.frame(
    Variante = var,
    age_45 = round(age_45, 2),
    age_Ziel = round(age_ziel, 2),
    Altersdifferenz = round(Altersdifferenz, 2),
    Annuitaet = round(Annuitaet, 2)
  )
})

#6.5 Ergebnisausgabe
results_df <- bind_rows(results)
print(results_df)

