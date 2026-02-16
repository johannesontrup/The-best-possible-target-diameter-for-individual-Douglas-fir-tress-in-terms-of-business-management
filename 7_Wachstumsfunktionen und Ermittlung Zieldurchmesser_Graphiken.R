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

#1 Laden und Vorbereiten der Daten
#2 Durchmesserwachstum und -zuwachs
#3 Vorratswachstum und -zuwachs
#4 Bewertetes Produktionsmodell
#5 Zieldurchmesser bei Boden- und Kapitalknappheit
#6 Darstellung der Integrale (Ökonomischer Nutzen und Nutzenentgang)
#7 Vergleich der Ziedurchmesser unterschiedlicher Ertragsklassen
#8 Vergleich der Zieldurchmesser unterschiedlicher Bestandesdichten
#9 Vergleich der Zieldurchmesser unterschiedlicher Durchforstungsvarianten
#10 Vergleich der Zieldurchmesser einer durchforsteten und undurchforsteten Variante
#11 Vergleich der Zieldurchmesser unterschiedlicher Qualitäten
#12 Vergleich von Preiszu- und Preisabschlag bei einer Wertästung bis 12 m Höhe
#13 Vergleich von Preiszu- und Preisabschlag bei einer Wertästung bis 6,5 m Höhe
#_____________________________________________________________________________________________#
#1 Laden und Vorbereiten der Daten


Df5_I.Ekl_z80_int1_nbestock <-dbReadTable(channel2, "Df5_I.Ekl_z80_int1_nbestock")

Validierung_Baeume<-dbReadTable(channel2, "Validierung_Baeume")
Validierung_Baeume <- Validierung_Baeume %>%
  rename(age = Al_ba)
Validierung_Baeume <- Validierung_Baeume %>% rename (D1= Bhd)
Validierung_Baeume$D1<- Validierung_Baeume$D1/10
Validierung_Baeume <- Validierung_Baeume %>% rename(vol_1000=VolR)

#_________________________________________________________________________________________#
#2 Durchmesserwachstum und -zuwachs

#

library(dplyr)
library(ggplot2)
library(minpack.lm)
library(nlme)

# 2.1 Daten vorbereiten


data_model <- Df5_I.Ekl_z80_int1_nbestock_ZBaum %>%
  select(age, D1) %>%
  filter(!is.na(age), !is.na(D1), age > 0, age< 100, D1 > 0, D1<=120)

# 2.2 Durchmesser skalieren


scale_factor <- 50
data_model <- data_model %>%
  mutate(D1_scaled = D1 / scale_factor)


# 2.3 Startwerte schätzen 


nls_fit <- nlsLM(
  D1_scaled ~ a * (1 - exp(-b * age))^c,
  data = data_model,
  start = list(a = max(data_model$D1_scaled), b = 0.005, c = 2),
  control = nls.lm.control(maxiter = 500)
)

start_vals <- coef(nls_fit)

#2.4 Parameter werden über Levenberg-Marquardt Methode geschätzt
 chapman_model <- nlsLM(
    D1_scaled ~ a * (1 - exp(-b * age))^c,
    data = data_model,
    start = start_vals,
    control = nls.lm.control(maxiter = 1000)
  )


model_summary <- summary(chapman_model)
model_summary

#2.5 Zurückskalieren


data_model <- data_model %>%
  mutate(predicted_D = predict(chapman_model) * scale_factor)


#2.6 Parameter und p-Werte extrahieren 


coef_table <- model_summary$coefficients


value_col <- grep("Value|Estimate", colnames(coef_table), value = TRUE)
p_col     <- grep("p-value|Pr", colnames(coef_table), value = TRUE)

a_val <- round(coef_table["a", value_col], 3)
b_val <- round(coef_table["b", value_col], 3)
c_val <- round(coef_table["c", value_col], 3)

p_a <- signif(coef_table["a", p_col], 3)
p_b <- signif(coef_table["b", p_col], 3)
p_c <- signif(coef_table["c", p_col], 3)


# Text für Plot inkl. RSE
text_label <- paste0(
  "a = ", 109.7,"\n",
  "b = ", 0.023, "\n",
  "c = ", c_val, "\n",
  "RSE = ", "10.4 cm"
)


#2.7 Plot erstellen

# 2.7.1 Box-Position der Parameter-Werte
x_box <-0.88 * max(data_model$age)    
y_box_bhd <- min(data_model$D1) + 1     
label_bhd <- data.frame(x = x_box, y = y_box_bhd, label = text_label)
#2.7.2 Plot
plot_bhd<-ggplot(data_model %>% filter(age <= 125), aes(x = age, y = D1)) +
  geom_point(color = "lightgray", alpha = 0.7) +
  geom_line(aes(y = predicted_D), color = "blue", size = 1) +
  geom_label(data = label_bhd, aes(x = x, y = y, label = label),
             hjust = 0, vjust = 0, size = 5, fill = "white", color = "black", linewidth = 0.5) +
  labs( x = "Alter [a]", y = "BHD [cm]" )+
  scale_x_continuous(breaks = seq(0, 130, 10)) +
  scale_y_continuous(breaks = seq(30, 150, 10)) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

plot_bhd

# 2.8 VALIDIERUNG 


# 2.8.1 Daten der Validierung (Tabelle Validierung_Baeume) vorbereiten
punkte_rot <- Validierung_Baeume %>%
  filter(Ekl == "I", b0 > 0.9, b0 <= 1.1)


D1_q75 <- quantile(punkte_rot$D1, 0.75, na.rm = TRUE)

punkte_rot_top25 <- punkte_rot %>%
  filter(!is.na(age), !is.na(D1)) %>%   
  group_by(age) %>%                     
  filter(D1 >= quantile(D1, 0.75)) %>%  
  ungroup() %>%                          
  select(age, D1)
punkte_rot_top25


# 2.8.2 RMSE + nRMSE berechnen

punkte_rot_top25 <- punkte_rot_top25 %>%
  mutate(
    pred_scaled = predict(chapman_model, newdata = .),
    pred_D = pred_scaled * scale_factor,
    resid = D1 - pred_D
  )

RMSE  <- sqrt(mean(punkte_rot_top25$resid^2))
rRMSE <- RMSE / mean(punkte_rot_top25$D1) * 100

RMSE_round  <- round(RMSE, 2)
rRMSE_round <- round(rRMSE, 2)

RMSE_round
rRMSE_round



text_label_val <- paste0(
  "RMSE  = 7.0 cm"
)

# 2.8.3 Validierungsplot erstellen
# Box-Position
x_box <- 0.87 * max(data_model$age)
y_box_val <- min(data_model$D1) + 0.25

label_val <- data.frame(
  x = x_box,
  y = y_box_val,
  label = text_label_val
)

# VALIDIERUNGS-PLOT

plot_val_d <- ggplot(data_model, aes(x = age, y = D1)) +
  
  geom_point(color = "lightgray", alpha = 0.7) +                     
  geom_line(aes(y = predicted_D), color = "blue", size = 1) +       
  
  geom_point(data = punkte_rot_top25,                                      
             aes(x = age, y = D1),
             color = "red", size = 2.5, alpha = 0.8) +
  
  geom_label(data = label_val,                                       
             aes(x = x, y = y, label = label),
             hjust = 0, vjust = 0,
             size = 5,
             fill = "white", color = "black",
             linewidth = 0.5) +
  
  labs(
    x = "Alter [a]",
    y =  expression(BHD~"["*cm~Eb^{-1}*"]") 
  ) +
  
  scale_x_continuous(breaks = seq(0, 130, 10)) +
  scale_y_continuous(breaks = seq(30, 120, 10)) +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

plot_val_d


# 2.9 laufender Durchmesserzuwachs (1. Ableitung)

# 2.9.1 Ableitungfunktion definieren 
chapman_deriv <- function(age, a, b, c) {
  a * c * (1 - exp(-b * age))^(c - 1) * b * exp(-b * age)
}

# 2.9.2 Vorhersage der Wachstumsrate auf Originalmaßstab 
data_model <- data_model %>%
  mutate(predicted_growth = chapman_deriv(age, coef(chapman_model)["a"], 
                                          coef(chapman_model)["b"], 
                                          coef(chapman_model)["c"]) * scale_factor)

# 2.9.3 Plot des laufenden Durchmesserzuwachses
plot_growth <- ggplot(data_model %>% filter(age <= 100), aes(x = age, y = predicted_growth)) +
  geom_line(color = "blue", size = 1) +
  labs( x = "Alter [a]", y =  expression(Durchmesserzuwachs (BHD)~"["*cm~a^{-1}*"]"))  +
  scale_x_continuous(breaks = seq(0, 130, 10)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.1)) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

plot_growth

#2.10 Plots zusammenfügen
library(patchwork)
library(ggplot2)
combined_plot<- plot_bhd |plot_growth
combined_plot <- plot_bhd + plot_spacer() + plot_growth +
  plot_layout(ncol = 3, widths = c(1, 0.05, 1))
ggsave("BHD_Zuwachs_Abbildung.png", combined_plot, width=14, height=6, dpi=300)

#____________________________________________________________________________________________#

#3 Vorratswachstum und -zuwachs in Abhängigkeit des Alters
library(dplyr)
library(minpack.lm)
library(ggplot2)


#3.1 Daten vorbereiten

data_model <- Df5_I.Ekl_z80_int1_nbestock_ZBaum %>%
  select(D1, vol_1000) %>%
  filter(!is.na(D1), !is.na(vol_1000), D1 > 0, vol_1000 > 0, D1 <= 80)


#3.2 Skalierung
scale_factor <- 1000
data_model <- data_model %>%
  mutate(vol_scaled = vol_1000 / scale_factor)


#3.3 Allometrisches Modell fitten
allo_fit <- nlsLM(
  vol_scaled ~ a * D1^b,
  data = data_model,
  start = list(a = 0.001, b = 2),
  control = nls.lm.control(maxiter = 1000)
)

model_summary <- summary(allo_fit)
coef_table <- model_summary$coefficients
model_summary

#3.4 Parameterwerte extrahieren
a_val <- round(coef_table["a","Estimate"], 3)
b_val <- round(coef_table["b","Estimate"], 3)

# Text für Plot-Textbox
text_label <- paste0(
  "a = ", 0.0004, "\n",
  "b = ", b_val, "\n",
  "RSE = 0.39 Vfm"
)

#3.4 Vorhersage zurückskalieren
data_model <- data_model %>%
  mutate(pred_allo = predict(allo_fit) * scale_factor)
data_model

mean_vol <- mean(data_model$vol_1000, na.rm = TRUE)
mean_vol


#3.5 Plot
# Position der Box festlegen

x_box <- 0.86 * max(data_model$D1)
y_box <- max(data_model$vol_1000) * 0.10  # 10 % über der x-Achse

label_df <- data.frame(
  x = x_box,
  y = y_box,
  label = text_label
)


# Plot erstellen

plot_allo <- ggplot(data_model, aes(x = D1, y = vol_1000)) +
  geom_point(color = "grey70", alpha = 0.3) +
  geom_line(aes(y = pred_allo), color = "blue", size = 1.2) +
  geom_label(
    data = label_df,
    aes(x = x, y = y, label = label),
    size = 5, fill = "white", color = "black",
    hjust = 0, vjust = 0, label.size = 0.6
  ) +
  labs(
    x = "BHD [cm]",
    y =  expression(Vorrat~"["*Vfm~Eb^{-1}*"]")
  ) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(data_model$vol_1000)), 1)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 1)
  )

plot_allo


#3.6 Validierung


# 3.6.1 Validierungsdaten auswählen

val_data <- Validierung_Baeume %>%
  filter(Ekl == "I", b0 > 0.9, b0 <= 1.1, D1<=80) %>%
  filter(!is.na(D1), !is.na(vol_1000), D1 > 0, vol_1000 > 0)

val_data_top25 <- val_data %>%
  group_by(age) %>%                       # pro Altersklasse
  filter(D1 >= quantile(D1, 0.75, na.rm = TRUE)) %>%  # oberste 25 %
  ungroup()  
val_data_top25

# 3.6.2 Vorhersage mit Allometrie-Modell
#    → a_val und b_val aus nlsLM-Fit, Rückskalierung

val_data <- val_data %>%
  mutate(pred_allo = predict(allo_fit, newdata = val_data) * scale_factor)
val_data <- val_data %>%
  mutate(resid = vol_1000 - pred_allo)


# 3.6.3 RMSE + nRMSE berechnen
RMSE <- sqrt(mean(val_data$resid^2))
RMSE_round <- round(RMSE, 2)

rRMSE <- RMSE / mean(val_data$vol_1000) * 100
rRMSE_round <- round(rRMSE, 2)
RMSE_round
rRMSE_round


#3.6.4 Plot

#Textbox für RMSE + nRMSE

text_label_val <- paste0(
  "RMSE  = 0.16 Vfm"
)


# 5) Boxposition bestimmen
x_box_val <- min(val_data$D1) + 1 * diff(range(val_data$D1))
y_box_val <- min(val_data$vol_1000) + 0.05 * diff(range(val_data$vol_1000))

label_val_df <- data.frame(
  x = x_box_val,
  y = y_box_val,
  label = text_label_val
)


# 6) Validierungsplot erstellen 

plot_val_allo <- ggplot(data_model, aes(x = D1, y = vol_1000)) +
  geom_point(color = "grey70", alpha = 0.3) +                      
  geom_line(aes(y = pred_allo), color = "blue", size = 1.2) +      
  geom_point(data = val_data_top25,                                       
             aes(x = D1, y = vol_1000),
             color = "red", size = 2.5, alpha = 0.8) +
  geom_label(data = label_val_df,                                    
             aes(x = x, y = y, label = label),
             size = 5, fill = "white", color = "black",
             hjust = 0, vjust = 0, label.size = 0.6) +
  labs(
    x = "BHD [cm]",
    y =  expression(Vorrat~"["*Vfm~Eb^{-1}*"]")
  ) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(data_model$vol_1000)), 1)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

plot_val_allo

#3.7 Durchmesserzuwachs

#3.7.1 Ableitungsfunktion für das allometrische Modell


allo_deriv <- function(D1, a, b) {
  a * b * D1^(b - 1)
}


#3.7.2 Vorhersage der Wachstumsrate auf Originalmaßstab
data_model <- data_model %>%
  mutate(predicted_growth = allo_deriv(
    D1,
    coef(allo_fit)["a"],
    coef(allo_fit)["b"]
  ) * scale_factor)



#3.7.3 Plot erstellen

plot_allo_growth <- ggplot(data_model %>% filter(D1 <= 100), aes(x = D1)) +
  geom_line(aes(y = predicted_growth), color = "blue", size = 1.2) +
  labs(
    x = "BHD [cm]",
    y =  expression(Vorrat~"["*Vfm~Eb^{-1}~a^{-1}*"]")
  ) +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  scale_y_continuous(breaks = c(0.05, 0.10, 0.15, 0.20, 0.25), limits= c(0.05,0.25) )+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 1)
  )

plot_allo_growth

#3.7.4 Plots zusammenfügen
library(patchwork)
library(ggplot2)
combined_plot<- plot_allo |plot_allo_growth
combined_plot <- plot_allo + plot_spacer() + plot_allo_growth +
  plot_layout(ncol = 3, widths = c(1, 0.05, 1))
ggsave("Vorrat.png", combined_plot, width=14, height=6, dpi=300)

combined_plot<- plot_val_d |plot_val_allo
combined_plot <- plot_val_d + plot_spacer() + plot_val_allo +
  plot_layout(ncol = 3, widths = c(1, 0.05, 1))
ggsave("Validierung.png", combined_plot, width=14, height=6, dpi=300)

#___________________________________________________________________________________________#
#4 Bewertetes Produktionsmodell

library(dplyr)
library(minpack.lm)
library(nlme)
library(ggplot2)

#4.1 Daten vorbereiten und filtern ---
data_model <- Df5_I.Ekl_z80_int1_nbestock_ZBaum %>%
  select(D1, Ax_mittel) %>%
  filter(!is.na(D1), !is.na(Ax_mittel), D1 > 0, Ax_mittel > 0, D1 <= 80)
data_model

#4.2 Skalierung des Abtriebswertes
scale_factor <- 1000
data_model <- data_model %>%
  mutate(Ax_scaled = Ax_mittel / scale_factor)

#4.3 Modell fitten
chapman_model <- nlsLM(
  Ax_scaled ~ a * (1 - exp(-b * D1))^c,
  data = data_model,
  start = list(
    a = max(data_model$Ax_scaled),
    b = 0.01,
    c = 2
  ),
  control = nls.lm.control(maxiter = 500)
)

summary(chapman_model)

#4.4 Vorhersage auf Originalmaßstab zurückskalieren
data_model <- data_model %>%
  mutate(predicted_Ax = predict(chapman_model) * scale_factor)

#4.5 Parameter extrahieren
coef_tab <- model_summary$coefficients

if (is.matrix(coef_tab)) {
  # GNLS
  value_col <- grep("Value|Estimate", colnames(coef_tab), value = TRUE)
  a_val <- round(coef_tab["a", value_col], 3)
  b_val <- round(coef_tab["b", value_col], 5)
  c_val <- round(coef_tab["c", value_col], 3)
} else {
  # nlsLM
  a_val <- round(coef_tab["a"], 3)
  b_val <- round(coef_tab["b"], 5)
  c_val <- round(coef_tab["c"], 3)
}

text_label <- paste0(
  "a = ", 1732, "\n",
  "b = ", 0.014, "\n",
  "c = ", 3.414, "\n",
  "RSE= 14,20 €"
)

#4.6 Plot
#4.6.1 Plotkasten
x_box <- max(data_model$D1) * 0.88
y_box <- min(data_model$Ax_mittel) + 0.05 * (max(data_model$Ax_mittel) - min(data_model$Ax_mittel))

label_df <- data.frame(x = x_box, y = y_box, label = text_label)
#4.6.2 Plot erstellen
Ax<-ggplot(data_model, aes(x = D1, y = Ax_mittel)) +
  geom_point(color = "lightgrey", alpha = 0.3) +
  geom_line(aes(y = predicted_Ax), color = "blue", size = 1) +
  geom_label(
    data = label_df,
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 0,
    size = 5,
    fill = "white",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "BHD [cm]",
    y = expression(Abtriebswert (Ax)~"["*"€"~Eb^{-1}*"]")
  ) +
  scale_x_continuous(breaks = seq(35, 80, by = 5)) +
  scale_y_continuous(breaks = seq(100, 500, by = 100))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title  = element_text(size = 16),
    axis.text   = element_text(size = 14),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )
Ax


#4.7 Modellierung des Wertzuwaches
# 4.7.1 Fit der Chapman-Richards-Funktion 
chapman_model <- nlsLM(
  Ax_scaled ~ a * (1 - exp(-b * D1))^c,
  data = data_model,
  start = list(a = max(data_model$Ax_scaled), b = 0.01, c = 2),
  control = nls.lm.control(maxiter = 1000)
)

# 4.7.2 Parameter extrahieren
params <- coef(chapman_model)
a <- params["a"]
b <- params["b"]
c <- params["c"]

#4.7.3 1. Ableitung berechnen ---
data_model <- data_model %>%
  mutate(dAx_dD1 = a * c * b * exp(-b * D1) * (1 - exp(-b * D1))^(c - 1) * scale_factor)

#4.7.4  Plot nur der Ableitung ---
WZW <-ggplot(data_model, aes(x = D1, y = dAx_dD1)) +
  geom_line(color = "blue", size = 1) +
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
  ) +
  scale_x_continuous(breaks = seq(35, 80, by = 5)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title  = element_text(size = 16),
    axis.text   = element_text(size = 14),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )
WZW

#4.7.5 Plots zusammenfügen
library(patchwork)
library(ggplot2)
combined_plot<- Ax |WZW
combined_plot <- Ax + plot_spacer() + WZW +
  plot_layout(ncol = 3, widths = c(1, 0.05, 1))
ggsave("Produktionsmodell.png", combined_plot, width=14, height=6, dpi=300)


#________________________________________________________________________________________________________#
#5 Zieldurchmesser bei Boden- und Kapitalknappheit

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)

#5.1 Daten vorbereiten (NUR eine Tabelle, Ax_mittel) 
data_model <- Df5_I.Ekl_z80_int1_nbestock_ZBaum %>%
  select(D1, Ax_mittel, bb) %>%
  filter(
    !is.na(D1),
    !is.na(Ax_mittel),
    !is.na(bb),
    D1 > 0,
    Ax_mittel > 0,
    D1 <= 80
  )

#5.2 Opportunitätskosten berechnen
data_model <- data_model %>%
  mutate(
    Opp_Kapital = Ax_mittel * 0.03,   # Ax * i
    Opp_Boden   = Opp_Kapital + bb    # Ax*i + bb
  )

#5.3 Chapman-Richards-Fit ---
fit_cr <- function(y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = data_model,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}

fit_Ax   <- fit_cr(data_model$Ax_mittel)
fit_Kap  <- fit_cr(data_model$Opp_Kapital)
fit_Bod  <- fit_cr(data_model$Opp_Boden)

#5.4 Wertzuwachs-Regression (1. Ableitung)
coef_Ax <- coef(fit_Ax)

WZW_func <- function(D) {
  coef_Ax["a"] * coef_Ax["c"] * coef_Ax["b"] *
    (1 - exp(-coef_Ax["b"] * D))^(coef_Ax["c"] - 1) *
    exp(-coef_Ax["b"] * D)
}

#5.5 Vorhersagen
data_model <- data_model %>%
  mutate(
    WZW            = WZW_func(D1),
    Opp_Kapital_p  = predict(fit_Kap),
    Opp_Boden_p    = predict(fit_Bod)
  )

#5.6 Long-Format für Plot
plot_data <- data_model %>%
  select(D1, WZW, Opp_Kapital_p, Opp_Boden_p) %>%
  rename(
    "lfd. Wertzuwachs"          = WZW,
    "Opp.kosten Eb"        = Opp_Kapital_p,
    "Opp.kosten Boden und Eb"          = Opp_Boden_p
  ) %>%
  pivot_longer(
    cols = -D1,
    names_to = "Variable",
    values_to = "Wert"
  )

#5.7 Plot ---
WZW_plot <- ggplot(plot_data, aes(x = D1, y = Wert, color = Variable)) +
  
  geom_line(linewidth = 1.3) +
  
  scale_color_manual(
    values = c(
      "lfd. Wertzuwachs"          = "blue",
      "Opp.kosten Eb"             = "green4",
      "Opp.kosten Boden und Eb"   = "red"
    ),
    guide = guide_legend(
      ncol = 1,           
      byrow = TRUE,
      label.hjust = 0      
    )
  ) +
  
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*""),
    color = NULL
  ) +
  
  scale_x_continuous(breaks = seq(35, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    
    legend.position = "bottom",
    legend.justification = "left",
    legend.text = element_text(size = 14),
    legend.key.width = unit(1.2, "cm"), 
    
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

WZW_plot


ggsave("Zieldurchmesser.png", WZW_plot, width=8, height=7, dpi=300)

#____________________________________________________________________________________________#
#6 Darstellung der Integrale (Ökonomischer Nutzen und Nutzenentgang)

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)

#6.1 Daten vorbereiten

data_model <- Df5_I.Ekl_z80_int1_nbestock_ZBaum %>%
  select(D1, Ax_mittel, bb) %>%
  filter(
    !is.na(D1),
    !is.na(Ax_mittel),
    !is.na(bb),
    D1 > 0,
    Ax_mittel > 0,
    D1 <= 80
  ) %>%
  mutate(
    Opp_Kapital = Ax_mittel * 0.03,
    Opp_Boden   = Opp_Kapital + bb
  )


#6.2 Chapman–Richards-Fits des Wertzuwachses und der Opportunitätskosten

fit_cr <- function(y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = data_model,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}

fit_Ax  <- fit_cr(data_model$Ax_mittel)
fit_Bod <- fit_cr(data_model$Opp_Boden)


#6.3 Funktionen

coef_Ax <- coef(fit_Ax)

WZW_func <- function(D) {
  coef_Ax["a"] * coef_Ax["c"] * coef_Ax["b"] *
    (1 - exp(-coef_Ax["b"] * D))^(coef_Ax["c"] - 1) *
    exp(-coef_Ax["b"] * D)
}

Opp_Boden_func <- function(D) {
  predict(fit_Bod, newdata = data.frame(D1 = D))
}


#6.4 Ermittlung des Zieldurchmessers

BHD_target <- uniroot(
  function(D) WZW_func(D) - Opp_Boden_func(D),
  lower = 45,
  upper = 80
)$root

D_seq <- seq(35, 80, by = 0.1)

plot_df <- data.frame(
  D   = D_seq,
  WZW = WZW_func(D_seq),
  OK  = Opp_Boden_func(D_seq)
)


#6.5 Korrekte Reihenfolge in der Legende festlegen
plot_df$fill_label <- factor(
  dplyr::case_when(
    plot_df$D >= 45 & plot_df$D <= BHD_target                 ~ "Eb-Erwartungswert",
    plot_df$D > BHD_target & plot_df$D <= BHD_target + 1      ~ "Nutzenentgang +1 cm",
    plot_df$D > BHD_target + 1 & plot_df$D <= BHD_target + 5  ~ "Nutzenentgang +5 cm",
    plot_df$D > BHD_target + 5 & plot_df$D <= BHD_target + 10 ~ "Nutzenentgang +10 cm",
    TRUE ~ NA_character_
  ),
  levels = c(
    "Eb-Erwartungswert",
    "Nutzenentgang +1 cm",
    "Nutzenentgang +5 cm",
    "Nutzenentgang +10 cm"
  )
)


#6.6 Plot

Ew <- ggplot() +
  
  # --------- FLÄCHEN ---------
geom_ribbon(
  data = subset(plot_df, !is.na(fill_label)),
  aes(x = D, ymin = OK, ymax = WZW, fill = fill_label),
  alpha = 0.7
) +
  
  # --------- LINIEN ---------
geom_line(
  data = plot_df,
  aes(x = D, y = WZW, color = "lfd. Wertzuwachs"),
  linewidth = 1.4
) +
  geom_line(
    data = plot_df,
    aes(x = D, y = OK, color = "Opp.kosten Boden und Eb"),
    linewidth = 1.4
  ) +
  
  # --------- SKALEN ---------
scale_color_manual(
  values = c(
    "lfd. Wertzuwachs"        = "blue",
    "Opp.kosten Boden und Eb" = "red"
  ),
  guide = guide_legend(
    ncol  = 1,
    order = 1,
    title = NULL
  )
) +
  
  scale_fill_manual(
    values = c(
      "Eb-Erwartungswert"      = "#90EE90",
      "Nutzenentgang +1 cm"  = "#FADADD",
      "Nutzenentgang +5 cm"  = "#FF6F6F",
      "Nutzenentgang +10 cm" = "#8B0000"
    ),
    guide = guide_legend(
      ncol  = 1,
      order = 2,
      title = NULL
    )
  ) +
  
  # --------- LABELS ---------
labs(
  x = "BHD [cm]",
  y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
) +
  
  scale_x_continuous(breaks = seq(35, 80, 5)) +
  scale_y_continuous(breaks = seq(2, 16, 2)) +
  
  # --------- THEME ---------
theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "left",
    legend.text.align = 0,
    legend.text = element_text(size = 14),  # größerer Legendentext
    legend.spacing.x = unit(2.5, "cm"),
    
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

Ew

ggsave("Erwartungswert.png", Ew, width = 8, height = 7, dpi = 300)

#________________________________________________________________________________________________#
#7 Vergleich der Zieldurchmesser der Ertragsklassen

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)
library(ggnewscale)


#7.1 Chapman-Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}

fit_cr <- function(df, y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = df,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}


#7.2 Auswählen der Z-Bäume unterschiedlicher Ertragsklassen

varianten <- list(
  "0.Ekl" = list(df = Df5_0.Ekl_z80_int1_nbestock_ZBaum,
                           col_wzw = "#009E73",
                           col_opp = "#B2DF8A"),
  "I.Ekl" = list(df = Df5_I.Ekl_z80_int1_nbestock_ZBaum,
                            col_wzw = "#0072B2",
                            col_opp = "#A6CEE3"),
  "II.Ekl" = list(df = Df5_II.Ekl_z80_int1_nbestock_ZBaum,
                          col_wzw = "#ff7f0e",
                          col_opp = "#fdbf6f")
)


#7.3 Regressionsmodellierung und Ermittlung der Zieldurchmesser

plot_data <- tibble()
zielpunkte <- tibble()

for (vn in names(varianten)) {
  
  v <- varianten[[vn]]
  
  df <- v$df %>%
    select(D1, Ax_mittel, bb) %>%
    filter(!is.na(D1), !is.na(Ax_mittel), !is.na(bb),
           D1 > 0, D1 <= 80, Ax_mittel > 0) %>%
    mutate(Ax_i = Ax_mittel * 0.03,
           Opp  = Ax_i + bb)
  
  fit_Ax  <- fit_cr(df, df$Ax_mittel)
  fit_Opp <- fit_cr(df, df$Opp)
  
  ca <- coef(fit_Ax)
  
  WZW <- function(D) chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  Opp <- function(D) predict(fit_Opp, newdata = data.frame(D1 = D))
  
  # Zieldurchmesser
  D_star <- uniroot(function(D) WZW(D) - Opp(D), c(40, 80))$root
  

  zielpunkte <- bind_rows(
    zielpunkte,
    tibble(
      D1 = D_star,
      Wert = WZW(D_star),
      Legende = paste("lfd. Wertzuwachs", vn)  
    )
  )
  
  D_seq <- seq(min(df$D1), max(df$D1), by = 0.1)
  
  plot_data <- bind_rows(
    plot_data,
    tibble(D1 = D_seq, Wert = WZW(D_seq),
           Legende = paste("lfd. Wertzuwachs", vn)),
    tibble(D1 = D_seq, Wert = Opp(D_seq),
           Legende = paste("Opp. Kosten Boden und Eb", vn))
  )
}


#7.4 Legende festlegen

legenden_levels <- c(
  "lfd. Wertzuwachs 0.Ekl",
  "lfd. Wertzuwachs I.Ekl",
  "lfd. Wertzuwachs II.Ekl",
  "Zieldurchmesser",
  "Opp. Kosten Boden und Eb 0.Ekl",
  "Opp. Kosten Boden und Eb I.Ekl",
  "Opp. Kosten Boden und Eb II.Ekl"
)

plot_data$Legende <- factor(plot_data$Legende, levels = legenden_levels)
zielpunkte$Legende <- factor(zielpunkte$Legende, levels = legenden_levels)


#7.5 Farben für Kurvenverläufe

farben_wzw <- c(
  "lfd. Wertzuwachs 0.Ekl" = "#009E73",
  "lfd. Wertzuwachs I.Ekl" = "#0072B2",
  "lfd. Wertzuwachs II.Ekl" = "#ff7f0e"
)

farben_opp <- c(
  "Opp. Kosten Boden und Eb 0.Ekl" = "#B2DF8A",
  "Opp. Kosten Boden und Eb I.Ekl" = "#A6CEE3",
  "Opp. Kosten Boden und Eb II.Ekl" = "#fdbf6f"
)


#7.6 Plot erstellen

Ekl <- ggplot() +
  
  # Linien: Wertzuwachs
  geom_line(
    data = plot_data %>% filter(grepl("lfd. Wertzuwachs", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = farben_wzw,
    guide = guide_legend(ncol = 1, order = 1, title = NULL)
  ) +
  
  # Linien: Opp.-Kosten
  new_scale_color() +
  geom_line(
    data = plot_data %>% filter(grepl("Opp. Kosten", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2,
    alpha = 0.6
  ) +
  scale_color_manual(
    values = farben_opp,
    guide = guide_legend(ncol = 1, order = 2, title = NULL)
  ) +
  

# Zieldurchmesser-Punkte 
new_scale_color() +
  geom_point(
    data = zielpunkte,
    aes(x = D1, y = Wert, color = Legende),
    size = 3,
    shape = 1,        # offener Kreis
    stroke = 1.4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = farben_wzw, guide = "none") +
  
  # Achsen und Theme
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
  ) +
  scale_x_continuous(breaks = seq(35, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.spacing.x = unit(2.5, "cm"),
    legend.text = element_text(size = 13),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

Ekl

ggsave("ZD_Ertagsklasse.png", Ekl, width = 8, height = 7, dpi = 300)





#_________________________________________________________________________________________________#
#8 Vergleich der Zieldurchmesser der Bestandesdichten

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)
library(ggnewscale)


#8.1 Chapman-Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}

fit_cr <- function(df, y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = df,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}


#8.2 Auwahl der Z-Bäume unterschiedlicher Bestandesdichten

varianten <- list(
  "unterbestockt" = list(df = Df5_I.Ekl_z80_int1_ubestock_ZBaum,
                           col_wzw = "#009E73",
                           col_opp = "#B2DF8A"),
  "normalbestockt" = list(df = Df5_I.Ekl_z80_int1_nbestock_ZBaum,
                            col_wzw = "#0072B2",
                            col_opp = "#A6CEE3"),
  "überbestockt" = list(df = Df5_I.Ekl_z80_int1_uebestock_ZBaum,
                          col_wzw = "#ff7f0e",
                          col_opp = "#fdbf6f")
)


#8.3 Regressionsmodellierung und Ermittlung der Zieldurchmesser

plot_data <- tibble()
zielpunkte <- tibble()

for (vn in names(varianten)) {
  
  v <- varianten[[vn]]
  
  df <- v$df %>%
    select(D1, Ax_mittel, bb) %>%
    filter(!is.na(D1), !is.na(Ax_mittel), !is.na(bb),
           D1 > 0, D1 <= 80, Ax_mittel > 0) %>%
    mutate(Ax_i = Ax_mittel * 0.03,
           Opp  = Ax_i + bb)
  
  fit_Ax  <- fit_cr(df, df$Ax_mittel)
  fit_Opp <- fit_cr(df, df$Opp)
  
  ca <- coef(fit_Ax)
  
  WZW <- function(D) chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  Opp <- function(D) predict(fit_Opp, newdata = data.frame(D1 = D))
  
  # Zieldurchmesser
  D_star <- uniroot(function(D) WZW(D) - Opp(D), c(40, 80))$root
  
  # Punkt entspricht Wertzuwachs-Farbe
  zielpunkte <- bind_rows(
    zielpunkte,
    tibble(
      D1 = D_star,
      Wert = WZW(D_star),
      Legende = paste("lfd. Wertzuwachs", vn)  # gleiche Legende wie Wertzuwachs
    )
  )
  
  D_seq <- seq(min(df$D1), max(df$D1), by = 0.1)
  
  plot_data <- bind_rows(
    plot_data,
    tibble(D1 = D_seq, Wert = WZW(D_seq),
           Legende = paste("lfd. Wertzuwachs", vn)),
    tibble(D1 = D_seq, Wert = Opp(D_seq),
           Legende = paste("Opp. Kosten Boden und Eb", vn))
  )
}

#8.4 Legende

legenden_levels <- c(
  "lfd. Wertzuwachs unterbestockt",
  "lfd. Wertzuwachs normalbestockt",
  "lfd. Wertzuwachs überbestockt",
  "Zieldurchmesser",
  "Opp. Kosten Boden und Eb unterbestockt",
  "Opp. Kosten Boden und Eb normalbestockt",
  "Opp. Kosten Boden und Eb überbestockt"
)

plot_data$Legende <- factor(plot_data$Legende, levels = legenden_levels)
zielpunkte$Legende <- factor(zielpunkte$Legende, levels = legenden_levels)


#8.5 Farben der Kurven

farben_wzw <- c(
  "lfd. Wertzuwachs unterbestockt" = "#0072B2",
  "lfd. Wertzuwachs normalbestockt" = "#009E73",
  "lfd. Wertzuwachs überbestockt" = "#ff7f0e"
)

farben_opp <- c(
  "Opp. Kosten Boden und Eb unterbestockt" = "#B2DF8A",
  "Opp. Kosten Boden und Eb normalbestockt" = "#A6CEE3",
  "Opp. Kosten Boden und Eb überbestockt" = "#fdbf6f"
)


#8.6 Plot

B0 <- ggplot() +
  
  # Linien: Wertzuwachs
  geom_line(
    data = plot_data %>% filter(grepl("lfd. Wertzuwachs", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = farben_wzw,
    guide = guide_legend(ncol = 1, order = 1, title = NULL)
  ) +
  
  # Linien: Opp.-Kosten
  new_scale_color() +
  geom_line(
    data = plot_data %>% filter(grepl("Opp. Kosten", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2,
    alpha = 0.6
  ) +
  scale_color_manual(
    values = farben_opp,
    guide = guide_legend(ncol = 1, order = 2, title = NULL)
  ) +
  

# Zieldurchmesser-Punkte (offene Kreise, ganz oben)

new_scale_color() +
  geom_point(
    data = zielpunkte,
    aes(x = D1, y = Wert, color = Legende),
    size = 3,
    shape = 1,        # offener Kreis
    stroke = 1.4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = farben_wzw, guide = "none") +
  
  # Achsen und Theme
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
  ) +
  scale_x_continuous(breaks = seq(35, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.2, "cm"),
    legend.text = element_text(size = 13),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )


B0

ggsave("ZD_Bestockung.png", B0, width = 8, height = 7, dpi = 300)


#____________________________________________________________________________________________________#
#9 Vergleich der Zieldurchmesser unterschiedlicher Durchforstungsvarianten

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)
library(ggnewscale)


#9.1 Chapman–Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}

fit_cr <- function(df, y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = df,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}


#9.2 Auswahl der Z-Bäume unterschiedlicher Durchforstungsvarianten

varianten <- list(
  "Df-Variante A" = Df5_I.Ekl_z80_int1_nbestock_ZBaum,
  "Df-Variante B" = Df10_I.Ekl_z80_int1_nbestock_ZBaum,
  "Df-Variante C" = Df5_I.Ekl_z120_int1_nbestock_ZBaum,
  "Df-Variante D" = Df5_I.Ekl_z80_int12_nbestock_ZBaum
)


#9.3 Regressionsmodellierung und Ermittlung der Zieldurchmesser

plot_data  <- tibble()
Zielpunkte <- tibble()

for (vn in names(varianten)) {
  
  df <- varianten[[vn]] %>%
    select(D1, Ax_mittel, bb) %>%
    filter(
      !is.na(D1), !is.na(Ax_mittel), !is.na(bb),
      D1 > 0, D1 <= 80, Ax_mittel > 0
    ) %>%
    mutate(
      Ax_i = Ax_mittel * 0.03,
      Opp  = Ax_i + bb
    )
  
  # --- Modellfits
  fit_Ax  <- fit_cr(df, df$Ax_mittel)
  fit_Opp <- fit_cr(df, df$Opp)
  
  ca <- coef(fit_Ax)
  
  WZW_fun <- function(D) chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  Opp_fun <- function(D) predict(fit_Opp, newdata = data.frame(D1 = D))
  
  D_seq <- seq(min(df$D1), max(df$D1), by = 0.1)
  
  WZW_vals <- WZW_fun(D_seq)
  Opp_vals <- Opp_fun(D_seq)
  
  # --- Schnittpunkt
  diff_vals <- WZW_vals - Opp_vals
  idx <- which(diff(sign(diff_vals)) != 0)[1]
  
  if (!is.na(idx)) {
    D_star <- D_seq[idx]
    Y_star <- WZW_fun(D_star)
    
    Zielpunkte <- bind_rows(
      Zielpunkte,
      tibble(
        D1 = D_star,
        Wert = Y_star,
        Legende = paste("lfd. Wertzuwachs", vn)
      )
    )
  }
  
  # --- Linien
  plot_data <- bind_rows(
    plot_data,
    tibble(
      D1 = D_seq,
      Wert = WZW_vals,
      Legende = paste("lfd. Wertzuwachs", vn)
    ),
    tibble(
      D1 = D_seq,
      Wert = Opp_vals,
      Legende = paste("Opp.Kosten Boden und Eb", vn)
    )
  )
}


#9.4 Legende

legenden_levels <- c(
  "lfd. Wertzuwachs Df-Variante A",
  "lfd. Wertzuwachs Df-Variante B",
  "lfd. Wertzuwachs Df-Variante C",
  "lfd. Wertzuwachs Df-Variante D",
  "Opp.Kosten Boden und Eb Df-Variante A",
  "Opp.Kosten Boden und Eb Df-Variante B",
  "Opp.Kosten Boden und Eb Df-Variante C",
  "Opp.Kosten Boden und Eb Df-Variante D"
)

plot_data$Legende  <- factor(plot_data$Legende, levels = legenden_levels)
Zielpunkte$Legende <- factor(Zielpunkte$Legende, levels = legenden_levels)


#9.5 Farben der Kurven

farben_wzw <- c(
  "lfd. Wertzuwachs Df-Variante A" = "#1f78b4",
  "lfd. Wertzuwachs Df-Variante B" = "#33a02c",
  "lfd. Wertzuwachs Df-Variante C" = "#ff7f0e",
  "lfd. Wertzuwachs Df-Variante D" = "#6a3d9a"
)

farben_opp <- c(
  "Opp.Kosten Boden und Eb Df-Variante A" = "#a6cee3",
  "Opp.Kosten Boden und Eb Df-Variante B" = "#b2df8a",
  "Opp.Kosten Boden und Eb Df-Variante C" = "#fdbf6f",
  "Opp.Kosten Boden und Eb Df-Variante D" = "#cab2d6"
)


#9.6 Plot

WZW_plot <- ggplot() +
  
  # Linien: Wertzuwachs
  geom_line(
    data = plot_data %>% filter(grepl("lfd. Wertzuwachs", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = farben_wzw,
    guide = guide_legend(ncol = 1, order = 1, title = NULL)
  ) +
  
  # Linien: Opp.-Kosten
  new_scale_color() +
  geom_line(
    data = plot_data %>% filter(grepl("Opp.Kosten", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2,
    alpha = 0.6
  ) +
  scale_color_manual(
    values = farben_opp,
    guide = guide_legend(ncol = 1, order = 2, title = NULL)
  ) +
  

# Zieldurchmesser-Punkte (offene Kreise, ganz oben)

new_scale_color() +
  geom_point(
    data = Zielpunkte,
    aes(x = D1, y = Wert, color = Legende),
    size = 3,
    shape = 1,        # offener Kreis
    stroke = 1.4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = farben_wzw, guide = "none") +
  
  # Achsen und Theme
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
  ) +
  scale_x_continuous(breaks = seq(35, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.2, "cm"),
    legend.text = element_text(size = 13),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

WZW_plot

ggsave("ZD_Df-Variante.png", WZW_plot, width = 8, height = 7, dpi = 300)



#___________________________________________________________________________________________#
#10 Vergleich der Zieldurchmesser bei durchforsteten und undurchforsteten Simulationen

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)
library(ggnewscale)


#10.1 Chapman-Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}

fit_cr <- function(df, y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = df,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}


#10.2 Z-Bäume auswählen

varianten <- list(
  "Df-Variante A" = Df5_I.Ekl_z80_int1_nbestock_ZBaum,
  "kDf"           = kDf_I.Ekl_nbestock_ZBaum
)


#10.3 Regressionsmodellierung und Ermittlung der Zieldurchmesser

plot_data   <- tibble()
Zielpunkte  <- tibble()

for (vn in names(varianten)) {
  
  df <- varianten[[vn]] %>%
    select(D1, Ax_mittel, bb) %>%
    filter(
      !is.na(D1), !is.na(Ax_mittel), !is.na(bb),
      D1 > 0, D1 <= 80, Ax_mittel > 0
    ) %>%
    mutate(
      Ax_i = Ax_mittel * 0.03,
      Opp  = Ax_i + bb
    )
  
  # --- Fits
  fit_Ax  <- fit_cr(df, df$Ax_mittel)
  fit_Opp <- fit_cr(df, df$Opp)
  
  ca <- coef(fit_Ax)
  
  WZW_fun <- function(D) chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  Opp_fun <- function(D) predict(fit_Opp, newdata = data.frame(D1 = D))
  
  D_seq <- seq(min(df$D1), max(df$D1), by = 0.1)
  
  WZW_vals <- WZW_fun(D_seq)
  Opp_vals <- Opp_fun(D_seq)
  
  # --- Schnittpunkt bestimmen (Vorzeichenwechsel)
  diff_vals <- WZW_vals - Opp_vals
  idx <- which(diff(sign(diff_vals)) != 0)[1]
  
  if (!is.na(idx)) {
    D_star <- D_seq[idx]
    Y_star <- WZW_fun(D_star)
    
    Zielpunkte <- bind_rows(
      Zielpunkte,
      tibble(
        D1 = D_star,
        Wert = Y_star,
        Legende = paste("lfd. Wertzuwachs", vn)
      )
    )
  }
  
  # --- Linien
  plot_data <- bind_rows(
    plot_data,
    tibble(
      D1 = D_seq,
      Wert = WZW_vals,
      Legende = paste("lfd. Wertzuwachs", vn)
    ),
    tibble(
      D1 = D_seq,
      Wert = Opp_vals,
      Legende = paste("Opp.Kosten Boden und Eb", vn)
    )
  )
}


#10.4 Legende

legenden_levels <- c(
  "lfd. Wertzuwachs Df-Variante A",
  "lfd. Wertzuwachs kDf",
  "Opp.Kosten Boden und Eb Df-Variante A",
  "Opp.Kosten Boden und Eb kDf"
)

plot_data$Legende  <- factor(plot_data$Legende, levels = legenden_levels)
Zielpunkte$Legende <- factor(Zielpunkte$Legende, levels = legenden_levels)


#10.5 Farben der Kurven

farben_wzw <- c(
  "lfd. Wertzuwachs Df-Variante A" = "#1f78b4",
  "lfd. Wertzuwachs kDf"           = "#ff7f0e"
)

farben_opp <- c(
  "Opp.Kosten Boden und Eb Df-Variante A" = "#a6cee3",
  "Opp.Kosten Boden und Eb kDf"           = "#fdbf6f"
)


#10.6 Plot

WZW_plot <- ggplot() +
  
  # Linien: Wertzuwachs
  geom_line(
    data = plot_data %>% filter(grepl("lfd. Wertzuwachs", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = farben_wzw,
    guide = guide_legend(ncol = 1, order = 1, title = NULL)
  ) +
  
  # Linien: Opp.-Kosten
  new_scale_color() +
  geom_line(
    data = plot_data %>% filter(grepl("Opp.Kosten", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2,
    alpha = 0.6
  ) +
  scale_color_manual(
    values = farben_opp,
    guide = guide_legend(ncol = 1, order = 2, title = NULL)
  ) +
  

# Zieldurchmesser-Punkte 

new_scale_color() +
  geom_point(
    data = Zielpunkte,
    aes(x = D1, y = Wert, color = Legende),
    size = 3,
    shape = 1,        # offener Kreis
    stroke = 1.4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = farben_wzw, guide = "none") +
  
  # Achsen und Theme
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
  ) +
  scale_x_continuous(breaks = seq(25, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.2, "cm"),
    legend.text = element_text(size = 13),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

WZW_plot


ggsave("ZD_kDf.png", WZW_plot, width = 8, height = 7, dpi = 300)


#___________________________________________________________________________________________#
#11 Vergleich der Zieldurchmesser verschiedener Qualitäten

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)
library(ggnewscale)


#11.1 Chapman-Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}

fit_cr <- function(df, y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = df,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}


#11.2 Auswählen der Z-Bäume unterschiedlicher Qualitäten

ax_varianten <- tibble(
  ax_col = c(
    "Ax_WAs12",
    "Ax_WAs6.5",
    "Ax_mittel",
    "Ax_schlecht"
  ),
  label_wzw = c(
    "lfd. Wertzuwachs Wertästung 12 m",
    "lfd. Wertzuwachs Wertästung 6,5 m",
    "lfd. Wertzuwachs mittlere Qualität",
    "lfd. Wertzuwachs schlechte Qualität"
  ),
  label_opp = c(
    "Opp.Kosten Wertästung 12 m",
    "Opp.Kosten Wertästung 6,5 m",
    "Opp.Kosten mittlere Qualität",
    "Opp.Kosten schlechte Qualität"
  )
)


#11.3 Regressionsmodellierung und Ermittlung der Zieldurchmesser

plot_data <- tibble()
schnittpunkte <- tibble()

for (i in seq_len(nrow(ax_varianten))) {
  
  ax_var   <- ax_varianten$ax_col[i]
  lab_wzw <- ax_varianten$label_wzw[i]
  lab_opp <- ax_varianten$label_opp[i]
  
  df <- Df5_I.Ekl_z80_int1_nbestock_ZBaum %>%
    select(D1, bb, all_of(ax_var)) %>%
    rename(Ax = all_of(ax_var)) %>%
    filter(!is.na(D1), !is.na(Ax), !is.na(bb),
           D1 > 0, D1 <= 80, Ax > 0) %>%
    mutate(
      Ax_i = Ax * 0.03,
      Opp  = Ax_i + bb
    )
  
  fit_Ax  <- fit_cr(df, df$Ax)
  fit_Opp <- fit_cr(df, df$Opp)
  
  ca <- coef(fit_Ax)
  
  WZW_fun <- function(D) chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  Opp_fun <- function(D) predict(fit_Opp, newdata = data.frame(D1 = D))
  
  D_seq <- seq(min(df$D1), max(df$D1), by = 0.05)
  
  WZW_vals <- WZW_fun(D_seq)
  Opp_vals <- Opp_fun(D_seq)
  
  # ---- Schnittpunkt bestimmen
  diff_vals <- WZW_vals - Opp_vals
  idx <- which(diff(sign(diff_vals)) != 0)[1]
  
  if (!is.na(idx)) {
    D_star <- D_seq[idx]
    Y_star <- WZW_fun(D_star)
    
    schnittpunkte <- bind_rows(
      schnittpunkte,
      tibble(
        D1 = D_star,
        Wert = Y_star,
        Legende = lab_wzw
      )
    )
  }
  
  # ---- Linien
  plot_data <- bind_rows(
    plot_data,
    tibble(D1 = D_seq, Wert = WZW_vals, Legende = lab_wzw),
    tibble(D1 = D_seq, Wert = Opp_vals, Legende = lab_opp)
  )
}


#11.4 Legende

legenden_levels <- c(
  ax_varianten$label_wzw,
  ax_varianten$label_opp
)

plot_data$Legende      <- factor(plot_data$Legende, levels = legenden_levels)
schnittpunkte$Legende  <- factor(schnittpunkte$Legende, levels = legenden_levels)


#11.5 Farben der Kurven

farben_wzw <- c(
  "lfd. Wertzuwachs Wertästung 12 m"    = "#1f78b4",
  "lfd. Wertzuwachs Wertästung 6,5 m"   = "#33a02c",
  "lfd. Wertzuwachs mittlere Qualität"  = "#ff7f0e",
  "lfd. Wertzuwachs schlechte Qualität" = "#6a3d9a"
)

farben_opp <- c(
  "Opp.Kosten Wertästung 12 m"    = "#a6cee3",
  "Opp.Kosten Wertästung 6,5 m"   = "#b2df8a",
  "Opp.Kosten mittlere Qualität"  = "#fdbf6f",
  "Opp.Kosten schlechte Qualität" = "#cab2d6"
)


#11.6 Plot 
WZW_plot_Ax <- ggplot() +
  
  # Linien: Wertzuwachs
  geom_line(
    data = plot_data %>% filter(grepl("lfd. Wertzuwachs", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = farben_wzw,
    guide = guide_legend(ncol = 1, order = 1, title = NULL)
  ) +
  
  # Linien: Opp.-Kosten
  new_scale_color() +
  geom_line(
    data = plot_data %>% filter(grepl("Opp.Kosten", Legende)),
    aes(x = D1, y = Wert, color = Legende),
    linewidth = 1.2,
    alpha = 0.6
  ) +
  scale_color_manual(
    values = farben_opp,
    guide = guide_legend(ncol = 1, order = 2, title = NULL)
  ) +
  

# Zieldurchmesser-Punkte 

new_scale_color() +
  geom_point(
    data = schnittpunkte,
    aes(x = D1, y = Wert, color = Legende),
    size = 3,
    shape = 1,        # offener Kreis
    stroke = 1.4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = farben_wzw, guide = "none") +
  
  # Achsen und Theme
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
  ) +
  scale_x_continuous(breaks = seq(35, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.spacing.x = unit(1, "cm"),
    legend.text = element_text(size = 13),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )


WZW_plot_Ax

ggsave("ZD_Qualität.png", WZW_plot_Ax, width = 8, height = 7, dpi = 300)



#________________________________________________________________________________________________________#
#12 Vergleich von Preiszu- und Preisabschlag bei einer Wertästung bis 12m Höhe

library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)


#12.1 Chapman-Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}

fit_cr <- function(df, y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = df,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}


#12.2 Auswählen der Z-Bäume

ax_varianten <- tibble(
  ax_col = c(
    "Ax_WAs12_gut",
    "Ax_WAs12",
    "Ax_WAs12_schlecht"
  ),
  label_wzw = c(
    "lfd. Wertzuwachs Wertästung 12 m +15 %",
    "lfd. Wertzuwachs Wertästung 12 m",
    "lfd. Wertzuwachs Wertästung 12 m -15 %"
  ),
  label_opp = c(
    "Opp.Kosten Boden und Eb Wertästung 12 m +15 %",
    "Opp.Kosten Boden und Eb Wertästung 12 m",
    "Opp.Kosten Boden und Eb Wertästung 12 m -15 %"
  )
)


#12.3 Regressionsmodellierung und Ermittlung der Zieldurchmesser

plot_data <- tibble()
schnittpunkte <- tibble()

for (i in seq_len(nrow(ax_varianten))) {
  
  ax_var   <- ax_varianten$ax_col[i]
  lab_wzw <- ax_varianten$label_wzw[i]
  lab_opp <- ax_varianten$label_opp[i]
  
  df <- Df5_I.Ekl_z80_int1_nbestock_ZBaum %>%
    select(D1, bb, all_of(ax_var)) %>%
    rename(Ax = all_of(ax_var)) %>%
    filter(!is.na(D1), !is.na(Ax), !is.na(bb),
           D1 > 0, D1 <= 80, Ax > 0) %>%
    mutate(
      Ax_i = Ax * 0.03,
      Opp  = Ax_i + bb
    )
  
  fit_Ax  <- fit_cr(df, df$Ax)
  fit_Opp <- fit_cr(df, df$Opp)
  
  ca <- coef(fit_Ax)
  
  WZW_fun <- function(D) chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  Opp_fun <- function(D) predict(fit_Opp, newdata = data.frame(D1 = D))
  
  D_seq <- seq(min(df$D1), max(df$D1), by = 0.05)
  
  WZW_vals <- WZW_fun(D_seq)
  Opp_vals <- Opp_fun(D_seq)
  
  # ---- Schnittpunkt bestimmen
  diff_vals <- WZW_vals - Opp_vals
  idx <- which(diff(sign(diff_vals)) != 0)[1]
  
  if (!is.na(idx)) {
    D_star <- D_seq[idx]
    Y_star <- WZW_fun(D_star)
    
    schnittpunkte <- bind_rows(
      schnittpunkte,
      tibble(
        D1 = D_star,
        Wert = Y_star,
        Legende = lab_wzw
      )
    )
  }
  
  # ---- Linien
  plot_data <- bind_rows(
    plot_data,
    tibble(D1 = D_seq, Wert = WZW_vals, Legende = lab_wzw),
    tibble(D1 = D_seq, Wert = Opp_vals, Legende = lab_opp)
  )
}


#12.4 Legende

legenden_levels <- c(
  ax_varianten$label_wzw,
  ax_varianten$label_opp
)

plot_data$Legende      <- factor(plot_data$Legende, levels = legenden_levels)
schnittpunkte$Legende  <- factor(schnittpunkte$Legende, levels = legenden_levels)


#12.5 Farben der Kurven

farben <- c(

  "lfd. Wertzuwachs Wertästung 12 m +15 %" = "#3182bd",
  "lfd. Wertzuwachs Wertästung 12 m"       = "#1f78b4",
  "lfd. Wertzuwachs Wertästung 12 m -15 %" = "#6baed6",
  

  "Opp.Kosten Boden und Eb Wertästung 12 m +15 %" = "#a6cee3",
  "Opp.Kosten Boden und Eb Wertästung 12 m"       = "#9ecae1",
  "Opp.Kosten Boden und Eb Wertästung 12 m -15 %" = "#c6dbef"
)


#12.6 Plot

WZW_plot_Ax12 <- ggplot() +
  
  # Linien
  geom_line(
    data = plot_data,
    aes(D1, Wert, color = Legende),
    linewidth = 1.2
  ) +
  
  # Schnittpunkte
  geom_point(
    data = schnittpunkte,
    aes(x = D1, y = Wert, color = Legende),
    size = 3,
    shape = 1,
    stroke = 1.2,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = farben,
    guide = guide_legend(title = NULL, ncol = 1)
  ) +
  
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
  ) +
  scale_x_continuous(breaks = seq(35, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.justification = "left",
    legend.spacing.x = unit(0.5, "cm"),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

WZW_plot_Ax12

#_____________________________________________________________________________________________________#
#13 Vergleich von Preiszu- und Preisabschlag bei einer Wertästung bis 6,5 m Höhe


library(dplyr)
library(minpack.lm)
library(ggplot2)
library(tidyr)


#13.1 Chapman-Richards Funktionen

chapman <- function(D, a, b, c) {
  a * (1 - exp(-b * D))^c
}

chapman_deriv <- function(D, a, b, c) {
  a * c * b * (1 - exp(-b * D))^(c - 1) * exp(-b * D)
}

fit_cr <- function(df, y) {
  nlsLM(
    y ~ a * (1 - exp(-b * D1))^c,
    data = df,
    start = list(a = max(y), b = 0.01, c = 2),
    control = nls.lm.control(maxiter = 500)
  )
}


#13.2 Auswählen der Z-Bäume

ax_varianten <- tibble(
  ax_col = c(
    "Ax_WAs6.5_gut",
    "Ax_WAs6.5",
    "Ax_WAs6.5_schlecht"
  ),
  label_wzw = c(
    "lfd. Wertzuwachs Wertästung 6,5 m +15 %",
    "lfd. Wertzuwachs Wertästung 6,5 m",
    "lfd. Wertzuwachs Wertästung 6,5 m -15 %"
  ),
  label_opp = c(
    "Opp.Kosten Boden und Eb Wertästung 6,5 m +15 %",
    "Opp.Kosten Boden und Eb Wertästung 6,5 m",
    "Opp.Kosten Boden und Eb Wertästung 6,5 m -15 %"
  )
)


#13.3 Regressionsmodellierung und Ermittlung der Zieldurchmesser

plot_data <- tibble()
schnittpunkte <- tibble()

for (i in seq_len(nrow(ax_varianten))) {
  
  ax_var   <- ax_varianten$ax_col[i]
  lab_wzw <- ax_varianten$label_wzw[i]
  lab_opp <- ax_varianten$label_opp[i]
  
  df <- Df5_I.Ekl_z80_int1_nbestock_ZBaum %>%
    select(D1, bb, all_of(ax_var)) %>%
    rename(Ax = all_of(ax_var)) %>%
    filter(!is.na(D1), !is.na(Ax), !is.na(bb),
           D1 > 0, D1 <= 80, Ax > 0) %>%
    mutate(
      Ax_i = Ax * 0.03,
      Opp  = Ax_i + bb
    )
  
  fit_Ax  <- fit_cr(df, df$Ax)
  fit_Opp <- fit_cr(df, df$Opp)
  
  ca <- coef(fit_Ax)
  
  WZW_fun <- function(D) chapman_deriv(D, ca["a"], ca["b"], ca["c"])
  Opp_fun <- function(D) predict(fit_Opp, newdata = data.frame(D1 = D))
  
  D_seq <- seq(min(df$D1), max(df$D1), by = 0.05)
  
  WZW_vals <- WZW_fun(D_seq)
  Opp_vals <- Opp_fun(D_seq)
  
  #Schnittpunkt bestimmen
  diff_vals <- WZW_vals - Opp_vals
  idx <- which(diff(sign(diff_vals)) != 0)[1]
  
  if (!is.na(idx)) {
    D_star <- D_seq[idx]
    Y_star <- WZW_fun(D_star)
    
    schnittpunkte <- bind_rows(
      schnittpunkte,
      tibble(
        D1 = D_star,
        Wert = Y_star,
        Legende = lab_wzw
      )
    )
  }
  
  # Linien
  plot_data <- bind_rows(
    plot_data,
    tibble(D1 = D_seq, Wert = WZW_vals, Legende = lab_wzw),
    tibble(D1 = D_seq, Wert = Opp_vals, Legende = lab_opp)
  )
}


#13.4 Legende

legenden_levels <- c(
  ax_varianten$label_wzw,
  ax_varianten$label_opp
)

plot_data$Legende      <- factor(plot_data$Legende, levels = legenden_levels)
schnittpunkte$Legende  <- factor(schnittpunkte$Legende, levels = legenden_levels)


#13.5 Farben der Kurven

farben <- c(
  # WZW kräftige Grüntöne
  "lfd. Wertzuwachs Wertästung 6,5 m +15 %" = "#33a02c",
  "lfd. Wertzuwachs Wertästung 6,5 m"       = "#1b9e77",
  "lfd. Wertzuwachs Wertästung 6,5 m -15 %" = "#b2df8a",
  
  # Opportunitätskosten hellere Grüntöne
  "Opp.Kosten Boden und Eb Wertästung 6,5 m +15 %" = "#a6dba0",
  "Opp.Kosten Boden und Eb Wertästung 6,5 m"       = "#66c2a4",
  "Opp.Kosten Boden und Eb Wertästung 6,5 m -15 %" = "#d9f0d3"
)


#13.6 Plot

WZW_plot_Ax6.5 <- ggplot() +
  
  # Linien
  geom_line(
    data = plot_data,
    aes(D1, Wert, color = Legende),
    linewidth = 1.2
  ) +
  
  # Schnittpunkte
  geom_point(
    data = schnittpunkte,
    aes(x = D1, y = Wert, color = Legende),
    size = 3,
    shape = 1,
    stroke = 1.2,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = farben,
    guide = guide_legend(title = NULL, ncol = 1)
  ) +
  
  labs(
    x = "BHD [cm]",
    y = expression(""*"€"~Eb^{-1}~cm^{-1}*"")
  ) +
  scale_x_continuous(breaks = seq(35, 80, 5)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.justification = "left",
    legend.spacing.x = unit(0.5, "cm"),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

WZW_plot_Ax6.5

#13.7 Plots zusammenfügen

library(patchwork)
library(ggplot2)
combined_plot<- WZW_plot_Ax12 |WZW_plot_Ax6.5
combined_plot <- WZW_plot_Ax12 + plot_spacer() +WZW_plot_Ax6.5  +
  plot_layout(ncol = 3, widths = c(1, 0.05, 1))
ggsave("ZD_Preisfluktuation.png", combined_plot, width=14, height=8, dpi=300)

