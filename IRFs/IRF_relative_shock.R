############################## IRF WITH RELATIVE QE SHOCKS

# data
# code from "Graphiques_problématique" to get the dataset
# Charger le package lubridate pour travailler avec les dates
library(lubridate)
# Créer la colonne week_number dans les deux datasets (en format année-semaine)
ecb_data$week_number <- paste(year(ecb_data$observation_date), 
                              week(ecb_data$observation_date), sep = "-")
fed_data$week_number <- paste(year(fed_data$observation_date), 
                              week(fed_data$observation_date), sep = "-")
# Fusionner les deux datasets par week_number
merged_data <- merge(ecb_data, fed_data, by = "week_number", all = FALSE)
# Calculer le ratio "relative balance sheet" en pourcentage
merged_data$relative_balance <- (merged_data$ECBASSETSW / merged_data$WALCL) * 100
# Créer la colonne week_date à partir de week_number pour l'axe des x
merged_data$week_date <- as.Date(paste0(merged_data$week_number, "-1"), format = "%Y-%U-%u")
# Vérifier la structure de merged_data et vérifier l'ordre des dates
head(merged_data)
# Trier les données par date pour garantir qu'elles sont dans l'ordre chronologique
merged_data <- merged_data[order(merged_data$week_date), ]
# Supprimer les lignes avec des valeurs manquantes dans week_date ou relative_balance
merged_data_clean <- merged_data[!is.na(merged_data$week_date) & !is.na(merged_data$relative_balance), ]


# relative qe shock based on the ratio "relative_balance"
merged_data_2$QE_shock_rel <- merged_data_2$relative_balance



### IRF FOR EXCHANGE RATE 

# VAR
# estimating the var with the relative qe shock
var_select_rel <- VARselect(merged_data_2[, c("EUR.USD", "QE_shock_rel")], lag.max = 10, type = "both")

# optimal order for the var
var_select_rel$selection  # AIC(n), HQ(n), SC(n), FPE(n)
# 2 for every indicator

# estimating with order 2
var_model_rel <- VAR(merged_data_2[, c("EUR.USD", "QE_shock_rel")], p = 2, type = "both")

# estimate irf
irf_model_rel <- irf(var_model_rel, impulse = "QE_shock_rel", response = "EUR.USD", n.ahead = 15, boot = TRUE)

# Tracer l'IRF
plot(irf_model_rel)


### IRF for d_rate

# optimal var
var_select_rel <- VARselect(merged_data_2[, c("d_rate", "QE_shock_rel")], lag.max = 10, type = "both")
# optimal order
print(var_select_rel$selection)  # AIC(n), HQ(n), SC(n), FPE(n)
# 3 is better

# estimate var with order 3
var_model_d_rate <- VAR(merged_data_2[, c("d_rate", "QE_shock_rel")], p = 3, type = "both")

# irf with relative qe shock as shock and response from d_rate
irf_model_d_rate <- irf(var_model_d_rate, impulse = "QE_shock_rel", response = "d_rate", n.ahead = 10, boot = TRUE)

# plot
plot(irf_model_d_rate)

