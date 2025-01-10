# reproduction des graphiques de la problématique de l'article

########## USD/EUR ############################################################

# Charger les données et coup d'oeil rapide
data <- read.csv("~/work/usd-eur_rate.csv")
head(data, 10)

# Supprimer les lignes avec des valeurs manquantes dans observation_date et DEXUSEU
data <- na.omit(data[, c("observation_date", "DEXUSEU")])
# Vérifiez les longueurs pour confirmer
nrow(data)

# format date
data$observation_date <- as.Date(data$observation_date)

# représentation
plot(data$observation_date, data$DEXUSEU, 
     type = "l",                
     col = "blue",             
     lwd = 2,                   
     xlab = "Date",             
     ylab = "USD/EUR", 
     main = "Dollar-Euro exchange rate over time") 

# pimpons ce graphe un peu

dates_trimestrielles <- seq(min(data$observation_date, na.rm = TRUE), 
                            max(data$observation_date, na.rm = TRUE), 
                            by = "3 months")

dates_debut_annee <- dates_trimestrielles[format(dates_trimestrielles, "%m") == "01"]

plot(data$observation_date, data$DEXUSEU, 
     type = "l", 
     col = "blue", 
     lwd = 2, 
     xlab = "Time", 
     ylab = "USD/EUR", 
     main = "Dollar-Euro exchange rate over time",
     xaxt = "n")  # Supprime l'axe X par défaut

# Ajouter l'axe X avec des graduations pour les trimestres
axis(1, at = dates_trimestrielles, labels = format(dates_trimestrielles, "%Y-%m"), 
     tick = TRUE, tcl = -0.5)  # tcl définit la longueur des graduations (ici, courte)
# Ajouter des graduations plus longues pour le début d'année
axis(1, at = dates_debut_annee, labels = FALSE, 
     tick = TRUE, tcl = -1)  # tcl = -1 pour des graduations plus longues




###### relative balance sheet ####################################################

#j'ai mes deux séries, il faut que je fasse le "relative bs" donc ratio des deux

ecb_data <- read.csv("~/work/EBC_BS.csv")
fed_data <- read.csv("~/work/FED_BS.csv")

ecb_data$observation_date <- as.Date(ecb_data$observation_date)
fed_data$observation_date <- as.Date(fed_data$observation_date)

#je veux aligner mes séries

#comme la fed publie le mercredi et la bce le mercredi, j'ajoute une colonne "semaine"
install.packages("lubridate")
library(lubridate)
ecb_data$week_number <- week(ecb_data$observation_date)
fed_data$week_number <- week(fed_data$observation_date)
# Ajouter une colonne pour le numéro de semaine dans chaque série, incluant l'année
ecb_data$week_number <- paste0(year(ecb_data$observation_date), "-", week(ecb_data$observation_date))
fed_data$week_number <- paste0(year(fed_data$observation_date), "-", week(fed_data$observation_date))

# Fusionner les deux datasets par le numéro de semaine (année-semaine)
merged_data <- merge(ecb_data, fed_data, by = "week_number", all = FALSE)
head(merged_data)

# Calculer le ratio "relative balance sheet" en pourcentage
merged_data$relative_balance <- (merged_data$ECBASSETSW / merged_data$WALCL) * 100
head(merged_data)

# représentation essai 1 : CHAOTIQUE

# Créer une date à partir de la semaine
# Utiliser le début de la semaine comme référence (par exemple, le lundi)
merged_data$week_date <- as.Date(paste0(merged_data$week_number, "-1"), format = "%Y-%U-%u")

# Vérifier la structure
head(merged_data)

# Représentation du ratio "relative balance sheet"
plot(merged_data$week_date, merged_data$relative_balance, 
     type = "l",                # Type de graphique : ligne
     col = "blue",              # Couleur de la ligne
     lwd = 2,                   # Épaisseur de la ligne
     xlab = "Date",             # Étiquette de l'axe des x
     ylab = "ECB/Fed (%)",      # Étiquette de l'axe des y
     main = "Relative Balance Sheet (ECB/Fed)")  # Titre


# essai 2 

# Charger le package lubridate pour travailler avec les dates
library(lubridate)

# Créer la colonne week_number dans les deux datasets
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

# Supprimer les lignes avec des valeurs manquantes dans week_date ou relative_balance
merged_data_clean <- merged_data[!is.na(merged_data$week_date) & !is.na(merged_data$relative_balance), ]

# Vérifier la structure de merged_data_clean
head(merged_data_clean)

# Représentation graphique du ratio "relative balance sheet"
plot(merged_data_clean$week_date, merged_data_clean$relative_balance, 
     type = "l",                # Type de graphique : ligne
     col = "blue",              # Couleur de la ligne
     lwd = 2,                   # Épaisseur de la ligne
     xlab = "Date",             # Étiquette de l'axe des x
     ylab = "ECB/Fed (%)",      # Étiquette de l'axe des y
     main = "Relative Balance Sheet (ECB/Fed) essai 2")  # Titre


# essai 3 CA MAAAARCHE

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

# Représentation graphique du ratio "relative balance sheet"
plot(merged_data_clean$week_date, merged_data_clean$relative_balance, 
     type = "l",                # Type de graphique : ligne
     col = "blue",              # Couleur de la ligne
     lwd = 2,                   # Épaisseur de la ligne
     xlab = "",             # Étiquette de l'axe des x
     ylab = "ECB/Fed (%)",      # Étiquette de l'axe des y
     main = "Relative Balance Sheet (ECB/Fed)",  # Titre
     xaxt = "n")                # Ne pas afficher d'axes x par défaut


# Ajouter des indentations trimestrielles sur l'axe des x
# Créer une séquence de dates trimestrielles pour les ticks
quarters <- seq(min(merged_data_clean$week_date), max(merged_data_clean$week_date), by = "3 months")

# Ajouter ces ticks avec des étiquettes appropriées
axis(1, at = quarters, labels = format(quarters, "%b %Y"), las = 2)

 









