#Skript zum Bereinigen der Kreisdaten aus der PKS
library(needs)
needs(tidyverse, purrr)

# 1. Import data ----------------------------------------------------------
kreise_2015 <- read.csv("input/2015.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
kreise_2016 <- read.csv("input/2016.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
kreise_2017 <- read.csv("input/2017.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
kreise_2018 <- read.csv("input/2018.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
kreise_2019 <- read.csv("input/2019.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
kreise_2020 <- read.csv("input/2020.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
kreise_2021 <- read.csv("input/2021.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
kreise_2022 <- read.csv("input/2022.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
kreise_2023 <- read.csv("input/2023.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")

# 2. Clean Data -----------------------------------------------------------
kreise_2015 <- kreise_2015[-1, ]
kreise_2020 <- kreise_2020[, -((ncol(kreise_2020)-4):ncol(kreise_2020))]

# Liste der DataFrames
kreise_list <- list(kreise_2015, kreise_2016, kreise_2017, kreise_2018, kreise_2019, kreise_2020, kreise_2021, kreise_2022, kreise_2023)

# Neue Spaltennamen
new_colnames <- c("schluessel", "straftat", "gemeindeschluessel", "kreis", "kreisart", 
                  "faelle", "haeufigkeit", "versuche", "versuche_anteil", "schusswaffe_drohung", 
                  "schusswaffe_schuss", "aufklaerung_faelle", "aufklaerungsquote", "tatverdaechtige", 
                  "tatverdaechtige_maennlich", "tatverdaechtige_weiblich", "tatverdaechtige_auslaender", 
                  "tatverdaechtige_auslaender_anteil")

# Funktion, um Änderungen durchzuführen
process_kreise <- function(df) {
  # Umbenennen der Spalten
  colnames(df) <- new_colnames
  
  # Entfernen der Kommata und Konvertieren der Spalten 6 bis 18 in numeric
  for (i in 6:18) {
    # Entfernen von Kommata und Konvertieren in numeric
    df[[i]] <- as.numeric(gsub(",", "", df[[i]]))
    
    # Ersetzen von NAs durch 0
    df[[i]][is.na(df[[i]])] <- 0
  }
  
  return(df)
}

# Anwenden der Funktion auf alle DataFrames
kreise_list <- lapply(kreise_list, process_kreise)

# Dataframes zurückspeichern
kreise_2015 <- kreise_list[[1]]
kreise_2016 <- kreise_list[[2]]
kreise_2017 <- kreise_list[[3]]
kreise_2018 <- kreise_list[[4]]
kreise_2019 <- kreise_list[[5]]
kreise_2020 <- kreise_list[[6]]
kreise_2021 <- kreise_list[[7]]
kreise_2022 <- kreise_list[[8]]
kreise_2023 <- kreise_list[[9]]

rm(kreise_list, new_colnames, process_kreise)

# Liste der DataFrames
dataframes <- list(kreise_2015, kreise_2016, kreise_2017, kreise_2018, kreise_2019, kreise_2020, kreise_2021, kreise_2022, kreise_2023)

# Ändere den Wert in jedem DataFrame um Namensdopplungen zu umgehen
dataframes <- map(dataframes, ~ .x %>%
                    mutate(kreis = case_when(
                      # Erste Regel: "Landkreis Heidekreis" in "Heidekreis" ändern
                      kreis == "Landkreis Heidekreis" ~ "Heidekreis",
                      
                      # Weitere spezifische Anpassungen für den gemeindeschluessel
                      gemeindeschluessel == 9561 ~ "Ansbach, Stadt",
                      gemeindeschluessel == 9571 ~ "Ansbach, Landkreis",
                      gemeindeschluessel == 9661 ~ "Aschaffenburg, Stadt",
                      gemeindeschluessel == 9671 ~ "Aschaffenburg, Landkreis",
                      gemeindeschluessel == 9761 ~ "Augsburg, Stadt",
                      gemeindeschluessel == 9772 ~ "Augsburg, Landkreis",
                      gemeindeschluessel == 9461 ~ "Bamberg, Stadt",
                      gemeindeschluessel == 9471 ~ "Bamberg, Landkreis",
                      gemeindeschluessel == 9462 ~ "Bayreuth, Stadt",
                      gemeindeschluessel == 9472 ~ "Bayreuth, Landkreis",
                      gemeindeschluessel == 9463 ~ "Coburg, Stadt",
                      gemeindeschluessel == 9473 ~ "Coburg, Landkreis",
                      gemeindeschluessel == 9563 ~ "Fürth, Stadt",
                      gemeindeschluessel == 9573 ~ "Fürth, Landkreis",
                      gemeindeschluessel == 8121 ~ "Heilbronn, Stadt",
                      gemeindeschluessel == 8125 ~ "Heilbronn, Landkreis",
                      gemeindeschluessel == 9464 ~ "Hof, Stadt",
                      gemeindeschluessel == 9475 ~ "Hof, Landkreis",
                      gemeindeschluessel == 7312 ~ "Kaiserslautern, Stadt",
                      gemeindeschluessel == 7335 ~ "Kaiserslautern, Landkreis",
                      gemeindeschluessel == 8212 ~ "Karlsruhe, Stadt",
                      gemeindeschluessel == 8215 ~ "Karlsruhe, Landkreis",
                      gemeindeschluessel == 6611 ~ "Kassel, Stadt",
                      gemeindeschluessel == 6633 ~ "Kassel, Landkreis",
                      gemeindeschluessel == 9261 ~ "Landshut, Stadt",
                      gemeindeschluessel == 9274 ~ "Landshut, Landkreis",
                      gemeindeschluessel == 14713 ~ "Leipzig, Stadt",
                      gemeindeschluessel == 14729 ~ "Leipzig, Landkreis",
                      gemeindeschluessel == 9162 ~ "München, Stadt",
                      gemeindeschluessel == 9184 ~ "München, Landkreis",
                      gemeindeschluessel == 6438 ~ "Offenbach, Landkreis",
                      gemeindeschluessel == 6413 ~ "Offenbach, Stadt",
                      gemeindeschluessel == 3403 ~ "Oldenburg, Stadt",
                      gemeindeschluessel == 3458 ~ "Oldenburg, Landkreis",
                      gemeindeschluessel == 3404 ~ "Osnabrück, Stadt",
                      gemeindeschluessel == 3459 ~ "Osnabrück, Landkreis",
                      gemeindeschluessel == 9262 ~ "Passau, Stadt",
                      gemeindeschluessel == 9275 ~ "Passau, Landkreis",
                      gemeindeschluessel == 9362 ~ "Regensburg, Stadt",
                      gemeindeschluessel == 9375 ~ "Regensburg, Landkreis",
                      gemeindeschluessel == 9163 ~ "Rosenheim, Stadt",
                      gemeindeschluessel == 9187 ~ "Rosenheim, Landkreis",
                      gemeindeschluessel == 9662 ~ "Schweinfurt, Stadt",
                      gemeindeschluessel == 9678 ~ "Schweinfurt, Landkreis",
                      gemeindeschluessel == 9663 ~ "Würzburg, Stadt",
                      gemeindeschluessel == 9679 ~ "Würzburg, Landkreis",
                      
                      TRUE ~ kreis  # Standardmäßig wird der ursprüngliche Wert von "kreis" beibehalten, wenn keine der Bedingungen zutrifft
                    )))

# Zurückspeichern der Ergebnisse
kreise_2015 <- dataframes[[1]]
kreise_2016 <- dataframes[[2]]
kreise_2017 <- dataframes[[3]]
kreise_2018 <- dataframes[[4]]
kreise_2019 <- dataframes[[5]]
kreise_2020 <- dataframes[[6]]
kreise_2021 <- dataframes[[7]]
kreise_2022 <- dataframes[[8]]
kreise_2023 <- dataframes[[9]]


## kreise 2015 und 2016: Osterode und Göttingen zusammenfassen -----------
merge_osterode_goettingen <- function(df, einwohner_goettingen, einwohner_osterode) {
  # Filtern der Zeilen für Göttingen und Osterode im Harz
  goettingen_data <- df[df$kreis == "Göttingen", ]
  osterode_data <- df[df$kreis == "Osterode am Harz", ]
  
  # Zusammenführen der beiden Datensätze basierend auf der Straftat (wegen der Spalte "straftat")
  merged_data <- merge(osterode_data, goettingen_data, by = "straftat", suffixes = c("_osterode", "_goettingen"))
  
  # Für die Spalten gemeindeschluessel, kreis, kreisart: die Werte von Göttingen übernehmen
  merged_data$schluessel <- merged_data$schluessel_goettingen
  merged_data$gemeindeschluessel <- 3159
  merged_data$kreis <- "Göttingen"
  merged_data$kreisart <- merged_data$kreisart_goettingen
  
  # Für die numerischen Spalten (faelle, versuche, schusswaffe_drohung, schusswaffe_schuss, etc.)
  # werden die Werte summiert
  merged_data$faelle <- merged_data$faelle_osterode + merged_data$faelle_goettingen
  merged_data$versuche <- merged_data$versuche_osterode + merged_data$versuche_goettingen
  merged_data$versuche_anteil <- merged_data$versuche / merged_data$faelle * 100
  merged_data$schusswaffe_drohung <- merged_data$schusswaffe_drohung_osterode + merged_data$schusswaffe_drohung_goettingen
  merged_data$schusswaffe_schuss <- merged_data$schusswaffe_schuss_osterode + merged_data$schusswaffe_schuss_goettingen
  merged_data$aufklaerung_faelle <- merged_data$aufklaerung_faelle_osterode + merged_data$aufklaerung_faelle_goettingen
  merged_data$aufklaerungsquote <- merged_data$aufklaerung_faelle / merged_data$faelle * 100
  merged_data$tatverdaechtige <- merged_data$tatverdaechtige_osterode + merged_data$tatverdaechtige_goettingen
  merged_data$tatverdaechtige_maennlich <- merged_data$tatverdaechtige_maennlich_osterode + merged_data$tatverdaechtige_maennlich_goettingen
  merged_data$tatverdaechtige_weiblich <- merged_data$tatverdaechtige_weiblich_osterode + merged_data$tatverdaechtige_weiblich_goettingen
  merged_data$tatverdaechtige_auslaender <- merged_data$tatverdaechtige_auslaender_osterode + merged_data$tatverdaechtige_auslaender_goettingen
  merged_data$tatverdaechtige_auslaender_anteil <- merged_data$tatverdaechtige_auslaender / merged_data$tatverdaechtige * 100
  
  # Umstellen der Spalten, damit "schluessel" ganz am Anfang steht
  merged_data <- merged_data[, c("schluessel", setdiff(names(merged_data), "schluessel"))]
  
  # Berechnung der Häufigkeit für das gegebene Jahr mit den jeweiligen Einwohnerzahlen
  merged_data$einwohner_goettingen <- einwohner_goettingen
  merged_data$einwohner_osterode <- einwohner_osterode
  merged_data$haeufigkeit <- (merged_data$faelle_goettingen + merged_data$faelle_osterode) / (merged_data$einwohner_goettingen + merged_data$einwohner_osterode) * 100000
  
  # Entfernen der ursprünglichen Spalten mit den Suffixen _osterode und _goettingen
  merged_data <- merged_data[, !grepl("_osterode$|_goettingen$", colnames(merged_data))]
  
  merged_data <- merged_data %>%
    select(
      schluessel, straftat, gemeindeschluessel, kreis, kreisart, faelle, haeufigkeit, everything()         # Alle übrigen Spalten
    )
  
  # Rückgabe des zusammengeführten DataFrames
  return(merged_data)
}

# Anwendung der Funktion auf kreise_2015 mit den Einwohnerzahlen von 2015
kreise_2015_merged <- merge_osterode_goettingen(kreise_2015, einwohner_goettingen = 250220, einwohner_osterode = 73793)

# Entfernen der Daten für die Kreise Osterode am Harz und Göttingen in kreise_2015
kreise_2015 <- kreise_2015[!(kreise_2015$kreis %in% c("Osterode am Harz", "Göttingen")), ]
kreise_2015 <- rbind(kreise_2015, kreise_2015_merged)

# Anwendung der Funktion auf kreise_2016 mit den Einwohnerzahlen von 2016
kreise_2016_merged <- merge_osterode_goettingen(kreise_2016, einwohner_goettingen = 255653, einwohner_osterode = 73885)

# Entfernen der Daten für die Kreise Osterode am Harz und Göttingen in kreise_2016
kreise_2016 <- kreise_2016[!(kreise_2016$kreis %in% c("Osterode am Harz", "Göttingen")), ]
kreise_2016 <- rbind(kreise_2016, kreise_2016_merged)


## kreise 2015 bis 2021: Eisenach und Wartburgkreis zusammenfassen ----------
merge_eisenach_wartburgkreis <- function(df, einwohner_wartburgkreis, einwohner_eisenach) {
  # Filtern der Zeilen für Göttingen und eisenach im Harz
  wartburgkreis_data <- df[df$kreis == "Wartburgkreis", ]
  eisenach_data <- df[df$kreis == "Eisenach", ]
  
  # Zusammenführen der beiden Datensätze basierend auf der Straftat (wegen der Spalte "straftat")
  merged_data <- merge(eisenach_data, wartburgkreis_data, by = "straftat", suffixes = c("_eisenach", "_wartburgkreis"))
  
  # Für die Spalten gemeindeschluessel, kreis, kreisart: die Werte von Göttingen übernehmen
  merged_data$schluessel <- merged_data$schluessel_wartburgkreis
  merged_data$gemeindeschluessel <- merged_data$gemeindeschluessel_wartburgkreis
  merged_data$kreis <- merged_data$kreis_wartburgkreis
  merged_data$kreisart <- merged_data$kreisart_wartburgkreis
  
  # Für die numerischen Spalten (faelle, versuche, schusswaffe_drohung, schusswaffe_schuss, etc.)
  # werden die Werte summiert
  merged_data$faelle <- merged_data$faelle_eisenach + merged_data$faelle_wartburgkreis
  merged_data$versuche <- merged_data$versuche_eisenach + merged_data$versuche_wartburgkreis
  merged_data$versuche_anteil <- merged_data$versuche / merged_data$faelle * 100
  merged_data$schusswaffe_drohung <- merged_data$schusswaffe_drohung_eisenach + merged_data$schusswaffe_drohung_wartburgkreis
  merged_data$schusswaffe_schuss <- merged_data$schusswaffe_schuss_eisenach + merged_data$schusswaffe_schuss_wartburgkreis
  merged_data$aufklaerung_faelle <- merged_data$aufklaerung_faelle_eisenach + merged_data$aufklaerung_faelle_wartburgkreis
  merged_data$aufklaerungsquote <- merged_data$aufklaerung_faelle / merged_data$faelle * 100
  merged_data$tatverdaechtige <- merged_data$tatverdaechtige_eisenach + merged_data$tatverdaechtige_wartburgkreis
  merged_data$tatverdaechtige_maennlich <- merged_data$tatverdaechtige_maennlich_eisenach + merged_data$tatverdaechtige_maennlich_wartburgkreis
  merged_data$tatverdaechtige_weiblich <- merged_data$tatverdaechtige_weiblich_eisenach + merged_data$tatverdaechtige_weiblich_wartburgkreis
  merged_data$tatverdaechtige_auslaender <- merged_data$tatverdaechtige_auslaender_eisenach + merged_data$tatverdaechtige_auslaender_wartburgkreis
  merged_data$tatverdaechtige_auslaender_anteil <- merged_data$tatverdaechtige_auslaender / merged_data$tatverdaechtige * 100
  
  # Umstellen der Spalten, damit "schluessel" ganz am Anfang steht
  merged_data <- merged_data[, c("schluessel", setdiff(names(merged_data), "schluessel"))]
  
  # Berechnung der Häufigkeit für das gegebene Jahr mit den jeweiligen Einwohnerzahlen
  merged_data$einwohner_wartburgkreis <- einwohner_wartburgkreis
  merged_data$einwohner_eisenach <- einwohner_eisenach
  merged_data$haeufigkeit <- (merged_data$faelle_wartburgkreis + merged_data$faelle_eisenach) / (merged_data$einwohner_wartburgkreis + merged_data$einwohner_eisenach) * 100000
  
  # Entfernen der ursprünglichen Spalten mit den Suffixen _eisenach und _wartburgkreis
  merged_data <- merged_data[, !grepl("_eisenach$|_wartburgkreis$", colnames(merged_data))]
  
  merged_data <- merged_data %>%
    select(
      schluessel, straftat, gemeindeschluessel, kreis, kreisart, faelle, haeufigkeit, everything()         # Alle übrigen Spalten
    )
  
  # Rückgabe des zusammengeführten DataFrames
  return(merged_data)
}

# Anwendung der Funktion auf kreise_2015 mit den Einwohnerzahlen von 2015
kreise_2015_merged <- merge_eisenach_wartburgkreis(kreise_2015, einwohner_wartburgkreis = 126283, einwohner_eisenach = 42417)
kreise_2015 <- kreise_2015[!(kreise_2015$kreis %in% c("Eisenach", "Wartburgkreis")), ]
kreise_2015 <- rbind(kreise_2015, kreise_2015_merged)

# Anwendung der Funktion auf kreise_2016 mit den Einwohnerzahlen von 2016
kreise_2016_merged <- merge_eisenach_wartburgkreis(kreise_2016, einwohner_wartburgkreis = 125835, einwohner_eisenach = 42588)
kreise_2016 <- kreise_2016[!(kreise_2016$kreis %in% c("Eisenach", "Wartburgkreis")), ]
kreise_2016 <- rbind(kreise_2016, kreise_2016_merged)

# Anwendung der Funktion auf kreise_2017 mit den Einwohnerzahlen von 2017
kreise_2017_merged <- merge_eisenach_wartburgkreis(kreise_2017, einwohner_wartburgkreis = 125655, einwohner_eisenach = 42710)
kreise_2017 <- kreise_2017[!(kreise_2017$kreis %in% c("Eisenach", "Wartburgkreis")), ]
kreise_2017 <- rbind(kreise_2017, kreise_2017_merged)

# Anwendung der Funktion auf kreise_2018 mit den Einwohnerzahlen von 2018
kreise_2018_merged <- merge_eisenach_wartburgkreis(kreise_2018, einwohner_wartburgkreis = 124729, einwohner_eisenach = 42370)
kreise_2018 <- kreise_2018[!(kreise_2018$kreis %in% c("Eisenach", "Wartburgkreis")), ]
kreise_2018 <- rbind(kreise_2018, kreise_2018_merged)

# Anwendung der Funktion auf kreise_2019 mit den Einwohnerzahlen von 2019
kreise_2019_merged <- merge_eisenach_wartburgkreis(kreise_2019, einwohner_wartburgkreis = 123025, einwohner_eisenach = 42370)
kreise_2019 <- kreise_2019[!(kreise_2019$kreis %in% c("Eisenach", "Wartburgkreis")), ]
kreise_2019 <- rbind(kreise_2019, kreise_2019_merged)

# Anwendung der Funktion auf kreise_2020 mit den Einwohnerzahlen von 2020
kreise_2020_merged <- merge_eisenach_wartburgkreis(kreise_2020, einwohner_wartburgkreis = 118974, einwohner_eisenach = 42250)
kreise_2020 <- kreise_2020[!(kreise_2020$kreis %in% c("Eisenach", "Wartburgkreis")), ]
kreise_2020 <- rbind(kreise_2020, kreise_2020_merged)

# Anwendung der Funktion auf kreise_2021 mit den Einwohnerzahlen von 2021
kreise_2021_merged <- merge_eisenach_wartburgkreis(kreise_2021, einwohner_wartburgkreis = 117967, einwohner_eisenach = 41970)
kreise_2021 <- kreise_2021[!(kreise_2021$kreis %in% c("Eisenach", "Wartburgkreis")), ]
kreise_2021 <- rbind(kreise_2021, kreise_2021_merged)

rm(dataframes, kreise_2015_merged, kreise_2016_merged, kreise_2017_merged, kreise_2018_merged, kreise_2019_merged, kreise_2020_merged, kreise_2021_merged, merge_eisenach_wartburgkreis, merge_osterode_goettingen)

# 3. Vorbereiten für Merge mit Bundesländern ---------------------------------------------------
# Liste der DataFrames (kreise_2015 bis kreise_2023)
kreise_list <- list(kreise_2015, kreise_2016, kreise_2017, kreise_2018, 
                    kreise_2019, kreise_2020, kreise_2021, kreise_2022, kreise_2023)

# Funktion zur Bearbeitung der DataFrames
process_kreise_dataframe <- function(df) {
  df %>%
    rename(region = kreis) %>%  # 'kreis' in 'region' umbenennen
    select(schluessel, straftat, gemeindeschluessel, region, faelle, haeufigkeit, versuche, 
           versuche_anteil, aufklaerung_faelle, aufklaerungsquote, tatverdaechtige, 
           tatverdaechtige_auslaender, tatverdaechtige_auslaender_anteil)
}

# Anwendung der Funktion auf alle DataFrames
kreise_list <- lapply(kreise_list, process_kreise_dataframe)

# Zuweisung zurück zu den ursprünglichen DataFrames
list2env(setNames(kreise_list, c("kreise_2015", "kreise_2016", "kreise_2017", 
                                 "kreise_2018", "kreise_2019", "kreise_2020", 
                                 "kreise_2021", "kreise_2022", "kreise_2023")), envir = .GlobalEnv)



