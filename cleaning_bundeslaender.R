#Skript zum Bereinigen der Bundesland-Daten aus der PKS
library(needs)
needs(tidyverse, purrr, stringr, openxlsx)

# 1. Import data ----------------------------------------------------------
bundeslaender_2015 <- read.csv("input/bl_2015.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
bundeslaender_2016 <- read.csv("input/bl_2016.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
bundeslaender_2017 <- read.csv("input/bl_2017.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
bundeslaender_2018 <- read.csv("input/bl_2018.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
bundeslaender_2019 <- read.csv("input/bl_2019.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
bundeslaender_2020 <- read.csv("input/bl_2020.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
bundeslaender_2021 <- read.csv("input/bl_2021.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")
bundeslaender_2022 <- read.xlsx("input/bl_2022.xlsx", startRow = 7)
bundeslaender_2023 <- read.csv("input/bl_2023.csv", sep = ";", skip = 1, fileEncoding = "ISO-8859-1")

# 2. Clean Data -----------------------------------------------------------
colnames(bundeslaender_2016) <- c("schluessel", "straftat", "gemeindeschluessel", "Bundesland", "faelle", 
                                  "haeufigkeit", "versuche", "versuche_anteil", "aufklaerung_faelle", 
                                  "aufklaerungsquote", "tatverdaechtige", "tatverdaechtige_auslaender", 
                                  "tatverdaechtige_auslaender_anteil")

bundeslaender_2016 <- bundeslaender_2016 %>%
  select(-gemeindeschluessel, gemeindeschluessel) %>%  # Spalte nach hinten verschieben
  mutate(Bundesland = ifelse(Bundesland == "Baden-Wuerttemberg", "Baden-Württemberg", Bundesland)) %>%
  mutate(Bundesland = ifelse(Bundesland == "Thueringen", "Thüringen", Bundesland))

colnames(bundeslaender_2022) <- colnames(bundeslaender_2021)


#Add AGS
bundeslaender_list <- list(bundeslaender_2015, bundeslaender_2016, bundeslaender_2017, 
                           bundeslaender_2018, bundeslaender_2019, bundeslaender_2020, 
                           bundeslaender_2021, bundeslaender_2022, bundeslaender_2023)

# Bundesland-AGS-Zuordnung
ags_map <- c(
  "Schleswig-Holstein" = 1, "Hamburg" = 2, "Niedersachsen" = 3, "Bremen" = 4,
  "Nordrhein-Westfalen" = 5, "Hessen" = 6, "Rheinland-Pfalz" = 7, "Baden-Württemberg" = 8,
  "Bayern" = 9, "Saarland" = 10, "Berlin" = 11, "Brandenburg" = 12, "Mecklenburg-Vorpommern" = 13,
  "Sachsen" = 14, "Sachsen-Anhalt" = 15, "Thüringen" = 16
)

# Umbenennung der Spalte "X" in "gemeindeschluessel" für 2017 und 2023
bundeslaender_list[c(3,9)] <- map(bundeslaender_list[c(3,9)], ~ rename(.x, gemeindeschluessel = X))

# Spalte "gemeindeschluessel" hinzufügen für 2015, 2018-2022, falls nicht vorhanden
bundeslaender_list[c(1, 4:9)] <- map(bundeslaender_list[c(1, 4:9)], ~ mutate(.x, gemeindeschluessel = ags_map[Bundesland]))

# DataFrames zurückspeichern
list2env(setNames(bundeslaender_list, paste0("bundeslaender_", 2015:2023)), envir = .GlobalEnv)

# Umwandlung der Werte für bundeslaender_2023
bundeslaender_2023[4:13] <- lapply(bundeslaender_2023[4:13], function(x) {
  # Entfernen des Tausendertrennzeichens (Punkt)
  x <- gsub("\\.", "", x)
  # Ersetzen des Kommas als Dezimaltrennzeichen durch Punkt
  x <- gsub(",", ".", x)
  # Umwandlung der Werte in numeric
  as.numeric(x)
})

# Liste der DataFrames
bundeslaender_list <- list(bundeslaender_2015, bundeslaender_2016, bundeslaender_2017, 
                           bundeslaender_2018, bundeslaender_2019, bundeslaender_2020, 
                           bundeslaender_2021, bundeslaender_2022, bundeslaender_2023)

# Funktion zur Umwandlung der Werte und Spaltennamen für die DataFrames
process_dataframe <- function(df) {
  # Konvertierung der Werte von Spalte 4 bis 13: Kommata entfernen und in numeric umwandeln
  df[4:13] <- lapply(df[4:13], function(x) {
    x <- gsub(",", "", x)  # Entfernen der Kommata als Tausendertrennzeichen
    as.numeric(x)  # Umwandlung zu numeric
  })
  df[4:13][is.na(df[4:13])] <- 0  # NAs durch 0 ersetzen
  return(df)
}

# Anwendung der Funktion auf alle DataFrames
bundeslaender_list <- lapply(bundeslaender_list, process_dataframe)

# DataFrames zurückspeichern
list2env(setNames(bundeslaender_list, paste0("bundeslaender_", 2015:2023)), envir = .GlobalEnv)

# Spaltennamen für alle DataFrames anpassen
new_colnames <- c("schluessel", "straftat", "region", "faelle", 
                  "haeufigkeit", "versuche", "versuche_anteil", "aufklaerung_faelle", 
                  "aufklaerungsquote", "tatverdaechtige", "tatverdaechtige_auslaender", 
                  "tatverdaechtige_auslaender_anteil", "gemeindeschluessel")

# Spaltennamen in allen DataFrames anpassen
bundeslaender_list <- lapply(bundeslaender_list, function(df) {
  colnames(df) <- new_colnames
  return(df)
})

# DataFrames zurückspeichern
list2env(setNames(bundeslaender_list, paste0("bundeslaender_", 2015:2023)), envir = .GlobalEnv)

# Spalte "gemeindeschluessel" an die dritte Stelle verschieben
bundeslaender_list <- map(bundeslaender_list, ~ .x %>% relocate(last_col(), .after = 2))

# DataFrames zurückspeichern
list2env(setNames(bundeslaender_list, paste0("bundeslaender_", 2015:2023)), .GlobalEnv)

rm(bundeslaender_list, new_colnames, process_dataframe, ags_map)

# Werte für Deutschland umwandeln
bundeslaender_2015 <- bundeslaender_2015 %>%
  mutate(region = ifelse(region == "Bund echte Zählung der Tatverdächtigen", "Deutschland", region),
         gemeindeschluessel = ifelse(gemeindeschluessel == 70, 0, gemeindeschluessel))

bundeslaender_2016 <- bundeslaender_2016 %>%
  mutate(region = ifelse(region == "Bund echte Zaehlung der Tatverdaechtigen", "Deutschland", region),
         gemeindeschluessel = ifelse(gemeindeschluessel == 70, 0, gemeindeschluessel))

bundeslaender_2017 <- bundeslaender_2017 %>%
  mutate(region = ifelse(region == "Bund echte Zählung der Tatverdächtigen", "Deutschland", region),
         gemeindeschluessel = ifelse(gemeindeschluessel == 70, 0, gemeindeschluessel))

bundeslaender_2018 <- bundeslaender_2018 %>%
  mutate(region = ifelse(region == "Bundesrepublik Deutschland", "Deutschland", region))

bundeslaender_2019 <- bundeslaender_2019 %>%
  mutate(region = ifelse(region == "Bundesrepublik Deutschland", "Deutschland", region))

bundeslaender_2020 <- bundeslaender_2020 %>%
  mutate(region = ifelse(region == "Bundesrepublik Deutschland", "Deutschland", region))

bundeslaender_2021 <- bundeslaender_2021 %>%
  mutate(region = ifelse(region == "Bundesrepublik Deutschland", "Deutschland", region))

bundeslaender_2022 <- bundeslaender_2022 %>%
  mutate(region = ifelse(region == "Bundesrepublik Deutschland", "Deutschland", region))

bundeslaender_2023 <- bundeslaender_2023 %>%
  mutate(region = ifelse(region == "Bundesrepublik Deutschland", "Deutschland", region))


# Vorbereiten für Merge mit Kreisdaten ---------------------------------------------------
#Joinen
bundeslaender_list <- list(bundeslaender_2015, bundeslaender_2016, bundeslaender_2017, bundeslaender_2018,
                           bundeslaender_2019, bundeslaender_2020, bundeslaender_2021, bundeslaender_2022, bundeslaender_2023)


# Bundesländer bearbeiten: Berlin, Hamburg, Bremen raus um Dopplung mit Kreisdaten zu vermeiden
process_bundeslaender_dataframe <- function(df) {
  df %>% filter(!region %in% c("Berlin", "Hamburg", "Bremen"))
}

# Anwendung der Funktion auf alle DataFrames
bundeslaender_list <- lapply(bundeslaender_list, process_bundeslaender_dataframe)

list2env(setNames(bundeslaender_list, c("bundeslaender_2015", "bundeslaender_2016", "bundeslaender_2017", 
                                        "bundeslaender_2018", "bundeslaender_2019", "bundeslaender_2020", 
                                        "bundeslaender_2021", "bundeslaender_2022", "bundeslaender_2023")), envir = .GlobalEnv)






