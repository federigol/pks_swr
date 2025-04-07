#Skript zum Mergen und Analysieren der PKS-Daten für Kreise und Bundesländer

# 1. dfs laden und rbinden ------------------------------------------------
# Laden der benötigten Skripte
source("cleaning_kreise.R")    # Lädt das Skript für die Kreise
source("cleaning_bundeslaender.R")  # Lädt das Skript für die Bundesländer

bind_kreise_bundeslaender <- function(kreise, bundeslaender) {
  rbind(kreise, bundeslaender)
}

# Anwendung der Funktion auf alle DataFrame-Paare
combined_list <- mapply(bind_kreise_bundeslaender, kreise_list, bundeslaender_list, SIMPLIFY = FALSE)

# Zurückspeichern der Dataframes
list2env(setNames(combined_list, c("pks_2015", "pks_2016", "pks_2017", 
                                   "pks_2018", "pks_2019", "pks_2020", 
                                   "pks_2021", "pks_2022", "pks_2023")), envir = .GlobalEnv)


#Einmal alle Daten zusammenfügen und speichern
map2_dfr(list(pks_2015, pks_2016, pks_2017, pks_2018, pks_2019, pks_2020, pks_2021, pks_2022, pks_2023), 
         2015:2023, 
         ~ mutate(.x, Jahr = .y)) %>% 
  select(Jahr, everything()) %>% 
  write.csv("output/pks_lang.csv", row.names = FALSE)


# Entfernen der ursprünglichen DataFrames
rm(kreise_2015, kreise_2016, kreise_2017, kreise_2018, 
   kreise_2019, kreise_2020, kreise_2021, kreise_2022, kreise_2023,
   bundeslaender_2015, bundeslaender_2016, bundeslaender_2017, bundeslaender_2018,
   bundeslaender_2019, bundeslaender_2020, bundeslaender_2021, bundeslaender_2022, bundeslaender_2023,
   kreise_list, bundeslaender_list, bind_kreise_bundeslaender, process_kreise_dataframe, process_bundeslaender_dataframe, combined_list)

# 2. Analysis und Schreiben der Daten -------------------------------------------------------------
# Jahre auswählen, die für den jeweiligen Straftatbestand relevant sind
dataframes <- list("2015" = pks_2015, 
                   "2016" = pks_2016, "2017" = pks_2017,
  "2018" = pks_2018, "2019" = pks_2019, "2020" = pks_2020, 
                   "2021" = pks_2021, "2022" = pks_2022, "2023" = pks_2023)

# Achtung: Deliktschlüssel ändern, um nach Zeilen mit jeweiligem Straftatbestand zu filtern
dataframes <- map(dataframes, ~ .x %>% filter(schluessel == "670013"))

# 2. Behalte nur relevante Spalten und benenne sie um
dataframes <- map2(dataframes, names(dataframes), function(df, year) {
  # Umbenennen der Spalten für haeufigkeit, faelle und aufklaerungsquote
  df <- df %>%
    select(region, gemeindeschluessel, haeufigkeit, faelle, aufklaerungsquote) %>%
    rename(
      !!paste0("haeufigkeit_", year) := haeufigkeit,
      !!paste0("faelle_", year) := faelle,
      !!paste0("aufklaerungsquote_", year) := aufklaerungsquote
    )
  
  # Manuelle Anordnung der Spalten
  df <- df %>%
    select(
      region, gemeindeschluessel, 
      paste0("haeufigkeit_", year), 
      paste0("faelle_", year), 
      paste0("aufklaerungsquote_", year)
    )
  
  return(df)
})

# 3. Führe die DataFrames zusammen
straftatbestand <- reduce(dataframes, full_join, by = c("region", "gemeindeschluessel"))

straftatbestand <- straftatbestand %>%
  select(
    region, gemeindeschluessel, 
    starts_with("haeufigkeit"), 
    starts_with("faelle"), 
    starts_with("aufklaerungsquote")
  )


# Nur Bundesländer --------------------------------------------------------
#Wert für Hessen ggf. manuell korrigieren, weil falsch in der PKS-Tabelle
#straftatbestand$haeufigkeit_2023[straftatbestand$region == "Hessen"] <- 362400 / 6391360 * 100000
bundeslaender <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",
  "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen",
  "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt",
  "Schleswig-Holstein", "Thüringen", "Deutschland"
)

straftatbestand <- straftatbestand %>%
  filter(region %in% bundeslaender)

#dfs schreiben; Achtung: Dateinamen und Jahre anpassen
straftatbestand %>% 
  select(region, haeufigkeit_2015,
         haeufigkeit_2016, haeufigkeit_2017,
         haeufigkeit_2018, haeufigkeit_2019, haeufigkeit_2020, haeufigkeit_2021, haeufigkeit_2022, haeufigkeit_2023) %>%
  write.csv("output/autodiebstaehle_bl.csv", row.names = FALSE)
