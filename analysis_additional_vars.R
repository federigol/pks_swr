#Skript, um die PKS-Daten auf Kreisebene mit weiteren Variablen zu mergen und zu analysieren

# 1. Laden der benötigten Skripte -----------------------------------------
source("cleaning_kreise.R")
source("bevoelkerung.R")

# 2. Joinen ------------------------------------
kreise_23_bevoelkerung <- kreise_2023 %>%
  inner_join(bevoelkerung_kreise, by = c("gemeindeschluessel" = "schluessel"))


kreise_23_bevoelkerung %>%
  #filter(schluessel == "217000") %>%
  select(gemeindeschluessel, region, haeufigkeit, bevoelkerung, bevoelkerungsdichte) %>%
  write.csv("output/straftaten_bevdichte.csv", row.names = FALSE)
